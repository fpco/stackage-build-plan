{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Stackage.BuildPlan
    ( Settings
    , defaultSettings
    , getBuildPlan
    , toSimpleText
    , toShellScript
    , mkPackageName
    ) where

import           Control.Applicative         ((<|>))
import qualified Control.Exception           as E
import           Control.Monad               (forM_, unless, when)
import           Control.Monad.Catch         (Exception, MonadThrow, throwM)
import           Control.Monad.State.Strict  (MonadState, execStateT, get,
                                              modify)
import           Control.Monad.Writer.Strict (execWriter, tell)
import           Data.Aeson                  (object, (.=))
import qualified Data.Aeson                  as A
import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as L
import           Data.Foldable               (Foldable)
import qualified Data.Foldable               as F
import           Data.Function               (fix)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid                 ((<>))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Read              (decimal)
import           Data.Time                   (Day)
import           Data.Typeable               (Typeable)
import           Data.Version                (Version)
import           Data.Yaml                   (decodeFileEither)
import           Distribution.Package        (PackageName)
import           Network.HTTP.Client         (Manager, brRead, httpLbs,
                                              newManager, parseUrl,
                                              responseBody, withResponse)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Stackage.Types              (BuildPlan (..), Component (..),
                                              DepInfo (..),
                                              PackageConstraints (..),
                                              PackagePlan (..), SystemInfo (..),
                                              display, mkPackageName,
                                              sdPackages, unFlagName)
import           System.Directory            (createDirectoryIfMissing,
                                              doesFileExist,
                                              getAppUserDataDirectory,
                                              renameFile)
import           System.FilePath             ((<.>), (</>))
import           System.IO                   (IOMode (WriteMode),
                                              withBinaryFile)
import           Text.Read                   (readMaybe)

catchIO :: IO a -> (E.IOException -> IO a) -> IO a
catchIO = E.catch

data CompleteSpec
    = Nightly !Day
    | LTS !Int !Int
instance Show CompleteSpec where
    show (Nightly d) = "nightly-" ++ show d
    show (LTS x y) = concat ["lts-", show x, ".", show y]
data IncompleteSpec
    = LTSMajor !Int
    | LTSNewest
    | NightlyNewest
    deriving Show
data SnapshotSpec
    = CompleteSpec !CompleteSpec
    | IncompleteSpec !IncompleteSpec
    deriving Show

resolveSpec :: Manager -> SnapshotSpec -> IO CompleteSpec
resolveSpec _ (CompleteSpec x) = return x
resolveSpec man (IncompleteSpec spec) = do
    res <- httpLbs "https://www.stackage.org/download/lts-snapshots.json" man
    let lbs = responseBody res
    m <-
        case A.eitherDecode' lbs of
            Left e -> throwM $ InvalidSnapshotsJson lbs e
            Right m -> return m
    case Map.lookup key m of
        Nothing -> throwM $ SpecNotResolved key m
        Just val -> parseCompleteSpec val
  where
    key =
        case spec of
            LTSMajor m -> T.pack $ "lts-" ++ show m
            LTSNewest -> "lts"
            NightlyNewest -> "nightly"

parseCompleteSpec :: MonadThrow m => Text -> m CompleteSpec
parseCompleteSpec t =
    maybe (throwM $ InvalidSpec t) return $
        parseNightly <|> parseLts
  where
    parseNightly = do
        d <- T.stripPrefix "nightly-" t
        x <- readMaybe $ T.unpack d
        Just $ Nightly x
    parseLts = do
        t1 <- T.stripPrefix "lts-" t
        Right (x, t2) <- Just $ decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ decimal t3
        Just $ LTS x y

data BuildPlanException
    = InvalidSnapshotsJson !L.ByteString !String
    | SpecNotResolved !Text !(Map Text Text)
    | InvalidSpec !Text
    | PackageNotFound !PackageName
    deriving (Show, Typeable)
instance Exception BuildPlanException

data Settings = Settings
    { _snapshot   :: !SnapshotSpec
    , _getManager :: !(IO Manager)
    , _fullDeps   :: !Bool
    -- ^ include test and benchmark deps
    }

defaultSettings :: Settings
defaultSettings = Settings
    { _snapshot = IncompleteSpec LTSNewest
    , _getManager = newManager tlsManagerSettings
    , _fullDeps = False
    }

yamlFP :: Manager -> SnapshotSpec -> IO FilePath
yamlFP man spec' = do
    spec <- resolveSpec man spec'
    root <- getAppUserDataDirectory "stackage"
        `catchIO` \_ -> return "/tmp/.stackage" -- server does not set HOME
    let dir = root </> "build-plan"
        fp = dir </> show spec <.> "yaml"
    exists <- doesFileExist fp
    if exists
        then return fp
        else do
            createDirectoryIfMissing True dir
            let tmp = fp <.> "tmp"
            download man spec tmp
            renameFile tmp fp
            return fp

download :: Manager -> CompleteSpec -> FilePath -> IO ()
download man spec dest = do
    req <- parseUrl $ specUrl spec
    withResponse req man $ \res -> withBinaryFile dest WriteMode $ \h ->
        fix $ \loop -> do
            bs <- brRead $ responseBody res
            unless (S.null bs) $ S.hPut h bs >> loop

specUrl :: CompleteSpec -> String
specUrl (LTS x y) = concat
    [ "https://raw.githubusercontent.com/fpco/lts-haskell/master/lts-"
    , show x
    , "."
    , show y
    , ".yaml"
    ]
specUrl (Nightly x) = concat
    [ "https://raw.githubusercontent.com/fpco/stackage-nightly/master/nightly-"
    , show x
    , ".yaml"
    ]

data ToInstall = ToInstall
    { tiPackage :: !PackageName
    , tiVersion :: !Version
    , tiIsCore  :: !Bool
    , tiFlags   :: !(Map Text Bool)
    }
    deriving Show

instance A.ToJSON ToInstall where
    toJSON ti = object
        [ "name" .= display (tiPackage ti)
        , "version" .= display (tiVersion ti)
        , "flags" .= tiFlags ti
        , "is-core" .= tiIsCore ti
        ]

getBuildPlan :: Settings -> [PackageName] -> IO [ToInstall]
getBuildPlan _ [] = return []
getBuildPlan set packages = do
    man <- _getManager set
    fp <- yamlFP man $ _snapshot set
    bp <- decodeFileEither fp >>= either throwM return
    (_, front) <- execStateT (getDeps bp (_fullDeps set) packages) (Set.empty, id)
    return $ front []

type TheState =
    ( Set PackageName
    , DList ToInstall
    )
type DList a = [a] -> [a]

getDeps :: forall f m. (MonadThrow m, MonadState TheState m, Foldable f)
        => BuildPlan
        -> Bool
        -> f PackageName
        -> m ()
getDeps bp fullDeps =
    F.mapM_ goName
  where
    goName :: PackageName -> m ()
    goName name = do
        (s, _) <- get
        when (name `Set.notMember` s) $
            case Map.lookup name $ bpPackages bp of
                Just pkg -> goPkg name pkg
                Nothing ->
                    case Map.lookup name $ siCorePackages $ bpSystemInfo bp of
                        Just version -> do
                            addToSet name
                            addToList name version Map.empty True
                        Nothing -> throwM $ PackageNotFound name

    goPkg :: PackageName -> PackagePlan -> m ()
    goPkg name pp = do
        addToSet name
        forM_ (Map.toList $ sdPackages $ ppDesc pp) $ \(name', depInfo) ->
            when (includeDep depInfo) (goName name')
        addToList name (ppVersion pp)
            (Map.mapKeysWith const unFlagName
             $ pcFlagOverrides $ ppConstraints pp)
            False

    addToSet name = modify $ \(s, front) -> (Set.insert name s, front)

    addToList name version flags isCore =
        modify $ \(s, front) -> (s, front . (x:))
      where
        x = ToInstall
            { tiPackage = name
            , tiVersion = version
            , tiFlags = flags
            , tiIsCore = isCore
            }

    includeDep di =
        fullDeps ||
        CompLibrary    `Set.member` diComponents di ||
        CompExecutable `Set.member` diComponents di

toSimpleText :: [ToInstall] -> Text
toSimpleText =
    T.unlines . map go
  where
    go ti = T.unwords
        [ display $ tiPackage ti
        , display $ tiVersion ti
        ]

toShellScript :: Settings -> [ToInstall] -> Text
toShellScript _set packages = T.unlines $ ($ []) $ execWriter $ do
    yield "#!/usr/bin/env bash\nset -eux\n"
    forM_ packages $ \ti -> unless (tiIsCore ti) $ do
        let prefix = T.concat
                [ display $ tiPackage ti
                , "-"
                , display $ tiVersion ti
                ]
            tarball = prefix <> ".tar.gz"
        mapM_ yield
            [ ""
            , T.concat
                [ "rm -rf "
                , prefix
                , " "
                , tarball
                ]
            , "wget https://s3.amazonaws.com/hackage.fpcomplete.com/package/" <> tarball
            , "tar xf " <> tarball
            , "cd " <> prefix
            , T.concat
                [ "runghc Setup configure --user --flags='"
                , showFlags $ tiFlags ti
                , "'"
                ]
            , "runghc Setup build"
            , "runghc Setup copy"
            , "runghc Setup register"
            , "cd .."
            ]
  where
    yield x = tell (x:)
    showFlags =
        T.unwords . map go . Map.toList
      where
        go (name, isOn) = (if isOn then id else (T.cons '-')) name
