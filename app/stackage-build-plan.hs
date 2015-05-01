{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Data.Aeson                (toJSON)
import           Data.Aeson.Encode         (encodeToTextBuilder)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Text.Lazy.IO         as TLIO
import           Options.Applicative
import           Paths_stackage_build_plan (version)
import           Stackage.BuildPlan
import           Stackage.CLI              (simpleOptions, simpleVersion)

main :: IO ()
main = do
    ((set, render, packages), ()) <- simpleOptions
        $(simpleVersion version)
        "stackage-build-plan"
        "Calculate and print (in different formats) Stackage build plans"
        options
        empty
    tis <- getBuildPlan set $ map (mkPackageName . T.pack) packages
    TLIO.putStr $ render set tis
  where
    options = (,,)
        <$> setOptions
        <*> renderOptions
        <*> some (argument str (metavar "PACKAGES..."))

    mkSettings mmirror msnapshot =
          maybe id (setMirror . T.pack) mmirror
        $ maybe id setSnapshot msnapshot
        $ defaultSettings

    setOptions = mkSettings
        <$> ((fmap Just $ strOption
            ( long "mirror"
           <> help "Mirror to download packages from"
           <> metavar "URL"
            )) <|> pure Nothing)
        <*> ((fmap Just $ option readSnapshot
            ( long "snapshot"
           <> help "Which snapshot to pull the build plan from"
           <> metavar "SNAPSHOT"
            )) <|> pure Nothing)

    readSnapshot =
        str >>=
        either (fail . show) return . parseSnapshotSpec . T.pack

    renderOptions =
        (option readRender
            ( long "format"
           <> help "Output format: shell, simple, json"
           <> metavar "FORMAT"
           ))

    readRender = do
        x <- str
        case x of
            "shell" -> return $ \set tis ->
                TL.fromStrict $ toShellScript set tis
            "simple" -> return $ \_set tis ->
                TL.fromStrict $ toSimpleText tis
            "json" -> return $ \_set tis ->
                toLazyText $ encodeToTextBuilder $ toJSON tis
            _ -> fail $ "Invalid renderer: " ++ x
