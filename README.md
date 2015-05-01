# stackage-build-plan

[![Build Status](https://travis-ci.org/fpco/stackage-build-plan.svg?branch=master)](https://travis-ci.org/fpco/stackage-build-plan)

Calculate and print (in different formats) Stackage build plans. This includes
generating bootstrap shell scripts, such as needed by cabal-install. In fact,
that use case is [the primary
motivation](https://github.com/fpco/stackage-server/issues/95) for releasing
this code as its own package.

## Why Stackage?

This package takes advantage of the Stackage Nightly and LTS Haskell snapshots
to calculate known-good build plans to display to users.

## Future enhancements

None planned
