{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

{- |
 Functions to spawn cabal and ghc-pkg.
-}

module Hellno.Cabal where

import System.Process
import System.IO.Temp
import System.FilePath
import System.Exit
import System.Directory
import Text.Parsec
import Data.List (stripPrefix)
import Data.Maybe
import Data.Functor.Identity (Identity)
import qualified Control.Exception as E
import Control.Applicative hiding ((<|>), many, optional)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Configure

import Hellno
import Hellno.Packages
import Paths_hellno

cabalConfigure :: [String] -> IO LocalBuildInfo
cabalConfigure args = do
    cwd <- getCurrentDirectory
    noCabal <- null . filter ((==".cabal") . takeExtension) . filter (/=".cabal")
               <$> getDirectoryContents cwd

    if noCabal
        then do
        withSystemTempDirectory "hellno-dummy" $ \tmp ->
            E.bracket (setCurrentDirectory tmp) (const $ setCurrentDirectory cwd)
            $ const $ do
                dummyCabal <- getDataFileName "dummy.cabal_"
                copyFile dummyCabal "dummy.cabal"
                configure
        else configure
  where configure = do
            res <- maybeGetPersistBuildConfig "dist/setup-config"
            case res of
                Just lbi -> return lbi
                _ -> do
                    res <- runAndWait "cabal" $ "configure" : args
                    case res of
                        ExitFailure _ -> fail "failed to configure cabal package"
                        _ -> do
                            res <- tryGetPersistBuildConfig "dist"
                            case res of
                                Right lbi -> return lbi
                                Left e -> fail $ "failed to parse saved cabal configuration: " ++ fst e


-- | Do a @cabal install --dry-run@ and collect the package info.
-- If the first argument is True, use --only-dependencies
cabalDryRun :: Bool -> [String] -> IO [PackageId]
cabalDryRun onlyDeps args = do
    let d = if onlyDeps then ["--only-dependencies"] else []
    res <- fmap (parse pkgs "") $ readProcess "cabal" (["install", "--dry-run",
        "--avoid-reinstalls"] ++ d ++ args) ""
    case res of
        (Left err) -> error $ "Parsing cabal output failed: " ++ show err
        (Right a) -> return a


-- | Retrieve the list of currently installed packages as reported by ghc-pkg.
ghcPkgList :: IO [PackageId]
ghcPkgList = do
    res <- fmap (parse pkgs "") $ readProcess "cabal" ["exec", "ghc-pkg", "list"]
           $ ""
    case res of
        (Left err) -> error $ "Parsing ghc-pkg output failed: " ++ show err
        (Right a) -> return a


-- | Pass the argruments to @cabal install@. Returns True for /ExitSuccess/.
cabalInstall :: [String] -> IO Bool
cabalInstall args = do
    res <- runAndWait "cabal" ("install":"--user":args)
    return $ res == ExitSuccess

-- | Recache the user package database.
recacheUserDb :: IO ()
recacheUserDb = do
    -- If we don't wait for recaching to finish, it will result in annoying
    -- warnings.
    runAndWait "ghc-pkg" ["recache", "--user"]
    return ()

-- | Take the program name and arguments and wait for it to finish.
runAndWait :: String -> [String] -> IO ExitCode
runAndWait prg args = flip E.catch (\(_ :: E.AsyncException) ->
    putStrLn "\n\nInterrupting...\n" >> return (ExitFailure 13)) $ do
        (_, _, _, h) <- createProcess (proc prg args)
        waitForProcess h


-- | Parse a newline-delimited list of packages skipping lines
-- that can't be parsed as a package name.
pkgs :: (Stream s Identity Char) => Parsec s () [PackageId]
pkgs = fmap catMaybes $ many (try pkg <|> skipLine)
    where skipLine = many (noneOf "\n") >> newline >> return Nothing
          pkg = do
              spaces
              optional $ try $ char '('
              pid <- parsePackageName
              optional $ try $ char ')'
              skipLine
              return $ Just pid
