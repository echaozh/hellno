{-# LANGUAGE FlexibleContexts #-}

{- |
 Manage hellno's precompiled package database.
-}

module Hellno.Packages where

import qualified Distribution.InstalledPackageInfo as IPI
import System.Directory
import System.FilePath
import System.Posix.Files
import Data.Monoid
import Data.List (intercalate, isPrefixOf)
import Data.Functor.Identity (Identity)
import Data.Function
import Data.Composition
import Text.Parsec
import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad
import Control.Exception (throw, throwIO)
import Control.Applicative hiding (many)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.InstallDirs
import Distribution.Simple.Compiler

import Hellno


-- * Lookup functions


-- | Lookup a package in hellno's db.
lookupPackage :: PackageIdentifier -> IO [InstalledPackageId]
lookupPackage pid = do
    let (name, fullname) = packageIdToString pid
    let path = pkgRoot </> name </> fullname
    de <- doesDirectoryExist path
    if not de then
        return []
    else
        fmap (map InstalledPackageId) $ filterM (doesDirectoryExist .
            (path</>)) =<< getDirectoryContents' path

-- | Lookup the dependecies of a package in hellno's db.
lookupPackageDeps :: InstalledPackageId -> IO [InstalledPackageId]
lookupPackageDeps ipid = do
    let (pid, hashname) = parseInstalledPackageId ipid
    let (name, fullname) = packageIdToString pid
    res <- fmap IPI.parseInstalledPackageInfo $ readFile $
        pkgRoot </> name </> fullname </> (hashname ++ ".conf")
    case res of
        (IPI.ParseOk [] a) -> return $ IPI.depends a
        (IPI.ParseFailed err) -> throwIO $ userError $ show err


-- | List all packages in the database.
listPackages :: IO [InstalledPackageId]
listPackages =
    fmap (map InstalledPackageId . concat . concat) $
        getDirectoryContents' pkgRoot >>= mapM (\p ->
            getDirectoryContents' (pkgRoot </> p) >>= mapM (\v ->
                getDirectoryContents' (pkgRoot </> p </> v) >>=
                    filterM (doesDirectoryExist . ((pkgRoot </> p </> v) </>))))


-- | Get the current .conf file corresponding to the package
-- (without the \".conf\" part).
getHashName :: PackageIdentifier -> IO String
getHashName pid = do
    let (_, name) = packageIdToString pid
    let head' [] = throw $ userError $ "Package " ++ name ++
            " not found in GHC database"
        head' (a:_) = a
    fmap (dropExtension . head' . filter (name`isPrefixOf`)) $
        getDirectoryContents ghcPkg


-- * Managing packages


{- |
 \"grab\" a package that has just been installed by cabal-install
 and put it in the hellno package storage. After that you will probably
 want to \"push\" the package which would create appropriate symlinks in
 ~/.ghc and your cabal installation directory so that the package can be used.
 Returns the /InstalledPackageId/ of the newly grabbed package.
 The second argument is the names of executables provided by the package.
 The package must have a library.
-}
grabPackage :: LocalBuildInfo -> PackageIdentifier -> [String]
            -> IO InstalledPackageId
grabPackage lbi pid execs = do
    let (name, fullname) = packageIdToString pid
    putStrLn $ "grabbing package " ++ fullname
    hashname <- getHashName pid
    let path = pkgRoot </> name </> fullname </> hashname
    let moveStuff f = processStuffInDir moveRecursive True f pid lbi path ""
    moveStuff $ liftA2 (<>) libdir libsubdir
    moveStuff $ liftA2 (<>) datadir datasubdir
    moveStuff docdir
    moveRecursive (ghcPkg </> hashname <.> "conf") (path <.> "conf")
    unless (null execs) $ createDirectoryIfMissing True $ path </> "bin"
    forM_ execs $ processStuffInDir moveRecursive True bindir pid lbi path
    return $ InstalledPackageId hashname

instance Monoid PathTemplate where
    mempty = toPathTemplate ""
    mappend = toPathTemplate .: ((</>) `on` fromPathTemplate)
    mconcat = toPathTemplate . foldr (</>) "" . map fromPathTemplate

processStuffInDir op t f pid lbi path a = do
    let tpls = installDirTemplates lbi
        p = (fromPathTemplate $ f $ substituteInstallDirTemplates env tpls) </> a
    let path' = (fromPathTemplate $ f $ substituteInstallDirTemplates env
                 $ tpls {prefix = toPathTemplate path}) </> a
    print p
    print path'
    let toTest = if t then p else path'
    e <- (||) <$> doesDirectoryExist toTest <*> doesFileExist toTest
    when e $ do
        putStrLn $ "path exists: " ++ toTest
        op p path'
  where env = initialPathTemplateEnv
              pid
              (compilerId (compiler lbi))
              (hostPlatform lbi)


{- |
 Create symlinks to the package \"grabbed\" by hellno in ~/.ghc and the cabal
 installation directory.
-}
pushPackage :: LocalBuildInfo -> InstalledPackageId -> IO ()
pushPackage lbi ipid = do
    let (pid, hashname) = parseInstalledPackageId ipid
    let (name, fullname) = packageIdToString pid
    let path = pkgRoot </> name </> fullname </> hashname
    let makeLink f = processStuffInDir (flip createSymbolicLink) False f pid lbi
                     path ""
    putStrLn $ "pushing package" ++ fullname
    makeLink $ liftA2 (<>) libdir libsubdir
    makeLink $ liftA2 (<>) datadir datasubdir
    makeLink docdir
    linkIfExists (path <.> "conf") $ ghcPkg </> hashname <.> "conf"
    let linkChildren path dest = do
            children <- getDirectoryContents' dest
            forM_ children (liftA2 createSymbolicLink ($dest) ($path)
                                . (</>))
    processStuffInDir linkChildren False bindir pid lbi path ""
  where linkIfExists dest path = do
            e <- (||) <$> doesDirectoryExist dest <*> doesFileExist dest
            when e $ createSymbolicLink dest path


{- |
 Remove the package's symlinks hereby effectively removing it from GHC database.
-}
pullPackage :: LocalBuildInfo -> PackageIdentifier -> IO ()
pullPackage lbi pid = do
    let (name, fullname) = packageIdToString pid
    putStrLn $ "pulling package " ++ fullname
    hashname <- getHashName pid
    let path = pkgRoot </> name </> fullname </> hashname
    let removeLink f = processStuffInDir (const . removeFile) True f pid lbi ""
                       $ ""
    removeLink $ liftA2 (<>) libdir libsubdir
    removeLink $ liftA2 (<>) datadir datasubdir
    removeLink docdir
    removeFile $ ghcPkg </> hashname <.> "conf"
    let removeChildren dest path = do
            children <- getDirectoryContents' path
            forM_ children $ removeFile . (dest</>)
    processStuffInDir removeChildren False bindir pid lbi path ""

-- | Removes the package from hellno's database.
dropPackage :: LocalBuildInfo -> InstalledPackageId -> IO ()
dropPackage lbi ipid = do
    let (pid, hashname) = parseInstalledPackageId ipid
        (name, fullname) = packageIdToString pid
        path = pkgRoot </> name </> fullname </> hashname
        deleteStuff f = processStuffInDir (deleteRecursive .: flip const) False
                        f pid lbi path ""
    deleteStuff $ liftA2 (<>) libdir libsubdir
    deleteStuff $ liftA2 (<>) datadir datasubdir
    deleteStuff docdir
    deleteStuff bindir
    removeFile $ pkgRoot </> name </> fullname </> hashname <.> "conf"
    deleteIfEmpty $ pkgRoot </> name </> fullname
    deleteIfEmpty $ pkgRoot </> name
    where deleteIfEmpty dir = do
            e <- fmap null $ getDirectoryContents' dir
            when e $ removeDirectory dir


-- | \"Pull\" all packages managed by hellno.
clearPackages :: LocalBuildInfo -> IO ()
clearPackages lbi = getDirectoryContents' ghcPkg >>= filterM islnk >>=
    mapM_ (pullPackage lbi . fst . parseInstalledPackageId . InstalledPackageId)
    where islnk f = fmap isSymbolicLink $ getSymbolicLinkStatus $ ghcPkg </> f


-- * Parsing/showing


-- | Converts a package identifier to a tuple of (\"foo\", \"foo-0.2.1\")
packageIdToString :: PackageIdentifier -> (String, String)
packageIdToString (PackageIdentifier (PackageName name) (Version ver _)) =
    (name, name ++ "-" ++ intercalate "." (map show ver))


-- | Takes input of the form \"foo-1.2.3\".
parsePackageName :: (Stream s Identity Char) => Parsec s () PackageId
parsePackageName = do
    name <- fmap (intercalate "-") $ many $ try $
        many (noneOf "-.\n") <* char '-'
    ver <- fmap (map read) $ sepBy1 (many1 digit) (char '.')
    return $ PackageIdentifier (PackageName name) (Version ver [])


parseInstalledPackageId :: InstalledPackageId -> (PackageId, String)
parseInstalledPackageId (InstalledPackageId str) =
     (fromRight $ parse parsePackageName "" str, str)
     where fromRight (Right a) = a
           fromRight _ = throw $ userError $ "Couldn't parse " ++ str


-- * Moving directories around


moveRecursive :: FilePath -> FilePath -> IO ()
moveRecursive path dest = copyRecursive path dest >> deleteRecursive path


copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive path dest = do
    fs <- getSymbolicLinkStatus path
    case (isDirectory fs, isSymbolicLink fs) of
        (True, False) -> do
            createDirectoryIfMissing True dest
            getDirectoryContents' path >>=
                mapM_ (\p -> copyRecursive (path</>p) (dest</>p))
        (False, True) -> readSymbolicLink path >>=
            (flip createSymbolicLink) dest
        otherwise -> do
            createDirectoryIfMissing True $ takeDirectory dest
            copyFile path dest


deleteRecursive :: FilePath -> IO ()
deleteRecursive path = do
    fs <- getSymbolicLinkStatus path
    if isDirectory fs then do
        getDirectoryContents' path >>=
            mapM_ (deleteRecursive . (path</>))
        removeDirectory path
    else
        removeFile path


getDirectoryContents' a = fmap (filter (`notElem` [".",".."])) $
    getDirectoryContents a
