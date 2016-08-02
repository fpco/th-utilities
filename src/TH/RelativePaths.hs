{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides Template Haskell utilities for loading files
-- based on paths relative to the root of your Cabal package.
--
-- Normally when building a cabal package, GHC is run with its current
-- directory set at the package's root directory. This allows using
-- relative paths to refer to files. However, this becomes problematic
-- when you want to load modules from multiple projects, such as when
-- using "stack ghci".
--
-- This solves the problem by getting the current module's filepath from
-- TH via 'location'. It then searches upwards in the directory tree for
-- a .cabal file, and makes the provided path relative to the folder
-- it's in.
module TH.RelativePaths where

import           Control.Exception (IOException, catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import           Language.Haskell.TH (Q, Loc(loc_filename), location, runIO, reportWarning)
import           Language.Haskell.TH.Syntax (addDependentFile)
import           System.Directory (getDirectoryContents, getCurrentDirectory, setCurrentDirectory, canonicalizePath)
import           System.FilePath

-- | Reads a file as a strict ByteString. The path is specified relative
-- to the package's root directory, and 'addDependentfile' is invoked on
-- the target file.
qReadFileBS :: FilePath -> Q BS.ByteString
qReadFileBS fp = do
    fp' <- pathRelativeToCabalPackage fp
    addDependentFile fp'
    runIO $ BS.readFile fp'

-- | Reads a file as a lazy ByteString. The path is specified relative
-- to the package's root directory, and 'addDependentfile' is invoked on
-- the target file.
qReadFileLBS :: FilePath -> Q LBS.ByteString
qReadFileLBS fp = do
    fp' <- pathRelativeToCabalPackage fp
    addDependentFile fp'
    runIO $ LBS.readFile fp'

-- | Reads a file as a strict Text. The path is specified relative
-- to the package's root directory, and 'addDependentfile' is invoked on
-- the target file.
qReadFileText :: FilePath -> Q T.Text
qReadFileText fp = do
    fp' <- pathRelativeToCabalPackage fp
    addDependentFile fp'
    runIO $ T.readFile fp'

-- | Reads a file as a lazy Text. The path is specified relative
-- to the package's root directory, and 'addDependentfile' is invoked on
-- the target file.
qReadFileLazyText :: FilePath -> Q LT.Text
qReadFileLazyText fp = do
    fp' <- pathRelativeToCabalPackage fp
    addDependentFile fp'
    runIO $ LT.readFile fp'

-- | Reads a file as a String. The path is specified relative
-- to the package's root directory, and 'addDependentfile' is invoked on
-- the target file.
qReadFileString :: FilePath -> Q String
qReadFileString fp = do
    fp' <- pathRelativeToCabalPackage fp
    addDependentFile fp'
    runIO $ readFile fp'

-- | Runs the 'Q' action, temporarily setting the current working
-- directory to the root of the cabal package.
withCabalPackageWorkDir :: Q a -> Q a
withCabalPackageWorkDir f = do
    cwd' <- pathRelativeToCabalPackage "."
    cwd <- runIO $ getCurrentDirectory
    runIO $ setCurrentDirectory cwd'
    x <- f
    runIO $ setCurrentDirectory cwd
    return x

-- | This utility takes a path that's relative to your package's cabal
-- file, and resolves it to an absolute location.
--
-- Note that this utility does _not_ invoke 'qAddDependentFile'.
pathRelativeToCabalPackage :: FilePath -> Q FilePath
pathRelativeToCabalPackage fp = do
    loc <- location
    parent <-
        if loc_filename loc == "<interactive>"
            then runIO getCurrentDirectory
            else do
                mcanonical <- runIO $ fmap Just (canonicalizePath (loc_filename loc))
                   `catch` \(_err :: IOException) -> return Nothing
                mcabalFile <- runIO $ maybe (return Nothing) findCabalFile mcanonical
                case mcabalFile of
                    Just cabalFile -> return (takeDirectory cabalFile)
                    Nothing -> do
                        reportWarning "Failed to find cabal file, in order to resolve relative paths in TH.  Using current working directory instead."
                        runIO getCurrentDirectory
    return (parent </> fp)

-- | Given the path to a file or directory, search parent directories
-- for a .cabal file.
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = do
    let parent = takeDirectory dir
    contents <- getDirectoryContents parent
    case find (\fp -> takeExtension fp == ".cabal") contents of
        Nothing
            | parent == dir -> return Nothing
            | otherwise -> findCabalFile parent
        Just fp -> return (Just (parent </> fp))
