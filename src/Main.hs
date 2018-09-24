{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow
import Control.Exception (Exception (..))
import Control.Funflow
import Control.Funflow.ContentStore (Content (..), contentItem, itemPath, (^</>))
import qualified Control.Funflow.ContentStore as CS
import qualified Control.Funflow.External.Docker as Docker
import Control.Funflow.External
import Path
import Path.IO
import Control.Funflow.ContentHashable
import Control.Arrow.Free

import System.Posix.Files
import Control.Monad
import Data.Foldable
import Data.Default
import qualified Data.Map as M

main :: IO ()
main = do
    cwd <- getCurrentDir
    r <- withSimpleLocalRunner (cwd </> [reldir|funflow-example/store|]) $ \run ->
      run (mainFlow >>> storePath) ()
    case r of
      Left err ->
        putStrLn $ "FAILED: " ++ displayException err
      Right out -> do
        putStrLn $ "SUCCESS"
        putStrLn $ toFilePath out

-- | Filter a list given an arrow filter
partitionA :: ArrowChoice a => a b (Either c d) -> a [b] ([c], [d])
partitionA f = proc xs ->
  case xs of
    [] -> returnA -< ([], [])
    (y:ys) -> do
      b <- f -< y
      case b of
        Left c -> ((second (partitionA f)) >>> arr (\(c, (cs, ds)) -> (c:cs, ds))) -< (c,ys)
        Right d -> ((second (partitionA f)) >>> arr (\(d, (cs, ds)) -> (cs, d:ds))) -< (d, ys)

boolChoice :: ArrowFlow eff ex a => a CS.Item (Either CS.Item (Content Dir))
boolChoice = proc dir -> do
  str <- readString_ -< dir
  case str of
    '0':_ -> returnA -< (Left dir)
    '1':_ -> returnA -< (Right (All dir))
    _ -> returnA -< error str


deployScripts :: SimpleFlow (Content Dir) ()
deployScripts = proc dir -> do
  cwd <- stepIO (const getCurrentDir) -< ()
  function_dir_upload <- copyDirToStore -< (DirectoryContent (cwd </> [reldir|functions/upload-world-file|]), Nothing)
  function_dir_verif <- copyDirToStore -< (DirectoryContent (cwd </> [reldir|functions/send-verification-email|]), Nothing)
  function_dir_final <- copyDirToStore -< (DirectoryContent (cwd </> [reldir|functions/do-verification|]), Nothing)
  nixScript [relfile|deploy-function|] [] (\fndir -> [contentParam fndir])
    -< (dir, function_dir_upload)
  nixScript [relfile|deploy-verif-function|] [] (\fndir -> [contentParam fndir])
    -< (dir, function_dir_verif)
  nixScript [relfile|deploy-final-function|] [] (\fndir -> [contentParam fndir])
    -< (dir, function_dir_final)
  returnA -< ()

getWorldFiles :: SimpleFlow (Content Dir) (Content Dir)
getWorldFiles = proc script_dir -> do
  step All <<< impureNixScript [relfile|get-world-files|] [] (\_ -> [outParam]) -< (script_dir, ())

groupKeysPure :: (Content File, M.Map String (Content File)) -> (Content File, Maybe (Content File))
groupKeysPure (c, map) =
  let getHash = reverse . drop 7 . reverse
  in (c, M.lookup (getHash (toFilePath (CS.contentFilename c))) map)


groupKeys :: SimpleFlow ([Content File], Content Dir) ([(Content File, Maybe (Content File))])
groupKeys = proc (cs, d) -> do
  d' <- splitDir -< d
  let getHash = reverse . drop 4 . reverse
  let world_file_map = M.fromList [ (getHash (toFilePath (CS.contentFilename c)), c) | c <- d' ]
  mapA (step groupKeysPure) -< map (, world_file_map) cs


mainFlow :: SimpleFlow () (Content Dir)
mainFlow = proc () -> do
  cwd <- stepIO (const getCurrentDir) -< ()

  script_dir <- copyDirToStore -< (DirectoryContent (cwd </> [reldir|scripts/|]), Nothing)

  worldFiles <- getWorldFiles  -< script_dir
  stepIO print -< worldFiles

  meta_dir <- step All <<< scrape -< (script_dir, ())
  meta_keys <- splitDir -< meta_dir
  keys <- groupKeys -< (meta_keys, worldFiles)
  (maps, raw_maps) <- partitionA boolChoice <<< mapA (fetch) -< [( script_dir, event) | event <- keys]
  raw_map_dir <- mergeDirs <<< mapA rmOut -< raw_maps
  manifest_dir <- createManifest -< (script_dir, raw_map_dir)
  uploadManifest -< (script_dir, All manifest_dir)

  stepIO print <<< storePath -< All manifest_dir

  res <- georefFlow -< (script_dir, meta_dir, maps)

  deployScripts -< script_dir

  georefSite <- copyDirToStore -< (DirectoryContent (cwd </> [reldir|html/|]), Nothing)

  uploadSite -< (script_dir, res)

  uploadGeoreferencer -< (script_dir, georefSite)

  returnA -< res





{-
Plan

1. Upload a big JSON dictionary which lists all the non-georeferenced maps
2. Display this list in the browser, click on an entry to load the map into georeferencing pane
3. Upload sends file to google cloud function which puts the file in a staging area
4. Simple web interface grabs jobs from the staging area and checks the georeferencing is correct
5. World file is pushed to bucket which is then used for master.
-}


georefFlow :: SimpleFlow (Content Dir, Content Dir, [CS.Item]) (Content Dir)
georefFlow = proc (script_dir, meta_dir, maps) -> do
  mapJpgs <- mapA convertToGif -< [(script_dir, m) | m <- maps]
  merge_dir <- mergeDirs' <<< mapA (step All) <<< mapA warp -< [(script_dir, jpg) | jpg <- mapJpgs ]
  toMerge <- splitDir -< merge_dir
  vrt_dir <- step All <<< mergeRasters -< (script_dir, toMerge)
  merged_vrts <- splitDir -< vrt_dir
  tiles <- mergeDirs' <<< mapA (step All) <<< mapSeqA makeTiles -< [(script_dir, vrt) | vrt <- merged_vrts]

  leaflet <- step All <<< makeLeaflet -< ( script_dir, (merge_dir, meta_dir))

  mergeDirs -< [leaflet, tiles]

singleton x = [x]


-- Need to mark this as impure
scrape = impureNixScript [relfile|scraper.py|] [[relfile|shell.nix|]]
          (\() -> [ outParam ])

fetch = nixScriptWithOutput [relfile|fetch.py|] [[relfile|shell.nix|]]
          (\(metadata, mwf)-> [ outParam, contentParam metadata ] ++ (maybe [] (singleton . contentParam) mwf))

convertToGif = nixScript [relfile|convert_gif|] [] (\dir -> [ pathParam (IPItem dir), outParam ])

warp = nixScript [relfile|do_warp|] [] (\dir -> [ pathParam (IPItem dir), outParam ])

mergeRasters = nixScript [relfile|merge-rasters.py|] [[relfile|merge-rasters.nix|]] (\rs -> outParam : map contentParam rs )

makeTiles = nixScript [relfile|make_tiles|] [] (\dir -> [ contentParam dir, outParam, textParam "16" ])

makeLeaflet = nixScript [relfile|create-leaflet.py|] [[relfile|leaflet.nix|]] (\(vrt_dir, meta_dir) ->
                [ contentParam vrt_dir, contentParam meta_dir, textParam "16", outParam ])

uploadManifest = impureNixScript [relfile|upload-manifest|] [] (\dir -> [ contentParam dir ])

createManifest = nixScript [relfile|create-manifest.py|] [] (\dir -> [ outParam, contentParam dir ])

uploadSite = nixScript [relfile|upload-maps|] [] (\dir -> [ contentParam dir ])

uploadGeoreferencer = nixScript [relfile|upload-georeferencer|] [] (\dir -> [ contentParam dir ])

---

nixScript = nixScriptX False NoOutputCapture

nixScriptWithOutput = nixScriptX False StdOutCapture

impureNixScript :: ArrowFlow eff ex arr => Path Rel File -> [Path Rel File]
                    -> (a -> [Param]) -> arr (Content Dir, a) CS.Item
impureNixScript = nixScriptX True NoOutputCapture


--contentParam (
nixScriptX :: ArrowFlow eff ex arr => Bool
                                   -> OutputCapture
                                   -> Path Rel File
                                   -> [Path Rel File]
                                   -> (a -> [Param])
                                   -> arr (Content Dir, a) CS.Item
nixScriptX impure std script scripts params = proc (scriptDir, a) -> do
  env <- mergeFiles -< absScripts scriptDir
  external' props (\(s, args) -> ExternalTask
        { _etCommand = "perl"
        , _etParams = contentParam (s ^</> script) : params args
        , _etWriteToStdOut = std
        , _etEnv = [("NIX_PATH", envParam "NIX_PATH")] }) -< (env, a)
  where
    props = def { ep_impure = impure }
    absScripts sd = map (sd ^</>) (script : scripts)



-- | Merge a number of store directories together into a single output directory.
--   This uses hardlinks to avoid duplicating the data on disk.
mergeDirs' :: ArrowFlow eff ex arr => arr [CS.Content Dir] (CS.Content Dir)
mergeDirs' = proc inDirs -> do
  paths <- internalManipulateStore
    ( \store items -> return $ CS.contentPath store <$> items) -< inDirs
  arr CS.All <<< putInStore
    ( \d inDirs -> for_ inDirs $ \inDir -> do
      (subDirs, files) <- listDirRecur inDir
      for_ subDirs $ \absSubDir -> do
        relSubDir <- stripProperPrefix inDir absSubDir
        createDirIfMissing True (d </> relSubDir)
      for_ files $ \absFile -> do
        relFile <- stripProperPrefix inDir absFile
        let target = (toFilePath $ d </> relFile)
        exist <- fileExist target
        when (not exist) (createLink (toFilePath absFile) target)
    ) -< paths


rmOut :: ArrowFlow eff ex arr => arr (Content Dir) (Content Dir)
rmOut = proc dir -> do mergeFiles <<< globDir -< (dir, "*.pickle")

splitDir :: ArrowFlow eff ex arr => arr (Content Dir) ([Content File])
splitDir = proc dir -> do
  (_, fs) <- listDirContents -< dir
  mapA reifyFile -< fs


-- Put a file, which might be a pointer into a dir, into its own store
-- location.
reifyFile :: ArrowFlow eff ex arr => arr (Content File) (Content File)
reifyFile = proc f -> do
  file <- getFromStore return -< f
  putInStoreAt (\d fn -> copyFile fn d) -< (file, CS.contentFilename f)


storePath :: ArrowFlow eff ex arr => arr (Content Dir) (Path Abs Dir)
--storePath = internalManipulateStore (\cs d -> return (CS.itemPath cs (CS.contentItem d)))

storePath = getFromStore return

copyDirFromStore :: ArrowFlow eff ex arr => Path b1 Dir -> arr (Content Dir) ()
copyDirFromStore dest = getFromStore (\p -> copyDirRecur p dest)
