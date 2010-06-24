{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Batch.Parallel where

import LHCOAnalysis.NewAnalysis

import LHCOAnalysis.Analysis.Hist
import LHCOAnalysis.FetchBinary

import HROOT

import Data.Array.Unboxed

import System.IO
import System.Directory

import Control.Concurrent

import Foreign.C.String
import qualified Foreign.Marshal.Array as FA

type SingleParallelTask a = (a,[FilePath])

-- | Command multiple serial action at once. Each serial action consists of parallel actions. 

doMultipleTsks :: (AnalysisTask a) => [SingleParallelTask a] -> IO () 
doMultipleTsks lst =  mapM_ doTsk lst 

-- | Command one action containing several parallel actions 

doTsk :: (AnalysisTask a) => SingleParallelTask a -> IO ()
doTsk tsk = 
    do let outfilename = rootfile (fst tsk)
       putStrLn $ "Work on " ++ outfilename
       checkoutputfile <- doesFileExist outfilename
       if checkoutputfile
         then putStrLn "File Exist!"
         else uncurry recordaction tsk
    
-- | record action : take an Analysis Task and File path list and make histogram from data of file names in parallel
--   and then we combine the histograms into one histogram. 
--   
       
recordaction :: (AnalysisTask a) => a -> [FilePath] -> IO ()
recordaction anal filepaths = 
    do let openfile x = openFile x ReadMode

       inhs  <- mapM openfile filepaths 
       mvars <- sequence (take (length inhs) $ repeat newEmptyMVar)

       let inhmvar = zip inhs mvars

       let eachthread x = 
             do result <- myread (fst x) $! anal
                putMVar (snd x) $! result
 
       mapM_ (forkIO.eachthread) inhmvar

       threadDelay 5000
                           
       arr <- mapM takeMVar mvars

       let total = foldr1 add_two_hist_array arr 

       print total
                                            
       let (histname,histenv,numbin) = histinfo anal

       c_histname <- newCString histname
       c_filename <- newCString (rootfile anal) 

       let totallist = elems total
           totallist' = map fromIntegral totallist
          
       print $ totallist'

       totalptr <- FA.newArray totallist'
  
       c_makehist c_histname c_filename 
                      (realToFrac.starting $ histenv) 
                      (realToFrac.ending $ histenv) 
                      (fromIntegral numbin) 
                      totalptr


       mapM_ hClose inhs



