module LHCOAnalysis.MakeBinary where

import LHCOAnalysis
import LHCOAnalysis.Utility
import LHCOAnalysis.Analysis.CutSets
import LHCOAnalysis.Parse
import LHCOAnalysis.PhysObj
import LHCOAnalysis.NewAnalysis

import LHCOAnalysis.Analysis.Hist
--import LHCOAnalysis.Analysis.Plot


import Data.Array.Unboxed

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Binary as Bi
import System.IO

import Data.Int

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad.State.Lazy
import qualified Data.Binary.Get as G

import Data.IORef


makebinary inputfilename outputfilename 
    = do putStrLn inputfilename
         putStrLn $ "output  = " ++ outputfilename
         inh <- openFile inputfilename ReadMode
         outh <- openFile outputfilename  WriteMode

         str <- B.hGetContents inh
         let parsed = parsestr str
             encodedbs = map Bi.encode parsed

         count <- newIORef (0::Int )

         let myaction str = do B.hPut outh str 
                               modifyIORef count (+1)
                               countnum <- readIORef count
                               if (countnum `mod` 1000) == 0
                                 then putStrLn (outputfilename ++ ":" ++ (show countnum))
                                 else (return ())

         mapM_ myaction encodedbs

         hClose inh 
         hClose outh

