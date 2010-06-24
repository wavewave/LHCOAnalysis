{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE BangPatterns #-}

module LHCOAnalysis.FetchBinary where

import LHCOAnalysis.PhysObj
import LHCOAnalysis.NewAnalysis
import LHCOAnalysis.Utility

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Binary as Bi
import System.IO

import Data.Int
import Data.Array.Unboxed

import Control.Monad.State.Lazy
import qualified Data.Binary.Get as G



onefetch :: (Bi.Binary a) => Bi.Get (Bool,a)
onefetch = do boolresult <- G.isEmpty
              case boolresult of
                True  -> return (False,undefined)
                False -> do result <- Bi.get
                            return (True,result)

onefetchNonIO :: (Bi.Binary a) => State (Int64,B.ByteString) (Bool,a)
onefetchNonIO = do (byte,bytestr) <- get
                   let (result,bytestr',byte') = G.runGetState onefetch bytestr byte
                   put (byte',bytestr')
--                liftIO $ print byte'
                   return $ result

readbyte :: Handle -> IO [PhyEventClassified]
readbyte inh = do bytecontent <- B.hGetContents inh
                  let listfetchNonIO = sequence $ repeat onefetchNonIO
                      lst = evalState listfetchNonIO (0,bytecontent)
         --         print item 
                  return $ map snd $ takeWhile fst lst 
          --        lst <- sequence $ repeat onefetchIO 
         ---         return $ map snd $ takeWhile fst lst 


myread :: (AnalysisTask a) => Handle -> a -> IO (UArray Int Int)
myread inh !anal = 
    do lst <- readbyte inh
       let result = make_histogram (snd3.histinfo $ anal) (analfunc anal) lst

       return result
