module LHCOAnalysis.FetchBinary where

import LHCOAnalysis.Analysis.CutSets
import LHCOAnalysis.PhysObj
import LHCOAnalysis.NewAnalysis
import LHCOAnalysis.Analysis.Hist
import LHCOAnalysis.ROOTApp


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

myread :: Handle -> IO (UArray Int Int)
myread inh = 
    do lst <- readbyte inh
       let start = 0
           end   = 1000
           step  = 20 
           numbin = 50 
           filename = "mytest.root"
           histname = "mytest"


           histenv    = HistEnv start end step 
           curranal   = dilepton_inv_mass_jet_veto_bjet_veto (100,5,40)
           sqrtresult = make_histogram histenv  curranal lst

       return sqrtresult
