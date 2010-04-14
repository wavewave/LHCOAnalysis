{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LHCOAnalysis.NewAnalysis where

import Debug.Trace

import LHCOAnalysis 
import LHCOAnalysis.PhysObj
--import LHCOAnalysis.Parse
import LHCOAnalysis.Analysis.CutSets


import Control.Monad.ST
import Data.STRef
import Data.Array.Unboxed
import Data.Array.ST
import LHCOAnalysis.Analysis.Hist

import Control.Monad 
-- import Control.Monad.StateT
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed.Mutable as MV


-- I know this is not good at all, but let's postpone vector solution to later. 
import qualified Data.IntMap as I   

lstprint :: (Show a, Show b) => [(a,b)] -> String                      
lstprint lst = concat $ map (\(a,b) -> show a ++ " " ++ show b ++ "\n") lst


process_with_cut :: (PhyEventClassified->Either a Double )-> [PhyEventClassified] -> UArray Int Int 
process_with_cut cut_and_analysis treeresult = 
  let -- resultphyevents = map snd treeresult 
      resultinvmasssqr = map cut_and_analysis treeresult
      result = map uncoverright $ filter sortoutright resultinvmasssqr

      myaction histenv hist count item  
          = do add_to_histogram histenv hist item
               modifySTRef count (+1) 
               readcount <- readSTRef count
               if readcount `mod` 1000 == 0
                  then trace (show readcount ++ ":") $ return ()
                  else return ()

      run = do (histenv,hist) <- build_histogram 0 1000 20 
               count <- newSTRef (0 :: Int)
               mapM_ (myaction histenv hist count) result
               freeze hist -- :: ST s (UArray Int Int)
      (answer :: UArray Int Int) = runST run
  in {- assocs -} answer 


sortoutright (Left x) = False
sortoutright (Right x) = True

uncoverright (Right x) = x





addtwohist :: UArray Int Int -> UArray Int Int -> UArray Int Int
addtwohist arr1 arr2 
    = let bnds = bounds arr1
          assoc1 = assocs arr1
          assoc2 = assocs arr2
      in accumArray (+) 0 bnds (assoc1 ++ assoc2)
 --   = let idx = indices arr1
 --         addfunc i = (i, arr1 !! i + arr2 !! i)
 --         lstmap = map addfunc idx
 --     in 