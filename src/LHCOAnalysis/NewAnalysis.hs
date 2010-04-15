{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LHCOAnalysis.NewAnalysis where

import Debug.Trace

import LHCOAnalysis.PhysObj


import Control.Monad.ST
import Data.STRef
import Data.Array.Unboxed
import Data.Array.ST
import LHCOAnalysis.Analysis.Hist

import Data.Maybe


make_histogram :: HistEnv -> (PhyEventClassified -> Maybe Double) 
               -> [PhyEventClassified] -> UArray Int Int 
make_histogram histenv cut_and_analysis lst = 
    let result1 = map cut_and_analysis lst
        result2 = filterout_nothing result1
        run = do hist <- build_histogram histenv 
                 count <- newSTRef (0 :: Int)
                 mapM_ (add_hist_and_count histenv hist count) result2
                 freeze hist 
    in runST run 
  where filterout_nothing = (map fromJust) .  (filter isJust)  
        add_hist_and_count henv hist count item  
          = do add_to_histogram henv hist item
               modifySTRef count (+1) 
               readcount <- readSTRef count
               if readcount `mod` 1000 == 0
                  then trace (show readcount ++ ":") $ return ()
                  else return ()


