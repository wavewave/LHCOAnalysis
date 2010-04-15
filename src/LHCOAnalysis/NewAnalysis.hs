{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}

module LHCOAnalysis.NewAnalysis where

import Debug.Trace

import LHCOAnalysis.PhysObj


import Control.Monad.ST
import Data.STRef
import Data.Array.Unboxed
import Data.Array.ST
import LHCOAnalysis.Analysis.Hist

import Data.Maybe

class AnalysisTask a where
    histinfo :: a -> (String,HistEnv,Int)
    rootfile :: a -> String
    analfunc :: a -> AnalFunc

type AnalFunc = PhyEventClassified -> Maybe Double

data DileptonInvMassJBJVETO = DileptonInvMassJBJVETO 
    {
      dilepton_start :: Double
    , dilepton_end   :: Double 
    , dilepton_step  :: Double
    , dilepton_numbin :: Int
    , dilepton_filename :: String
    , dilepton_histname :: String
    , dilepton_func  :: AnalFunc
    }

-- | smart constructor of DileptonInvMassJBJVETO
dileptonInvMassJBJVETO :: Double -> Double -> Double 
                       -> String -> String -> AnalFunc
                       -> DileptonInvMassJBJVETO
dileptonInvMassJBJVETO s e st fn hn func
    = DileptonInvMassJBJVETO {
        dilepton_start = s
      , dilepton_end   = e
      , dilepton_step  = st
      , dilepton_numbin= floor $ (e-s)/st
      , dilepton_filename = fn
      , dilepton_histname = hn 
      , dilepton_func = func
      }


instance AnalysisTask DileptonInvMassJBJVETO where
    histinfo x = (dilepton_filename x, HistEnv s e st, nb)
        where s = dilepton_start x
              e = dilepton_end   x
              st= dilepton_step  x
              nb= dilepton_numbin x 

    rootfile = dilepton_filename
    analfunc = dilepton_func


make_histogram :: HistEnv -> AnalFunc 
               -> [PhyEventClassified] -> UArray Int Int 
make_histogram !histenv !cut_and_analysis lst = 
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


