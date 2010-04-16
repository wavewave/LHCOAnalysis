{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}

module LHCOAnalysis.NewAnalysis where

import Debug.Trace

import LHCOAnalysis.PhysObj
import LHCOAnalysis.Analysis.CutSets

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

data DileptonInvMassJBJMETVETO = DileptonInvMassJBJMETVETO 
    {
      dilepton_start :: Double
    , dilepton_end   :: Double 
    , dilepton_step  :: Double
    , dilepton_numbin :: Int
    , dilepton_filename :: String
    , dilepton_histname :: String
    , dilepton_func  :: AnalFunc
    }

instance Show DileptonInvMassJBJMETVETO where
    show = dilepton_filename

-- | smart constructor of DileptonInvMassJBJVETO
dileptonInvMassJBJMETVETO :: Double -> Double -> Double 
                       -> String -> String -> (Double,Double,Double,Double) 
                       -> DileptonInvMassJBJMETVETO
dileptonInvMassJBJMETVETO s e st fn hn (jv,bjv,lv,metcut)
    = DileptonInvMassJBJMETVETO {
        dilepton_start = s
      , dilepton_end   = e
      , dilepton_step  = st
      , dilepton_numbin= floor $ (e-s)/st
      , dilepton_filename = fn
      , dilepton_histname = hn 
      , dilepton_func = dilepton_inv_mass_veto_jet_bjet_met_cut (jv,bjv,lv,metcut)
      }


instance AnalysisTask DileptonInvMassJBJMETVETO where
    histinfo x = (dilepton_histname x, HistEnv s e st, nb)
        where s = dilepton_start x
              e = dilepton_end   x
              st= dilepton_step  x
              nb= dilepton_numbin x 

    rootfile = dilepton_filename
    analfunc = dilepton_func


make_histogram :: HistEnv -> AnalFunc 
               -> [PhyEventClassified] -> UArray Int Int 
make_histogram !histenv !cut_and_analysis lst = 
    let --- result1 = map cut_and_analysis lst
        --- result2 = filterout_nothing result1
        run = do hist <- build_histogram histenv
                 totalcount <- newSTRef (0 :: Int)
                 passcount  <- newSTRef (0 :: Int)
                 mapM_  (myaction histenv hist totalcount passcount) lst 
                 freeze hist 
    in runST run 
  where -- filterout_nothing = (map fromJust) .  (filter isJust)  
        myaction henv hist totalcount passcount item
          = do modifySTRef totalcount (+1)
               readcount <- readSTRef totalcount
               
               if readcount `mod` 10000 == 0
                 then trace ("total event " ++ show readcount ++ ":") $ return ()
                 else return ()
                      
               case cut_and_analysis item of 
                 Nothing -> return ()
                 Just x  -> add_hist_and_count henv hist passcount x

        add_hist_and_count henv hist count item  
          = do add_to_histogram henv hist item
               modifySTRef count (+1) 
               readcount <- readSTRef count
               if readcount `mod` 1000 == 0
                  then trace ("passed event "++ show readcount ++ ":") $ return ()
                  else return ()


