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

data DileptonInvMassJ12METVETO = DileptonInvMassJ12METVETO 
    {
      dilepton12_start :: Double
    , dilepton12_end   :: Double 
    , dilepton12_step  :: Double
    , dilepton12_numbin :: Int
    , dilepton12_filename :: String
    , dilepton12_histname :: String
    , dilepton12_func  :: AnalFunc
    }

data DileptonInvMassJHLMETVETO = DileptonInvMassJHLMETVETO 
    {
      dileptonhl_start :: Double
    , dileptonhl_end   :: Double 
    , dileptonhl_step  :: Double
    , dileptonhl_numbin :: Int
    , dileptonhl_filename :: String
    , dileptonhl_histname :: String
    , dileptonhl_func  :: AnalFunc
    }



data Dilepton_CosTH_JBJMETVETO = Dilepton_CosTH_JBJMETVETO 
    {
      dilepton_cosTH_start :: Double
    , dilepton_cosTH_end   :: Double 
    , dilepton_cosTH_step  :: Double
    , dilepton_cosTH_numbin :: Int
    , dilepton_cosTH_filename :: String
    , dilepton_cosTH_histname :: String
    , dilepton_cosTH_func  :: AnalFunc
    }


instance Show DileptonInvMassJBJMETVETO where
    show = dilepton_filename

instance Show DileptonInvMassJ12METVETO where
    show = dilepton12_filename


instance Show Dilepton_CosTH_JBJMETVETO where
    show = dilepton_cosTH_filename


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

-- | smart constructor of DileptonInvMassJ12METVETO
dileptonInvMassJ12METVETO :: Double -> Double -> Double 
                       -> String -> String -> (Double,Double,Double,Double) 
                       -> DileptonInvMassJ12METVETO
dileptonInvMassJ12METVETO s e st fn hn (j1v,j2v,lv,metcut)
    = DileptonInvMassJ12METVETO {
        dilepton12_start = s
      , dilepton12_end   = e
      , dilepton12_step  = st
      , dilepton12_numbin= floor $ (e-s)/st
      , dilepton12_filename = fn
      , dilepton12_histname = hn 
      , dilepton12_func = dilepton_inv_mass_veto_jet1_2_met_cut (j1v,j2v,lv,metcut)
      }

-- | smart constructor of DileptonInvMassJHLMETVETO
dileptonInvMassJHLMETVETO :: Double -> Double -> Double 
                       -> String -> String -> (Double,Double,Double,Double) 
                       -> DileptonInvMassJHLMETVETO
dileptonInvMassJHLMETVETO s e st fn hn (jh,jl,lv,metcut)
    = DileptonInvMassJHLMETVETO {
        dileptonhl_start = s
      , dileptonhl_end   = e
      , dileptonhl_step  = st
      , dileptonhl_numbin= floor $ (e-s)/st
      , dileptonhl_filename = fn
      , dileptonhl_histname = hn 
      , dileptonhl_func = dilepton_inv_mass_veto_jethl_met_cut (jh,jl,lv,metcut)
      }


-- | smart constructor of Dilepton_CosTH_JBJVETO
dilepton_CosTH_JBJMETVETO :: Double -> Double -> Double 
                          -> String -> String -> (Double,Double,Double,Double) 
                       -> Dilepton_CosTH_JBJMETVETO
dilepton_CosTH_JBJMETVETO s e st fn hn (jv,bjv,lv,metcut)
    = Dilepton_CosTH_JBJMETVETO {
        dilepton_cosTH_start = s
      , dilepton_cosTH_end   = e
      , dilepton_cosTH_step  = st
      , dilepton_cosTH_numbin= floor $ (e-s)/st
      , dilepton_cosTH_filename = fn
      , dilepton_cosTH_histname = hn 
      , dilepton_cosTH_func = dilepton_cosTH_veto_jet_bjet_met_cut (jv,bjv,lv,metcut)
      }


instance AnalysisTask DileptonInvMassJBJMETVETO where
    histinfo x = (dilepton_histname x, HistEnv s e st, nb)
        where s = dilepton_start x
              e = dilepton_end   x
              st= dilepton_step  x
              nb= dilepton_numbin x 

    rootfile = dilepton_filename
    analfunc = dilepton_func

instance AnalysisTask DileptonInvMassJ12METVETO where
    histinfo x = (dilepton12_histname x, HistEnv s e st, nb)
        where s = dilepton12_start x
              e = dilepton12_end   x
              st= dilepton12_step  x
              nb= dilepton12_numbin x 

    rootfile = dilepton12_filename
    analfunc = dilepton12_func

instance AnalysisTask DileptonInvMassJHLMETVETO where
    histinfo x = (dileptonhl_histname x, HistEnv s e st, nb)
        where s = dileptonhl_start x
              e = dileptonhl_end   x
              st= dileptonhl_step  x
              nb= dileptonhl_numbin x 

    rootfile = dileptonhl_filename
    analfunc = dileptonhl_func



instance AnalysisTask Dilepton_CosTH_JBJMETVETO where
    histinfo x = (dilepton_cosTH_histname x, HistEnv s e st, nb)
        where s = dilepton_cosTH_start x
              e = dilepton_cosTH_end   x
              st= dilepton_cosTH_step  x
              nb= dilepton_cosTH_numbin x 

    rootfile = dilepton_cosTH_filename
    analfunc = dilepton_cosTH_func



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


