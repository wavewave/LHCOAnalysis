{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.CutSets where

import LHCOAnalysis.Utility
import LHCOAnalysis.PhysObj

import Control.Monad 
import Data.Maybe

--instance Monad (Either String) where
--  return v = Right v -
--  fail   s = Left s 
--  (Left s) >>= _ = Left s
--  (Right v) >>= f = f v 


dilepton_inv_mass p = 
  do  -- apply 100 GeV jet veto cut
      jet_veto 1000 p                        
      -- select hardest lepton over 10 GeV    
      (ellst,mulst) <- hardest_leptons 10 p 
      -- select two opposite charged leptons with the same kind. 
      -- if there are more leptons than we need, cut
      (posmom,negmom) <- two_same_kind_opp_sign_leptons (ellst,mulst)
--      return (posmom,negmom)
      -- construct invariant mass square
      return $ sqrt (2.0 * (dot4 posmom negmom))
 
check = return
 
jet_veto valGeV p = 
    do check $ numofobj Jet p >= 1
       check $ (pt.snd.head) (jetlst p) > valGeV
       return p 
                             
hardest_leptons valGeV p =  
  do let ellst = electronlst p 
         mulst = muonlst p 
         criterion x = (pt (snd x)) > valGeV
         ellst' = map snd $ filter criterion ellst
         mulst' = map snd $ filter criterion mulst
     check $ length ellst' == 0 && length mulst' == 0 
     return (ellst',mulst')

opp_sign_electron_exclusive ellst = 
  do check $ length ellst == 2
     x <- first_positive ellst 
     y <- first_negative ellst                  
     return (fourmom x, fourmom y)

opp_sign_muon_exclusive mulst =
  do check $ length mulst == 2
     x <- first_positive $ mulst 
     y <- first_negative $ mulst                  
     return (fourmom x, fourmom y)

xor :: Maybe a -> Maybe a -> Maybe a
xor (Just x) (Just y) = Nothing
xor Nothing (Just x)  = Just x
xor (Just x) Nothing  = Just x
xor Nothing Nothing   = Nothing
           
two_same_kind_opp_sign_leptons (ellst,mulst) =
  let elresult = opp_sign_electron_exclusive ellst
      muresult = opp_sign_muon_exclusive mulst
  in  xor elresult muresult
                
electroncut p = (numofobj Electron p < 1) 
