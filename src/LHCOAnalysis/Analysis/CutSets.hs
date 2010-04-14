{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.CutSets where

import LHCOAnalysis.Utility
import LHCOAnalysis.PhysObj

import Control.Monad 
import Data.Maybe

instance Monad (Either String) where
  return v = Right v 
  fail   s = Left s 
  (Left s) >>= _ = Left s
  (Right v) >>= f = f v 


dilepton_inv_mass_sqr p = 
  do  -- apply 100 GeV jet veto cut
      jet_veto 1000 p                        
      -- select hardest lepton over 10 GeV    
      (ellst,mulst) <- hardest_leptons 10 p 
      -- select two opposite charged leptons with the same kind. 
      -- if there are more leptons than we need, cut
      (posmom,negmom) <- two_same_kind_opp_sign_leptons (ellst,mulst)
--      return (posmom,negmom)
      -- construct invariant mass square
      return $ 2.0 * (dot4 posmom negmom)
  
jet_veto valGeV p = if (numofobj Jet p >= 1) 
                        then if pt (snd.head $ jetlst p) > valGeV
                             then Left "Too high energy jet"
                             else Right p 
                        else Right p
                             
hardest_leptons valGeV p =  
  let ellst = electronlst p 
      mulst = muonlst p 
      criterion x = (pt (snd x)) > valGeV
      ellst' = map snd $ filter criterion ellst
      mulst' = map snd $ filter criterion mulst
  in  if (length ellst' == 0 && length mulst' == 0) 
      then Left "No hard lepton"
      else Right (ellst',mulst')

opp_sign_electron_exclusive ellst = 
  if (length ellst == 2)
  then let twomoms = do x <- first_positive $ ellst 
                        y <- first_negative $ ellst                  
                        return (fourmom x, fourmom y)
       in  maybe (Left "Not two opp sign electrons") Right twomoms  
  else Left "Not two opp sign electrons"         

opp_sign_muon_exclusive mulst =
  if (length mulst == 2)
  then let twomoms = do x <- first_positive $ mulst 
                        y <- first_negative $ mulst                  
                        return (fourmom x, fourmom y)
       in  maybe (Left "Not two opp sign muons") Right twomoms  
  else Left "Not two opp sign muons"
 
           
two_same_kind_opp_sign_leptons (ellst,mulst) =
  let elresult = opp_sign_electron_exclusive ellst
      muresult = opp_sign_muon_exclusive mulst
      either_xor (Left x) (Left y) = Left (x++y)  
      either_xor (Left x) (Right y) = Right y
      either_xor (Right x) (Left y) = Right x
      either_xor (Right x) (Right y) = Left "two+two" 
  in  either_xor elresult muresult 

                
electroncut p = (numofobj Electron p < 1) 
