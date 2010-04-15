{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.CutSets where

import LHCOAnalysis.PhysObj
import LHCOAnalysis.Utility

check :: a -> Maybe a
check = return
 


dilepton_inv_mass :: PhyEventClassified -> Maybe Double
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
 
jet_veto :: Double -> PhyEventClassified -> Maybe PhyEventClassified
jet_veto valGeV p = 
    do check $ numofobj Jet p >= 1
       check $ (pt.snd.head) (jetlst p) > valGeV
       return p 

hardest_leptons :: Double -> PhyEventClassified 
                -> Maybe ([PhyObj Electron], [PhyObj Muon])
hardest_leptons valGeV p =  
  do let ellst = electronlst p 
         mulst = muonlst p 
         criterion x = (pt (snd x)) > valGeV
         ellst' = map snd $ filter criterion ellst
         mulst' = map snd $ filter criterion mulst
     check $ length ellst' == 0 && length mulst' == 0 
     return (ellst',mulst')

opp_sign_exclusive :: (ChargedObj a, MomObj a) => [a] 
                   -> Maybe (FourMomentum, FourMomentum)
opp_sign_exclusive lst = 
  do check $ length lst == 2
     x <- first_positive lst 
     y <- first_negative lst                  
     return (fourmom x, fourmom y)

xor :: Maybe a -> Maybe a -> Maybe a
xor (Just _) (Just _) = Nothing
xor Nothing (Just x)  = Just x
xor (Just x) Nothing  = Just x
xor Nothing Nothing   = Nothing
           
two_same_kind_opp_sign_leptons :: (ChargedObj a, MomObj a, 
                                   ChargedObj b, MomObj b) =>
                                  ([a],[b]) -> Maybe (FourMomentum,FourMomentum)
two_same_kind_opp_sign_leptons (ellst,mulst) =
  let elresult = opp_sign_exclusive ellst
      muresult = opp_sign_exclusive mulst
  in  xor elresult muresult
 
electroncut :: PhyEventClassified -> Bool               
electroncut p = (numofobj Electron p < 1) 
