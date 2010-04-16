{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.CutSets where

import Debug.Trace
import System.IO.Unsafe

import LHCOAnalysis.PhysObj
import LHCOAnalysis.Utility

check :: Bool -> Maybe Bool
check True  = Just True
check False = Nothing
 


dilepton_inv_mass_veto_jet_bjet_met_cut :: (Double,Double,Double,Double) 
                                     -> PhyEventClassified ->Maybe Double
dilepton_inv_mass_veto_jet_bjet_met_cut (jveto,bjveto,leptoncut,metcut) p = 
  do  check (jet_veto jveto p)
      check (bjet_veto bjveto p)
      x <- check (met_cut metcut p)
      (ellst,mulst) <- hardest_leptons leptoncut p 
      (posmom,negmom) <- two_same_kind_opp_sign_leptons (ellst,mulst)
      return $ invmass0 posmom negmom
 

met_cut :: Double -> PhyEventClassified -> Bool 
met_cut valGeV p = let ettest = (snd.phiptmet.met) p 

                   in {-trace ("ettest = "++ show ettest ++ ": valGeV" ++ show valGeV) $-} ettest >= valGeV


bjet_veto :: Double -> PhyEventClassified -> Bool 
bjet_veto valGeV p = not ( numofobj BJet p >= 1
                           && (pt.snd.head) (bjetlst p) > valGeV )

jet_veto :: Double -> PhyEventClassified -> Bool
jet_veto valGeV p = not ( numofobj Jet p >= 1
                          && (pt.snd.head) (jetlst p) > valGeV )
       

hardest_leptons :: Double -> PhyEventClassified 
                -> Maybe ([PhyObj Electron], [PhyObj Muon])
hardest_leptons valGeV p =  
  do let ellst = electronlst p 
         mulst = muonlst p 
         criterion x = (pt (snd x)) > valGeV
         ellst' = map snd $ filter criterion ellst
         mulst' = map snd $ filter criterion mulst
     check $ length ellst' > 0 || length mulst' > 0 
     return (ellst',mulst')

opp_sign_first :: (ChargedObj a, MomObj a) => [a] 
                   -> Maybe (FourMomentum, FourMomentum)
opp_sign_first lst = 
  do check $ length lst >= 2
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
  let elresult = opp_sign_first ellst
      muresult = opp_sign_first mulst
  in  xor elresult muresult
 
electroncut :: PhyEventClassified -> Bool               
electroncut p = (numofobj Electron p < 1) 
