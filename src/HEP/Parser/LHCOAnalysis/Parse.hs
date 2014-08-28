{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHCOAnalysis.Parse
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- parsing LHCO format files
--
-----------------------------------------------------------------------------

module HEP.Parser.LHCOAnalysis.Parse where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text 
import qualified Data.Text as T
--
import HEP.Parser.LHCOAnalysis.PhysObj

comment :: Parser T.Text
comment = do skipSpace
             char '#' 
             str <- takeTill isEndOfLine
             endOfLine
             return str

header :: Parser [T.Text]
header = many comment

zeroline :: Parser (Int,Int)
zeroline = do skipSpace
              char '0' 
              skipSpace
              n1 <- decimal
              skipSpace
              n2 <- decimal
              takeTill isEndOfLine
              endOfLine
              return (n1,n2)   

data LHCOLine = LHCOLine { lhco_lineno :: Int
                         , lhco_typ :: Int
                         , lhco_eta :: Double
                         , lhco_phi :: Double
                         , lhco_pt :: Double
                         , lhco_jmas :: Double
                         , lhco_ntrk :: Double
                         , lhco_btag :: Double
                         , lhco_hadem :: Double
                         , lhco_dum1 :: Double
                         , lhco_dum2 :: Double } 
              deriving (Show,Eq,Ord) 

nonzeroline :: Parser LHCOLine
nonzeroline = do no  <- skipSpace >> decimal
                 if no == 0 
                   then mzero 
                   else do
                     typ <- skipSpace >> decimal
                     eta <- skipSpace >> double
                     phi <- skipSpace >> double
                     pt  <- skipSpace >> double 
                     jmas <- skipSpace >> double
                     ntrk <- skipSpace >> double
                     btag <- skipSpace >> double
                     hadem <- skipSpace >> double
                     dum1 <- skipSpace >> double
                     dum2 <- skipSpace >> double
                     takeTill isEndOfLine >> endOfLine
                     return (LHCOLine no typ eta phi pt jmas ntrk btag hadem dum1 dum2)

event :: Parser PhyEventClassified -- ((Int,Int),[(Int,EachObj)])
event = do z <- zeroline
           nz <- many1 nonzeroline 
           let dat = (z,map ((,) <$> lhco_lineno <*> mkEachObj) nz)
               f ((i,_),x) = (i,x)
           (return . constructPhysObjClass . f) dat
             
lhco :: Parser [PhyEventClassified] -- [((Int,Int),[(Int,EachObj)])] -- [((Int,Int),[Text])] -- [PhyEventClassified] 
lhco = do header
          many event

-- |
mkEachObj :: LHCOLine -> EachObj
mkEachObj (LHCOLine _no typ eta phi pt jmas ntrk btag hadem dum1 dum2)
  = case typ of
           0 -> EO $ ObjPhoton   { etaphiptphoton = (eta,phi,pt) }
           1 -> EO $ ObjElectron { etaphiptelectron = (eta,phi,pt)
                                 , chargeelectron = ntrktoecharge ntrk }
           2 -> EO $ ObjMuon     { etaphiptmuon  = (eta,phi,pt)
                                 , chargemuon    = ntrktoecharge ntrk }
           3 -> EO $ ObjTau      { etaphipttau   = (eta,phi,pt)
                                 , chargetau     = ntrktoecharge ntrk 
                                 , prongtau      = ntrktotauprong ntrk }
           4 -> if btag == 0.0 
                then EO $ ObjJet  { etaphiptjet   = (eta,phi,pt)
                                  , mjet          = jmas
                                  , numtrkjet     = round ntrk }
                else EO $ ObjBJet { etaphiptbjet  = (eta,phi,pt)
                                  , mbjet         = jmas
                                  , numtrkbjet    = round ntrk }
           6 -> EO $ ObjMET { phiptmet = (phi,pt) }
           _ -> error "not a predefined type"

-- |
addEachObjtoClassified :: PhyEventClassified -> (Int,EachObj) -> PhyEventClassified
addEachObjtoClassified phyclass (idx,(EO photon@(ObjPhoton _))) 
  = phyclass { photonlst  = (idx,photon) : (photonlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO electron@(ObjElectron _ _))) 
  = phyclass { electronlst = (idx,electron) : (electronlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO muon@(ObjMuon _ _)))  
  = phyclass { muonlst     = (idx,muon) : (muonlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO tau@(ObjTau _ _ _)))
  = phyclass { taulst      = (idx,tau) : (taulst phyclass) }
addEachObjtoClassified phyclass (idx,(EO jet@(ObjJet _ _ _))) 
  = phyclass { jetlst      = (idx,jet) : (jetlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO bjet@(ObjBJet _ _ _)))
  = phyclass { bjetlst     = (idx,bjet) : (bjetlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO met'@(ObjMET _)))  
  = phyclass { met         = met' }

-- |
constructPhysObjClass :: (Int,[(Int, EachObj)]) -> PhyEventClassified
constructPhysObjClass (xid,lst) = 
    let classified = foldl addEachObjtoClassified (zeroevent {eventid = xid}) lst
        sortedclassified = sortPhyEventC classified 
    in  sortedclassified

