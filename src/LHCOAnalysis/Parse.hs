{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Parse where

import LHCOAnalysis.PhysObj

import qualified Data.List.Split as SP

import Data.ByteString.Lex.Lazy.Double
import Data.ByteString.Internal  
import qualified Data.ByteString.Lazy.Char8 as B


data Parsed = Comment B.ByteString | Zero Int | Nonzero (Int,EachObj) | Strange
              deriving (Show)


parsestr :: B.ByteString -> [PhyEventClassified]
parsestr str1 = 
    let strlines = B.lines str1 
        parsed = map classify_line strlines
        commentfiltered = filter (not.isComment) parsed
        strangefiltered = filter (not.isStrange) commentfiltered
        splitted  = SP.splitWhen isZero strangefiltered 
        againfiltered = filter (not.null) splitted
        unnonzeroed = map (map unNonzero) againfiltered
        classified  = map constructPhysObjClass unnonzeroed 
    in  classified 

isComment :: Parsed -> Bool 
isComment (Comment _) = True
isComment _ = False

isStrange :: Parsed -> Bool
isStrange Strange = True
isStrange _ = False

isZero :: Parsed -> Bool
isZero (Zero _) = True
isZero _ = False

unNonzero :: Parsed -> (Int, EachObj)
unNonzero (Nonzero x) = x
unNonzero _ = undefined

constructPhysObjClass :: [(Int, EachObj)] -> PhyEventClassified
constructPhysObjClass lst = 
    let classified = foldl addEachObjtoClassified zeroevent lst
        sortedclassified = sortPhyEventC classified 
    in  sortedclassified



classify_line :: B.ByteString -> Parsed 
classify_line bstr = let trimmed = B.dropWhile (isSpaceChar8) bstr
                     in if B.null trimmed 
                        then Strange 
                        else let h = B.head trimmed 
                             in case h of 
                                  '#' -> Comment trimmed 
                                  '0' -> parse_zero trimmed
                                  _   -> parse_nonzero trimmed


parse_zero :: B.ByteString -> Parsed
parse_zero bstr = let bstrlst = B.split ' ' bstr
                      _:a2:[] = take 2 $ filter (not . B.null) bstrlst
                      b2 = maybe 0 fst $ B.readInt a2
                  in  Zero b2


parse_nonzero :: B.ByteString -> Parsed
parse_nonzero bstr 
    = let bstrlst = B.split ' ' bstr
          a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:[]
              = take 11 $ filter (not . B.null) bstrlst
          objid = maybe 0 fst $ B.readInt a1 
          typ = maybe 0 fst $ B.readInt a2
          eta = maybe 0 fst $ readDouble a3
          phi = maybe 0 fst $ readDouble a4
          pt  = maybe 0 fst $ readDouble a5
          jmas= maybe 0 fst $ readDouble a6
          ntrk= maybe 0 fst $ readDouble a7
          btag= maybe 0 fst $ readDouble a8
          hadem= maybe 0 fst $ readDouble a9
          dum1= maybe 0 fst $ readDouble a10
          dum2= maybe 0 fst $ readDouble a11
      in  Nonzero (objid,  
                   mkEachObj typ eta phi pt jmas ntrk  
                             btag hadem dum1 dum2 )
                           
mkEachObj :: Int -> Double -> Double -> Double -> Double -> Double 
                 -> Double -> Double -> Double -> Double -> EachObj
mkEachObj typ eta phi pt jmas ntrk btag hadem dum1 dum2 
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
     
addEachObjtoClassified :: PhyEventClassified -> (Int,EachObj) 
                          -> PhyEventClassified
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

