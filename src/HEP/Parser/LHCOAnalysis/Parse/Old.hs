{-# LANGUAGE ScopedTypeVariables, BangPatterns, GADTs, FlexibleContexts, 
             FlexibleInstances, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHCOAnalysis.Parse.Old
-- Copyright   : (c) 2009,2010,2013,2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- parsing LHCO format files
--
-----------------------------------------------------------------------------

module HEP.Parser.LHCOAnalysis.Parse.Old where

import           Data.ByteString.Lex.Lazy.Double
import           Data.ByteString.Internal  
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.Split as SP
-- 
import HEP.Parser.LHCOAnalysis.PhysObj
-- 
import qualified HEP.Parser.LHCOAnalysis.Parse as LHCOP

data Parsed = Comment B.ByteString | Zero Int | Nonzero (Int,EachObj) | Strange
              deriving (Show)


type Merged = Parsed

parsestr :: B.ByteString -> [PhyEventClassified]
parsestr str1 = 
    let strlines = B.lines str1 
        parsed = map classify_line strlines
        commentfiltered = filter (not.isComment) parsed
        strangefiltered = filter (not.isStrange) commentfiltered
        splitted  = tail $ SP.split (SP.whenElt isZero) strangefiltered
       
        splitted' = mergeZeroParsed $ splitted 
                
        unnonzeroed = map unnonzeroaction splitted'
                      
        unnonzeroaction (x,xs) = (x, map unNonzero xs)
                      
        classified  = map LHCOP.constructPhysObjClass unnonzeroed 
    in  classified 





mergeZeroParsed :: [[Parsed]] -> [(Int,[Parsed])]
mergeZeroParsed [] = []
mergeZeroParsed (x0:x1:xs) = mergeZero x0 x1 : mergeZeroParsed xs

mergeZero [Zero evnum] x1 = (evnum,x1) 

--mergeZero x0 x1 = x1

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
                      
                  in Zero b2


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
     
