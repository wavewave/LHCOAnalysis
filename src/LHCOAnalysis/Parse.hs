{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Parse where

import Debug.Trace

import LHCOAnalysis
import LHCOAnalysis.PhysObj

--import Prelude hiding (map,foldl,null,filter,lines,(++))
-- import Data.List.Stream
import qualified Data.List.Split as SP

import Data.ByteString.Lex.Lazy.Double
import Data.ByteString.Internal  
import qualified Data.ByteString.Lazy.Char8 as B

--import Text.ParserCombinators.Parsec
--import Text.Parsec.ByteString.Lazy 
--import qualified Text.ParserCombinators.Parsec.Token as P
--import Text.ParserCombinators.Parsec.Language (haskellDef)

import Text.Parsec
import Text.Parsec.ByteString.Lazy
--import Text.Parsec.ByteString
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data Parsed = Comment B.ByteString | Zero Int | Nonzero (Int,EachObj) | Strange
              deriving (Show)


{--
run :: Parser a -> B.ByteString -> Either String a 
run p input = 
    case (parse p "" input) of 
      Left err -> Left $"parse error at " ++ show err ++ "\n" 
      Right x  -> Right x
--}

parsestr str1 = 
    let strlines = B.lines str1 
--       myparser = run lineparser 
        parsed = map classify_line strlines
--        filtered = map  $ filter (isRight) parsed
        commentfiltered = filter (not.isComment) parsed
        strangefiltered = filter (not.isStrange) commentfiltered
        splitted  = SP.splitWhen isZero strangefiltered 
        againfiltered = filter (not.null) splitted
        unnonzeroed = map (map unNonzero) againfiltered
        classified  = map constructPhysObjClass unnonzeroed 
    in  classified 

isRight (Left x)  = False 
isRight (Right x) = True

unRight (Right x) = x
unRight (Left x)  = undefined

isComment (Comment _) = True
isComment _ = False

isStrange Strange = True
isStrange _ = False

isZero (Zero _) = True
isZero _ = False

unNonzero (Nonzero x) = x
unNonzero _ = undefined

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
                                  otherwise -> parse_nonzero trimmed


parse_zero :: B.ByteString -> Parsed
parse_zero bstr = let bstrlst = B.split ' ' bstr
                      a1:a2:[] = take 2 $ filter (not . B.null) bstrlst
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
                                  , numtrkjet     = round ntrk }
                else EO $ ObjBJet { etaphiptbjet  = (eta,phi,pt)
                                  , numtrkbjet    = round ntrk }
           6 -> EO $ ObjMET { phiptmet = (phi,pt) }
                  
     
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
addEachObjtoClassified phyclass (idx,(EO jet@(ObjJet _ _))) 
  = phyclass { jetlst      = (idx,jet) : (jetlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO bjet@(ObjBJet _ _)))
  = phyclass { bjetlst     = (idx,bjet) : (bjetlst phyclass) }
addEachObjtoClassified phyclass (idx,(EO met'@(ObjMET _)))  
  = phyclass { met         = met' }

{--

lineparser = do  (try commentline >>= return.Comment )
                 <|> (try zeroline >>= return.Zero   )
                 <|> (try nonzeroline >>= return.Nonzero )
                 <|> return Strange
 





lexer   = P.makeTokenParser haskellDef    
            
integer = P.integer lexer
float   = P.float lexer
                         


--parseLHCO :: String -> Either ParseError [(Int,PhyEventClassified)]  
--parseLHCO inputfilename = parseFromFile lhcoFile inputfilename

{--
lhcoFile :: GenParser Char8 st [(Int,PhyEventClassified)]
lhcoFile = do many (try commentline) -- <?> "nocommentline"
              events <- many eventblock
              eof 
              return events

eventblock :: GenParser Char8 st (Int,PhyEventClassified)
eventblock = do eventnumber <- zeroline <?> "nozeroline"
                eventbody <- nonzerolines
                return (eventnumber,eventbody)
--}
                
zeroline :: GenParser B.ByteString st Int
zeroline = do empty
              char '0'
              empty
              eventnumber <- myint
--              let eventnumber = trace (show eventnumber') eventnumber'
--              let eventnumber = readsPrec eventnumberstr
              empty 
              myint
              empty
              return eventnumber
{--              
nonzerolines :: GenParser Char8 st PhyEventClassified               
nonzerolines = do unclassified <- many (try nonzeroline)
                  let classified = foldl addEachObjtoClassified zeroevent unclassified
                      sortedclassified = sortPhyEventC classified 
                  return classified
--}
                
nonzeroline :: GenParser B.ByteString st (Int,EachObj)
nonzeroline = do empty
                 objid <- myintnonzero
--                 let objid = trace (show objid') objid'
                 empty
                 typ  <- myint
                 empty 
                 eta <- myroughfloat
                 empty
                 phi <- myroughfloat
                 empty
                 pt <- myroughfloat
                 empty
                 jmas <- myroughfloat
                 empty 
                 ntrk <- myroughfloat
                 empty
                 btag <- myroughfloat
                 empty
                 hadem <- myroughfloat
                 empty
                 dum1 <- myroughfloat
                 empty
                 dum2 <- myroughfloat
                 empty
                 many (noneOf "\n\r") 
                 return $ (objid,  mkEachObj typ eta phi pt jmas ntrk  
                                             btag hadem dum1 dum2 )
                 



{--                 
integer' :: GenParser Char st Int                  
integer' = do result <- integer
              return $ fromInteger result
--}           
                 
myintnonzero :: GenParser B.ByteString st Int
myintnonzero = do i <- oneOf "123456789"
                  rest <- many (oneOf "0123456789")
                  return $ read (i:rest)

myint :: GenParser B.ByteString st Int
myint = do rest <- many (oneOf "0123456789")
           return $ read rest


myroughfloat :: GenParser B.ByteString st Double
myroughfloat = do result <- many (oneOf "+-0123456789.")
                  return $ read result
                  
commentline :: GenParser B.ByteString st B.ByteString
commentline = do empty 
                 char '#' 
                 many (noneOf "\n\r") 
                 return $ B.pack "COMMENT" 

{--
eol :: GenParser Char8 st String
eol = try (string "\n\r")
      <|> (string "\r\n")
      <|> string "\n"
      <|> string "\r"
--} 

empty = many (oneOf emptyletters)

emptyletters = [' ']
--parseLHCO 
--}