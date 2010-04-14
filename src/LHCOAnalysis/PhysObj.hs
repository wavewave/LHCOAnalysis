{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LHCOAnalysis.PhysObj where

import LHCOAnalysis.Utility

--import Data.ByteString.Lazy 
-- import Data.ByteString.Lazy hiding (reverse)
import Data.Binary 
import Data.List (sortBy)
--  phantom type for the object type

--class ObjTag a 

data Photon 
data Electron
data Muon
data Tau
data Jet
data BJet
data MET           

--instance ObjTag Photon
--instance ObjTag Electron
--instance ObjTag Muon
--instance ObjTag Jet
--instance ObjTag BJet
--instance ObjTag MET

data ObjTag a where
  Photon   :: ObjTag Photon 
  Electron :: ObjTag Electron 
  Muon     :: ObjTag Muon 
  Tau      :: ObjTag Tau  
  Jet      :: ObjTag Jet
  BJet     :: ObjTag BJet
  MET      :: ObjTag MET



cot x = 1.0 / tan x
csc x = 1.0 / sin x


data TauProng = Prong1 | Prong3
data ECharge  = CPlus  | CMinus

ntrktoecharge ntrk = if ntrk > 0 then CPlus else CMinus
ntrktotauprong ntrk = let absntrk = abs ntrk
                      in if ntrk > 0.9 && ntrk < 1.1  
                         then Prong1
                         else Prong3

data PhyObj a where   
  ObjPhoton   :: { etaphiptphoton :: (Double, Double, Double) 
                 } -> PhyObj Photon
  ObjElectron :: { etaphiptelectron :: (Double, Double, Double)
                 , chargeelectron :: ECharge
                 } -> PhyObj Electron 
  ObjMuon     :: { etaphiptmuon :: (Double, Double, Double)
                 , chargemuon :: ECharge
                 } -> PhyObj Muon 
  ObjTau      :: { etaphipttau :: (Double, Double, Double)
                 , chargetau   :: ECharge
                 , prongtau    :: TauProng 
                 } -> PhyObj Tau 
  ObjJet      :: { etaphiptjet :: (Double, Double, Double)
                 , numtrkjet   :: Int
                 } -> PhyObj Jet
  ObjBJet     :: { etaphiptbjet :: (Double, Double, Double)
                 , numtrkbjet   :: Int
                 } -> PhyObj BJet
  ObjMET      :: { phiptmet    :: (Double,Double)
                 } -> PhyObj MET
                 

class MomObj a where
  fourmom :: a -> FourMomentum 
  eta     :: a -> Double
  phi     :: a -> Double
  pt      :: a -> Double

ptcompare :: MomObj a => a -> a -> Ordering
ptcompare x y = compare (pt x) (pt y)


class ChargedObj a where
  charge  :: a -> Int

headsafe []     = Nothing
headsafe (x:xs) = Just x 

first_positive :: (ChargedObj a) => [a] -> Maybe a 
first_positive lst = headsafe $ dropWhile (\x->(charge x < 0)) lst

first_negative :: (ChargedObj a) => [a] -> Maybe a 
first_negative lst = headsafe $ dropWhile (\x->(charge x > 0)) lst 

class MultiTrkObj a where
  numoftrk :: a -> Int





--  Existential type for heterotic container for different objects.      
                 
--data EachObj = forall a. (Show (PhyObj a), Binary (PhyObj a)) => EO (PhyObj a)
data EachObj where
  EO :: (Show (PhyObj a), Binary (PhyObj a)) => PhyObj a -> EachObj

type PhyEvent = [(Int,EachObj)] 

data PhyEventClassified = PhyEventClassified 
                         { photonlst   :: [(Int,(PhyObj Photon))], 
                           electronlst :: [(Int,(PhyObj Electron))],
                           muonlst     :: [(Int,(PhyObj Muon))], 
                           taulst      :: [(Int,(PhyObj Tau))], 
                           jetlst      :: [(Int,(PhyObj Jet))],
                           bjetlst     :: [(Int,(PhyObj BJet))],
                           met         :: PhyObj MET }

zeroevent = PhyEventClassified [] [] [] [] [] [] (ObjMET (0,0))

-- | sort PhysEventClassfied with PT ordering
sortPhyEventC :: PhyEventClassified -> PhyEventClassified
sortPhyEventC p = let phl = photonlst p 
                      ell = electronlst p 
                      mul = muonlst p 
                      tal = taulst p 
                      jel = jetlst p 
                      bjl = bjetlst p 
                      met'= met p 
                      cmpfun (i,x) (j,y) = ptcompare x y 
                      phl' = reverse $ sortBy cmpfun phl
                      ell' = reverse $ sortBy cmpfun ell
                      mul' = reverse $ sortBy cmpfun mul
                      tal' = reverse $ sortBy cmpfun tal
                      jel' = reverse $ sortBy cmpfun jel
                      bjl' = reverse $ sortBy cmpfun bjl
                  in  PhyEventClassified phl' ell' mul' tal' jel' bjl' met'

-- | num of object in one event
numofobj :: ObjTag a -> PhyEventClassified -> Int 
numofobj Photon p   = Prelude.length (photonlst p)
numofobj Electron p = Prelude.length (electronlst p)
numofobj Muon p     = Prelude.length (muonlst p)
numofobj Tau p      = Prelude.length (taulst p)
numofobj Jet p      = Prelude.length (jetlst p)
numofobj BJet p     = Prelude.length (bjetlst p)
numofobj MET p      = 1


--  From here on, I am defining the instances. 
instance Binary (TauProng) where
  put (Prong1) = putWord8 1
  put (Prong3) = putWord8 3
  get = do tag <- getWord8
           return $ case tag of 
                      1 -> Prong1
                      3 -> Prong3

instance Binary (ECharge) where
  put (CPlus)  = putWord8 1
  put (CMinus) = putWord8 (-1)
  get = do tag <- getWord8
           return $ case tag of 
                      1  -> CPlus
                      -1 -> CMinus

instance Binary (PhyObj Photon) where
  put (ObjPhoton x) = putWord8 100 >> put x 
  get = do tag <- getWord8
           x   <- get
           return (ObjPhoton x)
           
instance Binary (PhyObj Electron) where
  put (ObjElectron x y) = putWord8 101 >> put x >> put y 
  get = do tag <- getWord8
           x <- get 
           y <- get 
           return (ObjElectron x y)

instance Binary (PhyObj Muon) where
  put (ObjMuon x y) = putWord8 102 >> put x >> put y 
  get = do tag <- getWord8
           x <- get
           y <- get
           return (ObjMuon x y)

instance Binary (PhyObj Tau) where
  put (ObjTau x y z) = putWord8 103 >> put x >> put y >> put z 
  get = do tag <- getWord8
           x <- get 
           y <- get
           z <- get
           return (ObjTau x y z)

instance Binary (PhyObj Jet) where
  put (ObjJet x y) = putWord8 104 >> put x >> put y 
  get = do tag <- getWord8
           x <- get 
           y <- get 
           return (ObjJet x y)

instance Binary (PhyObj BJet) where
  put (ObjBJet x y) = putWord8 105 >> put x >> put y 
  get = do tag <- getWord8
           x <- get            
           y <- get 
           return (ObjBJet x y)

instance Binary (PhyObj MET) where
  put (ObjMET x) = putWord8 106 >> put x 
  get = do tag <- getWord8
           x <- get            
           return (ObjMET x)
           
           

instance Show (PhyObj Photon) where
  show x = "(photon)"
  
instance Show (PhyObj Electron) where
  show x = "(electron)"

instance Show (PhyObj Muon) where
  show x = "(muon)"

instance Show (PhyObj Tau) where
  show x = "(tau)"

instance Show (PhyObj Jet) where
  show x = "(jet)"

instance Show (PhyObj BJet) where
  show x = "(bjet)"

instance Show (PhyObj MET) where
  show x = "(met)"


instance Binary EachObj where
  put (EO x) = put x 
--  get = getWord8 >> \tag -> get >>= \(x :: (Show (PhyObj a), Binary (PhyObj 
{--  get = getWord8 >>= \tag ->
        case tag of  
          100 -> get >>= \(x :: PhyObj Photon) -> 
                 return $ EO x 
          101 -> get >>= \(x :: PhyObj Electron) ->
                 return $ EO x
          102 -> get >>= \(x :: PhyObj Muon) ->
                 return $ EO x 
          103 -> get >>= \(x :: PhyObj Tau) ->
                 return $ EO x
          104 -> get >>= \(x :: PhyObj Jet) ->
                 return $ EO x
          105 -> get >>= \(x :: PhyObj BJet) ->
                 return $ EO x
          106 -> get >>= \(x :: PhyObj MET) ->
                 return $ EO x --}
  get = do tag <- getWord8 
           case tag of
             100 -> do (x :: PhyObj Photon) <- get
                       return $ EO x
             101 -> do (x :: PhyObj Electron) <- get
                       return $ EO x
             102 -> do (x :: PhyObj Muon) <- get
                       return $ EO x
             103 -> do (x :: PhyObj Tau) <- get
                       return $ EO x
             104 -> do (x :: PhyObj Jet) <- get
                       return $ EO x
             105 -> do (x :: PhyObj BJet) <- get
                       return $ EO x
             106 -> do (x :: PhyObj MET) <- get
                       return $ EO x

{--
instance Binary EachObj where
  put (EO x) = putWord8 200 >> put x 
--  get = getWord8 >> \tag -> get >>= \(x :: (Show (PhyObj a), Binary (PhyObj 
  get = do tag <- getWord8 
     -- (x :: (Show (PhyObj a), Binary (PhyObj a)) => PhyObj a) <- get
           x <- get
           return $ (EO (x :: (Show (PhyObj a), Binary (PhyObj a)) => PhyObj a) )
--}

instance Show EachObj where
  show (EO x) = show x

instance Binary PhyEventClassified where
  put x = putWord8 300 >> put (photonlst x)
                       >> put (electronlst x)
                       >> put (muonlst x)
                       >> put (taulst x )
                       >> put (jetlst x)
                       >> put (bjetlst x)
                       >> put (met x)
  get = do tag <- getWord8 
           x0 <- get 
           x1 <- get
           x2 <- get 
           x3 <- get
           x4 <- get
           x5 <- get
           x6 <- get
           return $ PhyEventClassified x0 x1 x2 x3 x4 x5 x6

instance Show PhyEventClassified where
  show (PhyEventClassified x0 x1 x2 x3 x4 x5 x6) = 
    show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6


etatocosth et =  ( exp (2.0 * et) - 1 ) / (exp (2.0 * et) + 1 )
costhtoeta costh =  0.5 * log ( ( 1 + costh ) / ( 1 - costh ) )  

fst3 (a,_,_) = a
snd3 (_,a,_) = a
trd3 (_,_,a) = a 

fourmomfrometaphipt etaphipt = (p0, p1, p2, p3 )
  where eta = fst3 etaphipt 
        phi = snd3 etaphipt
        pt  = trd3 etaphipt
        costh = etatocosth eta
        sinth = sqrt (1 - costh*costh)
        p1  = pt * cos phi 
        p2  = pt * sin phi
        p3  = pt * costh / sinth 
        p0  = pt / sinth

-- Charged Object  

instance ChargedObj (PhyObj Electron) where
  charge x = case chargeelectron x of
               CPlus  -> 1 
               CMinus -> -1

instance ChargedObj (PhyObj Muon) where
  charge x = case chargemuon x of
               CPlus  -> 1 
               CMinus -> -1
               
instance ChargedObj (PhyObj Tau) where
  charge x = case chargetau x of 
               CPlus  -> 1
               CMinus -> -1 

instance MultiTrkObj (PhyObj Tau) where
  numoftrk x = case (prongtau x) of 
                 Prong1 -> 1 
                 Prong3 -> 3

instance MultiTrkObj (PhyObj Jet) where
  numoftrk = numtrkjet

instance MultiTrkObj (PhyObj BJet) where
  numoftrk = numtrkbjet



instance MomObj (PhyObj Photon) where
  fourmom = fourmomfrometaphipt . etaphiptphoton
  eta = fst3 . etaphiptphoton   
  phi = snd3 . etaphiptphoton
  pt  = trd3 . etaphiptphoton
  
instance MomObj (PhyObj Electron) where
  fourmom = fourmomfrometaphipt . etaphiptelectron
  eta = fst3 . etaphiptelectron
  phi = snd3 . etaphiptelectron
  pt  = trd3 . etaphiptelectron
  
instance MomObj (PhyObj Muon) where
  fourmom = fourmomfrometaphipt . etaphiptmuon
  eta = fst3 . etaphiptmuon
  phi = snd3 . etaphiptmuon
  pt  = trd3 . etaphiptmuon
  
instance MomObj (PhyObj Tau) where
  fourmom = fourmomfrometaphipt . etaphipttau
  eta = fst3 . etaphipttau
  phi = snd3 . etaphipttau
  pt  = trd3 . etaphipttau

instance MomObj (PhyObj Jet) where
  fourmom = fourmomfrometaphipt . etaphiptjet
  eta = fst3 . etaphiptjet
  phi = snd3 . etaphiptjet
  pt  = trd3 . etaphiptjet

instance MomObj (PhyObj BJet) where
  fourmom = fourmomfrometaphipt . etaphiptbjet
  eta = fst3 . etaphiptbjet
  phi = snd3 . etaphiptbjet
  pt  = trd3 . etaphiptbjet

  
  
