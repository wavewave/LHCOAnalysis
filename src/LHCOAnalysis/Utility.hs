{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Utility where

-- | FourMomentum is a type synonym of (E,px,py,pz)
type FourMomentum = (Double,Double,Double,Double) 
 

energy :: (Double,Double,Double,Double) -> Double
energy (a,_,_,_) = a

px :: (Double,Double,Double,Double) -> Double
px     (_,a,_,_) = a

py :: (Double,Double,Double,Double) -> Double
py     (_,_,a,_) = a 

pz :: (Double,Double,Double,Double) -> Double
pz     (_,_,_,a) = a

dot4 :: FourMomentum -> FourMomentum -> Double 
dot4 (t1,x1,y1,z1) (t2,x2,y2,z2) = t1*t2 - x1*x2 - y1*y2 - z1*z2


cot :: Double -> Double
cot x = 1.0 / tan x

csc :: Double -> Double
csc x = 1.0 / sin x


etatocosth :: Double -> Double 
etatocosth et =  ( exp (2.0 * et) - 1 ) / (exp (2.0 * et) + 1 )

costhtoeta :: Double -> Double
costhtoeta costh =  0.5 * log ( ( 1 + costh ) / ( 1 - costh ) )  

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a

trd3 :: (a,b,c) -> c
trd3 (_,_,a) = a 


fourmomfrometaphipt :: (Double,Double,Double) -> FourMomentum
fourmomfrometaphipt etaphipt = (p0, p1, p2, p3 )
  where eta' = fst3 etaphipt 
        phi' = snd3 etaphipt
        pt'  = trd3 etaphipt
        costh = etatocosth eta'
        sinth = sqrt (1 - costh*costh)
        p1  = pt' * cos phi' 
        p2  = pt' * sin phi'
        p3  = pt' * costh / sinth 
        p0  = pt' / sinth



--lstprint :: (Show a, Show b) => [(a,b)] -> String                      
--lstprint lst = foldr1 f lst 
--    where f (a,b) str = show a ++ " " ++ show b ++ "\n" ++ str
