{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Utility where

--import LHCOAnalysis.PhysObj 

-- | FourMomentum is a type synonym of (E,px,py,pz)
type FourMomentum = (Double,Double,Double,Double) 
 
energy (a,_,_,_) = a
px     (_,a,_,_) = a
py     (_,_,a,_) = a 
pz     (_,_,_,a) = a

dot4 :: FourMomentum -> FourMomentum -> Double 
dot4 (t1,x1,y1,z1) (t2,x2,y2,z2) = t1*t2 - x1*x2 - y1*y2 - z1*z2


--lstprint :: (Show a, Show b) => [(a,b)] -> String                      
--lstprint lst = foldr1 f lst 
--    where f (a,b) str = show a ++ " " ++ show b ++ "\n" ++ str
