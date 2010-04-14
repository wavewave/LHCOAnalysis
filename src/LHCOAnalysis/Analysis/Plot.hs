{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.Plot where

-- import LHCOAnalysis 
--import LHCOAnalysis.PhysObj
-- import LHCOAnalysis.Parse

-- import Control.Monad 
-- import Control.Monad.StateT
-- import Data.Maybe

equalsteplist start step = let l = start : map (+step) l 
                           in  l 

invmdist scale mcusp mmax start end step 
   = let pointlist = takeWhile (<end) $ equalsteplist start step
         f m | (m>=0) && (m<mcusp)    = m * log (mmax/mcusp)
             | (m>=mcusp) && (m<mmax) = m * log (mmax/m) 
             | otherwise              = 0 
     in  map (\m->(m, scale * f m)) pointlist

