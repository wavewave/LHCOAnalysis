{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.Hist where

import Control.Monad.ST
import Data.Array.ST

import Data.STRef


type Hist s = STUArray s Int 
type Hist3D s = STUArray s (Int,Int,Int)


data HistEnv = HistEnv { starting :: Double
                       , ending   :: Double 
                       , step     :: Double 
                       }

build_histogram :: Double -> Double -> Double -> ST s (HistEnv, Hist s Int)
build_histogram start end step = do let histenv = HistEnv start end step 
                                    hist <- build start end step
                                    return (histenv, hist)

add_to_histogram :: HistEnv -> Hist s Int -> Double -> ST s ()
add_to_histogram histenv hist val =
  let start' = starting histenv
      end'   = ending   histenv
      step'  = step     histenv
      bin'   = findbin start' end' step' val 
  in  if val < start' || val > end'
      then return ()
      else apply hist bin' (+1)


findbin :: Double -> Double -> Double -> Double -> Int 
findbin start end stp val = fromInteger $ floor $ (val - start) / stp  


--build :: Double -> Double -> Double -> ST s (Hist s Int) 
build start end step 
    = do let n = floor ((end - start) / step )
         hist <- newArray (1,n) 0 
         return hist

peek :: Hist s Int -> Int -> ST s Int
peek hist binnum = readArray hist binnum


poke :: Hist s Int -> Int -> Int -> ST s ()
poke hist binnum val = writeArray hist binnum val

apply :: Hist s Int -> Int -> (Int -> Int) -> ST s ()
apply hist binnum func = do val <- readArray hist binnum
                            writeArray hist binnum (func val)

{--
main = print $ runST $ do hist <- build 0 100.0 10.0
                          x <- peek hist 1 
                          poke hist 2 3
                          apply hist 4 (+1)
                          y <- peek hist 2
                          z <- peek hist 4
                          return $ (x,y,z) --}

build3d :: (Int,Int,Int) -> (Int,Int,Int) -> ST s (Hist3D s Int)
build3d (start1,start2,start3) (end1,end2,end3)
    = do hist <- newArray ((start1,start2,start3),(end1,end2,end3)) 0 
         return hist
