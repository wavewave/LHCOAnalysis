{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LHCOAnalysis.Analysis where

import LHCOAnalysis.PhysObj
import Debug.Trace
import qualified Data.ListLike as LL 
import qualified Data.Iteratee as Iter

import HROOT

import Control.Monad
import Control.Monad.IO.Class

type EventCountIO a = Iter.IterateeG [] (PhyEventClassified) IO a

type EventAnalysisFunc = PhyEventClassified -> Maybe Double

iter_count_total_event :: EventCountIO Int 
iter_count_total_event = Iter.length

iter_count_marker :: Int -> Int -> EventCountIO ()
iter_count_marker num start = do 
  h <- Iter.peek 
  case h of 
    Nothing -> return ()
    Just _ -> if start `mod` num == 0 
                then do liftIO $ putStrLn (" data : " ++  show start)
                        iter_count_marker num (start+1)
                else iter_count_marker num (start+1)

iter_hist1 :: TH1F -> EventAnalysisFunc -> EventCountIO () 
iter_hist1 hist func = do 
  h <- Iter.peek
  case h of 
    Nothing -> return ()
    Just x -> do   
           let fx = func x
           case fx of
             Nothing -> do Iter.head 
                           iter_hist1 hist func
             Just val -> do liftIO $ do fillTH1F hist val
                                        -- putStrLn "one event passed"
                            Iter.head
                            iter_hist1 hist func


 

{-
import LHCOAnalysis 
import LHCOAnalysis.PhysObj
import LHCOAnalysis.Parse
import LHCOAnalysis.Analysis.CutSets

import Control.Monad 
-- import Control.Monad.StateT
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed.Mutable as MV


-- I know this is not good at all, but let's postpone vector solution to later. 
import qualified Data.IntMap as I   

data HistEnv = HistEnv { starting :: Double
                       , ending   :: Double 
                       , step     :: Double 
                       , numstep  :: Int }
               
type Histogram = MV.IOVector Double

construct_histenv :: Double -> Double -> Double -> HistEnv
construct_histenv start end step  = 
  let numstep = 1 + (floor $ (end - start) / step )
  in  HistEnv start end step numstep 
      
      
construct_histogram :: HistEnv -> IO Histogram
construct_histogram histenv = do MV.newWith (numstep histenv) 0.0 


add_to_histogram :: HistEnv -> Double -> Histogram -> IO Histogram
add_to_histogram histenv val hist =
  let start' = starting histenv
      end'   = ending   histenv
      step'  = step     histenv
      bin'   = findbin start' end' step' val 
  in  if val < start' || val > end'
      then return hist
      else MV.read hist bin' >>= \curr -> 
           MV.write hist bin' (curr+1.0) >>
           return hist      

testval histenv val = let start' = starting histenv 
                          end'   = ending   histenv 
                          step'  = step     histenv
                      in  if val < starting histenv || val > ending histenv
                          then Nothing
                          else Just $ findbin start' end' step' val 
unJust (Just x) = x 
unJust Nothing = 0



hist2list :: (HistEnv,Histogram) -> IO [(Double,Double)]                       
hist2list (histenv,hist) = do let start'  = starting histenv
                                  end'    = ending   histenv
                                  step'   = step     histenv 
                                  numstep' = numstep histenv 
                                  intlist = enumFromTo 0 (numstep'-1)
                                  f x = findval start' end' step' x 
                                  vallist = map f intlist
                              occurlist <- mapM (\x -> MV.read hist x) intlist 
                              return $ zip vallist occurlist 
              
       
       
findbin :: Double -> Double -> Double -> Double -> Int 
findbin start end stp val = fromInteger $ floor $ (val - start) / stp  

findval :: Double -> Double -> Double -> Int -> Double 
findval start end stp bin = start + stp * (fromIntegral bin) 
                  




lstprint :: (Show a, Show b) => [(a,b)] -> String                      
lstprint lst = concat $ map (\(a,b) -> show a ++ " " ++ show b ++ "\n") lst


make_binary_file ifilename ofilename = 
  do inpStr <- readFile ifilename
     let result = parseLHCO inpStr
         rightresult = case result of 
                         Right a -> a
         v = B.encode $ rightresult 
     BS.writeFile ofilename v

binary_to_data_structure :: String -> IO [(Int, PhyEventClassified)]      
binary_to_data_structure ifilename = 
  BS.readFile ifilename >>= return . B.decode 



-- type EventAnalysisState st  = S.StateT st (Either String PhyEventClassified)

-- mycut' :: EventAnalysisState st 

makehist :: Double -> Double -> Double -> [Double] -> IO (HistEnv, Histogram)
makehist start end step vallist = 
  do let histenv = construct_histenv start end step 
     hist <- construct_histogram histenv 
     hist' <- F.foldrM (add_to_histogram histenv) hist vallist
     return (histenv, hist')

makefinallist :: Double -> Double -> Double -> [Double] -> IO String
                 {-- IO [(Double,Double)] --}
makefinallist start end step lst  = 
  makehist start end step lst >>= \(histenv,hist) ->
  hist2list (histenv,hist) >>= \finallist ->
  return $ lstprint finallist

process_with_cut cut_and_analysis treeresult = 
  let -- resultphyevents = map snd treeresult 
      resultinvmasssqr = map cut_and_analysis treeresult
  in map uncoverright $ filter sortoutright resultinvmasssqr
        

sortoutright (Left x) = False
sortoutright (Right x) = True

uncoverright (Right x) = x

-}