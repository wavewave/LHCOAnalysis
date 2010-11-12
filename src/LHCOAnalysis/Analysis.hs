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

type EventCountIO a = Iter.Iteratee [PhyEventClassified] IO a

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
                        Iter.head
                        iter_count_marker num (start+1)
                else do Iter.head
                        iter_count_marker num (start+1)

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
             Just val -> do liftIO $ do fill hist val
                                        -- putStrLn "one event passed"
                            Iter.head
                            iter_hist1 hist func

iter_hist2 :: TH2F -> 
              (PhyEventClassified -> Maybe (Double,Double)) 
              -> EventCountIO () 
iter_hist2 hist func = do 
  h <- Iter.peek
  case h of 
    Nothing -> return ()
    Just x -> do   
           let fx = func x
           case fx of
             Nothing -> do Iter.head 
                           iter_hist2 hist func
             Just val -> do liftIO $ do fill hist val
                                        -- putStrLn "one event passed"
                            Iter.head
                            iter_hist2 hist func

 
