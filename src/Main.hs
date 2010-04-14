module Main where

import LHCOAnalysis
import LHCOAnalysis.Utility
import LHCOAnalysis.Analysis.CutSets
import LHCOAnalysis.Parse
import LHCOAnalysis.PhysObj
import LHCOAnalysis.NewAnalysis

import LHCOAnalysis.Analysis.Hist

import LHCOAnalysis.MakeBinary
import LHCOAnalysis.FetchBinary


import Data.Array.Unboxed

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Binary as Bi
import System.IO

import Data.Int

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad.State.Lazy
import qualified Data.Binary.Get as G

import Data.IORef


main = do inh1 <- openFile "ttbar1-clean.binary" ReadMode
          inh2 <- openFile "ttbar2-clean.binary" ReadMode
          inh3 <- openFile "ttbar3-clean.binary" ReadMode
          inh4 <- openFile "ttbar4-clean.binary" ReadMode
          inh5 <- openFile "ttbar5-clean.binary" ReadMode

          mvar1 <- newEmptyMVar
          mvar2 <- newEmptyMVar
          mvar3 <- newEmptyMVar
          mvar4 <- newEmptyMVar
          mvar5 <- newEmptyMVar

          
          forkIO $ do result1 <- myread inh1
                      putMVar mvar1 $! result1
                  
          forkIO $ do result2 <- myread inh2
                      putMVar mvar2 $! result2

          forkIO $ do result3 <- myread inh3
                      putMVar mvar3 $! result3

          forkIO $ do result4 <- myread inh4
                      putMVar mvar4 $! result4

          forkIO $ do result5 <- myread inh5
                      putMVar mvar5 $! result5



          arr1 <- readMVar mvar1
          arr2 <- readMVar mvar2
          arr3 <- readMVar mvar3
          arr4 <- readMVar mvar4
          arr5 <- readMVar mvar5



          print $ arr1 `addtwohist` arr2 `addtwohist` arr3 `addtwohist` arr4 `addtwohist` arr5

          hClose inh1
          hClose inh2
          hClose inh3
          hClose inh4
          hClose inh5


{--

                     writeIORef result2 arr;
--                     print result2


          result1 <- newIORef (undefined :: UArray Int Int )
          result2 <- newIORef (undefined :: UArray Int Int )
          arr1 <- readIORef result1
          arr2 <- readIORef result2

                     writeIORef result1 arr;
--                     print result1

--          print $ result1 `addtwohist` result2

--}