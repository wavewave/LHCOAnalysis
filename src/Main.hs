import LHCOAnalysis
import LHCOAnalysis.Utility
import LHCOAnalysis.Analysis.CutSets
import LHCOAnalysis.Parse
import LHCOAnalysis.PhysObj
import LHCOAnalysis.NewAnalysis

import LHCOAnalysis.Analysis.Hist
--import LHCOAnalysis.Analysis.Plot


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

maww = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/wwleptonic1-clean.lhco" "wwleptonic1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/wwleptonic2-clean.lhco" "wwleptonic2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/wwleptonic3-clean.lhco" "wwleptonic3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/wwleptonic4-clean.lhco" "wwleptonic4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/wwleptonic5-clean.lhco" "wwleptonic5-clean.binary"

mazw = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zwleptonic1-clean.lhco" "zwleptonic1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zwleptonic2-clean.lhco" "zwleptonic2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zwleptonic3-clean.lhco" "zwleptonic3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zwleptonic4-clean.lhco" "zwleptonic4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/zwleptonic5-clean.lhco" "zwleptonic5-clean.binary"


mazz = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zzleptonic1-clean.lhco" "zzleptonic1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zzleptonic2-clean.lhco" "zzleptonic2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zzleptonic3-clean.lhco" "zzleptonic3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/zzleptonic4-clean.lhco" "zzleptonic4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/zzleptonic5-clean.lhco" "zzleptonic5-clean.binary"

matt = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/ttbar1-clean.lhco" "ttbar1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/ttbar2-clean.lhco" "ttbar2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/ttbar3-clean.lhco" "ttbar3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/ttbar4-clean.lhco" "ttbar4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/ttbar5-clean.lhco" "ttbar5-clean.binary"

mabb = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/bbbar1-clean.lhco" "bbbar1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/bbbar2-clean.lhco" "bbbar2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/bbbar3-clean.lhco" "bbbar3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/bbbar4-clean.lhco" "bbbar4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/bbbar5-clean.lhco" "bbbar5-clean.binary"

madj = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/dijets1-clean.lhco" "dijets1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/dijets2-clean.lhco" "dijets2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/dijets3-clean.lhco" "dijets3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/dijets4-clean.lhco" "dijets4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/dijets5-clean.lhco" "dijets5-clean.binary"

mady = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/drellyan1-clean.lhco" "drellyan1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/drellyan2-clean.lhco" "drellyan2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/drellyan3-clean.lhco" "drellyan3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/drellyan4-clean.lhco" "drellyan4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/drellyan5-clean.lhco" "drellyan5-clean.binary"

maiw = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlew1-clean.lhco" "singlew1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlew2-clean.lhco" "singlew2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlew3-clean.lhco" "singlew3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlew4-clean.lhco" "singlew4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/singlew5-clean.lhco" "singlew5-clean.binary"

maiz = do forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlez1-clean.lhco" "singlez1-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlez2-clean.lhco" "singlez2-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlez3-clean.lhco" "singlez3-clean.binary"
          forkIO $makebinary "../../../montecarlo/SMBGD/sm14/singlez4-clean.lhco" "singlez4-clean.binary"
          makebinary "../../../montecarlo/SMBGD/sm14/singlez5-clean.lhco" "singlez5-clean.binary"


makebinary inputfilename outputfilename 
    = do putStrLn inputfilename
         putStrLn $ "output  = " ++ outputfilename
         inh <- openFile inputfilename ReadMode
         outh <- openFile outputfilename  WriteMode

         str <- B.hGetContents inh
         let parsed = parsestr str
             encodedbs = map Bi.encode parsed

         count <- newIORef (0::Int )

         let myaction str = do B.hPut outh str 
                               modifyIORef count (+1)
                               countnum <- readIORef count
                               if (countnum `mod` 1000) == 0
                                 then putStrLn (outputfilename ++ ":" ++ (show countnum))
                                 else (return ())

         mapM_ myaction encodedbs

         hClose inh 
         hClose outh



------------------------------

onefetch :: (Bi.Binary a) => Bi.Get (Bool,a)
onefetch = do boolresult <- G.isEmpty
              case boolresult of
                True  -> return (False,undefined)
                False -> do result <- Bi.get
                            return (True,result)

onefetchNonIO :: (Bi.Binary a) => State (Int64,B.ByteString) (Bool,a)
onefetchNonIO = do (byte,bytestr) <- get
                   let (result,bytestr',byte') = G.runGetState onefetch bytestr byte
                   put (byte',bytestr')
--                liftIO $ print byte'
                   return $ result

readbyte :: Handle -> IO [PhyEventClassified]
readbyte inh = do bytecontent <- B.hGetContents inh
                  let listfetchNonIO = sequence $ repeat onefetchNonIO
                      lst = evalState listfetchNonIO (0,bytecontent)
         --         print item 
                  return $ map snd $ takeWhile fst lst 
          --        lst <- sequence $ repeat onefetchIO 
         ---         return $ map snd $ takeWhile fst lst 

myread inh = 
    do lst <- readbyte inh
       let sqrresult = process_with_cut dilepton_inv_mass_sqr lst
            -- sqrtresult = map sqrt sqrresult

            -- print $ sqrresult
                  
       return sqrresult

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