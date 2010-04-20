import LHCOAnalysis.Analysis.Plot
import LHCOAnalysis.Analysis.Fitting
import LHCOAnalysis.ROOTApp

import System.IO

testfun :: Double -> Double 
testfun x = x + 1.0

{-
mai' = do let testcfun = makeCfunction testfun
          funptr <- makeFunPtr testcfun
          
          putStrLn "start test"
          c_testfunptr funptr -}
          

main = do hist <- read_histogram_from_file "zprime_20100418.root" "20100418"
--          th1f_fill hist 500.0
--          print hist
--          sequence_ $ replicate 100 (th1f_fill hist 500.0)  
          
          fitting hist 0 2000 [100,700,700,1500] fitinvm

          let hdeco = HistDecoration "test" "xtest" "ytest"
              
              
              
          makePicsFromHist hist "test.eps" hdeco
       
       
--          write_histogram_to_file hist "test3.root"  

{--main = do let c_fitinvm = makeCfunctionPtr fitinvm
          funptr    <- makeFunPtrPtr c_fitinvm          
          
          putStrLn "start test"
          c_testfunptr funptr --}
          
          
          
mai' = do print $ fitinvm 3.2 3.2 3.2 3.3
          
