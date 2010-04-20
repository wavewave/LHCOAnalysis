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
          
{--
main = do hist <- read_histogram_from_file "zprime_20100418.root" "20100418"
          th1f_fill hist 500.0
--          print hist
          sequence_ $ replicate 100 (th1f_fill hist 500.0)  
          
          write_histogram_to_file hist "test2.root"  --}

main = do let c_fitinvm = makeCfunctionPtr fitinvm
          funptr    <- makeFunPtrPtr c_fitinvm          
          
          putStrLn "start test"
          c_testfunptr funptr
          
