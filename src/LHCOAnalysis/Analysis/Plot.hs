{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.Plot where

import LHCOAnalysis.ROOTApp

import System.IO
import Foreign.C
import Foreign.C.String

type HistName = String

data PlotFileType = EPSFile | PDFFile

data HistDecoration = HistDecoration 
                      { histtitle    :: String
                      , xaxistitle   :: String
                      , yaxistitle   :: String 
                      }
                      
                      
plotHist :: PlotFileType -> HistName -> FilePath -> FilePath -> HistDecoration 
            -> IO CInt
plotHist plottype hname infile outfile hdeco = do 
  c_hname   <- newCString hname
  c_infile  <- newCString infile 
  c_outfile <- newCString outfile
  c_histtitle  <- newCString $ histtitle hdeco
  c_xaxistitle <- newCString $ xaxistitle hdeco
  c_yaxistitle <- newCString $ yaxistitle hdeco
  c_null    <- newCString ""
  let c_zero = fromIntegral 0
  c_plothist c_hname c_infile c_outfile 
    c_histtitle c_xaxistitle c_yaxistitle c_zero
    

