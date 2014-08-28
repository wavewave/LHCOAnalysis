{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HEP.Parser.LHCOAnalysis.Parse

main = do 
  str <- TIO.readFile "test.lhco"  
  let str' = str 
  let r = parseOnly lhco str'
  case r of 
    Left err -> print err
    Right lst -> print (length lst)

