module BinaryMake where


import LHCOAnalysis
import LHCOAnalysis.Utility
import LHCOAnalysis.Analysis.CutSets
import LHCOAnalysis.Parse
import LHCOAnalysis.PhysObj
import LHCOAnalysis.NewAnalysis

import LHCOAnalysis.Analysis.Hist

import LHCOAnalysis.MakeBinary



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
