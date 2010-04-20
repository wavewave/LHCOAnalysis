{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module LHCOAnalysis.ROOTApp where

import Foreign.C            
import Foreign.C.String 
import Foreign.Ptr
import Foreign.ForeignPtr


#include "makehist.h"

type CFunction = CDouble -> Ptr () -> CDouble

foreign import ccall "makehist.h makehist" c_makehist
  :: CString -> CString -> CDouble -> CDouble -> CInt -> Ptr CInt 
     -> IO CInt

foreign import ccall "makehist.h plothist" c_plothist
  :: CString -> CString -> CString -> CString -> CString -> CString -> CInt  
     -> IO CInt

foreign import ccall "makehist.h testfunptr" c_testfunptr
  :: (FunPtr CFunction) -> IO CInt
     
data RawTH1F 
newtype TH1F = TH1F (ForeignPtr RawTH1F) deriving (Eq, Ord,Show )

foreign import ccall "makehist.h TH1F_Fill" c_th1f_fill
  :: (Ptr RawTH1F) -> CDouble -> IO ()
     
foreign import ccall "makehist.h read_histogram_from_file" c_read_histogram_from_file
  :: CString -> CString -> IO (Ptr RawTH1F) 
     
foreign import ccall "makehist.h write_histogram_to_file" c_write_histogram_to_file
  :: (Ptr RawTH1F) -> CString -> IO () 
     
     