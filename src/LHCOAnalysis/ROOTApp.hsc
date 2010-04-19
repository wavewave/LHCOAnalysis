{-# LANGUAGE ForeignFunctionInterface #-}

module LHCOAnalysis.ROOTApp where

import Foreign.C            
import Foreign.C.String 
import Foreign.Ptr

#include "makehist.h"

foreign import ccall "makehist.h makehist" c_makehist
  :: CString -> CString -> CDouble -> CDouble -> CInt -> Ptr CInt 
     -> IO CInt

foreign import ccall "makehist.h plothist" c_plothist
  :: CString -> CString -> CString -> CString -> CString -> CString -> CInt  
     -> IO CInt
