{-# LANGUAGE ForeignFunctionInterface #-}

module LHCOAnalysis.ROOTApp where

import Foreign.C            

#include "makehist.h"

foreign import ccall "makehist.h makehist" c_makehist
    :: IO CInt
