{-# LANGUAGE CPP #-}
-- | A compat module to take fixed points in 'Q'.
module TH.FixQ (fixQ) where

#if MIN_VERSION_template_haskell(2,17,0)
import Control.Monad.Fix (mfix)
import Language.Haskell.TH.Syntax (Q (..))

fixQ :: (a -> Q a) -> Q a
fixQ = mfix

#else

-- We don't have a MonadFix instance for Q
import Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import Control.Exception (BlockedIndefinitelyOnMVar (..), catch, throwIO)
import Control.Exception.Base (FixIOException (..))
import Language.Haskell.TH.Syntax (Q (..), runIO)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)

fixQ :: (a -> Q a) -> Q a
fixQ k = do
  m <- runIO newEmptyMVar
  ans <- runIO (unsafeDupableInterleaveIO
           (readMVar m `catch` \BlockedIndefinitelyOnMVar ->
                                  throwIO FixIOException))
  result <- k ans
  runIO (putMVar m result)
  return result

#endif
