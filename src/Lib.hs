{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib
  where
import           Foreign              (Int32, Ptr, castPtr, newForeignPtr_,
                                       peekArray)
import           Foreign.C
-- import           Foreign.C.String              (peekCString)
import           Data.Array           (Array, array, bounds, ixmap)
import qualified Data.Array           as A
import           Data.Array.Unboxed   (UArray)
import qualified Data.Array.Unboxed   as UA
import           Data.Ratio
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as UV
import           Foreign.R            (SEXP, SEXP0, sexp, unsexp)
import qualified Foreign.R            as R

type MatrixI = UArray (Int, Int) Int32
type Array1dI = UArray Int Int32
type MatrixD = UArray (Int, Int) Double

toRational :: RealFloat a => a -> Rational
toRational x = (s % 1) * (2 % 1) ^^ e
  where
  (s,e) = decodeFloat x

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral

cDblToDbl :: CDouble -> Double
cDblToDbl = realToFrac

sexpToRealVector :: SEXP s 'R.Real -> IO (SV.Vector Double)
sexpToRealVector vectorR = do
    vectorPtr <- R.real vectorR
    -- let vectorPtr' = castPtr vectorPtr :: Ptr CDouble
    n <- R.length vectorR
    fptr <- newForeignPtr_ vectorPtr
    return $ SV.unsafeFromForeignPtr0 fptr n
    -- peekArray n vectorPtr

sexpToIntegerVector :: SEXP s 'R.Int -> IO (SV.Vector Int32)
sexpToIntegerVector vectorR = do
    vectorPtr <- R.integer vectorR
    n <- R.length vectorR
    fptr <- newForeignPtr_ vectorPtr
    return $ SV.unsafeFromForeignPtr0 fptr n

indexMatrix :: Int -> Int -> Int -> Int
indexMatrix i j nrow = j*nrow + i

indexMatrix' :: Int -> Int -> (Int,Int)
indexMatrix' = divMod -- k nrow

makeIntegerMatrix :: SV.Vector Int32 -> Int -> Int -> MatrixI
makeIntegerMatrix v nrow ncol =
  UA.array ((0,0),(nrow-1,ncol-1)) [((i,j), (SV.!) v (indexMatrix i j nrow)) | i <- [0 .. nrow-1], j <- [0 .. ncol-1]]

subsetColumns :: MatrixI -> UV.Vector Int -> MatrixI
subsetColumns m indices =
  UA.ixmap ((0,0),(nrowMinus1, l - 1)) f m
  where
  (_,(nrowMinus1, _)) = UA.bounds m
  -- nrow = nrowMinus1 + 1
  -- indices = SV.findIndices (== True) v
  l = UV.length indices
  f :: (Int,Int) -> (Int,Int)
--  f (i,j) = indexMatrix' ((SV.!) indices (indexMatrix i j l)) nrow
  f (i,j) = (i, (UV.!) indices j)

extractColumn :: MatrixI -> Int -> Array1dI
extractColumn m j =
  let (_, (nrow',_)) = UA.bounds m in
  UA.ixmap (0,nrow') (\i -> (i,j)) m

extractColumn' :: MatrixI -> Int -> Int -> Array1dI
extractColumn' m nrow' j =
  UA.ixmap (0,nrow') (\i -> (i,j)) m

--
foreign import ccall unsafe "vectorAppend" c_vectorAppend :: SEXP0 -> SEXP0 -> SEXP0

vectorAppend :: SEXP s 'R.Vector -> SEXP s a -> SEXP s 'R.Vector
vectorAppend list x = sexp (c_vectorAppend (unsexp list) (unsexp x))
