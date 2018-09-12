{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib
  where
import           Foreign              (Int32, Ptr, castPtr, newForeignPtr_,
                                       peekArray)
import           Foreign.C
import           Foreign.C.String              (withCString)
import           Data.Array           (Array, array, bounds, ixmap)
import qualified Data.Array           as A
import           Data.Array.Unboxed   (UArray)
import qualified Data.Array.Unboxed   as UA
import           Data.Ratio
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as UV
import           Foreign.R            (SEXP, SEXP0, sexp, unsexp)
import qualified Foreign.R            as R
import           Data.Singletons      (sing)
import           Control.Memory.Region (V)
import           Language.R.Literal   (mkProtectedSEXPVector)


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

ij2k :: Int -> Int -> Int -> Int
ij2k = indexMatrix

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

inInterval :: Ord a => a -> (a,a) -> Bool
inInterval x (l,u) = (x >= l) && (x <= u)

--
foreign import ccall unsafe "vectorAppend" c_vectorAppend :: SEXP0 -> SEXP0 -> SEXP0

vectorAppend :: SEXP s 'R.Vector -> SEXP s a -> SEXP s 'R.Vector
vectorAppend list x = sexp (c_vectorAppend (unsexp list) (unsexp x))

foreign import ccall unsafe "toList" c_toList :: SEXP0 -> SEXP0

toList :: SEXP s a -> SEXP s 'R.Vector
toList x = sexp (c_toList (unsexp x))

foreign import ccall unsafe "realToSEXP" c_realToSEXP :: CInt -> Ptr CDouble -> SEXP0

realToSEXP :: [Double] -> IO (SEXP s 'R.Real)
realToSEXP list =
  SV.unsafeWith
    (SV.fromList (map realToFrac list :: [CDouble]))
      (return . sexp . c_realToSEXP (fromIntegral (length list)))

realToSEXP' :: SV.Vector Double -> IO (SEXP s 'R.Real)
realToSEXP' v =
  SV.unsafeWith
    (SV.map realToFrac v :: SV.Vector CDouble)
      (return . sexp . c_realToSEXP (fromIntegral (SV.length v)))

u2s :: UV.Vector Double -> SV.Vector Double
u2s = UV.convert

realToSEXP'' :: UV.Vector Double -> IO (SEXP s 'R.Real)
realToSEXP'' v =
  SV.unsafeWith
    (SV.map realToFrac (u2s v) :: SV.Vector CDouble)
      (return . sexp . c_realToSEXP (fromIntegral (UV.length v)))

-- integer to sexp
foreign import ccall unsafe "intToSEXP" c_intToSEXP :: CInt -> Ptr CInt -> SEXP0

intToSEXP' :: SV.Vector Int32 -> IO (SEXP s 'R.Int)
intToSEXP' v =
  SV.unsafeWith
    (SV.map fromIntegral v :: SV.Vector CInt)
      (return . sexp . c_intToSEXP (fromIntegral (SV.length v)))

intToSEXP :: [Int32] -> IO (SEXP s 'R.Int)
intToSEXP list =
  SV.unsafeWith
    (SV.fromList (map fromIntegral list :: [CInt]))
      (return . sexp . c_intToSEXP (fromIntegral (length list)))

listOfNames :: [String] -> IO (SEXP V 'R.Vector)
listOfNames names = do
  sexpstrings <- mapM (\x -> withCString x R.mkString) names :: IO [SEXP V 'R.String]
  let list = mkProtectedSEXPVector sing sexpstrings
  return list

--
foreign import ccall unsafe "setDim" c_setDim :: SEXP0 -> CInt -> CInt -> IO ()

setDim :: SEXP s a -> Int32 -> Int32 -> IO ()
setDim v nrow ncol =
  c_setDim (unsexp v) (fromIntegral nrow) (fromIntegral ncol)
