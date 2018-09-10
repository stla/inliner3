{-# LANGUAGE BangPatterns                #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Dim where
import           Control.Monad        ((<$!>), (=<<), when)
import           Data.Array           (array, elems)
import qualified Data.Array           as A
import qualified Data.Array.Unboxed   as UA
import           Data.Maybe
import           Data.Singletons      (sing)
import qualified Data.Vector          as V
import qualified Data.Vector.SEXP     as VS
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as UV
import           Foreign
import           Foreign.C
import           Foreign.R            (SEXP, SEXP0, SomeSEXP)
import qualified Foreign.R            as R
import           Foreign.R.Internal   (somesexp)
import           Language.R.Instance  (runRegion)
import           Language.R.Literal   (mkProtectedSEXPVector)
import qualified Language.R.Matcher   as M
import           Lib
import qualified Data.Sequence as S
import           Data.Vector.Unboxed.Mutable (IOVector, new, unsafeRead,
                                              unsafeWrite)
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Data.List (elemIndices)
import Data.IORef

foreign export ccall getDim :: SEXP0 -> IO (SEXP s R.Int)
getDim :: SEXP0 -> IO (SEXP s R.Int)
getDim matrix = do
  let matrix' = somesexp matrix
  runRegion $ do
    Right dimensions <- M.matchOnly M.dim matrix'
    return $ (VS.toSEXP . VS.fromList) (map fromIntegral dimensions)

foreign export ccall fidVertex :: Ptr (SEXP s 'R.Real) -> Ptr CInt
                               -> Ptr (SEXP s 'R.Int)
                               -> Ptr (SEXP s 'R.Real) -> Ptr CDouble
                               -> Ptr CDouble -> Ptr CInt -> Ptr CInt
                               -> Ptr (SEXP s R.Vector) -> IO ()
fidVertex :: Ptr (SEXP s 'R.Real) -> Ptr CInt -> Ptr (SEXP s 'R.Int)
          -> Ptr (SEXP s 'R.Real) -> Ptr CDouble -> Ptr CDouble -> Ptr CInt
          -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
fidVertex _vt1 _p _cc1 _vtsum _u _v _dim _n _result = do
  p <- (<$!>) cIntToInt (peek _p)
  u <- (<$!>) cDblToDbl (peek _u)
  v <- (<$!>) cDblToDbl (peek _v)
  dim <- (<$!>) cIntToInt (peek _dim)
  n <- (<$!>) cIntToInt (peek _n)
  vtsum <- (=<<) sexpToRealVector (peek _vtsum)
  cc1 <- (=<<) sexpToIntegerVector (peek _cc1)
  -- let l = fromIntegral (SV.length vtsum) :: Int32 -- this is p
  -- vtsum <- (<$!>) VS.fromSEXP (peek _vtsum)
  -- let l = fromIntegral (VS.length vtsum) :: Int32
  let whichl = SV.map (>= v) vtsum
      -- whichl' = SV.findIndices (==True) whichl
      whichl'' = SV.convert whichl :: UV.Vector Bool
      whichl''' = UV.findIndices (==True) whichl''
      whichu = SV.map (<= u) vtsum
      whichu'' = SV.convert whichu :: UV.Vector Bool
      both = UV.zipWith (&&) whichl'' whichu''
      -- checkl = SV.map not whichl
      -- checkl' = SV.findIndices (==False) whichl
      checkl''' = UV.findIndices (==False) whichl''
      checku = UV.map not whichu''
      cc1' = makeIntegerMatrix cc1 dim p
      cA = subsetColumns cc1' checkl'''
      cB = subsetColumns cc1' whichl'''
      -- any_checkl = isJust (SV.findIndex (==True) checkl)
      int = if not (UV.null checkl''')
        then UA.array ((0,0),(2*n-1,l-1)) associations :: MatrixI
        else UA.array((0,0),(0,0)) [((0,0),0::Int32)]
          where
          -- l = SV.foldl' (\i b -> i + fromEnum b) 0 checkl -- number of 'True' in checkl
          l = UV.length checkl'''
          range_i = [0 .. 2*n-1]
          indices = [(i,j) | i <- range_i, j <- [0 .. l-1]]
          zero32 = 0 :: Int32
          one32 = 1 :: Int32
          -- to try: listArray ((0,0),(2*n-1,l-1)) (replicate (2*n*l) (0::Int32))
          -- puis utiliser \\ [((i,j),1) | j <- [0 .. l-1], i <- "column j"]
          -- columns = V.fromList (map (\j -> map fromIntegral (UA.elems (extractColumn cA j))) [0 .. l-1])
          cA_vector = UV.map fromIntegral $ UV.fromList (UA.elems cA)
          columns = V.map (\j -> UV.unsafeBackpermute cA_vector (UV.fromList [j + i*l | i <- [0 .. dim-1]])) (V.enumFromN 0 l)
          -- il faut re et dim ! ici c'est re, pas dim
          f :: (Int,Int) -> ((Int,Int), Int32)
          f (i,j) = let col = (V.!) columns j in
                    if i `UV.elem` col then ((i,j),one32) else ((i,j),zero32)
          associations = map f indices
          --
          --vtsum_l = UV.unsafeBackpermute vtsum whichl'''
          --vtsum_ll = UV.unsafeBackpermute vtsum checkl'''
  poke _result $ mkProtectedSEXPVector sing
    (map (VS.unsafeToSEXP . VS.fromList) [[fromIntegral $ UV.length checkl'''],[(UA.!) int (0,0)], UA.elems int] :: [SEXP s 'R.Int])


foreign export ccall fidVertex2 :: SEXP s 'R.Real -> SEXP s 'R.Int
                                -> SEXP s 'R.Int -> SEXP s 'R.Real
                                -> SEXP s 'R.Real -> SEXP s 'R.Real
                                -> SEXP s 'R.Int -> SEXP s 'R.Int
                                -> IO (SEXP s R.Vector)
fidVertex2 :: SEXP s 'R.Real -> SEXP s 'R.Int
           -> SEXP s 'R.Int -> SEXP s 'R.Real
           -> SEXP s 'R.Real -> SEXP s 'R.Real
           -> SEXP s 'R.Int -> SEXP s 'R.Int
           -> IO (SEXP s R.Vector)
fidVertex2 _vt1 _p _cc1 _vtsum _u _v _dim _n = do
  let p = fromIntegral $ (VS.!) (VS.unsafeFromSEXP _p) 0 :: Int
      u = (VS.!) (VS.unsafeFromSEXP _u) 0
      v = (VS.!) (VS.unsafeFromSEXP _v) 0
      dim = fromIntegral $ (VS.!) (VS.unsafeFromSEXP _dim) 0 :: Int
      n = fromIntegral $ (VS.!) (VS.unsafeFromSEXP _n) 0 :: Int
  vtsum <- sexpToRealVector _vtsum
  cc1 <- sexpToIntegerVector _cc1
  let whichl = SV.map (>= v) vtsum
      -- whichl' = SV.findIndices (==True) whichl
      whichl'' = SV.convert whichl :: UV.Vector Bool
      whichl''' = UV.findIndices (==True) whichl''
      whichu = SV.map (<= u) vtsum
      whichu'' = SV.convert whichu :: UV.Vector Bool
      both = UV.zipWith (&&) whichl'' whichu''
      -- checkl = SV.map not whichl
      -- checkl' = SV.findIndices (==False) whichl
      checkl''' = UV.findIndices (==False) whichl''
      checku = UV.map not whichu''
      cc1' = makeIntegerMatrix cc1 dim p
      cA = subsetColumns cc1' checkl'''
      cB = subsetColumns cc1' whichl'''
      -- any_checkl = isJust (SV.findIndex (==True) checkl)
      int = if not (UV.null checkl''')
        then UA.array ((0,0),(2*n-1,l-1)) associations :: MatrixI
        else UA.array((0,0),(0,0)) [((0,0),0::Int32)]
          where
          -- l = SV.foldl' (\i b -> i + fromEnum b) 0 checkl -- number of 'True' in checkl
          l = UV.length checkl'''
          indices = [(i,j) | i <- [0 .. 2*n-1], j <- [0 .. l-1]]
          zero32 = 0 :: Int32
          one32 = 1 :: Int32
--          columns = V.fromList (map (\j -> map fromIntegral (UA.elems (extractColumn cA j))) [0 .. l-1])
          columns = V.fromList (map (\j -> S.fromFunction dim (\i -> fromIntegral ((UA.!) cA (i,j)))) [0 .. l-1])
          f :: (Int,Int) -> ((Int,Int), Int32)
          f (i,j) = let col = (V.!) columns j in
                    if isJust (S.elemIndexL i col) then ((i,j),one32) else ((i,j),zero32)
                    -- if i `elem` col then ((i,j),one32) else ((i,j),zero32)
          associations = map f indices
          --
          --vtsum_l = UV.unsafeBackpermute vtsum whichl'''
          --vtsum_ll = UV.unsafeBackpermute vtsum checkl'''
  return $ mkProtectedSEXPVector sing
    (map (VS.unsafeToSEXP . VS.fromList) [[fromIntegral $ UV.length checkl'''],[(UA.!) int (0,0)], UA.elems int] :: [SEXP s 'R.Int])
--
foreign export ccall fidVertex3 :: SEXP s 'R.Real -> SEXP s 'R.Int
                                -> SEXP s 'R.Int -> SEXP s 'R.Real
                                -> SEXP s 'R.Real -> SEXP s 'R.Real
                                -> SEXP s 'R.Int -> SEXP s 'R.Int
                                -> IO (SEXP s R.Vector)
fidVertex3 :: SEXP s 'R.Real -> SEXP s 'R.Int
           -> SEXP s 'R.Int -> SEXP s 'R.Real
           -> SEXP s 'R.Real -> SEXP s 'R.Real
           -> SEXP s 'R.Int -> SEXP s 'R.Int
           -> IO (SEXP s R.Vector)
fidVertex3 _vt1 _p _cc1 _vtsum _u _v _dim _n = do
  let p = fromIntegral $ (VS.!) (VS.unsafeFromSEXP _p) 0 :: Int
      u = (VS.!) (VS.unsafeFromSEXP _u) 0
      v = (VS.!) (VS.unsafeFromSEXP _v) 0
      dim' = (VS.!) (VS.unsafeFromSEXP _dim) 0
      dim = fromIntegral dim' :: Int
      n' = (VS.!) (VS.unsafeFromSEXP _n) 0
      n = fromIntegral n' :: Int
  vtsum <- sexpToRealVector _vtsum
  cc1 <- sexpToIntegerVector _cc1
  let whichl = SV.map (>= v) vtsum
      whichl'' = SV.convert whichl :: UV.Vector Bool
      whichl''' = UV.findIndices (==True) whichl''
      whichu = SV.map (<= u) vtsum
      whichu'' = SV.convert whichu :: UV.Vector Bool
      both = UV.zipWith (&&) whichl'' whichu''
      checkl''' = UV.findIndices (==False) whichl''
      checku = UV.map not whichu''
      cc1' = makeIntegerMatrix cc1 dim p
      -- !cA = subsetColumns cc1' checkl'''
      cB = subsetColumns cc1' whichl'''
      int = if not (UV.null checkl''')
        -- then int0 UA.// [((i,j),1) | j <- [0 .. l-1], i <- UA.elems (extractColumn cA j)]
        -- then UA.amap f int0
        then UA.array ((0,0),(2*n'-1,l-1)) assocs :: UA.UArray (Int32, Int) Int32
        else UA.array((0,0),(0,0)) [((0,0),0::Int32)]
          where
          l = UV.length checkl'''
          -- int0 = UA.listArray ((0,0),(2*n'-1,l-1)) (replicate (2*n*l) (0::Int32)) :: UA.UArray (Int32, Int) Int32
--          columns = V.fromList (map (\j -> map fromIntegral (UA.elems (extractColumn cA j))) [0 .. l-1])
          -- !int0 = UA.array ((0,0),(2*n'-1,l-1)) [((i,j),1) | j <- [0 .. l-1], i <- UA.elems (extractColumn' cA (dim-1) j)] :: UA.UArray (Int32, Int) Int32
          -- f :: Int32 -> Int32
          -- f 1 = 1
          -- f _ = 0
          --
          !columns = V.fromList (map (\j -> UA.elems (extractColumn cc1' (checkl''' UV.! j))) [0 .. l-1])
          assocs = concat [[((i,j),1) | j <- [0 .. l-1], i <- columns V.! j],
                           [((i,j),0) | j <- [0 .. l-1], i <- [0 .. dim'-1], i `notElem` columns V.! j]]
          --vtsum_l = UV.unsafeBackpermute vtsum whichl'''
          --vtsum_ll = UV.unsafeBackpermute vtsum checkl'''
  return $ mkProtectedSEXPVector sing
    (map (VS.unsafeToSEXP . VS.fromList) [[fromIntegral $ UV.length checkl'''],[(UA.!) int (0,0)], UA.elems int] :: [SEXP s 'R.Int])

--
foreign export ccall fidVertex4 :: SEXP s 'R.Real -> SEXP s 'R.Int
                                -> SEXP s 'R.Int -> SEXP s 'R.Real
                                -> SEXP s 'R.Real -> SEXP s 'R.Real
                                -> SEXP s 'R.Int -> SEXP s 'R.Int
                                -> SEXP s 'R.Int -> IO (SEXP s R.Vector)
fidVertex4 :: SEXP s 'R.Real -> SEXP s 'R.Int
           -> SEXP s 'R.Int -> SEXP s 'R.Real
           -> SEXP s 'R.Real -> SEXP s 'R.Real
           -> SEXP s 'R.Int -> SEXP s 'R.Int -> SEXP s 'R.Int
           -> IO (SEXP s R.Vector)
fidVertex4 _vt1 _p _cc1 _vtsum _u _v _dim _n _k = do
  let p = (VS.!) (VS.unsafeFromSEXP _p) 0
      p' = fromIntegral p :: Int
      u = (VS.!) (VS.unsafeFromSEXP _u) 0
      v = (VS.!) (VS.unsafeFromSEXP _v) 0
      dim = (VS.!) (VS.unsafeFromSEXP _dim) 0
      dim' = fromIntegral dim :: Int
      range_dim = [0 .. dim - 1]
      range_dim' = [0 .. dim' - 1]
      n = (VS.!) (VS.unsafeFromSEXP _n) 0
      k = (VS.!) (VS.unsafeFromSEXP _k) 0
      vtsum = VS.unsafeFromSEXP _vtsum
      cc1 = VS.unsafeFromSEXP _cc1
  -- let whichl = VS.map (>= v) vtsum
  --     whichu = VS.map (<= u) vtsum
  --     both = VS.zipWith (&&) whichl whichu
  --     checkl = VS.map not whichl
      -- cc1' = makeIntegerMatrix cc1 dim p
      -- !cA = subsetColumns cc1' checkl'''
      -- cB = subsetColumns cc1' whichl'''
      l = VS.foldl' (\i b -> i + fromEnum (b < v)) 0 vtsum
  -- temp <- UMV.replicate dim' 0 :: IO (IOVector Int32)
  -- cctemp <- new 0 0 :: IO (IOVector Int32)
  cctemp <- newIORef [] :: IO (IORef [Int32])
  vert <- newIORef 0 :: IO (IORef Int32)
  when (l>0) $ do
  -- then int0 UA.// [((i,j),1) | j <- [0 .. l-1], i <- UA.elems (extractColumn cA j)]
  -- then UA.amap f int0
    let checkl = UV.fromList [j | j <- [0 .. p'-1], (vtsum VS.! j) < v]
        range_l = [0 .. l-1]
        columns = V.fromList (map (\j -> [cc1 VS.! indexMatrix i (checkl UV.! j) dim' |
                                           i <- range_dim'])
                                   range_l)
        assocs = [((i,j),1) | j <- range_l, i <- columns V.! j] ++
                 [((i,j),0) | j <- range_l, i <- range_dim, i `notElem` columns V.! j]
        int = UA.array ((0,0),(2*n-1,l-1)) assocs :: UA.UArray (Int32, Int) Int32
    --vtsum_l = UV.unsafeBackpermute vtsum whichl'''
    --vtsum_ll = UV.unsafeBackpermute vtsum checkl'''
        whichl = UV.fromList [j | j <- [0 .. p'-1], (vtsum VS.! j) >= v]
        go :: Int -> IO ()
        go ii | ii == p'-l = return ()
              | otherwise = do
                let whichl_ii = whichl UV.! ii
                    int2 = map (\j -> [int UA.! (i,j) |
                                       i <- [cc1 VS.! indexMatrix r whichl_ii dim' |
                                             r <- range_dim']]) range_l
                    colSums = map sum int2
                    use = elemIndices (dim-1) colSums
                    len_use = length use
                    inner :: Int -> IO ()
                    inner dd | dd == len_use = return ()
                             | otherwise = do
                               let xx = elemIndices 1 (int2 !! (use !! dd))
                                   inter = [cc1 VS.! indexMatrix k whichl_ii dim' |
                                            k <- xx]
                               x <- readIORef cctemp
                               writeIORef cctemp (x ++ inter ++ [k+n])
                               modifyIORef vert (+1)
                               inner (dd+1)

                -- let int2_col1 = [int UA.! (i,0) |
                --                  i <- [cc1 VS.! indexMatrix k (whichl UV.! ii) dim' |
                --                        k <- [0 .. dim' - 1]]]
                    -- inner :: Int -> IO ()
                    -- inner kk | kk == dim' = return ()
                    --          | otherwise = do
                    --            unsafeWrite temp kk (int2_col1 !! kk)
                    --            inner (kk+1)
                inner 0
                go (ii+1)
    go 0
  out_cctemp <- readIORef cctemp
  out_vert <- readIORef vert
  -- out <- UV.unsafeFreeze temp
  -- let out' = UV.toList out
--       where
--       -- int0 = UA.listArray ((0,0),(2*n'-1,l-1)) (replicate (2*n*l) (0::Int32)) :: UA.UArray (Int32, Int) Int32
-- --          columns = V.fromList (map (\j -> map fromIntegral (UA.elems (extractColumn cA j))) [0 .. l-1])
--       -- !int0 = UA.array ((0,0),(2*n'-1,l-1)) [((i,j),1) | j <- [0 .. l-1], i <- UA.elems (extractColumn' cA (dim-1) j)] :: UA.UArray (Int32, Int) Int32
--       --
--       checkl = UV.fromList [j | j <- [0 .. p'-1], (vtsum VS.! j) < v]


  return $ mkProtectedSEXPVector sing
    (map (VS.unsafeToSEXP . VS.fromList) [[fromIntegral l], [out_vert], out_cctemp] :: [SEXP s 'R.Int])
