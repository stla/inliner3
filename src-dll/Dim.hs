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
import           Data.List (elemIndices, intersect)
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
          assocs = [((i,j),1) | j <- [0 .. l-1], i <- columns V.! j] ++
                    [((i,j),0) | j <- [0 .. l-1], i <- [0 .. dim'-1], i `notElem` columns V.! j]
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
      range_p' = [0 .. p'-1]
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
      vt1 = VS.unsafeFromSEXP _vt1
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
  vttemp <- newIORef [] :: IO (IORef [Double])
  when (l>0) $ do -- && l < p'
    let checkl_list = [j | j <- range_p', (vtsum VS.! j) < v]
        checkl = UV.fromList checkl_list
        range_l = [0 .. l-1]
        columns = V.fromList (map (\j -> [cc1 VS.! indexMatrix i (checkl UV.! j) dim' |
                                           i <- range_dim'])
                                   range_l)
        assocs = [((i,j),1) | j <- range_l, i <- columns V.! j] ++
                 [((i,j),0) | j <- range_l, i <- range_dim, i `notElem` columns V.! j]
        int = UA.array ((0,0),(2*n-1,l-1)) assocs :: UA.UArray (Int32, Int) Int32
        whichl_list = [j | j <- range_p', (vtsum VS.! j) >= v]
        vtsum_l = UV.fromList [vtsum VS.! j | j <- whichl_list]
        whichl = UV.fromList whichl_list
        vtsum_ll = UV.fromList [vtsum VS.! j | j <- checkl_list]
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
                    vtsum_l_ii = vtsum_l UV.! ii
                    vt1_l_ii = [vt1 VS.! indexMatrix i whichl_ii dim' | i <- range_dim']
                    inner :: Int -> IO ()
                    inner dd | dd == len_use = return ()
                             | otherwise = do
                               let ddi = use !! dd
                                   xx = elemIndices 1 (int2 !! ddi)
                                   inter = [cc1 VS.! indexMatrix k whichl_ii dim' |
                                            k <- xx]
                               x <- readIORef cctemp
                               writeIORef cctemp (x ++ inter ++ [k+n])
                               modifyIORef vert (+1)
                               let lambda = (v - vtsum_l_ii) / ((vtsum_ll UV.! ddi) - vtsum_l_ii)
                                   vt1_ll_dd = [vt1 VS.! indexMatrix i (checkl UV.! ddi) dim' | i <- range_dim']
                                   vtnew = zipWith (\a b -> lambda*a + (1-lambda)*b) vt1_ll_dd vt1_l_ii
                               modifyIORef vttemp (++ vtnew)
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
  ----
  let lu = VS.foldl' (\i b -> i + fromEnum (b > u)) 0 vtsum
  when (lu>0) $ do -- && lu < p'
    let checku_list = [j | j <- [0 .. p'-1], (vtsum VS.! j) > u]
        checku = UV.fromList checku_list
        range_lu = [0 .. lu-1]
        columns = V.fromList (map (\j -> [cc1 VS.! indexMatrix i (checku UV.! j) dim' |
                                           i <- range_dim'])
                                   range_lu)
        assocs = [((i,j),1) | j <- range_lu, i <- columns V.! j] ++
                 [((i,j),0) | j <- range_lu, i <- range_dim, i `notElem` columns V.! j]
        int = UA.array ((0,0),(2*n-1,lu-1)) assocs :: UA.UArray (Int32, Int) Int32
        whichu_list = [j | j <- range_p', (vtsum VS.! j) <= u]
        vtsum_u = UV.fromList [vtsum VS.! j | j <- whichu_list]
        vtsum_uu = UV.fromList [vtsum VS.! j | j <- checku_list]
        whichu = UV.fromList whichu_list
        go :: Int -> IO ()
        go ii | ii == p'-lu = return ()
              | otherwise = do
                let whichu_ii = whichu UV.! ii
                    int2 = map (\j -> [int UA.! (i,j) |
                                       i <- [cc1 VS.! indexMatrix r whichu_ii dim' |
                                             r <- range_dim']]) range_lu
                    colSums = map sum int2
                    use = elemIndices (dim-1) colSums
                    len_use = length use
                    vtsum_u_ii = vtsum_u UV.! ii
                    vt1_u_ii = [vt1 VS.! indexMatrix i whichu_ii dim' | i <- range_dim']
                    inner :: Int -> IO ()
                    inner dd | dd == len_use = return ()
                             | otherwise = do
                               let ddi = use !! dd
                                   xx = elemIndices 1 (int2 !! ddi)
                                   inter = [cc1 VS.! indexMatrix k whichu_ii dim' |
                                            k <- xx]
                               x <- readIORef cctemp
                               writeIORef cctemp (x ++ inter ++ [k])
                               modifyIORef vert (+1)
                               let lambda = (u - vtsum_u_ii) / ((vtsum_uu UV.! ddi) - vtsum_u_ii)
                                   vt1_uu_dd = [vt1 VS.! indexMatrix i (checku UV.! ddi) dim' | i <- range_dim']
                                   vtnew = zipWith (\a b -> lambda*a + (1-lambda)*b) vt1_uu_dd vt1_u_ii
                               modifyIORef vttemp (++ vtnew)
                               inner (dd+1)
                inner 0
                go (ii+1)
    go 0
  --
  let which_both = UV.fromList [j | j <- range_p', (vtsum VS.! j) >= v, (vtsum VS.! j) <= u]
      lb = UV.length which_both
  when (lb > 0) $ do
    let go :: Int -> IO ()
        go ii | ii == lb = return ()
              | otherwise = do
                modifyIORef vert (+1)
                let iii = which_both UV.! ii
                    cc1_ii = [cc1 VS.! indexMatrix i iii dim' | i <- range_dim']
                    vt1_ii = [vt1 VS.! indexMatrix i iii dim' | i <- range_dim']
                modifyIORef cctemp (++ cc1_ii)
                modifyIORef vttemp (++ vt1_ii)
                go (ii+1)
    go 0
  --
  out_cctemp <- readIORef cctemp
  out_vert <- readIORef vert
  out_vttemp <- readIORef vttemp
  -- out <- UV.unsafeFreeze temp
  -- let out' = UV.toList out
--       where
--       -- int0 = UA.listArray ((0,0),(2*n'-1,l-1)) (replicate (2*n*l) (0::Int32)) :: UA.UArray (Int32, Int) Int32
-- --          columns = V.fromList (map (\j -> map fromIntegral (UA.elems (extractColumn cA j))) [0 .. l-1])
--       -- !int0 = UA.array ((0,0),(2*n'-1,l-1)) [((i,j),1) | j <- [0 .. l-1], i <- UA.elems (extractColumn' cA (dim-1) j)] :: UA.UArray (Int32, Int) Int32
--       --
--       checkl = UV.fromList [j | j <- [0 .. p'-1], (vtsum VS.! j) < v]

  let out_integerList = mkProtectedSEXPVector sing
                        (map (VS.unsafeToSEXP . VS.fromList) [[fromIntegral l], [out_vert], out_cctemp] :: [SEXP s 'R.Int])
  return $ vectorAppend out_integerList ((VS.unsafeToSEXP . VS.fromList) out_vttemp :: SEXP s 'R.Real)
