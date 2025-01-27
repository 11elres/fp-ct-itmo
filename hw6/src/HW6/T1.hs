module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import           Control.Concurrent.Classy     (MonadConc (atomically), STM)
import           Control.Concurrent.Classy.STM (MonadSTM (TVar, newTVar, readTVar, writeTVar),
                                                TArray, modifyTVar)
import           Control.Monad                 (forM_)
import           Data.Array.MArray             (MArray (getBounds, newArray),
                                                modifyArray, readArray,
                                                writeArray)
import           Data.Hashable                 (Hashable (hash))

-- | Initial capacity of the 'CHT'.
initCapacity :: Int
initCapacity = 16

-- | Load factor of the 'CHT'.
loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | Concurrent hash table.
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Create a new 'CHT'.
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  tarray  <- newArray (0, initCapacity - 1) []
  buckets <- newTVar tarray
  size    <- newTVar 0
  return $ CHT buckets size

-- | Get value by key from 'CHT'.
getCHT
  :: ( MonadConc m
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT k cht = atomically $ do
  (_, capacity, buckets) <- dataCHT cht
  let idx = hash k `mod` capacity
  bucket <- readArray buckets idx
  return $ lookup k bucket

-- | Put value by key to 'CHT'.
putCHT
  :: ( MonadConc m
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT k v cht = atomically $ do
  (_, capacity, buckets) <- dataCHT cht
  let idx = hash k `mod` capacity

  bucket <- readArray buckets idx
  case lookup k bucket of
    Just _  -> writeArray buckets idx $
      map (\(k', v') ->
        if k == k'
        then (k, v)
        else (k', v')
      ) bucket
    Nothing -> do
      modifyArray buckets idx ((k, v) :)
      modifyTVar (chtSize cht) (+ 1)
      ensureResizingCHT cht

-- | Ensure resizing of the 'CHT'.
ensureResizingCHT :: (MonadSTM m, Hashable k) => CHT m k v -> m ()
ensureResizingCHT cht = do
  (size, capacity, buckets) <- dataCHT cht

  if fromIntegral size < fromIntegral capacity * loadFactor
  then return ()
  else do
    let newCapacity = capacity * 2
    newBuckets <- newArray (0, newCapacity - 1) []
    writeTVar (chtBuckets cht) newBuckets
    forM_ [0 .. capacity - 1] $ \i -> do
      bucket <- readArray buckets i
      forM_ bucket $ \(k, v) -> do
        let newIndex = hash k `mod` newCapacity
        modifyArray newBuckets newIndex ((k, v) :)

-- | Get size of the 'CHT'.
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = atomically . readTVar . chtSize

-- | Get main data of the 'CHT': (size, capacity, buckets).
dataCHT :: MonadSTM m => CHT m k v -> m (Int, Int, TArray m Int (Bucket k v))
dataCHT cht = do
  size            <- readTVar $ chtSize cht
  buckets         <- readTVar $ chtBuckets cht
  (_, upperBound) <- getBounds buckets
  return (size, upperBound + 1, buckets)
