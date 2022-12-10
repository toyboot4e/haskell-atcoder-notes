# Union-Find

参考: [Union-Find とは | アルゴ式](https://algo-method.com/descriptions/132)

データの相互排他的なデータ分けができます。

## 実装 (`MVector` ベース)

`MVector` を使う都合上、添字の範囲が極端に広い場合はメモリ使用量が制限を超えてしまいますが、座標圧縮を使ってメモリ使用量を抑えることができます。

> 以下のコードでは `PrimMonad` を使っていますが、実質的には `IO` でしか利用できません。

```hs
-- {{{ Union-Find tree

-- | Union-find implementation (originally by `@pel`)
newtype UnionFind s = UnionFind (VM.MVector s UfNode)

type IOUnionFind = UnionFind RealWorld

type STUnionFind s = UnionFind s

-- | `Child parent | Root size`. Not `Unbox` :(
data UfNode = Child {-# UNPACK #-} !Int | Root {-# UNPACK #-} !Int

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newUF #-}
newUF :: (PrimMonad m) => Int -> m (UnionFind (PrimState m))
newUF n = UnionFind <$> VM.replicate n (Root 1)

-- | Returns the root node index.
{-# INLINE root #-}
root :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> m Int
root uf@(UnionFind vec) i = do
  node <- VM.read vec i
  case node of
    Root _ -> return i
    Child p -> do
      r <- root uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VM.write vec i (Child r)
      return r

-- | Checks if the two nodes are under the same root.
{-# INLINE same #-}
same :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> Int -> m Bool
same uf x y = liftM2 (==) (root uf x) (root uf y)

-- | Just an internal helper.
unwrapRoot :: UfNode -> Int
unwrapRoot (Root s) = s
unwrapRoot (Child _) = undefined

-- | Unites two nodes.
{-# INLINE unite #-}
unite :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> Int -> m ()
unite uf@(UnionFind vec) x y = do
  px <- root uf x
  py <- root uf y
  when (px /= py) $ do
    sx <- unwrapRoot <$> VM.read vec px
    sy <- unwrapRoot <$> VM.read vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (par, chld) = if sx < sy then (px, py) else (py, px)
    VM.write vec chld (Child par)
    VM.write vec par (Root (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE size #-}
size :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> m Int
size uf@(UnionFind vec) x = do
  px <- root uf x
  s <- unwrapRoot <$> VM.read vec px
  return s

-- }}}
```

TODO: ちゃんと `PrimMonad` を活かして `ST` モナドが活きるようにする？

## 実装 (`IntMap` ベース)

TODO

