# 2 分探索

参考: [AtCoder灰・茶・緑色の方必見！二分探索を絶対にバグらせないで書く方法│FORCIA CUBE│フォルシア株式会社](https://www.forcia.com/blog/001434.html)

いわゆる『はねる式 2 分探索』です。

## 実装

```hs
-- {{{ Binary search

-- | Binary search for sorted items in an inclusive range (from left to right only)
-- |
-- | It returns an `(ok, ng)` index pair at the boundary.
-- |
-- | # Example
-- |
-- | With an OK predicate `(<= 5)`, list `[0..9]` can be seen as:
-- |
-- | > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- | >  <-------------->  <-------->
-- | >         ok             ng
-- |
-- | In this case `bsearch` returns the `(ok, ng)` = `(5, 6)` pair:
-- |
-- | > > let xs = [0..9] in do
-- | > >   print $ bsearch (0, 9) (\i -> xs !! i <= 5)
-- | > (5, 6)
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (low, high) isOk = bimap wrap wrap (loop (low - 1, high + 1) isOk)
  where
    loop (ok, ng) isOk
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = loop (m, ng) isOk
      | otherwise = loop (ok, m) isOk
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap x
      | x == low - 1 || x == high + 1 = Nothing
      | otherwise = Just x

-- }}}
```

