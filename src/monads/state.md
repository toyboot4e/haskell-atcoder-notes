# State

## 例: `State` モナドで `mapAccumL`

累積和の計算には `scanl`, `scanl1` を使うと思いますが、 `mapAccumL` も利用できます。

`mapAccumL` は状態を持った `map` 関数です:

```hs
#!/usr/bin/env stack

import Data.List

main :: IO ()
main = do
  -- 数列 → 累積和
  -- [1, 2, 3] -> (6, [1, 3, 6])
  print $ mapAccumL (\acc x -> (acc + x, acc + x)) (0 :: Int) [1, 2, 3]

  -- 累積和 → 数列もできる (`foldl'` では無理だと思います)
```

同様の計算は `mapM` と `State` モナドにより表現可能です:

```hs
#!/usr/bin/env stack

import Control.Monad.State
import Data.List

main :: IO ()
main = do
  -- 数列 → 累積和
  -- [1, 2, 3] -> ([1, 3, 6], 6)
  print $ runState (mapM (\x -> state $ \acc -> (x + acc, x + acc)) [1, 2, 3]) (0 :: Int)
```

`mapAccumL` は `Traversable` なデータ型にしか適用できませんが、`mapM` なら `Vector` にもあるので適用できます。

<!-- ```hs -->
<!--   print $ runState (sequenceA $ map (\x -> state $ \acc -> (x + acc, x + acc)) [1, 2, 3]) (0 :: Int) -->
<!--   print $ runState (traverse (\x -> state $ \acc -> (x + acc, x + acc)) [1, 2, 3]) (0 :: Int) -->
<!-- ``` -->

## 例: `do` 記法

TODO: ポイントフリースタイルの恩恵

TODO: パフォーマンスへの (悪) 影響

