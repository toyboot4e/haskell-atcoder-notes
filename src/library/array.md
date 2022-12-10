# array

[accumArray]: http://zvon.org/other/haskell/Outputarray/accumArray_f.html
[Ix]: https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Ix.html

`Array` は主に多次元配列の表現に利用します。

## `Array` と `Vector` の使い分け

1 次元配列の場合は `Vector` を使うことが多いです (API がリッチため) 。 `Array` には添字アクセス以外の API がほぼありませんが、 [accumArray] がハマる場合は `Array` を使うこともあります。

2 次元配列の場合は `Array` を使うことが多いです:

- 2 次元の可変配列は `Array` でしか表現できない気がします (?)
- 2 次元の不変配列も `Array` の方が添字アクセスが綺麗になります:
  - `arr ! (y, x)`
  - `vec VU.! (x + y * w)`

## `class Ix`

`array` の添字は [Ix] クラスで抽象されています。 [Ix] を知ることで `Array` の API が理解できるようになります。

[Ix] は `Int` や `Char` などに対して実装されており、また n 次元に拡張されています。

### [Ix] の使い方 (1 次元)

[Ix] を試してみます。まずは REPL を起動します:

```sh
$ ghci
Prelude> import Data.Ix
Prelude Data.Ix>
```

[Ix] クラスの `index` 関数は、添字範囲 \\([x1, x2]\\) に対する相対位置を返します:

```hs
Prelude Data.Ix> index (0, 4) 2
2
Prelude Data.Ix> index (2, 6) 2
0
```

### [Ix] の使い方 (2 次元)

[Ix] は n 次元に拡張されており、 2 次元の添字も表現できます:

```hs
Prelude Data.Ix> index ((0, 0), (5, 5)) (0, 1)
1
Prelude Data.Ix> index ((0, 0), (5, 5)) (1, 0)
6
Prelude Data.Ix> index ((0, 0), (5, 5)) (5, 5)
36
```

注意すべきなのは、タプルの右端の値が 1 次元目を表すということです。 Row-major な行列の場合は、 `Array` 生成時のサイズは `(h, w)` であり、 `Array` への添字アクセスには `(y, x)` (すなわち `(row, column)` ) を使います。

## Immutable array

### `accumArray` で生成

[accumArray] は配列への畳み込みです。使い方はリンク先の通り:

```hs
Prelude> import Data.Array
Prelude Data.Array> accumArray (+) 0 (1, 3) [(1, -1), (2, 1), (2, 2), (3, 5)]
--                             ~~~ ~  ~~~~    ~~~~
--                             |   |  |       |
--                             |   |  |       +--- (添字 1, 添字 1 に対する入力 -1)
--                             |   |  +--- 添字範囲 = [1, 3]
--                             |   +--- 初期の蓄積値 = 0
--                             +--- (\蓄積値 入力 -> 新しい蓄積値)
array (1,3) [(1,-1),(2,3),(3,5)]
```

## Mutable array

### ST モナドを使った配列操作

- [runSTArray](https://www.stackage.org/haddock/lts-20.1/array-0.5.4.0/Data-Array-ST.html#t:STUArray)
- [runSTUArray](https://www.stackage.org/haddock/lts-20.1/array-0.5.4.0/Data-Array-ST.html#v:runSTUArray)

TODO

### IO モナドを使った配列操作

TODO

### `accumArray` からの `thaw` (解凍)

TODO

## 注意点

### タプルの array には boxed array を使わざるを得ない

TODO

