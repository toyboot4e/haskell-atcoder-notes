# vector

`Vector` は高効率な配列のパッケージです。 `Array` と比べて API が豊かなため、基本的には `Vector` が好まれますが、 2 次元配列の表現には弱いです。使い分けが必要となります。

## 主な使い方

基本的に添字アクセスが \\(O(1)\\) なリストのように扱えます。パタンマッチができなくて戸惑いますが、大半の操作は高階関数で実現できますし、高階関数を使った方が効率の良いコードになります。リスト内包表記に対応するコードも、高階関数や `do` 記法で表現できることが多いです。

詳しくは [【電子版単体】Haskellで戦う競技プログラミング 第2版](https://booth.pm/ja/items/1577541) 第 4 章を参照してください。

## ノート

### Stream fusion

TODO: リストとのパフォーマンス比較 (差は無い？)

### `PrimMonad` や `INLINE` 関数

TODO:

### `Vector` で 2 次元配列を表現するには

不変配列なら `V.Vector (VU.Vector a)` が使えます。たまーに入力の処理で役立ちます。

可変配列なら `Array` を使います。

### タプルに対する実装

なんと内部実装としては 2 本の配列に切り替わります。

TODO: Struct of Arrays, データ族？

### Resizable な `MVector`

TODO: 実装例へのリンク？

