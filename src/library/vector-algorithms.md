# vector-algorithms

`vector-algorithms` は可変配列に対するソートや検索を提供します。

不変配列に対しても適用できます:

- [VU.modify](https://www.stackage.org/haddock/lts-16.11/vector-0.12.1.2/Data-Vector-Unboxed.html#v:modify)  
`VU.Vector` を一時的に `VUM.Vector` に変えて操作を適用し、再び `VU.Vector` に凍結します。  

- [VU.thaw](https://www.stackage.org/haddock/lts-16.11/vector-0.12.1.2/Data-Vector-Unboxed.html#v:thaw) (解凍)  
`VU.Vector` を `VUM.Vector` に変えます。

> [`VU.modify`](https://www.stackage.org/haddock/lts-16.11/vector-0.12.1.2/Data-Vector-Unboxed.html#v:modify) は [`VUM.modify`](https://www.stackage.org/haddock/lts-16.11/vector-0.12.1.2/Data-Vector-Unboxed-Mutable.html#v:modify) とはまったく別物です。

