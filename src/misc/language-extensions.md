# 言語拡張について

TODO: GHC の言語拡張とは

## 書き方

フォーマッタ (`ormolu`) は言語拡張を 1 行ずつの分けてしまいます:

```hs
{-# LANGUAGE BangPatterns #-}
{-# BlockArguments #-}
{-# LambdaCase #-}
{-# MultiWayIf #-}
{-# PatternGuards #-}
{-# TupleSections #-}
-- ..
```

そこで言語拡張の宣言をフォーマッタの適用外にして、コード行数を削減できます:

```hs
{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase, MultiWayIf, PatternGuards, TupleSections #-}
{-# LANGUAGE NumDecimals, NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- ORMOLU_ENABLE -}
```

## おすすめしない言語拡張

### NPlusKPatterns

パタンを `n + 1` のように書ける拡張です。この拡張を有効にして `ghci` を起動してみます:

```sh
$ ghci -XNPlusKPatterns
Prelude> 
```

一見 N + K パタンは問題なく動きます:

```hs
Prelude> let (n + 1) = 1
Prelude> n
0
```

しかし `n` が負の数になるパタンでは実行時エラーが発生します:

```hs
Prelude> let (n + 1) = 0
Prelude> n
*** Exception: <interactive>:3:5-15: Non-exhaustive patterns in n+1
```

このように罠がある以上、多少冗長でも `let` や `where` を書こうと思いました。 [詳細](https://sites.google.com/site/haskell/notes/nkpatterns) (未確認)

