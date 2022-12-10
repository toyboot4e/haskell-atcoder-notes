# プロジェクト構成

[lts-16.11]: https://www.stackage.org/lts-16.11
[implicit-hie]: https://github.com/Avi-D-coder/implicit-hie#readme

HLS が動く Stack プロジェクトを作ります。またすべての `Main.hs` は stack script です。

> **『正しい』構成ではない** と思いますが、一応動くので載せておきます。

## ファイル構成

僕は以下を使っています:

```sh
abc-hs/
├── .git/
├── abc278/
└── abc279/ # 参加コンテスト毎にディレクトリを作成
    ├── .projectile      # ※
    ├── abc279.cabal     # 自動生成
    ├── hie.yaml         # `gen-hie` で生成
    ├── package.yaml     # 編集対象
    ├── stack.yaml       # 編集対象
    └── stack.yaml.lock  # 自動生成
```

> ※ Emacs では LSP workspace のルートディレクトリが `abc-hs/` になったため、仕方なく `.projectile` ファイルを作って `abc279/` などがルートディレクトリであると Emacs に伝えています。

## プロジェクト毎の GHC のバージョン切り替え

僕は `stack.yaml` で `system-ghc: true` (後述) を設定し、 [direnv](https://direnv.net/) で `PATH` を書き換えて GHC / HLS のバージョンを切り替えています。間に合わせとしては十分なのではないでしょうか……。

## `stack.yaml`

Stack にはプロジェクト下の `.stack-work/` に GHC をインストールする機能がありますが、プロジェクトの数だけ GHC がインストールされるのは無駄です。 `PATH` 中の GHC を使用するように `system-ghc: true` を設定しています。また AtCoder で使用されるパッケージを利用できるように設定します:

```yaml
system-ghc: true
resolver: lts-16.11
packages:
- .
extra-deps:
- repa-3.4.1.4
```

> 主なパッケージは [lts-16.11] に含まれますが、 `repa-3.4.1.4` のみ例外です。

## `package.yaml`

パッケージ、 (HLS で使用される) 警告のレベル、実行ファイルなどを設定します:

<details>
<summary><code>package.yaml</code></summary>

```yaml
dependencies:
   - base >= 4.7 && < 5

   - QuickCheck
   - array
   - attoparsec
   - bytestring
   - containers
   - deepseq
   - extra
   - fgl
   - hashable
   - heaps
   - integer-logarithms
   - lens
   - massiv
   - mono-traversable
   - mtl
   - mutable-containers
   - mwc-random
   - parallel
   - parsec
   - primitive
   - psqueues
   - random
   - reflection
   - repa
   - template-haskell
   - text
   - tf-random
   - transformers
   - unboxing-vector
   - unordered-containers
   - utility-ht
   - vector
   - vector-algorithms
   - vector-th-unbox

# DRY for package.yaml executables:
# <https://www.reddit.com/r/haskell/comments/haeqin/dry_for_packageyaml_executables/>
_exe-defs: &exe-defaults
  # dependencies:
  # - abs
  ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    - -Wall # all warnings
  other-modules: []

# REMARK: See `README` for the langauge server support.
executables:
  a-exe:
    <<: *exe-defaults
    source-dirs: a
    main:                Main.hs

  b-exe:
    <<: *exe-defaults
    source-dirs: b
    main:                Main.hs

  c-exe:
    <<: *exe-defaults
    source-dirs: c
    main:                Main.hs

  d-exe:
    <<: *exe-defaults
    source-dirs: d
    main:                Main.hs

  e-exe:
    <<: *exe-defaults
    source-dirs: e
    main:                Main.hs

  f-exe:
    <<: *exe-defaults
    source-dirs: f
    main:                Main.hs

  g-exe:
    <<: *exe-defaults
    source-dirs: g
    main:                Main.hs

  ex-exe:
    <<: *exe-defaults
    source-dirs: ex
    main:                Main.hs
```

</details>

`executables` を省くと HLS 動作が不安定になった (動いたり動かなかったりする) ため、書くようにしています。

## `hie.yaml`

`package.yaml` を書いた後、 `gen-hie` ([implicit-hie]) を使って `hie.yaml` を生成します。これが無いと HLS が正常に動きませんでした:

```sh
$ gen-hie > hie.yaml
```

<details>
<summary><code>hie.yaml</code></summary>

```yaml
cradle:
  stack:
    - path: "./a/Main.hs"
      component: "abc279:exe:a-exe"

    - path: "./b/Main.hs"
      component: "abc279:exe:b-exe"

    - path: "./c/Main.hs"
      component: "abc279:exe:c-exe"

    - path: "./d/Main.hs"
      component: "abc279:exe:d-exe"

    - path: "./e/Main.hs"
      component: "abc279:exe:e-exe"

    - path: "./ex/Main.hs"
      component: "abc279:exe:ex-exe"

    - path: "./f/Main.hs"
      component: "abc279:exe:f-exe"

    - path: "./g/Main.hs"
      component: "abc279:exe:g-exe"
```

> `abc279` の部分は現在のディレクトリ名になります。

</details>

## Haskell のテンプレート

### Stack script

僕のテンプレートでは以下のようなファイルを使っています:

```hs
#!/usr/bin/env stack
{- stack script --resolver lts-16.11
--package array --package bytestring --package containers
--package vector --package vector-algorithms --package primitive --package transformers
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase, MultiWayIf, PatternGuards, TupleSections #-}
{-# LANGUAGE NumDecimals, NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- ORMOLU_ENABLE -}

-- 関数など
```

`Main.hs` を [cabel script / stack script](https://zenn.dev/mod_poppo/articles/haskell-script) にすると、実行ファイルとして扱うことができます。環境構築が楽な反面、 stack script の実行速度は `oj` によると 1 秒を超えるため、実行時間の正確な見積もりができなくなるのが欠点です。

`stack` でビルド・実行する環境を作った方が良いとは思うのですが……

### Stack script の関数を REPL から呼ぶ

デバッグのため、 `Main.hs` の関数を REPL から動作確認したい場合があります。

`ghci` から `:load` するとコメントが無視されるため、 Stackage のパッケージの `import` に失敗します。代わりに `stack repl <file>` で stack script をロードできます:

```
$ stack repl a/Main.hs
Package name not specified, inferred "abc279"
Using configuration for abc279:exe:a-exe to load /path/to/abc279/a/Main.hs
Using main module: 1. Package `abc279' component abc279:exe:a-exe with main-is file: /path/to/abc-hs/abc279/a/Main.hs
Building all executables for `abc279' once. After a successful build of all of them, only specified executables will be rebuilt.
abc279> initial-build-steps (exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: abc279
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( /path/to/abc-hs/abc279/a/Main.hs, interpreted )

<中略 警告など>

Ok, one module loaded.
Loaded GHCi configuration from /private/var/folders/w1/gylc7bkj22z1wxrwkdk6jg040000gr/T/haskell-stack-ghci/3a2d78e8/ghci-script
*Main>
```

あるいは `stack repl` とだけ打つと、起動時にロードするファイルを選択できます:

```
$ stack repl
Package name not specified, inferred "abc279"

* * * * * * * *
The main module to load is ambiguous. Candidates are:
1. Package `abc279' component abc279:exe:a-exe with main-is file: /path/to/abc-hs/abc279/a/Main.hs
2. Package `abc279' component abc279:exe:b-exe with main-is file: /path/to/abc-hs/abc279/b/Main.hs
3. Package `abc279' component abc279:exe:c-exe with main-is file: /path/to/abc-hs/abc279/c/Main.hs
4. Package `abc279' component abc279:exe:d-exe with main-is file: /path/to/abc-hs/abc279/d/Main.hs
5. Package `abc279' component abc279:exe:e-exe with main-is file: /path/to/abc-hs/abc279/e/Main.hs
6. Package `abc279' component abc279:exe:ex-exe with main-is file:/path/tos/abc-hs/abc279/ex/Main.hs
7. Package `abc279' component abc279:exe:f-exe with main-is file: /path/to/abc-hs/abc279/f/Main.hs
8. Package `abc279' component abc279:exe:g-exe with main-is file: /path/to/abc-hs/abc279/g/Main.hs
You can specify which one to pick by:
 * Specifying targets to stack ghci e.g. stack ghci abc279:exe:a-exe
 * Specifying what the main is e.g. stack ghci --main-is abc279:exe:a-exe
 * Choosing from the candidate above [1..8]
* * * * * * * *

Specify main module to use (press enter to load none): 1
Loading main module from candidate 1, --main-is /path/to/abc279/a/Main.hs

Building all executables for `abc279' once. After a successful build of all of them, only specified executables will be rebuilt.
abc279> initial-build-steps (exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: abc279
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( /path/to/abc279/a/Main.hs, interpreted )

<中略 警告など>

Ok, one module loaded.
Loaded GHCi configuration from /private/var/folders/w1/gylc7bkj22z1wxrwkdk6jg040000gr/T/haskell-stack-ghci/d02d2a00/ghci-script
*Main>
```

> TODO: ファイルの更新があった場合、 `:load` では `--resolver` などのコメントの更新に対応できない気がしますが……

## 参考

- [プロジェクトを作らずにHaskellをやる](https://zenn.dev/mod_poppo/articles/haskell-without-project)

