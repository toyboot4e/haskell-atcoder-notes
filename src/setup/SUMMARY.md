# 環境構築

最低限、言語サーバが動いてテストケースを実行できる環境がほしいところです。

[lts-16.11]: https://www.stackage.org/lts-16.11
[implicit-hie]: https://github.com/Avi-D-coder/implicit-hie#readme

## AtCoder ブラウザ拡張

Haskell のハイライトを直したり、問題の難度を視覚化できます。

- [AtCoderのシンタックスハイライトをマシにするUserScriptを作った](https://qiita.com/mod_poppo/items/af11f07169fa9bdab844)  
変数名の `'` 文字が文字列リテラルとして認識される問題を解消できます。

- [AtCoderをするとき、入れておくといい拡張機能など](https://scrapbox.io/magurofly/AtCoder%E3%82%92%E3%81%99%E3%82%8B%E3%81%A8%E3%81%8D%E3%80%81%E5%85%A5%E3%82%8C%E3%81%A6%E3%81%8A%E3%81%8F%E3%81%A8%E3%81%84%E3%81%84%E6%8B%A1%E5%BC%B5%E6%A9%9F%E8%83%BD%E3%81%AA%E3%81%A9)  
問題・ユーザに推定難度・レートに応じた色が付くスクリプトなどがあります。

## Haskell のバージョンについて

[Language Test 202001](https://atcoder.jp/contests/language-test-202001) で AtCoder で使われるシステムが確認できます:

- GHC 8.8.3 (HLS 1.5.1)
- [lts-16.11] (`repa-3.4.1.4` のみ `lts-16.11` 外のパッケージ)

## Haskell のインストール

GHC (コンパイラ) と [HLS](https//github.com/haskell/haskell-language-server) (言語サーバ) は [ghcup](https://www.haskell.org/ghcup/) を使ってインストールします:

```sh
$ ghcup install ghc 8.8.3
$ ghcup install hls 1.5.1
```

> 他のパッケージ管理ツールや `stack` で `ghc` をインストールすると、 `ghcup` と干渉するらしいです。 `ghcup` 以外で入れた Haskell 関係のツールはアンインストールしておくのがおすすめです。

# 参考

- [AtCoder:Haskellの実行環境の再現 - 雑多な感じ](https://scrapbox.io/dragoon8192-main/AtCoder:Haskell%E3%81%AE%E5%AE%9F%E8%A1%8C%E7%92%B0%E5%A2%83%E3%81%AE%E5%86%8D%E7%8F%BE)
- [Haskell Language Server - 雑多な感じ](https://scrapbox.io/dragoon8192-main/Haskell_Language_Server)
- [GHC version support - haskell language server documentation](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)
- [Haskellの環境構築2023](https://zenn.dev/mod_poppo/articles/haskell-setup-2023)

