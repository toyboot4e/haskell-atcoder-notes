# text

`text` は Unicode 文字 (?) が扱える文字列のパッケージです。競技プログラミングではあまり使う機会がありません。 `bytestring` が使えたら十分です。

なお AtCoder のバージョンでは `text` の内部実装には UTF-16 が使われていますが、最近の実装では UTF-8 に移行したようです。 AtCoder の外では `text` の天下……なのかもしれません。

## 主な API

- `T.pack`, `T.unpack`
- `T.printf`

