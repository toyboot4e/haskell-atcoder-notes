# bytestring

`ByteString` および `ByteStringBuilder` は高速なテキスト処理 (ASCII 限定) に利用します。

詳しくは [【電子版単体】Haskellで戦う競技プログラミング 第2版](https://booth.pm/ja/items/1577541) 第 1 章を参照してください。

## 主な API

- 空文字: `mzero @ BSB.Builder` (だっけ？)
- 連結: `<>`, `mconcat`
- 変換: `BSB.intDec`, `BSB.charUtf8`, `BSB.stringUtf8`
- 出力: `hPutBuilder stdout bs`

## 例

TODO: fold

