# MinCaml

## これなに

MinCaml の処理系であって F# で書かれており Whitespace を吐くものです。練習用。未対応の機能も多い


## 動かし方

``` console
$ git clone https://github.com/kmyk/min-caml-fsharp-whitespace
$ cd min-caml-fsharp-whitespace
$ dotnet build
$ dotnet run < hoge.ml > hoge.ws
```

.NET のインストール (<https://dotnet.microsoft.com/download>) も必要です。
Whitespace の処理系は各自で用意してください。テスト用に `tests/whitespace.pl` が同梱されており、これを使うこともできます。


## 実行例

``` console
$ cat examples/fact.ml 
let rec mult a b =
    if a = 0 then 0
    else mult (a - 1) b in
let rec fact n =
    if n = 0 then 1
    else mult n (fact (n - 1)) in
fact 10

$ dotnet run < examples/fact.ml > fact.ws

$ perl tests/whitespace.pl fact.ws
3628800
```


## Q & A

-   Q. なぜ MinCaml なの？
    -   A. 練習用なので
-   Q. 独自言語ではないの？
    -   A. 必要もないのに独自性を出しても面倒なだけでしょ
-   Q. なぜ F# なの？
    -   A. F# はいいぞ
-   Q. self-hosting は？
    -   A. self-hosting はロマンはあるけどそれ以外はほぼ何もなくないか
-   Q. なぜ Whitespace なの？
    -   A. Whitespace は x86 よりもよほど低級な言語だし練習用にはよいかなと思ったので
-   Q. バイナリ生成はしないの？
    -   A. 実行ファイルフォーマットに詳しくなりたいわけではなかった
-   Q. レジスタ割り付けしてなくない？
    -   A. Whitespace はレジスタが実質 2 個しかなかったので (誤算1)
-   Q. 浮動小数点数が未対応なのはなぜ？
    -   A. Whitespace に浮動小数点数演算の命令がなくて面倒なので (誤算2)
-   Q. closure が未対応なのはなぜ？
    -   A. Whitespace に実行時にアドレス指定して jump できる命令がなくて面倒なので (誤算3)
-   Q. tuple が未対応なのはなぜ？
    -   A. 飽きたので
-   Q. これ実用性ある？
    -   A. ない
