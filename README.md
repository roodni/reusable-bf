# bf-reusable

brainfuckに変換されるプログラミング言語です。
- 抽象化されたポインタ操作
- brainfuckに近い命令セット
- ML風のメタプログラミング機構
  * Q. これOCamlとかの内部DSLとして作るべきだったんじゃ
  * A. ……

## Hello World!
  ```
  let fix f =
    let g x = f (fun y -> x x y) in
    g g

  let gen_puts l = [
    $alloc { c } in
    *fix
      (fun loop prev l ->
        match l with
        | () -> []
        | hd . tl -> [
            + c (hd - prev)
            . c
            *loop hd tl
          ])
      0 l
  ]

  codegen [ *gen_puts "Hello World!\n" ]
  ```



## インストール

### 準備
1. [opam](https://opam.ocaml.org/) をインストールする
2. opamで ocaml (>= 4.12) をインストールする

### ビルド
```sh
git clone https://github.com/roodni/bf-reusable
cd bf-reusable
opam install .
```

## 実行

* コンパイルする `bfre file.bfr`
* コンパイルして実行する `bfre -r file.bfr`
* brainfuckのプログラムを実行する `bfre -b file.b`

### サンプルプログラムの実行例

* `sample/hello.bfr`: ハローワールド
  ```sh
  bfre -r sample/hello.bfr
  ```

* `sample/bfi.bfr`: brainfuckインタプリタ
  ```sh
  mkdir _sandbox
  bfre sample/bfi.bfr > _sandbox/bfi.b
  bfre sample/hello.bfr > _sandbox/hello.b

  # hello.b を実行する
  echo '\' >> _sandbox/hello.b
  bfre -b _sandbox/bfi.b < _sandbox/hello.b

  # 自分自身を実行して hello.b を入力に与える
  cp _sandbox/bfi.b _sandbox/input.txt
  echo '\' >> _sandbox/input.txt
  cat _sandbox/hello.b >> _sandbox/input.txt
  bfre -b _sandbox/bfi.b < _sandbox/input.txt
  ```

## 資料
* ドキュメントは準備中です
* 解説スライド https://www.slideshare.net/roodni/brainfuckbfreusable
  * 情報が古いです
  * 現状とは構文がすこし違います

<!--
### 負のセルに関する注意
bf-reusableは`$alloc`で確保されたセルに対して以下の操作
* ゼロ初期化 (`[-]`)
* ムーブ (`[->>+<<]` など)

を必要に応じて自動挿入します。

brainfuckの処理系にはセルの中身が負になりうるものがあって、これらの操作がエラーや無限ループになることがあります。そのような処理系であっても、以下の事項に留意することで、セルの中身が一時的に負になるようなプログラムを動作させることができます。
* `$alloc`のスコープの終わりの時点でセルの中身を非負にする。
* インデックスシフト文 (`> a@i` `< a@i`) の時点でセルの中身を非負にする。

```
(* 例 *)
$alloc { x } in

, x
- x 'A'

? x
  [ (* 入力された文字は A でない *) ]
  [ (* 入力された文字は A である *) ]

+ x 'A'  (* 非負になるように足す *)
```

-->