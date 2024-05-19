# bf-reusable

brainfuckに変換されるプログラミング言語です。
- 抽象化されたポインタ操作
- brainfuckに近い命令セット
- ML風のメタプログラミング機構

## 例

[Playgroundで試せます](https://roodni.github.io/bf-reusable-playground-frontend/)

```
// Hello World!

// 文字列を引数に取り、それを出力する命令列を返す
let gen_puts s = [
  $alloc { c }

  **let rec loop prev = function
      | nil -> []
      | hd :: tl -> [
          + c (hd - prev)
          . c
          *loop hd tl
        ]
    in $
  
  *loop 0 (string_to_list s)
]

let main = [ *gen_puts "Hello World!\n" ]
```

## インストール

### 準備
1. [opam](https://opam.ocaml.org/) をインストールする
2. opamで ocaml (>= 4.14) をインストールする

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

* `examples/hello.bfr`: Hello World!
  ```sh
  bfre -r examples/hello.bfr
  ```

* `examples/bfi.bfr`: brainfuckインタプリタ
  ```sh
  mkdir _sandbox
  cd _sandbox
  bfre ../examples/bfi.bfr > bfi.b
  bfre ../examples/hello.bfr > hello.b

  # hello.b を実行する
  echo '\' >> hello.b
  bfre -b bfi.b < hello.b

  # 自分自身を実行して hello.b を入力に与える
  cp bfi.b input.txt
  echo '\' >> input.txt
  cat hello.b >> input.txt
  bfre -b bfi.b < input.txt
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

これらの操作は、処理系によっては、セルの中身が負である場合にエラーや無限ループを引き起こす可能性があります。以下の事項に留意することで、セルの中身が一時的に負になるようなプログラムを動作させられます。
* `$alloc`のスコープの終わりの時点でセルの中身を非負にする。
* インデックスシフト文 (`> a@i` `< a@i`) の時点でセルの中身を非負にする。

```
(* 例 *)
$alloc { x }

, x
- x 'A'

? x
  [ (* 入力された文字は A でない *) ]
  [ (* 入力された文字は A である *) ]

+ x 'A'  (* 非負になるように足す *)
```

-->