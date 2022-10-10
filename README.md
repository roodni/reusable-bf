# bf-reusable

brainfuckに変換されるプログラミング言語です。
- 抽象化されたポインタ操作
- brainfuckに近い命令セット
- ML風のメタプログラミング機構

## Hello, World!
  ```
  let fix f =
    let g x = f (fun y -> x x y) in
    g g

  let gen_puts l = [
    $alloc { c } in
    *fix
      (fun loop prev l ->
        match l with
        | nil -> []
        | hd :: tl -> [
            + c (hd - prev)
            . c
            *loop hd tl
          ])
      0 l
  ]

  main {} [ *gen_puts "Hello, World!\n" ]
  ```

## 資料
* https://www.slideshare.net/roodni/brainfuckbfreusable
  * 情報がやや古いです

## 依存ソフトウェア

https://opam.ocaml.org/

```
opam install dune menhir fileutils ounit2
```

## 実行

コンパイルする
```
dune exec main -- file.bfr
```

コンパイルして実行する
```
dune exec main -- -r file.bfr
```

brainfuckのプログラムを実行する
```
dune exec main -- -b file.b
```

## 実行例

* `sample/hello.bfr`: ハローワールド
```
dune exec main -- -r sample/hello.bfr
```

* `sample/bfi.bfr`: brainfuckインタプリタ

```
mkdir _sandbox
dune exec main -- sample/bfi.bfr > _sandbox/bfi.b
dune exec main -- sample/hello.bfr > _sandbox/hello.b
echo '\' >> _sandbox/hello.b

# hello.b を実行する
dune exec main -- -b _sandbox/bfi.b < _sandbox/hello.b

# 自分自身を実行して hello.b を入力に与える
cp _sandbox/bfi.b _sandbox/input.txt
echo '\' >> _sandbox/input.txt
cat _sandbox/hello.b >> _sandbox/input.txt
dune exec main -- -b _sandbox/bfi.b < _sandbox/input.txt
```