# bf-reusable

brainfuckにコンパイルされるプログラミング言語

## 資料
* https://www.slideshare.net/roodni/brainfuckbfreusable

## 依存ソフトウェア

https://opam.ocaml.org/

```
opam install dune menhir ounit2 fileutils batteries
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

* `sample/rev.bfr`: 改行が入力されるまでの入力を逆順に出力するプログラム
```
dune exec main -- -r sample/rev.bfr
```

* `sample/bfi.bfr`: brainfuckインタプリタ

```
mkdir _sandbox
dune exec main -- sample/bfi.bfr > _sandbox/bfi.b
dune exec main -- sample/rev.bfr > _sandbox/rev.txt
echo '\rats' >> _sandbox/rev.txt

# rev.txt を実行する
dune exec main -- -b _sandbox/bfi.b < _sandbox/rev.txt

# 自分自身を実行して rev.txt を入力に与える
cp _sandbox/bfi.b _sandbox/bfi.txt
echo '\' >> _sandbox/bfi.txt
cat _sandbox/rev.txt >> _sandbox/bfi.txt
dune exec main -- -b _sandbox/bfi.b < _sandbox/bfi.txt
```