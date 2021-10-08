# bf-reusable

brainfuckにトランスパイルするプログラミング言語

## 資料
* https://www.slideshare.net/roodni/brainfuckbfreusable

## 依存ソフトウェア

https://opam.ocaml.org/

```
opam install dune menhir ounit2 fileutils batteries
```

## 実行

トランスパイルする
```
dune exec main -- file
```

トランスパイルして実行する
```
dune exec main -- -r file
```

## 実行例

* `sample/rev.bfr`: 改行が入力されるまでの入力を逆順に出力するプログラム
```
dune exec main -- -r sample/rev.bfr
```

* `sample/bfi.bfr`: brainfuckインタプリタ

```
mkdir _sandbox
dune exec main -- sample/rev.bfr > _sandbox/rev.txt
echo '\hello' >> _sandbox/rev.txt
dune exec main -- -r sample/bfi.bfr < _sandbox/rev.txt
```