# bf-reusable

brainfuckにトランスパイルするプログラミング言語

## 依存ソフトウェア

https://opam.ocaml.org/

```
opam install dune batteries menhir ounit2
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

* `demo/rev.bfr`: 改行が入力されるまでの入力を逆順に出力するプログラム
```
dune exec main -- -r demo/rev.bfr
```

* `demo/bfi.bfr`: brainfuckインタプリタ

```
mkdir _sandbox
dune exec main -- demo/rev.bfr > _sandbox/rev.txt
echo '\hello' >> _sandbox/rev.txt
dune exec main -- -r demo/bfi.bfr < _sandbox/rev.txt
```