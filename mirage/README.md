## Instructions


### Install MirageOS

```
opam install mirage
```

### Configure the application

For testing on Unix:

```
$ mirage configure --network=unix
...
$ make depends
...
$ mirage build
...
$ ls main.native
main.native
```

### Run the application

```
$ ./main.native -p 1234 &
$ nc localhost 1234
# 1 + 2*2
5
# let x: int = 3 in x*x
9
```
