# ping-pong

## Up and Running

Install dependencies:
```
opam install lwt lwt_ppx
```

To start the server and the client:
```
dune exec ./server.exe
dune exec ./client.exe
```

You can see the available arguments by:

```
dune exec ./client.exe -- -help
```