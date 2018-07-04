# wss-client

A-little-higher-level WebSocket client library.  
Thanks to [http-client](https://hackage.haskell.org/package/http-client) and [http-client-tls](https://hackage.haskell.org/package/http-client-tls), this package supports `HTTP_PROXY` environment variable and TLS.

## TODO

- Support non-TLS connection via an HTTP proxy server (I have to add one more API to http-client to check if the connection is proxied package to do that).
- Add APIs to modify config of both http-client and websockets.

## Example

An example program is here: [app/sample.hs](app/sample.hs).  
Build the executable by enabling  build-sample flag:

```bash
stack unpack wss-client
cd wss-client-*
stack init
stack build wss-client --flag wss-client:build-sample
stack exec wss-client-sample
```
