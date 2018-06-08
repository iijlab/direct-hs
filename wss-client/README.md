# ws-client

A-little-higher-level WebSockets client.
With [http-client](https://hackage.haskell.org/package/http-client) and [http-client-tls](https://hackage.haskell.org/package/http-client-tls), this package supports `HTTP_PROXY` environment variable and TLS.

## TODO

- Support non-TLS connection via an HTTP proxy server (I have to modify the [websockets](https://hackage.haskell.org/package/websockets) package to do that).
- Add APIs to modify config of both http-client and websockets.
- Test with a mock server.
- Wrap existing APIs in [websockets](https://hackage.haskell.org/package/websockets) to hide the lower level.

<!-- Uncomment after uploading on Hackage.

## Example

An example program is here: [app/Main.hs](app/Main.hs).  
Build the executable by enabling  build-sample flag:

```bash
stack install ws-client --flag ws-client:build-sample
```
-->

