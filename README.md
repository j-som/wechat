wechat
=====

微信sdk

Build
-----

    $ rebar3 compile

Dependence
-----
[cowboy](https://github.com/ninenines/cowboy.git)
[jsx](https://github.com/talentdeficit/jsx.git)

Configures
-----
`sys.config`
```
[
  "config/woap.config",
].
```

`onfig/woap.config`
```
[{woap, [
      {http, [{port,8080}]},
      {https, [{port, 8443},{certfile, "/path/to/certfile"},{keyfile, "/path/to/keyfile"}]}
    ]}
].
```
