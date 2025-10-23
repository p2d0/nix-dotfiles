{ lib }:

lib.runTests {

  testSlurpJson = {
    expr = {config = builtins.fromJSON ''{
      "QV2RAY_OUTBOUND_METADATA": {
        "displayName": "test"
      },
        "_QV2RAY_USE_GLOBAL_FORWARD_PROXY_": false,
        "mux": {
          "concurrency": 1,
          "enabled": false
        },
        "protocol": "shadowsocks",
        "sendThrough": "0.0.0.0",
        "settings": {
          "servers": [
            {
              "address": "pogpogvpnvpnpogpog.fly.dev",
              "method": "chacha20-ietf-poly1305",
              "password": "barbus007",
              "port": 443
            }
          ]
        },
        "streamSettings": {
          "network": "ws",
          "security": "tls",
          "wsSettings": {
            "path": "/v2"
          }
        },
        "tag": "Personal"
    }'';
           };
    expected = { config = "y";};
  };
}
