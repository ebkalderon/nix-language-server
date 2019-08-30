#!/usr/bin/env bash

printf 'Content-Length: 512\r\n\r\n{"jsonrpc":"2.0","method":"initialize","params":{"capabilities":{"textDocument":{"colorProvider":null,"completion":{"completionItem":{"snippetSupport":false}},"signatureHelp":{"signatureInformation":{"parameterInformation":{"labelOffsetSupport":true}}}},"workspace":{"applyEdit":true,"didChangeWatchedFiles":{"dynamicRegistration":true}}},"processId":41790,"rootPath":"/Users/eyalkalderon/Documents/nix-language-server","rootUri":"file:///Users/eyalkalderon/Documents/nix-language-server","trace":"off"},"id":10}'
# sleep 1
printf 'Content-Length: 52\r\n\r\n{"jsonrpc":"2.0","method":"initialized","params":{}}'
# sleep 1
printf 'Content-Length: 196\r\n\r\n{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///Users/eyalkalderon/Documents/nix-language-server/foo.nix","languageId":"nix","version":1,"text":"hello"}}}'
# sleep 3
printf 'Content-Length: 44\r\n\r\n{"jsonrpc":"2.0","id":1,"method":"shutdown"}'
# sleep 1
printf 'Content-Length: 33\r\n\r\n{"jsonrpc":"2.0","method":"exit"}'
# sleep 1
printf 'Content-Length: 44\r\n\r\n{"jsonrpc":"2.0","id":1,"method":"shutdown"}'
