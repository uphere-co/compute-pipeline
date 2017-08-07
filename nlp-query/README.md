# nlp-query

nlp-query is a daemonized app to use nlp-pipeline without dependency on nlp-pipeline itself.

## Usage
> nix-shell shell.nix
> ./dist/build/nlp-query/nlp-query <host> <host-broadcast> <port> <port-broadcast>
<host> and <port> are used by nlp-query app itself. <host-broadcast> and <port-broadcast> are used by
broadcast server which provides process IDs of nlp-query to clients.

To see the working example, you can type
> ./dist/build/nlp-query/nlp-query 127.0.0.1 127.0.0.1 11110 11115
> runhaskell script/test-client.hs

The necessary parameters are hard-coded in the script.