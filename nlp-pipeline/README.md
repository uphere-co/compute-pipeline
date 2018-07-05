# nlp-pipeline

nlp-pipeline is the app for aggregating and distributing text corpus and its
analysis result. 

## corenlp-runner

This is the app to run the CoreNLP parser and save the result with our own
data type. Saved data will be loaded to be used by our NLP analysis modules.

## analysis-runner

This is the app to run our NLP analysis modules. Each result can be saved
and loaded.

## analysis-daemon

This is the app to feed analysis-runner by articles collected by fetchfin in
real-time.

# Usages of Apps
You can omit argument string by setting them in NIX_PATH.
```
> nix-shell shell.nix --argstr <repo-name> <repo-path>
> cabal build
> ./dist/build/<app-name>/<app-name>
```

To run analysis-daemon, you should give arguments.

```
> ./dist/build/analysis-daemon/analysis-daemon <host> <host-broadcast> <port> <port-broadcast>
```

host and port are used by analysis-daemon itself. host-broadcast and port-broadcast are 
used by broadcast server which provides process IDs of analysis-daemon to clients.

The following is the example.
```
> ./dist/build/nlp-query/nlp-query 127.0.0.1 127.0.0.1 11110 11115
```