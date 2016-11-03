Nine mens morris in Haskell
====

author:              Felix Moessbauer
maintainer:          moessbauer@cip.ifi.lmu.de

# Description
Implementation of a nine men morris ai that communicates with the LMU NMN gameserver and handles all specified protocol cases.
This implementation fullfills all usefull specifications given here:
http://www.nm.ifi.lmu.de/teaching/Praktika/2014ws/sysprak/

After the build, start a game [here](http://sysprak.priv.lab.nm.ifi.lmu.de/sysprak/NMMorris/) and start the binary with the corresponding gameid and the playernumber.

# Build with cabal
The easiest way to build the client is to call 'cabal build' in the current directory. This builds the binaries with default flags.
After building, binary files are in the dist folder.

### Other possible flags are:
    - debug         enables debug messages and increases verbosity
    - functionalai  use the simple test ai instead of the clever one
    
### Example:

~~~
cabal configure --flags="debug"
cabal configure --flags="debug functionalai"
~~~
    
### Run with cabal
Call `cabal run` to start the client. If no or wrong arguments are passed to the program, it shows a help message.
To start the client with arguments the best way is to do it this way:
`cabal run -- arg1 arg2 ...`
The double dash is to seperate flags sent to cabal and program arguments. If you want to send flags to cabal,
place these between 'run' and the double dash: `cabal run -v -- arg1 arg2 ...`

### Test with cabal
Call `cabal test` to test the AI integrity. If you get a `cabal prelude: no parse` error, try to remove all test
reports and delete the dist directory. This is a cabal bug :(


# Build with stack

You can easily build this client with stack too. It currently supports the latest snapshot lts-7.7 and ghc-8.0.1. If you want to specify an older snapshot, just modify it in the stack.yaml file. (No tests for previous versions yet)
```
$ git clone https://github.com/fmoessbauer/haskell-nm-9morris
$ cd haskell-nm-9morris/
$ stack build
```

Or with flags:
```
$ git clone https://github.com/fmoessbauer/haskell-nm-9morris
$ cd haskell-nm-9morris/
$ stack build --flags haskell-nm-9morris:debug
```

### Run with stack

```
$ cd haskell-nm-9morris/
$ stack exec -- haskell-nm-9morris <gameid> config.ini <playernr>
```

### Test with stack

```
$ cd haskell-nm-9morris/ && stack test
```

## Profiling
If you want to profile, you need to install the profiling version of all libraries. See https://nikita-volkov.github.io/profiling-cabal-projects/
for more information

## Common mistakes
cabal-install fails because the profiling versions of the libaries are not installed:
If you do not need profiling, delete or rename the file cabal.config. Otherwise you have to install the profiling versions with the
reinstall command. See [Profiling](#Profiling)

## No Connection
Be aware, the gameserver is only accessable inside the MWN (Münchner Wissenschaftsnetz). If the client cannot
connect, check your VPN Connection

## Connection gets interrupted
If your internet connection is very slow or has a high latency time, the server might close the connection
in the think phase, because it does not get a response in the expected time window. To check this problem
ping the gameserver and check the round trip time.

If you cannot fix this problem, increase the `aiTimeoutBuffer` set in `Globals.hs`
