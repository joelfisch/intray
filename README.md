# Intray


## Installation 

### Cloning

Clone the repository

``` shell
git clone https://github.com/NorfairKing/intray.git --recursive
```


### Building


#### With Nix

To install only the command-line application `intray`:

``` shell
nix-env --install --file nix/pkgs.nix --attr intrayPackages.intray-cli
```

To also install the other intray applications like the server and web server:

``` shell
nix-env --install --file nix/pkgs.nix --attr intrayPackages
```


#### With stack

Follow the instructions at https://docs.haskellstack.org/en/stable/README/
to install `stack`.
For example:

``` shell
curl -sSL https://get.haskellstack.org/ | sh
```

Then install `autoexporter`:

``` shell
stack install autoexporter
```

Finally, install the intray cli:

``` shell
stack install intray
```

To also install the other intray applications like the server and web server:

``` shell
stack install
```

## Troubleshooting 

### Permission Denied (publickey)

If you see an error like this during cloning:

```
Permission Denied (publickey)
```

You probably used ssh-based cloning instead of https-based cloning.
Make sure to use the clone command as shown.

### Could not execute autoexporter

If you see an error like this during building with stack:

```
intray-data > ghc: could not execute: autoexporter
```

You forgot to run `stack install autoexporter`.

### Aeson exceptoin

If you see an error like this during building with stacK:

```
Aeson exception:
Error in $.packages[10].completed: failed to parse field 'packages': failed to parse field 'completed': [...]
```

You probably cloned an old version and `git pull`-ed recently.
You still need to remove `stack.yaml.lock`.
You will only need to do this once.
