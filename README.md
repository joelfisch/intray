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

### Troubleshooting 

#### Permission Denied (publickey)

If you see an error like this during cloning:

```
Permission Denied (publickey)
```

You probably used ssh-based cloning instead of https-based cloning.
Make sure to use the clone command as shown.

#### Could not execute autoexporter

If you see an error like this during building with stack:

```
intray-data > ghc: could not execute: autoexporter
```

You forgot to run `stack install autoexporter`.

#### Aeson exception

If you see an error like this during building with stacK:

```
Aeson exception:
Error in $.packages[10].completed: failed to parse field 'packages': failed to parse field 'completed': [...]
```

You probably cloned an old version and `git pull`-ed recently.
You still need to remove `stack.yaml.lock`.
You will only need to do this once.


## Configuration

### Options

Every configuration option can be specified using command-line flags as well.
See `intray --help` or `intray <command> --help` for more information about the options for each command.

```
--cache-dir:      The cache dir
--data-dir:       The data dir
--sync:           Definitely sync every time it's appropriate
--no-sync:        Never try to sync automatically
--url:            The sync server api url.
--username:       The sync username
--password:       The sync password
```

Every option can also be specified via environment variables.

```
INTRAY_CONFIG_FILE:    Config file
INTRAY_CACHE_DIR:      The cache dir
INTRAY_DATA_DIR:       The data dir
INTRAY_SYNC_STRATEGY:  The sync strategy
INTRAY_URL:            The sync server api url.
INTRAY_USERNAME:       The sync username
INTRAY_PASSWORD:       The sync password
```

Every option can also be specified in the config file.

```
cache-dir:      The cache dir
data-dir:       The data dir
sync:           The sync strategy
url:            The sync server api url.
username:       The sync username
password:       The sync password
```

### Config file location

The `intray` cli application looks for config files in these locations by default, in order:

```
- $XDG_CONFIG_HOME/intray/config.yaml
- $HOME/.config/intray/config.yaml
- $HOME/.intray/config.yaml
```
