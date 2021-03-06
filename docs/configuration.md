# Configure an dsdin node installed using a release binary

This document describes how to configure your dsdin node installed using a release binary for joining a public network of nodes (e.g. testnet) knowing an initial network peer to join.

## Notable user configuration parameters

### Peer-to-peer network

In order for your node to join the testnet, you need to specify in the configuration file, how peers (on the Internet) can contact your node - specifically the TCP port (`sync` > `port` parameter).

(You do not need to specify the host at which your node can be contacted from the Internet, as each peer you ping will infer that from the address of the inbound TCP connection.)

Please notice that, if your node is behind a firewall, you need to open a TCP port in your firewall (`sync` > `port` parameter) and map that port to the one the node actually listens on (`sync` > `port` parameter - the same). If the publicly available port needs to be different from the internal one, you need to set (`sync` > `external_port`) accordingly.

The following example configuration assumes that:
* The listening TCP port on your public IP address is `3015`;
* The listening TCP port on your node is `3115`.

### Channels

In order to be able to connect to your node as a channel participant you need to have
a port set up for channels' websocket communication. That would be the TCP port (`websocket` > `channel` > `port` parameter).

Please notice that, if your node is behind a firewall, you need to open a TCP port in your firewall (the one you want to connect to) and map that port to the one the node actually listens on (`websocket` > `channel` > `port` parameter).

### Keys management

In order for your node to manage the correct account (able to hold tokens on the blockchain), you need to specify in the configuration file the location of your public-private key pair.
The storage of the key pair by the node is basic:
* Each node handles one key pair;
* The key pair is stored on disk;
* The location of the key pair is configurable (`keys` > `dir` parameter);
* The key pair is encrypted with a configurable password stored in clear in the configuration file (`keys` > `password` parameter);
* A fresh key pair is generated if none is found in the configured location.

You do not need to create a key pair yourself: the node will generate one (`keys` > `dir` parameter) in the configured location if none found there.
After the node generates the key pair in the configured location, you should back up that directory (and remember the password): if you destroy the node, you can setup a new node with the same account in order not to lose the tokens you had obtained by mining on the chain.
You shall not share the private key (or the password) with anyone.

## Instructions

The instructions below assume that:
* The node is deployed in directory `/tmp/node`;
* No custom peers are specified under the `peers:` key in the config. If the `peers:` key is undefined, the *testnet* seed peers (built-in in the package source) are used.

If any of the assumptions does not hold, you need to amend the instructions accordingly.

Create the file `/tmp/node/dsdin.yaml` with the following content (amend the `sync` > `port` parameter with your actual value):
```yaml
---
sync:
    port: 3115
    external_port: 3015

keys:
    dir: keys
    password: "secret"

http:
    external:
        port: 3013
    internal:
        port: 3113

websocket:
    internal:
        port: 3114
    channel:
        port: 3014

mining:
    autostart: true

chain:
    persist: true
    db_path: ./my_db
```

The node automatically creates the directory `db_path`, for storing the blockchain, if not present.

Note that YAML files have significant whitespace so make sure that you indent the file correctly and that the file ends with a newline.

You can validate the configuration file before starting the node:
```bash
cd /tmp/node
bin/dsdin check_config dsdin.yaml
```
You shall read output like the following:
```
OK
```
If the file is valid YAML but does not contain a valid configuration, it prints a helpful output.
