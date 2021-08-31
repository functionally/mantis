# Mantra Tools for Cardano

This tool provides several script-oriented utilities for the Cardano blockchain.

In particular, it posts metadata or mints/burns tokens. By default, it gathers the UTxOs from the address into a transaction with output to that same address, but minting/burning tokens and/or posting metadata in the process. The cost is the minimum network fee for the transaction.

It can also generate scripts, compute script addresses, compute the fingerprint of a token, and download scripts.

Please post questions and issues [here](https://github.com/functionally/mantra/issues).


## Security considerations

*As is the case for any third-party tool that accesses private/signing keys, one should carefully review the source code for maliciousness or security vulnerabilities and verify the provenance of the executable code.*

Instead of storing the signing key in a file that is referenced by the configuration file, the tool can read the key from a pipe such as standard input or from a Unix socket. For example, if the key file was encrypted using GnuPG, the following command will pipe the decrypted key into the tool:

	gpg -d my-key.skey.gpg | mantra transact my-config.mantra . . .

where the configuration file `my-config.mantra` has set the signing key to `/dev/stdin`. Similar, one can use a Unix socket to achieve a similar result:

	if [ ! -e payment.skey ]
	then
	  mkfifo payment.skey
	fi
	gpg --pinentry loopback --decrypt payment.skey.gpg > payment.skey &
	
	mantra transact my-config.mantra . . .

where the configuration file has set the signing key to the socket `payment.skey`. Both methods avoid storing the unencrypted key in a disk file.


## Installation

This package uses the [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) build system. Simply clone this repository and execute the build command:

	nix-build -A mantra.components.exes.mantra -o build

The executable result will be in `./build/bin/mantra`.

Alternatively, one can use the `cabal install` installation approach, which relies on the [cabal.project](cabal.project) file and which is known to succeed with cabal 3.4.0.0 and ghc 8.10.4.

A docker image is available at https://github.com/wutzebaer/mantra-docker/.


### Development environment

Due to quirks in how [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) and [`cabal.project`](https://cabal.readthedocs.io/en/3.4/cabal-project.html) interact, the following procedure needs to be followed to create a development environment for compiling `mantra`:

1.  Run `nix-shell`. This takes a while to build unless you set `withHoogle = false` in [shell.nix](shell.nix).
2.  Temporarily comment-out the `source-repository-package` lines in [cabal.project](cabal.project).
3.  Run `cabal build`, `hoogle`, or other development tools defined in [shell.nix](shell.nix).


## Configuration file format

The configuration file contains the basic network information, along with the funds address and location of the verification and signing keys.

| Field                 | Description                                                                                          | Example Value                                                                                               |
|-----------------------|------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------|
| `socketPath`          | The path to the `cardano-node` socket.                                                               | `"/home/myusername/.local/share/Daedalus/testnet/cardano-node.socket"`                                      |
| `magic`               | The network magic: `Nothing` for `mainnet`, or `Just` followed by an integer for a test network.     | `Just 1097911063`                                                                                           |
| `epochSlots`          | The number of slots in an epoch on the network, generally always `21600`.                            | `21600`                                                                                                     |
| `addressString`       | The address for the source of UTxOs.                                                                 | `"addr1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"` |
| `verificationKeyFile` | Location of the verification key file for the address.                                               | `"payment.vkey"`                                                                                            |
| `signingKeyFile`      | Location of the signing key file. This may be `/dev/stdin` or a Unix pipe instead of an actual file. | `"payment.skey"`                                                                                            |


*   Sample configuration for `mainnet`: [sample-mainnet.mantra](sample-mainnet.mantra).
*   Sample configuration for `testnet`: [sample-testnet.mantra](sample-testnet.mantra).


## Command-line options

	$ mantra --help
	
	Mantra Cardano tool.
	
	Usage: mantra [--version] [--quiet] COMMAND
	  Utilities for Cardano scripts.
	
	Available options:
	  -h,--help                Show this help text
	  --version                Show version.
	  --quiet                  Minimal output.
	
	Available commands:
	  bech32-decode            Decode a Bech32 string.
	  bech32-encode            Encode a Bech32 string.
	  fingerprint              Compute the Bech32 fingerprint of a token.
	  info-address             Print information about addresses.
	  info-tx                  Print contents of transaction files.
	  info-txbody              Print contents of transaction body files.
	  info-utxo                Print UTxO information for addresses.
	  mint                     Mint batches of Cardano non-fungible tokens.
	  script                   Construct a minting script and compute its Policy ID.
	  transact                 Submit Cardano metadata or mint Cardano tokens.
	  watch-address            Watch transactions at an address.
	  watch-coin               Watch transactions for a coin.
	  watch-scripts            Download scripts used as transaction witnesses.

*   [Mint batches of Cardano non-fungible tokens](man/mint.md)
    *   `mantra mint`
*   [Submit Cardano metadata or mint Cardano tokens](man/transact.md)
    *   `mantra transact`
*   [Construct a minting script and compute its Policy ID](man/script.md)
    *   `mantra script`
*   [Download information from all blocks and transactions](man/watch.md)
    *   `mantra watch-scripts`
    *   `mantra watch-address`
    *   `mantra watch-coin`
*   [Encoding and decoding Bech32 text](man/bech32.md)
    *   `mantra bech32-decode`
    *   `mantra bech32-encode`
*   [Computing the Bech32 fingerprint of a token](man/fingerprint.md)
    *   `mantra fingerprint`
*   [Show information about a transaction file or address](man/info.md)
    *   `mantra info-address`
    *   `mantra info-tx`
    *   `mantra info-txbody`
    *   `mantra info-utxo`


API documentation
-----------------

See https://functionally.github.io/mantra/ for API documentation.
