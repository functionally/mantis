# Mantis Tools for Cardano

This tool provides several script-oriented utilities for the Cardano blockchain.

In particular, it posts metadata or mints/burns tokens. By default, it gathers the UTxOs from the address into a transaction with output to that same address, but minting/burning tokens and/or posting metadata in the process. The cost is the minimum network fee for the transaction.

It can also generate scripts, compute script addresses, compute the fingerprint of a token, and download scripts.

Please post questions and issues [here](https://github.com/functionally/mantis/issues).


## Installation

This package uses the [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) build system. Simply clone this repository and execute the build command:

	nix-build -A mantis.components.exes.mantis -o build

The executable result will be in `./build/bin/mantis`.

Alternatively, one can use the `cabal install` installation approach, which relies on the [cabal.project](cabal.project) file and which is known to succeed with cabal 3.4.0.0 and ghc 8.10.4.


### Development environment

Due to quirks in how [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) and [`cabal.project`](https://cabal.readthedocs.io/en/3.4/cabal-project.html) interact, the following procedure needs to be followed to create a development environment for compiling `mantis`:

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


*   Sample configuration for `mainnet`: [sample-mainnet.mantis](sample-mainnet.mantis).
*   Sample configuration for `testnet`: [sample-testnet.mantis](sample-testnet.mantis).


## Command-line options

	$ mantis --help
	
	Mantis Cardano tool.
	
	Usage: mantis [--version] [--quiet] COMMAND
	  Utilities for Cardano scripts.
	
	Available options:
	  -h,--help                Show this help text
	  --version                Show version.
	  --quiet                  Minimal output.
	
	Available commands:
	  bech32-decode            Decode a Bech32 string.
	  bech32-encode            Encode a Bech32 string.
	  chain-scripts            Download scripts used as transaction witnesses.
	  fingerprint              Compute the Bech32 fingerprint of a token.
	  info-address             Print information about addresses.
	  info-tx                  Print contents of transaction files.
	  info-txbody              Print contents of transaction body files.
	  info-utxo                Print UTxO information for addresses.
	  mint                     Mint batches of Cardano non-fungible tokens.
	  script                   Construct a minting script and compute its Policy ID.
	  transact                 Submit Cardano metadata or mint Cardano tokens.


### Encoding and decoding Bech32 text


#### Encoding

	$ mantis bech32-decode --help
	
	Usage: mantis bech32-decode BECH32
	  Decode a Bech32 string.
	
	Available options:
	  BECH32                   The Bech32 text.
	  -h,--help                Show this help text

#### Decoding

	$ mantis bech32-encode --help
	
	Usage: mantis bech32-encode PREFIX DATA
	  Encode a Bech32 string.
	
	Available options:
	  PREFIX                   The human-readable part.
	  DATA                     The data part.
	  -h,--help                Show this help text


### Computing the Bech32 fingerprint of a token

	$ mantis fingerprint --help
	
	Usage: mantis fingerprint POLICY_ID ASSET_NAME
	  Compute the Bech32 fingerprint of a token.
	
	Available options:
	  POLICY_ID                Policy ID for the token.
	  ASSET_NAME               Asset name for the token.
	  -h,--help                Show this help text


### Download information from all blocks and transactions


#### Download scripts from blockchain

    Usage: mantis chain-scripts CONFIG_FILE [--output OUTPUT_DIR]
      Download scripts used as transaction witnesses.
    
    Available options:
      CONFIG_FILE              Path to configuration file.
      --output OUTPUT_DIR      Output directory for script files.
      -h,--help                Show this help text


### Show information about a transaction file or address


#### Addresses

	$ mantis info-address --help
	Usage: mantis info-address [ADDRESS]
	  Print information about addresses.
	
	Available options:
	  ADDRESS                  Shelley address.
	  -h,--help                Show this help text


#### Transaction files

	$ mantis info-tx --help
	
	Usage: mantis info-tx [TX_FILE]
	  Print contents of transaction files.
	
	Available options:
	  TX_FILE                  Signed transaction file.
	  -h,--help                Show this help text


#### Transaction body files

	$ mantis info-txbody --help
	Usage: mantis info-txbody [TXBODY_FILE]
	  Print contents of transaction body files.
	
	Available options:
	  TXBODY_FILE              Transaction body file.
	  -h,--help                Show this help text


#### UTxOs

	$ mantis info-utxo --help
	Usage: mantis info-utxo CONFIG_FILE [ADDRESS]
	  Print UTxO information for addresses.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  ADDRESS                  Shelley address.
	  -h,--help                Show this help text


### Construct a minting script and compute its Policy ID

	$ mantis script --help
	
	Usage: mantis script CONFIG_FILE [--expires SLOT] [--script SCRIPT_FILE]
	  Construct a minting script and compute its Policy ID.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  --expires SLOT           Slot number after which tokens are not mintable /
	                           burnable; prefix `+` if relative to tip.
	  --script SCRIPT_FILE     Path to output script JSON file.
	  -h,--help                Show this help text


### Submit Cardano metadata or mint Cardano tokens

	$ mantis transact --help
	
	Usage: mantis transact CONFIG_FILE [TOKEN] [--count INTEGER] [--expires SLOT] 
	                       [--output ADDRESS] [--script SCRIPT_FILE] 
	                       [--metadata METADATA_FILE]
	  Submit Cardano metadata or mint Cardano tokens.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  TOKEN                    Name of token to mint or burn.
	  --count INTEGER          Number of tokens to mint or burn.
	  --expires SLOT           Slot number after which tokens are not mintable /
	                           burnable; prefix `+` if relative to tip.
	  --output ADDRESS         Address for output of transaction.
	  --script SCRIPT_FILE     Path to output script JSON file.
	  --metadata METADATA_FILE Path to metadata JSON file.
	  -h,--help                Show this help text


### Mint batches of Cardano non-fungible tokens.

	$ mantis mint --help
	
	Usage: mantis mint CONFIG_FILE MINTING_FILE [--expires SLOT] [--output ADDRESS] 
	                   [--script SCRIPT_FILE] [--metadata METADATA_FILE]
	  Mint batches of Cardano non-fungible tokens.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  MINTING_FILE             Path to minting JSON file.
	  --expires SLOT           Slot number after which tokens are not mintable /
	                           burnable; prefix `+` if relative to tip.
	  --output ADDRESS         Address for output of transaction.
	  --script SCRIPT_FILE     Path to output script JSON file.
	  --metadata METADATA_FILE Path to output metadata JSON file.
	  -h,--help                Show this help text

The `MINTING_FILE` must be a JSON-formatted object with keys equal to the asset names and values equal to the metadata for that asset. For example, the JSON below will mint two NFTs with commonly used metadata tags, though there is no requirement to use particular tags:

	{
	  "First Token" : {
	    "name"        : "First Token"
	  , "description" : "A first example token
	  , "ticker"      : "TOK-1"
	  , "copyright"   : "(c) 2021 My Name"
	  , "image"       : "ipfs://QmSLFyen7pmtU5wGRwQbjbB5b8c5RKqCzidjbDGnc9ddVV"
	  , "logo"        : "ipfs://QmYb4fMh2ceGwZtkFo97XLznJn2A5BjsxcB5BZpUCgf1ab"
	  , "policy"      : "ipfs://Qme7QmpUJnyEEuqmNWvAMAzu3H3RTUxF4LgmiiuCQ2WRry"
	  , "id"          : "asset1q6nqcr0ynrvqf4ml5n70j87gc7rrh0hsl40peg"
	  },
	  "Second Token" : {
	    "name"        : "Second Token"
	  , "description" : "A second example token
	  , "ticker"      : "TOK-2"
	  , "copyright"   : "(c) 2021 My Name"
	  , "image"       : "ipfs://Qmcb3dyCuNQCj4KkeC8BXBkWX9ToNC3aAtpGTYzG9BMxHi"
	  , "logo"        : "ipfs://QmTzYgJExTjD6nsD6uXthPD6mtkqyzEwt8wxsaWjnwTMR6"
	  , "policy"      : "ipfs://Qme7QmpUJnyEEuqmNWvAMAzu3H3RTUxF4LgmiiuCQ2WRry"
	  , "id"          : "asset13dd0e3hyd94vf2cae0d36xxyqgecz4t5wcxutn"
	  }
	}
