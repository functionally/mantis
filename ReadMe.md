# Mantis Tools for Cardano

This tool provides several script-oriented utilities for the Cardano blockchain.

In particular, it posts metadata or mints/burns tokens. By default, it gathers the UTxOs from the address into a transaction with output to that same address, but minting/burning tokens and/or posting metadata in the process. The cost is the minimum network fee for the transaction.

It can also generate scripts, compute script addresses, and compute the fingerprint of a token.


## Required environment variable

The tool requires the environment variable `CARDANO_NODE_SOCKET_PATH` to be set. For example,

	export CARDANO_NODE_SOCKET_PATH=$USER/.local/share/Daedalus/$NETWORK/cardano-node.socket

where `$NETWORK` is either `mainnet` or `testnet`.


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
	  fingerprint              Compute the Bech32 fingerprint of a token.
	  info-address             Print information about addresses.
	  info-tx                  Print contents of transaction files.
	  info-txbody              Print contents of transaction body files.
	  info-utxo                Print UTxO information for addresses.
	  mint                     Mint batches of Cardano non-fungible tokens.
	  script                   Construct a minting script and compute its Policy ID.
	  transact                 Submit Cardano metadata or mint Cardano tokens.


### Computing the Bech32 fingerprint of a token

	$ mantis fingerprint --help
	
	Usage: mantis fingerprint POLICY_ID ASSET_NAME
	  Compute the Bech32 fingerprint of a token.
	
	Available options:
	  POLICY_ID                Policy ID for the token.
	  ASSET_NAME               Asset name for the token.
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


## Sample configuration files


### For Mainnet

	$ cat mainnet.mantis 
	
	Configuration {
	  magic               = Nothing
	, epochSlots          = 21600
	, addressString       = "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
	, verificationKeyFile = "payment.vkey"
	, signingKeyFile      = "payment.skey"
	}


### For Testnet.

	$ cat testnet.mantis 
	
	Configuration {
	  magic               = Just 1097911063
	, epochSlots          = 21600
	, addressString       = "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
	, verificationKeyFile = "payment.vkey"
	, signingKeyFile      = "payment.skey"
	}
