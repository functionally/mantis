# Mantis Tools for Cardano

This tool provides several script-oriented utilities for the Cardano blockchain.

In particular, it posts metadata or mints/burns tokens. It gathers the UTxO from the address into a transaction with output to that same address, but minting/burning tokens and/or posting metadata in the process. The cost is the minimum network fee for the transaction.

It can also generate scripts, compute script addresses, and compute the fingerprint of a token.


## Required environment variable

The tool requires the environment variable `CARDANO_NODE_SOCKET_PATH` to be set. For example,

	export CARDANO_NODE_SOCKET_PATH=$USER/.local/share/Daedalus/$NETWORK/cardano-node.socket

where `$NETWORK` is either `mainnet` or `testnet`.


## Command-line options

	$ mantis --help
	
	Mantis Cardano tool.
	
	Usage: mantis [--version] COMMAND
	  Utilities for Cardano scripts.
	
	Available options:
	  -h,--help                Show this help text
	  --version                Show version.
	
	Available commands:
	  fingerprint              Compute the Bech32 fingerprint of a token.
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


### Construct a minting script and compute its Policy ID

	$ mantis script --help
	
	Usage: mantis script CONFIG_FILE [SCRIPT_FILE] [--expires SLOT]
	  Construct a minting script and compute its Policy ID.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  SCRIPT_FILE              Path to script JSON file.
	  --expires SLOT           Slot number after which tokens are not mintable /
	                           burnable; prefix `+` if relative to tip.
	  -h,--help                Show this help text


### Submit Cardano metadata or mint Cardano tokens

	$ mantis transact --help

	Usage: mantis transact CONFIG_FILE [TOKEN] [--count INTEGER] [--expires SLOT] 
	                       [--metadata METADATA_FILE]
	  Submit Cardano metadata or mint Cardano tokens.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  TOKEN                    Name of token to mint or burn.
	  --count INTEGER          Number of tokens to mint or burn.
	  --expires SLOT           Slot number after which tokens are not mintable /
	                           burnable; prefix `+` if relative to tip.
	  --metadata METADATA_FILE Path to metadata JSON file.
	  -h,--help                Show this help text


## Sample configuration file

	$ cat sample.mantis 
	
	Configuration {
	  magic               = Just 1097911063
	, epochSlots          = 21600
	, addressString       = "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
	, verificationKeyFile = "payment.vkey"
	, signingKeyFile      = "payment.skey"
	}
