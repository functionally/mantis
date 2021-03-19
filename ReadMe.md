Mantis Tools for Cardano
========================

This tool posts metadata or mints/burns tokens. It gathers the UTxO from the address into a transaction with output to that same address, but minting/burning tokens and/or posting metadata in the process. The cost is the minimum network fee for the transaction.


Required environment variable
-----------------------------

The tool requires the environment variable `CARDANO_NODE_SOCKET_PATH` to be set. For example,

	export CARDANO_NODE_SOCKET_PATH=$USER/.local/share/Daedalus/$NETWORK/cardano-node.socket

where `$NETWORK` is `mainnet` or `testnet`.


Command-line options
--------------------

	$ mantis --help
	Mantis Cardano tool.
	
	Usage: mantis [--version] CONFIG [TOKEN] [--count ARG] [--before ARG] 
	              [--metadata ARG]
	  Submit Cardano metadata or mint Cardano tokens.
	
	Available options:
	  -h,--help                Show this help text
	  --version                Show version.
	  CONFIG                   Path to configuration file.
	  TOKEN                    Name of token to mint or burn.
	  --count ARG              Number of tokens to mint or burn.
	  --before ARG             Number of slots into the future when tokens are mintable/burnable.
	  --metadata ARG           Path to metadata JSON file.


Sample configuration file
-------------------------

	$ cat sample.mantis 
	Configuration {
	  magic               = Just 1097911063
	, epochSlots          = 21600
	, addressString       = "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
	, verificationKeyFile = "payment.vkey"
	, signingKeyFile      = "payment.skey"
	}

