# Show information about a transaction file or address


## Addresses

	$ mantra info-address --help
	Usage: mantra info-address [ADDRESS]
	  Print information about addresses.
	
	Available options:
	  ADDRESS                  Shelley address.
	  -h,--help                Show this help text


## Transaction files

	$ mantra info-tx --help
	
	Usage: mantra info-tx [TX_FILE]
	  Print contents of transaction files.
	
	Available options:
	  TX_FILE                  Signed transaction file.
	  -h,--help                Show this help text


## Transaction body files

	$ mantra info-txbody --help
	Usage: mantra info-txbody [TXBODY_FILE]
	  Print contents of transaction body files.
	
	Available options:
	  TXBODY_FILE              Transaction body file.
	  -h,--help                Show this help text


## UTxOs

	$ mantra info-utxo --help
	Usage: mantra info-utxo CONFIG_FILE [ADDRESS]
	  Print UTxO information for addresses.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  ADDRESS                  Shelley address.
	  -h,--help                Show this help text
