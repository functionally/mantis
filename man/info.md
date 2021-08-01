# Show information about a transaction file or address


## Addresses

	$ mantis info-address --help
	Usage: mantis info-address [ADDRESS]
	  Print information about addresses.
	
	Available options:
	  ADDRESS                  Shelley address.
	  -h,--help                Show this help text


## Transaction files

	$ mantis info-tx --help
	
	Usage: mantis info-tx [TX_FILE]
	  Print contents of transaction files.
	
	Available options:
	  TX_FILE                  Signed transaction file.
	  -h,--help                Show this help text


## Transaction body files

	$ mantis info-txbody --help
	Usage: mantis info-txbody [TXBODY_FILE]
	  Print contents of transaction body files.
	
	Available options:
	  TXBODY_FILE              Transaction body file.
	  -h,--help                Show this help text


## UTxOs

	$ mantis info-utxo --help
	Usage: mantis info-utxo CONFIG_FILE [ADDRESS]
	  Print UTxO information for addresses.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  ADDRESS                  Shelley address.
	  -h,--help                Show this help text
