# Submit Cardano metadata or mint Cardano tokens

	$ mantra transact --help
	
	Usage: mantra transact CONFIG_FILE [TOKEN] [--count INTEGER] [--expires SLOT] 
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
