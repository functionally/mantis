# Download information from all blocks and transactions


## Download scripts from blockchain

	$ mantra watch-scripts --help
	
	Usage: mantra watch-scripts CONFIG_FILE [--output OUTPUT_DIR]
	  Download scripts used as transaction witnesses.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  --output OUTPUT_DIR      Output directory for script files.
	  --continue               Whether to continue when the current tip of the chain
	                           is reached.
	  -h,--help                Show this help text


## Print transactions at an address

	$ mantra watch-address --help
	
	Usage: mantra watch-address CONFIG_FILE [ADDRESS] [--continue]
	  Watch transactions at an address.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  ADDRESS                  Shelley address.
	  --continue               Whether to continue when the current tip of the chain
	                           is reached.
	  -h,--help                Show this help text


## Print transactions involving a policy or asset

	$ mantra watch-coin --help
	
	Usage: mantra watch-coin CONFIG_FILE POLICY_ID [ASSET_NAME] [--continue]
	  Watch transactions for a coin.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  POLICY_ID                Policy ID for the token.
	  ASSET_NAME               Asset name for the token.
	  --continue               Whether to continue when the current tip of the chain
	                           is reached.
	  -h,--help                Show this help text
