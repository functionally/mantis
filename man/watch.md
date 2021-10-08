# Download information from all blocks and transactions

In the following, the `--continue` flag will keep the program running when the current tip of the chain is reached. Otherwise the program will terminate when it reaches the tip.

If the `--restart` flag, followed by a filename, is used, then the program will attempt to start watching the chain at the point specified in the file. (Because of the possibility of a fork, this is not guaranteed to be possible.) If the file does not exist or is unreadable, then the program will start watching at the very start of the chain. In any case, the program will record in this file the latest point reached. This file can be manually edited to specify the slot number and block header's hash, for instance from the [Cardano Explorer](https://explorer.cardano.org/).


## Download scripts from blockchain

	$ mantra watch-scripts --help
	
	Usage: mantra watch-scripts CONFIG_FILE [--output OUTPUT_DIR]
	  Download scripts used as transaction witnesses.
	
	Available options:
	  CONFIG_FILE              Path to configuration file.
	  --output OUTPUT_DIR      Output directory for script files.
	  --continue               Whether to continue when the current tip of the chain
	                           is reached.
          --restart POINT_FILE     File for restoring and saving current point on the
                                   chain.
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
          --restart POINT_FILE     File for restoring and saving current point on the
                                   chain.
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
          --restart POINT_FILE     File for restoring and saving current point on the
                                   chain.
	  -h,--help                Show this help text
