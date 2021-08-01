# Mint batches of Cardano non-fungible tokens

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
