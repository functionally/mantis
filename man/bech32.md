# Encoding and decoding Bech32 text


## Encoding

	$ mantis bech32-decode --help
	
	Usage: mantis bech32-decode BECH32
	  Decode a Bech32 string.
	
	Available options:
	  BECH32                   The Bech32 text.
	  -h,--help                Show this help text


## Decoding

	$ mantis bech32-encode --help
	
	Usage: mantis bech32-encode PREFIX DATA
	  Encode a Bech32 string.
	
	Available options:
	  PREFIX                   The human-readable part.
	  DATA                     The data part.
	  -h,--help                Show this help text
