{
	"ganache": {
		"port": 7545,
		"accounts": [
			{ "balance": "0xffffffffffffffff", "secretKey": "0xca6c2c284d4fbd608a96963ee7f55ca290af1d00321cd516afce40c20a56f052" },
			{ "balance": "0xffffffffffffffff", "secretKey": "0x7c572d2fe17784abb606d9d380bee088ddf7b715ff73029070e97460b264d12e" },
			{ "balance": "0xffffffffffffffff", "secretKey": "0xece8556836380b227429473c0f2e8dd13f01dcf61b3b3bc8cf6359009b0683c2" },
			{ "balance": "0xffffffffffffffff", "secretKey": "0xefca9129092f14eac0125e0dceb51925c0cd48e819496ec40fd02df10a015cdc" }
		]
	},
	"record": {
		"name": "VoterRecord",
		"filename": "voterrecord.sol",
		"args": {
			"admins": ["0x38bACc542367B182cC2fECF22eca7F819aB51d6a"]
		}
	},
	"system": {
		"name": "VotingSystem",
		"filename": "votingsystem.sol",
		"args": {
			"candidates": ["Latiif", "Sarah", "Kevin", "Eric", "Zack", "Oscar"],
			"blocksUntilStart": 0,
			"blocksUntilEnd": 10000,
			"admins": ["0x38bACc542367B182cC2fECF22eca7F819aB51d6a"],
			"a": 49,
			"b": 109,
			"p": 541,
			"q": 61,
			"gx": 153,
			"gy": 308,
			"bx": 419,
			"by": 248
		}
	},
	"backend": {
		"port": 8080,
		"elections": [
			{
				"id": 1,
				"name": "Election",
				"nodeAddr": null,
				"contracts": {
					"voterecord": {
						"bcAddr": null,
						"abi": null
					},
					"votesystem": {
						"bcAddr": null,
						"abi": null
					}
				}
			}
		]		
	}
}