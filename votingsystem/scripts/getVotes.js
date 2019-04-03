// Replace 'host' and 'contractAddress' with the location of the blockchain
// and the address (location) of the smart contract.
//
// The first print is the number of voters.
// The second print is the amount of candidates
// The rest is all the cast votes; first address followed by all its votes.
//
// The script will finish with a bool indicating the execution success

var host = "ws://localhost:7545";
var contractAddress = '0x5b35e2352b32dadf6452cbf09fbf974be88fcf5a';

if (typeof web3 !== 'undefined') {
 web3 = new Web3(web3.currentProvider);
} else {
 // set the provider you want from Web3.providers
 web3 = new Web3(new Web3.providers.WebsocketProvider(host));
}

var abi = [
	{
		"constant": false,
		"inputs": [
			{
				"name": "candidate",
				"type": "bytes32"
			}
		],
		"name": "addCandidate",
		"outputs": [],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "function"
	},
	{
		"inputs": [
			{
				"name": "candidates",
				"type": "bytes32[]"
			},
			{
				"name": "blocksUntilStart",
				"type": "uint256"
			},
			{
				"name": "blocksUntilEnd",
				"type": "uint256"
			},
			{
				"name": "voterecordAddress",
				"type": "address"
			},
			{
				"name": "admins",
				"type": "address[]"
			},
			{
				"name": "_a",
				"type": "uint256"
			},
			{
				"name": "_b",
				"type": "uint256"
			},
			{
				"name": "_p",
				"type": "uint256"
			},
			{
				"name": "_q",
				"type": "uint256"
			},
			{
				"name": "_gx",
				"type": "uint256"
			},
			{
				"name": "_gy",
				"type": "uint256"
			},
			{
				"name": "_bx",
				"type": "uint256"
			},
			{
				"name": "_by",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "constructor"
	},
	{
		"constant": false,
		"inputs": [
			{
				"name": "candidates",
				"type": "uint256[4][]"
			}
		],
		"name": "vote",
		"outputs": [],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"name": "allCandidates",
		"outputs": [
			{
				"name": "id",
				"type": "uint256"
			},
			{
				"name": "name",
				"type": "bytes32"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [],
		"name": "blocksLeft",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [],
		"name": "candidateCount",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [
			{
				"name": "id",
				"type": "uint256"
			}
		],
		"name": "doesCandidateExist",
		"outputs": [
			{
				"name": "",
				"type": "bool"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [],
		"name": "getNumberOfVoters",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [],
		"name": "getPublicKey",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"name": "idToIndexMap",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [],
		"name": "isVotingOpen",
		"outputs": [
			{
				"name": "",
				"type": "bool"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [
			{
				"name": "",
				"type": "address"
			},
			{
				"name": "",
				"type": "uint256"
			},
			{
				"name": "",
				"type": "uint256"
			}
		],
		"name": "votedFor",
		"outputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	},
	{
		"constant": true,
		"inputs": [
			{
				"name": "",
				"type": "uint256"
			}
		],
		"name": "voters",
		"outputs": [
			{
				"name": "",
				"type": "address"
			}
		],
		"payable": false,
		"stateMutability": "view",
		"type": "function"
	}
];

// creation of contract object
var VotingSystemContract = web3.eth.contract(abi);
// initiate contract for an address
var votingSystemInstance = VotingSystemContract.at(contractAddress);

var nmbrOfVotes = votingSystemInstance.getNumberOfVoters();
var nmbrOfCandidates = votingSystemInstance.candidateCount();


console.log(nmbrOfVotes);
console.log(nmbrOfCandidates);
for(var i = 0; i < nmbrOfVotes; i++){
  var voter = votingSystemInstance.voters(i);
  console.log(voter);
  for(var j = 0; j< nmbrOfCandidates; j++){
    console.log(votingSystemInstance.votedFor(voter, j, 0) +" "+
     votingSystemInstance.votedFor(voter, j, 1)+" "+
     votingSystemInstance.votedFor(voter, j, 2) +" "+
     votingSystemInstance.votedFor(voter, j, 3));
  }

}
