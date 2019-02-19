# Function Coventions

All functions that only should be use for debuging starts with "function **debug**Function...". These functions should not be used by the front end or exist in the final product.

# VotingSystem Smartcontracts
Explains all the **public** functions and their in/out-puts
Hint: good for front-end developers looking to hook things up! 

## Types
* Voter:
  * voterAdr : address
  * hasVoted : bool
  * voteID : uint
  
* Candidate:
  * id : uint
  * name : bytes32
  * voteCount : uint

## Constructor
  * constructor(bytes32[] names, blockamount)
    - Takes all the names of starting candidates in bytecode (cheaper gasprice) aswell as the amount of blocks the voting is held for
    
## Public variables

* finishedVoters : Voter[]
  - array containing every voter that has voted

* allCandidates : Candidate[]
  - contains all the candidates that a voter can vote on (index 0 is BlankVote)
  
## Functions
* vote (uint id) -> void 
  - Lets you vote for a candidate witd ID: id. Gives error if voting has stoped.
  - gascost: ????

* getCandidateInLead () -> (uint id, bytes32 name, uint votes)
  - Gives the candidate id currently in the lead
  - gascost: ???
  
* addCandidate(bytes32 name) -> void
  - Adds a candidate to the allCandidate list
  - gascost: ???
  
* isVotingOpen () -> bool
  - is the voting open? find out
  - gascost: ???
  
* blocksLeft () -> uint
  - gives the amount of blocks left until the voting closes. The time this takes will take depends on the blockchain, however on the main ethereum network it is blocksLeft * ~15sec 
  - gascost: ???
  
