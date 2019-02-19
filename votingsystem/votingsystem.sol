pragma solidity >=0.5.1 <0.6.0;

contract VotingSystem {

    struct Voter {
        bool whitelisted; //If the voter is part of the "röstlängd"
        uint votedFor; //Id of the candidate the voter voted for.
    }

    struct Candidate {
        uint id; //A hash of the candidate
        bytes32 name; //The candidates name
        uint votecount; //The amount of votes
    }

     bytes32 private constant NOT_INSTANTIATED = 0x0000000000000000000000000000000000000000000000000000000000000000;

    Candidate[] public allCandidates; //All the candidates
    uint private blockStopNumber; //when the block.number reaches this stop the voting
    mapping(address => Voter) public voterMap; //The accounts that are allowed to vote
    mapping(uint => uint) public idToIndexMap; //Gets the position of the candidate in allCandidates
    bool enableWhitelist = false; //Disable the whitlist (röstlängd) until someone is added to it
    
    constructor(bytes32[] memory candidates, uint blockamount) public{ //blockamount == amount of blocks
    
    
        //Sets the block number where to voting will stop
        blockStopNumber = blockamount + block.number;

        //Add BlankVote
        addCandidate(0x426c616e6b566f74650000000000000000000000000000000000000000000000);

        //Adds all canidates from the constructor
        for(uint i=0; i < candidates.length; i++){
            addCandidate(candidates[i]);
        }
        
    }
    
    //Creates a candidate from the name of the candidate and adds it to allCandidates[].
    function addCandidate(bytes32 candidate) public{
        require(isVotingOpen(), "Voting is closed!");
        require(candidate != NOT_INSTANTIATED, "A candidate may not have 0x00.. as name");
        uint hash = uint( keccak256(abi.encodePacked(candidate,allCandidates.length)));
        
        idToIndexMap[hash] = allCandidates.length;
            
        allCandidates.push(Candidate({
              id: hash,
              name: candidate,
              votecount: 0
        }));
    }
    
    //Adds a voter to the whitelist (röstlängd), allowing it to vote.
    //If no voter is added, the whitelist is disabled
    function addVoterToWhitelist(address adr) public {
        enableWhitelist = true;
        voterMap[adr].whitelisted = true;
    }
    
    //Removes a voter from the whitelist(röstlängd), disallowing it to vote
    function removeVoterFromWhitelist(address adr) public{
        voterMap[adr].whitelisted = false;
    }
    
   
    //Returns the amounts of blocks left until the vote is over
    function blocksLeft () public view returns (uint){
         return blockStopNumber - block.number;
    }

    //Checks if the voting is open
    function isVotingOpen () public view returns (bool){
        if(blocksLeft() <= 0)
            return false;
        return true;
    }
    
    //Checks if an address is on the whitelist. If the whitelist
    //isn't enabled then always returns true
    function isOnWhitelist(address adr) private view returns (bool){
        if(!enableWhitelist){ return true;}
        
        return voterMap[adr].whitelisted;
    }

    //Checks if a candidate exists
    function doesCandidateExist (uint id) private view returns (bool){
        return allCandidates[idToIndexMap[id]].id == id;
    }

    //Currently Loops through all candidates (o(n)).
    //Get the candidate thats in lead
    function getCandidateInLead() public view returns
        (uint id, bytes32 name, uint votes){
            for(uint i = 0; i < allCandidates.length ; i++){
                if(allCandidates[i].votecount >= votes){
                    votes = allCandidates[i].votecount;
                    name = allCandidates[i].name;
                    id = allCandidates[i].id;
                }
            }
        }

    //msg.sender is the address of person or
    //other contract that is interacting with
    //contract right now
    //This function votes
    function vote (uint id) public {
        require(isVotingOpen(), "Voting is closed!");
        require(doesCandidateExist(id), "Not a valid ID");
        require(isOnWhitelist(msg.sender), "You are not allowed to vote!");

        if(voterMap[msg.sender].votedFor == 0){ //Have not voted before
            voterMap[msg.sender].votedFor = id;
            allCandidates[idToIndexMap[id]].votecount++;
        } else if (voterMap[msg.sender].votedFor != id){
            allCandidates[idToIndexMap[voterMap[msg.sender].votedFor]].votecount--;
            voterMap[msg.sender].votedFor = id;
            allCandidates[idToIndexMap[id]].votecount++;
        }
    }
    
/*************************** ONLY DEBUG FUNCTIONS BELOW ****************************/

//from ethereum.stackexchange.com. Author ismael
    //Should only be used for debugging
    function bytes32ToString (bytes32 data) internal pure returns (string memory) {
        bytes memory bytesString = new bytes(32);
        for (uint j=0; j<32; j++) {
            byte char = byte(bytes32(uint(data) * 2 ** (8 * j)));
            if (char != 0) {
                bytesString[j] = char;
            }
        }
        return string(bytesString);
    }

    //Adds some accounts to the whitelist. Theese accounts are the first three
    //accounts in the "remix" IDE.
    function debugAddTestWhitelistVoters() public {
        //Add some default accounts that are allowed to vote:
        voterMap[0xCA35b7d915458EF540aDe6068dFe2F44E8fa733c].whitelisted = true;
        voterMap[0x14723A09ACff6D2A60DcdF7aA4AFf308FDDC160C].whitelisted = true;
        voterMap[0x4B0897b0513fdC7C541B6d9D7E929C4e5364D2dB].whitelisted = true;
    
        enableWhitelist = true;
    }



    //with pure you cannot access the contract storage
    //Gets the candidate string name from index in array position
    function debugGetCandidateStringNameIdx(uint index) public view returns (string memory){
        if(index >= allCandidates.length){
            return "Candidate not found";
        }

        Candidate memory c = allCandidates[index];
        return bytes32ToString(c.name);
    }

    //Get candidate string name from its ID.
    function debugGetCandidateStringNameID(uint id) public view returns (string memory){
        if(!doesCandidateExist(id)){
            return "Candidate not found";
        }
        uint index = debugGetCandidateIndex(id);
        Candidate memory c = allCandidates[index];
        return bytes32ToString(c.name);
    }


    //Helper function for debugGetCandidateStringNameID
    function debugGetCandidateIndex(uint id) private view returns (uint idx){
        require(doesCandidateExist(id),"Error candidate doesent exist");
        for(uint i = 0; i < allCandidates.length ; i++){
            if(allCandidates[i].id == id){
                idx = i;
            }
        }
    }

    
}
