pragma solidity >=0.5.1 <0.6.0;

interface Record {
    function addVoterToWhitelist(address adr) external;
    function removeVoterFromWhitelist(address adr) external;
    function isOnWhiteList(address adr) external view returns (bool);
    function disableWhitelist () external; 
    function enableWhitelist () external;
}

contract VotingSystem {

    struct Candidate {
        uint id; //A hash of the candidate
        bytes32 name; //The candidates name
    }

     bytes32 private constant NOT_INSTANTIATED = 0x0000000000000000000000000000000000000000000000000000000000000000;

    Candidate[] public allCandidates; //All the candidates
    
    uint[][] public votedfor; //Who is voted for in with 'one' an 'zeroes'
    mapping(address => uint) public votedForPos;
    
    uint private blockStopNumber; //when the block.number reaches this stop the voting
    uint private blockStartNumber;
    mapping(uint => uint) public idToIndexMap; //Gets the position of the candidate in allCandidates
    mapping(address => bool) private adminMap;
    //bool enableWhitelist = false; //Disable the whitlist (röstlängd) until someone is added to it
    Record private record;
    
    constructor(bytes32[] memory candidates, uint blocksUntilStart, uint blocksUntilEnd, address voterecordAddress, address[] memory admins) public{ //blockamount = amount of blocks
        
        for(uint i = 0; i < admins.length; i++){
            adminMap[admins[i]] = true;
        }


        //Temp blockstartnumber to allow adding candidates when startnumber=0
        blockStartNumber = 1;

        //Add BlankVote
        addCandidate(0x426c616e6b566f74650000000000000000000000000000000000000000000000);

        //Adds all canidates from the constructor
        for(uint i=0; i < candidates.length; i++){
            addCandidate(candidates[i]);
        }
        
                
        //Sets the block number where to voting will stop
        blockStopNumber = blocksUntilEnd + block.number;
        blockStartNumber = blocksUntilStart + block.number;

        
        record = Record(voterecordAddress);
        
        
    }

    //Returns amount of candidates
    function candidateCount() public view returns (uint){
        return allCandidates.length;
    }
    
    //Creates a candidate from the name of the candidate and adds it to allCandidates[].
    function addCandidate(bytes32 candidate) public{
        require(adminMap[msg.sender], "You are not admin");
        require(!isVotingOpen(), "Voting is closed!");
        require(candidate != NOT_INSTANTIATED, "A candidate may not have 0x00.. as name");
        uint hash = uint( keccak256(abi.encodePacked(candidate,allCandidates.length)));
        
        idToIndexMap[hash] = allCandidates.length;
            
        allCandidates.push(Candidate({
              id: hash,
              name: candidate
        }));
    }

    //Returns the amounts of blocks left until the vote is over
    function blocksLeft () public view returns (uint){
         return blockStopNumber - block.number;
    }

    //Checks if the voting is open
    function isVotingOpen () public view returns (bool){
        return block.number <= blockStopNumber && block.number >= blockStartNumber;
    }

    //Checks if a candidate exists
    function doesCandidateExist (uint id) public view returns (bool){
        return allCandidates[idToIndexMap[id]].id == id;
    }

    function vote (uint[] memory candidates) public {
        require(isVotingOpen(), "Voting is closed!");
        require(record.isOnWhiteList(msg.sender), "You are not allowed to vote!");
        require(candidates.length == allCandidates.length, "You have not voted for everyone!");
        
        votedForPos[msg.sender] = votedfor.length;
        votedfor.push(candidates);
    }
}