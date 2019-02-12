pragma solidity >=0.5.1 <0.6.0;

contract VotingSystem {

    struct Voter {
        address voterAdr;
        bool hasVoted; //in solidity this is false from start
        uint voteID; //id of the candidate the voter voted for. //and this is zero
    }

    struct Candidate {
        uint id; //TODO: unique id
        bytes32 name;
        uint votecount;
    }

    Candidate[] public allCandidates;
    Voter[] public finishedVoters;
    uint private blockStopNumber; //when the block.number reaches this stop the voting
    address[] whitelist; //The accounts that are allowed to vote
    bool enableWhitelist = false;

    constructor(bytes32[] memory candidates, uint blockamount) public{ //blockamount == amount of blocks
    
        //Add some default accounts that are allowed to vote:
        whitelist.push(0xCA35b7d915458EF540aDe6068dFe2F44E8fa733c);
        whitelist.push(0x14723A09ACff6D2A60DcdF7aA4AFf308FDDC160C);
        whitelist.push(0x4B0897b0513fdC7C541B6d9D7E929C4e5364D2dB);
    
        enableWhitelist = true;
    
        //Sets the block number where to voting will stop
        blockStopNumber = blockamount + block.number;

        //Add BlankVote
        allCandidates.push(Candidate({
            id: uint(keccak256(abi.encodePacked("0x426c616e6b566f7465"))), //maybe id = 0x0000 ???
            name: "0x426c616e6b566f7465", //name = BlankVote //maybe should just be zeroes??
            votecount: 0
        }));

        for(uint i=0; i < candidates.length; i++){
            //create new candite for every entry in array.
            allCandidates.push(Candidate({
                id: uint( keccak256(abi.encodePacked(candidates[i],i))),
                name: candidates[i],
                votecount: 0
            }));
        }
    }

    function blocksLeft () public view returns (uint){
         return blockStopNumber - block.number;
    }

    function isVotingOpen () public view returns (bool){
        if(blocksLeft() <= 0)
            return false;
        return true;
    }
    
    //Checks if an address is on the whitelist
    function isOnWhitelist(address adr) private view returns (bool){
        if(!enableWhitelist){ return false;} //For easy debugging
        for(uint i=0; i<whitelist.length; i++){
            if(adr == whitelist[i]){
                return true;
            }
        }
        return false;
    }

    //from ethereum.stackexchange.com. Author ismael
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

    //with pure you cannot access the contract storage
    function getCandidateStringNameIdx(uint index) public view returns (string memory){
        if(index >= allCandidates.length){
            return "Candidate not found";
        }

        Candidate memory c = allCandidates[index];
        return bytes32ToString(c.name);
    }

    function addCandidate(bytes32 _name) public{
        require(isVotingOpen(), "Voting is closed!");
        allCandidates.push(Candidate({
                id: uint( keccak256(abi.encodePacked(_name,allCandidates.length))),
                name: _name,
                votecount: 0
            }));
    }

    function getCandidateStringNameID(uint id) public view returns (string memory){
        if(!doesCandidateExist(id)){
            return "Candidate not found";
        }
        uint index = getCandidateIndex(id);
        Candidate memory c = allCandidates[index];
        return bytes32ToString(c.name);
    }

    function getCandidateIndex(uint id) private view returns (uint idx){
        require(doesCandidateExist(id),"Error candite doesent exist");
        for(uint i = 0; i < allCandidates.length ; i++){
            if(allCandidates[i].id == id){
                idx = i;
            }
        }
    }

    function doesCandidateExist (uint id) private view returns (bool isValid){
        isValid = false;
        for(uint i = 0; i < allCandidates.length ; i++){
            if(allCandidates[i].id == id){
                isValid = true;
            }
        }
    }
    //unnessecary right now but maybe in the future a better check should be implemented
    function hasNotVoted(address prospectVoter) private view returns (bool notVoted){
        notVoted = true;
        for(uint i = 0; i < finishedVoters.length ; i++){
            if(finishedVoters[i].voterAdr == prospectVoter && finishedVoters[i].hasVoted){
                notVoted = false;
            }
        }
    }


    //Currently Loops through all candidates (o(n)).
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
    function vote (uint id) public {
        require(isVotingOpen(), "Voting is closed!");
        require(doesCandidateExist(id), "Not a valid ID");
        require(hasNotVoted(msg.sender), "You have already voted");
        require(isOnWhitelist(msg.sender), "You are not allowed to vote!");

            for(uint i = 0; i < allCandidates.length ; i++){
                if(allCandidates[i].id == id){
                    allCandidates[i].votecount++; //maybe dosent work. Not sure on how objects work
                    break;
                }
            }
            finishedVoters.push(Voter({
                voterAdr: msg.sender,
                hasVoted: true,
                voteID: id
            }));
    }
}
