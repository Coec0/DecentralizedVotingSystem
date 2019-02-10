pragma solidity >=0.4.22 <0.6.0;

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

    constructor(bytes32[] memory candidates) public{
        for(uint i=0; i < candidates.length; i++){
            //create new candite for every entry in array.
            allCandidates.push(Candidate({
                id: uint( keccak256(abi.encodePacked(candidates[i],i))),
                name: candidates[i],
                votecount: 0
            }));
        }
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

    //msg.sender is the address of person or
    //other contract that is interacting with
    //contract right now
    function vote (uint id) public {
        if(doesCandidateExist(id) && hasNotVoted(msg.sender)){

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
}
