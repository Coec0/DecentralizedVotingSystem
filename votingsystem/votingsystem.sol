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
                id: i,
                name: candidates[i],
                votecount: 0
            }));
        }
    }

    function isValidVote (uint id) public view returns (bool isValid){
        isValid = false;
        for(uint i = 0; i < allCandidates.length ; i++){
            if(allCandidates[i].id == id){
                isValid = true;
            }
        }
    }
    //unnessecary right now but maybe in the future a better check should be implemented
    function hasNotVoted(address prospectVoter) public view returns (bool notVoted){
        notVoted = true;
        for(uint i = 0; i < finishedVoters.length ; i++){
            if(finishedVoters[i].voterAdr == prospectVoter && finishedVoters[i].hasVoted){
                notVoted = false;
            }
        }
    }

   /* function findCandidate(uint id) public view returns (Candidate candite){
        for(uint i = 0; i < allCandidates.length ; i++){
            if(allCandidates[i].id == id){
                candite = allCandidates[i]; //maybe dosent work. Not sure on how objects work
            }
        }
    }*/

    //msg.sender is the address of person or
    //other contract that is interacting with
    //contract right now
    function vote (uint id) public {
        if(isValidVote(id) && hasNotVoted(msg.sender)){

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
