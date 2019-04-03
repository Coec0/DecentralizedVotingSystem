pragma solidity >=0.5.1 <0.6.0;

interface Record {
    function addVoterToWhitelist(address adr) external;
    function removeVoterFromWhitelist(address adr) external;
    function isOnWhiteList(address adr) external view returns (bool);
    function disableWhitelist () external; 
    function enableWhitelist () external;
}

/**
 * @title This contract should be used for voting and adding candidates for the
 * voting system.
 * @notice  It requires a 'Record' contract which handles all the persons
 * that are allowed to vote. The votes should also be encrypted on the front end
 * with homomorphic elgamal to ensure confidentiality.
 * @dev The private key for elgamal must be kept secret!
 */
contract VotingSystem {
    
    //Variables for the Elgamal public key:
    uint private a;  // Constant on the curve
    uint private b;  // Constant on the curve
    uint private p;  // Constant on the curve
    uint private q;  // Ordo on the generator
    uint private gx; // Point x on the generator
    uint private gy; // Point y on the generator
    uint private bx; // Secret key * gen x
    uint private by; // Secret key * gen y
    

    struct Candidate {
        uint id; //A hash of the candidate
        bytes32 name; //The candidates name
    }

     bytes32 private constant NOT_INSTANTIATED = 0x0000000000000000000000000000000000000000000000000000000000000000;

    Candidate[] public allCandidates; //All the candidates
    
    address[] public voters; //All who has voted
    mapping(address => uint[]) public votedFor; //See the encrypted values that an address has voted for
    
    uint private blockStopNumber; //when the block.number reaches this stop the voting
    uint private blockStartNumber; //when the block.number reaches this start the voting
    mapping(uint => uint) public idToIndexMap; //Gets the position of the candidate in allCandidates
    mapping(address => bool) private adminMap; //Gets if an address is an admin

    Record private record; //The voterrecord.sol contract, fed to the constructor
    
    
    /**
     * @dev You can add more candidates later as long as the voting hasn't started.
     * @param candidates An array of names in bytes32 of the candidates you can vote for.
     * @param blocksUntilStart The amount of blocks until the voting starts, calculated from current block
     * @param blocksUntilEnd The amount of blocks until the voting ends, calculated from current block
     * @param voterecordAddress The address where the voterecord (whitelist) is
     * @param admins The addresses (persons) that can add more candidates to vote for
     */
    constructor(bytes32[] memory candidates, uint blocksUntilStart, uint blocksUntilEnd, address voterecordAddress, address[] memory admins,
    uint _a, uint _b, uint _p, uint _q, uint _gx, uint _gy, uint _bx, uint _by) public{ //blockamount = amount of blocks
        
        for(uint i = 0; i < admins.length; i++){
            adminMap[admins[i]] = true;
        }

        //Instantiating the public key variables
        a = _a;
        b = _b;
        p = _p;
        q = _q;
        gx = _gx;
        gy = _gy;
        bx = _bx;
        by = _by;

        //Temp blockstartnumber to allow adding candidates when startnumber=0
        blockStartNumber = 1;

        //Add BlankVote
        addCandidate(0x426c616e6b566f74650000000000000000000000000000000000000000000000);

        //Adds all canidates from the constructor
        for(uint i=0; i < candidates.length; i++){
            addCandidate(candidates[i]);
        }
        
                
        //Sets the block number where to voting will stop and start
        blockStopNumber = blocksUntilEnd + block.number;
        blockStartNumber = blocksUntilStart + block.number;

        
        record = Record(voterecordAddress);
        
        
    }
    
    /**
     * @return Returns the public key, consisting of 8 constants (a,b,p,q,gx,gy,bx,by)
     */
    function getPublicKey() public view returns (uint, uint, uint, uint, uint, uint, uint, uint){
        return (a,b,p,q,gx,gy,bx,by);
    }

    /**
     * @return The amount of candidates
     */
    function candidateCount() public view returns (uint){
        return allCandidates.length;
    }
    
    /**
     * @notice You can only add a candidate if you are an admin and the voting is closed
     * @dev Creates a canidate and its personal 'hash'. Adds the candidate to
     * 'allCandidates' list and maps the 'hash (id)' to the right place in the
     * array using 'idToIndexMap'
     * @param candidate Name of the candidate in bytes32 that should be added
     */
    function addCandidate(bytes32 candidate) public{
        require(adminMap[msg.sender], "You are not admin");
        require(!isVotingOpen(), "Voting is open!");
        require(candidate != NOT_INSTANTIATED, "A candidate may not have 0x00.. as name");
        uint hash = uint( keccak256(abi.encodePacked(candidate,allCandidates.length)));
        
        idToIndexMap[hash] = allCandidates.length;
            
        allCandidates.push(Candidate({
              id: hash,
              name: candidate
        }));
    }

    /**
     * @return The amount of blocks lef until the vote is over
     */
    function blocksLeft () public view returns (uint){
         return blockStopNumber - block.number;
    }

    /**
     * @return True or False corresponding to whether the voting is open or not
     */
    function isVotingOpen () public view returns (bool){
        return block.number <= blockStopNumber && block.number >= blockStartNumber;
    }
    
    /**
     * @notice Checks if a candidate exists
     * @param id The id (hash) of the candidate to shuld be checked
     * @return True or False whether the candidate exists or not
     */
    function doesCandidateExist (uint id) public view returns (bool){
        return allCandidates[idToIndexMap[id]].id == id;
    }
    
    /**
     * @return The number of people who has voted.
     */
    function getNumberOfVoters() public view returns (uint){
        return voters.length;
    }

    /**
     * @notice Takes the encrypted votes and publish them to the blockchain. All votes should
     * be encrypted with elgamal before published. The votes needs to sum up to '1' to be
     * considered a legal vote. This means that the person you vote for should get an
     * encrypted '1', and the rest should get encrypted '0's.
     * @dev The front end should handle the encryption for the client side
     * @param candidates The candidates voted for encrypted with elgamal. The array should
     * be the same length as the amount of candidates
     */
    function vote (uint[] memory candidates) public {
        require(isVotingOpen(), "Voting is closed!");
        require(record.isOnWhiteList(msg.sender), "You are not allowed to vote!");
        require(candidates.length == allCandidates.length, "You have not voted for everyone!");
        
        //If the voter hans't voted before, add to list
      if(votedFor[msg.sender].length == 0){ 
          voters.push(msg.sender);
      }
      
      //Connects this address to the encrypted votes it cast
      votedFor[msg.sender] = candidates; 
    }
}