pragma solidity >=0.5.1 <0.6.0;

contract VoterRecord {
    
    address public owner;
    
    mapping(address => bool) public whitelist; //The accounts that are allowed to vote

    constructor() public {        
    }
    
    bool private isWhitelistEnabled;
    
    //Adds a voter to the whitelist (röstlängd), allowing it to vote.
    //If no voter is added, the whitelist is disabled
    function addVoterToWhitelist(address adr) public {
        whitelist[adr] = true;
    }
    
    //Removes a voter from the whitelist(röstlängd), disallowing it to vote
    function removeVoterFromWhitelist(address adr) public{
        whitelist[adr]= false;
    }
    
    //Checks if an address is on the whitelist. If the whitelist
    //isn't enabled then always returns true
    function isOnWhiteList(address adr) public view returns (bool){
        if(!isWhitelistEnabled){ return true;}
            return whitelist[adr];
    }
    
    function enableWhitelist () public {
        isWhitelistEnabled = true;
    }
    
    function disableWhitelist () public {
        isWhitelistEnabled = false;
    }
}