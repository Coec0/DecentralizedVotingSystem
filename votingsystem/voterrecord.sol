pragma solidity >=0.5.1 <0.6.0;

/**
 * @title A whitelist to keep track of addresses that may vote
 * @notice Admins will be added in the constructor. These addresses are the only ones
 * that can edit and activate/deactivate the whitelist
 */
contract VoterRecord {
    
    mapping(address => bool) public whitelist; //The accounts that are allowed to vote
    mapping(address => bool) private adminMap; //The admins that may edit the whitelist
    
    bool private isWhitelistEnabled;
    
    
    /**
     * @param admins The addresses that are allowed to add and remove from the whitelist.
     * The addresses can also activate and deactivate the whitelist
     */
    constructor(address[] memory admins) public{
        for(uint i = 0; i < admins.length; i++){
            adminMap[admins[i]] = true;
        }
    }

    /**
     * @notice Adds a voter to the whitelist. You're required to be an admin
     * to add a voter. 
     * @param adr The address that should be added to the whitelist
     */
    function addVoterToWhitelist(address adr) public {
        require(adminMap[msg.sender], "You are not admin");
        whitelist[adr] = true;
    }
    
    /**
     * @notice Removes a voter from the whitelist, disallowing it to vote. You're 
     * required to be an admin to do this
     * @param adr The address that should be removed from the whitelist
     */
    function removeVoterFromWhitelist(address adr) public{
        require(adminMap[msg.sender], "You are not admin");
        whitelist[adr]= false;
    }
    
    /**
     * @notice Checks if and address is on the whitelist. If the whitelist is disabled,
     * then all persons will appear to be on the whitelist
     * @param adr The address to check if it is on the whitelist
     * @return True or False whether the address is on the list or not. If the list is
     * disabled, then it will always return True
     */
    function isOnWhiteList(address adr) public view returns (bool){
        if(!isWhitelistEnabled){ return true;}
            return whitelist[adr];
    }
    
    /**
     * @notice Enables the whitelist. Requires you to be an admin
     */
    function enableWhitelist () public {
        require(adminMap[msg.sender], "You are not admin");
        isWhitelistEnabled = true;
    }
    
     /**
     * @notice Disables the whitelist. Requires you to be an admin
     */
    function disableWhitelist () public {
        require(adminMap[msg.sender], "You are not admin");
        isWhitelistEnabled = false;
    }
}