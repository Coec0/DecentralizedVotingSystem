package cth.dvs.server.pojo;

import io.jsondb.annotation.Document;
import io.jsondb.annotation.Id;

import java.util.*;


@Document(collection = "elections", schemaVersion = "1.0")
public class Election {

    @Id

    private String id;

    public Election(){
        contracts = new HashMap<>();
        result = "UNASSIGNED";
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getNodeAddr() {
        return nodeAddr;
    }

    public void setNodeAddr(String nodeAddr) {
        this.nodeAddr = nodeAddr;
    }


    public void addContract(String contractName, Contract contract){
        contracts.put(contractName,contract);
    }

    public void setContracts(Map<String,Contract> contracts){
        this.contracts = new HashMap<>(contracts);
    }
    public Map<String,Contract> getContracts(){
        return  contracts;
    }


    private String name;

    public Long getExpirationDate() {
        return expirationDate;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public void setExpirationDate(Long expirationDate) {
        this.expirationDate = expirationDate;
    }

    private transient Long expirationDate;

    private String nodeAddr;
    private String result;

    private Map<String,Contract> contracts;

}
