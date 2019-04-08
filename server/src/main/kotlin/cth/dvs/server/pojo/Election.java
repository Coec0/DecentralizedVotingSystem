package cth.dvs.server.pojo;

import io.jsondb.annotation.Document;
import io.jsondb.annotation.Id;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;


@Document(collection = "elections", schemaVersion = "1.0")
public class Election {

    @Id

    private String id;

    public Election(){
        contracts = new HashMap<>();
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

    public List<Contract> getContracts(){
        return new LinkedList<>(contracts.values());
    }

    private String name;

    public Long getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(Long expirationDate) {
        this.expirationDate = expirationDate;
    }

    private transient Long expirationDate;

    private String nodeAddr;

    Map<String,Contract> contracts;

}
