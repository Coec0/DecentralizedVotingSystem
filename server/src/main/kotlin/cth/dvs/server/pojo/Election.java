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


        System.out.println("Start");
        System.out.println(String.valueOf(contracts.size()));
        contracts.forEach((s, contract) -> {
            System.out.println(contract.getAbi());
            System.out.println(contract.getBcAddr());
        });

        System.out.println("End");
        return  contracts;
//        Integer[] spam = new Integer[] { 1, 2, 3 };
//        return Arrays.asList(spam);
//        return new LinkedList<>(contracts.values());
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

    private Map<String,Contract> contracts;

}
