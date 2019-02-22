package cth.dvs.server.pojo;

import io.jsondb.annotation.Document;
import io.jsondb.annotation.Id;


@Document(collection = "elections", schemaVersion = "1.0")
public class Election {

    @Id

    private String id;

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

    public String getBcAddr() {
        return bcAddr;
    }

    public void setBcAddr(String bcAddr) {
        this.bcAddr = bcAddr;
    }

    public String getAbi() {
        return abi;
    }

    public void setAbi(String abi) {
        this.abi = abi;
    }

    private String name;

    public Long getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(Long expirationDate) {
        this.expirationDate = expirationDate;
    }

    private transient Long expirationDate;

    private String nodeAddr, bcAddr;

    private String abi;

}
