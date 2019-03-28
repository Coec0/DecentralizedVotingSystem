package cth.dvs.server.cli;

import cth.dvs.server.pojo.Election;

import java.util.ArrayList;

public class SettingsFile {
    int port;
    ArrayList<Election> contracts;

    public SettingsFile() {
        port = 8080;
        contracts = new ArrayList<>();
    }
}
