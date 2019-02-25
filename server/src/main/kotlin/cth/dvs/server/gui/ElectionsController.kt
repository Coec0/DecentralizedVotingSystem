package cth.dvs.server.gui

import cth.dvs.server.DatabaseSupplier
import cth.dvs.server.pojo.Election
import tornadofx.Component
import tornadofx.ScopedInstance

public class ElectionsController : ScopedInstance {
    fun getAllElections():List<Election>{
        return DatabaseSupplier.getElections()
    }
}