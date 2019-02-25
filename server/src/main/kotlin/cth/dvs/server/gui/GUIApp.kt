package cth.dvs.server.gui
import cth.dvs.server.DatabaseSupplier
import cth.dvs.server.isActive
import cth.dvs.server.isInTheFuture
import cth.dvs.server.pojo.Election
import javafx.beans.property.ReadOnlyListProperty
import javafx.beans.property.SimpleStringProperty
import javafx.collections.FXCollections
import javafx.scene.control.Label
import javafx.scene.layout.HBox
import javafx.scene.layout.Priority
import tornadofx.*
import tornadofx.App
import tornadofx.View
import java.util.*

class GUIApp :App() {
 override val primaryView = Test::class
}


class Test: View("Main"){

    val currName = SimpleStringProperty()
    val currNodeAddr = SimpleStringProperty()
    val currBcAddr = SimpleStringProperty()
    val currAbi = SimpleStringProperty()
    val myData = FXCollections.observableArrayList<Election>()

    override val root = borderpane {
        center = vbox {

            button("Refresh"){
                action {
                    myData.setAll(DatabaseSupplier.getElections())
                }
            }
            tableview<Election>(myData) {
                column("#", Election::getId)
                column("Name", Election::getName)
                column("Ongoing?", Election::getExpirationDate).cellFormat {
                    text = it.isInTheFuture().toString()
                }

                onUserSelect {
                    currName.set(it.name)
                    currNodeAddr.set(it.nodeAddr)
                    currBcAddr.set(it.bcAddr)
                    currAbi.set(it.abi)
                }

                columnResizePolicy = SmartResize.POLICY
            }
        }

        right = vbox{
            form {
                fieldset("Information") {
                    field("Name") {
                        label().bind(currName)
                    }
                    field("Node Address") {
                        label().bind(currNodeAddr)
                    }

                    field("Blockchain Address") {
                        label().bind(currBcAddr)
                    }

                    field("ABI") {
                        label().bind(currAbi)
                    }

                }
            }
        }
    }

}