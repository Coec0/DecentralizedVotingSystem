package cth.dvs.server.gui
import cth.dvs.server.DatabaseSupplier
import cth.dvs.server.isInTheFuture
import cth.dvs.server.pojo.Election
import javafx.beans.property.SimpleStringProperty
import javafx.collections.FXCollections
import tornadofx.*
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.lang.StringBuilder

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
        right = vbox {

            hbox {
                button("Refresh") {
                    action {
                        myData.setAll(DatabaseSupplier.getElections())
                    }
                }

                button("Copy to clipboard") {
                    action {
                        val sb = StringBuilder()
                                .append("Node Address:\t${currNodeAddr.value}\n")
                                .append("Blockchain Address:\t${currBcAddr.value}\n")
                                .append("ABI:\t${currAbi.value}\n")

                        val clipboard = Toolkit.getDefaultToolkit().systemClipboard
                        clipboard.setContents(StringSelection(sb.toString()), null)
                    }
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
                    //currBcAddr.set(it.bcAddr)
                    //currAbi.set(it.abi)
                }

                columnResizePolicy = SmartResize.POLICY
            }
        }

        left = vbox{
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