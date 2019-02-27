package cth.dvs.server.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import cth.dvs.server.SettingsBundle

public class AddCommand : CliktCommand() {

    val node: String by option(help="Node address of the contract").default("")
    val abi: String by option(help="Abi of the contract").default("")
    val sc: String by option(help="Blockchain address of the contract").default("")

    override fun run() {

        try {
            require(abi.isNotEmpty())
            require(node.isNotEmpty())
            require(sc.isNotEmpty())
        }catch (e :IllegalArgumentException){
            return
        }
        SettingsBundle.ELECTION.id = "9090"
        SettingsBundle.ELECTION.name = "Ghost Election"
        SettingsBundle.ELECTION.nodeAddr = node
        SettingsBundle.ELECTION.bcAddr = sc
        SettingsBundle.ELECTION.abi = abi


    }

}