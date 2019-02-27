package cth.dvs.server.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.int
import cth.dvs.server.SettingsBundle

class PortCommand : CliktCommand() {

    val port: Int by option(help = "Port to run server on").int().default(8080)

    override fun run() {
        SettingsBundle.PORT = port
    }
}