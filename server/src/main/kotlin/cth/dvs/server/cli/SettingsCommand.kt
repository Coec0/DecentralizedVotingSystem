package cth.dvs.server.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.google.gson.*
import cth.dvs.server.DatabaseSupplier
import cth.dvs.server.SettingsBundle
import cth.dvs.server.pojo.Election
import java.io.File
import javax.json.JsonString

public class SettingsCommand : CliktCommand() {

    val settings: String by option(help = "Settings for the server").default("")


    override fun run() {

        try {
            require(settings.isNotEmpty());

            val rawJson = File(settings).readText()

            val j = JsonObject()

            val parser = JsonParser()
            val root = parser.parse(rawJson)
            root.asJsonObject["contracts"].asJsonArray.forEach {

                val sanitizedAbi = if (it.asJsonObject["abi"].isJsonNull)
                    "null"
                else
                    it.asJsonObject["abi"].asJsonArray.toString();

                it.asJsonObject.remove("abi")
                it.asJsonObject.add("abi", JsonPrimitive(sanitizedAbi))
            }

            val settingsFile = Gson().fromJson(root.toString(), SettingsFile::class.java)

            SettingsBundle.PORT = settingsFile.port



            settingsFile.contracts.forEach {
                if (it != null) {
                    it.expirationDate = Long.MAX_VALUE
                    DatabaseSupplier.db.insert<Election>(it)
                }
            }

        } catch (e: IllegalArgumentException) {
            return
        }
    }

}