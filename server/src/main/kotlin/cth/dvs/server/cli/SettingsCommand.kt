package cth.dvs.server.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.google.gson.Gson
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import com.google.gson.JsonPrimitive
import cth.dvs.server.DatabaseSupplier
import cth.dvs.server.SettingsBundle
import cth.dvs.server.pojo.Election
import java.io.File

public class SettingsCommand : CliktCommand() {

    val settings: String by option(help = "Settings for the server").default("")


    override fun run() {

        try {
            require(settings.isNotEmpty());

            val rawJson = File(settings).readText()

            val j = JsonObject()

            val parser = JsonParser()
            val root = parser.parse(rawJson)
            root.asJsonObject["elections"].asJsonArray.forEach {


                it.asJsonObject["contracts"].asJsonObject.entrySet().forEach {
                    val currContract = it.value
                    val sanitizedAbi = if (currContract.asJsonObject["abi"].isJsonNull)
                        "null"
                    else
                        currContract.asJsonObject["abi"].asJsonArray.toString();

                    currContract.asJsonObject.remove("abi")
                    currContract.asJsonObject.add("abi", JsonPrimitive(sanitizedAbi))
                }

            }

            val rawElections = root.asJsonObject["elections"].asJsonArray;

            val elections : List<Election> =  rawElections.map {
                Gson().fromJson(it.toString(),Election::class.java)
            }


            SettingsBundle.PORT = root.asJsonObject["port"].asInt

            elections.forEach {
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