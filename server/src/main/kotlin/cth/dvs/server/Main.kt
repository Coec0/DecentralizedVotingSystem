package cth.dvs.server

import cth.dvs.server.cli.SettingsCommand
import cth.dvs.server.gui.GUIApp
import spark.kotlin.Http
import spark.kotlin.ignite
import tornadofx.launch

object Main {

    const val DEFAULT_PORT = 8080

    @JvmStatic
    fun main(args: Array<String>) {


        SettingsCommand().main(args)

        println("Running Server on port ${SettingsBundle.PORT}")


        val http: Http = ignite().port(SettingsBundle.PORT)


        DatabaseSupplier.init()

        http.get("/ping") {
            makeCORS()
            "Up and running!\n"
        }


        http.get("/api/getElections") {
            makeCORS()
            makeJSON()
            return@get DatabaseSupplier.findAllElections()
        }


        http.get("/api/getElection/:id") {
            makeCORS()


            val id = request.params(":id") ?: ""
            if (id.isNullOrEmpty()) {
                response.status(400)
                return@get "400 - BAD REQUEST"
            } else {
                makeJSON()
                return@get DatabaseSupplier.findElectionById(id)
            }


        }

        http.post("/api/add", "application/json") {
            makeCORS()

            val result = DatabaseSupplier.addFromJson(request.queryParams().first() ?: "")

            if (result) {

                response.status(200)
                return@post "Added"
            } else {
                response.status(400)
                return@post "Invalid JSON object"
            }

        }

        launch<GUIApp>(args)
    }
}