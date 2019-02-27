package cth.dvs.server

import cth.dvs.server.cli.AddCommand
import cth.dvs.server.cli.PortCommand
import cth.dvs.server.gui.GUIApp
import spark.kotlin.Http
import spark.kotlin.ignite
import tornadofx.launch

object Main {

    const val DEFAULT_PORT  = 8080

    @JvmStatic
    fun main(args: Array<String>) {


        //PortCommand().main(args)


        AddCommand().main(args)

        println("Running Server on port ${SettingsBundle.PORT}")


        val http: Http = ignite().port(SettingsBundle.PORT)


        DatabaseSupplier.init()

        http.get("/hello") {
            makeCORS()
            "Hello world from the server!\n"
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

        http.post("/api/add","application/json"){
            makeCORS()

            val result = DatabaseSupplier.addFromJson( request.queryParams().first() ?:"")

            if (result) {

                response.status(200)
                return@post "Added"
            }else{
                response.status(400)
                return@post "Invalid JSON object"
            }

        }

        launch<GUIApp>(args)
    }
}