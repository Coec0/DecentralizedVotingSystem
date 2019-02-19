package cth.dvs.server

import spark.Redirect
import spark.kotlin.*

object Main {

    @JvmStatic
    fun main(args: Array<String>) {

        val http: Http = ignite().port(8080)


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

    }
}