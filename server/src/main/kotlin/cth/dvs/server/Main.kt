package cth.dvs.server

import spark.kotlin.*

object Main{

    @JvmStatic
    fun main(args: Array<String>) {

        val http: Http = ignite().port(8080)


        DatabaseSupplier.init()

        http.get("/hello") {
            makeCORS()
            "Hello world from the server!\n"
        }


        http.get("/api/getElections"){
            makeCORS()
            makeJSON()
            return@get DatabaseSupplier.findAllElections()
        }


    }
}