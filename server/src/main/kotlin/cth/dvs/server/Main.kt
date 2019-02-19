package cth.dvs.server

import spark.kotlin.*

object Main{

    @JvmStatic
    fun main(args: Array<String>) {

        val http: Http = ignite().port(8080)

        val x : RouteHandler;

        http.get("/hello") {
            makeCORS()
            "Hello world from the server!\n"
        }



    }
}