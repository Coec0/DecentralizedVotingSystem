package cth.dvs.server

import spark.kotlin.*

object Main{

    @JvmStatic
    fun main(args: Array<String>) {

        val http: Http = ignite().port(8080)

        http.get("/hello") {
            "Hello world from the server!\n"
        }


    }
}