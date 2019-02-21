package cth.dvs.server

import org.slf4j.Logger
import spark.kotlin.*

object Main {

    const val DEFAULT_PORT  = 8080

    @JvmStatic
    fun main(args: Array<String>) {

        var strport : String
        var port = DEFAULT_PORT
        try {
            if (args.isNotEmpty()) {
                strport = args[0]
                port = Integer.valueOf(strport)
            }
        }
        catch (e : Exception){
        }


        println("Running Server on port $port")

        val http: Http = ignite().port(port)


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