package cth.dvs.server

import com.github.javafaker.Faker
import cth.dvs.server.pojo.Election
import io.jsondb.InvalidJsonDbApiUsageException
import io.jsondb.JsonDBTemplate
import spark.Request
import spark.Response
import spark.kotlin.RouteHandler
import java.lang.StringBuilder


fun RouteHandler.makeCORS() {
    this.response.header("Access-Control-Allow-Origin", "*")
}

fun RouteHandler.makeJSON() {
    this.response.type("application/json")
}


fun Char.repeat(n: Int): String {
    var sb = StringBuilder()

    repeat(n) {
        sb = sb.append(this)
    }

    return sb.toString()
}

fun Election.randomize(): Election {
    this.id = Faker.instance().number().digits(6)

    this.name = Faker.instance().demographic().demonym() + " Election"
    this.bcAddr = Faker.instance().internet().ipV4Address()
    this.nodeAddr = Faker.instance().internet().ipV4Address()

    this.abi = Faker.instance().numerify('#'.repeat(Faker.instance().number().numberBetween(50,200)))

    return this
}

object DatabaseSupplier {
    const val DB_PATH = "./db"
    const val POJO_PACKAGE = "cth.dvs.server.pojo"

    init {
        val db = JsonDBTemplate(DB_PATH, POJO_PACKAGE)




        try {
            db.createCollection(Election::class.java)


            // Seeding the db

            repeat(10) {
                val e = Election()
                db.insert<Election>(e.randomize())
            }


        }catch (e:InvalidJsonDbApiUsageException){
            // The collection is already existing
        }


    }


    public fun init() {

    }


}