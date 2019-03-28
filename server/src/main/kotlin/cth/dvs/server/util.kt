package cth.dvs.server

import com.github.javafaker.Faker
import com.google.gson.Gson
import com.google.gson.JsonArray
import com.google.gson.JsonObject
import cth.dvs.server.pojo.Election
import io.jsondb.InvalidJsonDbApiUsageException
import io.jsondb.JsonDBTemplate
import javafx.collections.ObservableList
import spark.kotlin.RouteHandler
import tornadofx.observable
import java.lang.Exception
import java.lang.StringBuilder
import java.util.concurrent.TimeUnit


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

    this.expirationDate = Faker.instance().date().future(10, TimeUnit.DAYS).time;

    this.abi = Faker.instance().numerify('#'.repeat(Faker.instance().number().numberBetween(50, 200)))

    return this
}

fun Long.isInTheFuture(): Boolean {
    val currTimestamp = System.currentTimeMillis() / 1000
    return (this ?: 0) > currTimestamp
}

fun Election.isActive() = this.expirationDate.isInTheFuture()

object DatabaseSupplier {
    const val DB_PATH = "./db"
    const val POJO_PACKAGE = "cth.dvs.server.pojo"

    var db: JsonDBTemplate


    init {
        db = JsonDBTemplate(DB_PATH, POJO_PACKAGE)


        try {
            db.createCollection(Election::class.java)


        } catch (e: InvalidJsonDbApiUsageException) {
            // The collection is already existing
        }

    }


    public fun init() {

    }


    public fun findAllElections(): String {

        val res = db.getCollection(Election::class.java)
        val jsonArr = JsonArray(res.size)

        res.filter {
            it.isActive()
        }.forEach {
            val e = JsonObject()
            e.addProperty("id", it.id)
            e.addProperty("name", it.name)
            jsonArr.add(e)
        }

        return jsonArr.toString()
    }

    public fun getElections(): ObservableList<Election> {
        val res = db.getCollection(Election::class.java)
        return res.observable()
    }

    public fun findElectionById(id: String): String {
        val res = db.findById(id, Election::class.java)

        return if (res == null || !res.isActive()) {
            "{}"
        } else {
            Gson().toJson(res).toString()
        }

    }


    fun addFromJson(rawData: String): Boolean {

        try {
            val gson = Gson()

            val election = gson.fromJson(rawData, Election::class.java)

            db.insert<Election>(election)

            return true
        } catch (e: Exception) {
            return false
        }


    }
}

data class Result(val result: String, val success: Boolean)
