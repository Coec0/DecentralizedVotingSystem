package cth.dvs.server

import spark.Request
import spark.Response
import spark.kotlin.RouteHandler


fun RouteHandler.makeCORS(){
    this.response.header("Access-Control-Allow-Origin","*")
}