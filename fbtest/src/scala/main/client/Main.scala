package spray.examples

import scala.util.{Failure, Success}
import akka.actor.ActorSystem
import akka.event.Logging

object Main extends App
  with ConnectionLevelApiDemo
  with HostLevelApiDemo
  with RequestLevelApiDemo {

  // we always need an ActorSystem to host our application in
  implicit val system = ActorSystem("simple-example")
  import system.dispatcher // execution context for future transformations below
  val log = Logging(system, getClass)

  // the spray-can client-side API has three levels (from lowest to highest):
  // 1. the connection-level API
  // 2. the host-level API
  // 3. the request-level API
  //
  // this example demonstrates all three APIs by retrieving the server-version
  // of http://spray.io in three different ways

  val host = "127.0.0.1:8080"

  demoRequestLevelApi(host)

}
