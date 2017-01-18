/**
  * Created by Shiva K on 11/23/2015.
  */

package fbserver

import java.util.concurrent.ConcurrentHashMap

import akka.actor._
import akka.actor.{ActorSystem, Props}
import akka.actor.ActorContext
import akka.actor.ActorSelection
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.can.server.Stats
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import spray.routing._
import MediaTypes._
import scala.concurrent.Future
import scala.concurrent.duration._
import java.io._
import spray.http.{HttpData, MultipartFormData}

import scala.collection.mutable.ListBuffer

object ServerStats {
  var httpRequestsProcessed:Long = 0.toLong
  var prevRequests:Long = 0.toLong
  var upTime:Integer = 0.toInt
  var totalPhotos:Long = 0.toLong
  var totalPosts:Long = 0.toLong
  var totalUsers:Long = 0.toLong
  var totalFriendships:Long = 0.toLong
  var totalNewsFeedRequests:Long = 0.toLong
}

case object DisplayStats


class HttpHandler extends Actor with HttpService with Backend {
  //import context._
  implicit val timeout: Timeout = 1.second
  import context.dispatcher
  import JsonSupports._
  import ServerStats._

  var httpListener : Option[ActorRef] = None

  def actorRefFactory = context

  def httpReceive = runRoute(restAPI)

  def handle : Receive = {
    case Http.Bound(_) =>
      httpListener = Some(sender)
    case Http.Unbound =>
      context.stop(self)
  }

  def receive = handle orElse httpReceive

  val restAPI = {

    path("newUserRegistration") {
      pathEndOrSingleSlash {
        httpRequestsProcessed += 1
        post {
          println("Created New User: ")
          entity(as[UserAccountData]) { userData =>
            respondWithMediaType(`application/json`) {
              complete {
                if(registerNewUser(userData))
                {
                  println("Registering new users in the system...")
                  "Created New User: "+userData.userName
                } else {
                  "User: "+userData.userName+ " already exists"
                }
              }
            }
          }
        }
      }
    } ~
    pathPrefix("user") { headerValueByName("CLIENT_USER_ID") { requesterID =>
      httpRequestsProcessed += 1
      path("createProfile") {
        post {
          entity(as[UserProfile]) { userProfile =>
            respondWithMediaType(`application/json`) {
              complete {
                if(updateProfile(requesterID,userProfile))
                {
                  "Updated profile data "+userProfile.name
                } else {
                  "Some problem updating profile"
                }
              }
            }
          }
        }
      }
    } //end of header
    } //end of /user
  } // end of restApi


} // end of HttpHandler


class StatActor extends Actor {

  import context.dispatcher
  import ServerStats._

  val stat_interval = 2
  val file = new File("stats.txt")
  if (file.exists()) {
      file.delete()
  }
  file.createNewFile()

  val fw1 = new FileWriter(file, true)
  fw1.write("Uptime,Server Requests,Requests/Sec,NewsFeed,Posts,Friendships,Photos")
  fw1.write("\r\n")
  fw1.close()

  context.system.scheduler.schedule(30 seconds,stat_interval seconds,self,DisplayStats)

  def receive = {
    case DisplayStats =>
            upTime = upTime + stat_interval
            var req = httpRequestsProcessed
            var diff = (req - prevRequests)/stat_interval
            prevRequests = req

            val fw = new FileWriter(file, true)
            fw.write(upTime+","+req+","+diff+","+totalNewsFeedRequests+","+totalPosts+","+totalFriendships+","+totalPhotos)
            fw.write("\r\n")
            fw.close()

            println("=======================================")
            println("=========SERVER STATS==================")
            println("Server uptime: " + upTime)
            println("Server requests so far: " + req)
            println("Server requestsPerSec: " + diff + "req/sec")
            println("=======================================")

    case _ => {}
  }
}
