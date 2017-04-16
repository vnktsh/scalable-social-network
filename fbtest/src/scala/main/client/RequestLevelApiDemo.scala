package spray.examples

import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.http._
import spray.can.Http
import HttpMethods._
import spray.httpx.SprayJsonSupport
import spray.httpx.unmarshalling._
import spray.httpx.unmarshalling.FromResponseUnmarshaller
import spray.json
import spray.json._
import MediaTypes._
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.json.DefaultJsonProtocol
import java.io._
import scala.io.{Source}


object JsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val PhotoFormat=jsonFormat2(PhotoPost)
  //implicit val AlbumFormat=jsonFormat1(Album)
}

case class PhotoPost(albumName:String,imageData:Array[Byte])
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    return this.toJson.toString()
  }
}



trait RequestLevelApiDemo {
  private implicit val timeout: Timeout = 5.seconds

  // The request-level API is the highest-level way to access the spray-can client-side infrastructure.
  // All you have to do is to send an HttpRequest instance to `IO(Http)` and wait for the response.
  // The spray-can HTTP infrastructure looks at the URI (or the Host header if the URI is not absolute)
  // to figure out which host to send the request to. It then sets up a HostConnector for that host
  // (if it doesn't exist yet) and forwards it the request.
  def crons:Unit = {
    Thread.sleep(5000)
  }
  def getUri(s:String):String = {
    println("------")
    println("SENDING: "+s)
    println("------")
    s
  }

  def demoRequestLevelApi(host: String)(implicit system: ActorSystem): Unit = {
    import JsonSupport._
    println("---")
    import system.dispatcher // execution context for future transformation below
    var user = "user12"
    var user2 = "user13"
    var hed = List(RawHeader("CLIENT_USER_ID", user))
    var hed2 = List(RawHeader("CLIENT_USER_ID", user2))

    var uri = getUri("http://"+host+"/newUserRegistration")
    var ent = HttpEntity(`application/json`, """{ "userName" : "user12","password" : "DOS" }""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

/*
    ent = HttpEntity(`application/json`, """{ "userName" : "user13","password" : "DOS" }""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/createProfile")
    ent = HttpEntity(`application/json`, """{ "name" : "tom",
                                              "age" : "2",
                                              "sex" : "cat",
                                              "relationShipStatus" : "complicated"
                                         }""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), headers = hed, entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/updateStatus")
    ent = HttpEntity(`application/json`, """{ "postContent" : "Writing unit test"}""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), headers = hed, entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/updateStatus")
    ent = HttpEntity(`application/json`, """{ "postContent" : "For two users"}""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), headers = hed2, entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user/timeLine")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user2/timeLine")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user2/timeLine/createPost")
    ent = HttpEntity(`application/json`, """{ "text" : "I'm the Stalker!"}""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), headers = hed, entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user2/sendFriendRequest")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user2/timeLine/createPost")
    ent = HttpEntity(`application/json`, """{ "postContent" : "Hello my new friend"}""")
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), headers = hed, entity = ent)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user2/timeLine")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user2/friends")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user/profile")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

    uri = getUri(s"http://$host/user/$user/newsFeed")
    for {
      response <- IO(Http).ask(HttpRequest(GET, Uri(uri), headers = hed)).mapTo[HttpResponse]
    } yield {
      println(response)
    }
    crons

*/
    val file = "image1.jpg"
    val bis = new BufferedInputStream(new FileInputStream(file))
    val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    val photo1=new PhotoPost("defaultAlbum",bArray)
    uri = getUri(s"http://$host/user/$user/uploadPhoto")
    ent = HttpEntity(`application/json`, photo1.makeJsonString())
    hed = List(RawHeader("CLIENT_USER_ID", user))
    for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(uri), entity = ent,headers = hed))
    } yield {
      println("UPLOAD PHOTO:"+response)
    }
    crons
  }
}
