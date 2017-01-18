


/**
  * Created by Shiva K on 11/25/2015.
  */
  package fbclient


  import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.{Timeout}
import spray.can.Http
import spray.http._
import MediaTypes._
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import java.io._
import scala.io.{Source}
import fbclient.mainglobs.serverAddress

  //MESSAGES
  //Global variable declarations.
  object Globs
  {
    var ctr:Integer=0
    var offsetCounter:FiniteDuration=0 milliseconds
    var port:Integer=7200
    val no_of_clients=20000
  }



  //Definition of the User Actor
  class User(userName:String,password:String) extends Actor with ActorLogging {
    import context._
  implicit val timeout: Timeout = 300.seconds
  var serverAddress = fbclient.mainglobs.serverAddress+":"
  var counter:Int=0
  var photosCounter:Int=Random.nextInt(1001)
  var albumCount:Int=1
  var photochooser=1+Random.nextInt(3)
  var albumchooser=1+Random.nextInt(3)
    var album_counter:Int=Random.nextInt(101)
  override def preStart() = {
    {
      import Globs._
      val newUser = new UserDataStructure(userName, password)
      ctr += 1
      offsetCounter.+(50 milliseconds)
      port = 7200 + ctr % 20
      var portval: String = port.toString()
      var timeOffset:FiniteDuration=offsetCounter
      timeOffset.+(offsetCounter)
      serverAddress += portval

      if(offsetCounter.equals(1000 milliseconds) ||timeOffset.equals(1000 milliseconds))
        {
          offsetCounter.-(1000 milliseconds)
          timeOffset.-(1000 milliseconds)
        }
      for
      {
        response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/newUserRegistration"), entity = HttpEntity(`application/json`, newUser.makeJsonString()))).mapTo[HttpResponse]

      } yield {

        println(response.entity.asString)
      }
      val newProfile=Profile(userName,"22","M","Single")
      for
      {
        response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/createProfile"), headers = List(RawHeader("CLIENT_USER_ID", userName)), entity = HttpEntity(`application/json`, newProfile.makeJsonString()))).mapTo[HttpResponse]

      } yield {
        //println(response.entity.asString)
      }
    system.scheduler.schedule(25000 milliseconds,timeOffset*3,self,schedule())
    }

  }

    def receive ={

      //Main scheduler which periodically starts the request scheduler
      case schedule() => {
        counter+=1
        var Prob_to_send=counter%10
        if(Prob_to_send==0) {
          self ! callSchedulers()
        }
      }
      //Schedulers which call the requests.
      case callSchedulers()=> {
          counter+=1
          photosCounter+=1
          album_counter+=1
          var actionProbability=Array()
          var requestMatch=counter%14

          requestMatch match{
            case 0=> self ! sendFriendRequest(userName)
            case 1=> self !statusUpdate(userName)
            case 2=> self ! viewFriendList(userName)
            case 3 => self ! newsFeed(userName)
            case 4=>self ! viewTimeline(userName)
            case 5=> self ! viewProfile(userName)
            case 6 => self ! statusUpdate(userName)
            case 7 => self ! sendFriendRequest(userName)
            case 8 => self ! newsFeed(userName)
            case 9=> self ! createPost(userName)
            case 10=> self ! createPost(userName)
            case 11=>{
              if(photosCounter%100==0){
              self ! uploadPhoto(userName)
              photosCounter=0
              }
            }
            case 12=>{
              if(photosCounter%500==0){
                self ! viewPhoto(userName)
                photosCounter=0
              }
            }
            case 13=>{
              if(photosCounter%1000==0 && album_counter %100==0 ){
                self ! uploadAlbum(userName)
                photosCounter=0
              }
            }
          }}

      //Updating status message
      //Path example : http://192.168.0.16:1920/user/shivak/statusUpdate
      case statusUpdate(userName: String) => {
        val newPost = post("I am a status update!!")
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/updateStatus"), headers = List(RawHeader("CLIENT_USER_ID", userName)),entity = HttpEntity(`application/json`, newPost.makeJsonString())))
        } yield {
          println("STATUS UPDATE:"+response)
        }
      }

      //Request to create a post.
      //Path example : http://192.168.0.16:1920/user/shivak/createPost
      case createPost(userName: String) => {
        val newPost = post("Hey! its thanksgiving and I'm here coding stuff :(")
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/$userName/timeLine/createPost"), headers = List(RawHeader("CLIENT_USER_ID", userName)),entity = HttpEntity(`application/json`, newPost.makeJsonString())))
        } yield {
          println("CREATE POST:"+response)
        }
      }

      //Request to obtain a user's newsfeed.
      //Path example : http://192.168.0.16:1920/user/shivak/createPost
      case newsFeed(userName: String) => {
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/newsFeed"),headers = List(RawHeader("CLIENT_USER_ID", userName))))
        } yield {
          println("NEWSFEED:"+response)
        }
      }

      //Sending a friend Request
      //Path example : http://192.168.0.16:1920/user/shivak/sendFriendRequest
      case sendFriendRequest(userName: String) => {
        var random:Int=Random.nextInt(100)
        var friend:String ="user"+random
        for {
        response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$friend/sendFriendRequest"), headers = List(RawHeader("CLIENT_USER_ID", userName))))
        } yield {
        println("SEND FRIEND REQUEST:"+response)
        }
      }

      //Uploading an album
      //Path example : http://192.168.0.16:1920/user/shivak/albums/uploadAlbum
      case uploadAlbum(userName: String) => {
        photochooser+=1
        albumchooser+=1
        if(photochooser>3){photochooser=1}
        val file1 = "image"+photochooser+".jpg"
        val bis = new BufferedInputStream(new FileInputStream(file1))
        val bArray = (Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray)
        if(albumchooser>3){albumchooser=1}
        var album_id="album"+albumchooser
        val photo1=new PhotoPost("album"+albumchooser,bArray)
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/$userName/albums/$album_id/uploadAlbum"), entity = HttpEntity(`image/jpeg`, photo1.makeJsonString())))
        } yield {
          println("UPLOAD ALBUM:"+response)
        }
      }

      //Uploading a picture
      //Path example : http://192.168.0.16:1920/user/shivak/photos/uploadPicture
      case uploadPhoto(userName: String) => {
        photochooser+=1
        if(photochooser>3){photochooser=1}
        val file = "image"+photochooser+".jpg"
        val bis = new BufferedInputStream(new FileInputStream(file))
        val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
        val photo1=new PhotoPost("defaultAlbum",bArray)
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/uploadPhoto"), entity = HttpEntity(`application/json`, photo1.makeJsonString()),headers = List(RawHeader("CLIENT_USER_ID", userName))))
        } yield {
          println("UPLOAD PHOTO:"+response)
        }
      }

      //Viewing a user's timeline
      //Path example : http://192.168.0.16:1920/user/shivak/timeline
      case viewTimeline(userName: String) => {
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/timeLine"),headers = List(RawHeader("CLIENT_USER_ID", userName))))
        } yield {
          println("VIEW TIMELINE:"+response)
        }
      }

      //Viewing a user's profile
      //Path example : http://192.168.0.16:1920/user/shivak/profile/
      case viewProfile(userName: String) =>
      {
      for {
        response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/profile"),headers = List(RawHeader("CLIENT_USER_ID", userName))))

      } yield {
        println("VIEW PROFILE:"+response)
      }
      }

      //Viewing a user's picture
      //Path example : http://192.168.0.16:1920/user/shivak/photos/viewPhoto
      case viewPhoto(userName: String) => {
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/albums/defaultAlbum"),headers = List(RawHeader("CLIENT_USER_ID", userName))))
        } yield {
          println("VIEW PHOTO:"+response)
        }
      }

      //Viewing a user's album
      //Path example : http://192.168.0.16:1920/user/shivak/photos/viewAlbum
      case viewAlbum(userName: String) => {
        for {
            response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/albums"),headers = List(RawHeader("CLIENT_USER_ID", userName))))
        }yield {
            println("VIEW ALBUM:"+response)
        }
      }

      //Viewing a user's friendlist
      //Path example : http://192.168.0.16:1920/user/shivak/friends
      case viewFriendList(userName: String) => {
        for{
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/friends"),headers = List(RawHeader("CLIENT_USER_ID", userName))))
        } yield {
          println("VIEW FRIEND LIST:"+response)
        }
      }
    }
  }

  // This actor spawns the clients in the simulator.
  class UserSpawner extends Actor {
    import Globs._
    def receive =
    {
      case StartSpawning => {
        for( i <-0 to no_of_clients ) {
          val UserActor = context.actorOf(Props(new User("user"+i,"badpassword")))
          }
      }
    }
  }

  //Akka Messages
  //User's Creation Messages
  case class createProfile(name:String,gender:String,location:String,birthday:String)
  case class createPost(postContent:String)
  case class uploadPhoto(userName:String)
  case class uploadAlbum(userName:String)
  case class statusUpdate(userName:String)
  case class comeToLife()
  case class schedule()
  //User's read messages
  case class viewProfile(userName:String)
  case class viewTimeline(userName:String)
  case class viewPhoto(userName:String)
  case class viewAlbum(userName:String)
  case class viewFriendList(userName:String)
  case class newsFeed(userName:String)
  // User's other messages
  case class sendFriendRequest(userName:String)
  case class callSchedulers()
  //UserSpawner's messages
  case class StartSpawning()