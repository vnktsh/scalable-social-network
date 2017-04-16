
/*
This contains the Datastructure definitions for the client side, with function definitions to convert the datastructures
into JSON strings which are sent through HTTP Post requests.
*/


package fbclient

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json._
import scala.collection.mutable.ArrayBuffer

//Json Conversion Formats for DataStructures.
object JsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val UserFormat = jsonFormat2(UserDataStructure)
  implicit val ProfileFormat=jsonFormat4(Profile)
  implicit val PostFormat=jsonFormat1(post)
  implicit val PhotoFormat=jsonFormat2(PhotoPost)
  //implicit val AlbumFormat=jsonFormat1(Album)
}

//Data Structures Definitions

// User
case class UserDataStructure(userName:String,password:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
}
//User Profile
case class Profile(name:String,age:String,sex:String,relationShipStatus:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
}
//Posts
case class post(postContent:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
}
//Photos
case class PhotoPost(albumName:String,imageData:Array[Byte])
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    return this.toJson.toString()
  }

}
//Albums
case class Album(Album:ArrayBuffer[PhotoPost])
{
  /*  import JsonSupport._
  def makeJsonString(): String =
   {
     return this.toJson.toString()
   }*/
}
