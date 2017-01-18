package fbserver
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import spray.json
import spray.json._

object JsonSupports extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val UserAccountDataFormat = jsonFormat2(UserAccountData)
  implicit val UserProfileFormat = jsonFormat4(UserProfile)
  implicit val WriteOnWallFormat = jsonFormat1(WriteOnWall)
  implicit val StatusUpdateFormat = jsonFormat1(StatusUpdate)
  implicit val PhotoPostFormat = jsonFormat2(PhotoPost)
}

//Classes
case object OutputStats
case class UserAccountData(userName:String, password:String)
case class WriteOnWall(postContent:String)
case class StatusUpdate(postContent:String)
case class PhotoPost(albumName:String,imageData:Array[Byte])


case class UserProfile(
  name:String, age:String,
  sex:String,relationShipStatus:String) 
{
	import JsonSupports._
  	def makeJsonString():String = {
    	return this.toJson.toString()
  	}
}