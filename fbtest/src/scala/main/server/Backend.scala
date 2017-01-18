package fbserver
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import java.io.{File, FileOutputStream}
import spray.http.{HttpData, MultipartFormData}
import Math._


trait Backend {
import ServerStats._


//Data Structures
var userAccountsDataMap = new ConcurrentHashMap[String,UserAccountData]()
var userProfilesDataMap = new ConcurrentHashMap[String,UserProfile]()
var userPostsDataMap = new ConcurrentHashMap[String,String]()
var userFriendListMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()
var userTimelinesMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()
var userNewsFeedsMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()
var userAlbumDataMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()

//Definitions
def registerNewUser(userData: UserAccountData) : Boolean = {
  try {
    if(!userAccountsDataMap.containsKey(userData.userName)) {

      userAccountsDataMap.putIfAbsent(userData.userName,userData)

      var friendList = new ArrayBuffer[String]
      var timeLine = new ArrayBuffer[String]
      var newsFeed = new ArrayBuffer[String]

      userFriendListMap.putIfAbsent(userData.userName, friendList)
      userTimelinesMap.putIfAbsent(userData.userName,timeLine)
      userNewsFeedsMap.putIfAbsent(userData.userName,newsFeed)

      return true
    } 
    return false
  } catch {
      case NonFatal(t) => return false
  }
}

def updateProfile(userID: String,userProfileData: UserProfile) : Boolean = {
  try {
    userProfilesDataMap.putIfAbsent(userID,userProfileData)
    return true
  } catch {
    case NonFatal(t) => return false
  }
}

def updateStatus(userID: String,statusUpdate: StatusUpdate): Boolean = {
  try {
    var uuid = java.util.UUID.randomUUID().toString()
    userPostsDataMap.put(uuid,statusUpdate.postContent)
    userTimelinesMap.get(userID) += uuid
    totalPosts += 1
    return true
  } catch {
    case NonFatal(t) => return false
  }
}

def addFriendship(friendID: String,userID: String): Boolean = {
  try {
    var friendLimit = 500
    if(isSameUser(userID,friendID)) {return false}

    if((userFriendListMap.get(userID).length > friendLimit) ||
      (userFriendListMap.get(friendID).length > friendLimit)) {return false}

    userFriendListMap.get(userID) += friendID
    userFriendListMap.get(friendID) += userID
    totalFriendships += 1
    return true
  } catch {
    case NonFatal(t) => return false
  }
}

def writeOnWall(userID: String, wallPost: WriteOnWall): Boolean = {
  try {
    var uuid = java.util.UUID.randomUUID().toString()
    userPostsDataMap.put(uuid,wallPost.postContent)
    userTimelinesMap.get(userID) += uuid
    totalPosts += 1
    return true
  } catch {
    case NonFatal(t) => return false
  }
}

def serveTimeLine(userID: String): String = {
  try {
    var feedLimit = 20
    userTimelinesMap.get(userID).takeRight(min(feedLimit,userTimelinesMap.get(userID).length)).mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving timeline, try again"
  }
}

def serveUserProfile(userID: String): String = {
  try {
    userProfilesDataMap.get(userID).makeJsonString()
  } catch {
    case NonFatal(t) => "Error in serving profile, try again"
  }
}

def serveFriendList(userID: String): String = {
  try {
    userFriendListMap.get(userID).mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving friendlist, try again"
  }
}

def serveNewsFeed(userID: String): String = {
  try {
    var newsFeed = new ArrayBuffer[String]
    var friendLimit = 10
    var feedLimit = 5
    var fi = 0

    var flist = userFriendListMap.get(userID)

    while( fi < min(friendLimit,flist.length)) {
      var tlist = userTimelinesMap.get(flist(fi))
      newsFeed ++= tlist.takeRight(min(feedLimit,tlist.length))
      fi += 1
    }
    totalNewsFeedRequests += 1
    newsFeed.mkString(",")
  
  } catch {
      case NonFatal(t) => "Error in serving news feed, try again"
  }
}

///*
def uploadPhoto(requesterID:String,photoPost:PhotoPost): Boolean = {
  try {
    //println("IN UPLOAD ALBUM" + requesterID)
    val img_id = updateImageData(requesterID,photoPost.albumName)
    val image_descriptor = new File("media/"+requesterID+"/"+photoPost.albumName+"/"+img_id+".jpg")
    val fw = new FileOutputStream(image_descriptor)
    fw.write(photoPost.imageData)
    fw.close()
    totalPhotos += 1
    return true
  } catch {
    case NonFatal(t) => {
      false
    }
  }
}

def serveAlbumList(requesterID:String): String = {
  try { 
    //println("IN ALBUM DEFAULT PHOTO")
    userAlbumDataMap.get(requesterID+"defaultAlbum").mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving album list" 
  }
}

def servePhotoAlbum(requesterID:String,albumName:String): String = {
  try { 
    //println("IN VIEW PHOTO" + requesterID+albumName)
    userAlbumDataMap.get(requesterID+albumName).mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving photo album"  
  }
}

//*/

//Helper funtions
def isAllowedToAccess(requesterID:String, userID:String): Boolean = {
  isSameUser(requesterID,userID) || isFriend(requesterID,userID)
}

def isFriend(requesterID:String, userID:String): Boolean = {
  try {
    userFriendListMap.get(requesterID).contains(userID)
  } catch {
      case NonFatal(t) => false
  }
}

def isSameUser(requesterID:String, userID:String): Boolean = {
  requesterID.equalsIgnoreCase(userID)
}

def updateImageData(requesterID:String,album:String): String = {
  var img_id = "dfault"
  try { 
    if(userAlbumDataMap.containsKey(requesterID+album)) {
        var album_dir = userAlbumDataMap.get(requesterID+album) 
        img_id = (album_dir.length + 1).toString()
        album_dir += img_id
    } else {
        var dir = new File("media/"+requesterID+"/"+album)
        dir.mkdirs()
        var img_list = new ArrayBuffer[String]
        img_id = 1.toString()
        img_list += img_id
        userAlbumDataMap.put(requesterID+album,img_list)
    }
  } catch {
    case NonFatal(t) => "Error in updating image data" 
  }
  img_id
}

} //end of trait


