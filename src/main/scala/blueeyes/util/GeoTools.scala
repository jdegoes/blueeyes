package blueeyes.util

trait GeoTools {
  val earthRadiusInMiles = 3958.75  
  def distanceInMiles(lat1: Double, lng1: Double, lat2: Double, lng2: Double) = {
    val dLat = math.toRadians(lat2 - lat1)
    val dLng = math.toRadians(lng2 - lng1)
    val a    = math.sin(dLat/2) * math.sin(dLat/2) + math.cos(math.toRadians(lat1)) * math.cos(math.toRadians(lat2)) * math.sin(dLng/2) * math.sin(dLng/2)
    val c    = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a));

    earthRadiusInMiles * c;
  }
}