import sttp.client4.quick.*
import sttp.client4.Response
import scala.util.matching.Regex
/** 
 * Requests web.vstat.info for Monthly Visits counter for a specific domain
 */
object DomainStat {

  val SESSION_ID = "vstat_session=ErJt7YU24evVK9RUNNqcn95FhC5yai0jHAHmnVwN;"

  def requestMontlyVisits(urlName:String): Int = {
    if(!ConfigReader.getProperty("vstat.enable").getOrElse("0").equals("true")) {
      return 0
    }
    if (urlName.isEmpty()) 
      return 0
    
    val headers = Map(
      "authority" -> "web.vstat.info",
      "cookie" -> SESSION_ID
    )

    val request = quickRequest
      .get(uri"https://web.vstat.info/$urlName")
      .headers(headers)
      .response(asString)

    val response = request.send()
    
    val responseBody = response.body.getOrElse("")
    
    val monthlyVisitsRegex: Regex = """.*MONTHLY_VISITS.*data\-smvisits=\"(\d+)\"""".r
    val monthlyVisits = monthlyVisitsRegex.findFirstMatchIn(responseBody).map(_.group(1))

    return monthlyVisits match {
      case Some(visits) => visits.toInt
      case None => 0
    }
  }

}