import sttp.client4.quick.*
import sttp.client4.Response
import scala.util.matching.Regex
import io.circe._
import io.circe.parser._
import java.util.concurrent._

object StoreReview {

  val validCategoryIds: Set[String] = Set("clothing_store", "outerwear_store")

  def requestMontlyVisits(urlName:String): Unit = {
    if (urlName.isEmpty()) 
      return 
    
    val headers = Map(
      "authority" -> "web.vstat.info",
      "cookie" -> "vstat_session=ErJt7YU24evVK9RUNNqcn95FhC5yai0jHAHmnVwN;"
    )

    val request = quickRequest
      .get(uri"https://web.vstat.info/$urlName")
      .headers(headers)
      .response(asString)

    val response = request.send()
    
    val responseBody = response.body.getOrElse("")
    val monthlyVisitsRegex: Regex = """.*MONTHLY_VISITS.*data\-smvisits=\"(\d+)\"""".r
    val monthlyVisits = monthlyVisitsRegex.findFirstMatchIn(responseBody).map(_.group(1))

    monthlyVisits match {
      case Some(visits) => println(s"Monthly Visits: $visits")
      case None => println("Monthly visits information not found")
    }
  }

  def extractStores(json: String): Option[List[(String, List[String])]] = {
    val parseResult = parse(json)

    parseResult.toOption.flatMap { jsonValue =>
      val businessUnits = jsonValue.hcursor.downField("pageProps").downField("recentlyReviewedBusinessUnits").as[List[Json]].toOption.getOrElse(List.empty)

      val data = businessUnits.flatMap { businessUnit =>
        val identifyingName = businessUnit.hcursor.downField("identifyingName").as[String].toOption.getOrElse("")
        val categories = businessUnit.hcursor.downField("categories").as[List[Json]].toOption.getOrElse(List.empty)
        val categoryIds = categories.flatMap { category =>
          category.hcursor.downField("categoryId").as[String].toOption
        }

        if (identifyingName.nonEmpty && categoryIds.nonEmpty) Some(identifyingName -> categoryIds)
        else None
      }

      if (data.isEmpty) None
      else Some(data)
    }
  }
  
  def processReviews(): Unit = {
    val response: Response[String] = quickRequest
      .get(uri"https://www.trustpilot.com/_next/data/categoriespages-consumersite-3703/categories/clothing_store.json?categoryId=clothing_store")
      .send()
  
    val extractedData = extractStores(response.body)

    extractedData match {
      case Some(dataList) =>
        dataList.filter { case (_, categoryIds) =>
          categoryIds.exists(validCategoryIds)
        }.foreach { case (identifyingName, categoryIds) =>
          println(s"Identifying Name: $identifyingName")
          println("Category IDs:")
          categoryIds.foreach(println)
          requestMontlyVisits(identifyingName)

          println("\n")
        }
      case None =>
        println("No data found")
    }
  }

  def main(args: Array[String]): Unit = {

    val executor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
    
    val initialDelay = 0L // Start immediately
    val period = 5L       // Repeat every 5 minutes
    
    val future: ScheduledFuture[_] = executor.scheduleAtFixedRate(() => processReviews(), initialDelay, period, TimeUnit.MINUTES)
    
    // To stop the execution after a certain time (e.g., 1 hour)
    executor.schedule(() => future.cancel(true), 15, TimeUnit.MINUTES)
  }

}
