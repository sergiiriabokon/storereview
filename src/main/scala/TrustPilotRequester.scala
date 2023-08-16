import sttp.client4.quick.*
import sttp.client4.Response
import sttp.model.Uri
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit;
import java.time.Duration;

/**
 * Reqeusts TrustPilot for domains with latest reviews
 */
object TrustPilotRequester {
  /**
   * Printed domains are restricted to these categories
   */
  val VALID_CATEGORY_IDS: Set[String] = Set("clothing_store", "outerwear_store", "jewelry_store", "electronics_technology")
  /**
   * List of web-pages each corresponding to page_name.json API end-point. Used if conf is not provided
   */
  val CATEGORY_PAGES = "clothing_store, jewelry_store"

  /**
   * holds list of retrieved values
   */
  var stores = Map[String, Store]()
  
  def processReviews(): Unit = {

    var storePages = ConfigReader.getProperty("store.categories").getOrElse(CATEGORY_PAGES).split(",").map(s=>s.trim())

    storePages.foreach{ categoryName => {
      println(s"\nProcessing $categoryName")
      for i <- 1 until 3
        do synchronized {
          stores = stores ++ processCategoryEndpoint(i, categoryName)
          print(".")
        }
      }
    }

    val delay = Scheduler.START_TIME.plus(Duration.of(25, ChronoUnit.MINUTES));

    if (!LocalDateTime.now.isAfter(delay)) {
      println("\nwaiting to aggregate more results")
      return
    }

    var storesList = stores.values.toList.sortBy( s => (s.reviews.size, s.monthlyVisits) ).reverse

    println("\nResults: ")
    println(storesList.take(5)
    .map { store =>
      s"${store.url} ${store.reviews.size} ${store.monthlyVisits} " + 
      store.reviews.head.text.substring(0, Math.min(store.reviews.head.text.length(), 50))
      + " " + store.reviews.head.timestamp
    }.mkString(", "))

    // storesList.foreach(s => print(s.url + " "))

    if (storesList.isEmpty) 
      println("no data found")
  }

  def processCategoryEndpoint(pageNumber: Int, categoryName: String): Map[String, Store] = {
    val suffixOption = ConfigReader.getProperty("categories.suffix")

    if (suffixOption.isEmpty) {
      println("clothing store suffix not found")
      return Map[String,Store]()
    }
    val suffix = suffixOption.getOrElse("")

    val pageNumberParam: Option[Int] = if (pageNumber == 1)  None else Some(pageNumber)
    
    val uriStr = uri"https://www.trustpilot.com/_next/data/$suffix/categories/${categoryName}.json?page=${pageNumberParam}&categoryId=${categoryName}&sort=latest_review"

    val response: Response[String] = quickRequest
      .get(uriStr)
      .send()
  
    val extractedData = TrustPilotParser.extractStores(response.body)
    
    extractedData match {
      case Some(dataList) =>
        dataList.filter { store =>
          store.categories.exists(VALID_CATEGORY_IDS)
        }.map(s => s.url -> s).toMap
      case None =>
        Map[String,Store]()
    }
  }

  def requestReview(storeId: String, identifyingName: String): List[Review] = {
    val response: Response[String] = quickRequest
      .get(uri"https://www.trustpilot.com/api/categoriespages/$storeId/reviews?locale=en-US")
      .send()
  
    val newReviews = TrustPilotParser.extractReviews(response.body).getOrElse(List[Review](Review("no reviews found", "no date")))
    
    // merging with previously found reviews
    stores.get(identifyingName) match {
      case Some(store) => {
        
        var allReviews:List[Review] = newReviews ++ store.reviews
        allReviews.groupBy(_.timestamp).map(x => x._2.head).toList
        
      }
      case None => newReviews
    }
    
  }
}