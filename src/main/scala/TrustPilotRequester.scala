import sttp.client4.quick.*
import sttp.client4.Response
import sttp.model.Uri

/**
 * Reqeusts TrustPilot for domains with latest reviews
 */
object TrustPilotRequester {
  /**
   * Printed domains are restricted to these categories
   */
  val VALID_CATEGORY_IDS: Set[String] = Set("clothing_store", "outerwear_store")
  /**
   * holds list of retrieved values
   */
  var stores = Map[String, Store]()
  
  def processReviews(): Unit = {
    println("Processing clothing stores")
    for i <- 1 until 20
      do synchronized {
         stores = stores ++ processClothingStores(i)
         print(".")
      }

    var storesList = stores.values.toList.sortBy( s => (s.numberOfReviews, s.monthlyVisits) ).reverse

    println("\nResults: ")
    println(storesList.take(5)
    .map { store =>
      s"${store.url} ${store.numberOfReviews} ${store.monthlyVisits}"
    }.mkString(", "))

    // storesList.foreach(s => print(s.url + " "))

    if (storesList.isEmpty) 
      println("no data found")
  }

  def processClothingStores(pageNumber: Int): Map[String, Store] = {
    val suffixOption = ConfigReader.getProperty("clothing_store.suffix")

    if (suffixOption.isEmpty) {
      println("clothing store suffix not found")
      return Map[String,Store]()
    }
    val suffix = suffixOption.getOrElse("")

    val pageNumberParam: Option[Int] = if (pageNumber == 1)  None else Some(pageNumber)
    
    val uriStr = uri"https://www.trustpilot.com/_next/data/$suffix/categories/clothing_store.json?page=${pageNumberParam}&categoryId=clothing_store&sort=latest_review"

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

  def requestReview(storeId: String): Option[List[String]] = {
    val response: Response[String] = quickRequest
      .get(uri"https://www.trustpilot.com/api/categoriespages/$storeId/reviews?locale=en-US")
      .send()
  
    TrustPilotParser.extractReviews(response.body)
  }
}