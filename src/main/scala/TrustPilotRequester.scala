import sttp.client4.quick.*
import sttp.client4.Response

/**
 * Reqeusts TrustPilot for domains with latest reviews
 */
object TrustPilotRequester {
  /**
   * Printed domains are restricted to these categories
   */
  val VALID_CATEGORY_IDS: Set[String] = Set("clothing_store", "outerwear_store")

  def processReviews(): Unit = {
    val suffixOption = ConfigReader.getProperty("clothing_store.suffix")

    if (suffixOption.isEmpty) {
      println("clothing store suffix not found")
      return
    }
    val suffix = suffixOption.getOrElse("")

    val response: Response[String] = quickRequest
      .get(uri"https://www.trustpilot.com/_next/data/$suffix/categories/clothing_store.json?categoryId=clothing_store&sort=latest_review")
      .send()
  
    val extractedData = TrustPilotParser.extractStores(response.body)

    extractedData match {
      case Some(dataList) =>
        dataList.filter { store =>
          store.categories.exists(VALID_CATEGORY_IDS)
        }.foreach { store =>
          println(s"Identifying Name: ${store.url} unitID: ${store.id}")
          println("Category IDs: ")
          println(store.categories.mkString(", "))
          println("Monthly visits: " + DomainStat.requestMontlyVisits(store.url))
          println(s"Review: ${store.review}")
          println("")
        }
      case None =>
        println("No data found")
    }
  }

  def requestReview(storeId: String): Option[String] = {
    val response: Response[String] = quickRequest
      .get(uri"https://www.trustpilot.com/api/categoriespages/$storeId/reviews?locale=en-US")
      .send()
  
    TrustPilotParser.extractReview(response.body)
  }
}