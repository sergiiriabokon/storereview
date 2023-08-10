import sttp.client4.quick.*
import sttp.client4.Response

/**
 * Reqeusts TrustPilot for domains with latest reviews
 */
object TrustPilotRequester {
  /**
   * Printed domains are restricted (filtered out) to these categories
   */
  val VALID_CATEGORY_IDS: Set[String] = Set("clothing_store", "outerwear_store")

  def processReviews(): Unit = {
    val response: Response[String] = quickRequest
      .get(uri"https://www.trustpilot.com/_next/data/categoriespages-consumersite-3703/categories/clothing_store.json?categoryId=clothing_store")
      .send()
  
    val extractedData = TrustPilotParser.extractStores(response.body)

    extractedData match {
      case Some(dataList) =>
        dataList.filter { case (_, categoryIds) =>
          categoryIds.exists(VALID_CATEGORY_IDS)
        }.foreach { case (identifyingName, categoryIds) =>
          println(s"Identifying Name: $identifyingName")
          println("Category IDs: ")
          categoryIds.foreach(println)
          println("Monthly visits: " + DomainStat.requestMontlyVisits(identifyingName) + "\n")
        }
      case None =>
        println("No data found")
    }
  }
}