import io.circe.*
import io.circe.parser.*
/**
 * Parses JSON data retrieved from TrustPilot
 * to a list of (DomainName, CategoriesList) pairs
 */
object TrustPilotParser {

  def extractStores(json: String): Option[List[Store]] = {
    val parseResult = parse(json)

    parseResult.toOption.flatMap { jsonValue =>
      val businessUnits = jsonValue.hcursor.downField("pageProps").downField("recentlyReviewedBusinessUnits").as[List[Json]].toOption.getOrElse(List.empty)

      val data = businessUnits.flatMap { businessUnit =>
        val identifyingName = businessUnit.hcursor.downField("identifyingName").as[String].toOption.getOrElse("")
        val unitId = businessUnit.hcursor.downField("businessUnitId").as[String].toOption.getOrElse("")
        val categories = businessUnit.hcursor.downField("categories").as[List[Json]].toOption.getOrElse(List.empty)
        val categoryIds = categories.flatMap { category =>
          category.hcursor.downField("categoryId").as[String].toOption
        }
        val review = TrustPilotRequester.requestReview(unitId, identifyingName)
        val numberOfReviews = businessUnit.hcursor.downField("numberOfReviews").as[Int].toOption.getOrElse(0)
        val monthlyVisits = DomainStat.requestMontlyVisits(identifyingName)
        
        if (identifyingName.nonEmpty && categoryIds.nonEmpty) Some(Store(unitId, identifyingName, categoryIds, review, numberOfReviews, monthlyVisits))
        else None
      }

      if (data.isEmpty) None
      else Some(data)
    }
  }

  def extractReviews(json: String): Option[List[Review]] = {
    val parseResult = parse(json)

    parseResult.toOption.flatMap { jsonValue =>
      val reviewsArray = jsonValue.hcursor.downField("reviews").focus.flatMap(_.asArray)
      val reviewTexts = reviewsArray.map(_.flatMap{ r =>
        Some(Review(r.hcursor.downField("text").as[String].toOption.getOrElse("noreview text"),
               r.hcursor.downField("date").downField("createdAt").as[String].toOption.getOrElse("noreview date")))
      })
      
      reviewTexts.map(_.toList)
    }
  }
}