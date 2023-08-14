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
        val review = TrustPilotRequester.requestReview(unitId).getOrElse("review not found");
        if (identifyingName.nonEmpty && categoryIds.nonEmpty) Some(Store(unitId, identifyingName, categoryIds, review))
        else None
      }

      if (data.isEmpty) None
      else Some(data)
    }
  }

  def extractReview(json: String): Option[String] = {
    val parseResult = parse(json)
    
    parseResult.toOption.flatMap { jsonValue =>
      val reviewCursor = jsonValue.hcursor.downField("reviews").downN(0).downField("text")
      reviewCursor.as[String].toOption
    }
  }
}