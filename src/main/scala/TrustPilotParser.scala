import io.circe.*
import io.circe.parser.*

object TrustPilotParser {

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

}