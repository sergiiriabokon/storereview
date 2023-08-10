import io.circe._
import io.circe.parser._
import munit.FunSuite

class TrustPilotParserTests extends FunSuite {

  test("extractStores should extract valid data from JSON") {
    val json = """
      |{
      |  "pageProps": {
      |    "recentlyReviewedBusinessUnits": [
      |      {
      |        "businessUnitId": "544f321c00006400057b2ed3",
      |        "displayName": "Fashion Nova",
      |        "identifyingName": "www.fashionnova.com",
      |        "categories": [
      |          {
      |            "categoryId": "mens_clothing_store",
      |            "isPredicted": false
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |}
    """.stripMargin

    val result = TrustPilotParser.extractStores(json)

    assertEquals(result, Some(List(("www.fashionnova.com", List("mens_clothing_store")))))
  }

  test("extractStores should return None when JSON is empty") {
    val json = "{}"

    val result = TrustPilotParser.extractStores(json)

    assertEquals(result, None)
  }

  test("extractStores should return None when key is missing") {
    val json = """
      |{
      |  "pageProps": {
      |    "recentlyReviewedBusinessUnits": []
      |  }
      |}
    """.stripMargin

    val result = TrustPilotParser.extractStores(json)

    assertEquals(result, None)
  }
}
