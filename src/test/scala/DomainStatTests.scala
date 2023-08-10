import munit.FunSuite

class DomainStatTests extends FunSuite {

  test("requestMontlyVisits with valid URL should return monthly visits count") {
    val urlName = "www.example.com"

    val monthlyVisits = DomainStat.requestMontlyVisits(urlName)
    assert(monthlyVisits > 1)
  }

  test("requestMontlyVisits with empty URL should return 0") {
    val urlName = ""

    val monthlyVisits = DomainStat.requestMontlyVisits(urlName)
    assert(monthlyVisits == 0)
  }
}
