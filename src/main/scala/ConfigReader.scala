import java.util.Properties
import java.io.{FileInputStream, InputStreamReader}

object ConfigReader {
  private val properties = new Properties()

  // Load properties from the file
  val configFile = getClass.getResourceAsStream("/config.properties")
  properties.load(new InputStreamReader(configFile, "UTF-8"))

  // Method to get a property value
  def getProperty(key: String): Option[String] = Option(properties.getProperty(key))
}