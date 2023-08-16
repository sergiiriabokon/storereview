import sttp.client4.quick.*
import sttp.client4.Response
import scala.util.matching.Regex
import io.circe.*
import io.circe.parser.*
import java.util.concurrent.*
import java.time.LocalDateTime

/**
 * Runs a paralel thread with scheduled execution of a function
 */
object Scheduler {
  val MAIN_THREAD_MAX_TIME_MIN = 30
  val START_TIME = LocalDateTime.now()
  def schedule(fnToRun: () => Unit ): Unit = {

    val executor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
    
    val initialDelay = 0L // Start immediately
    val period = 5L       // Repeat every 5 minutes
    
    val future: ScheduledFuture[_] = executor.scheduleAtFixedRate(() => fnToRun(), initialDelay, period, TimeUnit.MINUTES)
    
    // To stop the execution after a certain time (e.g., 1 hour)
    executor.schedule(() => future.cancel(true), 60, TimeUnit.MINUTES)

    // regulates the main thread execution, program goes on as long as main thread does
    Thread.sleep(MAIN_THREAD_MAX_TIME_MIN * 60 * 1000) // script runs for N minutes with "sbt run"
  }
}