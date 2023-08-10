@main
def main(): Unit = 
  Scheduler.schedule(TrustPilotRequester.processReviews)

