package code
import akka.actor._
import akka.event.LoggingReceive

object ActorStack {
  sealed trait Command
  final case class Push(item: Int) extends Command
  case object Pop extends Command
  case object isEmpty extends Command
  case object Peek extends Command
  def props = Props[ActorStack]
}
class ActorStack extends Actor with Stash with ActorLogging {
  import ActorStack._

  def receive = emptyReceive

  def emptyReceive: Receive = LoggingReceive {
    case Push(item) ⇒
      context.become(nonEmptyReceive(List(item)))
      unstashAll
    case `Pop` ⇒
      stash
    case `isEmpty` ⇒
      log.info("Stack is Empty")
    case `Peek` ⇒
      log.info("Nothing to Peek")
  }
  def nonEmptyReceive(items: List[Int]): Receive = LoggingReceive {
    case Push(item) ⇒
      context.become(nonEmptyReceive(item :: items))

    case `Pop` ⇒
      sender() ! items.head
      context.become(determineReceive(items))
    case `isEmpty` ⇒
      log.info("Stack is not Empty")
    case `Peek` ⇒
      log.info("Top is " + items.head)
  }
  def determineReceive(items: List[Int]): Receive =
    if (items.tail.isEmpty) emptyReceive else nonEmptyReceive(items.tail)
}

object InfoStackActor {
  def props(stack: ActorRef): Props =
    Props(new InfoStackActor(stack))
}
class InfoStackActor(stack: ActorRef) extends Actor {
  def receive = {
    case "isEmpty" ⇒ stack ! ActorStack.isEmpty
    case "Peek" ⇒ stack ! ActorStack.Peek
  }
}

object ProducerStackActor {
  def props(size: Int, stack: ActorRef): Props =
    Props(new ProducerStackActor(size, stack))

}

class ProducerStackActor(size: Int, stack: ActorRef) extends Actor {
  def receive = {
    case "start" ⇒
      for (i ← 1 to size) stack ! ActorStack.Push(i)
  }
}
object ConsumerStackActor {
  def props(size: Int, stack: ActorRef): Props =
    Props(new ConsumerStackActor(size, stack))
}
class ConsumerStackActor(size: Int, stack: ActorRef)
    extends Actor
    with ActorLogging {
  def receive = consumerReceive(size)
  def consumerReceive(remaining: Int): Receive = {
    case "start" ⇒
      stack ! ActorStack.Pop
    case i: Int ⇒
      val newRemaining = remaining - 1
      if (newRemaining == 0) {
        log.info("Consumer {} is done consuming", self.path)
        context.stop(self)
      } else {
        stack ! ActorStack.Pop
        context.become(consumerReceive(newRemaining))
      }
  }
}
object ActorStackExample extends App {
  val system = ActorSystem()
  val stack = system.actorOf(ActorStack.props)
  val numStacks = 1

  val triples =
    for (i ← 1 to numStacks) yield {
      val producer = system.actorOf(ProducerStackActor.props(5, stack))
      val consumer = system.actorOf(ConsumerStackActor.props(2, stack))
      val observer = system.actorOf(InfoStackActor.props(stack))
      (consumer, producer, observer)
    }

  //  val reaper = system.actorOf(ShutdownReaper.props)
  triples.foreach {
    case (consumer, producer, observer) ⇒
      //    reaper ! consumer
      consumer ! "start"
      producer ! "start"
      Thread.sleep(10)
      observer ! "isEmpty"
      observer ! "Peek"
  }

  Thread.sleep(10)
}
