package aia.state

import akka.actor.{Actor, ActorLogging, ActorRef, FSM}

import math.min
import scala.concurrent.duration._

// events
case class BookRequest(context: AnyRef, target: ActorRef)
case class BookSupply(nrBooks: Int)
case object BookSupplySoldOut
case object Done
case object PendingRequests

//responses
case object PublisherRequest
case class BookReply(context: AnyRef, reserveId: Either[String, Int])

//states
sealed trait State
case object WaitForRequests extends State
case object ProcessRequest extends State
case object WaitForPublisher extends State
case object SoldOut extends State
case object ProcessSoldOut extends State

case class StateData(nrBooksInStore: Int,
                     pendingRequests: Seq[BookRequest])

class Inventory(publisher: ActorRef) extends Actor
  with FSM[State, StateData] with ActorLogging {

  var reserveId = 0
  startWith(WaitForRequests, new StateData(0, Seq()))

  when(WaitForRequests) {
    case Event(request: BookRequest, data: StateData) => {
      log.info(s"Inventory:State = WaitForRequests, event = BookRequest, $data")
      val newStateData = data.copy(
        pendingRequests = data.pendingRequests :+ request)
      if (newStateData.nrBooksInStore > 0) {
        goto(ProcessRequest) using newStateData
      } else {
        goto(WaitForPublisher) using newStateData
      }
    }
    case Event(PendingRequests, data: StateData) => {
      log.info(s"Inventory:State = WaitForRequests, event = PendingRequests, $data")
      if (data.pendingRequests.isEmpty) {
        stay
      } else if (data.nrBooksInStore > 0) {
        goto(ProcessRequest)
      } else {
        goto(WaitForPublisher)
      }
    }
  }
  when(WaitForPublisher) {
    case Event(supply: BookSupply, data: StateData) => {
      log.info(s"Inventory:State = WaitForPublisher, event = BookSupply, $data")
      goto(ProcessRequest) using data.copy(
        nrBooksInStore = supply.nrBooks)
    }
    case Event(BookSupplySoldOut, _) => {
      log.info("Inventory:State = WaitForPublisher, event = BookSupplySoldOut")
      goto(ProcessSoldOut)
    }
  }
  when(ProcessRequest) {
    case Event(Done, data: StateData) => {
      log.info(s"Inventory:State = ProcessRequest, event = Done, $data")
      goto(WaitForRequests) using data.copy(
        nrBooksInStore = data.nrBooksInStore - 1,
        pendingRequests = data.pendingRequests.tail)
    }
  }
  when(SoldOut) {
    case Event(request: BookRequest, data: StateData) => {
      log.info(s"Inventory:State = SoldOut, event = BookRequest, $data")
      goto(ProcessSoldOut) using new StateData(0, Seq(request))
    }
  }
  when(ProcessSoldOut) {
    case Event(Done, data: StateData) => {
      log.info(s"Inventory:State = ProcessSoldOut, event = Done, $data")
      goto(SoldOut) using new StateData(0, Seq())
    }
  }
  whenUnhandled {
    // common code for all states
    case Event(request: BookRequest, data: StateData) => {
      log.info(s"Inventory:State = Unhandled, event = BookRequest, $data")
      stay using data.copy(
        pendingRequests = data.pendingRequests :+ request)
    }
    case Event(e, s) => {
      log.warning("received unhandled request {} in state {}/{}",
        e, stateName, s)
      stay
    }
  }
  initialize

  onTransition {
    case _ -> WaitForRequests => {
      if (!nextStateData.pendingRequests.isEmpty) {
        log.info(s"Transition: -> WaitForRequests - send PendingRequests ")
        // go to next state
        self ! PendingRequests
      }
    }
    case _ -> WaitForPublisher => {
      log.info(s"Transition: -> WaitForPublisher - send PublisherRequest ")
      //send request to publisher
      publisher ! PublisherRequest
    }
    case _ -> ProcessRequest => {
      val request = nextStateData.pendingRequests.head
      //send request to publisher
      reserveId += 1
      log.info(s"Transition: -> ProcessRequest - send BookReply $reserveId ")
      request.target !
        new BookReply(request.context, Right(reserveId))
      self ! Done
    }
    case _ -> ProcessSoldOut => {
      nextStateData.pendingRequests.foreach(request => {
        log.info(s"Transition: -> ProcessSoldOut - send BookReply SoldOut ")
        request.target !
          new BookReply(request.context, Left("SoldOut"))
      })
      self ! Done
    }
  }
}


class Publisher(totalNrBooks: Int, nrBooksPerRequest: Int)
  extends Actor with ActorLogging {

  var nrLeft = totalNrBooks
  def receive = {
    case PublisherRequest => {
      log.info(s"Publisher: nrLeft= $nrLeft")
      if (nrLeft == 0)
        sender() ! BookSupplySoldOut
      else {
        val supply = min(nrBooksPerRequest, nrLeft)
        nrLeft -= supply
        log.info(s"Publisher: Take from supply , now have nrLeft= $nrLeft")
        val s =sender()
        log.info(s"Publisher:Sending $supply books to $s")
        sender() ! new BookSupply(supply)
      }
    }
  }
}


class InventoryWithTimer(publisher: ActorRef) extends Actor
  with FSM[State, StateData] {

  var reserveId = 0
  startWith(WaitForRequests, new StateData(0, Seq()))

  when(WaitForRequests) {
    case Event(request: BookRequest, data: StateData) => {
      val newStateData = data.copy(
        pendingRequests = data.pendingRequests :+ request)
      if (newStateData.nrBooksInStore > 0) {
        goto(ProcessRequest) using newStateData
      } else {
        goto(WaitForPublisher) using newStateData
      }
    }
    case Event(PendingRequests, data: StateData) => {
      if (data.pendingRequests.isEmpty) {
        stay
      } else if (data.nrBooksInStore > 0) {
        goto(ProcessRequest)
      } else {
        goto(WaitForPublisher)
      }
    }
  }
  when(WaitForPublisher, stateTimeout = 5 seconds) {
    case Event(supply: BookSupply, data: StateData) => {
      goto(ProcessRequest) using data.copy(
        nrBooksInStore = supply.nrBooks)
    }
    case Event(BookSupplySoldOut, _) => {
      goto(ProcessSoldOut)
    }
    case Event(StateTimeout, _) => goto(WaitForRequests)
  }
  when(ProcessRequest) {
    case Event(Done, data: StateData) => {
      goto(WaitForRequests) using data.copy(
        nrBooksInStore = data.nrBooksInStore - 1,
        pendingRequests = data.pendingRequests.tail)
    }
  }
  when(SoldOut) {
    case Event(request: BookRequest, data: StateData) => {
      goto(ProcessSoldOut) using new StateData(0, Seq(request))
    }
  }
  when(ProcessSoldOut) {
    case Event(Done, data: StateData) => {
      goto(SoldOut) using new StateData(0, Seq())
    }
  }
  whenUnhandled {
    // common code for all states
    case Event(request: BookRequest, data: StateData) => {
      stay using data.copy(
        pendingRequests = data.pendingRequests :+ request)
    }
    case Event(e, s) => {
      log.warning("received unhandled request {} in state {}/{}",
        e, stateName, s)
      stay
    }
  }
  initialize

  onTransition {
    case _ -> WaitForRequests => {
      if (!nextStateData.pendingRequests.isEmpty) {
        // go to next state
        self ! PendingRequests
      }
    }
    case _ -> WaitForPublisher => {
      //send request to publisher
      publisher ! PublisherRequest
    }
    case _ -> ProcessRequest => {
      val request = nextStateData.pendingRequests.head
      reserveId += 1
      request.target !
        new BookReply(request.context, Right(reserveId))
      self ! Done
    }
    case _ -> ProcessSoldOut => {
      nextStateData.pendingRequests.foreach(request => {
        request.target !
          new BookReply(request.context, Left("SoldOut"))
      })
      self ! Done
    }
  }
}
