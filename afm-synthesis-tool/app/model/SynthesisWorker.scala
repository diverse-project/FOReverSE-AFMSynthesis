package model

import akka.actor.Actor
import akka.actor.Actor.Receive

/**
 * Created by gbecan on 3/24/15.
 */
class SynthesisWorker extends Actor {

  val domainKnowledge = new InteractiveDomainKnowledge(self)

  override def receive: Receive = {
    case "toto" => println("yeah")
    case _ => println("error")
  }
}
