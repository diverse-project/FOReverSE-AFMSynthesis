package model

import akka.actor.{Props, ActorRef, Actor}
import akka.actor.Actor.Receive
import play.api.libs.json.{JsArray, JsValue, Json}

/**
 * Created by gbecan on 3/24/15.
 */
class SynthesisWorker(val out : ActorRef) extends Actor {

  val domainKnowledge = new InteractiveDomainKnowledge(self)

  override def receive: Receive = {
    case x : JsArray => {
      println(x)
      out ! Json.toJson(List("toto", "tata"))
    }
    case _  => println("error")
  }
}


object SynthesisWorker {
  def props(out : ActorRef) = Props(new SynthesisWorker(out))
}