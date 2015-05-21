package controllers

import java.io.File
import java.util.UUID

import akka.actor.Props
import com.github.tototoshi.csv._
import foreverse.afmsynthesis.algorithm.{SimpleDomainKnowledge, ConfigurationMatrix, AFMSynthesizer}
import model.{SynthesisWorker, InteractiveDomainKnowledge}
import play.api._
import play.api.libs.iteratee.{Enumerator, Iteratee}
import play.api.mvc.WebSocket.FrameFormatter
import play.api.mvc._
import scala.collection.mutable
import akka.actor.ActorSystem
import play.api.Play.current

import play.api.libs.json._

import scala.util.control.NonFatal


object Application extends Controller {


  type Matrix = List[List[String]]
  val matrices = mutable.Map.empty[String, Matrix] // FIXME : not thread safe !
  val synthesizers = mutable.Map.empty[String, AFMSynthesizer] // FIXME : not thread safe !
  // FIXME : empty maps when possible

  // Classic

  def step0 = Action {
    Ok(views.html.step0_load())
  }

  def step1 = Action(parse.multipartFormData) { request =>
    request.body.file("configuration_matrix").map { file =>

      // Create a temporary file to read the uploaded matrix
      val uploadedFile = File.createTempFile("afm_", ".csv")
      file.ref.moveTo(uploadedFile, replace = true)

      // Read the CSV file
      val reader = CSVReader.open(uploadedFile)
      val matrix = reader.all()
      reader.close()



      // Delete the temporary file
      uploadedFile.delete()


      // Create synthesis worker
//      val synthesisWorker = system.actorOf(synthesisWorkerProp)
//      val knowledge = new InteractiveDomainKnowledge;
      val knowledge = new SimpleDomainKnowledge;
      val synthesizer = new AFMSynthesizer()
      val configurationMatrix = new ConfigurationMatrix(matrix.head.toArray, matrix.tail.map(_.toArray))
      println(configurationMatrix.labels.toList)
      configurationMatrix.configurations.toList.foreach(c => println(c.toList))
      synthesizer.synthesize(configurationMatrix, knowledge, true, Some(3), "/tmp")

      // Create working session
      val sessionID = UUID.randomUUID().toString
      matrices += sessionID -> matrix
      synthesizers += sessionID -> synthesizer

      Ok(views.html.step1_features_attributes(matrix)).withSession("id" -> sessionID)
    }.getOrElse {
      Redirect(routes.Application.step0())
    }

  }

  def step2 = Action { request =>

    request.session.get("id").map{ sessionID =>

      val matrix = matrices.get(sessionID)
      val synthesizer = synthesizers.get(sessionID)

      val variableTypes = request.body.asFormUrlEncoded.get.map(t => (t._1, t._2.head))
      for ((variable, variableType) <- variableTypes) {
        // TODO : send column types to synthesis worker
      }

      Ok(views.html.step2_hierarchy())
    }.getOrElse {
      Redirect(routes.Application.step0())
    }


  }

  def step3 = Action {
    Ok(views.html.step3_feature_groups())
  }

  def step4 = Action {
    Ok(views.html.step4_constraints())
  }

  def step5 = Action {
    Ok(views.html.step5_afm())
  }



  // Websockets

  def index = Action {
    Ok(views.html.index())
  }

  def home = Action {
    Ok(views.html.home())
  }

  def load = Action {
    Ok(views.html.load())
  }

  def variables = Action {
    Ok(views.html.variables())
  }

  def hierarchy = Action {
    Ok(views.html.hierarchy())
  }

  def groups = Action {
    Ok(views.html.groups())
  }

  def constraints = Action {
    Ok(views.html.constraints())
  }

  def afm = Action {
    Ok(views.html.afm())
  }

  def synthesize = WebSocket.acceptWithActor[JsValue, JsValue] { request => out =>
    SynthesisWorker.props(out)
  }

}

