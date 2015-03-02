package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.step0_load())
  }

  def step0 = Action {
    Ok(views.html.step0_load())
  }

  def step1 = Action {
    Ok(views.html.step1_features_attributes())
  }

  def step2 = Action {
    Ok(views.html.step2_hierarchy())
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

}