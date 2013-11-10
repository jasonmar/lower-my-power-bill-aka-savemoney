package controllers

import play.api._
import play.api.mvc._
import models.Usage

object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }
  
  def user(id: Int) = Action {
	val usage = new Usage(id)
    Ok(views.html.displayUsage(usage))
  }

}