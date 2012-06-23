package controllers

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee.{Iteratee, Enumerator}
import play.api.libs.concurrent.Promise
import akka.util.duration._
import org.hyperic.sigar.Sigar
import scala.collection.JavaConverters._

object Dashboard extends Controller {
  // TODO Replace me with a great UI
  def index = Action { Ok(views.html.websockets.index()) }

  def controller = WebSocket.using[JsValue] { request =>
    var widgets = Set[String]()
    val sigar = new Sigar

    val counter: Enumerator[JsValue] = Enumerator.fromCallback { () =>
      def getValueForWidget(widget: String): JsValue = {
        widget match {
          case "cpu" => {
            var cpu = sigar.getCpuPerc
            JsObject(Seq(
              "cpu" -> JsObject(Seq("user" -> JsNumber(cpu.getUser), "system" -> JsNumber(cpu.getSys), "wait" -> JsNumber(cpu.getWait)))
            ))
          }
          case other => JsObject(Seq(
            other -> JsString("unknown")
            ))
        }
      }

      Promise.timeout({
        if (widgets.isEmpty)
          Some(JsString("no widgets configured"))
        else
          Some(JsObject(Seq("updates" -> JsArray(widgets.toSeq.map(getValueForWidget(_))))))
      }, 1 seconds)
    }

    val echoer = Enumerator.imperative[JsValue]()
    def addWidget(widget: String) {
      echoer.push(JsString("adding a %s widget".format(widget)))
      widgets += widget
    }
    def deleteWidget(widget: String) {
      echoer.push(JsString("deleting the %s widget".format(widget)))
      widgets -= widget
    }

    val in = Iteratee.foreach[JsValue] {
      case msg@JsObject(Seq(("addWidget", JsString(widget)))) => {
        addWidget(widget)
      }
      case JsObject(Seq(("deleteWidget", JsString(widget)))) => {
        deleteWidget(widget)
      }
      case msg => echoer.push(msg)
    }

    (in, echoer >- counter)
  }
}
