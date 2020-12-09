package controllers
import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import scala.collection.mutable.ListBuffer
import models._

@Singleton
class TodoListController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  private val todoList = new ListBuffer[TodoListItem]()
  todoList += TodoListItem(1, "test", true)
  todoList += TodoListItem(2, "some other value", false)
  implicit val todoListJson = Json.format[TodoListItem]
  implicit val newTodoListJson = Json.format[NewTodoListItem]

  def getAll() = Action {
    if (todoList.isEmpty) {
      NoContent
    } else Ok(Json.toJson(todoList))
  }

  def getOne(itemId : Long) = Action {
    val foundItem = todoList.find((item) => item.id == itemId )
    println(foundItem)
    foundItem match {
        case Some(item) => Ok(Json.toJson(item));
        case None => NotFound
    }
  }


  def addOne() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val todoListItem: Option[NewTodoListItem] =
      jsonObject.flatMap(
        Json.fromJson[NewTodoListItem](_).asOpt
      )
    todoListItem match {
      case Some(newItem) =>
        val nextId = todoList.map(_.id).max + 1
        val toBeAdded = TodoListItem(nextId, newItem.description, false)
        todoList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }
  }

  def deleteOne(id : Long) = Action { implicit request =>
    val len = todoList.length
    println(len)
    todoList.dropWhileInPlace(_.id == id)
    println(todoList.length)
    todoList.length match {
      case len => {
        NoContent
      }
      case _ => Accepted
    }
  }

  def getDone(id : Long) = Action {
    val foundItem = todoList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(isItDone = true)
        todoList.dropWhileInPlace(_.id == id)
        todoList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }

  }

}