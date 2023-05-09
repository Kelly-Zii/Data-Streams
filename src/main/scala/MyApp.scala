/**
 * We are developing a simple web app, completely in Scala. Importantly, the app is a Single Page App (SPA),
 * and interacts completely asynchronously with the back-end.
 * Its objective is to show simple UI element manipulation, as well as
 * interaction with a database.
 *
 * The app implements a simple TODO list.
 *
 * This app will be the basis for your project as well.
 */

import com.typesafe.config.{Config, ConfigFactory}
import sourcecode.Text.generate
import java.io.InputStreamReader
import java.sql.{Connection, DriverManager}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

/** Cask is a web framework similar to Python's Flask. As a framework, it has its own 'main' and does a lot of magic
 *  behind the scenes. */
object MyApp extends cask.MainRoutes:
  /** Allows access to the web server from a different machine */
  override def host: String = "0.0.0.0"
  /** Good to set the port explicitly, and not use port 80 for experiments -- leave encryption to a wrapper such as nginx */
  override def port: Int = sys.env.get("PORT").map(_.toInt).getOrElse(8000)
  /** Turn this on during development */
  override def debugMode: Boolean = true

  /** Homepage entry point */
  @cask.get("/")
  def index() = cask.Redirect("/static/index.html")

  /** Static file repository, in case we need to serve static files, e.g. index.html */
  @cask.staticFiles("/static/", headers = Seq("Content-Type" -> "text/html"))
  def staticFileRoutes1() = "/static/"

  /** Static file repository for Javascript files (the target compilation folder for our ScalaJS project) */
  @cask.staticFiles("/sjs/", headers = Seq("Content-Type" -> "text/javascript"))
  def staticFileRoutes2() = "/sjs/target/scala-3.2.2/sjs-fastopt/"

  /** End-point for TODO item submission */
  import sourcecode.Text.generate


  @cask.post("/submit")
  def submit(request: cask.Request): Unit = {
    val todoInsertStmt = DbDetails.conn.prepareStatement("""insert into todo (item, priority, duedate, description, created_time) values(?,?,?,?,CURRENT_TIMESTAMP)""")
    val submitText = request.text().split("\n")

    todoInsertStmt.setString(1, submitText.toList.head)
    todoInsertStmt.setInt(2, submitText.toList.apply(1).toInt)

    val dateString = submitText.toList.apply(2)
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    try {
      val parsedDate = LocalDate.parse(dateString, formatter)
      todoInsertStmt.setDate(3, java.sql.Date.valueOf(parsedDate))
    } catch {
      case _: DateTimeParseException => todoInsertStmt.setNull(3, java.sql.Types.DATE)
    }
    todoInsertStmt.setString(4, submitText.toList.apply(3))
    
    try {
      todoInsertStmt.execute()
    } catch {
      case e: Exception =>
        println("Error inserting row: " + e.getMessage())
        e.printStackTrace()
    }
  }


  /** End-point for reading all TODOs, for updating the item panel after every modification */
  @cask.get("/readtodos")
  def readtodos(): String =
    val readStmt = DbDetails.conn.prepareStatement("""select item, priority, duedate, description from todo order by priority""")
    val qResults = readStmt.executeQuery()
    val todos = Iterator.continually {
      if qResults.next() then
        val item = qResults.getString("item")
        val priority = qResults.getString("priority")
        val duedate = qResults.getString("duedate")
        val description = qResults.getString("description")
        Option(s"""{"item": "$item", "priority": "$priority", "duedate": "$duedate", "description": "$description"}""")
      else None
    }.takeWhile(_.nonEmpty).flatten
    val result =
      if todos.isEmpty
      then "[]"
      else todos.mkString("[", ",", "]")
    val wrapped = s"""{ "items" : $result }"""
    println(wrapped)
    wrapped

  
  @cask.get("/filtertodos")
  def filtertodos(priorityFilter: String): String =
    val readStmt = DbDetails.conn.prepareStatement(s"""select item, priority, duedate from todo where priority = '$priorityFilter'""")
    val qResults = readStmt.executeQuery()
    val todos = Iterator.continually {
      if qResults.next() then Option(qResults.getString("item"), qResults.getInt("priority"), qResults.getString("duedate")) else None
    }.takeWhile(_.nonEmpty).flatten
    val result =
      if todos.isEmpty
      then "[]"
      else todos.map(t => s"""${t._1}; ${t._2};${t._3}""").mkString("[\"", "\",\"", "\"]")
    val wrapped = s"""{ "items" : $result }"""
    println(wrapped)
    wrapped
    
  /** End-point for deleting one TODO item */
  @cask.delete("/delete")
  def delete(request: cask.Request): Unit =
    val delStmt = DbDetails.conn.prepareStatement("""delete from todo where item = ?""")
    delStmt.setString(1, request.text())
    delStmt.execute()

  /** This starts the Cask framework */
  initialize()