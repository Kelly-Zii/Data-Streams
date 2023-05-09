import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.*
import org.scalajs.dom.html.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.Dynamic.{global => g}
import java.time.format.DateTimeFormatter
import scala.scalajs.js._
import scala.scalajs.js.Thenable.Implicits._
import scala.concurrent.ExecutionContext.Implicits.global
val priorityStyles = """
  .priority1 { color: red; }
  .priority2 { color: orange; }
  .priority3 { color: green; }
"""
object MainJS:
  @JSExportTopLevel("main")
  def main() =
    val workspace: Div = document.getElementById("workspace").asInstanceOf[Div] // retrieve the workspace
    workspace.appendChild(createInputArea())
    workspace.appendChild(createPriorityFilter())// add the input area to the workspace
    val panel = document.createElement("div").asInstanceOf[Div]                 // create the panel containing the todo items
    panel.id = "panel"                                                          // name it so it can be accessed later
    panel.style.background = "lightyellow"                                      // give it a distinguishing color so that it is easily identifiable visually
    workspace.appendChild(panel)                                                // add the panel to the workspace, below the input area
    updatePanel()                                                               // populate the panel with the current todo items

/** Create buttons with a suggestive animation when clicked on */
def createButton(text: String, action: UIEvent => Unit): Button =
  val button = document.createElement("button").asInstanceOf[Button]
  button.innerText = text
  button.addEventListener("click", action)
  button

/** Create read-only text fields */
def createROText(text: String): Div =
  val div = document.createElement("div").asInstanceOf[Div]
  div.className = "divreadonly"
  div.style.display = "inline-block"  // stack them left-to-right instead of top-down
  div.innerText = text
  div

/** Create read-write fields (i.e. input fields) */
def createRWText(initialText: String): Div =
  val div = document.createElement("div").asInstanceOf[Div]
  div.className = "divreadwrite"
  div.style.display = "inline-block"     // stack them left-to-right instead of top-down
  div.contentEditable = "true"           // the attribute making fields editable.
  div.innerText = initialText
  div

/** Create the input area, containing the editable field allowing the user to add todo items.
 * Adding items is done by pressing a 'Submit' button */
def createInputArea(): Div =
  val enclosure = document.createElement("div").asInstanceOf[Div] // Enclosure to contain all the elements for this area
  val label          = createROText("Input TODO item:") // Label inviting the user to add TODO items
  val item           = createRWText("")                 // Input field where the todo item is to be added
  item.id            = "todoitem"                       // Name it so it can be access later from a global scope
  val dueDateLabel   = createROText("Due Date:")        // Label for the due date of the todo item
  val dueDate        = createRWText("")                 // Input field for the due date of the todo item
  dueDate.id         = "duedate"                        // Name it so it can be access later from a global scope


  val priorityLabel  = createROText("Priority:")        // Label for the priority of the todo item
  val priority       = createRWText("")                 // Input field for the priority of the todo item
  priority.id        = "priority"                       // Name it so it can be access later from a global scope

  val descriptionLabel = createROText("Description:")   // Label for the description of the todo item
  val description      = createRWText("")               // Input field for the description of the todo item
  description.id       = "description"                  // Name it so it can be access later from a global scope
  val submit         = createButton("Submit", submitAction) // Create the submit button, with a submit action to be executed when clicked
  Seq(
    label, item,
    dueDateLabel, dueDate,
    priorityLabel, priority,
    descriptionLabel, description,
    submit
  ).foreach(enclosure.appendChild)
  enclosure.style.background = "lightblue" // Use a distinct color to make this area stand out visually
  enclosure
                                                      // Return the enclosure, which is to be added to the workspace
def createPriorityFilter(): Select = {
  val select = document.createElement("select").asInstanceOf[Select]
  val priorities = List("All", "1", "2", "3")

  priorities.foreach { priority =>
    val option = document.createElement("option").asInstanceOf[Option]
    option.value = priority
    option.textContent = if (priority == "All") "All Priorities" else s"Priority $priority"
    select.appendChild(option)
  }

  select.addEventListener("change", (_: Event) => updatePanel())
  select
}

def submitAction = (_: UIEvent) =>
  val submitText = document.getElementById("todoitem").innerText
  val dueDateText = document.getElementById("duedate").innerText
  val priorityText = document.getElementById("priority").innerText
  val descriptionText = document.getElementById("description").innerText
  // check if priority is an integer between 1 and 3
  val isPriorityValid = priorityText.matches("""[1-3]""")
  // check if dueDate is in yyyy-mm-dd format
  val isDueDateValid = dueDateText.matches("\\d{4}-\\d{2}-\\d{2}")

  if submitText != null && submitText.nonEmpty && isPriorityValid && isDueDateValid
  then
    fetch("/submit", new RequestInit {
      method = HttpMethod.POST
      body = s"$submitText\n$priorityText\n$dueDateText\n$descriptionText"
    })
    window.setTimeout(updatePanel,100)
  else if (!isPriorityValid)
    window.alert("Priority must be an integer between 1 and 3.")
  else if (!isDueDateValid)
    window.alert("Due Date must be in yyyy-mm-dd format.")
  else
    window.alert("Please fill in all required fields.")
  document.getElementById("todoitem").innerText = ""
  document.getElementById("duedate").innerText = ""
  document.getElementById("priority").innerText = ""
  document.getElementById("description").innerText = ""

/** We update the panel on every change to the DB */
def updatePanel = () => fetchContent(contentAction)

/** Asynchronously fetch all the todos, so that refreshing the panel becomes possible */
def fetchContent(contentAction: String => Unit) =
  val contentFuture = for
    response <- fetch("/readtodos", new RequestInit { // fetch returns a promise, which is a Monad, so we deal with it in a 'for'
      method = HttpMethod.GET
    })
    text <- response.text()  // response is a promise in turn, and 'text' is a future
  yield text
  for // change the 'for', to handle the future
    content <- contentFuture
  yield {
    contentAction(content)  // we can't take the content out of the Mondad, so we put the processing function 'contentAction' inside the Monad.
  }

/**
 * Update the panel by creating a visual element for every item fetched from the database. Add a 'Delete' btn to each items container.
 */
def createTableHeader(): html.TableRow = {
  val header = document.createElement("tr").asInstanceOf[html.TableRow]
  val headers = List("TODO Item", "Priority", "Due Date", "Description", "Actions", "DuePassed")
  headers.foreach { headerText =>
    val th = document.createElement("th").asInstanceOf[html.TableCell]
    th.textContent = headerText
    header.appendChild(th)
  }
  header
}


def isDueDatePassed(dueDate: String): Boolean = {
  val now = new js.Date().getTime()
  val due = new js.Date(dueDate).getTime()
  now > due
}

def contentAction(content: String): Unit = {
  val payload = JSON.parse(content).selectDynamic("items").asInstanceOf[Array[js.Dynamic]]
  val table = document.createElement("table").asInstanceOf[html.Table]
  table.className = "todoTable"
  table.appendChild(createTableHeader())
  val selectedPriority = document
    .querySelector("select")
    .asInstanceOf[Select]
    .value

  val filteredPayload = selectedPriority match {
    case "All" => payload
    case _ => payload.filter(item => item.selectDynamic("priority").asInstanceOf[String] == selectedPriority)
  }
  val elements = filteredPayload.map { item =>
    val text = item.selectDynamic("item").asInstanceOf[String]
    val priority = item.selectDynamic("priority").asInstanceOf[String]
    val dueDate = item.selectDynamic("duedate").asInstanceOf[String]
    val description = item.selectDynamic("description").asInstanceOf[String]

    val tr = document.createElement("tr").asInstanceOf[html.TableRow]

    val tdItem = document.createElement("td").asInstanceOf[html.TableCell]
    val roTextItem = createROText(text)
    tdItem.appendChild(roTextItem)
    tr.appendChild(tdItem)

    val tdPriority = document.createElement("td").asInstanceOf[html.TableCell]
    val roTextPriority = createROText(priority)
    tdPriority.appendChild(roTextPriority)
    tr.appendChild(tdPriority)

    val tdDueDate = document.createElement("td").asInstanceOf[html.TableCell]
    val roTextDueDate = createROText(dueDate)
    tdDueDate.appendChild(roTextDueDate)
    tr.appendChild(tdDueDate)

    val tdDescription = document.createElement("td").asInstanceOf[html.TableCell]
    val roTextDescription = createROText(description)
    tdDescription.appendChild(roTextDescription)
    tr.appendChild(tdDescription)

    val tdActions = document.createElement("td").asInstanceOf[html.TableCell]
    val deleteBtn = createButton("Delete", deleteAction(text))
    tdActions.appendChild(deleteBtn)
    tr.appendChild(tdActions)

    val tdPassed = document.createElement("td").asInstanceOf[html.TableCell]
    val passedText = if (isDueDatePassed(dueDate)) "Yes" else "No"
    tdPassed.appendChild(createROText(passedText))
    tdPassed.style.color = if (isDueDatePassed(dueDate)) "red" else "green"
    tr.appendChild(tdPassed)

    tr
  }.foreach(table.appendChild)

  val panel = document.getElementById("panel").asInstanceOf[Div]
  panel.replaceChildren(table)
}

/** Delete action to go into the Delete button for every item */
def deleteAction(text: String): UIEvent => Unit = _ =>
  fetch("/delete", new RequestInit { // Async request to the backend to delete the item from the DB
    method = HttpMethod.DELETE
    body = text
  })
  window.setTimeout(updatePanel,100) // After a small timeout to allow the DB transaction to complete, refresh the panel to allow the changes to be reflected.
  ()