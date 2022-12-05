import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

object MyApp extends App {

  // *******************************************************************************************************************
  // Reading data from file functionality
  def readFile(filename: String): Map[String, List[Int]] = {
    var mapBuffer: Map[String, List[Int]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) { // for each line
        val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List
          mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map(_.toInt))
      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }

  val mapdata = readFile("data.txt")
  println(mapdata)


  // *******************************************************************************************************************
  // Menu functionality
  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo)

  var opt = 0
  do {
    opt = readOption
  } while (menu(opt))

  // shows menu and reads input
  def readOption: Int = {
    println(
      """|Please select one of the following:
         |  1- Current price for all stocks.
         |  2- Highest and lowest price for all stocks within a period.
         |  3- Median price for all stocks within a period.
         |  4- The stock with highest rise over last week.
         |  5- Compare the average price of two stocks within a period.
         |  6- Enter a portfolio and get its total value
         |  7- quit""".stripMargin)
    readInt()
  }

  // invokes selected menu option
  // finds corresponding function to invoke in action map using get
  // pattern matching used as get returns an Option
  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  // handlers for menu options
  def handleOne(): Boolean = {
    menuShowCurrentStocks(currentPrice) // calls function mnuShowPoints, which invokes function currentPoints
    true
  }

  def handleTwo(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }



  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  def menuShowCurrentStocks(f: () => Map[String, Int]) = {
    f() foreach { case (x, y) => println(s"$x: $y") }
  }



  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def currentPrice(): Map[String, Int] = {
    var Current = Map.empty[String, Int]
    for((stock,value) <- mapdata){
      var recent = value.last
      Current += (stock -> recent)
    }
    Current
  }


}