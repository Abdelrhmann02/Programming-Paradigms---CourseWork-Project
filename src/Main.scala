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
  val actionMap = Map[Int, () => Boolean](1 -> handleOne
                                                ,2 -> handleTwo
                                                ,3 -> handleThree
                                                ,7 -> handleSeven)

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
    menuShowCurrentStocks(currentPrice)
    true
  }

  def handleTwo(): Boolean = {
    menuShowHighLow(HighLow)
    true
  }

  def handleThree(): Boolean = {
    menuShowMedian(Median)
    true
  }

  def handleSeven(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }



  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  def menuShowCurrentStocks(f: () => Map[String, Int]) = {
    f() foreach { case (stock, price) => println(s"$stock: $price") }
  }

  def menuShowHighLow(f: (Int,Int) => Map[String, List[Int]]) = {
    //The user is allowed to enter the period of time to be searched
    print("From day: ")
    val start_day = readLine().toInt
    print("To day: ")
    val end_day = readLine().toInt
    val result = f(start_day,end_day)
    result.foreach(stock => println(s"${stock._1} -> Low: ${stock._2(0)}, High: ${stock._2(1)}"))
  }

  def menuShowMedian(f:() => Map[String,Int]) = {
    f() foreach { case (stock,median) => println(s"$stock: $median")}
  }



  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def currentPrice(): Map[String, Int] = {
    for ((stock,values) <- mapdata)
      yield(stock,values.last)
  }

  def HighLow(start: Int,end: Int): Map[String,List[Int]] = {
    mapdata.map { case (stock,values) =>
      var period = values.slice(start-1,end)
      stock -> List(period.min,period.max)
    }
  }

  def Median(): Map[String,Int] = {
    var Current = Map.empty[String, Int]
    Current
  }
}