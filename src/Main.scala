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
    , 2 -> handleTwo
    , 3 -> handleThree
    , 4 -> handleFour
    , 5 -> handleFive
    , 6 -> handleSix
    , 7 -> handleSeven)

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

  def handleFour(): Boolean = {
    menuShowRise(Rise)
    true
  }

  def handleFive(): Boolean = {
    menuCompareAverage(Average)
    true
  }

  def handleSix() : Boolean = {
    menuPortfolio(Portfolio)
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

  def menuShowHighLow(f: (Int, Int) => Map[String, List[Int]]) = {
    //The user is allowed to enter the period of time to be searched
    print("From day: ")
    val start_day = readLine().toInt
    print("To day: ")
    val end_day = readLine().toInt
    val result = f(start_day, end_day)
    result.foreach(stock => println(s"${stock._1} -> Low: ${stock._2(0)}, High: ${stock._2(1)}"))
  }

  def menuShowMedian(f: () => Map[String, Double]) = {
    f() foreach { case (stock, median) => println(s"$stock: $median") }
  }

  def menuShowRise(f: () => String) = {
    val result = f()
    println(s"The stock $result has risen most over the last week.")
  }

  def menuCompareAverage(f:(String,String) =>  Map[String,Double]) = {
    print("First Stock: ")
    val FirstStock = readLine()
    print("Second Stock: ")
    val SecondStock = readLine()
    f(FirstStock,SecondStock) foreach { case (stock, mean) => println(s"$stock -> Mean: $mean") }
  }

  def menuPortfolio(f:(Map[String,Int] => Int)) = {
    var portfolio = Map.empty[String,Int]
    println("How many stocks do you own?")
    val number = readLine().toInt
    for( i <- 1 to number) {
      print("Enter Stock Name: ")
      val stock = readLine()
      print("Enter Shares: ")
      val shares = readLine().toInt
      portfolio += (stock -> shares)
    }
    val result = f(portfolio)
    println(s"The total value of the portfolio is $result.")

  }



  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def currentPrice(): Map[String, Int] = {
    for ((stock, values) <- mapdata)
      yield (stock, values.last)
  }

  def HighLow(start: Int, end: Int): Map[String, List[Int]] = {
    mapdata.map { case (stock, values) =>
      var period = values.slice(start - 1, end)
      stock -> List(period.min, period.max)
    }
  }

  def Median(): Map[String, Double] = {
    mapdata.map { case (stock, values) =>
      val sortedValues = values.sorted
      val (up, down) = sortedValues.splitAt(sortedValues.size / 2)
      val median = (up.last + down.head) / 2.0
      stock -> median
    }
  }

  def Rise(): String = {
    var LargestRise = 0
    var LargestStock = ""
    mapdata.foreach { case (stock, values) =>
      var currentRise = values.last - values(values.size - 7)
      if (currentRise > LargestRise) {
        LargestRise = currentRise
        LargestStock = stock
      }
    }
    LargestStock
  }

  def Average(FirstStock: String, SecondStock: String): Map[String,Double] = {
    var first = mapdata.filter(_._1 == FirstStock)
    var second = mapdata.filter(_._1 == SecondStock)
    var AverageFirst = first.view.mapValues(v => (v.sum.toDouble/v.size)).toMap
    var AverageSecond = second.view.mapValues(v => (v.sum.toDouble/v.size)).toMap
    AverageFirst++AverageSecond
  }

  def Portfolio (portfolio: Map[String,Int]): Int = {
    var CurrentPrices = currentPrice()
    var value = 0
    portfolio.foreach { case (stock,shares) =>
      value+= CurrentPrices(stock) * shares
    }
    value
  }
}