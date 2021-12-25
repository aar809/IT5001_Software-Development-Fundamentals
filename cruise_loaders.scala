import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer

class cruise(
    val name: String, 
    val time: Int,
    val time_input: String,
    var loadersNeeded: Int = 1,
    var timeNeeded: Int = 30) {
    timeNeeded = if (name(0) == 'B') 60 else 30
    loadersNeeded = if (name(0) == 'B') 2 else 1
}

class loader(
    val cruises: ArrayBuffer[cruise] = ArrayBuffer(),
    var timeLoaded: Int,
    var timeFinished: Int = -999) {

    def loadCruise(cruise: cruise): Unit = {
        timeLoaded = cruise.time
        cruises.append(cruise)
        timeFinished = timeLoaded + cruise.timeNeeded
    }
    def printSched(): Unit = {
        for (n <- cruises) {
            println(s"|    ${n.name}@${n.time_input}                    |")

            } 
        }
    }

object Main_Level_4 extends App {
    var inputList = ArrayBuffer[String]()
    val number = readInt()
    for (n <- (1 to number).toList) {
        val string1 = readLine()
        inputList.append(string1)
        }

    var cruiseList = ArrayBuffer[cruise]()
    var loaderList = ArrayBuffer[loader]()

    //loop over cruise list to create each Cruise object:
    for (n <- (0 to number-1).toList) {
        val name = inputList(n).split(" ")(0)
        val time_input = inputList(n).split(" ")(1)
        val time = time_input.slice(0,2).toInt * 60 + (time_input.toString.slice(2,4).toInt)
        var myCruise = new cruise(name, time, time_input)
        cruiseList.append(myCruise)
    }

    for (n <- cruiseList){
        if (loaderList.length == 0){        //if there are 0 loaders, create the 1st loader
            var myLoader = new loader(timeLoaded = n.time)
            myLoader.loadCruise(n)
            loaderList.append(myLoader)
            n.loadersNeeded -= 1
            }
        else {   
            for (ldr <- loaderList){   //check each existing loader to see if time finished happens before cruise start
                if (ldr.timeFinished <= n.time && n.loadersNeeded > 0){     
                    ldr.loadCruise(n)
                    n.loadersNeeded -= 1
                    }
                } 
            while (n.loadersNeeded > 0){ 
                var myLoader = new loader(timeLoaded = n.time)
                myLoader.loadCruise(n)
                loaderList.append(myLoader)                   
                n.loadersNeeded -= 1
                }
            }
        }

    println("+==================================+")
    println("| Cruise Statistics                |")
    println("+----------------------------------+")
    var normalCruises = 0
    for (n <- cruiseList){
        if (n.name(0) != 'B') normalCruises += 1
    }
    println(f"| Number of normal cruises   = ${normalCruises}%3d |")
    println(f"| Number of big cruises      = ${cruiseList.length - normalCruises}%3d |")
    println("+==================================+")
    println("| Equipment statistics             |")
    println("+----------------------------------+")
    println(f"| Number of loaders          = ${loaderList.length}%3d |")
    println("+==================================+")
    for (n <- 1 to loaderList.length) {
        println(s"| Loader ${n} serves:                 |")
        loaderList(n-1).printSched()
        println("+==================================+")
        }
}

