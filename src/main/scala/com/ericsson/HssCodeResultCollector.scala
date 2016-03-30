package com.ericsson

/**
  * Created by esipeng on 2/18/2016.
  */
case class FunctionInformation(functionName:String,codeFile:HssCodeFileName,missingEntry:Set[Int])
class HssCodeResultCollector(val codeName:HssCodeFileName,val functions:Set[HssFunction]) {

  val enterEntry = collection.mutable.Set.empty[Int]
  val exitEntry = collection.mutable.Set.empty[Int]

  var corruptedFunctions:Set[FunctionInformation] = null
  var notPerfectFunctions:Set[FunctionInformation] = null
  var neverTouchedFunctions:Set[FunctionInformation] = null
  //optimize the search, make Enter -> function, and EXIT -> function
  //val enterLines = for (f <- functions) yield f.enterLine
  //val exitLines = for (f <- functions)  {
  //  for ( tf <- f.exitLine) yield tf
 // }

  def indicateLine(number:Int, isEnter:Boolean)  = {
    if(isEnter)
      enterEntry += number
    else
      exitEntry += number
  }

  def getAllCodeLines():Int = {
    var lines = 0
    functions.filter(f=>f.problems.size == 0 && f.enterLine >= 0 && f.exitLine.size > 0).foreach( f => {

      lines += f.exitLine.max - f.enterLine + 1
    })
    lines
  }

  def getCoveredLines():Int = {
    var covered = 0
    functions.filter(f=>f.problems.size == 0  && f.enterLine >= 0 && f.exitLine.size > 0).foreach( f => {
      val sortedExits = f.exitLine.sortWith( _ < _)
      if(enterEntry.contains(f.enterLine) == false)
        return 0

      var pos = f.enterLine

      sortedExits.foreach( s => {
        if(exitEntry.contains(s)) {
          covered += s - pos +1
        }

        pos = s
      })
    })

    covered

  }

  private def evaluateFunctions():Unit = {
    val corrupted = collection.mutable.HashSet.empty[FunctionInformation]
    val notPerfect = collection.mutable.HashSet.empty[FunctionInformation]
    val neverTouched = collection.mutable.HashSet.empty[FunctionInformation]

    functions.foreach( f=> {
      if(f.problems.size > 0)
        corrupted += FunctionInformation(f.qualifiedName,codeName,Set.empty)
      else if(f.problems.size == 0 && f.enterLine >= 0 && f.exitLine.size > 0)  {
        var touchCount = 0
        if(enterEntry.contains(f.enterLine))
          touchCount += 1

        touchCount += f.exitLine.filter(exitEntry.contains(_)).size

        if(touchCount == 0)
          neverTouched += FunctionInformation(f.qualifiedName,codeName,Set.empty)
        else  {
          val missingEntry:collection.mutable.HashSet[Int] = collection.mutable.HashSet.empty[Int]
          if(enterEntry.contains(f.enterLine) == false)
            missingEntry += f.enterLine

          missingEntry ++= f.exitLine.filter( e => exitEntry.contains(e) == false)

          if(missingEntry.size > 0) {
            notPerfect += FunctionInformation(f.qualifiedName,codeName,missingEntry.toSet)
          }
        }

      }
    })

    this.corruptedFunctions = corrupted.toSet
    this.neverTouchedFunctions = neverTouched.toSet
    this.notPerfectFunctions = notPerfect.toSet

  }

  def getNotPerfectFunctions():Set[FunctionInformation] = {
    if(notPerfectFunctions == null)
      evaluateFunctions()
    notPerfectFunctions
  }

  def getNeverTouchedFunctions():Set[FunctionInformation] = {
    if(neverTouchedFunctions == null)
      evaluateFunctions()
    neverTouchedFunctions
  }

  def getCorruptedFunctions():Set[FunctionInformation] = {
    if(corruptedFunctions == null)
      evaluateFunctions()

    corruptedFunctions
  }
}
