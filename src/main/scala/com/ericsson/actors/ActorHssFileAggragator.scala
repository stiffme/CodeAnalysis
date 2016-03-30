package com.ericsson.actors

import java.io.{FileWriter, FileOutputStream, File}

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import akka.routing.FromConfig
import com.ericsson.{HssCodeResultCollector, HssCodeFile, HssCodeFileName, HssCodeSearcher}
import com.opencsv.CSVWriter

import scala.collection.mutable
import scalax.file.Path

/**
  * Created by esipeng on 2/16/2016.
  */

case class SigHssCodeRoot(root:String,appTraceDir:String)

private case class SigStartAppTraceParsing(dir:String)
private case object SigStartSummary
class ActorHssFileAggragator(hssCodeRouter:ActorRef,apptraceRouter:ActorRef) extends Actor with ActorLogging{

  val groupSize = context.system.settings.config.getInt("akka.number-of-instances")
  var resultReceived = 0
  var allFunctions: collection.mutable.HashMap[String,collection.immutable.Set[HssCodeResultCollector]] = collection.mutable.HashMap.empty[String,collection.immutable.Set[HssCodeResultCollector]]
  var appTraceDir = ""
  var rootPath = ""
  def receive = {
    case SigHssCodeRoot(root,appTraceDir) => {
      rootPath = root
      this.appTraceDir = appTraceDir
      log.info("searching code files in {}", root)
      val search = new HssCodeSearcher
      val codeFiles: Set[HssCodeFileName] = search.searchRoot(root)
      codeFiles.foreach(c => println(c.fileName, c.fullName))
      log.info(" {} files are found!",codeFiles.size)

      val seperatedTasks = new Array[collection.mutable.Set[HssCodeFileName]](groupSize)
      for( i <- 0 to groupSize -1) seperatedTasks(i) = collection.mutable.Set.empty[HssCodeFileName]

      var pos = 0
      for ( c <- codeFiles) {
        val ind = pos % groupSize
        seperatedTasks(ind) += c
        pos += 1
      }
      resultReceived = 0
      //send them to the router
      seperatedTasks.foreach( t => hssCodeRouter.tell(SigHssCodeFileWork(t.toSet) , self))
    }

    case SigHssCodeFileWorkResult(oneResult) => {
      resultReceived += 1
      //merge them,
      for ( c <- oneResult) {
        val current = allFunctions.getOrElse(c._1,default = collection.immutable.Set.empty[HssCodeResultCollector])
        val newCollectors = for (tf <- c._2) yield new HssCodeResultCollector(tf.codeName,tf.functions.toSet)
        allFunctions += (c._1 -> (current ++ newCollectors))
      }



      if(resultReceived == groupSize ) { //yeah! we have done~
        var functionLines = 0
        allFunctions.foreach(fn => fn._2.foreach(f => f.functions.foreach(fun => {
          if(fun.enterLine >=0 && fun.exitLine.size != 0)
            functionLines += fun.exitLine.last - fun.enterLine + 1
        })))

        log.info(" {} functional lines are parsed.", functionLines)

        self ! SigStartAppTraceParsing(this.appTraceDir)
      }
    }

    case SigStartAppTraceParsing(dir) => {
      var functionNumber = 0
      for ( t <- allFunctions) {
        for(resultCollector <- t._2)  functionNumber += resultCollector.functions.size
      }
      log.info("{} functions parsed.",functionNumber)

      //start parsing apptraces
      val apptraceDirFile = new File(dir)
      val seperatedApptraces = new Array[collection.mutable.ArrayBuffer[String]](groupSize)
      for(i <- 0 to groupSize -1)
        seperatedApptraces(i) = collection.mutable.ArrayBuffer.empty[String]

      var index = 0
      for(f <- apptraceDirFile.listFiles() if f.isDirectory == false) {
        seperatedApptraces(index % groupSize) += f.getPath()
        index += 1
      }
      resultReceived = 0
      //send seperated traces to route
      seperatedApptraces.foreach( f => apptraceRouter.tell(SigParseTrace(f.toSeq),self) ) //apptraceRouter ! SigParseTrace( f.toSeq))
    }

    case SigParseTraceAnswer(enterMap,exitMap) => {
      log.debug("answer apptrace received")
      resultReceived += 1

      enterMap.foreach(entry => {
        allFunctions.get(entry._1) match {
          case Some(resultsCollection)  =>  {
            resultsCollection.foreach( oneResult => {
              entry._2.foreach( line => oneResult.indicateLine(line,true))
            })
          }
          case None =>{
            log.warning("File {} not found!",entry._1)
          }
        }
      })

      exitMap.foreach(entry => {
        allFunctions.get(entry._1) match {
          case Some(resultsCollection)  =>  {
            resultsCollection.foreach( oneResult => {
              entry._2.foreach( line => oneResult.indicateLine(line,false))
            })
          }
          case None =>{
            log.warning("File {} not found!",entry._1)
          }
        }
      })

      if(resultReceived == groupSize) {
        //everything is ready
        self ! SigStartSummary
      }
    }

    case SigStartSummary => {
      log.debug("Receive SigStartSummary")
      var codeLines = 0
      var coveredLines = 0


      allFunctions.foreach( fu => {
        fu._2.foreach( result => {
          codeLines += result.getAllCodeLines()
          coveredLines += result.getCoveredLines()
        })
      })

      val overallCoverage:Double = coveredLines / codeLines * 100

      val noAllExcel = new CSVWriter(new FileWriter("NeverTouchedFunctions.csv"))
      val noPerfectExcel = new CSVWriter(new FileWriter("NotPerfectExcel.csv"))
      val problemFunctions = new CSVWriter(new FileWriter("ProblemFunctions.csv"))
      allFunctions.foreach(si => {
        si._2.foreach( re => {
          re.getNeverTouchedFunctions().foreach(fi => {
            noAllExcel.writeNext ( Array(fi.functionName.stripMargin.replaceAll("\n"," ") , fi.codeFile.fullName ))
          })
        })
      })


      allFunctions.foreach(si => {
        si._2.foreach( re => {
          re.getNotPerfectFunctions().foreach(fi => {

            noPerfectExcel.writeNext( Array(fi.functionName.stripMargin.replaceAll("\n"," ") , fi.codeFile.fullName, fi.missingEntry.mkString(" ") ))
          })
        })
      })

      allFunctions.foreach(si => {
        si._2.foreach( re => {
          re.getCorruptedFunctions().foreach(fi => {
            problemFunctions.writeNext(Array (fi.functionName.stripMargin.replaceAll("\n"," ") , fi.codeFile.fileName ))
          })
        })
      })

      noAllExcel.close()
      noPerfectExcel.close()
      problemFunctions.close()

      log.info("Overall coverage is {}",overallCoverage)
      context.system.shutdown()
    }
  }
}


object ActorHssFileAggragator {
  def props(hssCodeRouter:ActorRef,apptraceRouter:ActorRef) = Props(new ActorHssFileAggragator(hssCodeRouter,apptraceRouter))
}
