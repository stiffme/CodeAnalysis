package com.ericsson.actors

import java.io.{FileReader, BufferedReader}

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import scala.collection.mutable

/**
  * Created by esipeng on 2/19/2016.
  */
case class SigParseTrace(traces:Seq[String])
case class SigParseTraceAnswer(answerEnter:Map[String,Set[Int]], answerExit:Map[String,Set[Int]])

private case class continueParsing(task:String)

class ActorAppTraceParser extends Actor with ActorLogging {
  val appTraceEnterExit = """.*APP-TRACE.*<(ENTER|EXIT)><([A-Za-z0-9_\.]+):(\d+)>.*\" Method:.*""".r
  val tempEnterResult = mutable.HashMap.empty[String,Set[Int]]
  val tempExitResult = mutable.HashMap.empty[String,Set[Int]]
  val pendingTasks = mutable.Queue.empty[String]
  //var requestSender:ActorRef = null
  def receive = {
    case SigParseTrace(traces) => {
      tempEnterResult.clear()
      tempExitResult.clear()
      pendingTasks.clear()
      pendingTasks ++= traces
      //requestSender = sender()
      log.debug("Receive SigParseTrace")
      startOneTask()
    }
    case continueParsing(task) => {
      val reader = new BufferedReader(new FileReader(task))
      var line = reader.readLine()
      while(line != null) {
        line match {
          case appTraceEnterExit(typeStr,file,line) => {
            //log.debug("typeString {} file {} line {}",typeStr,file,line)

            typeStr match {
              case "EXIT" =>  {
                val existing: Set[Int] = tempExitResult.getOrElse(file,Set.empty[Int])
                tempExitResult.put(file , existing + line.toInt)
              }
              case "ENTER" => {
                val existing = tempEnterResult.getOrElse(file,Set.empty[Int])
                tempEnterResult.put(file,existing + line.toInt)
              }
              case _ => {}
            }
          }
          case _ => {}
        }
        line = reader.readLine()
      }

      startOneTask()
    }
  }


  private def startOneTask(): Unit ={
    if(pendingTasks.size == 0)  {

      context.system.actorSelection("/user/main") ! SigParseTraceAnswer(tempEnterResult.toMap,tempExitResult.toMap)
    } else{
      val current = pendingTasks.dequeue()
      self ! continueParsing(current)
    }
  }


}
