package com.ericsson.actors

import akka.actor.{ActorRef, Actor, ActorLogging}
import com.ericsson.{HssCodeFile, HssCodeFileName}


/**
  * Created by esipeng on 2/16/2016.
  */
case class SigHssCodeFileWork(files:Set[HssCodeFileName])
case class SigHssCodeFileWorkResult(result:Map[String,Set[HssCodeFile]])

private case class continueTask(singleTask:HssCodeFileName)

class ActorHssCodeAnalyzer() extends Actor with ActorLogging{
  val remainingTasks:collection.mutable.Queue[HssCodeFileName] = collection.mutable.Queue.empty[HssCodeFileName]
  val result:collection.mutable.Map[String,collection.mutable.Set[HssCodeFile]] = collection.mutable.HashMap.empty[String,collection.mutable.Set[HssCodeFile]]
  var requestSender:ActorRef = null

  def receive = {
    case SigHssCodeFileWork(tasks) => {
      log.debug("got {} tasks", tasks.size)
      remainingTasks.clear()
      result.clear()
      requestSender = sender()
      remainingTasks ++= tasks
      startOneTask()
    }

    case continueTask(t) => {
      val hssCodeFile = new HssCodeFile(t)
      val set = result.getOrElse(t.fileName,collection.mutable.Set.empty[HssCodeFile])
      set += hssCodeFile
      result += (t.fileName -> set)

      startOneTask()
    }
  }


  private def startOneTask(): Unit = {
    if(remainingTasks.size == 0)  {
      val finalResult = for (t <- result) yield (t._1,t._2.toSet)
      requestSender ! SigHssCodeFileWorkResult(finalResult.toMap)
    } else  {
      val nextTask = remainingTasks.dequeue()
      self ! continueTask(nextTask)
    }
  }
}


