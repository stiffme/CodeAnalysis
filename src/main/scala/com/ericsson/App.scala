package com.ericsson

import akka.actor.{Props, ActorSystem}
import akka.routing.FromConfig
import com.ericsson.actors.{ActorAppTraceParser, ActorHssCodeAnalyzer, SigHssCodeRoot, ActorHssFileAggragator}

/**
 * @author ${user.name}
 */
object App {
  
  def main(args : Array[String]) {
    implicit val system = ActorSystem("MainApp")
    implicit val ec = system.dispatcher

    val hssCodeRouter = system.actorOf(FromConfig.props(Props[ActorHssCodeAnalyzer]), "codeFileRouter")
    val hssTraceParser = system.actorOf(FromConfig.props(Props[ActorAppTraceParser]), "traceParserRouter")
    val aggregator = system.actorOf(ActorHssFileAggragator.props(hssCodeRouter,hssTraceParser),"main")
    aggregator ! SigHssCodeRoot("""C:\HSSCode\hss16acp0lsv2\16acp0_lsv2\esm\HssEsm_SA\HssEsm_SE\HssEsmServices_BG\HssEsmS6aIncomingServices_OB""","""C:\HSSCode\test\apptrace""")
  }

}
