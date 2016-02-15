package com.ericsson

import com.ericsson.HssFunctionProblem.HssFunctionProblem
import org.eclipse.cdt.core.dom.ast.IASTNode
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage
import org.eclipse.cdt.core.parser.{IncludeFileContentProvider, DefaultLogService, ScannerInfo, FileContent}
import org.eclipse.cdt.internal.core.dom.parser.cpp._
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.collection.mutable
import scalax.file.Path

/**
 * Created by esipeng on 2/3/2016.
 */
class HssCodeFile(val fileName:String) {

  val translationUnit = initTranslationUnit()
  val lineStartPositions = initLineStartPositions()
  val problem:collection.mutable.Set[HssFunctionProblem] = collection.mutable.Set.empty[HssFunctionProblem]

  //parsing source file to get all the functions
  for( node <- translationUnit.getChildren()) {
    if(node.isInstanceOf[CPPASTFunctionDefinition]) {
      val function = node.asInstanceOf[CPPASTFunctionDefinition]
      parseFunction(function)
    }
  }



  def initTranslationUnit() = {
    val fileContent = FileContent.createForExternalFileLocation(fileName)

    //stubs for CDT
    val definedSymbols = new mutable.HashMap[String,String]()
    val includePaths = Array("")
    val info = new ScannerInfo(definedSymbols,includePaths)
    val log = new DefaultLogService()
    val emptyIncludes = IncludeFileContentProvider.getEmptyFilesProvider()

    val opts = 8
    val translationUnit = GPPLanguage.getDefault().getASTTranslationUnit(fileContent,info,emptyIncludes,null,opts,log)
    translationUnit
  }

  def initLineStartPositions() = {
    implicit val codec = scalax.io.Codec.UTF8

    //get starting position of each line
    val lineLengths = Path.fromString(fileName).lines(includeTerminator = true).map(_.length)
    //calculating line start positions
    val lineStartPositions = new Array[Int](lineLengths.size)
    lineStartPositions(0) = 0
    for( i <- 1 to lineLengths.size-1) {
      lineStartPositions(i) = lineStartPositions(i-1) + lineLengths(i-1)
    }
    lineStartPositions
  }

  def parseFunction(function:CPPASTFunctionDefinition) = {
    var functionName:String = ""
    var enterLine:Int = -1
    val exitLine:collection.mutable.ArrayBuffer[Int] = collection.mutable.ArrayBuffer.empty[Int]


    getFunctionDeclarator(function) match {
      case Some(declarator) => { functionName = declarator; HssCodeFile.logger.debug("Function Name = {}",declarator)}
      case None => {HssCodeFile.logger.warn("Function Declarator not found {}",function.getRawSignature)}
    }

    val statements = getFunctionStatements(function)
    if(statements.size < 2) {
      if(statements.size != 0)
        problem.add(HssFunctionProblem.INSUFFICIENT_STATEMENT)
    } else  {
      val enterMacro = """.*HSS_\w+_TRACE_ENTER.*""".r
      val exitMacro = """.*HSS_\w+_TRACE_EXIT.*""".r
      //Rule: First statement should be ENTER
      val firstStatement = statements.head
      if(enterMacro.pattern.matcher(firstStatement.getRawSignature).matches())  {
        val temp = firstStatement.getNodeLocations
        enterLine = getLineNumberFromPosition(firstStatement.getNodeLocations.head.getNodeOffset)
        HssCodeFile.logger.debug("ENTER line is {}",enterLine)
      }

      //Rule: Last statement should be EXIT or RETURN
      val lastStatement = statements.last
      if(lastStatement.isInstanceOf[CPPASTReturnStatement] == false && exitMacro.pattern.matcher(lastStatement.getRawSignature).matches() == false) {
        problem.add(HssFunctionProblem.LAST_STATEMENT_NOT_EXIT)
      }

      //go over all the statements to find all the EXIT ways
      for( c <- statements) {
        if (exitMacro.pattern.matcher(c.getRawSignature).matches()) {
          val line = getLineNumberFromPosition(c.getNodeLocations().head.getNodeOffset)
          HssCodeFile.logger.debug("EXIT line is {}",line)
          exitLine.add(line)
        }
      }
    }
  }

  def getLineNumberFromPosition(position:Int):Int = {
    var result = lineStartPositions.length - 1
    for(lineStart <- this.lineStartPositions.reverse) {
      if(position < lineStart)
        result = result - 1
      else
        return result +1 //line start position starts at 0, but line number starts at 1,
    }
    return result +1
  }

  def getFunctionStatements(node:IASTNode):Seq[IASTNode] = {
    val collect = collection.mutable.ArrayBuffer.empty[IASTNode]
    //Recursively get all the statement, seeking first statement if needed to see if it is enter macro
    for (c <- node.getChildren
          if (c.isInstanceOf[CPPASTCompoundStatement] || c.isInstanceOf[CPPASTExpressionStatement] || c.isInstanceOf[CPPASTDeclarationStatement] || c.isInstanceOf[CPPASTReturnStatement]))  {
      if(c.isInstanceOf[CPPASTCompoundStatement])
        collect ++= getFunctionStatements(c)
      else  {
        collect += c
      }
    }

    collect.toSeq
  }



  def getFunctionDeclarator(node:CPPASTFunctionDefinition):Option[String] = {
    val declears = node.getChildren.partition(_.isInstanceOf[CPPASTFunctionDeclarator])._1
    if(declears.size == 1)  {
      Some(declears(0).getRawSignature)
    }
    else
      None
  }

}


object HssCodeFile  {
  def logger = LoggerFactory.getLogger(HssCodeFile.getClass)
}
