package com.ericsson

import com.ericsson.HssFunctionProblem.HssFunctionProblem
import org.eclipse.cdt.internal.core.model.FunctionDeclaration

/**
 * Created by esipeng on 2/4/2016.
 */

object HssFunctionProblem extends Enumeration {
  type HssFunctionProblem = Value
  val GOOD,FIRST_STATEMENT_NOT_ENTER, LAST_STATEMENT_NOT_EXIT,INSUFFICIENT_STATEMENT = Value
}

case class HssFunction(sourceFile:String,functionDeclaration: String, startLine:Int,problem:Set[HssFunctionProblem])
