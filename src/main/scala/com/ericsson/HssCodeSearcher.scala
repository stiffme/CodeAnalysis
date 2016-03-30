package com.ericsson

import java.io.File

import com.ericsson.HssCodeDirType.HssCodeDirType
import com.ericsson.SearchStrategy.SearchStrategy
import org.slf4j.LoggerFactory

/**
 * Created by esipeng on 2/15/2016.
 */
case class HssCodeFileName(fullName:String,fileName:String)

object HssCodeDirType extends Enumeration {
  type HssCodeDirType = Value
  val UNKNOWN,SA, SE,IDPG,IDP,BG,OB,SWI,SWILib,SCC,OU,SWCLib = Value
}

object SearchStrategy extends Enumeration {
  type SearchStrategy = Value
  val TYPED_SEARCH,ANY_SEARCH, STOP_SEARCH = Value
}

class HssCodeSearcher {
  //define search strategy
  val strategy:Map[HssCodeDirType,SearchStrategy] = collection.immutable.HashMap(
    HssCodeDirType.UNKNOWN -> SearchStrategy.STOP_SEARCH,
    HssCodeDirType.SA -> SearchStrategy.TYPED_SEARCH,
    HssCodeDirType.SE -> SearchStrategy.TYPED_SEARCH,
    HssCodeDirType.IDPG -> SearchStrategy.STOP_SEARCH,
    HssCodeDirType.IDP -> SearchStrategy.STOP_SEARCH,
    HssCodeDirType.BG -> SearchStrategy.TYPED_SEARCH,
    HssCodeDirType.OB -> SearchStrategy.TYPED_SEARCH,
    HssCodeDirType.SWI -> SearchStrategy.STOP_SEARCH,
    HssCodeDirType.SWILib -> SearchStrategy.STOP_SEARCH,
    HssCodeDirType.SCC -> SearchStrategy.ANY_SEARCH,
    HssCodeDirType.OU -> SearchStrategy.ANY_SEARCH,
    HssCodeDirType.SWCLib -> SearchStrategy.TYPED_SEARCH
  )

  def searchRoot(root:String):Set[HssCodeFileName] = {
    val files = collection.mutable.Set.empty[String]
    val file = new File(root)
    typedSearch(file)
  }


  private def typedSearch(root:File):Set[HssCodeFileName] = {
    HssCodeSearcher.logger.debug("Typed search {}",root.getName)
    if(root.exists() == false || root.isDirectory() == false)
      return Set.empty[HssCodeFileName]
    else  {
      val temp = collection.mutable.Set.empty[HssCodeFileName]
      for(child <- root.listFiles() if child.isDirectory == true) {
        val fileType = getDirType(child.getName)
        val nextStrategy = strategy.getOrElse(fileType,SearchStrategy.STOP_SEARCH)
        nextStrategy match {
          case SearchStrategy.TYPED_SEARCH =>temp ++= typedSearch(child)
          case SearchStrategy.ANY_SEARCH =>temp ++= anySearch(child)
          case _ =>
        }
      }
      temp.toSet
    }
  }

  private def anySearch(root:File):Set[HssCodeFileName] = {
    HssCodeSearcher.logger.debug("Any search {}",root.getName)
    if(root.exists() == false || root.isDirectory() == false)
      return Set.empty[HssCodeFileName]
    else  {
      val temp = collection.mutable.Set.empty[HssCodeFileName]

      for(child <- root.listFiles())  {
        if(child.isDirectory)
          temp ++= anySearch(child)
        else {
          val fileName = child.getName
          if(fileName.endsWith(".hh") || fileName.endsWith(".cc") /*|| fileName.endsWith(".cpp") || fileName.endsWith(".c") || fileName.endsWith(".h")*/)
            temp += HssCodeFileName(child.getAbsolutePath,child.getName)
        }
      }
      temp.toSet
    }
  }

  private def getDirType(name:String):HssCodeDirType =  {
    /* UNKNOWN,SA, SE,IDPG,IDP,BG,OB,SWI,SWILib,SCC,OU */
    val reg = """\w+_(\w+)""".r
    name match {
      case reg(typeString) => {
        typeString match {
          case "SA" =>  HssCodeDirType.SA
          case "SE" =>  HssCodeDirType.SE
          case "IDPG" =>  HssCodeDirType.IDPG
          case "IDP" =>  HssCodeDirType.IDP
          case "BG" =>  HssCodeDirType.BG
          case "OB" =>  HssCodeDirType.OB
          case "SWI" =>  HssCodeDirType.SWI
          case "SWILib" =>  HssCodeDirType.SWILib
          case "SCC" =>  HssCodeDirType.SCC
          case "OU" =>  HssCodeDirType.OU
          case "SWCLib" => HssCodeDirType.SWCLib
          case _ => HssCodeDirType.UNKNOWN
        }
      }
      case _ =>HssCodeDirType.UNKNOWN
    }
  }
}


object HssCodeSearcher  {
  val logger = LoggerFactory.getLogger(HssCodeSearcher.getClass)
}