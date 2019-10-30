package com.ciczan.learnscala

import java.io.{File, PrintWriter}

import scala.io.Source

object WorkflowyParser {

  def parseFile(file: File) = {

    val content = Source.fromFile(file).mkString

    val tree = WfyNode.parseFull(content)

    tree.foreach(node => println(node))//wkfly


  }

  def main(args: Array[String]): Unit = {

    val buPath = "C:\\Users\\cicza\\Dropbox\\Apps\\WorkFlowy\\Data\\(ciczan@gmail.com).2019-9-17.workflowy.backup"
    val singlePath = "C:\\Users\\cicza\\Desktop\\mynode.json"
    parseFile(new File(buPath))

  }

}