package com.ciczan.learnscala

import tethys._
import tethys.jackson._
import tethys.derivation.semiauto._

final case class WfyNode( id: String,
                    nm: String,
                    lm: Long,
                    cp: Option[Long],
                    no: Option[String],
                    ch: Option[Seq[WfyNode]])

object WfyNode {


  implicit val nodeReader: JsonReader[WfyNode] = jsonReader[WfyNode]

  def parseFull(input: String): Either[Exception, Seq[WfyNode]] = input.jsonAs[Seq[WfyNode]]

}