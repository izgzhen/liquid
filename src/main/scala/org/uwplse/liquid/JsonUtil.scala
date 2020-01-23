package com.semantic_graph

import com.fasterxml.jackson.databind.{MapperFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, ScalaObjectMapper}

object JsonUtil {
  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)

  def toJson(value: Any): String = {
    mapper.writerWithDefaultPrettyPrinter.writeValueAsString(value)
  }

  def fromJSON[T](json: String)(implicit m : Manifest[T]) : T = {
    mapper.readValue[T](json)
  }
}
