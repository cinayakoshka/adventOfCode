package com.aoc

import java.io.File

import org.apache.commons.io.FileUtils

trait Utils {

  def getResource(resourcePath: String): Option[String] =
    Option(getClass.getResource(resourcePath)).map { r =>
      val file = new File(r.getPath)
      FileUtils.readFileToString(file, "UTF-8")
    }

  def pathFor(fileName: String): String =
    getClass.getClassLoader.getResource(fileName).getPath

}
