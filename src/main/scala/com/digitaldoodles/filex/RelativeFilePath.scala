package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-02-1, 3:25 PM
 * License: LGPL
 */

import java.io._

object RelativeFilePath {
	val Empty = new RelativeFilePath(IndexedSeq())
}

class RelativeFilePath(components: IndexedSeq[String]) extends FilePath(components) {
	lazy val javaFile = new File(components.mkString(sep=FilePath.separator.toString))

	override def toString() = "RelativeFilePath(%s)".format(pathString)

	/** Create an absolute file path via an invocation of `FilePath.currentDirectory / this`. */
	override def toAbsolute = FilePath.currentDirectory / this
	/** Return `this`. */
	override def toRelative = this

	def parent = if (components.length == 0) None else Some(new RelativeFilePath(components dropRight 1))

	def /(child: RelativeFilePath): RelativeFilePath = new RelativeFilePath(components ++ child.components)
	def /(child: String): RelativeFilePath = new RelativeFilePath(components :+ child)

	override def escapeForShell = new RelativeFilePath(components map (c => OS.currentOS.shellEscapeComponent(c)))
}
