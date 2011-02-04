package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-31, 11:17 PM
 * License: LGPL
 */

import java.io._

private[filex] trait FileInformation {
	val javaFile: File;

	def canRead = javaFile.canRead()
	def canWrite = javaFile.canWrite()
	def exists = javaFile.exists()
	def isFile = javaFile.isFile()
	def isDirectory = javaFile.isDirectory()
	def isAbsolute = javaFile.isAbsolute()
	def isRelative = ! isAbsolute
	def isHidden = javaFile.isHidden()
	def lastModified: Long = javaFile.lastModified()
	def lastModified_=(time: Long) { javaFile setLastModified time }
	def touch { this.lastModified = System.currentTimeMillis }
	def length = javaFile.length()
	def name: String = javaFile.getName()
	def pathString: String = javaFile.getPath()
}