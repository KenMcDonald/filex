package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-27, 1:34 PM
 * License: LGPL
 */

import java.io._
import scala.io.BufferedSource

private[filex] object OS {
	val currentOS: OS = OSX
}

private[filex] trait OS {
	def isComponentNameLegal(name: String): Boolean;
	def areComponentsLegal(components: Seq[String]): Boolean = components.map(isComponentNameLegal).filter(_==false).length == 0

	/**Given a file path component name, returns an equivalent name that is safe for shell (command-line) execution.
	 * This is operating-system dependent, and is typically done by adding in escape sequence to make problematic
	 * characters safe for inclusion in command-line commands.
	 */
	def shellEscapeComponent(component: String): String;

	def setFileAttribute(file: FilePath, attribute: String, value: String);
	def getFileAttribute(file: FilePath, attribute: String): String;
	def listFileAttributes(file: FilePath): IndexedSeq[String];
	def deleteFileAttribute(file: FilePath, attribute: String);
}

private[filex] object OSX extends OS {
	private val regexToShellEscape =  "[\\\\]"

	def shellEscapeComponent(component: String) = component.replaceAll(regexToShellEscape, "\\\\$0")
	
	private def inputStreamToString(input: InputStream): String = new BufferedSource(input).mkString

	def isComponentNameLegal(name: String): Boolean = name.length > 0 && name.indexOf(FilePath.separator) < 0

	def setFileAttribute(file: FilePath, attribute: String, value: String) {
		if (! file.exists) throw new NonExistentFileException("Attempt to set attribute value on non-existent file `%s`." format file.pathString)
		val command = Array("xattr", "-w", attribute, value, file.pathString)
		//println(command)
		val process = Runtime.getRuntime.exec(command)
		process.waitFor()

		if (process.exitValue != 0)
			throw new FileAttributeException("Attempt to write attribute `%s` of file `%s` resulted in error `%s`." format (
					attribute,
					file.pathString,
					inputStreamToString(process.getErrorStream())
					))
	}

	def getFileAttribute(file: FilePath, attribute: String): String = {
		if (! file.exists) throw new NonExistentFileException("Attempt to get attribute value on non-existent file `%s" format file.pathString)
		val command = Array("xattr","-p", attribute, file.pathString)
		val process = Runtime.getRuntime.exec(command)
		process.waitFor()

		if (process.exitValue != 0)
			throw new FileAttributeException("Attempt to read attribute `%s` of file `%s` resulted in error `%s`." format (
					attribute,
					file.pathString,
					inputStreamToString(process.getErrorStream())
					))
		// The dropRight gets rid of a trailing newline
		else inputStreamToString(process.getInputStream()).dropRight(1)
	}

	def listFileAttributes(file: FilePath): IndexedSeq[String] = {
		if (! file.exists) throw new NonExistentFileException("Attempt to list file attributes on non-existent file `%s" format file.pathString)
		val command = Array("xattr", file.pathString)
		val process = Runtime.getRuntime.exec(command)
		process.waitFor()

		if (process.exitValue != 0)
			throw new FileAttributeException("Attempt to read attribute names for  file `%s` resulted in error `%s`." format (
					file.pathString,
					inputStreamToString(process.getErrorStream())
					))
		else {
			// The dropRight gets rid of a trailing newline
			val array = inputStreamToString(process.getInputStream()).dropRight(1).split("\n")
			IndexedSeq(array:_*).sortWith(_ < _)
		}
	}

	def deleteFileAttribute(file: FilePath, attribute: String) {
		if (! file.exists) throw new NonExistentFileException("Attempt to delete attribute value on non-existent file `%s" format file.pathString)
		val command = Array("xattr","-d", attribute, file.pathString)
		val process = Runtime.getRuntime.exec(command)
		process.waitFor()

		if (process.exitValue != 0)
			throw new FileAttributeException("Attempt to delete attribute `%s` of file `%s` resulted in error `%s`." format (
					attribute,
					file.pathString,
					inputStreamToString(process.getErrorStream())
					))
	}

}