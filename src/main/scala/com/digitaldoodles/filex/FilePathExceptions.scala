package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-02-1, 2:57 PM
 * License: LGPL
 */

private[filex] trait FilePathExceptions extends FileInformation {
	def requireExists = if (!exists) throw new NonExistentFileException("File/directory does not exist: `%s`" format this)

	def requireIsDirectory = {
		requireExists
		if (!isDirectory) throw new IncorrectFileTypeException("Expected directory, found non-directory: `%s`" format this)
	}

	def requireIsFile = {
		requireExists
		if (!isFile) throw new IncorrectFileTypeException("Expected data file, found non-data file: `%s`" format this)
	}
}