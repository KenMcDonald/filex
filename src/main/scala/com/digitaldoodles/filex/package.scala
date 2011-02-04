package com.digitaldoodles

import java.io._

/** This package implements filesystem-related classes and utilities. */
package object filex {

	abstract class FilexException(message: String) extends IOException(message)

	/**
	 * An operation that changes the filesystem could not be completed; creating a directory, deleting
	 * a file, etc. Altering an existing file is not considered a filesystem operation.
	 */
	class FilesystemOperationException(message: String) extends FilexException(message)

	/**A file name was invalid; it contained illegal characters, is not yet supported by this package, etc.
	 *
	 */
	class FilenameException(message: String) extends FilexException(message)

	/**
	 *  Thrown when a file or directory needed for some operation does not exist.
	 */
	class NonExistentFileException(message: String) extends FilexException(message)

	/**
	 * Used when a filesystem operation is invoked on a filesystem member that cannot support that operation,
	 * such as a directory operation invoked on a plain file, or a data read or write operation invoked on
	 * a directory.
	 */
	class IncorrectFileTypeException(message: String) extends FilexException(message)

	/**
	 * Error in trying to read or write a file attribute.
	 */
	class FileAttributeException(message: String) extends FilexException(message)

}