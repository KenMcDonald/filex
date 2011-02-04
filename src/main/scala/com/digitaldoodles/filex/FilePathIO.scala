package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-02-1, 2:35 PM
 * License: LGPL
 */

import java.io._

private[filex] trait FilePathIO extends FileInformation with FilePathExceptions {

	/** Read all data in a file into a new `Array[Byte]` and return it.
	 *
	 * @note This is a convenience method intended for reading relatively small files. Since it reads all data
	 * in a file into memory, it may cause memory resource problems if used to read large files.
	 * @throws NonExistentFileException if the file does not exist.
	 * @throws IncorrectFileTypeException if the file is not readable as data (eg. is a directory).
	 * @throws FilesystemOperationException if the number of bytes read was not the same
	 * as the size of the file when `readAllBytes` was invoked.
	 */
	def readAllBytes(bufferSize: Int = FilePath.DefaultBufferSize): Array[Byte] = {
		requireIsFile
		val reader = new FilexByteReader(this, bufferSize)
		val result = new Array[Byte](length.toInt)
		var resultPos = 0
		while (reader.didRead()) {
			Array.copy(reader.bytes, 0, result, resultPos, reader.numberOfBytesRead)
			resultPos += reader.numberOfBytesRead
		}
		if (result.length != resultPos) throw new FilesystemOperationException(
			"Size of file %s changed during `readAllBytes` operation" format pathString)
		result
	}

	/** Convenience method that appends a string to a file. If the file does not exist it will be created.
	 *
	 * @define noteAboutFileOpensAndCloses The file will be created if it does not exist; however, all ancestor
	 * directories must already exist, they will not be created. See [[createAncestorDirectories]].
	 *
	 * @note $noteAboutFileOpensAndCloses
	 *
	 * @param s the string to be written.
	 * @param append if true, the string is appended to the file contents, otherwise the string overwrites the file.
	 */
	def write(s:String, append:Boolean) {
		val writer = new BufferedWriter(new FileWriter(javaFile, append))
		writer.write(s, 0, s.length)
		writer.close()
	}

	/**Writes all of the data from `bytes` by calling `write(bytes, 0, bytes.length, append)`.
	 *
	 * @see documentation for `write(Array[Byte],Int,Int,Boolean)`
	 */
	def write(bytes:Array[Byte], append:Boolean) {
		write(bytes, 0, bytes.length, append)
	}

	/**Open the invoking FilePath for writing, writes data from the given
	 * array of bytes, and closes the file.
	 *
	 * @note $noteAboutFileOpensAndCloses
	 *
	 * @param bytes Array containing the data to be written.
	 * @param offset index into `bytes` where the data to be written starts.
	 * @param length number of bytes to be written.
	 */
	def write(bytes:Array[Byte], offset:Int, length:Int, append:Boolean) {
		val writer = new FileOutputStream(javaFile, append)
		writer.write(bytes, offset, length)
		writer.close()
	}

	/** Create and return a new, buffered, output stream for this file. */
	def createOutputStream(append: Boolean) = new BufferedOutputStream(new FileOutputStream(javaFile, append), FilePath.DefaultBufferSize)
}