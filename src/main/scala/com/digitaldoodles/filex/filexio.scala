package com.digitaldoodles.filex

import java.io._

private[filex] class FilexByteReader(file: FilePathIO, bufferSize: Int = FilePath.DefaultBufferSize) {
	/** TODO does this need to be wrapped in a BufferedInputStream? */
	private var input = new FileInputStream(file.javaFile)
	val bytes = new Array[Byte](bufferSize)
	var numberOfBytesRead = 0

	def didRead(): Boolean = {
		if (input == null) {
			numberOfBytesRead = 0
			false
		} else {
			try {
				numberOfBytesRead = input.read(bytes)
				if (numberOfBytesRead < 0) {
					close()
					false
				} else true
			} catch { case e: IOException => { close(); throw e } }
		}
	}

	def close() {
		input.close()
		input = null
	}
}