package com.digitaldoodles.filex

import com.digitaldoodles.filexio._
import java.util.zip.{ZipInputStream, ZipEntry, ZipOutputStream}
import java.io._

class Zip(val destination: FilePath, overwrite: Boolean=false) {
	val d = destination.toAbsolute
	if (!overwrite && d.exists) throw new IOException("`Zipper` would overwrite file `%s`" format d.pathString)
	val output = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(d.javaFile)))

	def addFile(inputPath: FilePath, pathInZip: RelativeFilePath) = {
		val input = new FilexByteReader(inputPath)
		// The zip entry name is a file path type string, using "/" as a separatory, and terminated with a "/"
		// if this entry is a directory. Adding directories as entries ensures that even empty directories are
		// included in the zip file.
		// TODO "normalize" the path name by stripping out things like "..", ensure it is a relative path.
		val entryName = pathInZip.components.mkString(start="", sep="/", end=(if (inputPath.isDirectory) "/" else ""))
		val entry = new ZipEntry(entryName)
		output.putNextEntry(entry)
		while(input.didRead) output.write(input.bytes, 0, input.numberOfBytesRead)
	}

	def close() { output.close() }

	def zipFiles(files: Iterable[FilePath]) = files map (f => addFile(f, f.toRelative))
}

class UnZip(val source: FilePath, val destination: FilePath) {
	val s = source.toAbsolute
	val d = destination.toAbsolute
	if (!d.isDirectory) throw new IOException("Destination `%s` of unzip attempt is not a directory." format d.pathString)
	val input = new ZipInputStream(new BufferedInputStream(new FileInputStream(s.javaFile)))
	val buffer = new Array[Byte](FilePath.DefaultBufferSize)

	def unzipEntries() {
		var output: OutputStream = null
		def readBytesFromEntryAndWriteThem() {
			val numberOfBytesRead = input.read(buffer, 0, FilePath.DefaultBufferSize)
			if (numberOfBytesRead >= 0) {
				output.write(buffer, 0, numberOfBytesRead)
				readBytesFromEntryAndWriteThem()
			}
		}

		val entry = input.getNextEntry()
		if (entry != null) {
			output = (d / entry.getName).createOutputStream(append=false)
			readBytesFromEntryAndWriteThem()
			unzipEntries()
		} else {
			input.close()
			output.close()
		}
	}
}

