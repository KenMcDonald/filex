package com.digitaldoodles.filex


/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-25, 2:47 PM
 * License: LGPL
 */

import java.io._
import java.security.MessageDigest
import java.net.{URI, URL}

object FilePath {
	private[filex] val SHA256AttrName = "SHA-256.sha:time"

	implicit def filePathToFile(filepath: FilePath) = filepath.javaFile



	val DefaultBufferSize = 4096

	def roots = IndexedSeq(File.listRoots():_*) map (Absolute(_))

	val separator = File.separator

	def currentDirectory = Absolute(new File(System.getProperty("user.dir")))

	val homeDirectory = Absolute(new File(System.getProperty("user.home")))

	/**`true` if and only if this filesystem is a UNIX-style one, meaning that it has a single root denoted
	 * by `/` and the path separator chareacter is `/`.
	 */
	lazy val isUNIXStyle = separator == "/" && IndexedSeq(File.listRoots():_*).mkString == "/"



	/** Creates an empty file in the default temporary-file directory, using the given prefix and suffix to generate its name.
	 * @param suffix The suffix string to be used in generating the file's name.
	 * @return An abstract pathname denoting a newly-created empty file.
	 * @throws IllegalArgumentException `prefix` contains fewer than three characters.
	 * @throws IOException if a file could not be created.
	 * @throws SecurityException if a security manager exists and its `SecurityManager.checkWrite(java.lang.String)` method does not allow a file to be created.
	 */
	def createTemporaryFile(prefix: String="tmp", suffix: String="") = Absolute(File.createTempFile(prefix, suffix))

	def createTemporaryDirectory(prefix: String="tmp", suffix: String=""): AbsoluteFilePath = {
		val path = createTemporaryFile(prefix, suffix)
		path.delete()
		path.createDirectory()
		path
	}


	def Absolute(root: String, components: Seq[String]) = new AbsoluteFilePath(root, IndexedSeq(components:_*))
	
	/**Create a new absolute file path from a `File` instance.
	 * @param file an instance of java.io.File such that `file.isAbsolute()` is `true`.
	 * @return a new `AbsoluteFilePath` instance.
	 * @throws `IllegalArgumentException` if `file.isAbsolute` is not true.
	 */
	def Absolute(file: File): AbsoluteFilePath = {
		require(file.isAbsolute)
		val components = file.getPath.split(separator) filter (_.length > 0)
		assert(isUNIXStyle)
		new AbsoluteFilePath("/", IndexedSeq(components:_*))
	}

	def Absolute(path: String): AbsoluteFilePath = Absolute(new File(path))

	def Absolute(root: String, path: String): AbsoluteFilePath = Absolute(root + separator + path)

	def Relative(components: IndexedSeq[String]): RelativeFilePath = new RelativeFilePath(components)

	def Relative(path: String): RelativeFilePath = Relative(IndexedSeq(path.split(separator):_*))
}

abstract class FilePath(val components: IndexedSeq[String]) extends FilesystemOperations with FilePathIO with FilePathExceptions {

	if (! OS.currentOS.areComponentsLegal(components)) throw new FilenameException("Bad file name: `%s`." format components)

	def toRelative: RelativeFilePath;
	def toAbsolute: AbsoluteFilePath;

	def setAttribute(name: String, value: String) = OS.currentOS.setFileAttribute(this, name, value)
	def getAttribute(name: String) = OS.currentOS.getFileAttribute(this, name)
	def listAttributes = OS.currentOS.listFileAttributes(this)
	def deleteAttribute(name: String) = OS.currentOS.deleteFileAttribute(this, name)

	private def calculateDigest(digestType: String): BigInt = {
		val input = new FilexByteReader(this)
		val digester = MessageDigest.getInstance(digestType)
		while (input.didRead()) {
			digester.update(input.bytes, 0, input.numberOfBytesRead)
		}

		val sum = digester.digest()
		return BigInt(1, sum)
	}

	/**Compute and return the sha-256 digest for the file as a decimal string.
	 *
	 * @note This can be very time-consuming for large files.
	 */
	def calculateSHA256: BigInt = calculateDigest("SHA-256")

	def SHA256: BigInt = {
		def calculateAndSet = {
			val calculated = calculateSHA256
			setAttribute(FilePath.SHA256AttrName, "%s:%s" format (calculated, lastModified))
			calculated
		}

		requireIsFile
		try {
			val Array(sha256String, sha256TimeString) = getAttribute(FilePath.SHA256AttrName).split(":")
			val (sha256, sha256Time) = (BigInt(sha256String), sha256TimeString.toLong)
			if (lastModified <= sha256Time) sha256
			else calculateAndSet
		} catch {
			case e: FileAttributeException => calculateAndSet
		}
	}

	def parent: Option[FilePath];

	def endsWith(other: FilePath) = components endsWith other.components

	/**List the names of the children in the directory denoted by `this`, sorted lexicographically.
	 * @throws IncorrectFileTypeException if `this` is a non-directory file.
	 * @throws NonExistentFileException if `this` does not exist.
	 */
	def children: IndexedSeq[String] = {
		requireExists
		requireIsDirectory
		IndexedSeq(javaFile.list():_*).sortWith(_ < _)
	}

	def ==(other: FilePath) = this.javaFile.equals(other.javaFile)
	override def equals(other: Any) = other match {
		case x: FilePath  => javaFile.equals(x.javaFile)
		case _        => false
	}
	override def hashCode() = javaFile.hashCode()
	def uri: URI = javaFile.toURI()
	def url: URL = uri.toURL()

	//def /(child: RelativeFilePath): FilePath
	//def /(child: String): FilePath

	/**Create a string from this path using `sep` to separate path components instead of `File.separator`. This
	 * function is used to generate strings that are constant across operating systems, for testing purposes.
	 */
	def mkString(start: String = "/", sep: String = "/") = components.mkString(sep=sep)
	//def normalize: FilePath = new FilePath(javaFile.getCanonicalFile())

	def escapeForShell: FilePath;
}
