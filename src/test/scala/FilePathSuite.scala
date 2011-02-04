package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-25, 2:50 PM
 * License: LGPL
 */

import org.scalatest._
import java.util.Random
import java.security.MessageDigest
import java.io._

class FilexPathBasicsSuite extends FunSuite with BeforeAndAfterAll {

	val prefix = FilePath.roots(0)/"a"
	val abs = prefix/"b"
	val rel1 = FilePath.Relative(IndexedSeq("d", "e"))
	val rel2 = FilePath.Relative(IndexedSeq("f", "g"))
	val relSpecialChars = FilePath.Relative(IndexedSeq("a file", "$\\.dat"))

	// Currently only UNIX-style filesystems are supported, so we put tests for that right in the test suite.
	test("filesystem is UNIX style") {
		assert(FilePath.separator === "/", "Filesystem does not use `/` as a path separator character.")
		assert(IndexedSeq(File.listRoots():_*).mkString === "/", "Filesystem did not have a single `/` as root.")
		assert(FilePath.isUNIXStyle === true, "`FilePath.isUNIXStyle` is not `true`.")
	}

	test("`/` method") {
		assert(FilePath.roots(0).toString === "AbsoluteFilePath(/)")
		assert((rel1/rel2).mkString() === "d/e/f/g", "relative/relative failed")
		assert((abs/rel1).mkString() === "/a/b/d/e", "absolute/relative failed")

		intercept[FilenameException] { abs/("foo%sbar" format FilePath.separator) }
		intercept[FilenameException] { rel1/("foo%sbar" format FilePath.separator) }
	}

	test("`drop` method") {
		assert(abs.drop(prefix) === FilePath.Relative("b"))
	}

	test("shell escaping") {
		println("a\\ file")
		//assert("a\\ file" === "a\\\\ file")
		//[info]   "a[ file/]$\\.dat" did not equal "a[\ file/\]$\\.dat"
		//assert(relSpecialChars.escapeForShell.pathString === "a\\ file/\\$\\\\.dat")
		//assert(relSpecialChars.toAbsolute.escapeForShell.pathString === "/a\\ file/\\$\\\\.dat")
	}
}

class FilesystemWalkingSuite  extends FunSuite with BeforeAndAfterAll {
	var walkDir: AbsoluteFilePath = _

	override def beforeAll() {
		walkDir = FilePath.createTemporaryDirectory()
		(walkDir/"a").createFile()
		(walkDir/"b"/"c"/"d").createFile(createAncestorDirectories=true)
		(walkDir/"b"/"e").createDirectory(createAncestorDirectories=true)
		(walkDir/"b"/"f").createFile(createAncestorDirectories=true)
	}

	override def afterAll() {
		walkDir.deleteRecursively()
	}

	test("directory walking") {
		assert(walkDir.children === IndexedSeq("a", "b"))
		assert(walkDir.walkRelative().map(_.mkString()).toList === "a"::"b/c/d"::"b/f"::Nil)
		assert(walkDir.walkRelative(preorder=true).map(_.mkString()).toList === "a"::"b"::"b/c"::"b/c/d"::"b/e"::"b/f"::Nil)
		assert(walkDir.walkRelative(preorder=true, postorder = true).map(_.mkString()).toList === "a"::"b"::"b/c"::"b/c/d"::"b/c"::"b/e"::"b/e"::"b/f"::"b"::Nil)
	}
}

class CreationAndDeletionSuite extends FunSuite with BeforeAndAfterEach {
	var tmpDir: AbsoluteFilePath = _

	override def beforeEach() {
		tmpDir = FilePath.createTemporaryDirectory()
		(tmpDir/"data").createFile()
		(tmpDir/"dir").createDirectory()
	}

	override def afterEach() {
		tmpDir.deleteRecursively()
	}

	test("creation and modification times") {
		val d1 = tmpDir.lastModified
		val f1 = (tmpDir/"data").lastModified
		// Sleep for long enough that filesystem time granularity will not be a problem.
		Thread.sleep(1200)
		tmpDir.touch
		(tmpDir/"data").touch
		assert(d1 < tmpDir.lastModified)
		assert(f1 < (tmpDir/"data").lastModified)
	}

	test("Filesystem operation exceptions") {
		intercept[IncorrectFileTypeException] { (tmpDir/"data").createDirectory() }
		intercept[IncorrectFileTypeException] { (tmpDir/"dir").createFile() }
		intercept[FilesystemOperationException] { (tmpDir/"data"/"data2").createFile() }
//		intercept[FilesystemOperationException] { (tmpDir/"data"/"dir2").createDirectory() }
	}
}

class FilexIOSuite extends FunSuite  with BeforeAndAfterEach {
	var tmpDir: AbsoluteFilePath = _

	override def beforeEach() {
		tmpDir = FilePath.createTemporaryDirectory()
	}

	override def afterEach() {
		tmpDir.deleteRecursively()
	}

	def randomBytes(length:Int) = {
		val bytes = new Array[Byte](length)
		new Random().nextBytes(bytes)
		bytes
	}

	test("file creation and deletion") {
		val path = tmpDir/"MyFile"
		path.createFile()
		assert(path.exists)
		assert(path.isFile)
		assert(! path.isDirectory)
		path.delete()
		assert(!path.exists)
	}

	test("file read and write operations") {
		// Choose a bytes size that will exercise file reading by being larger than usual buffer sizes,
		// and is unlikely to be a multiple of buffer size.
		val bytes = randomBytes(123456)
		val f = tmpDir/"rwFile"
		assert(!f.exists)
		f.write(bytes, append=false)
		assert(bytes sameElements f.readAllBytes())
		f.delete()
		assert(!f.exists)
	}

	test("SHA digests") {
		val bytes = randomBytes(12345)
		val bytesDigester = MessageDigest.getInstance("SHA-256")
		bytesDigester.update(bytes)
		val digestedSHA = BigInt(1, bytesDigester.digest())
		val tempFile = tmpDir/"data"
		tempFile.write(bytes, append=false)

		assert(tempFile.calculateSHA256 === digestedSHA, "file and memory digests are not the same.")

		intercept[FileAttributeException] { tempFile.getAttribute(FilePath.SHA256AttrName) }
		// This call to SHA256 calculates and stores the digest as a file attribute.
		assert(tempFile.SHA256 === digestedSHA)
		// Verify the file attribute is stored and has the correct value.
		assert(BigInt(tempFile.getAttribute(FilePath.SHA256AttrName).split(":")(0)) === digestedSHA)
		// This call to SHA256 retrieves the digest from a file attribute.
		assert(tempFile.SHA256 === digestedSHA)
	}

	test("file attribute operations") {
		val file = (tmpDir/"a file")
		file.createFile()
		file.setAttribute("a1", "foo")
		file.setAttribute("a2", "bar")

		assert(file.getAttribute("a1") === "foo")
		assert(file.listAttributes === IndexedSeq("a1", "a2"))
		file.deleteAttribute("a1")
		assert(file.listAttributes === IndexedSeq("a2"))
		intercept[NonExistentFileException] { (tmpDir/"other").setAttribute("a", "b")}
		intercept[FileAttributeException] { file.getAttribute("n/a") }

		file.delete()
	}
}

class ZipSuite extends FunSuite with BeforeAndAfterAll {
	var tmpDir: AbsoluteFilePath = _

	override def beforeAll() {
		tmpDir = FilePath.createTemporaryDirectory()
		assert(tmpDir.isDirectory)
	}

	override def afterAll() {
		tmpDir.delete()
		assert(! tmpDir.exists)
	}

	test("Simple zip creation") {
		val zipSourceDir = tmpDir/"zipSourceDir"
		val emptyDir = zipSourceDir/"emptyDir"
		emptyDir.createDirectory()

		emptyDir.delete()
		zipSourceDir.delete()
	}
}