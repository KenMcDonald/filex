package com.digitaldoodles.filex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-02-1, 3:24 PM
 * License: LGPL
 */
import java.io._

class AbsoluteFilePath(val root: String, components: IndexedSeq[String]) extends FilePath(components) {
	// for now we only handle POSIX type filesystems, i.e. the root is ""
	require(root == "/")
	lazy val javaFile = {
		val File = new File(root, components.mkString(sep=FilePath.separator.toString))
		assert(File.isAbsolute)
		File
	}

	override def toString() = "AbsoluteFilePath(%s)".format(pathString)

	/** Return `this`. */
	override def toAbsolute = this
	/** Return a relative path that has the same path compnents as `this`, but does not reference a filesystem root. */
	override def toRelative = new RelativeFilePath(components)

	def parent = if (components.length == 0) None else Some(new AbsoluteFilePath(root, components dropRight 1))

	def /(child: RelativeFilePath): AbsoluteFilePath = new AbsoluteFilePath(root, components ++ child.components)
	def /(child: String) = new AbsoluteFilePath(root, components :+ child)

	def startsWith(other: AbsoluteFilePath) = (root == other.root) && (components startsWith other.components)

	def drop(prefix: AbsoluteFilePath): RelativeFilePath = {
		require(this startsWith prefix)
		new RelativeFilePath(components drop prefix.components.length)
	}

	/**Returns a `Stream` of files (and optionally subfiles) starting from a given file or directory path.
	 *
	 * @param preorder If true, then directories will be included in the result stream before their children. Defaults
	 * to false.
	 * @param postorder If true, then directories will be included in the result stream after their children. Defaults
	 * to false.
	 *
	 * @note Don't use this if you simply want to list a directory's children. Instead, use [[children]].
	 *
	 * @note If preorder and postorder are both false (the default), directores will not appear in the result stream.
	 * If these parameters are both true, then each directory will appear ''twice'' in the output stream.
	 *
	 * @note If `this` is not a directory, then the result will be a `Stream` containing just the element `this`. This
	 * will be true even if `this` does not actually exist in the filesystem.
	 *
	 * @return A `Stream[FilePath]` instance allowing one to walk the directory.
	 */
	def walkAbsolute(preorder: Boolean=false, postorder: Boolean=false): Stream[AbsoluteFilePath] = {
		if (isDirectory) {
			val first = if (preorder) Stream(this) else Stream.Empty
			val middle = children.toStream flatMap (child => (this/child).walkAbsolute(preorder=preorder, postorder=postorder))
			val last = if (postorder) Stream(this) else Stream.Empty
			first ++ middle ++ last
		}
		else Stream(this)
	}

	def walkRelative(preorder: Boolean=false, postorder: Boolean=false): Stream[RelativeFilePath] = {
		walkAbsolute(preorder, postorder) map (_ drop this) filter (_.components.length > 0)
	}

	/** Delete a directory and everything in it.
	 * @note BE CAREFUL.  You could inadvertently delete an entire filesystem with this procedure.
	 */
	def deleteRecursively() { walkAbsolute(postorder=true) foreach (_.delete()) }

	override def mkString(start: String = "/", sep: String = "/") = start + components.mkString(sep=sep)

	/**Returns a new `AbsoluteFilePath` whose root and components have been escaped in such a way
	 * that they may be safely included in a command-line argument.
	 */
	def escapeForShell = new AbsoluteFilePath(root, components map (OS.currentOS.shellEscapeComponent(_)))
}
