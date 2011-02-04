package com.digitaldoodles.filex

import java.io._


/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-31, 11:06 PM
 * License: LGPL
 */

private[filex] trait FilesystemOperations extends FileInformation {
	val javaFile: File

	/** Create ancestor directories for this file path.
	 *
	 * Invoke this procedure to ensure that the ancestor directories for this file path exist.
	 * @note This operation invokes `mkdirs` on an underlying `java.io.File` instance; this call may
	 * fail yet create some of the ancestor directories.
	 * @throws FilesystemOperationException if not all ancestor directories could be created.
	 */
	def createAncestorDirectories() {
		val jparent = javaFile.getParentFile()
		if (jparent != null) {
			// File.mkdirs is (IMHO) wonky in that it returns false if the given filepath is already a
			// directory. So test for that and if the dir already exists, do nothing.
			if (jparent.isDirectory) {}
			else if (! jparent.mkdirs()) throw new FilesystemOperationException("Couldn't create directory path to %s" format jparent.getPath())
		}
	}

	/** Create a new file at the filesystem location specified by this path; if such
	 * a file already exists, nothing is done.
	 * @param createAncestorDirectiories if true, nonexistent ancestor directories for this file will be created.
	 *
	 * @throws FilesystemOperationException if a directory of this name already exists.
	 * @throws IOException if an underlying call to java.io.File.createNewFile throws an IOException.
	 * @throws SecurityException if an underlying call to java.io.File.createNewFile throws a SecurityException.
	 * @return true if the file was created, or false if the file already existed.
	 */
	def createFile(createAncestorDirectories: Boolean = false): Boolean = {
		if (exists) {
			if (isDirectory) throw new IncorrectFileTypeException(
				"Can't create file '%s'; directory of same name already exists" format pathString)
			else false
		} else try {
			if (createAncestorDirectories) this.createAncestorDirectories()
			javaFile.createNewFile()
		} catch {
			case e: IOException => throw new FilesystemOperationException(e.getLocalizedMessage)
		}
	}

	/**
	 * Create a directory at the filesystem location indicated by `this`.
	 * @param createAncestorDirectiories if true, nonexistent ancestor directories for this file will be created.
	 *
	 * @return `true` if the directory was created, or `false` if the directory did not need to be created because
	 * it already existed. This function will only return normally if `this` exists and is a directory.
	 * @throws IncorrectFileTypeException if `this` specifies an existing non-directory file.
	 * @throws java.lang.SecurityException if a security manager exists and does not allow the directory to be created.
	 * @throws FilesystemOperationException if the directory could not be created for some other reason.
	 */
	def createDirectory(createAncestorDirectories: Boolean = false): Boolean = {
		if (exists) {
			if (!isDirectory) throw new IncorrectFileTypeException("Can't create file `%s`; non-directory of same name already exists" format pathString)
			else false
		} else try {
			if (createAncestorDirectories) this.createAncestorDirectories()
			javaFile.mkdir()
		} catch {
			case e: IOException => throw new FilesystemOperationException(e.getLocalizedMessage)
		}
	}

	/**Deletes a file or directory, if it exists on the filesystem.
	 * @note This operation will throw an exception if an attempt is made to delete
	 * a non-empty directory.
	 *
	 * @return `true` if a file or directory was deleted, `false` if `this.exists`
	 * returned false.
	 *
	 * @throws FilesystemOperationException if the underlying call to `java.io.File.delete()
	 * returned `false`.
	 * @throws java.lang.SecurityException if a security manager exists and its
	 * SecurityManager.checkDelete(java.lang.String) method denies delete access to the file.
	 */
	def delete():Boolean = {
		if (!javaFile.exists) false
		else if (javaFile.delete()) true
		else throw new FilesystemOperationException("Couldn't delete file %s" format this)
	}
}