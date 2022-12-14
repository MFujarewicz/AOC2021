package aoc2022.day7

import toolbox.DataLoader
import scala.collection.mutable.ListBuffer


case class File(name: String, size: Int)

case class Directory(name: String,
                     parentDirectory: Directory,
                     childDirectories: ListBuffer[Directory] = ListBuffer.empty,
                     files: ListBuffer[File] = ListBuffer.empty) {

  def getTop: Directory = {
    parentDirectory match
      case null => this
      case dir: Directory => dir.getTop
  }

  def addDirectory(dirName: String): Unit = {
    childDirectories += Directory(name = dirName, parentDirectory = this)
  }

  def addFile(name: String, size: Int): Unit = {
    files += File(name, size)
  }

  def getDir(dirName: String): Directory = {
    childDirectories.find(_.name == dirName).get
  }

  def getFlatDirectories: List[Directory] = {
    this +: childDirectories.flatMap(_.getFlatDirectories).toList
  }

  def size: Int = {
    val localSize = files.map(_.size).sum
    val childDirSize = childDirectories.map(_.size).sum
    localSize + childDirSize
  }

  def printTree(indentation: Int = 0): Unit = {
    print(" " * indentation)
    println(s"- $name")
    childDirectories.foreach(_.printTree(indentation + 2))
  }
}


@main def day7(): Unit = {
  val input = DataLoader(7, 2022)
  val maxSize = 100000
  val totalDisk = 70000000
  val sizeNeededForUpdate = 30000000

  var currentDirectory: Directory = Directory(name = "/", parentDirectory = null)

  for (line <- input) {
    line match
      case "$ cd /" => currentDirectory = currentDirectory.getTop
      case "$ cd .." => currentDirectory = currentDirectory.parentDirectory
      case s"$$ cd $dirName" => currentDirectory = currentDirectory.getDir(dirName)
      case s"$$ ls" => {}
      case s"dir $dirName" => currentDirectory.addDirectory(dirName)
      case s"$fileSize $filename" => currentDirectory.addFile(filename, fileSize.toInt)

  }

  val root = currentDirectory.getTop
  val allDirs = root.getFlatDirectories

  val answer1 = allDirs.map(_.size).filter(_ <= maxSize).sum
  println(answer1)

  val freeMemory = totalDisk - root.size
  val needToDelete: Int = sizeNeededForUpdate - freeMemory

  val answer2 = allDirs
    .map(_.size)
    .filter(_ >= needToDelete)
    .min

  println(answer2)
}
