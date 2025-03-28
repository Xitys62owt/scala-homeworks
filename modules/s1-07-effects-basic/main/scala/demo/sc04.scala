package sc04

import java.io.{BufferedReader, FileReader}
import scala.util.{Try, Using}


object ScalaUsingDemo extends App:

    // --- Demo 1 ---

    val content1: Try[String] =
        Using(new BufferedReader(new FileReader("file2.txt"))):
            reader =>
                reader.readLine()

    println("Demo 1")
    println(content1)
    println()


    // --- Demo 2 ---

    val content2: Seq[String] =
        Using.resource(new BufferedReader(new FileReader("file2.txt"))):
            reader =>
                Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq

    println("Demo 2")
    println(content2.mkString("<", ">, <", ">"))
    println()


    // --- Demo 3 ---

    val content3: Try[String] =
        Using.Manager: use =>
            val reader1 = use(new BufferedReader(new FileReader("file1.txt")))
            val reader2 = use(new BufferedReader(new FileReader("file2.txt")))
            s"<${reader1.readLine()}> + <${reader2.readLine()}>"

    println("Demo 3")
    println(content3)
    println()


