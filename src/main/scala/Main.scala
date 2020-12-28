import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

import com.ahaltindis.hashing.MurmurHash3

object Main extends App {
  val inputBuffer = Source.fromFile(
    "../../data-sample-generator/book_reviews_sample/output/1k.txt"
  )
  val outputFile = new File("1k_scala_hashed.txt")
  val outputBuffer = new BufferedWriter(new FileWriter(outputFile))

  for (line <- inputBuffer.getLines) {
    val t0 = System.nanoTime()
    val hash = MurmurHash3.hash_x64_128(line)
    val t1 = System.nanoTime()

    val hash1 = hash._1
    val hash2 = hash._2

    outputBuffer.write(s"$line\n")
    outputBuffer.write(s"${hash1.toHexString},${hash2.toHexString}\n") // hash
    outputBuffer.write(s"${(t1 - t0).toString()}\n") // duration nano seconds
  }

  inputBuffer.close
  outputBuffer.close

  println("done")

  // val hashValues: Seq[String] = Seq(
  //   "hello world!"
  // )

  // hashValues.foreach(v => {
  //   val hash = Murmur3hash.hash128(v)
  //   println(s"Hash of '$v' = (0x${hash._1.toHexString}, 0x${hash._2.toHexString})")
  // })
}
