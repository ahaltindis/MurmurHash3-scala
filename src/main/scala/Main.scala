
object Main extends App {
    val hashValues: Seq[String] = Seq(
        "hello world!"
    )

    hashValues.foreach(v => {
        val hash = Murmur3hash.hash128(v)
        println(s"Hash of '$v' = (0x${hash._1.toHexString}, 0x${hash._2.toHexString})")
    })
}