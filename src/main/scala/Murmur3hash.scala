import java.nio.{ByteBuffer, ByteOrder}
import java.lang.Long.{ rotateLeft => rotl }

object Murmur3hash {
    def hash128(data: String, seed: Int = 0): (Long, Long) = {
        hash128(data.getBytes(), seed)
    }

    def fmix64(chunk: Long): Long = {
        var k = chunk
        k ^= k >>> 33
        k *= 0xff51afd7ed558ccdL
        k ^= k >>> 33
        k *= 0xc4ceb9fe1a85ec53L
        k ^= k >>> 33
        k
    }

    def hash128(data: Array[Byte], seed: Int): (Long, Long) = {
        val dataLength: Int = data.length
        val nblocks: Int = dataLength >> 4

        val dataBuffer: ByteBuffer = ByteBuffer.wrap(data)
        dataBuffer.order(ByteOrder.LITTLE_ENDIAN)

        var h1: Long = seed
        var h2: Long = seed

        val c1: Long = 0x87c37b91114253d5L
        val c2: Long = 0x4cf5ad432745937fL

        // println("===begin")
        // print("data: ")
        // data.foreach(b => print(b.toHexString))
        // println("")
        // println(s"length: $dataLength")
        // println(s"nblocks: $nblocks")

        // body
        for(i <- 0 until nblocks) {
            var k1: Long = dataBuffer.getLong
            var k2: Long = dataBuffer.getLong

            // println(s"block $i, k_1: ${k1.toHexString}")
            // println(s"block $i: k_2: ${k2.toHexString}")

            k1 *= c1
            k1 = rotl(k1, 31)
            k1 *= c2
            h1 ^= k1

            h1 = rotl(h1, 27)
            h1 += h2
            h1 = h1*5 + 0x52dce729L

            k2 *= c2
            k2 = rotl(k2, 33)
            k2 *= c1
            h2 ^= k2

            h2 = rotl(h2, 31)
            h2 += h1
            h2 = h2*5 + 0x38495ab5L
        }

        // tail
        var k1: Long = 0
        var k2: Long = 0

        val tailStart: Int = nblocks << 4
        val tailLength = dataLength & 15

        // println(s"tail length: $tailLength")
        if (tailLength >= 15)  k2 ^= data(tailStart + 14).toLong << 48
        if (tailLength >= 14)  k2 ^= data(tailStart + 13).toLong << 40
        if (tailLength >= 13)  k2 ^= data(tailStart + 12).toLong << 32
        if (tailLength >= 12)  k2 ^= data(tailStart + 11).toLong << 24
        if (tailLength >= 11)  k2 ^= data(tailStart + 10).toLong << 16
        if (tailLength >= 10)  k2 ^= data(tailStart +  9).toLong <<  8
        if (tailLength >=  9){ k2 ^= data(tailStart +  8).toLong <<  0
            k2 *= c2;
            k2 = rotl(k2, 33)
            k2 *= c1
            h2 ^= k2
        }

        if (tailLength >=  8)  k1 ^= data(tailStart + 7).toLong << 56
        if (tailLength >=  7)  k1 ^= data(tailStart + 6).toLong << 48
        if (tailLength >=  6)  k1 ^= data(tailStart + 5).toLong << 40
        if (tailLength >=  5)  k1 ^= data(tailStart + 4).toLong << 32
        if (tailLength >=  4)  k1 ^= data(tailStart + 3).toLong << 24
        if (tailLength >=  3)  k1 ^= data(tailStart + 2).toLong << 16
        if (tailLength >=  2)  k1 ^= data(tailStart + 1).toLong <<  8
        if (tailLength >=  1) { k1 ^= data(tailStart + 0).toLong <<  0
            //println(s"  --- 1 ${(data(tailStart + 0).toLong << 0).toHexString}")
            k1 *= c1
            k1 = rotl(k1, 31)
            k1 *= c2
            h1 ^= k1
        }

        // finalization
        h1 ^= data.length
        h2 ^= data.length;
        h1 += h2;
        h2 += h1;
        h1 = fmix64(h1);
        h2 = fmix64(h2);
        h1 += h2;
        h2 += h1;

        (h1, h2)
    }
}
