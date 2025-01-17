package com.ahaltindis.hashing

import java.lang.Long.{rotateLeft => rotl}

/** MurmurHash3 was written by Austin Appleby, and is placed in the public
  * domain with no copyrights.
  * The original C++ version can be found at https://github.com/aappleby/smhasher
  *
  * This is a scala implementation of the x64_128 version.
  * Follows the C++ version closely and the output is identical.
  */
object MurmurHash3 {
  def hash_x64_128(data: String, seed: Int = 0): (Long, Long) = {
    hash_x64_128(data.getBytes(), seed)
  }

  def fmix64(chunk: Long): Long = {
    var k = chunk ^ (chunk >>> 33)
    k *= 0xff51afd7ed558ccdL
    k ^= k >>> 33
    k *= 0xc4ceb9fe1a85ec53L
    k ^= k >>> 33
    k
  }

  def hash_x64_128(data: Array[Byte], seed: Int): (Long, Long) = {
    val nblocks: Int = data.length >> 4

    var h1: Long = seed
    var h2: Long = seed

    val c1: Long = 0x87c37b91114253d5L
    val c2: Long = 0x4cf5ad432745937fL

    // body
    for (i <- 0 until nblocks) {
      var k1: Long = 0
      k1 |= data(i * 16 + 7).toLong << 56
      k1 |= data(i * 16 + 6).toLong << 48
      k1 |= data(i * 16 + 5).toLong << 40
      k1 |= data(i * 16 + 4).toLong << 32
      k1 |= data(i * 16 + 3).toLong << 24
      k1 |= data(i * 16 + 2).toLong << 16
      k1 |= data(i * 16 + 1).toLong <<  8
      k1 |= data(i * 16).toLong

      var k2: Long = 0
      k2 |= data(i * 16 + 15).toLong << 56
      k2 |= data(i * 16 + 14).toLong << 48
      k2 |= data(i * 16 + 13).toLong << 40
      k2 |= data(i * 16 + 12).toLong << 32
      k2 |= data(i * 16 + 11).toLong << 24
      k2 |= data(i * 16 + 10).toLong << 16
      k2 |= data(i * 16 +  9).toLong <<  8
      k2 |= data(i * 16 +  8).toLong

      k1 *= c1; k1 = rotl(k1, 31); k1 *= c2; h1 ^= k1
      h1 = rotl(h1, 27); h1 += h2; h1 = h1 * 5 + 0x52dce729L
      k2 *= c2; k2 = rotl(k2, 33); k2 *= c1; h2 ^= k2
      h2 = rotl(h2, 31); h2 += h1; h2 = h2 * 5 + 0x38495ab5L
    }

    // tail
    var k1: Long = 0
    var k2: Long = 0

    val tailStart: Int = nblocks << 4
    val tailLength: Int = data.length & 15

    if (tailLength >= 15)  k2 ^= data(tailStart + 14).toLong << 48
    if (tailLength >= 14)  k2 ^= data(tailStart + 13).toLong << 40
    if (tailLength >= 13)  k2 ^= data(tailStart + 12).toLong << 32
    if (tailLength >= 12)  k2 ^= data(tailStart + 11).toLong << 24
    if (tailLength >= 11)  k2 ^= data(tailStart + 10).toLong << 16
    if (tailLength >= 10)  k2 ^= data(tailStart +  9).toLong <<  8
    if (tailLength >= 9) { k2 ^= data(tailStart +  8).toLong
      k2 *= c2; k2 = rotl(k2, 33); k2 *= c1; h2 ^= k2
    }

    if (tailLength >= 8)   k1 ^= data(tailStart + 7).toLong << 56
    if (tailLength >= 7)   k1 ^= data(tailStart + 6).toLong << 48
    if (tailLength >= 6)   k1 ^= data(tailStart + 5).toLong << 40
    if (tailLength >= 5)   k1 ^= data(tailStart + 4).toLong << 32
    if (tailLength >= 4)   k1 ^= data(tailStart + 3).toLong << 24
    if (tailLength >= 3)   k1 ^= data(tailStart + 2).toLong << 16
    if (tailLength >= 2)   k1 ^= data(tailStart + 1).toLong <<  8
    if (tailLength >= 1) { k1 ^= data(tailStart + 0).toLong
      k1 *= c1; k1 = rotl(k1, 31); k1 *= c2; h1 ^= k1
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
