package com.github.luben.zstd

import org.scalatest.flatspec.AnyFlatSpec

import java.io._
import java.nio._
import scala.io._
import scala.util.Using

class ZstdDictSpec extends AnyFlatSpec {

  def source = Source.fromFile("src/test/resources/xml")(Codec.ISO8859).map{_.toByte}

  def train(legacy: Boolean, sampleSize: Int): Array[Byte] = {
    val src = source.sliding(1024, 1024).take(sampleSize).map(_.toArray)
    val trainer = new ZstdDictTrainer(1024 * sampleSize, 32 * sampleSize)
    for (sample <- src) {
      trainer.addSample(sample)
    }
    val dict = trainer.trainSamples(legacy)
    assert(Zstd.getDictIdFromDict(dict) != 0)
    dict
  }

  def wrapInDirectByteBuffer(array: Array[Byte]): ByteBuffer = {
    //use a slightly oversized buffer and a nonzero offset to test we use the position/limit too
    val bb = ByteBuffer.allocateDirect(array.length + 13);
    bb.limit(bb.capacity())
    bb.position(7)
    bb.limit(bb.position() + array.length)
    bb.put(array)
    bb.flip()
    bb
  }

  "Zstd" should "report error when failing to make a dict" in {
    val src = source.sliding(28, 28).take(4).map(_.toArray)
    val trainer = new ZstdDictTrainer(1024 * 1024, 32 * 1024)
    for (sample <- src) {
      trainer.addSample(sample)
    }
    intercept[com.github.luben.zstd.ZstdException] {
      trainer.trainSamples(false)
    }
  }

  val input = source.toArray
  val legacyS = List(true, false)
  val levels = List(1)
  for {
       legacy <- legacyS
       dict = train(legacy, 1024)
       dict2 = train(legacy, 512)
       dictInDirectByteBuffer = wrapInDirectByteBuffer(dict)
       level <- levels
  } {

    "Zstd" should s"round-trip compression/decompression with dict at level $level with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val decompressed = Zstd.decompress(compressed, dict, input.length)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression ByteBuffers with dict at level $level with legacy $legacy" in {
      val size = input.length
      val inBuf = ByteBuffer.allocateDirect(size)
      inBuf.put(input)
      inBuf.flip()
      val compressed = ByteBuffer.allocateDirect(Zstd.compressBound(size).toInt);
      Zstd.compress(compressed, inBuf, dict, level)
      compressed.flip()
      val decompressed = ByteBuffer.allocateDirect(size)
      Zstd.decompress(decompressed, compressed, dict)
      decompressed.flip()
      val out = new Array[Byte](decompressed.remaining)
      decompressed.get(out)
      assert(input.toSeq == out.toSeq)
    }

    it should s"round-trip compression/decompression with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression in place with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = new Array[Byte](size)
      val compressed_size = Zstd.compress(compressed, input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = new Array[Byte](size)
      Zstd.decompressFastDict(decompressed, 0, compressed, 0, compressed_size.toInt, ddict)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression ByteBuffers with fast dict at level $level with byReference $legacy" in {
      val byReference = legacy // Reuse the variance flag here.
      val size = input.length
      val inBuf = ByteBuffer.allocateDirect(size)
      inBuf.put(input)
      inBuf.flip()
      val cdict = new ZstdDictCompress(dictInDirectByteBuffer, level, byReference)
      val compressed = ByteBuffer.allocateDirect(Zstd.compressBound(size).toInt);
      Zstd.compress(compressed, inBuf, cdict)
      compressed.flip()
      cdict.close
      val ddict = new ZstdDictDecompress(dictInDirectByteBuffer, byReference)
      val decompressed = ByteBuffer.allocateDirect(size)
      Zstd.decompress(decompressed, compressed, ddict)
      decompressed.flip()
      ddict.close
      val out = new Array[Byte](decompressed.remaining)
      decompressed.get(out)
      assert(Zstd.getDictIdFromFrameBuffer(compressed) == Zstd.getDictIdFromDictDirect(dictInDirectByteBuffer))
      assert(input.toSeq == out.toSeq)
    }

    it should s"round-trip compression/decompression with byte[]/fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression with fast/byte[] dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a byte[] and decompress with a ByteBuffer using byte[] dict $level with legacy $legacy" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val compressedBuffer = ByteBuffer.allocateDirect(compressed.size)
      compressedBuffer.put(compressed)
      compressedBuffer.limit(compressedBuffer.position())
      compressedBuffer.flip()

      val decompressedBuffer = Zstd.decompress(compressedBuffer, dict, size)
      val decompressed = new Array[Byte](size)
      decompressedBuffer.get(decompressed)
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a byte[] and decompress with a ByteBuffer using fast dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val compressed = Zstd.compress(input, cdict)
      val compressedBuffer = ByteBuffer.allocateDirect(compressed.size)
      compressedBuffer.put(compressed)
      compressedBuffer.flip()
      cdict.close

      val ddict = new ZstdDictDecompress(dict)
      val decompressedBuffer = Zstd.decompress(compressedBuffer, ddict, size)
      val decompressed = new Array[Byte](size)
      decompressedBuffer.get(decompressed)
      ddict.close
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a ByteBuffer and decompress with a byte[] using fast dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val inputBuffer = ByteBuffer.allocateDirect(size)
      inputBuffer.put(input)
      inputBuffer.flip()
      val compressedBuffer: ByteBuffer = Zstd.compress(inputBuffer, cdict)
      val compressed = new Array[Byte](compressedBuffer.limit() - compressedBuffer.position())
      compressedBuffer.get(compressed)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a ByteBuffer and decompress with a byte[] using byte[] dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val inputBuffer = ByteBuffer.allocateDirect(size)
      inputBuffer.put(input)
      inputBuffer.flip()
      val compressedBuffer: ByteBuffer = Zstd.compress(inputBuffer, dict, level)
      val compressed = new Array[Byte](compressedBuffer.limit() - compressedBuffer.position())
      compressedBuffer.get(compressed)
      cdict.close
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip streaming compression/decompression with byte[] dict with legacy $legacy " in {
      val size  = input.length
      val os    = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos   = new ZstdOutputStream(os, 1)
      zos.setDict(dict)
      val block = 128 * 1024
      var ptr   = 0
      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zos.write(input, ptr, chunk)
        ptr += chunk
      }
      zos.flush
      zos.close
      val compressed = os.toByteArray
      // now decompress
      val is    = new ByteArrayInputStream(compressed)
      val zis   = new ZstdInputStream(is)
      zis.setDict(dict)
      val output= Array.fill[Byte](size)(0)
      ptr       = 0

      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zis.read(output, ptr, chunk)
        ptr += chunk
      }
      zis.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming compression/decompression with fast dict with legacy $legacy " in {
      val size  = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, 1)
      val os    = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos   = new ZstdOutputStream(os, 1)
      zos.setDict(cdict)
      val block = 128 * 1024
      var ptr   = 0
      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zos.write(input, ptr, chunk)
        ptr += chunk
      }
      zos.close
      val compressed = os.toByteArray

      // now decompress
      val ddict = new ZstdDictDecompress(dict)
      val is    = new ByteArrayInputStream(compressed)
      val zis   = new ZstdInputStream(is)
      zis.setDict(ddict)
      val output= Array.fill[Byte](size)(0)
      ptr       = 0

      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zis.read(output, ptr, chunk)
        ptr += chunk
      }
      zis.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming compression/decompression with multiple fast dicts with legacy $legacy " in {
      // given: compress using first one dictionary, then another
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)

      val compressedWithDict1 = compressWithDict(cdict)
      val compressedWithDict2 = compressWithDict(cdict2)

      // when: decompress with the both dictionaries configured and multiple dict references enabled
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)

      val dicts = ddict::ddict2::Nil
      val uncompressed1 = uncompressWithMultipleDicts(compressedWithDict1, dicts)
      val uncompressed2 = uncompressWithMultipleDicts(compressedWithDict2, dicts)

      // then: both compressed inputs decompressed successfully
      assert(uncompressed1.toSeq == input.toSeq)
      assert(Zstd.getDictIdFromFrame(compressedWithDict1) == Zstd.getDictIdFromDict(dict))

      assert(uncompressed2.toSeq == input.toSeq)
      assert(Zstd.getDictIdFromFrame(compressedWithDict2) == Zstd.getDictIdFromDict(dict2))
    }

    it should s"round-trip streaming compression/decompression with multiple fast dicts with legacy $legacy and disabled multiple dict references" in {
      // given: compress using first one dictionary, then another
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)

      val compressedWithDict1 = compressWithDict(cdict)
      val compressedWithDict2 = compressWithDict(cdict2)

      // when: decompress with the both dictionaries configured and multiple dict references disabled
      //       -> should be used only the second one
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)

      val dicts = ddict :: ddict2 :: Nil
      val uncompressed2 = uncompressWithMultipleDicts(compressedWithDict2, dicts, multipleDdicts = false)

      // then: decompression of compressed with the first dict should fail with dictionary mismatch,
      //       the second one should be decompressed successfully
      val caughtException = intercept[ZstdIOException] {
        uncompressWithMultipleDicts(compressedWithDict1, dicts, multipleDdicts = false)
      }
      assert(caughtException.getMessage == "Dictionary mismatch")

      assert(uncompressed2.toSeq == input.toSeq)
      assert(Zstd.getDictIdFromFrame(compressedWithDict2) == Zstd.getDictIdFromDict(dict2))
    }

    def compressWithDict(cdict: ZstdDictCompress): Array[Byte] = {
      val os = new ByteArrayOutputStream(Zstd.compressBound(input.length.toLong).toInt)
      Using(new ZstdOutputStream(os, 1)) { zos =>
        zos.setDict(cdict)
        zos.write(input)
      }
      os.toByteArray
    }

    def uncompressWithMultipleDicts(
      compressed: Array[Byte],
      dicts: List[ZstdDictDecompress],
      multipleDdicts: Boolean = true
    ): Array[Byte] = {
      Using.resources(
        new ZstdInputStream(new ByteArrayInputStream(compressed))
          .setRefMultipleDDicts(multipleDdicts),
        new ByteArrayOutputStream()
      ) { (zis, os) =>
        dicts.foreach(zis.setDict)

        zis.transferTo(os)
        os.toByteArray
      }
    }

    it should s"round-trip streaming ByteBuffer compression/decompression with byte[] dict with legacy $legacy" in {
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
      // compress
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStream(os, 1)
      osw.setDict(dict)
      osw.compress(ib)
      osw.flush
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))

      // now decompress
      val zis   = new ZstdDirectBufferDecompressingStream(os)
      zis.setDict(dict)
      val output= Array.fill[Byte](size)(0)
      val block = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      zis.close
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming ByteBuffer compression/decompression with fast dict with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.size, 1)
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)

      // compress
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStream(os, 1)
      osw.setDict(cdict)
      osw.compress(ib)
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))

      // now decompress
      val zis   = new ZstdDirectBufferDecompressingStream(os)
      val ddict = new ZstdDictDecompress(dict)
      zis.setDict(ddict)
      val output= Array.fill[Byte](size)(0)
      val block = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      zis.close
      assert(input.toSeq == output.toSeq)
    }

    def readFully(zis: InputStream, size: Int): Array[Byte] = {
      val output = Array.fill[Byte](size)(0)
      var ptr = 0
      while (ptr < size) {
        val read = zis.read(output, ptr, size - ptr)
        assert(read > 0)
        ptr += read
      }
      output
    }

    it should s"not free a fast dict while ZstdInputStream uses it with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val ddict = new ZstdDictDecompress(dict)
      val zis = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
      zis.setDict(ddict)
      // the stream references the native dictionary, closing it would leave a dangling pointer
      val e = intercept[IllegalStateException] {
        ddict.close()
      }
      assert(e.getMessage == "Attempt to close while in use")
      val output = readFully(zis, input.length)
      zis.close()
      assert(input.toSeq == output.toSeq)
      // closing the stream releases the dictionary
      ddict.close()
    }

    it should s"release the previous fast dict and failed fast dicts in ZstdInputStream with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val zis = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
      zis.setDict(ddict2)
      zis.setDict(ddict)
      // the replaced dictionary is not in use anymore
      ddict2.close()
      intercept[IllegalStateException] {
        ddict.close()
      }
      // advance into the frame, so loading a dictionary now fails
      val head = readFully(zis, 16)
      val ddict3 = new ZstdDictDecompress(dict2)
      intercept[ZstdIOException] {
        zis.setDict(ddict3)
      }
      // a dictionary that failed to load is not retained
      ddict3.close()
      val rest = readFully(zis, input.length - 16)
      zis.close()
      assert(input.toSeq == (head ++ rest).toSeq)
      ddict.close()
    }

    it should s"keep every fast dict in use while ZstdInputStream refers to multiple dicts with legacy $legacy" in {
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val zis = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(Zstd.compressUsingDict(input, dict, 1)))
      zis.setRefMultipleDDicts(true)
      zis.setDict(ddict2)
      zis.setDict(ddict)
      // in multi-dict mode the native context refers to both dictionaries
      intercept[IllegalStateException] {
        ddict2.close()
      }
      intercept[IllegalStateException] {
        ddict.close()
      }
      // leaving multi-dict mode releases all but the last loaded dictionary
      zis.setRefMultipleDDicts(false)
      ddict2.close()
      intercept[IllegalStateException] {
        ddict.close()
      }
      zis.close()
      ddict.close()
    }

    it should s"not free a fast dict while ZstdOutputStream uses it with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val os = new ByteArrayOutputStream(Zstd.compressBound(input.length.toLong).toInt)
      val zos = new ZstdOutputStreamNoFinalizer(os, 1)
      zos.setDict(cdict2)
      zos.setDict(cdict)
      // the replaced dictionary is not in use anymore
      cdict2.close()
      // the stream references the native dictionary, closing it would leave a dangling pointer
      val e = intercept[IllegalStateException] {
        cdict.close()
      }
      assert(e.getMessage == "Attempt to close while in use")
      zos.write(input)
      zos.close()
      // closing the stream releases the dictionary
      cdict.close()
      val compressed = os.toByteArray
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == Zstd.decompress(compressed, dict, input.length).toSeq)
    }

    it should s"not free a fast dict while ZstdBufferDecompressingStream uses it with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val zis = new ZstdBufferDecompressingStreamNoFinalizer(ByteBuffer.wrap(compressed))
      zis.setDict(ddict2)
      zis.setDict(ddict)
      // the replaced dictionary is not in use anymore
      ddict2.close()
      // the stream references the native dictionary, closing it would leave a dangling pointer
      val e = intercept[IllegalStateException] {
        ddict.close()
      }
      assert(e.getMessage == "Attempt to close while in use")
      val output = ByteBuffer.allocate(input.length)
      val block = ByteBuffer.allocate(128 * 1024)
      while (zis.hasRemaining) {
        block.clear()
        zis.read(block)
        block.flip()
        output.put(block)
      }
      zis.close()
      assert(!zis.hasRemaining)
      assert(input.toSeq == output.array.toSeq)
      // closing the stream releases the dictionary
      ddict.close()
    }

    it should s"not free a fast dict while ZstdCompressCtx uses it with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val cctx = new ZstdCompressCtx()
      cctx.loadDict(cdict2)
      cctx.loadDict(cdict)
      // the replaced dictionary is not in use anymore
      cdict2.close()
      // the context references the native dictionary, closing it would leave a dangling pointer
      val e = intercept[IllegalStateException] {
        cdict.close()
      }
      assert(e.getMessage == "Attempt to close while in use")
      val compressed = cctx.compress(input)
      cctx.close()
      // closing the context releases the dictionary
      cdict.close()
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == Zstd.decompress(compressed, dict, input.length).toSeq)
    }

    it should s"release the fast dict of a ZstdCompressCtx on byte[] dict load, failed load and reset with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val cctx = new ZstdCompressCtx()
      cctx.loadDict(cdict2)
      // loading a byte[] dictionary replaces the fast dict, which is not in use anymore
      cctx.loadDict(dict2)
      cdict2.close()
      cctx.loadDict(cdict)
      // start a frame, so loading a dictionary now fails
      val src = ByteBuffer.allocateDirect(input.length)
      src.put(input)
      src.flip()
      val dst = ByteBuffer.allocateDirect(Zstd.compressBound(input.length.toLong).toInt)
      cctx.compressDirectByteBufferStream(dst, src, EndDirective.CONTINUE)
      val cdict3 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      intercept[ZstdException] {
        cctx.loadDict(cdict3)
      }
      // a dictionary that failed to load is not retained
      cdict3.close()
      // the loaded dictionary is still in use
      intercept[IllegalStateException] {
        cdict.close()
      }
      while (!cctx.compressDirectByteBufferStream(dst, src, EndDirective.END)) {}
      dst.flip()
      val compressed = new Array[Byte](dst.remaining)
      dst.get(compressed)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == Zstd.decompress(compressed, dict, input.length).toSeq)
      // reset removes the dictionary from the context and releases it
      cctx.reset()
      cdict.close()
      val plain = cctx.compress(input)
      cctx.close()
      assert(Zstd.getDictIdFromFrame(plain) == 0)
      assert(input.toSeq == Zstd.decompress(plain, input.length).toSeq)
    }

    it should s"not free a fast dict while ZstdDecompressCtx uses it with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val dctx = new ZstdDecompressCtx()
      dctx.loadDict(ddict2)
      dctx.loadDict(ddict)
      // the replaced dictionary is not in use anymore
      ddict2.close()
      // the context references the native dictionary, closing it would leave a dangling pointer
      val e = intercept[IllegalStateException] {
        ddict.close()
      }
      assert(e.getMessage == "Attempt to close while in use")
      val decompressed = dctx.decompress(compressed, input.length)
      dctx.close()
      // closing the context releases the dictionary
      ddict.close()
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"release the fast dict of a ZstdDecompressCtx on byte[] dict load and reset with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val dctx = new ZstdDecompressCtx()
      dctx.loadDict(ddict2)
      // loading a byte[] dictionary replaces the fast dict, which is not in use anymore
      dctx.loadDict(dict)
      ddict2.close()
      assert(input.toSeq == dctx.decompress(compressed, input.length).toSeq)
      dctx.loadDict(ddict)
      intercept[IllegalStateException] {
        ddict.close()
      }
      // reset removes the dictionary from the context and releases it
      dctx.reset()
      ddict.close()
      intercept[ZstdException] {
        dctx.decompress(compressed, input.length)
      }
      dctx.close()
    }

    it should s"not free a fast dict while ZstdDirectBufferCompressingStream uses it with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val os = ByteBuffer.allocateDirect(Zstd.compressBound(input.length.toLong).toInt)
      val ib = ByteBuffer.allocateDirect(input.length)
      ib.put(input)
      ib.flip()
      val osw = new ZstdDirectBufferCompressingStreamNoFinalizer(os, 1)
      osw.setDict(cdict2)
      osw.setDict(cdict)
      // the replaced dictionary is not in use anymore
      cdict2.close()
      val e = intercept[IllegalStateException] {
        cdict.close()
      }
      assert(e.getMessage == "Attempt to close while in use")
      osw.compress(ib)
      // the initialized native stream references the dictionary, closing it would leave a dangling pointer
      val e2 = intercept[IllegalStateException] {
        cdict.close()
      }
      assert(e2.getMessage == "Attempt to close while in use")
      osw.close()
      // closing the stream releases the dictionary
      cdict.close()
      os.flip()
      val compressed = new Array[Byte](os.remaining)
      os.get(compressed)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == Zstd.decompress(compressed, dict, input.length).toSeq)
    }

    it should s"release the fast dict of an unused or byte[] dict ZstdDirectBufferCompressingStream with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      // closing a stream that was never used releases the dictionary
      val unused = new ZstdDirectBufferCompressingStreamNoFinalizer(ByteBuffer.allocateDirect(16), 1)
      unused.setDict(cdict)
      unused.close()
      cdict.close()
      // a byte[] dictionary replaces the fast dict, which is not in use anymore
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val os = ByteBuffer.allocateDirect(Zstd.compressBound(input.length.toLong).toInt)
      val ib = ByteBuffer.allocateDirect(input.length)
      ib.put(input)
      ib.flip()
      val osw = new ZstdDirectBufferCompressingStreamNoFinalizer(os, 1)
      osw.setDict(cdict2)
      osw.setDict(dict)
      cdict2.close()
      osw.compress(ib)
      osw.close()
      os.flip()
      val compressed = new Array[Byte](os.remaining)
      os.get(compressed)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == Zstd.decompress(compressed, dict, input.length).toSeq)
    }
  }

  val boundsDict = input.slice(0, 16 * 1024)
  val invalidDictRanges = List(
    (boundsDict.length - 10, boundsDict.length),   // reads past the end of the array
    (-1, boundsDict.length),                       // negative offset reads before the array
    (-1024, 16),                                   // negative offset with a small length
    (0, -1),                                       // negative length
    (0, boundsDict.length + 1),                    // length larger than the array
    (1, Int.MaxValue),                             // overflowing offset + length
    (boundsDict.length + 1, 0),                    // offset past the end of the array
    (Int.MaxValue, 1)                              // overflowing offset
  )

  for ((offset, length) <- invalidDictRanges) {
    "ZstdDictCompress" should s"reject out of bounds dictionary range offset=$offset length=$length" in {
      intercept[IllegalArgumentException] {
        new ZstdDictCompress(boundsDict, offset, length, 1)
      }
    }

    "ZstdDictDecompress" should s"reject out of bounds dictionary range offset=$offset length=$length" in {
      intercept[IllegalArgumentException] {
        new ZstdDictDecompress(boundsDict, offset, length)
      }
    }
  }

  "ZstdDict" should "round-trip with a dictionary at an offset filling the rest of the buffer" in {
    val prefix = 7
    val buffer = Array.fill[Byte](prefix)(0) ++ boundsDict
    val cdict = new ZstdDictCompress(buffer, prefix, buffer.length - prefix, 1)
    val compressed = Zstd.compress(input, cdict)
    cdict.close
    val ddict = new ZstdDictDecompress(buffer, prefix, buffer.length - prefix)
    val decompressed = Zstd.decompress(compressed, ddict, input.length)
    ddict.close
    assert(input.toSeq == decompressed.toSeq)
  }
}
