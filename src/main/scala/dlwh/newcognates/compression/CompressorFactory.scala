package dlwh.newcognates.compression

import dlwh.newcognates.FastTransducerFactory

/**
 * 
 * @author dlwh
 */

trait CompressorFactory[T]  { this : FastTransducerFactory =>
  trait Compressor {

  }
}