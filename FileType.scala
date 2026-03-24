package sizemap

import scala.util.boundary

enum FileType:
  case MachO, ELF
  case Unknown(firstFourBytes: String)

object FileType:
  def detect(ds: BinaryFile): FileType =
    ds.withRestore:
      lazy val littleEndian =
        ds.withRestore(CommonParsers.uint32()(using Endianness.LITTLE, ds))
      lazy val bigEndian = CommonParsers.uint32()(using Endianness.BIG, ds)
      if littleEndian == 0xfeedfacfL then MachO
      else if bigEndian == 0x7f454c46 then ELF
      else
        Unknown(
          bigEndian.toHexString
        )
      end if
end FileType
