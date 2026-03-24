package sizemap

import scala.util.boundary

enum FileType:
  case MachO, ELF
  case Unknown(firstFourBytes: String)

object FileType:
  def detect(ds: BinaryFile): FileType =
    ds.withRestore:
      val x = CommonParsers.uint32()(using Endianness.LITTLE, ds)
      if x == 0xfeedfacfL then MachO
      else if x == 0x7f454c46 then ELF
      else
        Unknown(
          x.toHexString
        )
      end if
end FileType
