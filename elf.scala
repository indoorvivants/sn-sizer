package sizemap

sealed trait Bits extends Product with Serializable
object Bits:
  case object X32 extends Bits
  case object X64 extends Bits

case class Header()
case class ProgramHeader(
)
case class Section(
    sh_name: UnsignedInt,
    sh_type: UnsignedInt,
    sh_offset: Long,
    sh_link: Int,
    sh_size: UnsignedInt
)
case class ELF(header: Header, sizes: Map[String, Long])

object ELF:
  import CommonParsers.*
  def parse(ds: BinaryFile): ELF =
    implicit val stream: BinaryFile = ds

    val magic = uint32()(using Endianness.BIG, stream)

    val cls = uint8()(using Endianness.BIG, stream)

    val endianness = uint8()(using Endianness.BIG, stream)

    val version = uint8()(using Endianness.BIG, stream)
    val abi = uint8()(using Endianness.BIG, stream)
    val abi_version = uint8()(using Endianness.BIG, stream)
    val padding = stream.skipNBytes(7L)

    implicit val endi: Endianness =
      if endianness == 1 then Endianness.LITTLE else Endianness.BIG
    implicit val bits: Bits = if cls == 1 then Bits.X32 else Bits.X64

    val fileType = uint16()
    val machine = uint16()
    val versionAgain = uint32()

    val entryPointAddress = readVariableSize()
    val programHeaderStart = readVariableSize()
    val sectionsHeaderStart = readVariableSize()

    val flags = uint32()
    val headerSize = uint16()
    val programHeaderSize = uint16()
    val programHeaderEntries = uint16()
    val sectionsHeaderSize = uint16()
    val sectionsHeaderEntries = uint16()

    val sectionNamesEntryIndex = uint16()

    import Bits.*

    bits match
      case X32 => assert(ds.position() == 0x34)
      case X64 => assert(ds.position() == 0x40)

    ds.seek(sectionsHeaderStart)
    val sections = readSectionHeaders(sectionsHeaderEntries)
    val sizes = Map.newBuilder[String, Long]
    val SHT_SYMTAB = sections.find(_.sh_type == 0x02)

    SHT_SYMTAB.foreach: symtab =>
      val strTableSection = sections(symtab.sh_link)
      val strings =
        val builder = Map.newBuilder[Int, String]
        ds.withRestore:
          ds.seek(strTableSection.sh_offset)
          val strTable = ds.readNBytes(strTableSection.sh_size.toInt)
          var i = 0
          val sb = Array.newBuilder[Byte]
          while i < strTable.length do
            if strTable(i) == 0 then
              val str = sb.result()
              if str.length > 0 then
                builder += ((i - str.length) -> new String(str))
              sb.clear()
            else sb.addOne(strTable(i))
            end if
            i += 1
          end while
        builder.result()
      end strings

      ds.withRestore:
        ds.seek(symtab.sh_offset)
        val entries = symtab.sh_size.toInt / 24

        for i <- 0 until entries do
          val st_name = uint32()
          val st_info = uint8()
          val st_other = uint8()
          val st_shndx = uint16()
          val st_value = uint64()
          val st_size = uint64()

          strings.get(st_name.toInt).foreach(name => sizes += (name -> st_size))
        end for

    ELF(Header(), sizes.result())
  end parse

  def readSectionHeaders(
      entries: UnsignedShort
  )(implicit ds: BinaryFile, bits: Bits, endi: Endianness) =

    val sections = List.newBuilder[Section]

    for i <- 0 until entries do
      val sectionNameAddress = uint32()
      val sectionType = uint32()
      val flags = readVariableSize()
      val virtualAddressInMemory = readVariableSize()
      val offsetInFileImage = readVariableSize()
      val sectionSizeInFileImage = readVariableSize()
      val sectionIndex = uint32()
      val sectionInfo = uint32()
      val sectionAlignment = readVariableSize()
      val entrySize = readVariableSize()

      sections += Section(
        sectionNameAddress,
        sectionType,
        offsetInFileImage,
        sectionIndex.toInt,
        sectionSizeInFileImage
      )

    end for

    sections.result()
  end readSectionHeaders

  // def readProgramHeaders(
  //     entries: UnsignedShort
  // )(implicit ds: BinaryFile, bits: Bits, endi: Endianness) =
  //   // pprint.log(s"$entries")
  //   for i <- 0 until entries do
  //     var flags: UnsignedInt = 0
  //     val tpe = uint32()

  //     if bits == Bits.X64 then flags = uint32()

  //     val offsetInFileImage = readVariableSize()
  //     val virtualAddressInMemory = readVariableSize()
  //     val physicalAddress = readVariableSize()
  //     val sizeInFileImage = readVariableSize()
  //     val sizeInMemory = readVariableSize()

  //     if bits == Bits.X32 then flags = uint32()

  //     val align = readVariableSize()

  // val MAGIC = 0x7f454c46

  def readVariableSize()(implicit
      ds: BinaryFile,
      bits: Bits,
      endi: Endianness
  ) =
    bits match
      case Bits.X32 => uint32().toLong
      case Bits.X64 => uint64()

end ELF
