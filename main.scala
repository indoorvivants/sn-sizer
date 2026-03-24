package sizemap

import decline_derive.*
import java.nio.file.Path
import java.io.File
import sizemap.BinaryFile
import java.io.RandomAccessFile

import com.indoorvivants.demangler.*
import scala.util.Try
import java.io.FileWriter
import scala.util.Using
import scala.scalanative.unsafe.Zone

enum CLI(val path: Path) derives CommandApplication:
  case Serve(
      @Positional("filename")
      filename: Path,
      @Short("p")
      port: Option[Int]
  ) extends CLI(filename)
end CLI

@main def hello(args: String*) =
  val cli = CommandApplication.parseOrExit[CLI](args)

  val file = cli.path.toFile()
  implicit val bf: BinaryFile = new BinaryFile(
    new RandomAccessFile(file, "r")
  )

  import macho.*

  val ftype = FileType.detect(bf)

  def demangled(sizes: Map[String, Long]): Map[String, Long] =
    sizes
      .filter(_._2 > 0)
      .map: (name, size) =>
        if name.startsWith("__S") && ftype == FileType.MachO then
          val stripped = name.stripPrefix("_")
          Try(Demangler.demangle(stripped)).fold(
            _ =>
              scribe.warn(s"Failed to demangle symbol ${stripped}")
              (name, size)
            ,
            s => (s, size)
          )
        else if name.startsWith("_S") && ftype == FileType.ELF then
          Try(Demangler.demangle(name)).fold(
            _ =>
              scribe.warn(s"Failed to demangle symbol ${name}")
              (name, size)
            ,
            s => (s, size)
          )
        else if name.startsWith("_Z") || name.startsWith("__Z") then
          (s"<C++>.$name", size)
        else ("<C>." + name, size)

  val indiv =
    demangled(
      if ftype == FileType.MachO then MachO.parse(bf).sizes
      else if ftype == FileType.ELF then ELF.parse(bf).sizes
      else
        sys.error(
          s"Only 64-bit MachO and ELF files are supported! Detected: ${ftype}"
        )
    )

  cli match
    case CLI.Serve(filename, port) =>
      Zone:
        Server(filename.toString, indiv.toMap).serve(port)
end hello
