package sizemap

import httplib.all.*
import scalanative.unsafe.*

class Server(filename: String, data: Map[String, Long]):
  def serve(port: Option[Int])(using Zone) =
    val handlers = Handlers()

    val dataJS =
      val serialised = data.toList.sortBy(s => (s._2 * -1, s._1))
      toCString(s"""
      |const FILENAME = "$filename";
      |const RAW = ${serialised
                    .map { case (k, v) => s"['$k', $v]" }
                    .mkString("[\n", ", \n", "\n]")};
      |const SYMBOL_NAMES_TOTAL = ${data.keySet.map(_.getBytes().length).sum};
      """.stripMargin.trim)
    end dataJS

    val indexHtml = io.Source
      .fromInputStream(getClass.getResourceAsStream("/index.html"))
      .mkString

    Server.raw = dataJS

    Server.indexHtml = toCString(indexHtml)

    (!handlers).data = () =>
      Resp(Code.OK, BodyType.JAVASCRIPT, Server.raw)(using Server.z)

    (!handlers).index = () =>
      Resp(Code.OK, BodyType.HTML, Server.indexHtml)(using Server.z)

    start_server(handlers, c"localhost", port.getOrElse(0))
  end serve
end Server

object Server:
  var raw: CString = null
  var indexHtml: CString = null
  val z = Zone.open()
