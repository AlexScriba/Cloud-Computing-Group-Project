package runner

import io.circe.parser
import io.circe.generic.semiauto.deriveDecoder
import scala.io.Source
import scala.util.Try

case class ExecutionSetup(args: List[String])

case class LangData(
  name: String,
  dir: String,
  setup: ExecutionSetup,
  execute: ExecutionSetup
)

object Json {
  
  val testJson = 
    """
      [
        {
          "name": "Javascript",
          "dir": "../Benchmarking/Javascript/",
          "setup": {
            "args": ["npm install"]
          },
          "execute": {
            "args": ["tsc", "node ./js/fib.ts"]
          }
        }
      ]
    """.stripMargin
  
  def decodeData(dataString: String): Either[Throwable, List[LangData]] = {
    implicit val executionSetupDecoder = deriveDecoder[ExecutionSetup]
    implicit val langDataDecoder = deriveDecoder[LangData]

    parser.decode[List[LangData]](dataString) match {
      case Left(err) => Left(new Throwable(s"Error formatting JSON: ${err.getMessage()}"))
      case data => data
    }
  }

  def readData(filePath: String): Either[Throwable, String] = {
    Try {
      Source.fromFile(filePath).getLines().mkString("\n")
    }.toEither
  }
}
