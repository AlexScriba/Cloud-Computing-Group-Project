package runner

import scala.io.Source

object Main extends App {
  val filePath: String = args.headOption match {
    case Some(str) => str
    case None => throw new Error("Error: A file path argument is required")
  }

  val res = 
    for {
      jsonContents <- Json.readData(filePath)
      langData <- Json.decodeData(jsonContents)
    } yield Cmd.setupAll(langData)

  res match {
    case Left(err) => throw err
    case _ => // do nothing
  }
}
