package runner

import scala.io.Source

object Mode extends Enumeration {
  val Run, Setup = Value
}

object Main extends App {

  val mode: Mode.Value = args.lift(0) match {
    case Some(modeStr) => modeStr match {
      case "run" => Mode.Run
      case "setup" => Mode.Setup
      case _ => throw new Error("Mode not recognized. Options: run, setup")
    }
    case None => throw new Error("Please supply a mode. Options: run, setup")
  }

  val filePath: String = args.lift(1) match {
    case Some(str) => str
    case None => throw new Error("A file path argument is required")
  }

  val res = 
    for {
      jsonContents <- Json.readData(filePath)
      langData <- Json.decodeData(jsonContents)
    } yield langData
  val langData = Utils.throwIfErr(res)

  mode match {
    case Mode.Run => Cmd.runAll(langData)
    case Mode.Setup => Cmd.setupAll(langData)
  }
}

object Utils {
  def throwIfErr[A](either: Either[Throwable, A]): A = {
    either match {
      case Left(err) => throw err
      case Right(data) => data
    }
  }
}
