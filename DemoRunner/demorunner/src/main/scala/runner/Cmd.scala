package runner

import scala.sys.process._
import java.nio.file.{Paths, Files, Path}

case class ExecutionSetup(args: List[String])

object Cmd {
  def setupAll(langData: List[LangData]): Unit = {
    langData.foreach { lang => 
      println(s"Running Setup for: ${lang.name}")

      val langPath = Paths.get(lang.dir)

      execute(lang.setup, langPath)
    }
  }

  def runAll(langData: List[LangData]): Unit = {
    val results = langData.map { lang => 
      println(s"Running Benchmark for: ${lang.name}")

      val langPath = Paths.get(lang.dir)

      val times = execute(lang.run, langPath)
      (lang -> times)
    }

    results.foreach {
      case (langData, times) => 
        println(s"${langData.name}:")

        val commands = langData.run.args
        val commandTimes = commands zip times

        for {
          (command, time) <- commandTimes
        } println(s"\t${command}\t->\t${time}")

        println()
    }

  }

  def execute(conf: ExecutionSetup, path: Path): List[Long]= {
    conf.args.map { cmd =>
      println(s"Running: ${cmd}")

      val processOutput = new StringBuilder
      val processError = new StringBuilder

      val logger = ProcessLogger(
        (output: String) => processOutput.append(output).append("\n"),
        (error: String) => processOutput.append(error).append("\n")
      )

      val startTime = System.currentTimeMillis
      val exitCode = Process(cmd, path.toFile).!(logger)
      val endTime = System.currentTimeMillis

      if(exitCode != 0) {
        println(s"Error code: $exitCode:\n${processError.toString}")
        sys.exit(1)
      }

      println("Success")
      println("Output:")
      println(processOutput.toString)

      val elapsedTime = endTime - startTime
      println(s"Time: $elapsedTime\n")

      elapsedTime
    }
  }
}
