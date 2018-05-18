package org.scoverage.coveralls

import java.io._
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.io.Source
import com.fasterxml.jackson.core.{JsonEncoding, JsonFactory, JsonGenerator}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class CoverallPayloadWriter(
    repoRootDir: File,
    coverallsFile: File,
    repoToken: Option[String],
    travisJobId: Option[String],
    serviceName: Option[String],
    sourceEncoding: Option[String],
    gitClient: GitClient) {

  val repoRootDirStr = repoRootDir.getCanonicalPath.replace(File.separator, "/") + "/"
  import gitClient._

  def generator(file: File) = {
    if (!file.getParentFile.exists) file.getParentFile.mkdirs
    val factory = new JsonFactory
    factory.createGenerator(file, JsonEncoding.UTF8)
  }

  def generator(sw: StringWriter) = {
    val factory = new JsonFactory
    factory.createGenerator(sw)
  }

  def makeCoverallsFile(sourceRawJson: String): Unit = {
    val gen = generator(coverallsFile)

    gen.writeStartObject()

    def writeOpt(fieldName: String, holder: Option[String]) =
      holder foreach { gen.writeStringField(fieldName, _) }

    writeOpt("repo_token", repoToken)
    writeOpt("service_name", serviceName)
    writeOpt("service_job_id", travisJobId)
    writeOpt("service_pull_request", sys.env.get("CI_PULL_REQUEST"))

    addGitInfo(gen)

    gen.writeFieldName("source_files")
    gen.writeStartArray()

    gen.writeRaw(sourceRawJson)

    gen.writeEndArray()
    gen.writeEndObject()
    gen.flush()
    gen.close()
  }

  private def addGitInfo(gen: JsonGenerator) {
    gen.writeFieldName("git")
    gen.writeStartObject()

    gen.writeFieldName("head")
    gen.writeStartObject()

    val commitInfo = lastCommit()

    gen.writeStringField("id", commitInfo.id)
    gen.writeStringField("author_name", commitInfo.authorName)
    gen.writeStringField("author_email", commitInfo.authorEmail)
    gen.writeStringField("committer_name", commitInfo.committerName)
    gen.writeStringField("committer_email", commitInfo.committerEmail)
    gen.writeStringField("message", commitInfo.shortMessage)

    gen.writeEndObject()

    gen.writeStringField("branch", currentBranch)

    gen.writeFieldName("remotes")
    gen.writeStartArray()

    addGitRemotes(remotes, gen)

    gen.writeEndArray()

    gen.writeEndObject()
  }

  private def addGitRemotes(remotes: Seq[String], gen: JsonGenerator) {
    remotes.foreach( remote => {
      gen.writeStartObject()
      gen.writeStringField("name", remote)
      gen.writeStringField("url", remoteUrl(remote))
      gen.writeEndObject()
    })
  }

  def addSourceFile(report: SourceFileReport): Future[String] = Future {
    val md5: MessageDigest = MessageDigest.getInstance("MD5")

    // create a name relative to the project root (rather than the module root)
    // this is needed so that coveralls can find the file in git.
    val fileName = report.file.replace(repoRootDirStr, "")

    val sw = new StringWriter()
    val tempGen = generator(sw)

    tempGen.writeStartObject()
    tempGen.writeStringField("name", fileName)

    val source = sourceEncoding match {
     case Some(enc) => Source.fromFile(report.file, enc)
     case None => Source.fromFile(report.file)
    }
    val sourceCode = source.getLines().mkString("\n")
    source.close()

    val sourceDigest = md5.digest(sourceCode.getBytes).map("%02X" format _).mkString

    tempGen.writeStringField("source_digest", sourceDigest)

    tempGen.writeFieldName("coverage")
    tempGen.writeStartArray()
    report.lineCoverage.foreach {
      case Some(x) => tempGen.writeNumber(x)
      case _ => tempGen.writeNull()
    }
    tempGen.writeEndArray()
    tempGen.writeEndObject()
    tempGen.flush()
    tempGen.close()

    sw.toString
  }

  def concatTempJson(tempJson: Seq[String]): Future[String] = {
    def concat(s1: String, s2: String): Future[String] =
      Future {
        Seq(s1, s2).mkString(",")
      }

    def inner(js: Seq[String]): Future[Seq[String]] = js match {
      case Nil => Future.successful(Nil)
      case x +: Nil => Future.successful(Seq(x))
      case x +: y +: xs =>
        (concat(x, y) zip inner(xs)).map {
          case (a, b) => a +: b
        }
    }

    def build(js: Seq[String]): Future[String] = js match {
      case Nil => Future.failed(new IllegalStateException())
      case x +: Nil => Future.successful(x)
      case xs => inner(xs).flatMap(build)
    }

    build(tempJson)
  }
}
