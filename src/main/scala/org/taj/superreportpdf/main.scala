package org.taj.superreportpdf

/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 TAJ - Société d'avocats
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */


import java.io.{FileOutputStream, File}
import com.itextpdf.text.pdf._
import com.itextpdf.text.Rectangle
import com.itextpdf.text.pdf.parser._
import scala.collection.mutable.ArrayBuffer


object main {
  val usage =
    """
      |Usage: superpdfreport --attachments C:\path\folder\ --original-pdf C:\path\file.pdf [--verbose] --save-as filename
    """.stripMargin
  val description = s"""
    |Super PDF Report
    |================
    |Super PDF Report helps you to create awesome PDF report including your attachments in a very easy way.
    |Put all your attachments in one folder.
    |Then, in your report insert the name of the attachment file where you want it to be available.
    |Start the command.
    |A little paperclip will appear next to the attachment name. You can click on it to open the attachment from the PDF.
    |Enjoy.

    $usage

    |#protip: Don't forget to let a little space after the name of the file in your document to let Super PDF Report insert a clickable link.

    |Super PDF Report has been written by the TMC Paris office of the lawfirm TAJ and is under MIT licence.
""".stripMargin
  val icon = "PushPin"
  var verbose = false
  var attachmentFolder: Option[File] = None
  var originalPDF: Option[File] = None
  var finalPDF: Option[File] = None
  val unknown = "(^-[^\\s])".r
  val ArgParser: PartialFunction[List[String], List[String]] = {
    case "--verbose" :: tail => verbose = true; tail
    case "--attachments" :: (arg: String) :: tail => attachmentFolder = Option(new File(arg)); tail
    case "--original-pdf" :: (arg: String) :: tail => originalPDF = Option(new File(arg)); tail
    case "--save-as" :: (arg: String) :: tail => finalPDF = Option(new File(arg)); tail
    case unknown(bad) :: tail => die("unknown argument " + bad + "\n" + usage)
  }

  val argTests = List("--attachments", "C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\attachments",
    "--original-pdf", "C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\test4.pdf", "--save-as",
    "C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\result.pdf", "--verbose")

  def main(args: Array[String]) {

    val listArgt = argTests.toList

    if (listArgt.length == 0) die(description, displayError = false)

    parseArgs(listArgt, ArgParser)

    List((attachmentFolder, "Attachment folder"), (originalPDF, "Original PDF"), (finalPDF, "Destination PDF")) filter (_._1.isEmpty) foreach {
      case (_, text) => die(s"$text is missing.\n$usage")
    }

    List(attachmentFolder, originalPDF) filter (!_.get.exists()) foreach {
      case Some(file) => die(s"This path doesn't exist:\n${file.getAbsolutePath}\n$usage")
      case None =>
    }

    if (!finalPDF.get.getParentFile.isDirectory) die("PDF destination folder doesn't exist.\n" + usage)

    if (!attachmentFolder.get.isDirectory) die("Attachment folder should be a folder.\n" + usage)

    val listFiles = attachmentFolder.get.listFiles().toList

    if (listFiles.size == 0) die(s"Folder ${attachmentFolder.get.getAbsolutePath} is empty.\n$usage")

    if (finalPDF.get.exists()) {
      finalPDF.get.delete()
      if (verbose) println(s"Existing file deleted:\n ${finalPDF.get.getAbsolutePath}")
    }

    val result = searchText(originalPDF.get, listFiles)
      .map {
      case (line, page, attachment) =>
        val x = line.getEndPoint.get(0)
        val y = line.getEndPoint.get(1)
        if (verbose) println(s"Add attachment page $page:\nExpression searched=${attachment.getName}\nX=$x\nY=$y\n")
        val rect = new Rectangle(x + 3, y, x + 10, y + 17)
        (page, rect, attachment, attachment.getName)
    }
    addAttachments(originalPDF.get, finalPDF.get, result)
  }

  def readPDFProperties(src: File) = {
    val reader = new PdfReader(src.getAbsolutePath)
    println(reader.getNumberOfPages)
  }

  def searchText(src: File, listFile: List[File]) = {
    val reader = new PdfReader(src.getAbsolutePath)
    val listener = new MyParser(listFile)
    val processor = new PdfContentStreamProcessor(listener)
    (1 to reader.getNumberOfPages)
      .map(pageNumber => (reader.getPageN(pageNumber).getAsDict(PdfName.RESOURCES), pageNumber))
      .foreach {
      case (pdfDic, pageNumber) =>
        listener.newPage(pageNumber)
        processor.processContent(ContentByteUtils.getContentBytesForPage(reader, pageNumber), pdfDic)
    }
    listener.getResult
  }

  def addAttachments(src: File, result: File, list: List[(Int, Rectangle, File, String)]) {
    val readerOriginalDocument = new PdfReader(src.getAbsolutePath)
    val stamper = new PdfStamper(readerOriginalDocument, new FileOutputStream(result))

    list.map {
      case (page: Int, rec: Rectangle, fileToAttach: File, description: String) =>
        val annotation = PdfAnnotation.createFileAttachment(stamper.getWriter, rec, description, null, fileToAttach.getAbsolutePath, fileToAttach.getName)
        annotation.put(PdfName.NAME, new PdfString(icon))
        (page, annotation)
    }
      .foreach {
      case (page, annotation) => stamper.addAnnotation(annotation, page)
    }
    stamper.close()
  }

  def parseArgs(args: List[String], argsParser: PartialFunction[List[String], List[String]]): List[String] = args match {
    case Nil => Nil
    case _ => if (argsParser isDefinedAt args) parseArgs(argsParser(args), argsParser) else args.head :: parseArgs(args.tail, argsParser)
  }

  def die(msg: String = usage, displayError: Boolean = true) = {
    println(if (displayError)
      s"""
        |ERROR
        |=====
        |$msg
      """.stripMargin
    else msg)
    sys.exit(1)
  }

  class MyParser(val searched: List[File]) extends RenderListener {
    private val mListPositions = new ArrayBuffer[(LineSegment, Int, File)]
    private val mPageContent = new StringBuilder
    private val mListFileName = searched.map(_.getName.toLowerCase).toList
    private var mCurrentPage = -1

    def newPage(page: Int) = {
      mPageContent.clear()
      mCurrentPage = page
    }

    override def renderText(renderInfo: TextRenderInfo) {
      mPageContent.append(renderInfo.getText.toLowerCase)
      mListFileName
        .filter(fileName => mPageContent.toString().contains(fileName))
        .foreach {fileName =>
        val f = searched.find(_.getName.toLowerCase == fileName).get
          mListPositions += Tuple3(renderInfo.getDescentLine, mCurrentPage, f)
          mPageContent.clear() //avoid finding several time the same item
      }
    }

    //not used
    override def beginTextBlock(): Unit = {}

    //not used
    override def endTextBlock(): Unit = {}

    //not used
    override def renderImage(renderInfo: ImageRenderInfo): Unit = {}

    def getResult = mListPositions.toList

    override def toString = mPageContent.toString()
  }
}