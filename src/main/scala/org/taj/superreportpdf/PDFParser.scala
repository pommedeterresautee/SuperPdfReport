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

package scala.org.taj.superreportpdf

import java.io.{FileOutputStream, File}
import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.parser._
import com.itextpdf.text.Rectangle
import scala.Tuple3
import scala.collection.mutable.ArrayBuffer
import scala.Tuple3
import scala.Tuple3
import org.taj.superreportpdf.ArgtParser


object PDFParser {
  val icon = "Paperclip"

  def process(parser:ArgtParser) {
    val result = searchText(parser.originalPDF.get, parser.attachmentFolder.get.listFiles().toList)
      .map {
      case (line, page, attachment) =>
        val x = line.getEndPoint.get(0)
        val y = line.getEndPoint.get(1)
        if (parser.verbose) println(s"Add attachment page $page:\nExpression searched=${attachment.getName}\nX=$x\nY=$y\n")
        val rect = new Rectangle(x + 3, y, x + 10, y + 17)
        (page, rect, attachment, parser.descriptionPDF.getOrElse(attachment.getName))
    }
    addAttachments(parser.originalPDF.get, parser.finalPDF.get, result)
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
