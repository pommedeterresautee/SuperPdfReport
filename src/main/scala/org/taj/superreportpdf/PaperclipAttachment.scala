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

package org.taj.superreportpdf

import java.io.{FileOutputStream, File}
import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.parser._
import com.itextpdf.text._
import scala.List


object PaperclipAttachment {
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
    addAttachments(parser.originalPDF.get, parser.finalPDF.get, result, parser.attachmentIcon)
  }

  private def searchText(src: File, listFile: List[File]) = {
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

  private def addAttachments(src: File, result: File, list: List[(Int, Rectangle, File, String)], customIcon:Option[File]) {
    val readerOriginalDocument = new PdfReader(src.getAbsolutePath)
    val stamper = new PdfStamper(readerOriginalDocument, new FileOutputStream(result))

    list.map {
      case (page: Int, rec: Rectangle, fileToAttach: File, description: String) =>
        val annotation = PdfAnnotation.createFileAttachment(stamper.getWriter, rec, description, null, fileToAttach.getAbsolutePath, fileToAttach.getName)
        annotation.put(PdfName.NAME, new PdfString(icon))
        customIcon.foreach{f =>
          val image = Image.getInstance(f.getAbsolutePath)
          val app = stamper.getOverContent(page).createAppearance(image.getWidth, image.getHeight)
          annotation.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, app)
          annotation.setAppearance(PdfAnnotation.APPEARANCE_DOWN, app)
          annotation.setAppearance(PdfAnnotation.APPEARANCE_ROLLOVER, app)
          val chunk = new Chunk(image,50,50)
          chunk.setAnnotation(annotation)
          val phrase = new Phrase(chunk)
          val cell = new PdfPCell(phrase)
          cell.setHorizontalAlignment(Element.ALIGN_CENTER)
          cell.setVerticalAlignment(Element.ALIGN_CENTER)
        }
        
        (page, annotation)
    }
      .foreach {
      case (page, annotation) => stamper.addAnnotation(annotation, page)
    }
    stamper.close()
  }
}