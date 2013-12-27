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

import com.itextpdf.text.pdf._
import java.io.{File, FileOutputStream}
import java.util.Calendar
import java.nio.file.attribute.{FileTime, BasicFileAttributes}
import java.nio.file.Files


object PortfolioAttachment {

  def process(parser:ArgtParser) {
    val reader = new PdfReader(parser.originalPDF.get.getAbsolutePath)
    val stamper = new PdfStamper(reader, new FileOutputStream(parser.finalPDF.get))
    if(parser.verbose) println(s"Main PDF file:  ${parser.originalPDF.get.getAbsolutePath}")
    parser
      .attachmentFolder
      .get
      .listFiles
      .toList
      .foreach(addAttachment(stamper, _, parser.descriptionPDF.get, parser.verbose))
    stamper.close()
  }

  def addAttachment(stamper:PdfStamper, fileToAttach:File, description:String, verbose:Boolean) {
    val pdfDictionary = new PdfDictionary()
    val lastModifDate = Calendar.getInstance()
    val attributes:BasicFileAttributes  =
      Files.readAttributes(fileToAttach.toPath, classOf[BasicFileAttributes])
    lastModifDate.setTimeInMillis(fileToAttach.lastModified())
    val creationDate = Calendar.getInstance()
    creationDate.setTimeInMillis(attributes.creationTime().toMillis)
    pdfDictionary.put(PdfName.NAME, new PdfString(fileToAttach.getName))
    pdfDictionary.put(PdfName.CREATIONDATE, new PdfDate(creationDate))
    pdfDictionary.put(PdfName.MODDATE, new PdfDate(lastModifDate))
    val fs = PdfFileSpecification.fileEmbedded(stamper.getWriter, fileToAttach.getAbsolutePath, fileToAttach.getName, null, true, null, pdfDictionary)
    stamper.addFileAttachment(description, fs)
    if(verbose) println(s"Attached: ${fileToAttach.getAbsolutePath}")
  }
}
