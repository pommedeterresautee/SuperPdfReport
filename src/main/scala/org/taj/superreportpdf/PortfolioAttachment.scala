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

import com.itextpdf.text.Document
import com.itextpdf.text.Paragraph
import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.collection._
import java.io.{File, ByteArrayOutputStream, FileOutputStream}

case class Attachment(fileToAttach: File, description: String)

object PortfolioTemp {
  private val DESCRIPTION = "Description"
  private val DESCRIPTION_FIELD = "DESCRIPTION"
  private val FILENAME = "File name"
  private val FILENAME_FIELD = "FILENAME"
  private val DATE = "Last file update"
  private val DATE_FIELD = "DATE"
  private val SIZE = "Size"
  private val SIZE_FIELD = "SIZE"

  private def getCollectionSchema: PdfCollectionSchema = {
    val schema = new PdfCollectionSchema()
    Seq((FILENAME_FIELD, FILENAME, PdfCollectionField.FILENAME),
      (DESCRIPTION_FIELD, DESCRIPTION, PdfCollectionField.TEXT),
      (SIZE_FIELD, SIZE, PdfCollectionField.SIZE),
      (DATE_FIELD, DATE, PdfCollectionField.MODDATE)
    ).foreach {
      case (columnName: String, columnDisplayedName: String, varType: Int) =>
        schema.addField(columnName, new PdfCollectionField(columnDisplayedName, varType))
    }
    schema
  }

  private def getMetadata(schema: PdfCollectionSchema, fileToAttach: File, description: String): PdfCollectionItem = {
    val item: PdfCollectionItem = new PdfCollectionItem(schema)
    item.addItem(DESCRIPTION_FIELD, description)
    item
  }

  private def createPdf(toAttach: Seq[Attachment], coverPageContentText: String, verbose: Boolean): Array[Byte] = {
    val document: Document = new Document

    val finalDocumentBytes: ByteArrayOutputStream = new ByteArrayOutputStream
    val writer: PdfWriter = PdfWriter.getInstance(document, finalDocumentBytes)

    document.open()
    document.add(new Paragraph(coverPageContentText))
    val schema: PdfCollectionSchema = getCollectionSchema
    val collection: PdfCollection = new PdfCollection(PdfCollection.DETAILS)
    collection.setSchema(schema)
    val sort: PdfCollectionSort = new PdfCollectionSort(DATE_FIELD)
    sort.setSortOrder(true)
    collection.setSort(sort)
    writer.setCollection(collection)

    toAttach.foreach {
      attachments =>
        val fs: PdfFileSpecification = PdfFileSpecification.fileEmbedded(writer, attachments.fileToAttach.getAbsolutePath, attachments.fileToAttach.getName, null)
        fs.addCollectionItem(getMetadata(schema, attachments.fileToAttach, attachments.description))
        writer.addFileAttachment(fs)
        if (verbose) println(s"Attached ${attachments.fileToAttach.getAbsolutePath}")
    }
    document.close()
    finalDocumentBytes.toByteArray
  }

  def process(parser: ArgtParser) {
    if (parser.verbose) println(s"Main PDF file:  ${parser.originalPDF.get.getAbsolutePath}")

    val filesToAttach = parser
      .attachmentFolder
      .get
      .listFiles
      .map(Attachment(_, "Some description here"))

    val output = parser.finalPDF.get
    val os = new FileOutputStream(output)
    os.write(createPdf(filesToAttach, "This is a cover page.\n\nTAJ", parser.verbose))
    os.flush()
    os.close()
  }
}