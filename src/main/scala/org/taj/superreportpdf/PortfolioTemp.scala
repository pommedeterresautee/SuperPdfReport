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

case class Attachments(fileToAttach: File, description: String)

object PortfolioTemp {
  private val DESCRIPTION = "Description"
  private val DESCRIPTION_FIELD = "DESCRIPTION"
  private val FILENAME = "File name"
  private val FILENAME_FIELD = "FILENAME"
  private val DATE = "Last file update"
  private val DATE_FIELD = "DATE"

  private def getCollectionSchema: PdfCollectionSchema = {
    val schema = new PdfCollectionSchema()
    val title = new PdfCollectionField(DESCRIPTION, PdfCollectionField.TEXT)
    title.setOrder(1)
    schema.addField(DESCRIPTION_FIELD, title)
    val filename = new PdfCollectionField(FILENAME, PdfCollectionField.FILENAME)
    filename.setOrder(0)
    schema.addField(FILENAME_FIELD, filename)
    val date = new PdfCollectionField(DATE, PdfCollectionField.MODDATE)
    date.setOrder(3)
    schema.addField(DATE_FIELD, date)
    schema
  }

  private def getMetadata(schema: PdfCollectionSchema, fileToAttach: File, description: String): PdfCollectionItem = {
    val item: PdfCollectionItem = new PdfCollectionItem(schema)
    item.addItem(DESCRIPTION_FIELD, description)
    item
  }

  private def createPdf(toAttach: Seq[Attachments], coverPageContentText: String): Array[Byte] = {
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
    }
    document.close()
    finalDocumentBytes.toByteArray
  }

  def main() {
    val output = new File("C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\lenezresult.pdf")

    val attachment1 = new File("C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\attachments\\Fichier.pdf")
    val attachment2 = new File("C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\attachments\\Fichier.xlsx")
    val attachment3 = new File("C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\attachments\\Fichier.txt")
    val attachment4 = new File("C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\attachments\\paperclip.jpg")

    val files = Seq(Attachments(attachment1, "ceci est une description qui est plus longue que d'habitude tralala"),
      Attachments(attachment2, "ceci est une description2 qui est plus longue que d'habitude tralala"),
      Attachments(attachment3, "ceci est une description3 qui est plus longue que d'habitude tralala"),
      Attachments(attachment4, "ceci est une description4 qui est plus longue que d'habitude tralala")
    )

    val os: FileOutputStream = new FileOutputStream(output)
    os.write(createPdf(files, "This is a cover page.\n\n\nTAJ"))
    os.flush()
    os.close()
  }
}
