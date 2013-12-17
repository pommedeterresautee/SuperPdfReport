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

import java.io.File

object AttachmentMode extends Enumeration {
  type Mode = Value
  val paperclip, portfolio = Value
}

class ArgtParser {

  val usage =
  """
      |Usage: superpdfreport --attachments C:\path\folder\ --original-pdf C:\path\original.pdf [--verbose] [--description anyText] [--paperclip-attachment] [--portfolio] --save-as C:\path\destination.pdf
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

    |Super PDF Report has been written by the TMC Paris office of the law firm TAJ and is under MIT licence.
""".stripMargin

  var verbose = false
  var attachmentFolder: Option[File] = None
  var originalPDF: Option[File] = None
  var finalPDF: Option[File] = None
  var descriptionPDF: Option[String] = None
  var attachmentMode: Option[AttachmentMode.Mode]= Option(AttachmentMode.paperclip)

  val ArgParser: PartialFunction[List[String], List[String]] = {
    case "--verbose" :: tail => verbose = true; tail
    case "--paperclip-attachment" :: tail => attachmentMode = Option(AttachmentMode.paperclip); tail
    case "--portfolio" :: tail => attachmentMode = Option(AttachmentMode.portfolio); tail
    case "--description" :: (arg: String) :: tail => descriptionPDF = Option(arg); tail
    case "--attachments" :: (arg: String) :: tail => attachmentFolder = Option(new File(arg)); tail
    case "--original-pdf" :: (arg: String) :: tail => originalPDF = Option(new File(arg)); tail
    case "--save-as" :: (arg: String) :: tail => finalPDF = Option(new File(arg)); tail
    case unknown(bad) :: tail => die("unknown argument " + bad + "\n" + usage)
  }

  val unknown = "(^-[^\\s])".r

  def get(listArgt:List[String]) {

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
  }

//  @inline
  private def parseArgs(args: List[String], argsParser: PartialFunction[List[String], List[String]]): List[String] = args match {
    case Nil => Nil
    case _ => if (argsParser isDefinedAt args) parseArgs(argsParser(args), argsParser) else args.head :: parseArgs(args.tail, argsParser)
  }


  private def die(msg: String = usage, displayError: Boolean = true) = {
    println(if (displayError)
      s"""
        |ERROR
        |=====
        |$msg
      """.stripMargin
    else msg)
    sys.exit(1)
  }
}

object ArgtParser {
  def apply(listArgt:List[String])= {
    val t = new ArgtParser()
    t.get(listArgt)
    t
  }
}
