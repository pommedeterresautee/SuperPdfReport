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

object main {

  val argTests = List("--attachments", "C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\attachments",
    "--original-pdf", "C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\test4.pdf", "--save-as",
    "C:\\Users\\MBenesty\\Private\\GIT\\SuperPdfReport\\test\\result.pdf", "--verbose", "--description", "une description un peu au hasard")

  def main(args: Array[String]) {

    val listArgt = argTests.toList
    val parser = ArgtParser(listArgt)


    if (parser.finalPDF.get.exists()) {
      parser.finalPDF.get.delete()
      if (parser.verbose) println(s"Existing file deleted:\n ${parser.finalPDF.get.getAbsolutePath}")
    }

    PDFParser.process(parser)
  }
}