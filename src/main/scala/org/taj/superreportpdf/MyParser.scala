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
import com.itextpdf.text.pdf.parser.{ImageRenderInfo, TextRenderInfo, LineSegment, RenderListener}
import scala.collection.mutable.ArrayBuffer


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