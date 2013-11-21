package ru.maxkar.jssample.doc

import java.io._
import com.coverity.security.Escape._

/** Documentation generator. */
object DocGen {

  /** Writes a meta-documentation. */
  def writeMeta(moddoc : Seq[String], base : File) : Unit = {
    writeDocCsss(base)

    val fs = new FileOutputStream(new File(base, "index.html"))
    try {
      val pn = new OutputStreamWriter(new BufferedOutputStream(fs), "UTF-8")
        pn.write("<!DOCTYPE html><html><head><meta charset=\"utf-8\">")
        pn.write("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"><title>JSLPG documentation")
        pn.write("</title></head><body class=\"framedoc\">")
        pn.write("<div class=\"pkglist\"><h1>Packages</h1>")
        for (x ← moddoc) {
          pn.write("<div><a target=\"moditem\" href=\"_")
          pn.write(html(x))
          pn.write(".html\">")
          pn.write(html(x))
          pn.write("</a></div>")
        }
        pn.write("</div>")
        pn.write("<div class=\"modframe\"><iframe name=\"moditem\" seamless=\"true\"/></div>")
        pn.write("</body></html>")
      try {
      } finally {
        pn.close
      }
    } finally {
      fs.close
    }
  }


  /** Writes a documentation css. */
  private def writeDocCsss(base : File) : Unit = {
    val fs = new FileOutputStream(new File(base, "style.css"))
    try {
      fs.write(STYLE.getBytes("UTF-8"))
    } finally {
      fs.close
    }
  }

  /** Writes a single documentation file. */
  def writeDoc(
      base : File,
      id : Seq[String],
      mod : DocBody,
      vars : Seq[VarDoc],
      funs : Seq[FunDoc]) : String = {

    val modName = id.mkString(".")

    val fs = new FileOutputStream(new File(base, "_" + modName + ".html"))
    try {
      val pn = new OutputStreamWriter(new BufferedOutputStream(fs), "UTF-8")
      try {
        pn.write("<!DOCTYPE html><html><head><meta charset=\"utf-8\">")
        pn.write("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"><title>Module ")
        pn.write(htmlText(modName))
        pn.write("</title></head><body>")
        pn.write("<header class=\"modname\"><h1>Module ")
        pn.write(htmlText(modName))
        pn.write("</h1></header>")

        pn.write("<div class=\"docarea\">")
        mod.writeTo(pn)
        pn.write("</div>")

        if (!vars.isEmpty) {
          pn.write("<div class=\"sect\"><header class=\"sectheader\"><h2>Variables</h2></header>")
          pn.write("<table><thead><tr><td>Access</td><td>Name</td><td>Exported</td><td class=\"descr\">Description</td></tr></thead>")

          vars.foreach(docVar(_, pn))

          pn.write("</table></div>")
        }


        if (!funs.isEmpty) {
          pn.write("<div class=\"sect\"><header class=\"sectheader\"><h2>Functions</h2></header>")
          pn.write("<table><thead><tr><td>Access</td><td>Name</td><td>Exported</td><td class=\"descr\">Description</td></tr></thead>")

          funs.foreach(docFun(_, pn))

          pn.write("</table></div>")
        }

        pn.write("</body></html>")
      } finally {
        pn.close
      }
    } finally {
      fs.close
    }

    modName
  }


  /** Documents a function. */
  private def docFun(v : FunDoc, w : Writer) : Unit = {
    w.write("<tr><td>")
    w.write(if (v.isPublic) "public" else "private")
    w.write("</td><td>")
    w.write(htmlText(v.name))
    w.write("</td><td>")
    v.globalName match {
      case None ⇒  ()
      case Some(x) ⇒ w.write(htmlText(x))
    }
    w.write("</td><td>")
    DocBody.write(v.contract, w)

    if (!v.args.isEmpty) {
      w.write("<div class=\"fnsection\">Arguments:</div><div class=\"fndata\"><table class=\"alist\">")
      w.write("<thead><tr><td>Name</td><td class=\"descr\">Description</td></tr></thead>")


      val aiter = v.args.iterator

      while (aiter.hasNext) {
        val a = aiter.next
        w.write("<tr><td><div>")
        if (v.isVararg && !aiter.hasNext)
          w.write("<span class=\"modifier\">vararg</span>&nbsp;")
        w.write(htmlText(a.name))
        w.write("</td><td>")
        DocBody.write(a.doc, w)
        w.write("</td></tr>")
      }
      w.write("</table></div>")
    }


    w.write("<div class=\"fnsection\">Returns:</div><div class=\"fndata\">")
    DocBody.write(v.ret, w)
    w.write("</div>")
    w.write("</td></tr>")
  }


  /** Documents a variable. */
  private def docVar(v : VarDoc, w : Writer) : Unit = {
    w.write("<tr><td>")
    w.write(if (v.isPublic) "public" else "private")
    w.write("</td><td>")
    w.write(htmlText(v.name))
    w.write("</td><td>")
    v.globalName match {
      case None ⇒  ()
      case Some(x) ⇒ w.write(htmlText(x))
    }
    w.write("</td><td>")
    DocBody.write(v.doc, w)
    w.write("</td></tr>")
  }



  /** Documentation style. */
  private val STYLE = """
    |html {
    |  height: 100%;
    |}
    |body {
    |  margin: 0px;
    |}
    |header.modname {
    |  background-color: green;
    |  padding-left: 3px;
    |  padding-right: 3px;
    |}
    |div {
    |  padding-left: 5px;
    |  padding-right: 5px;
    |}
    |.sect {
    |  border: 1px solid black;
    |  margin: 0px 10px;
    |  margin-top: 15px;
    |}
    |.sectheader {
    |  margin:0px;
    |  background-color: #E0E0E0;
    |  margin-top: 3px;
    |}
    |.sectheader h2 {
    |  margin:0px;
    |}
    |thead {
    |  background-color:cyan;
    |}
    |thead tr td {
    |  padding: 3px 10px 3px 5px;
    |}
    |td.descr {
    | width : 100%;
    |}
    |tbody tr td {
    |  padding: 3px;
    |}
    |tbody tr {
    |  border-top: 1px solid gray;
    |}
    |table {
    |  border-collapse: collapse;
    |  width: 100%;
    |}
    |body.framedoc {
    |  height: 100%;
    |  width: 100%;
    |  display: flex;
    |}
    |div.pkglist {
    |  background-color: khaki;
    |  min-height: 100%;
    |}
    |div.modframe {
    |  flex: 1 1 auto;
    |  display: flex;
    |}
    |iframe {
    |  border: 0px;
    |  width: 100%;
    |  height: auto;
    |}
    |.fnsection {
    |  font-weight:bold;
    |  margin-top: 7px;
    |  padding-left: 0px;
    |}
    |.fndata {
    |  width : 100;
    |  padding-left: 15px;
    |}
    |table.alist {
    |  border: 1px solid gray;
    |  margin-top: 4px;
    |}
    |.modifier {
    |  color: gray;
    |  font-style: italic;
    |}
  """.stripMargin('|')
}
