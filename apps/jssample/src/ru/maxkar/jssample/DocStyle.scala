package ru.maxkar.jssample


/** Documentation style. */
private[jssample] object DocStyle {
  val STYLE = """
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
  """.stripMargin('|')
}
