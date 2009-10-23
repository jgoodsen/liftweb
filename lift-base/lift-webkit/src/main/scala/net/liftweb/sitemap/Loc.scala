/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.sitemap

import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import auth._

import _root_.scala.xml.{NodeSeq, Text}

/**
 * A menu location
 */
trait Loc[T] {
  type LocRewrite =  Box[PartialFunction[RewriteRequest, (RewriteResponse, T)]]

  def name: String

  def link: Loc.Link[T]

  def text: Loc.LinkText[T]

  def params: List[Loc.LocParam]

  def overrideValue: Box[T] = Empty

  object requestValue extends RequestVar[Box[T]](Empty) {
    override val __nameSalt = randomString(10)
  }

  def defaultValue: Box[T]

  def childValues: List[T] = Nil

  def rewrite: LocRewrite = Empty

  def placeHolder_? : Boolean = _placeHolder_?

  private lazy val _placeHolder_? = allParams.contains(Loc.PlaceHolder)

  def hideIfNoKids_? = placeHolder_? || _hideIfNoKids_?

  private lazy val _hideIfNoKids_? = allParams.contains(Loc.HideIfNoKids)

  def allParams: List[Loc.LocParam] = params ::: siteMap.globalParams

  def siteMap: SiteMap = _menu.siteMap

  def createDefaultLink: Option[NodeSeq] = (requestValue.is or defaultValue).flatMap(p => link.createLink(p)).toOption

  def createLink(in: T): Option[NodeSeq] = link.createLink(in).toOption

  override def toString = "Loc("+name+", "+link+", "+text+", "+params+")"

  def rewritePF: Box[LiftRules.RewritePF] = rewrite.map(
    rw =>
    new NamedPartialFunction[RewriteRequest, RewriteResponse] {
      def functionName = rw match {
        case rw: NamedPartialFunction[_, _] =>
          // ugly code to avoid type erasure warning
          rw.asInstanceOf[NamedPartialFunction[RewriteRequest, RewriteResponse]].functionName
        case _ => "Unnamed"
      }

      def isDefinedAt(in: RewriteRequest) = rw.isDefinedAt(in)

      def apply(in: RewriteRequest): RewriteResponse = {
        val (ret, param) = rw.apply(in)
        requestValue.set(Full(param))
        ret
      }
    }
  )

  type SnippetTest = PartialFunction[(String, Box[T]), NodeSeq => NodeSeq]

  def snippets: SnippetTest = Map.empty

  lazy val calcSnippets: SnippetTest = {
    def buildPF(in: Loc.Snippet): PartialFunction[String, NodeSeq => NodeSeq] = {
      new PartialFunction[String, NodeSeq => NodeSeq] {
        def isDefinedAt(s: String) = s == in.name
        def apply(s: String): NodeSeq => NodeSeq = {
          if (isDefinedAt(s)) in.func
          else throw new MatchError()
        }
      }
    }

    val singles = {
      allParams.flatMap{ case v: Loc.Snippet => Some(v);     case _ => None }.toList.map(buildPF) ::: 
      allParams.flatMap{ case v: Loc.LocSnippets => Some(v); case _ => None }.toList
    }

    if (singles.isEmpty) Map.empty
    else {
      val func: PartialFunction[String, NodeSeq => NodeSeq] = singles match {
        case pf :: Nil => pf
        case pfs => pfs.reduceLeft[PartialFunction[String, NodeSeq => NodeSeq]](_ orElse _)
      }

      new SnippetTest {
        def isDefinedAt(in: (String, Box[T])): Boolean = func.isDefinedAt(in._1)
        def apply(in: (String, Box[T])): NodeSeq => NodeSeq = func.apply(in._1)
      }
    }
  }

  def snippet(name: String): Box[NodeSeq => NodeSeq] = {
    val test = (name, requestValue.is)

    if ((snippets orElse calcSnippets).isDefinedAt(test)) Full((snippets orElse calcSnippets)(test))
    else Empty
  }

  def testAccess: Either[Boolean, Box[() => LiftResponse]] = accessTestRes.is

  protected def _testAccess: Either[Boolean, Box[() => LiftResponse]] = {
    def testParams(what: List[Loc.LocParam]): Either[Boolean, Box[() => LiftResponse]] = what match {
      case Nil => Left(true)

      case Loc.If(test, msg) :: xs =>
        if (!test()) Right(Full(msg))
        else testParams(xs)

      case Loc.Unless(test, msg) :: xs =>
        if (test()) Right(Full(msg))
        else testParams(xs)

      case Loc.TestAccess(func) :: xs =>
        func() match {
          case Full(resp) => Right(Full(() => resp))
          case _ => testParams(xs)
        }

      case x :: xs => testParams(xs)
    }

    testParams(allParams) match {
      case Left(true) => _menu.testParentAccess
      case x => x
    }
  }

  protected object accessTestRes extends RequestVar[Either[Boolean, Box[() => LiftResponse]]](_testAccess) {
    override val __nameSalt = randomString(10)
  }

  def earlyResponse: Box[LiftResponse] = {
    def early(what: List[Loc.LocParam]): Box[LiftResponse] = what match {
      case Nil => Empty

      case Loc.EarlyResponse(func) :: xs =>
        func() match {
          case Full(r) => Full(r)
          case _ => early(xs)
        }

      case x :: xs => early(xs)
    }

    early(allParams)
  }

  /**
   * Is there a template assocaited with this Loc?
   */
  def template: Box[NodeSeq] = paramTemplate.map(_.template()) or calcTemplate

  /**
   * A method that can be override to provide a template for this Loc
   */
  def calcTemplate: Box[NodeSeq] = Empty

  /**
   * Look for the Loc.Template in the param list
   */
  lazy val paramTemplate: Box[Loc.Template] = {
    allParams.flatMap{case v: Loc.Template => Some(v) case _ => None}.firstOption
  }

  private def findTitle(lst: List[Loc.LocParam]): Box[Loc.Title[T]] = lst match {
    case Nil => Empty
    case (t: Loc.Title[_]) :: xs =>
      // ugly code to avoid type erasure warning
      Full(t.asInstanceOf[Loc.Title[T]])
    case _ => findTitle(lst.tail)
  }

  /**
   * The title of the location
   */
  def title: NodeSeq = ((overrideValue or requestValue.is or defaultValue).map(p => title(p)) or linkText) openOr Text(name)

  def title(in: T): NodeSeq = findTitle(params).map(_.title(in)) openOr linkText(in)

  def linkText: Box[NodeSeq] = (overrideValue or requestValue.is or defaultValue).map(p => linkText(p))

  def linkText(in: T): NodeSeq = text.text(in)

  private[sitemap] def setMenu(p: Menu) {
    _menu = p
    p.siteMap.addLoc(this)
  }

  private var _menu: Menu = _

  def menu = _menu

  private def testAllParams(what: List[Loc.LocParam], req: Req): Boolean = {
    what match {
      case Nil => true
      case (x: Loc.Test) :: xs =>
        if (!x.test(req)) false
        else testAllParams(xs, req)

      case x :: xs => testAllParams(xs, req)
    }
  }

  def doesMatch_?(req: Req): Boolean = {
    if (link.isDefinedAt( req ) ) {
      link(req) match {
        case Full(x) if testAllParams(allParams, req) => x
        case Full(x) => false
        case x => x.openOr(false)
      }
    } else false
  }

  def breadCrumbs: List[Loc[_]] = _menu.breadCrumbs ::: List(this)

  def buildKidMenuItems(kids: Seq[Menu]): List[MenuItem] = {
    kids.toList.flatMap(_.loc.buildItem(Nil, false, false)) ::: supplimentalKidMenuItems
  }

  def supplimentalKidMenuItems: List[MenuItem] = 
    for {p <- childValues 
         l <- link.createLink(p)} 
    yield MenuItem(
      text.text(p), 
      l, Nil, false, false,  
      allParams.flatMap {
        case v: Loc.LocInfo[_] => v()
        case _ =>  Nil
      }
    )
  

  def buildMenu: CompleteMenu = {
    CompleteMenu(_menu.buildUpperLines(_menu, _menu, buildKidMenuItems(_menu.kids)))
  }

  private[liftweb] def buildItem(kids: List[MenuItem], current: Boolean, path: Boolean): Box[MenuItem] = 
    (calcHidden(kids), testAccess) match {
      case (false, Left(true)) => {
          for {p <- (overrideValue or requestValue.is or defaultValue)
               t <- link.createLink(p)}
          yield new MenuItem(
            text.text(p),
            t, kids, current, path,
            allParams.flatMap {
              case v: Loc.LocInfo[_] => v()
              case _ =>  Nil
            }, 
            placeHolder_?
          )
        }

      case _ => Empty
    }

  protected def calcHidden(kids: List[MenuItem]) = hidden || (hideIfNoKids_? && kids.isEmpty)

  def hidden = _hidden

  private lazy val _hidden = allParams.contains(Loc.Hidden)

  private lazy val groupSet: Set[String] =
  Set(allParams.flatMap{case s: Loc.LocGroup => s.group case _ => Nil} :_*)

  def inGroup_?(group: String): Boolean = groupSet.contains(group)

  /**
   * A title for the page.  A function that calculates the title... useful
   * if the title of the page is dependent on current state
   */
  case class Title(title: T => NodeSeq) extends LocParam

  trait LocInfo extends LocParam {
    def apply(): Box[T]
  }
}


/**
 * The Loc companion object, complete with a nice constructor
 */
object Loc {
  type FailMsg = () => LiftResponse

  /**
   * Create a Loc (Location) instance
   *
   * @param name -- the name of the location.  This must be unique across your entire sitemap.
   * It's used to look up a menu item in order to create a link to the menu on a page.
   * @param link -- the Link to the page
   * @param text -- the text to display when the link is displayed
   * @param params -- access test, title calculation, etc.
   */
  def apply(theName: String,
            theLink: Link[Unit],
            theText: LinkText[Unit],
            theParams: LocParam*): Loc[Unit] =
  new Loc[Unit] {
    val name = theName
    val link: Loc.Link[Unit] = theLink

    val text: Loc.LinkText[Unit] = theText
    val defaultValue: Box[Unit] = Full(Unit)

    val params: List[LocParam] = theParams.toList

    checkProtected(link, params)
  }

  def apply(theName: String, theLink: Loc.Link[Unit],
            theText: LinkText[Unit],
            theParams: List[LocParam]): Loc[Unit] =
  new Loc[Unit] {
    val name = theName
    val link: Loc.Link[Unit] = theLink
    val defaultValue: Box[Unit] = Full(Unit)

    val text: Loc.LinkText[Unit] = theText

    val params: List[LocParam]  = theParams.toList

    checkProtected(link, params)
  }

  def checkProtected(link: Link[_], params: List[LocParam]) {
    params.map {
      case Loc.HttpAuthProtected(role) => LiftRules.httpAuthProtectedResource.append(
          new LiftRules.HttpAuthProtectedResourcePF() {
            def isDefinedAt(in: ParsePath) = in.partPath == link.uriList
            def apply(in: ParsePath): Box[Role] = role()
          })

      case x => x
    }
  }

  /**
   * Algebraic data type for parameters that modify handling of a Loc
   * in a SiteMap
   */ 
  sealed trait LocParam

  /**
   * Extension point for user-defined LocParam instances.
   */ 
  trait UserLocParam extends LocParam

  /**
   * If this parameter is included, the item will not be visible in the menu, but
   * will still be accessable.
   */
  case object Hidden extends LocParam

  /**
   * Indicates that the path denominated by Loc requires HTTP authentication
   * and only a user assigned to this role or to a role that is child-of this role
   * can access it.
   */
  case class HttpAuthProtected(role: () => Box[Role]) extends LocParam

  /**
   * If the Loc is in a group (or groups) like "legal" "community" etc.
   * the groups can be specified and recalled at the top level
   */
  case class LocGroup(group: String*) extends LocParam

  /**
   * If the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class If(test: () => Boolean, failMsg: FailMsg) extends LocParam

  /**
   * Unless the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class Unless(test: () => Boolean, failMsg: FailMsg) extends LocParam

  /**
   * Allows extra access testing for a given menu location such that
   * you can generically return a response during access control
   * testing
   */
  case class TestAccess(func: () => Box[LiftResponse]) extends LocParam

  /**
   * Allows you to generate an early response for the location rather than
   * going through the whole Lift XHTML rendering pipeline
   */
  case class EarlyResponse(func: () => Box[LiftResponse]) extends LocParam

  /**
   * Tests to see if the request actually matches the requirements for access to
   * the page.  For example, if a parameter is missing from the request, this
   * is a good way to restrict access to the page.
   */
  case class Test(test: Req => Boolean) extends LocParam

  /**
   * A single snippet that's assocaited with a given location... the snippet
   * name and the snippet function'
   */
  case class Snippet(name: String, func: NodeSeq => NodeSeq) extends LocParam

  case class Template(template: () => NodeSeq) extends LocParam

  /**
   * Allows you to create a handler for many snippets that are associated with
   * a Loc
   */
  trait LocSnippets extends PartialFunction[String, NodeSeq => NodeSeq] with LocParam

  /**
   * If the Loc has no children, hide the Loc itself
   */
  object HideIfNoKids extends LocParam

  /**
   * The Loc does not represent a menu itself, but is the parent menu for
   * children (implies HideIfNoKids)
   */
  object PlaceHolder extends LocParam
  
  /**
   * A subclass of LocSnippets with a built in dispatch method (no need to
   * implement isDefinedAt or apply... just
   * def dispatch: PartialFunction[String, NodeSeq => NodeSeq]
   */
  trait DispatchLocSnippets extends LocSnippets {
    def dispatch: PartialFunction[String, NodeSeq => NodeSeq]

    def isDefinedAt(n: String) = dispatch.isDefinedAt(n)

    def apply(n: String) = dispatch.apply(n)
  }

  /**
   * What's the link text.
   */
  case class LinkText[T](text: T => NodeSeq)

  /**
   * This defines the Link to the Loc.
   *
   * @param uri -- the relative (to parent menu item) or absolute path
   * to match for this Loc. <br />
   * "/foo" -- match the "foo" file <br/>
   * "foo" -- match the foo file in the directory defined by the parent Menu
   * @param matchOnPrefix -- false -- absolute match.  true -- match anything
   * that begins with the same path.  Useful for opening a set of directories
   * (for example, help pages)
   * @param create -- create a URL based on incoming parameters
   */
  class Link[T](val uriList: List[String], val matchHead_? : Boolean) extends PartialFunction[Req, Box[Boolean]] {
    def this(b: List[String]) = this(b, false)

    def isDefinedAt(req: Req): Boolean = {
      if (matchHead_?) req.path.partPath.take(uriList.length) == uriList
      else uriList == req.path.partPath
    }

    def apply(in: Req): Box[Boolean] = {
      if (isDefinedAt(in)) Full(true)
      else throw new MatchError("Failed for Link "+uriList)
    }

    def createLink(params: T): Box[NodeSeq] = {
      if (matchHead_?)
        Full(Text((uriList).mkString("/", "/", "") + "/"))
      else if (uriList.last == "index" && uriList.length > 1)
        Full(Text(uriList.dropRight(1).mkString("/", "/", "")+"/"))
      else Full(Text(uriList.mkString("/", "/", "")))
    }
  }

  /**
   * A companion object to create some variants on Link
   */
  object Link {
    def apply(urlLst: List[String], matchHead_? : Boolean, url: String) = {
      new Link[Unit](urlLst, matchHead_?) {
        override def createLink(params: Unit): Box[NodeSeq] = Full(Text(url))
      }
    }
  }

  object ExtLink {
    def apply(url: String) = new Link[Unit](Nil, false) {
      override def createLink(params: Unit): Box[NodeSeq] =
      Full(Text(url))
    }
  }

  def alwaysTrue(a: Req) = true
  def retString(toRet: String)(other: Seq[(String, String)]) = Full(toRet)

  implicit def nodeSeqToLinkText[T](in: => NodeSeq): LinkText[T] = LinkText[T](T => in)
  implicit def strToLinkText[T](in: => String): LinkText[T] = LinkText(T => Text(in))

  implicit def strLstToLink(in: Seq[String]): Link[Unit] = new Link[Unit](in.toList)
  implicit def strPairToLink(in: (Seq[String], Boolean)): Link[Unit] = new Link[Unit](in._1.toList, in._2)

  implicit def strToFailMsg(in: => String): FailMsg = () => {
    RedirectWithState(
      LiftRules.siteMapFailRedirectLocation.mkString("/", "/", ""),
      RedirectState(Empty, in -> NoticeType.Error)
    )
  }

  implicit def strFuncToFailMsg(in: () => String): FailMsg = strToFailMsg(in())

  implicit def redirectToFailMsg(in: => RedirectResponse): FailMsg = () => in
}

case class CompleteMenu(lines: Seq[MenuItem]) {
  lazy val breadCrumbs: Seq[MenuItem] = lines.flatMap(_.breadCrumbs)
}

case class MenuItem(text: NodeSeq, uri: NodeSeq,  kids: Seq[MenuItem],
                    current: Boolean,
                    path: Boolean,
                    info: List[Loc.LocInfoVal[_]]) {

  private var _placeholder = false
  def placeholder_? = _placeholder

  def this(text: NodeSeq, uri: NodeSeq,  kids: Seq[MenuItem],
           current: Boolean,
           path: Boolean,
           info: List[Loc.LocInfoVal[_]],
           ph: Boolean) = {
    this(text, uri, kids, current, path, info)
    _placeholder = ph
  }

  def breadCrumbs: Seq[MenuItem] = {
    if (!path) Nil
    else this :: kids.toList.flatMap(_.breadCrumbs)
  }
}
