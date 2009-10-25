package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
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

import JsonAST._

// FIXME add oldValue for changed
case class Diff(changed: JValue, added: JValue, deleted: JValue) {
  def map(f: JValue => JValue): Diff = this match {
    case x @ Diff(JNothing, JNothing, JNothing) => x
    case Diff(x, JNothing, JNothing) => Diff(f(x), JNothing, JNothing)
    case Diff(JNothing, x, JNothing) => Diff(JNothing, f(x), JNothing)
    case Diff(JNothing, JNothing, x) => Diff(JNothing, JNothing, f(x))
    case Diff(x, y, JNothing)        => Diff(f(x), f(y), JNothing)
    case Diff(x, JNothing, y)        => Diff(f(x), JNothing, f(y))
    case Diff(x, y, z)               => Diff(f(x), f(y), f(z))
  }
}

object Diff {
  def diff(val1: JValue, val2: JValue): Diff = (val1, val2) match {
    case (x, y) if x == y => Diff(JNothing, JNothing, JNothing)
    case (JObject(xs), JObject(ys)) => diffFields(xs, ys)
    case (JArray(xs), JArray(ys)) => diffVals(xs, ys)
    case (JField(xn, xv), JField(yn, yv)) if (xn == yn) => diff(xv, yv) map (JField(xn, _))
    case (x @ JField(xn, xv), y @ JField(yn, yv)) if (xn != yn) => Diff(JNothing, y, x)
    case (JInt(x), JInt(y)) if (x != y) => Diff(JInt(y), JNothing, JNothing)
    case (JDouble(x), JDouble(y)) if (x != y) => Diff(JDouble(y), JNothing, JNothing)
    case (JString(x), JString(y)) if (x != y) => Diff(JString(y), JNothing, JNothing)
    case (JBool(x), JBool(y)) if (x != y) => Diff(JBool(y), JNothing, JNothing)
    case (x, y) => Diff(JNothing, y, x)
  }

  def diffFields(vs1: List[JField], vs2: List[JField]) = {
    def diffRec(xleft: List[JField], yleft: List[JField]): Diff = xleft match {
      case Nil => Diff(JNothing, if (yleft.isEmpty) JNothing else JObject(yleft), JNothing)
      case x :: xs => yleft find (_.name == x.name) match {
        case Some(y) => 
          val Diff(c1, a1, d1) = diff(x, y)
          val Diff(c2, a2, d2) = diffRec(xs, yleft-y)
          Diff(c1 ++ c2, a1 ++ a2, d1 ++ d2) map { 
            case f: JField => JObject(f :: Nil)
            case x => x
          }
        case None => 
          val Diff(c, a, d) = diffRec(xs, yleft)
          Diff(c, a, JObject(x :: Nil) merge d)
      }
    }

    diffRec(vs1, vs2)
  }

  def diffVals(vs1: List[JValue], vs2: List[JValue]) = {
    def diffRec(xleft: List[JValue], yleft: List[JValue]): Diff = (xleft, yleft) match {
      case (xs, Nil) => Diff(JNothing, JNothing, if (xs.isEmpty) JNothing else JArray(xs))
      case (Nil, ys) => Diff(JNothing, if (ys.isEmpty) JNothing else JArray(ys), JNothing)
      case (x :: xs, y :: ys) =>
        val Diff(c1, a1, d1) = diff(x, y)
        val Diff(c2, a2, d2) = diffRec(xs, ys)
        Diff(c1 ++ c2, a1 ++ a2, d1 ++ d2)
    }

    diffRec(vs1, vs2)
  }

  private[json] trait Diffable { this: JValue =>
    def diff(other: JValue) = Diff.diff(this, other)
  }
}
