package net.liftweb.json

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class JsonASTTest extends Runner(JsonASTSpec) with JUnit
object JsonASTSpec extends Specification with JValueGen with ScalaCheck {
  import JsonAST._

  "Functor identity" in {
    val identityProp = (json: JValue) => json == (json map identity)
    forAll(identityProp) must pass
  }

  "Functor composition" in {
    val compositionProp = (json: JValue, fa: JValue => JValue, fb: JValue => JValue) => 
      json.map(fb).map(fa) == json.map(fa compose fb) 

    forAll(compositionProp) must pass
  }

  "Monoid identity" in {
    val identityProp = (json: JValue) => (json ++ JNothing == json) && (JNothing ++ json == json)
    forAll(identityProp) must pass    
  }

  "Monoid associativity" in {
    val assocProp = (x: JValue, y: JValue, z: JValue) => x ++ (y ++ z) == (x ++ y) ++ z
    forAll(assocProp) must pass
  }

  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genJValue)
}
