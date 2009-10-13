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
package net.liftweb.mapper

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.util._
import Helpers._
import _root_.java.sql.{Connection, DriverManager}

//import _root_.net.liftweb.mapper.DBVendors.{MySqlRunner, DerbyRunner}

class MapperSpecsAsTest extends JUnit3(MapperSpecs)
object MapperSpecsRunner extends ConsoleRunner(MapperSpecs)

object MapperSpecs extends Specification {
  def providers = DBProviders.asList

  /*
   private def logDBStuff(log: DBLog, len: Long) {
   println(" in log stuff "+log.getClass.getName)
   log match {
   case null =>
   case _ => println(log.allEntries)
   }
   }

   DB.addLogFunc(logDBStuff)
   */

  def dbSetup() {
    Schemifier.destroyTables_!!(ignoreLogger _, SampleModel, SampleTag, 
				Dog, User)
    Schemifier.schemify(true, ignoreLogger _, SampleModel, SampleTag,
                        User,
                        Dog)
  }

  providers.foreach(provider => {

      def cleanup() {
        try { provider.setupDB } catch { case e=> skip("Provider %s not available: %s".format(provider, e)) }

        Schemifier.destroyTables_!!(ignoreLogger _, SampleModel, SampleTag, User, Dog)
        Schemifier.schemify(true, ignoreLogger _, SampleModel, SampleTag, User, Dog)
      }

      ("Mapper for " + provider.name) should {

        "schemify" in {
	  cleanup()

          val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).open_!
          val madeline = SampleModel.find(By(SampleModel.firstName, "Madeline")).open_!
          val archer = SampleModel.find(By(SampleModel.firstName, "Archer")).open_!
          val notNull = SampleModel.find(By(SampleModel.firstName, "NotNull")).open_!

          elwood.firstName.is must_== "Elwood"
          madeline.firstName.is must_== "Madeline"
          archer.firstName.is must_== "Archer"

          archer.moose.is must_== Empty
          notNull.moose.is must_== Full(99L)

          val meow = SampleTag.find(By(SampleTag.tag, "Meow")).open_!

          meow.tag.is must_== "Meow"

          elwood.id.is must be_<(madeline.id.is)
        }


        "Like works" in {
          cleanup()

          val oo = SampleTag.findAll(Like(SampleTag.tag, "%oo%"))

          (oo.length > 0) must beTrue

          for (t <- oo)
	  (t.tag.is.indexOf("oo") >= 0) must beTrue

          for (t <- oo)
	  t.model.cached_? must beFalse

          val mm = SampleTag.findAll(Like(SampleTag.tag, "M%"))

          (mm.length > 0) must beTrue

          for (t <- mm)
	  (t.tag.is.startsWith("M")) must beTrue

          for (t <- mm) {
            t.model.cached_? must beFalse
            t.model.obj
            t.model.cached_? must beTrue
          }
        }

        "Nullable Long works" in {
          cleanup()

          SampleModel.create.firstName("fruit").moose(Full(77L)).save

          SampleModel.findAll(By(SampleModel.moose, Empty)).length must_== 3L
          SampleModel.findAll(NotBy(SampleModel.moose, Empty)).length must_== 2L
          SampleModel.findAll(NotNullRef(SampleModel.moose)).length must_== 2L
          SampleModel.findAll(NullRef(SampleModel.moose)).length must_== 3L
        }

        "enforce NOT NULL" in {
          cleanup()

          val nullString: String = null
          try {
            SampleModel.create.firstName("Not Null").notNull(nullString).save
            0 must_== 1
          } catch {
            case e: java.sql.SQLException =>
          }
        }

        "Precache works" in {

          cleanup()

          val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"),
                                     PreCache(SampleTag.model))

          (oo.length > 0) must beTrue

          for (t <- oo)
	  t.model.cached_? must beTrue
        }

        "Precache works with OrderBy" in {
          if ((provider ne DBProviders.DerbyProvider)
              && (provider ne DBProviders.MySqlProvider)) { // this doesn't work for Derby, but it's a derby bug
	    // nor does it work in MySQL, but it's a MySQL limitation
            //  try { provider.setupDB } catch { case e => skip(e.getMessage) }

            cleanup()

	    val dogs = Dog.findAll(By(Dog.name,"fido"),OrderBy(Dog.name,Ascending),
                                   PreCache(Dog.owner))

            val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending),
                                       MaxRows(2),
                                       PreCache(SampleTag.model))

            (oo.length > 0) must beTrue

            for (t <- oo)
            t.model.cached_? must beTrue
          }
        }

        "Non-deterministic Precache works" in {
          cleanup()

          val dogs = Dog.findAll(By(Dog.name,"fido"), PreCache(Dog.owner, false))
          val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"),
                                     PreCache(SampleTag.model, false))

          (oo.length > 0) must beTrue

          for (t <- oo)
	  t.model.cached_? must beTrue
        }

        "Non-deterministic Precache works with OrderBy" in {
          cleanup()

          val dogs = Dog.findAll(By(Dog.name,"fido"),OrderBy(Dog.name,Ascending),
                                 PreCache(Dog.owner, false))

          val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending),
                                     MaxRows(2),
                                     PreCache(SampleTag.model, false))

          (oo.length > 0) must beTrue

          for (t <- oo)
	  t.model.cached_? must beTrue
        }
      

        "Save flag works" in {
          cleanup()

          val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).open_!

          elwood.firstName.is must_== "Elwood"

          elwood.firstName("Frog").save

          val frog = SampleModel.find(By(SampleModel.firstName, "Frog")).open_!

          frog.firstName.is must_== "Frog"

          SampleModel.findAll().length must_== 4

          SampleModel.find(By(SampleModel.firstName, "Elwood")).isEmpty must_== true
        }

        "accept a Seq[T] as argument to ByList query parameter" in {
          // See http://github.com/dpp/liftweb/issues#issue/77 for original request
          cleanup()
          val seq: Seq[String] = List("Elwood", "Archer")
          val result = SampleModel.findAll(ByList(SampleModel.firstName, seq))
          result.length must_== 2
        }

      }
    })

  private def ignoreLogger(f: => AnyRef): Unit = ()
}

object SampleTag extends SampleTag with LongKeyedMetaMapper[SampleTag] {
  override def dbAddTable = Full(populate _)
  private def populate {
    val samp = SampleModel.findAll()
    val tags = List("Hello", "Moose", "Frog", "WooHoo", "Sloth",
		    "Meow", "Moof")
    for (t <- tags;
	 m <- samp) SampleTag.create.tag(t).model(m).save
  }
}

class SampleTag extends LongKeyedMapper[SampleTag] with IdPK {
  def getSingleton = SampleTag // what's the "meta" server

  object tag extends MappedString(this, 32)
  object model extends MappedLongForeignKey(this, SampleModel)
}

object SampleModel extends SampleModel with KeyedMetaMapper[Long, SampleModel] {
  override def dbAddTable = Full(populate _)

  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
    create.firstName("NotNull").moose(Full(99L)).save
  }
}

class SampleModel extends KeyedMapper[Long, SampleModel] {
  def getSingleton = SampleModel // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object firstName extends MappedString(this, 32)
  object moose extends MappedNullableLong(this)
  object notNull extends MappedString(this, 32) {
    override def dbNotNull_? = true
  }
}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {
  override def dbAddTable = Full(populate _)
  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
  }

  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
      <lift:bind /></lift:surround>)
  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
                                 locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true
}
 
/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
}

class Dog extends LongKeyedMapper[Dog] with IdPK {
  def getSingleton = Dog

  object name extends MappedPoliteString(this, 128)
  object weight extends MappedInt(this)
  object owner extends MappedLongForeignKey(this,User)
}

object Dog extends Dog with LongKeyedMetaMapper[Dog] {
  override def dbAddTable = Full(populate _)

  private def populate {
    create.name("Elwood").save
    create.name("Madeline").save
    create.name("Archer").save
    create.name("fido").owner(User.find(By(User.firstName, "Elwood"))).save
  }
}
