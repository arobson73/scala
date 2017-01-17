package com.andyr.testproject

import org.scalatest.FunSuite
import com.andyr.foo._
import com.andyr.bar._ 

class HelloSuite extends FunSuite {
 
  test("the Foo name is set correctly") {
    val f = Foo("Foo")
    assert(f.name == "Foo")
  }
 
  test("the Bar name is set correctly") {
    val b = Bar("Bar")
    assert(b.name == "Bar")
  }

}

