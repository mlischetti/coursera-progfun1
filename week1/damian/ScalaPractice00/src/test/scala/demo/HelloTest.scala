package demo

import org.scalatest.FunSuite

/**
  * Created by damiandecurgez on 07/06/16.
  */
class HelloTest extends FunSuite {
  test("sayHello method works OK"){
    val hello = new Hello
    assert(hello.sayHello("Scala") == "Hello, Scala!")
  }

}
