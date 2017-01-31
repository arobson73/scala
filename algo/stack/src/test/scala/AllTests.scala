package test.scala

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
  classOf[test.scala.adt.TestArrayStack],
  classOf[test.scala.adt.TestArrayQueue],
  classOf[test.scala.util.TestRPNCalc]))
class AllTests {}
