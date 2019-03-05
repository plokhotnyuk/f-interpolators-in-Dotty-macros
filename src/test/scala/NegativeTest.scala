// import org.junit.Test
// import org.junit.Assert._

// import Macro._

// /**
//   * These tests test some combinations that should make the f interpolator fail. 
//   * They come from https://github.com/lampepfl/dotty/blob/master/tests/untried/neg/stringinterpolation_macro-neg.scala 
//   * but they are slightly adapted to be junit tests.
//   */
// class NegativeTest {
//   val s = "Scala"
//   val d = 8
//   val b = false
//   val f = 3.14159
//   val c = 'c'
//   val t = new java.util.Date
//   val x = new java.util.Formattable {
//     def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff format "xxx"
//   }

//   @Test def numberOfArgumentsTest() = {
//     try{new StringContext().f2() ; fail} catch{case _ => }
//     try{new StringContext("", " is ", "%2d years old").f2(s); fail} catch {case _ => }
//     try{new StringContext("", " is ", "%2d years old").f2(s, d, d); fail} catch{case _ => }
//     try{new StringContext("", "").f2(); fail} catch{case _ => }
//   }

//   @Test def interpolationMismatchesTest() = {
//     try{f2"$s%b" ; fail} catch{case _ => }
//     try{f2"$s%c"; fail} catch{case _ => }
//     try{f2"$f%c"; fail} catch{case _ => }
//     try{f2"$s%x"; fail} catch{case _ => }
//     try{f2"$b%d"; fail} catch{case _ => }
//     try{f2"$s%d"; fail} catch{case _ => }
//     try{f2"$f%o"; fail} catch{case _ => }
//     try{f2"$s%e"; fail} catch{case _ => }
//     try{f2"$b%f"; fail} catch{case _ => }  

//     {
//       implicit val strToInt1 = (s: String) => 1
//       implicit val strToInt2 = (s: String) => 2
//       try{f2"$s%d" ; fail} catch{case _ => }
//     }

//     try{f2"$s%i" ; fail} catch{case _ => }
//   }

//   @Test def flagMismatchesTest() = {
//     try{f2"$s%+ 0,(s"; fail} catch{case _ => }
//     try{f2"$c%#+ 0,(c"; fail} catch{case _ => }
//     try{f2"$d%#d"; fail} catch{case _ => }
//     try{f2"$d%,x"; fail} catch{case _ => }
//     try{f2"$d%+ (x"; fail} catch{case _ => }
//     try{f2"$f%,(a"; fail} catch{case _ => }
//     try{f2"$t%#+ 0,(tT"; fail} catch{case _ => }
//   }

//   @Test def badPrecisionsTest() = {
//     try{f2"$c%.2c"; fail} catch{case _ => }
//     try{f2"$d%.2d"; fail} catch{case _ => }
//     try{f2"%.2%"; fail} catch{case _ => }
//     try{f2"%.2n"; fail} catch{case _ => }
//     try{f2"$f%.2a"; fail} catch{case _ => }
//     try{f2"$t%.2tT"; fail} catch{case _ => }
//   }

//   @Test def badIndexesTest() = {
//     try{f2"%<s"; fail} catch{case _ => }
//     try{f2"%<c"; fail} catch{case _ => }
//     try{f2"%<tT"; fail} catch{case _ => }
//     try{f2"${8}%d ${9}%d%3$$d"; fail} catch{case _ => }
//     try{f2"${8}%d ${9}%d%0$$d"; fail} catch{case _ => }
//   }

//   @Test def warningsTest() = {
//     try{f2"${8}%d ${9}%1$$d"; fail} catch{case _ => }
//     try{f2"$s%s $s%s %1$$<s"; fail} catch{case _ => }
//     try{f2"$s%s $s%1$$s"; fail} catch{case _ => }
//   }

//   @Test def badArgTypesTest() = {
//     try{f2"$s%#s"; fail} catch{case _ => }
//   }

//   @Test def misunderstoodConversions() = {
//     try{f2"$t%tG"; fail} catch{case _ => }
//     try{f2"$t%t"; fail} catch{case _ => }
//     try{f2"$s%10.5"; fail} catch{case _ => }
//   }

//   @Test def otherBrainFailuresTest() = {
//     try{f2"${d}random-leading-junk%d"; fail} catch{case _ => }
//   }
// }
