package org.ciroque

//import java.awt.image.BufferedImage
//import java.awt.{Graphics2D, Color, Font, BasicStroke}

import ar.com.hjg.pngj
import java.io.FileOutputStream

case class Point(x: Int, y:Int)
case class PointOfLight(group: Int, x: Int, y: Int, deltaX: Int, deltaY: Int)

object AoC extends Data with App {
  println(s"Part One: ${Solution.partOne(positionData)}")
  println(s"Part Two: ${Solution.partTwo()}")
}

object Solution {
    def partOne(positionData: List[PointOfLight]) = {

      def findBoundingBoxPoints(positions: List[PointOfLight]): (Int, Int, Int, Int) = { // (Top, Left, Bottom, Right)
        val top = positions.minBy(_.y)
        val left = positions.minBy(_.x)
        val bottom = positions.maxBy(_.y)
        val right = positions.maxBy(_.x)
        (top.y, left.x, bottom.y, right.x)
      }

      def calculateMovements(uLimit: Int): List[PointOfLight] = {
        (0 to uLimit).flatMap {
          index: Int => {
            positionData.map {
              case PointOfLight(_, x, y, deltaX, deltaY) =>
                PointOfLight(index, x + (index * deltaX), y + (index * deltaY), deltaX, deltaY)
            }
          }
        }.toList
      }

      def renderFrame(positions: List[PointOfLight], size: Point, offsets: Point, filestamp: Long): Unit = {
        println(
          s"""
             | group: ${positions.head.group}
             | size: $size
             | array length: ${size.x * size.y}
             | offsets: $offsets
             | filestamp: $filestamp
             | positions: $positions
             |
           """.stripMargin)

        val outputStream = new FileOutputStream(s"${filestamp}-${positions.head.group}.png")
        val imageInfo = new ar.com.hjg.pngj.ImageInfo(size.x, size.y, 8, false)
        val png = new ar.com.hjg.pngj.PngWriter(outputStream, imageInfo)
        png.getMetadata().setDpi(100.0)
        png.getMetadata().setTimeNow(0)
        png.getMetadata().setText("KEY", "Advent of Code, Day 10, Part 1")

        (0 to imageInfo.rows - 1).foreach {
          row =>
            val line = new ar.com.hjg.pngj.ImageLineByte(imageInfo)
            png.writeRow(line)
        }

        png.end()


//        val canvas = new BufferedImage(size.x, size.y, BufferedImage.TYPE_INT_RGB)
//        val g = canvas.createGraphics()
//        g.setColor(Color.WHITE)
//        g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
////        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
//        g.setColor(new Color(0, 128, 0)) // a darker green
//        g.setFont(new Font("Batang", Font.PLAIN, 1))
//
//        positions.foreach {
//          position => g.drawString("#", position.x + offsets.x, position.y + offsets.y)
//        }
//        g.dispose()
//
//        javax.imageio.ImageIO.write(canvas, "png", new java.io.File(s"${filestamp}-${positions.head.group}.png"))
      }

      def renderFrames(groups: Map[Int, List[PointOfLight]]): Unit = {
        val filestamp = System.currentTimeMillis / 1000
        val (top, left, bottom, right) = findBoundingBoxPoints(positionData)
        val size = Point(Math.abs(right - left), Math.abs(bottom - top))
        val offsets = Point(Math.abs(Math.min(left, 0)), Math.abs(Math.min(top, 0)))

        groups.foreach(group => renderFrame(group._2, size, offsets, filestamp))
      }

      val movementGroups = calculateMovements(10).groupBy(x => x.group)

      renderFrames(movementGroups)

      0
    }
    def partTwo() = 0
}

trait Data {

  lazy val testData = List(
    "position=< 9,  1> velocity=< 0,  2>",
    "position=< 7,  0> velocity=<-1,  0>",
    "position=< 3, -2> velocity=<-1,  1>",
    "position=< 6, 10> velocity=<-2, -1>",
    "position=< 2, -4> velocity=< 2,  2>",
    "position=<-6, 10> velocity=< 2, -2>",
    "position=< 1,  8> velocity=< 1, -1>",
    "position=< 1,  7> velocity=< 1,  0>",
    "position=<-3, 11> velocity=< 1, -2>",
    "position=< 7,  6> velocity=<-1, -1>",
    "position=<-2,  3> velocity=< 1,  0>",
    "position=<-4,  3> velocity=< 2,  0>",
    "position=<10, -3> velocity=<-1,  1>",
    "position=< 5, 11> velocity=< 1, -2>",
    "position=< 4,  7> velocity=< 0, -1>",
    "position=< 8, -2> velocity=< 0,  1>",
    "position=<15,  0> velocity=<-2,  0>",
    "position=< 1,  6> velocity=< 1,  0>",
    "position=< 8,  9> velocity=< 0, -1>",
    "position=< 3,  3> velocity=<-1,  1>",
    "position=< 0,  5> velocity=< 0, -1>",
    "position=<-2,  2> velocity=< 2,  0>",
    "position=< 5, -2> velocity=< 1,  2>",
    "position=< 1,  4> velocity=< 2,  1>",
    "position=<-2,  7> velocity=< 2, -2>",
    "position=< 3,  6> velocity=<-1, -1>",
    "position=< 5,  0> velocity=< 1,  0>",
    "position=<-6,  0> velocity=< 2,  0>",
    "position=< 5,  9> velocity=< 1, -2>",
    "position=<14,  7> velocity=<-2,  0>",
    "position=<-3,  6> velocity=< 2, -1>"
  )

  lazy val fullData = List(
    "position=<-20620, -41485> velocity=< 2,  4>",
    "position=<-51844,  41770> velocity=< 5, -4>",
    "position=<-51817, -20670> velocity=< 5,  2>",
    "position=< 21000,  52179> velocity=<-2, -5>",
    "position=< 21051,  31360> velocity=<-2, -3>",
    "position=< 41859, -41487> velocity=<-4,  4>",
    "position=< 41830,  31361> velocity=<-4, -3>",
    "position=<-51817, -51895> velocity=< 5,  5>",
    "position=<-51836, -41484> velocity=< 5,  4>",
    "position=< 21032, -51887> velocity=<-2,  5>",
    "position=<-31003, -41482> velocity=< 3,  4>",
    "position=< 52261, -51890> velocity=<-5,  5>",
    "position=<-51836,  10547> velocity=< 5, -1>",
    "position=< 10614, -10259> velocity=<-1,  1>",
    "position=<-10194,  41776> velocity=< 1, -4>",
    "position=<-10186,  10550> velocity=< 1, -1>",
    "position=< 31431, -51892> velocity=<-3,  5>",
    "position=<-51804,  52175> velocity=< 5, -5>",
    "position=< 10601,  52183> velocity=<-1, -5>",
    "position=<-51812,  10548> velocity=< 5, -1>",
    "position=< 10633,  20959> velocity=<-1, -2>",
    "position=<-41417, -41480> velocity=< 4,  4>",
    "position=< 10628,  31360> velocity=<-1, -3>",
    "position=<-10160, -20666> velocity=< 1,  2>",
    "position=<-51793, -20673> velocity=< 5,  2>",
    "position=<-41430, -41485> velocity=< 4,  4>",
    "position=< 21032, -20670> velocity=<-2,  2>",
    "position=<-30987, -51891> velocity=< 3,  5>",
    "position=< 52280, -31080> velocity=<-5,  3>",
    "position=<-30995, -10266> velocity=< 3,  1>",
    "position=<-10177, -51888> velocity=< 1,  5>",
    "position=<-30995,  10551> velocity=< 3, -1>",
    "position=< 21028,  20961> velocity=<-2, -2>",
    "position=< 31412, -51895> velocity=<-3,  5>",
    "position=< 31467, -10260> velocity=<-3,  1>",
    "position=< 31407, -51895> velocity=<-3,  5>",
    "position=< 10636,  41773> velocity=<-1, -4>",
    "position=< 31418, -51892> velocity=<-3,  5>",
    "position=<-20620,  41773> velocity=< 2, -4>",
    "position=<-10163, -10265> velocity=< 1,  1>",
    "position=<-20594, -10264> velocity=< 2,  1>",
    "position=< 52266, -10262> velocity=<-5,  1>",
    "position=< 52223,  52178> velocity=<-5, -5>",
    "position=< 21019, -20666> velocity=<-2,  2>",
    "position=<-20620,  20958> velocity=< 2, -2>",
    "position=< 21016,  20960> velocity=<-2, -2>",
    "position=< 10593, -10259> velocity=<-1,  1>",
    "position=< 21045,  31365> velocity=<-2, -3>",
    "position=< 10609,  10548> velocity=<-1, -1>",
    "position=< 21016, -20666> velocity=<-2,  2>",
    "position=< 10598, -10265> velocity=<-1,  1>",
    "position=<-41394,  20955> velocity=< 4, -2>",
    "position=<-20612, -10262> velocity=< 2,  1>",
    "position=<-30987, -41489> velocity=< 3,  4>",
    "position=< 52261,  31361> velocity=<-5, -3>",
    "position=< 31408,  31364> velocity=<-3, -3>",
    "position=<-10181, -31076> velocity=< 1,  3>",
    "position=<-20625, -10268> velocity=< 2,  1>",
    "position=< 41846,  10551> velocity=<-4, -1>",
    "position=<-20588, -31074> velocity=< 2,  3>",
    "position=<-41407,  31364> velocity=< 4, -3>",
    "position=<-20577,  41767> velocity=< 2, -4>",
    "position=< 21040,  31361> velocity=<-2, -3>",
    "position=<-20586, -51892> velocity=< 2,  5>",
    "position=<-41386,  31364> velocity=< 4, -3>",
    "position=< 21001,  20953> velocity=<-2, -2>",
    "position=<-51797, -20671> velocity=< 5,  2>",
    "position=<-20620, -41484> velocity=< 2,  4>",
    "position=< 31433, -51887> velocity=<-3,  5>",
    "position=< 10617,  31363> velocity=<-1, -3>",
    "position=< 10598,  52175> velocity=<-1, -5>",
    "position=<-41413, -51891> velocity=< 4,  5>",
    "position=<-41386, -10260> velocity=< 4,  1>",
    "position=<-51846, -20671> velocity=< 5,  2>",
    "position=< 52273, -51892> velocity=<-5,  5>",
    "position=<-51836, -51896> velocity=< 5,  5>",
    "position=<-20596,  20959> velocity=< 2, -2>",
    "position=< 41814,  31360> velocity=<-4, -3>",
    "position=< 10620, -31073> velocity=<-1,  3>",
    "position=<-41438,  31360> velocity=< 4, -3>",
    "position=< 21060,  31361> velocity=<-2, -3>",
    "position=<-20628, -41487> velocity=< 2,  4>",
    "position=<-10185, -51896> velocity=< 1,  5>",
    "position=<-31022, -20671> velocity=< 3,  2>",
    "position=< 31407,  10551> velocity=<-3, -1>",
    "position=< 41830, -51887> velocity=<-4,  5>",
    "position=< 52263,  52177> velocity=<-5, -5>",
    "position=<-41413, -10267> velocity=< 4,  1>",
    "position=< 10653, -10260> velocity=<-1,  1>",
    "position=<-20628, -31074> velocity=< 2,  3>",
    "position=< 41830, -51892> velocity=<-4,  5>",
    "position=<-51822,  20953> velocity=< 5, -2>",
    "position=< 52221,  52181> velocity=<-5, -5>",
    "position=<-20583, -41488> velocity=< 2,  4>",
    "position=<-51813, -31082> velocity=< 5,  3>",
    "position=< 41822,  52175> velocity=<-4, -5>",
    "position=<-41442, -41482> velocity=< 4,  4>",
    "position=<-10172, -31078> velocity=< 1,  3>",
    "position=<-10178, -10263> velocity=< 1,  1>",
    "position=<-51797,  31360> velocity=< 5, -3>",
    "position=< 10650,  31364> velocity=<-1, -3>",
    "position=<-51798,  10550> velocity=< 5, -1>",
    "position=< 31444, -51893> velocity=<-3,  5>",
    "position=< 21008, -51888> velocity=<-2,  5>",
    "position=<-51792, -31078> velocity=< 5,  3>",
    "position=<-30991,  20960> velocity=< 3, -2>",
    "position=< 10606, -10263> velocity=<-1,  1>",
    "position=<-41429,  31363> velocity=< 4, -3>",
    "position=< 41843,  20960> velocity=<-4, -2>",
    "position=< 31423, -51896> velocity=<-3,  5>",
    "position=<-20586,  20957> velocity=< 2, -2>",
    "position=< 21002, -20675> velocity=<-2,  2>",
    "position=< 21056,  20953> velocity=<-2, -2>",
    "position=< 31420,  10549> velocity=<-3, -1>",
    "position=< 21029,  31365> velocity=<-2, -3>",
    "position=< 21048,  20955> velocity=<-2, -2>",
    "position=< 31463, -10264> velocity=<-3,  1>",
    "position=<-31007, -41489> velocity=< 3,  4>",
    "position=<-20595, -20675> velocity=< 2,  2>",
    "position=<-51849, -20673> velocity=< 5,  2>",
    "position=< 41856, -10264> velocity=<-4,  1>",
    "position=<-31011, -20671> velocity=< 3,  2>",
    "position=<-31033,  41767> velocity=< 3, -4>",
    "position=<-10165,  20956> velocity=< 1, -2>",
    "position=<-41418, -20667> velocity=< 4,  2>",
    "position=< 21024,  41775> velocity=<-2, -4>",
    "position=<-20572,  10547> velocity=< 2, -1>",
    "position=<-41383,  52181> velocity=< 4, -5>",
    "position=<-31022,  20956> velocity=< 3, -2>",
    "position=< 31415, -51889> velocity=<-3,  5>",
    "position=<-10184, -51887> velocity=< 1,  5>",
    "position=<-41418,  10548> velocity=< 4, -1>",
    "position=< 52238,  10555> velocity=<-5, -1>",
    "position=<-41439,  10546> velocity=< 4, -1>",
    "position=< 21013, -51887> velocity=<-2,  5>",
    "position=< 52247,  41767> velocity=<-5, -4>",
    "position=<-20599, -51895> velocity=< 2,  5>",
    "position=< 41823,  31364> velocity=<-4, -3>",
    "position=<-41394,  41773> velocity=< 4, -4>",
    "position=<-41394,  10547> velocity=< 4, -1>",
    "position=<-31027, -51893> velocity=< 3,  5>",
    "position=< 31420,  10554> velocity=<-3, -1>",
    "position=< 52258, -10267> velocity=<-5,  1>",
    "position=< 52277, -20672> velocity=<-5,  2>",
    "position=< 10601, -41488> velocity=<-1,  4>",
    "position=< 52258,  31362> velocity=<-5, -3>",
    "position=<-31031, -31078> velocity=< 3,  3>",
    "position=<-30987, -31073> velocity=< 3,  3>",
    "position=<-30990,  41775> velocity=< 3, -4>",
    "position=< 31432, -41489> velocity=<-3,  4>",
    "position=<-31001,  41767> velocity=< 3, -4>",
    "position=<-20588, -20675> velocity=< 2,  2>",
    "position=<-20591,  41769> velocity=< 2, -4>",
    "position=<-31003,  20953> velocity=< 3, -2>",
    "position=<-51793,  52178> velocity=< 5, -5>",
    "position=<-41410,  31361> velocity=< 4, -3>",
    "position=<-20580,  31369> velocity=< 2, -3>",
    "position=<-20594,  31360> velocity=< 2, -3>",
    "position=< 41855, -41487> velocity=<-4,  4>",
    "position=<-10173,  20954> velocity=< 1, -2>",
    "position=< 21016,  20954> velocity=<-2, -2>",
    "position=< 41843, -51888> velocity=<-4,  5>",
    "position=< 52241, -51887> velocity=<-5,  5>",
    "position=<-51816, -31082> velocity=< 5,  3>",
    "position=<-31035, -20666> velocity=< 3,  2>",
    "position=< 41857,  31366> velocity=<-4, -3>",
    "position=< 41814, -51896> velocity=<-4,  5>",
    "position=< 31439,  20957> velocity=<-3, -2>",
    "position=< 21036,  31366> velocity=<-2, -3>",
    "position=<-20615, -41488> velocity=< 2,  4>",
    "position=< 10634, -20674> velocity=<-1,  2>",
    "position=< 52269,  20958> velocity=<-5, -2>",
    "position=<-41441, -20671> velocity=< 4,  2>",
    "position=<-30995, -20670> velocity=< 3,  2>",
    "position=< 41872, -20672> velocity=<-4,  2>",
    "position=< 52222,  31364> velocity=<-5, -3>",
    "position=<-41434,  20956> velocity=< 4, -2>",
    "position=< 41814,  10554> velocity=<-4, -1>",
    "position=< 41833, -41480> velocity=<-4,  4>",
    "position=<-31007,  31368> velocity=< 3, -3>",
    "position=< 10609, -41481> velocity=<-1,  4>",
    "position=<-51844,  41769> velocity=< 5, -4>",
    "position=< 41814,  31361> velocity=<-4, -3>",
    "position=< 41818,  20957> velocity=<-4, -2>",
    "position=< 41859, -51888> velocity=<-4,  5>",
    "position=< 31455, -20667> velocity=<-3,  2>",
    "position=< 21045,  10549> velocity=<-2, -1>",
    "position=< 10609,  10550> velocity=<-1, -1>",
    "position=<-20628,  10552> velocity=< 2, -1>",
    "position=<-41431, -20671> velocity=< 4,  2>",
    "position=< 41858,  52181> velocity=<-4, -5>",
    "position=< 10595,  20953> velocity=<-1, -2>",
    "position=<-10208,  20960> velocity=< 1, -2>",
    "position=<-51817,  10553> velocity=< 5, -1>",
    "position=<-20568, -31074> velocity=< 2,  3>",
    "position=< 52241,  10555> velocity=<-5, -1>",
    "position=< 31452,  41774> velocity=<-3, -4>",
    "position=< 41865, -41485> velocity=<-4,  4>",
    "position=< 10638, -31080> velocity=<-1,  3>",
    "position=<-20571,  31364> velocity=< 2, -3>",
    "position=<-41394,  31363> velocity=< 4, -3>",
    "position=< 31407,  10546> velocity=<-3, -1>",
    "position=< 41870,  31366> velocity=<-4, -3>",
    "position=< 21040, -31074> velocity=<-2,  3>",
    "position=< 10614, -20666> velocity=<-1,  2>",
    "position=<-51801,  52182> velocity=< 5, -5>",
    "position=<-31027,  41767> velocity=< 3, -4>",
    "position=< 31415, -20671> velocity=<-3,  2>",
    "position=<-51817, -41481> velocity=< 5,  4>",
    "position=< 31452,  52177> velocity=<-3, -5>",
    "position=< 21027, -20675> velocity=<-2,  2>",
    "position=<-31035,  20957> velocity=< 3, -2>",
    "position=<-51814, -31077> velocity=< 5,  3>",
    "position=<-20569,  41769> velocity=< 2, -4>",
    "position=< 21013, -51889> velocity=<-2,  5>",
    "position=< 21033, -51892> velocity=<-2,  5>",
    "position=<-10173, -20669> velocity=< 1,  2>",
    "position=<-20596,  41770> velocity=< 2, -4>",
    "position=< 21027,  52183> velocity=<-2, -5>",
    "position=<-41418, -31076> velocity=< 4,  3>",
    "position=< 52264,  52179> velocity=<-5, -5>",
    "position=< 21048,  10551> velocity=<-2, -1>",
    "position=< 52221,  20956> velocity=<-5, -2>",
    "position=<-51793,  20954> velocity=< 5, -2>",
    "position=<-30999,  10550> velocity=< 3, -1>",
    "position=< 41827, -31075> velocity=<-4,  3>",
    "position=<-41423,  10555> velocity=< 4, -1>",
    "position=< 52272,  41771> velocity=<-5, -4>",
    "position=< 21008,  52180> velocity=<-2, -5>",
    "position=<-20595, -31078> velocity=< 2,  3>",
    "position=<-10178, -51890> velocity=< 1,  5>",
    "position=< 21013,  10548> velocity=<-2, -1>",
    "position=<-51793, -41483> velocity=< 5,  4>",
    "position=<-51808,  10547> velocity=< 5, -1>",
    "position=< 21050, -41489> velocity=<-2,  4>",
    "position=<-20599, -41481> velocity=< 2,  4>",
    "position=<-51833,  31367> velocity=< 5, -3>",
    "position=<-51817, -31082> velocity=< 5,  3>",
    "position=<-30995,  31364> velocity=< 3, -3>",
    "position=< 31455,  10546> velocity=<-3, -1>",
    "position=< 31433,  41776> velocity=<-3, -4>",
    "position=<-30995, -10264> velocity=< 3,  1>",
    "position=<-51849, -51889> velocity=< 5,  5>",
    "position=<-31026,  20957> velocity=< 3, -2>",
    "position=< 31468,  31360> velocity=<-3, -3>",
    "position=< 31449, -51893> velocity=<-3,  5>",
    "position=<-41397,  20959> velocity=< 4, -2>",
    "position=<-10181,  10549> velocity=< 1, -1>",
    "position=< 10638, -10268> velocity=<-1,  1>",
    "position=< 41843, -10262> velocity=<-4,  1>",
    "position=<-10189,  52183> velocity=< 1, -5>",
    "position=< 41859, -51893> velocity=<-4,  5>",
    "position=< 41819, -51893> velocity=<-4,  5>",
    "position=< 31431, -10263> velocity=<-3,  1>",
    "position=<-51793,  52179> velocity=< 5, -5>",
    "position=< 10593, -10263> velocity=<-1,  1>",
    "position=< 21040,  31369> velocity=<-2, -3>",
    "position=< 10649, -10267> velocity=<-1,  1>",
    "position=< 41875, -10268> velocity=<-4,  1>",
    "position=<-20600,  10554> velocity=< 2, -1>",
    "position=< 31468,  20953> velocity=<-3, -2>",
    "position=<-31019,  20958> velocity=< 3, -2>",
    "position=<-20624, -51892> velocity=< 2,  5>",
    "position=< 41842,  20953> velocity=<-4, -2>",
    "position=<-20599, -41480> velocity=< 2,  4>",
    "position=< 10645, -51896> velocity=<-1,  5>",
    "position=<-10216,  52175> velocity=< 1, -5>",
    "position=< 21024,  31363> velocity=<-2, -3>",
    "position=< 41870,  10551> velocity=<-4, -1>",
    "position=<-20572, -20672> velocity=< 2,  2>",
    "position=<-20583,  31369> velocity=< 2, -3>",
    "position=<-20586,  31363> velocity=< 2, -3>",
    "position=<-20596,  10549> velocity=< 2, -1>",
    "position=< 31436,  41772> velocity=<-3, -4>",
    "position=<-41418,  20960> velocity=< 4, -2>",
    "position=<-10205, -51889> velocity=< 1,  5>",
    "position=< 10649, -31074> velocity=<-1,  3>",
    "position=< 10622,  20959> velocity=<-1, -2>",
    "position=< 52274, -10268> velocity=<-5,  1>",
    "position=< 52222, -51896> velocity=<-5,  5>",
    "position=< 52266, -31077> velocity=<-5,  3>",
    "position=< 41854, -10259> velocity=<-4,  1>",
    "position=< 31463, -20670> velocity=<-3,  2>",
    "position=< 21045,  10552> velocity=<-2, -1>",
    "position=<-10197,  20955> velocity=< 1, -2>",
    "position=< 31431, -51892> velocity=<-3,  5>",
    "position=< 52250, -10262> velocity=<-5,  1>",
    "position=< 41830, -20672> velocity=<-4,  2>",
    "position=<-20596, -51887> velocity=< 2,  5>",
    "position=<-10203, -41480> velocity=< 1,  4>",
    "position=< 41850,  52174> velocity=<-4, -5>",
    "position=< 10628,  52179> velocity=<-1, -5>",
    "position=<-30986,  31360> velocity=< 3, -3>",
    "position=<-20591, -31074> velocity=< 2,  3>",
    "position=< 52234, -51887> velocity=<-5,  5>",
    "position=<-20604, -51891> velocity=< 2,  5>",
    "position=< 10593,  41768> velocity=<-1, -4>",
    "position=< 31439,  31362> velocity=<-3, -3>",
    "position=< 41859, -51892> velocity=<-4,  5>",
    "position=<-10205,  31369> velocity=< 1, -3>",
    "position=<-31027,  10548> velocity=< 3, -1>",
    "position=< 10633,  10548> velocity=<-1, -1>",
    "position=< 21028,  20953> velocity=<-2, -2>",
    "position=< 21016,  20959> velocity=<-2, -2>",
    "position=< 31434, -10263> velocity=<-3,  1>",
    "position=<-10192,  10555> velocity=< 1, -1>",
    "position=< 52234,  31366> velocity=<-5, -3>",
    "position=< 41822,  31363> velocity=<-4, -3>",
    "position=< 41838, -51895> velocity=<-4,  5>",
    "position=<-31007,  20958> velocity=< 3, -2>",
    "position=<-41402,  31362> velocity=< 4, -3>",
    "position=< 52229,  41769> velocity=<-5, -4>",
    "position=< 10641, -41482> velocity=<-1,  4>",
    "position=<-51804, -41484> velocity=< 5,  4>",
    "position=< 41827,  20962> velocity=<-4, -2>",
    "position=<-10208, -31080> velocity=< 1,  3>",
    "position=< 41826,  41771> velocity=<-4, -4>",
    "position=< 21041, -10266> velocity=<-2,  1>",
    "position=<-51808, -51895> velocity=< 5,  5>",
    "position=<-51841,  41774> velocity=< 5, -4>",
    "position=<-20596,  41774> velocity=< 2, -4>",
    "position=< 41866,  31364> velocity=<-4, -3>",
    "position=< 31407, -20672> velocity=<-3,  2>",
    "position=< 10643, -20671> velocity=<-1,  2>",
    "position=< 41865,  10546> velocity=<-4, -1>",
    "position=<-51792,  10551> velocity=< 5, -1>",
    "position=<-30977, -10262> velocity=< 3,  1>",
    "position=< 31436,  41775> velocity=<-3, -4>",
    "position=< 31443,  20960> velocity=<-3, -2>",
    "position=<-30977,  10549> velocity=< 3, -1>",
    "position=<-31022, -31082> velocity=< 3,  3>",
    "position=<-41434, -41488> velocity=< 4,  4>",
    "position=<-20604, -31074> velocity=< 2,  3>",
    "position=< 52261,  31367> velocity=<-5, -3>",
    "position=< 21029,  20960> velocity=<-2, -2>",
    "position=< 10625,  52174> velocity=<-1, -5>",
    "position=<-41433,  41771> velocity=< 4, -4>",
    "position=< 52269,  20954> velocity=<-5, -2>",
    "position=< 10625,  41768> velocity=<-1, -4>",
    "position=< 41830,  20959> velocity=<-4, -2>",
    "position=< 31431, -10266> velocity=<-3,  1>",
    "position=<-20572,  41774> velocity=< 2, -4>",
    "position=< 52265, -20668> velocity=<-5,  2>",
    "position=< 10595, -41485> velocity=<-1,  4>",
    "position=<-20580, -10264> velocity=< 2,  1>",
    "position=< 52270, -10268> velocity=<-5,  1>",
    "position=<-31019,  31362> velocity=< 3, -3>",
    "position=< 31439, -10262> velocity=<-3,  1>",
    "position=<-20618,  20957> velocity=< 2, -2>",
    "position=<-41407,  31364> velocity=< 4, -3>",
    "position=< 41814, -31080> velocity=<-4,  3>",
    "position=<-10208, -20673> velocity=< 1,  2>",
    "position=< 31412, -20673> velocity=<-3,  2>",
    "position=< 31463,  52183> velocity=<-3, -5>",
    "position=<-30983, -51896> velocity=< 3,  5>",
    "position=< 31447,  10553> velocity=<-3, -1>",
    "position=< 41843, -51895> velocity=<-4,  5>",
    "position=< 31447, -10259> velocity=<-3,  1>",
    "position=<-41438, -31082> velocity=< 4,  3>",
    "position=< 10593, -31078> velocity=<-1,  3>",
    "position=<-30985,  10546> velocity=< 3, -1>",
    "position=<-31027, -41487> velocity=< 3,  4>",
    "position=< 21026,  52174> velocity=<-2, -5>",
    "position=<-51809,  52178> velocity=< 5, -5>",
    "position=<-41429, -10267> velocity=< 4,  1>",
    "position=< 52256,  10551> velocity=<-5, -1>",
    "position=< 52277,  41769> velocity=<-5, -4>",
    "position=<-31000,  41767> velocity=< 3, -4>",
    "position=<-20583, -51887> velocity=< 2,  5>",
    "position=<-20594,  10550> velocity=< 2, -1>",
    "position=<-41393,  20953> velocity=< 4, -2>",
    "position=< 21002, -20671> velocity=<-2,  2>",
    "position=<-10200,  41776> velocity=< 1, -4>",
    "position=< 52258,  31369> velocity=<-5, -3>"
  )

  lazy val PositionExtractor = """position=\<(.*),(.*)\> velocity=\<(.*),(.*)\>""".r

//  lazy val positionData: List[PointOfLight] = testData.map {
  lazy val positionData: List[PointOfLight] = fullData.map {
    case PositionExtractor(x, y, deltaX, deltaY) =>
      PointOfLight(0, x.trim.toInt, y.trim.toInt, deltaX.trim.toInt, deltaY.trim.toInt)
  }
}

