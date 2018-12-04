package org.ciroque

case class Point(x: Int, y: Int)
case class Claim(id: Int, xCoord: Int, yCoord: Int, width: Int, height: Int) {
  lazy val rightXCoord = xCoord + width - 1
  lazy val rightYCoord = yCoord + height - 1
  lazy val claimMap: Map[Point, Int] = {
    val x = xCoord to rightXCoord flatMap {
      x: Int =>
        yCoord to rightYCoord map {
          y: Int =>
            Point(x, y) -> id
        }
    }
    x toMap
  }
}

object AoC extends Data with App {
  println(s"Overclaimed fabric: ${Solution.calculateOverclaimedFabric(claims)}")
  println(s"Unclaimed Id: ${Solution.uncontestedClaim(claims)}")
}

object Solution {
  def claimedCounts(claims: Seq[Claim]) = {
    claims
      .flatMap { claim: Claim => claim.claimMap }
      .groupBy(c => c._1)
      .mapValues(_.size)
  }

  def calculateOverclaimedFabric(claims: Seq[Claim]): Int = {
      claimedCounts(claims).count(claim => claim._2 > 1)
  }

  def uncontestedClaim(claims: Seq[Claim]): Int = {
    val overlappingPoints = claimedCounts(claims).filter(_._2 > 1).map(_._1).toSeq

    @scala.annotation.tailrec
    def recurses(claims: Seq[Claim], overlapping: Int, id: Int): Int = {
      claims match {
        case _ if overlapping == 0 => id
        case head :: tail =>
          val x = head.claimMap.map(_._1).toSeq intersect overlappingPoints
          recurses(tail, x.length, head.id)
      }
    }

    recurses(claims, -1, -1)
  }
}

trait Data {
  val ClaimExtractor = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  lazy val claims = {
//    testData.map { claim =>
    rawData.map { claim =>
      claim match {
        case ClaimExtractor(id, xCoord, yCoord, width, height) =>
          Claim(id.toInt, xCoord.toInt, yCoord.toInt, width.toInt, height.toInt)
      }
    }
  }

  lazy val testData = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
  )

  lazy val rawData = List(
    "#1 @ 755,138: 26x19",
    "#2 @ 952,518: 15x24",
    "#3 @ 304,90: 28x12",
    "#4 @ 237,265: 17x13",
    "#5 @ 219,97: 26x24",
    "#6 @ 512,74: 19x21",
    "#7 @ 868,291: 22x25",
    "#8 @ 908,272: 17x11",
    "#9 @ 432,455: 10x8",
    "#10 @ 309,778: 13x25",
    "#11 @ 569,720: 28x29",
    "#12 @ 557,222: 26x15",
    "#13 @ 899,728: 12x22",
    "#14 @ 966,109: 13x15",
    "#15 @ 403,295: 27x24",
    "#16 @ 320,546: 10x28",
    "#17 @ 871,906: 10x20",
    "#18 @ 233,207: 27x11",
    "#19 @ 672,560: 16x25",
    "#20 @ 422,273: 22x11",
    "#21 @ 314,911: 17x26",
    "#22 @ 36,545: 18x13",
    "#23 @ 18,151: 13x24",
    "#24 @ 655,24: 28x22",
    "#25 @ 538,430: 21x26",
    "#26 @ 749,726: 19x18",
    "#27 @ 41,166: 22x29",
    "#28 @ 595,17: 11x20",
    "#29 @ 691,725: 18x20",
    "#30 @ 427,113: 11x25",
    "#31 @ 491,661: 19x23",
    "#32 @ 479,348: 20x16",
    "#33 @ 415,1: 16x17",
    "#34 @ 780,927: 23x19",
    "#35 @ 780,350: 21x3",
    "#36 @ 415,796: 12x15",
    "#37 @ 783,452: 29x24",
    "#38 @ 702,728: 16x28",
    "#39 @ 350,676: 17x21",
    "#40 @ 611,518: 12x21",
    "#41 @ 813,715: 13x26",
    "#42 @ 317,63: 29x24",
    "#43 @ 484,642: 26x14",
    "#44 @ 237,910: 24x25",
    "#45 @ 325,917: 28x20",
    "#46 @ 567,485: 20x12",
    "#47 @ 464,470: 18x26",
    "#48 @ 389,9: 13x11",
    "#49 @ 92,560: 16x21",
    "#50 @ 532,967: 23x14",
    "#51 @ 762,825: 11x13",
    "#52 @ 606,600: 23x23",
    "#53 @ 848,798: 19x24",
    "#54 @ 311,503: 10x15",
    "#55 @ 72,388: 13x19",
    "#56 @ 28,692: 15x29",
    "#57 @ 965,205: 13x13",
    "#58 @ 472,738: 18x17",
    "#59 @ 732,652: 25x18",
    "#60 @ 676,573: 23x17",
    "#61 @ 859,162: 17x24",
    "#62 @ 775,822: 19x26",
    "#63 @ 343,97: 22x20",
    "#64 @ 945,279: 13x19",
    "#65 @ 446,405: 13x13",
    "#66 @ 293,520: 26x16",
    "#67 @ 852,501: 26x19",
    "#68 @ 924,209: 21x29",
    "#69 @ 469,425: 17x17",
    "#70 @ 192,827: 14x11",
    "#71 @ 944,798: 25x24",
    "#72 @ 161,321: 16x20",
    "#73 @ 930,796: 25x10",
    "#74 @ 460,629: 26x23",
    "#75 @ 777,347: 29x10",
    "#76 @ 320,50: 20x20",
    "#77 @ 417,360: 26x18",
    "#78 @ 579,460: 14x26",
    "#79 @ 20,361: 29x11",
    "#80 @ 55,929: 26x25",
    "#81 @ 532,605: 26x29",
    "#82 @ 152,606: 24x20",
    "#83 @ 255,566: 24x18",
    "#84 @ 54,204: 13x22",
    "#85 @ 837,206: 19x16",
    "#86 @ 555,336: 13x24",
    "#87 @ 878,302: 16x15",
    "#88 @ 356,480: 21x22",
    "#89 @ 261,769: 16x10",
    "#90 @ 884,165: 27x10",
    "#91 @ 374,83: 20x11",
    "#92 @ 514,214: 17x18",
    "#93 @ 346,557: 21x16",
    "#94 @ 260,772: 27x20",
    "#95 @ 337,313: 25x21",
    "#96 @ 140,322: 17x10",
    "#97 @ 81,892: 24x20",
    "#98 @ 432,958: 10x23",
    "#99 @ 533,176: 22x10",
    "#100 @ 186,331: 14x20",
    "#101 @ 910,975: 11x11",
    "#102 @ 756,642: 24x22",
    "#103 @ 251,358: 14x21",
    "#104 @ 30,146: 18x26",
    "#105 @ 307,555: 14x12",
    "#106 @ 150,670: 13x20",
    "#107 @ 643,862: 17x27",
    "#108 @ 98,740: 29x13",
    "#109 @ 347,44: 11x14",
    "#110 @ 105,597: 11x27",
    "#111 @ 918,492: 15x19",
    "#112 @ 824,383: 20x17",
    "#113 @ 227,259: 21x17",
    "#114 @ 103,516: 5x10",
    "#115 @ 170,683: 13x14",
    "#116 @ 397,590: 22x24",
    "#117 @ 577,257: 21x25",
    "#118 @ 575,204: 13x13",
    "#119 @ 245,761: 16x16",
    "#120 @ 802,736: 13x19",
    "#121 @ 193,10: 26x16",
    "#122 @ 393,4: 25x28",
    "#123 @ 122,609: 18x19",
    "#124 @ 635,687: 12x11",
    "#125 @ 898,545: 25x15",
    "#126 @ 293,934: 17x15",
    "#127 @ 224,781: 27x22",
    "#128 @ 952,534: 17x15",
    "#129 @ 557,958: 11x12",
    "#130 @ 248,655: 13x20",
    "#131 @ 334,284: 21x18",
    "#132 @ 663,378: 25x25",
    "#133 @ 892,529: 27x24",
    "#134 @ 428,451: 21x18",
    "#135 @ 263,39: 22x14",
    "#136 @ 68,217: 12x29",
    "#137 @ 560,136: 15x13",
    "#138 @ 163,815: 29x18",
    "#139 @ 627,669: 20x12",
    "#140 @ 23,539: 16x13",
    "#141 @ 101,509: 19x25",
    "#142 @ 895,520: 12x26",
    "#143 @ 409,884: 27x12",
    "#144 @ 368,760: 11x20",
    "#145 @ 557,58: 28x10",
    "#146 @ 438,332: 12x21",
    "#147 @ 76,951: 28x20",
    "#148 @ 406,186: 22x27",
    "#149 @ 302,621: 19x29",
    "#150 @ 920,629: 17x13",
    "#151 @ 849,878: 11x28",
    "#152 @ 780,576: 20x28",
    "#153 @ 757,799: 26x23",
    "#154 @ 116,834: 29x16",
    "#155 @ 729,960: 23x11",
    "#156 @ 137,56: 20x23",
    "#157 @ 556,337: 13x18",
    "#158 @ 294,546: 29x20",
    "#159 @ 22,334: 28x14",
    "#160 @ 565,39: 14x16",
    "#161 @ 173,311: 12x18",
    "#162 @ 191,516: 15x17",
    "#163 @ 280,334: 29x25",
    "#164 @ 835,508: 10x15",
    "#165 @ 413,766: 23x10",
    "#166 @ 922,355: 16x22",
    "#167 @ 231,790: 13x16",
    "#168 @ 363,538: 16x23",
    "#169 @ 333,386: 25x16",
    "#170 @ 901,377: 13x19",
    "#171 @ 722,437: 18x25",
    "#172 @ 666,697: 26x22",
    "#173 @ 0,355: 10x10",
    "#174 @ 475,88: 23x12",
    "#175 @ 51,174: 21x29",
    "#176 @ 546,330: 21x25",
    "#177 @ 707,924: 15x11",
    "#178 @ 530,812: 22x14",
    "#179 @ 73,52: 20x25",
    "#180 @ 936,311: 13x17",
    "#181 @ 723,519: 28x11",
    "#182 @ 662,239: 17x19",
    "#183 @ 309,186: 23x23",
    "#184 @ 584,457: 26x22",
    "#185 @ 575,52: 19x19",
    "#186 @ 327,300: 19x24",
    "#187 @ 376,128: 12x21",
    "#188 @ 573,536: 8x7",
    "#189 @ 420,335: 12x29",
    "#190 @ 886,554: 12x10",
    "#191 @ 820,66: 10x22",
    "#192 @ 5,564: 25x27",
    "#193 @ 942,979: 29x17",
    "#194 @ 116,55: 27x11",
    "#195 @ 423,854: 20x16",
    "#196 @ 331,37: 19x24",
    "#197 @ 715,374: 29x13",
    "#198 @ 873,758: 10x29",
    "#199 @ 661,367: 20x22",
    "#200 @ 521,796: 26x20",
    "#201 @ 582,838: 15x26",
    "#202 @ 548,870: 10x19",
    "#203 @ 357,418: 21x11",
    "#204 @ 651,861: 28x11",
    "#205 @ 166,414: 18x15",
    "#206 @ 403,513: 16x13",
    "#207 @ 10,561: 23x19",
    "#208 @ 259,725: 27x13",
    "#209 @ 505,647: 13x14",
    "#210 @ 25,131: 28x15",
    "#211 @ 414,295: 28x16",
    "#212 @ 421,775: 19x14",
    "#213 @ 214,627: 12x19",
    "#214 @ 642,675: 24x26",
    "#215 @ 561,755: 22x10",
    "#216 @ 469,754: 23x15",
    "#217 @ 533,714: 21x21",
    "#218 @ 21,163: 13x12",
    "#219 @ 11,59: 12x21",
    "#220 @ 47,903: 10x20",
    "#221 @ 538,890: 12x24",
    "#222 @ 114,617: 23x29",
    "#223 @ 462,209: 16x12",
    "#224 @ 206,643: 17x29",
    "#225 @ 890,106: 24x10",
    "#226 @ 276,15: 16x12",
    "#227 @ 663,196: 22x26",
    "#228 @ 660,686: 17x28",
    "#229 @ 474,586: 10x26",
    "#230 @ 900,379: 12x23",
    "#231 @ 2,31: 12x12",
    "#232 @ 937,309: 24x29",
    "#233 @ 450,123: 16x19",
    "#234 @ 302,204: 29x16",
    "#235 @ 199,948: 29x12",
    "#236 @ 958,794: 29x19",
    "#237 @ 779,851: 18x23",
    "#238 @ 730,631: 18x29",
    "#239 @ 111,704: 18x24",
    "#240 @ 488,58: 25x12",
    "#241 @ 572,23: 26x22",
    "#242 @ 307,89: 12x22",
    "#243 @ 510,770: 24x15",
    "#244 @ 45,448: 11x28",
    "#245 @ 590,542: 18x17",
    "#246 @ 678,2: 23x12",
    "#247 @ 796,319: 11x16",
    "#248 @ 152,680: 26x25",
    "#249 @ 938,25: 19x21",
    "#250 @ 56,395: 23x22",
    "#251 @ 590,668: 18x24",
    "#252 @ 839,710: 29x14",
    "#253 @ 792,637: 24x20",
    "#254 @ 700,113: 26x14",
    "#255 @ 25,593: 11x23",
    "#256 @ 924,479: 28x25",
    "#257 @ 223,544: 24x20",
    "#258 @ 680,577: 18x18",
    "#259 @ 342,475: 23x29",
    "#260 @ 533,151: 17x13",
    "#261 @ 142,519: 22x20",
    "#262 @ 902,762: 15x28",
    "#263 @ 332,827: 13x23",
    "#264 @ 10,105: 19x25",
    "#265 @ 417,605: 17x19",
    "#266 @ 578,795: 10x12",
    "#267 @ 308,342: 12x21",
    "#268 @ 239,241: 27x24",
    "#269 @ 312,382: 10x3",
    "#270 @ 770,6: 13x20",
    "#271 @ 350,484: 18x22",
    "#272 @ 897,652: 14x19",
    "#273 @ 518,314: 17x12",
    "#274 @ 675,124: 9x12",
    "#275 @ 787,416: 28x17",
    "#276 @ 512,204: 18x19",
    "#277 @ 243,224: 16x18",
    "#278 @ 266,938: 6x3",
    "#279 @ 305,914: 22x22",
    "#280 @ 68,930: 22x10",
    "#281 @ 312,565: 25x10",
    "#282 @ 562,608: 16x19",
    "#283 @ 55,116: 17x10",
    "#284 @ 253,675: 19x16",
    "#285 @ 304,299: 19x15",
    "#286 @ 786,401: 26x20",
    "#287 @ 391,621: 21x25",
    "#288 @ 437,217: 19x18",
    "#289 @ 675,488: 25x28",
    "#290 @ 47,195: 10x26",
    "#291 @ 881,323: 25x29",
    "#292 @ 24,173: 25x27",
    "#293 @ 816,658: 22x21",
    "#294 @ 915,501: 29x18",
    "#295 @ 886,49: 12x23",
    "#296 @ 885,60: 10x15",
    "#297 @ 257,849: 26x28",
    "#298 @ 899,571: 29x15",
    "#299 @ 714,390: 15x11",
    "#300 @ 915,702: 14x27",
    "#301 @ 489,164: 10x6",
    "#302 @ 389,463: 16x10",
    "#303 @ 229,759: 28x11",
    "#304 @ 4,570: 17x17",
    "#305 @ 547,949: 28x24",
    "#306 @ 696,931: 17x16",
    "#307 @ 241,300: 18x16",
    "#308 @ 790,323: 15x17",
    "#309 @ 483,822: 17x17",
    "#310 @ 463,733: 21x22",
    "#311 @ 98,549: 20x10",
    "#312 @ 364,23: 13x12",
    "#313 @ 490,651: 18x15",
    "#314 @ 887,670: 19x19",
    "#315 @ 326,902: 10x19",
    "#316 @ 793,765: 17x19",
    "#317 @ 733,540: 17x10",
    "#318 @ 768,892: 28x20",
    "#319 @ 317,21: 29x10",
    "#320 @ 668,172: 13x27",
    "#321 @ 496,122: 12x4",
    "#322 @ 911,236: 12x12",
    "#323 @ 430,576: 25x15",
    "#324 @ 606,798: 27x25",
    "#325 @ 903,242: 21x15",
    "#326 @ 128,713: 14x10",
    "#327 @ 293,367: 25x15",
    "#328 @ 610,359: 13x23",
    "#329 @ 284,30: 22x16",
    "#330 @ 561,764: 17x20",
    "#331 @ 558,329: 11x17",
    "#332 @ 536,356: 23x27",
    "#333 @ 606,968: 26x19",
    "#334 @ 420,175: 15x24",
    "#335 @ 676,86: 29x20",
    "#336 @ 908,410: 17x16",
    "#337 @ 558,766: 12x28",
    "#338 @ 917,860: 23x20",
    "#339 @ 253,362: 8x11",
    "#340 @ 429,844: 14x27",
    "#341 @ 56,851: 10x23",
    "#342 @ 309,709: 22x18",
    "#343 @ 165,346: 25x21",
    "#344 @ 17,922: 23x14",
    "#345 @ 702,31: 16x21",
    "#346 @ 328,333: 19x27",
    "#347 @ 302,911: 17x18",
    "#348 @ 15,649: 12x19",
    "#349 @ 966,178: 24x21",
    "#350 @ 544,945: 27x17",
    "#351 @ 261,927: 28x20",
    "#352 @ 705,393: 28x12",
    "#353 @ 732,58: 17x26",
    "#354 @ 213,572: 20x26",
    "#355 @ 358,650: 16x29",
    "#356 @ 754,629: 10x14",
    "#357 @ 708,941: 10x21",
    "#358 @ 153,478: 19x18",
    "#359 @ 950,364: 29x16",
    "#360 @ 765,705: 12x22",
    "#361 @ 779,171: 20x11",
    "#362 @ 670,677: 14x26",
    "#363 @ 405,646: 27x17",
    "#364 @ 346,842: 25x25",
    "#365 @ 902,872: 22x23",
    "#366 @ 166,133: 16x10",
    "#367 @ 692,546: 24x26",
    "#368 @ 883,552: 23x10",
    "#369 @ 153,497: 12x16",
    "#370 @ 928,881: 15x28",
    "#371 @ 316,308: 19x22",
    "#372 @ 582,239: 28x23",
    "#373 @ 285,728: 16x24",
    "#374 @ 790,929: 17x15",
    "#375 @ 369,96: 14x21",
    "#376 @ 904,950: 19x18",
    "#377 @ 205,938: 13x13",
    "#378 @ 132,837: 13x16",
    "#379 @ 279,864: 14x14",
    "#380 @ 718,816: 29x23",
    "#381 @ 120,973: 29x17",
    "#382 @ 422,461: 10x15",
    "#383 @ 208,352: 23x26",
    "#384 @ 904,731: 18x23",
    "#385 @ 61,876: 26x27",
    "#386 @ 533,327: 27x22",
    "#387 @ 2,349: 20x13",
    "#388 @ 399,969: 24x23",
    "#389 @ 805,734: 13x15",
    "#390 @ 190,898: 24x13",
    "#391 @ 117,845: 19x18",
    "#392 @ 368,30: 16x19",
    "#393 @ 406,974: 13x4",
    "#394 @ 35,913: 10x29",
    "#395 @ 228,169: 11x28",
    "#396 @ 505,735: 21x12",
    "#397 @ 737,335: 10x16",
    "#398 @ 261,264: 28x15",
    "#399 @ 965,959: 15x25",
    "#400 @ 14,168: 25x23",
    "#401 @ 693,517: 14x13",
    "#402 @ 54,893: 13x16",
    "#403 @ 186,945: 26x14",
    "#404 @ 117,742: 21x19",
    "#405 @ 185,455: 11x14",
    "#406 @ 203,1: 20x11",
    "#407 @ 175,191: 27x18",
    "#408 @ 634,690: 19x21",
    "#409 @ 650,437: 27x15",
    "#410 @ 623,752: 15x24",
    "#411 @ 790,606: 16x24",
    "#412 @ 851,726: 19x12",
    "#413 @ 68,171: 25x23",
    "#414 @ 336,135: 19x11",
    "#415 @ 427,151: 10x28",
    "#416 @ 313,67: 10x12",
    "#417 @ 144,519: 23x11",
    "#418 @ 606,699: 27x28",
    "#419 @ 66,564: 17x24",
    "#420 @ 219,225: 28x29",
    "#421 @ 911,398: 11x17",
    "#422 @ 971,903: 28x27",
    "#423 @ 909,220: 20x20",
    "#424 @ 64,845: 29x18",
    "#425 @ 448,248: 23x26",
    "#426 @ 726,370: 15x14",
    "#427 @ 951,135: 24x20",
    "#428 @ 557,750: 24x23",
    "#429 @ 487,184: 26x21",
    "#430 @ 427,819: 27x18",
    "#431 @ 243,684: 16x12",
    "#432 @ 362,486: 27x10",
    "#433 @ 631,562: 22x24",
    "#434 @ 43,258: 16x21",
    "#435 @ 855,305: 21x11",
    "#436 @ 216,955: 29x11",
    "#437 @ 709,7: 21x27",
    "#438 @ 308,47: 16x11",
    "#439 @ 251,765: 16x10",
    "#440 @ 737,440: 16x20",
    "#441 @ 142,438: 22x18",
    "#442 @ 405,492: 17x23",
    "#443 @ 216,687: 28x12",
    "#444 @ 323,770: 20x17",
    "#445 @ 373,90: 28x26",
    "#446 @ 670,674: 10x17",
    "#447 @ 292,239: 5x5",
    "#448 @ 369,133: 17x18",
    "#449 @ 81,173: 13x14",
    "#450 @ 954,299: 29x17",
    "#451 @ 390,730: 21x10",
    "#452 @ 933,25: 10x12",
    "#453 @ 340,165: 14x13",
    "#454 @ 459,498: 10x13",
    "#455 @ 787,464: 10x24",
    "#456 @ 476,683: 29x26",
    "#457 @ 877,986: 12x12",
    "#458 @ 393,112: 27x27",
    "#459 @ 957,538: 16x22",
    "#460 @ 815,245: 12x11",
    "#461 @ 777,925: 29x13",
    "#462 @ 200,302: 24x14",
    "#463 @ 780,916: 19x16",
    "#464 @ 355,667: 20x11",
    "#465 @ 310,972: 17x17",
    "#466 @ 591,6: 12x25",
    "#467 @ 429,640: 10x19",
    "#468 @ 888,608: 29x26",
    "#469 @ 47,843: 15x13",
    "#470 @ 457,653: 17x21",
    "#471 @ 589,253: 24x29",
    "#472 @ 586,746: 15x20",
    "#473 @ 339,492: 19x21",
    "#474 @ 562,507: 19x23",
    "#475 @ 283,874: 25x19",
    "#476 @ 277,237: 25x10",
    "#477 @ 478,493: 13x17",
    "#478 @ 37,239: 15x11",
    "#479 @ 948,887: 25x24",
    "#480 @ 883,25: 15x21",
    "#481 @ 203,89: 17x14",
    "#482 @ 422,924: 25x20",
    "#483 @ 838,387: 7x3",
    "#484 @ 114,831: 26x25",
    "#485 @ 906,5: 20x17",
    "#486 @ 17,422: 5x8",
    "#487 @ 664,638: 14x13",
    "#488 @ 948,268: 11x12",
    "#489 @ 36,856: 29x10",
    "#490 @ 538,137: 12x26",
    "#491 @ 367,677: 20x22",
    "#492 @ 116,457: 29x11",
    "#493 @ 963,157: 23x22",
    "#494 @ 559,521: 11x13",
    "#495 @ 881,439: 15x24",
    "#496 @ 487,112: 27x25",
    "#497 @ 433,312: 23x23",
    "#498 @ 270,745: 16x13",
    "#499 @ 121,950: 17x21",
    "#500 @ 866,544: 24x13",
    "#501 @ 280,285: 13x23",
    "#502 @ 52,270: 20x29",
    "#503 @ 208,304: 6x8",
    "#504 @ 226,872: 28x14",
    "#505 @ 267,845: 13x13",
    "#506 @ 345,477: 23x20",
    "#507 @ 633,771: 25x27",
    "#508 @ 715,398: 27x21",
    "#509 @ 36,526: 28x28",
    "#510 @ 745,143: 13x28",
    "#511 @ 57,250: 10x29",
    "#512 @ 573,236: 25x12",
    "#513 @ 715,671: 12x13",
    "#514 @ 69,102: 4x21",
    "#515 @ 692,34: 22x10",
    "#516 @ 274,302: 16x29",
    "#517 @ 22,47: 25x18",
    "#518 @ 590,723: 28x17",
    "#519 @ 95,879: 18x16",
    "#520 @ 275,858: 11x13",
    "#521 @ 486,159: 25x16",
    "#522 @ 249,215: 12x15",
    "#523 @ 615,738: 16x18",
    "#524 @ 766,13: 19x12",
    "#525 @ 297,555: 18x20",
    "#526 @ 362,76: 16x18",
    "#527 @ 488,829: 10x22",
    "#528 @ 248,921: 22x24",
    "#529 @ 80,136: 18x15",
    "#530 @ 880,740: 13x12",
    "#531 @ 584,217: 18x23",
    "#532 @ 716,634: 7x12",
    "#533 @ 370,741: 23x21",
    "#534 @ 385,12: 16x16",
    "#535 @ 587,565: 23x29",
    "#536 @ 397,636: 16x28",
    "#537 @ 208,504: 14x20",
    "#538 @ 241,564: 15x12",
    "#539 @ 553,484: 18x11",
    "#540 @ 351,765: 28x26",
    "#541 @ 498,770: 3x12",
    "#542 @ 610,135: 14x29",
    "#543 @ 2,888: 24x16",
    "#544 @ 631,464: 16x16",
    "#545 @ 716,247: 15x27",
    "#546 @ 380,693: 18x21",
    "#547 @ 690,507: 11x17",
    "#548 @ 950,886: 28x17",
    "#549 @ 188,182: 12x16",
    "#550 @ 604,418: 20x22",
    "#551 @ 544,184: 27x13",
    "#552 @ 367,16: 23x13",
    "#553 @ 508,645: 21x26",
    "#554 @ 906,386: 13x16",
    "#555 @ 681,208: 17x17",
    "#556 @ 254,576: 12x14",
    "#557 @ 867,797: 17x18",
    "#558 @ 776,350: 9x4",
    "#559 @ 249,664: 11x29",
    "#560 @ 656,651: 21x29",
    "#561 @ 884,610: 25x25",
    "#562 @ 329,213: 13x27",
    "#563 @ 958,277: 20x25",
    "#564 @ 603,551: 12x13",
    "#565 @ 91,228: 21x14",
    "#566 @ 171,636: 19x25",
    "#567 @ 509,336: 14x17",
    "#568 @ 428,827: 26x28",
    "#569 @ 546,747: 20x17",
    "#570 @ 951,357: 17x15",
    "#571 @ 892,747: 15x27",
    "#572 @ 219,866: 21x16",
    "#573 @ 617,233: 20x22",
    "#574 @ 271,502: 15x11",
    "#575 @ 159,486: 17x17",
    "#576 @ 925,311: 26x10",
    "#577 @ 731,589: 10x19",
    "#578 @ 16,878: 21x17",
    "#579 @ 421,0: 11x27",
    "#580 @ 313,535: 20x21",
    "#581 @ 659,155: 19x24",
    "#582 @ 707,68: 29x27",
    "#583 @ 119,193: 25x19",
    "#584 @ 165,709: 23x18",
    "#585 @ 663,435: 25x12",
    "#586 @ 356,107: 28x10",
    "#587 @ 446,30: 22x17",
    "#588 @ 17,328: 28x24",
    "#589 @ 601,551: 25x16",
    "#590 @ 697,90: 12x21",
    "#591 @ 764,807: 23x22",
    "#592 @ 493,199: 22x14",
    "#593 @ 687,519: 18x26",
    "#594 @ 799,195: 16x11",
    "#595 @ 543,172: 22x26",
    "#596 @ 671,909: 24x10",
    "#597 @ 188,201: 21x24",
    "#598 @ 52,532: 24x25",
    "#599 @ 20,376: 10x29",
    "#600 @ 669,515: 25x12",
    "#601 @ 645,473: 15x20",
    "#602 @ 695,339: 11x26",
    "#603 @ 218,746: 16x16",
    "#604 @ 97,492: 11x26",
    "#605 @ 551,463: 14x27",
    "#606 @ 931,516: 25x28",
    "#607 @ 830,888: 26x14",
    "#608 @ 413,104: 24x25",
    "#609 @ 181,879: 17x20",
    "#610 @ 524,369: 14x29",
    "#611 @ 861,175: 23x19",
    "#612 @ 366,383: 20x24",
    "#613 @ 315,559: 11x11",
    "#614 @ 287,7: 23x21",
    "#615 @ 949,779: 12x21",
    "#616 @ 507,514: 21x21",
    "#617 @ 768,50: 29x11",
    "#618 @ 323,326: 12x28",
    "#619 @ 933,12: 24x17",
    "#620 @ 578,821: 24x29",
    "#621 @ 564,765: 14x22",
    "#622 @ 974,502: 11x16",
    "#623 @ 613,884: 19x28",
    "#624 @ 824,604: 11x13",
    "#625 @ 56,833: 11x21",
    "#626 @ 347,131: 13x29",
    "#627 @ 501,867: 28x19",
    "#628 @ 325,49: 22x24",
    "#629 @ 503,618: 17x21",
    "#630 @ 761,402: 12x22",
    "#631 @ 604,679: 15x19",
    "#632 @ 915,252: 19x24",
    "#633 @ 487,856: 29x15",
    "#634 @ 147,111: 21x23",
    "#635 @ 904,393: 28x27",
    "#636 @ 242,318: 22x19",
    "#637 @ 359,297: 16x19",
    "#638 @ 714,516: 23x20",
    "#639 @ 155,340: 24x10",
    "#640 @ 840,880: 17x25",
    "#641 @ 433,651: 13x26",
    "#642 @ 643,849: 23x14",
    "#643 @ 782,202: 29x18",
    "#644 @ 465,495: 26x15",
    "#645 @ 197,340: 28x15",
    "#646 @ 389,575: 25x29",
    "#647 @ 178,192: 27x27",
    "#648 @ 110,688: 16x28",
    "#649 @ 852,574: 15x11",
    "#650 @ 253,592: 14x10",
    "#651 @ 254,323: 28x20",
    "#652 @ 651,854: 21x15",
    "#653 @ 566,558: 19x27",
    "#654 @ 885,425: 15x25",
    "#655 @ 579,469: 3x4",
    "#656 @ 776,728: 22x23",
    "#657 @ 66,98: 21x29",
    "#658 @ 613,360: 16x16",
    "#659 @ 668,874: 19x16",
    "#660 @ 439,369: 11x19",
    "#661 @ 735,850: 25x21",
    "#662 @ 307,339: 12x20",
    "#663 @ 403,183: 19x12",
    "#664 @ 700,777: 12x16",
    "#665 @ 580,211: 28x17",
    "#666 @ 678,893: 15x14",
    "#667 @ 623,730: 16x11",
    "#668 @ 464,564: 23x25",
    "#669 @ 505,429: 11x23",
    "#670 @ 107,888: 13x28",
    "#671 @ 61,610: 28x22",
    "#672 @ 584,240: 27x23",
    "#673 @ 560,794: 24x23",
    "#674 @ 746,824: 18x18",
    "#675 @ 629,244: 14x24",
    "#676 @ 570,528: 18x23",
    "#677 @ 828,599: 28x26",
    "#678 @ 159,38: 13x10",
    "#679 @ 947,361: 10x19",
    "#680 @ 431,201: 15x22",
    "#681 @ 801,449: 27x11",
    "#682 @ 44,174: 12x22",
    "#683 @ 797,783: 15x24",
    "#684 @ 600,972: 22x12",
    "#685 @ 791,817: 29x15",
    "#686 @ 293,806: 20x26",
    "#687 @ 908,734: 19x22",
    "#688 @ 84,543: 12x11",
    "#689 @ 508,450: 15x17",
    "#690 @ 887,472: 21x15",
    "#691 @ 390,694: 14x12",
    "#692 @ 882,32: 16x13",
    "#693 @ 708,66: 19x22",
    "#694 @ 695,185: 22x20",
    "#695 @ 790,882: 24x18",
    "#696 @ 773,465: 20x12",
    "#697 @ 158,352: 22x23",
    "#698 @ 681,708: 18x13",
    "#699 @ 52,963: 11x29",
    "#700 @ 139,974: 11x21",
    "#701 @ 696,66: 26x27",
    "#702 @ 183,514: 11x13",
    "#703 @ 110,605: 18x20",
    "#704 @ 394,114: 23x26",
    "#705 @ 701,926: 19x12",
    "#706 @ 426,145: 18x24",
    "#707 @ 228,697: 25x13",
    "#708 @ 204,560: 15x16",
    "#709 @ 454,980: 14x20",
    "#710 @ 434,869: 19x16",
    "#711 @ 460,421: 21x18",
    "#712 @ 858,401: 25x22",
    "#713 @ 916,560: 13x27",
    "#714 @ 1,678: 28x28",
    "#715 @ 804,365: 24x21",
    "#716 @ 65,571: 28x16",
    "#717 @ 623,727: 16x29",
    "#718 @ 414,351: 11x15",
    "#719 @ 569,58: 25x14",
    "#720 @ 638,681: 13x11",
    "#721 @ 260,187: 26x20",
    "#722 @ 797,797: 11x26",
    "#723 @ 210,706: 25x11",
    "#724 @ 177,424: 29x23",
    "#725 @ 301,979: 16x19",
    "#726 @ 772,317: 19x17",
    "#727 @ 400,611: 19x26",
    "#728 @ 139,38: 21x10",
    "#729 @ 303,326: 27x16",
    "#730 @ 898,961: 11x17",
    "#731 @ 517,324: 17x15",
    "#732 @ 616,433: 12x22",
    "#733 @ 9,4: 14x25",
    "#734 @ 467,645: 15x15",
    "#735 @ 551,892: 10x27",
    "#736 @ 553,598: 14x16",
    "#737 @ 280,354: 19x22",
    "#738 @ 694,391: 24x22",
    "#739 @ 390,586: 13x12",
    "#740 @ 658,513: 20x11",
    "#741 @ 25,606: 15x14",
    "#742 @ 257,63: 26x21",
    "#743 @ 921,375: 18x14",
    "#744 @ 551,547: 16x27",
    "#745 @ 446,936: 26x11",
    "#746 @ 56,404: 24x24",
    "#747 @ 771,348: 22x10",
    "#748 @ 649,518: 23x26",
    "#749 @ 680,908: 27x21",
    "#750 @ 636,669: 13x23",
    "#751 @ 275,506: 25x17",
    "#752 @ 763,338: 24x19",
    "#753 @ 3,3: 28x28",
    "#754 @ 625,668: 11x22",
    "#755 @ 597,536: 23x11",
    "#756 @ 316,902: 16x16",
    "#757 @ 55,229: 22x21",
    "#758 @ 84,553: 14x17",
    "#759 @ 279,736: 12x20",
    "#760 @ 639,780: 4x6",
    "#761 @ 873,143: 12x25",
    "#762 @ 116,757: 18x26",
    "#763 @ 257,297: 24x27",
    "#764 @ 138,188: 10x13",
    "#765 @ 692,187: 10x23",
    "#766 @ 596,237: 10x12",
    "#767 @ 246,869: 21x20",
    "#768 @ 154,110: 25x28",
    "#769 @ 37,287: 23x26",
    "#770 @ 779,651: 22x14",
    "#771 @ 153,296: 21x17",
    "#772 @ 294,336: 7x19",
    "#773 @ 803,327: 11x11",
    "#774 @ 542,895: 14x19",
    "#775 @ 501,301: 25x19",
    "#776 @ 703,796: 28x19",
    "#777 @ 966,184: 15x27",
    "#778 @ 13,735: 11x28",
    "#779 @ 652,674: 22x23",
    "#780 @ 609,608: 20x20",
    "#781 @ 663,141: 21x23",
    "#782 @ 708,555: 10x19",
    "#783 @ 317,813: 25x25",
    "#784 @ 205,441: 17x12",
    "#785 @ 689,504: 13x25",
    "#786 @ 715,927: 10x14",
    "#787 @ 726,254: 11x13",
    "#788 @ 525,511: 27x17",
    "#789 @ 562,123: 10x28",
    "#790 @ 386,94: 11x18",
    "#791 @ 409,431: 24x17",
    "#792 @ 657,574: 22x14",
    "#793 @ 127,915: 16x22",
    "#794 @ 127,968: 25x13",
    "#795 @ 487,64: 22x15",
    "#796 @ 610,245: 28x16",
    "#797 @ 514,616: 18x21",
    "#798 @ 38,945: 19x21",
    "#799 @ 794,904: 20x14",
    "#800 @ 474,210: 20x17",
    "#801 @ 340,36: 16x16",
    "#802 @ 774,752: 29x11",
    "#803 @ 785,845: 15x20",
    "#804 @ 789,582: 10x25",
    "#805 @ 726,524: 21x20",
    "#806 @ 4,350: 27x27",
    "#807 @ 218,749: 15x29",
    "#808 @ 423,648: 27x10",
    "#809 @ 69,186: 21x10",
    "#810 @ 607,871: 16x19",
    "#811 @ 766,924: 19x21",
    "#812 @ 59,252: 4x21",
    "#813 @ 185,707: 23x29",
    "#814 @ 495,87: 18x18",
    "#815 @ 16,625: 19x25",
    "#816 @ 673,119: 17x26",
    "#817 @ 893,3: 14x24",
    "#818 @ 14,361: 22x19",
    "#819 @ 502,585: 18x28",
    "#820 @ 293,446: 27x18",
    "#821 @ 25,194: 25x11",
    "#822 @ 789,169: 14x11",
    "#823 @ 423,918: 29x18",
    "#824 @ 956,863: 28x25",
    "#825 @ 49,327: 11x16",
    "#826 @ 529,86: 21x18",
    "#827 @ 902,524: 23x24",
    "#828 @ 254,220: 16x19",
    "#829 @ 571,206: 23x21",
    "#830 @ 66,68: 10x17",
    "#831 @ 233,367: 23x28",
    "#832 @ 677,511: 25x17",
    "#833 @ 895,757: 7x9",
    "#834 @ 314,905: 21x29",
    "#835 @ 870,233: 13x12",
    "#836 @ 599,118: 25x22",
    "#837 @ 34,119: 10x22",
    "#838 @ 602,725: 10x23",
    "#839 @ 481,848: 20x12",
    "#840 @ 696,106: 18x12",
    "#841 @ 566,788: 22x26",
    "#842 @ 845,483: 26x23",
    "#843 @ 614,950: 11x24",
    "#844 @ 831,210: 21x28",
    "#845 @ 133,455: 12x12",
    "#846 @ 20,40: 29x24",
    "#847 @ 642,359: 24x22",
    "#848 @ 689,9: 14x15",
    "#849 @ 694,766: 10x24",
    "#850 @ 613,371: 17x20",
    "#851 @ 399,352: 21x23",
    "#852 @ 24,372: 16x20",
    "#853 @ 150,198: 19x26",
    "#854 @ 539,598: 29x19",
    "#855 @ 122,582: 25x27",
    "#856 @ 164,501: 12x21",
    "#857 @ 158,432: 20x19",
    "#858 @ 98,163: 11x22",
    "#859 @ 696,75: 26x24",
    "#860 @ 413,913: 14x11",
    "#861 @ 853,487: 27x20",
    "#862 @ 293,600: 21x26",
    "#863 @ 927,15: 10x18",
    "#864 @ 597,699: 10x10",
    "#865 @ 357,126: 22x13",
    "#866 @ 758,326: 25x17",
    "#867 @ 195,31: 17x17",
    "#868 @ 620,511: 19x20",
    "#869 @ 62,608: 29x26",
    "#870 @ 211,91: 13x10",
    "#871 @ 900,621: 13x18",
    "#872 @ 184,328: 15x16",
    "#873 @ 222,662: 28x20",
    "#874 @ 489,724: 27x24",
    "#875 @ 585,849: 10x13",
    "#876 @ 559,601: 25x16",
    "#877 @ 385,353: 21x15",
    "#878 @ 881,574: 20x25",
    "#879 @ 542,716: 27x18",
    "#880 @ 62,190: 10x12",
    "#881 @ 832,659: 19x29",
    "#882 @ 909,741: 27x20",
    "#883 @ 859,799: 26x17",
    "#884 @ 301,677: 24x23",
    "#885 @ 529,424: 19x16",
    "#886 @ 289,932: 18x28",
    "#887 @ 705,931: 17x10",
    "#888 @ 208,179: 23x24",
    "#889 @ 369,446: 10x16",
    "#890 @ 410,794: 3x5",
    "#891 @ 265,567: 19x24",
    "#892 @ 471,611: 16x20",
    "#893 @ 821,919: 25x15",
    "#894 @ 757,822: 24x18",
    "#895 @ 285,98: 12x10",
    "#896 @ 737,666: 10x19",
    "#897 @ 974,242: 16x29",
    "#898 @ 383,808: 13x25",
    "#899 @ 266,729: 15x4",
    "#900 @ 726,811: 10x17",
    "#901 @ 654,571: 19x27",
    "#902 @ 691,879: 17x18",
    "#903 @ 40,946: 28x13",
    "#904 @ 482,81: 19x11",
    "#905 @ 806,357: 19x26",
    "#906 @ 421,788: 22x11",
    "#907 @ 843,515: 14x21",
    "#908 @ 488,701: 23x24",
    "#909 @ 880,723: 11x23",
    "#910 @ 167,208: 29x26",
    "#911 @ 334,84: 21x25",
    "#912 @ 319,20: 13x18",
    "#913 @ 977,438: 20x16",
    "#914 @ 15,420: 10x15",
    "#915 @ 633,221: 13x20",
    "#916 @ 881,339: 10x17",
    "#917 @ 54,838: 20x23",
    "#918 @ 369,414: 16x21",
    "#919 @ 505,593: 23x29",
    "#920 @ 354,43: 18x3",
    "#921 @ 284,165: 29x29",
    "#922 @ 925,890: 22x21",
    "#923 @ 615,713: 16x22",
    "#924 @ 487,768: 22x24",
    "#925 @ 4,359: 28x22",
    "#926 @ 923,619: 13x18",
    "#927 @ 576,461: 10x19",
    "#928 @ 31,876: 14x13",
    "#929 @ 588,858: 18x10",
    "#930 @ 602,724: 14x18",
    "#931 @ 79,127: 26x29",
    "#932 @ 335,319: 25x11",
    "#933 @ 313,461: 17x22",
    "#934 @ 186,194: 16x23",
    "#935 @ 278,178: 16x12",
    "#936 @ 458,739: 14x27",
    "#937 @ 557,343: 17x24",
    "#938 @ 187,20: 21x15",
    "#939 @ 445,926: 12x14",
    "#940 @ 816,87: 14x10",
    "#941 @ 857,553: 17x23",
    "#942 @ 818,233: 16x20",
    "#943 @ 697,344: 6x8",
    "#944 @ 967,251: 17x10",
    "#945 @ 878,550: 21x28",
    "#946 @ 585,281: 28x19",
    "#947 @ 393,615: 18x26",
    "#948 @ 151,616: 11x10",
    "#949 @ 347,118: 14x24",
    "#950 @ 203,98: 20x16",
    "#951 @ 415,772: 28x24",
    "#952 @ 723,866: 14x20",
    "#953 @ 787,311: 10x20",
    "#954 @ 575,732: 18x17",
    "#955 @ 288,415: 11x17",
    "#956 @ 403,792: 20x12",
    "#957 @ 307,683: 28x21",
    "#958 @ 955,350: 18x15",
    "#959 @ 873,212: 15x22",
    "#960 @ 963,135: 28x19",
    "#961 @ 184,631: 14x15",
    "#962 @ 46,796: 19x23",
    "#963 @ 19,25: 19x18",
    "#964 @ 241,546: 13x26",
    "#965 @ 43,114: 19x20",
    "#966 @ 7,564: 22x12",
    "#967 @ 598,743: 10x17",
    "#968 @ 83,561: 15x16",
    "#969 @ 873,909: 5x10",
    "#970 @ 808,344: 21x10",
    "#971 @ 73,533: 16x24",
    "#972 @ 188,18: 15x17",
    "#973 @ 568,765: 19x12",
    "#974 @ 835,851: 20x13",
    "#975 @ 603,971: 17x18",
    "#976 @ 964,172: 20x15",
    "#977 @ 179,783: 5x7",
    "#978 @ 610,602: 18x16",
    "#979 @ 935,753: 17x24",
    "#980 @ 200,6: 26x14",
    "#981 @ 330,693: 23x26",
    "#982 @ 352,40: 23x10",
    "#983 @ 855,503: 18x14",
    "#984 @ 954,295: 16x13",
    "#985 @ 661,63: 19x19",
    "#986 @ 843,774: 18x28",
    "#987 @ 897,98: 13x23",
    "#988 @ 433,733: 25x23",
    "#989 @ 893,669: 10x12",
    "#990 @ 733,168: 14x26",
    "#991 @ 575,721: 13x15",
    "#992 @ 728,528: 14x16",
    "#993 @ 438,924: 11x25",
    "#994 @ 928,846: 17x21",
    "#995 @ 583,296: 15x23",
    "#996 @ 854,967: 18x15",
    "#997 @ 61,830: 25x23",
    "#998 @ 708,384: 11x14",
    "#999 @ 10,102: 15x17",
    "#1000 @ 712,951: 22x22",
    "#1001 @ 907,716: 13x21",
    "#1002 @ 237,219: 26x28",
    "#1003 @ 536,480: 25x21",
    "#1004 @ 418,165: 19x29",
    "#1005 @ 520,802: 17x22",
    "#1006 @ 416,652: 24x20",
    "#1007 @ 909,751: 13x16",
    "#1008 @ 878,281: 18x29",
    "#1009 @ 349,171: 19x17",
    "#1010 @ 971,157: 28x17",
    "#1011 @ 714,632: 15x19",
    "#1012 @ 756,786: 27x17",
    "#1013 @ 441,417: 17x14",
    "#1014 @ 10,36: 13x26",
    "#1015 @ 903,460: 14x22",
    "#1016 @ 614,215: 16x26",
    "#1017 @ 17,664: 20x14",
    "#1018 @ 127,199: 20x17",
    "#1019 @ 693,182: 27x13",
    "#1020 @ 296,714: 25x19",
    "#1021 @ 544,881: 13x29",
    "#1022 @ 405,502: 12x22",
    "#1023 @ 557,492: 10x25",
    "#1024 @ 246,83: 22x22",
    "#1025 @ 109,738: 27x26",
    "#1026 @ 458,503: 14x14",
    "#1027 @ 69,945: 25x19",
    "#1028 @ 967,854: 10x10",
    "#1029 @ 270,848: 16x24",
    "#1030 @ 430,952: 13x23",
    "#1031 @ 352,96: 17x22",
    "#1032 @ 315,68: 26x11",
    "#1033 @ 238,533: 25x24",
    "#1034 @ 449,690: 28x25",
    "#1035 @ 703,719: 21x13",
    "#1036 @ 836,383: 14x11",
    "#1037 @ 834,363: 22x21",
    "#1038 @ 367,398: 28x18",
    "#1039 @ 714,32: 26x19",
    "#1040 @ 80,836: 17x16",
    "#1041 @ 177,781: 11x12",
    "#1042 @ 842,700: 28x10",
    "#1043 @ 591,958: 28x16",
    "#1044 @ 190,972: 5x18",
    "#1045 @ 187,970: 15x24",
    "#1046 @ 674,225: 21x28",
    "#1047 @ 944,321: 23x23",
    "#1048 @ 671,633: 12x25",
    "#1049 @ 662,579: 14x25",
    "#1050 @ 597,597: 25x10",
    "#1051 @ 642,164: 23x17",
    "#1052 @ 646,558: 15x14",
    "#1053 @ 384,598: 17x28",
    "#1054 @ 627,435: 16x14",
    "#1055 @ 839,779: 11x16",
    "#1056 @ 901,634: 20x28",
    "#1057 @ 838,212: 23x14",
    "#1058 @ 228,598: 14x18",
    "#1059 @ 730,507: 11x25",
    "#1060 @ 306,200: 21x22",
    "#1061 @ 439,156: 29x12",
    "#1062 @ 941,101: 20x19",
    "#1063 @ 412,226: 19x20",
    "#1064 @ 90,901: 22x22",
    "#1065 @ 70,172: 16x15",
    "#1066 @ 857,777: 19x28",
    "#1067 @ 826,893: 16x12",
    "#1068 @ 88,494: 24x18",
    "#1069 @ 620,803: 12x13",
    "#1070 @ 182,458: 29x14",
    "#1071 @ 349,72: 24x13",
    "#1072 @ 681,880: 17x22",
    "#1073 @ 545,550: 26x26",
    "#1074 @ 509,694: 14x14",
    "#1075 @ 203,27: 14x28",
    "#1076 @ 881,547: 12x26",
    "#1077 @ 877,983: 13x10",
    "#1078 @ 94,184: 10x29",
    "#1079 @ 846,390: 23x23",
    "#1080 @ 947,325: 18x21",
    "#1081 @ 52,775: 29x22",
    "#1082 @ 222,594: 15x29",
    "#1083 @ 549,80: 12x17",
    "#1084 @ 534,867: 17x17",
    "#1085 @ 907,46: 11x29",
    "#1086 @ 442,256: 11x11",
    "#1087 @ 933,601: 13x19",
    "#1088 @ 717,636: 18x25",
    "#1089 @ 504,56: 17x22",
    "#1090 @ 301,933: 17x14",
    "#1091 @ 518,813: 13x25",
    "#1092 @ 717,425: 12x25",
    "#1093 @ 860,978: 19x21",
    "#1094 @ 850,479: 12x20",
    "#1095 @ 522,771: 12x13",
    "#1096 @ 593,16: 12x10",
    "#1097 @ 116,297: 21x18",
    "#1098 @ 363,461: 22x13",
    "#1099 @ 735,343: 23x10",
    "#1100 @ 956,194: 16x20",
    "#1101 @ 146,178: 26x26",
    "#1102 @ 628,730: 11x29",
    "#1103 @ 131,308: 25x14",
    "#1104 @ 8,633: 21x29",
    "#1105 @ 301,188: 13x19",
    "#1106 @ 373,813: 23x18",
    "#1107 @ 122,721: 12x10",
    "#1108 @ 841,692: 27x11",
    "#1109 @ 642,155: 17x10",
    "#1110 @ 897,863: 17x18",
    "#1111 @ 849,197: 27x15",
    "#1112 @ 311,553: 11x28",
    "#1113 @ 460,814: 16x16",
    "#1114 @ 813,889: 15x18",
    "#1115 @ 422,209: 23x18",
    "#1116 @ 775,143: 11x24",
    "#1117 @ 289,400: 16x27",
    "#1118 @ 322,381: 21x11",
    "#1119 @ 932,308: 17x13",
    "#1120 @ 178,729: 21x21",
    "#1121 @ 384,802: 16x19",
    "#1122 @ 703,651: 11x24",
    "#1123 @ 664,77: 11x13",
    "#1124 @ 759,825: 10x12",
    "#1125 @ 205,185: 28x11",
    "#1126 @ 152,675: 5x10",
    "#1127 @ 233,810: 11x17",
    "#1128 @ 25,245: 27x17",
    "#1129 @ 329,6: 25x16",
    "#1130 @ 82,241: 23x20",
    "#1131 @ 743,123: 26x16",
    "#1132 @ 191,160: 25x28",
    "#1133 @ 603,729: 21x16",
    "#1134 @ 16,742: 3x10",
    "#1135 @ 582,759: 14x29",
    "#1136 @ 424,262: 29x22",
    "#1137 @ 157,607: 25x17",
    "#1138 @ 822,460: 29x25",
    "#1139 @ 949,514: 26x11",
    "#1140 @ 683,393: 17x27",
    "#1141 @ 268,849: 27x16",
    "#1142 @ 823,899: 21x15",
    "#1143 @ 703,22: 17x16",
    "#1144 @ 723,382: 24x25",
    "#1145 @ 545,758: 21x14",
    "#1146 @ 449,978: 25x19",
    "#1147 @ 386,730: 14x26",
    "#1148 @ 335,359: 27x11",
    "#1149 @ 630,3: 28x25",
    "#1150 @ 55,277: 23x25",
    "#1151 @ 56,897: 27x19",
    "#1152 @ 418,909: 27x12",
    "#1153 @ 194,828: 25x26",
    "#1154 @ 135,829: 12x21",
    "#1155 @ 186,14: 22x16",
    "#1156 @ 314,538: 13x22",
    "#1157 @ 145,163: 23x29",
    "#1158 @ 901,654: 18x27",
    "#1159 @ 180,680: 17x17",
    "#1160 @ 848,858: 29x27",
    "#1161 @ 163,295: 22x12",
    "#1162 @ 929,609: 20x26",
    "#1163 @ 658,185: 27x12",
    "#1164 @ 885,964: 28x17",
    "#1165 @ 964,339: 18x29",
    "#1166 @ 405,448: 23x26",
    "#1167 @ 144,846: 13x11",
    "#1168 @ 881,548: 12x15",
    "#1169 @ 457,705: 11x25",
    "#1170 @ 150,122: 22x15",
    "#1171 @ 86,586: 25x20",
    "#1172 @ 276,102: 12x11",
    "#1173 @ 320,904: 7x10",
    "#1174 @ 577,806: 22x16",
    "#1175 @ 805,335: 21x12",
    "#1176 @ 852,806: 6x10",
    "#1177 @ 431,904: 14x29",
    "#1178 @ 503,68: 24x16",
    "#1179 @ 742,432: 11x28",
    "#1180 @ 493,661: 18x27",
    "#1181 @ 443,561: 10x20",
    "#1182 @ 161,672: 26x26",
    "#1183 @ 50,533: 28x22",
    "#1184 @ 790,331: 25x20",
    "#1185 @ 461,496: 10x16",
    "#1186 @ 42,337: 11x14",
    "#1187 @ 249,364: 20x13",
    "#1188 @ 915,734: 21x23",
    "#1189 @ 69,143: 18x15",
    "#1190 @ 626,206: 22x14",
    "#1191 @ 299,928: 29x19",
    "#1192 @ 406,209: 15x24",
    "#1193 @ 220,90: 20x19",
    "#1194 @ 308,373: 19x19",
    "#1195 @ 497,835: 19x18",
    "#1196 @ 442,327: 26x12",
    "#1197 @ 123,920: 14x25",
    "#1198 @ 945,654: 16x15",
    "#1199 @ 78,587: 11x17",
    "#1200 @ 176,713: 19x17",
    "#1201 @ 348,848: 13x20",
    "#1202 @ 855,707: 21x20",
    "#1203 @ 765,118: 16x15",
    "#1204 @ 84,542: 15x20",
    "#1205 @ 249,755: 12x19",
    "#1206 @ 891,40: 21x25",
    "#1207 @ 689,386: 28x14",
    "#1208 @ 270,937: 12x25",
    "#1209 @ 263,841: 24x24",
    "#1210 @ 604,244: 22x12",
    "#1211 @ 251,552: 19x17",
    "#1212 @ 930,381: 29x13",
    "#1213 @ 414,523: 10x19",
    "#1214 @ 450,813: 17x19",
    "#1215 @ 709,102: 19x19",
    "#1216 @ 108,601: 25x29",
    "#1217 @ 420,780: 11x11",
    "#1218 @ 376,801: 25x29",
    "#1219 @ 44,950: 23x15",
    "#1220 @ 118,456: 25x17",
    "#1221 @ 447,42: 25x19",
    "#1222 @ 333,29: 19x13",
    "#1223 @ 598,552: 16x15",
    "#1224 @ 390,355: 11x9",
    "#1225 @ 362,293: 13x10",
    "#1226 @ 251,876: 23x10",
    "#1227 @ 559,811: 18x16",
    "#1228 @ 844,735: 22x26",
    "#1229 @ 973,204: 12x10",
    "#1230 @ 370,73: 16x13",
    "#1231 @ 124,241: 15x24",
    "#1232 @ 150,324: 3x5",
    "#1233 @ 942,359: 13x10",
    "#1234 @ 111,355: 17x24",
    "#1235 @ 157,275: 19x17",
    "#1236 @ 192,440: 14x24",
    "#1237 @ 265,191: 28x16",
    "#1238 @ 607,426: 23x28",
    "#1239 @ 782,718: 13x24",
    "#1240 @ 90,138: 24x18",
    "#1241 @ 861,670: 24x22",
    "#1242 @ 429,446: 15x15",
    "#1243 @ 620,971: 17x21",
    "#1244 @ 778,811: 22x17",
    "#1245 @ 175,164: 13x18",
    "#1246 @ 448,141: 20x13",
    "#1247 @ 578,848: 13x24",
    "#1248 @ 61,392: 25x19",
    "#1249 @ 217,811: 28x28",
    "#1250 @ 548,186: 11x7",
    "#1251 @ 725,652: 16x27",
    "#1252 @ 448,719: 24x28",
    "#1253 @ 124,254: 29x10",
    "#1254 @ 334,224: 21x19",
    "#1255 @ 466,506: 22x15",
    "#1256 @ 266,598: 12x21",
    "#1257 @ 744,42: 28x25",
    "#1258 @ 700,645: 23x13",
    "#1259 @ 716,31: 17x29",
    "#1260 @ 974,418: 14x24",
    "#1261 @ 476,333: 24x19",
    "#1262 @ 158,148: 29x26",
    "#1263 @ 685,497: 29x19",
    "#1264 @ 876,672: 22x12",
    "#1265 @ 963,100: 27x19",
    "#1266 @ 725,583: 26x11",
    "#1267 @ 266,954: 13x11",
    "#1268 @ 140,620: 27x15",
    "#1269 @ 465,317: 18x20",
    "#1270 @ 942,85: 16x22",
    "#1271 @ 774,594: 18x10",
    "#1272 @ 115,370: 10x26",
    "#1273 @ 680,178: 24x11",
    "#1274 @ 589,387: 29x13",
    "#1275 @ 545,585: 26x26",
    "#1276 @ 416,310: 10x28",
    "#1277 @ 226,100: 10x25",
    "#1278 @ 444,500: 20x13",
    "#1279 @ 390,461: 10x26",
    "#1280 @ 941,891: 22x14",
    "#1281 @ 978,278: 11x26",
    "#1282 @ 183,317: 15x13",
    "#1283 @ 930,361: 13x24",
    "#1284 @ 134,204: 12x17",
    "#1285 @ 710,389: 6x5",
    "#1286 @ 197,501: 22x13",
    "#1287 @ 284,773: 28x24",
    "#1288 @ 130,449: 22x26",
    "#1289 @ 310,489: 14x27",
    "#1290 @ 528,540: 20x11",
    "#1291 @ 958,541: 25x11",
    "#1292 @ 705,734: 8x13",
    "#1293 @ 344,745: 26x23",
    "#1294 @ 556,603: 20x25",
    "#1295 @ 335,781: 23x19",
    "#1296 @ 887,584: 26x12",
    "#1297 @ 760,386: 10x19",
    "#1298 @ 486,627: 22x23",
    "#1299 @ 180,820: 22x15",
    "#1300 @ 141,206: 11x15",
    "#1301 @ 833,914: 16x25",
    "#1302 @ 536,596: 17x21",
    "#1303 @ 18,870: 25x16",
    "#1304 @ 550,445: 13x24",
    "#1305 @ 163,257: 22x24",
    "#1306 @ 932,665: 19x11",
    "#1307 @ 42,472: 13x14"
  )
}

