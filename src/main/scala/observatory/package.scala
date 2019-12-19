package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  // Testing tables

  final val TABLE1 ="""60 255 255 255
            |32 255	0	0
            |12	255	255	0
            |0	0	255	255
            |-15	0	0	255
            |-27	255	0	255
            |-50	33	0	107
            |-60	0	0	0
            |"""

  final val TABLE2 =
    """1.0 255 0 0
      |2.0 0 0 255
      |3.0 0 255 0
      |"""

  final val TABLE3 =
    """-1.0 255 0 0
      |0.0 0 0 255
      |"""

  final val TABLE4 =
    """-1.0 255 0 0
      |1.0 0 0 255
      |"""

  final val TABLE5 =
    """-1.0 255 0 0
      |-19.0 2 3 4
      |1.0 0 0 255
      |"""

  final val TABLE6 =
    """1.0 255 0 0
      |4.0 0 0 255
      |"""

  final val TABLE7 =
    """-3.0 255 0 0
      |1.0 0 0 255
      |"""

}
