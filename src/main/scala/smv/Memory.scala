package smv

// trait of memory related modules
sealed trait Memory {
  val dataType: Type

  // name of the current module
  def typeName: String
  // declaration of the current module
  def declaration: String
}

// memory reader
case class Reader(dataType: Type, latency: Int) extends Memory {
  override def typeName: String = Reader.typeName(dataType, latency)

  override def declaration: String = {
    val latRegDef = (0 until latency).map {
      i => s"_lat$i: ${dataType.serialize};"
    }.mkString("\n    ")
    val dataAssign = if (latency == 0) {
      "data := READ(mem, addr);"
    } else {
      (Seq(s"data := _lat${latency - 1};",
           "next(_lat0) := READ(mem, addr);") ++
       (1 until latency).map {
         i => s"next(_lat$i) := _lat${i - 1};"
       }).mkString("\n    ")
    }

    s"""MODULE $typeName(awidth, mem)
       |  VAR
       |    en: boolean;
       |    addr: unsigned word[awidth];
       |    data: ${dataType.serialize};
       |    $latRegDef
       |
       |  ASSIGN
       |    $dataAssign
     """.stripMargin
  }
}

object Reader {
  def typeName(dataType: Type, latency: Int): String =
    s""""$$Memory-Reader$latency${dataType.shortName}""""
}

// memory writer
case class Writer(dataType: Type, latency: Int) extends Memory {
  override def typeName: String = Writer.typeName(dataType, latency)

  override def declaration: String = {
    val latRegDef = (0 until latency).flatMap {
      i => Seq(s"_en_lat$i: boolean;",
               s"_addr_lat$i: unsigned word[awidth];",
               s"_data_lat$i: ${dataType.serialize};")
    }.mkString("\n    ")
    val dataAssign = if (latency == 0) {
      "mem := WRITE(mem, addr, en & mask ? data : READ(mem, addr));"
    } else {
      val last = latency - 1
      (Seq(s"mem := WRITE(mem, _addr_lat$last, _en_lat$last ? " ++
            s"_data_lat$last : READ(mem, _addr_lat$last));",
           "next(_en_lat0) := en & mask;",
           "next(_addr_lat0) := addr;",
           "next(_data_lat0) := data;") ++
       (1 until latency).flatMap {
         i => Seq(s"next(_en_lat$i) := _en_lat${i - 1};",
                  s"next(_addr_lat$i) := _addr_lat${i - 1};",
                  s"next(_data_lat$i) := _data_lat${i - 1};")
       }).mkString("\n    ")
    }

    s"""MODULE $typeName(awidth, mem)
       |  VAR
       |    en: boolean;
       |    addr: unsigned word[awidth];
       |    mask: boolean;
       |    data: ${dataType.serialize};
       |    $latRegDef
       |
       |  ASSIGN
       |    $dataAssign
     """.stripMargin
  }
}

object Writer {
  def typeName(dataType: Type, latency: Int): String =
    s""""$$Memory-Writer$latency${dataType.shortName}""""
}

// memory module
case class MemoryModule(dataType: Type, writeLat: Int, readLat: Int,
                        reader: String, writer: String) extends Memory {
  override def typeName: String =
    s""""$$Memory-$reader$readLat-$writer$writeLat-${dataType.shortName}""""

  override def declaration: String = {
    s"""MODULE $typeName(awidth)
       |  VAR
       |    mem: array word[awidth] of ${dataType.serialize};
       |    $reader: ${Reader.typeName(dataType, readLat)}(awidth, mem);
       |    $writer: ${Writer.typeName(dataType, writeLat)}(awidth, mem);
     """
  }
}
