object FS {
  //input should be something like :
  // prog "name1 name2 name3 name4" "othername1 othername2 othername3"
  def main(args: Array[String]): Unit = {
    if (args.length < 1  ) {
      println("need to supply at least one function name")
      return
    }
    //these are vim color tags (see :highlights in vim), add more if need more functions
    val funcColors = List("Comment","Constant","Identifier","Statement","PreProc","Type","airline_c_red","airline_c_bold")
    val bolderColors: Seq[String] = List("airline_y","airline_z","airline_warning","CtrlPwhite","airline_tabtype")

    val funcs = args(0).split(" ")
    val fz = funcColors zip funcs
    val out1 = (fz.map({case (f,c) => s"syn keyword $f $c"})).mkString("\n")
    val result = if(args.size > 1)
    {
      val out2 = ({bolderColors zip args(1).split(" ")}.map({case (f,c) => s"syn keyword $f $c"})).mkString("\n")
      out1 + "\n" + out2
    }
    else out1
    println(result)
  }
}
