object App {
  def main(args: Array[String]): Unit = {
    var stroka = "Hello, Scala!"
    println(stroka.reverse)
    stroka = stroka.map(_.toLower)
    stroka = stroka.replace("!", " ")
    stroka = stroka + "and goodbye python!"
    println(stroka)
    println("Оклад сотрудника " + oklad(5000, 45, 20).toString)
    val oklad_otdel = List(100, 150, 200, 80, 120, 75)
    val sr_znach = oklad_otdel.sum/oklad_otdel.size
    val otkcl = 100 * oklad(5000, 45, 20) / sr_znach - 100
    if (otkcl < 0) println("Оклад отклоняется в меньшую сторону на " + otkcl.toString + "%") else println("Оклад отклоняется в большую сторону на " + otkcl.toString + "%")
    val new_zp = additionally(oklad(5000, 45, 20), -20)
    var new_oklad_otdel = oklad_otdel :+ new_zp
    println("Максимальная зарплата " + new_oklad_otdel.min.toString)
    println("Максимальная зарплата " + new_oklad_otdel.max.toString)
    new_oklad_otdel = new_oklad_otdel :+ 350 :+ 90
    new_oklad_otdel = new_oklad_otdel.sorted

    var index_list = List[Int]()
    var index = 0
    for (n <- new_oklad_otdel) {
      if (n > 130 && index == 0) {
        index_list = index_list :+ 130
        index = 1
      }
      index_list = index_list :+ n
    }
    new_oklad_otdel = index_list
    println(new_oklad_otdel)

    vilka(new_oklad_otdel, 130)

    new_oklad_otdel = for (e <- new_oklad_otdel) yield (e * 1.07).round.toInt
    println(new_oklad_otdel)
  }

  def vilka(list_elem: List[Int], v: Int): Unit = {
    for (n <- list_elem){
      if (n <= v) println("Номер middle сотрудника " + list_elem.indexOf(n).toString)
    }
  }

  def insert_elem(list_elem: List[Int], elem: Int):Unit={
    var index_list = List[Int]()
    var index = 0
    for (n <- list_elem){
      if (n > elem && index == 0) {
        index_list = index_list :+ elem
        index = 1
        }
      index_list = index_list :+ n
    }
  }


  def oklad(year_income: Float, premium: Float, compensation: Float):Int =
    (compensation + (year_income * (100 - premium) / 100 / 12) * (1 - 0.13) * (1 - 0.22) * (1 - 0.051)).round.toInt

  def additionally(zp: Double, summa: Float):Int =
    (zp + summa).round.toInt


}
