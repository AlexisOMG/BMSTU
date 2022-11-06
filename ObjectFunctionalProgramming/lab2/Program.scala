class Applicant(xname: String, xfirstEx: Int, xsecondEx: Int, xthirdEx: Int, xoriginal: Boolean) {
    val name = xname
    val firstEx = xfirstEx
    val secondEx = xsecondEx
    val thirdEx = xthirdEx
    val original = xoriginal
    val sum = firstEx + secondEx + thirdEx

    override def toString (): String = {
        xname + " " + firstEx.toString() + " " + secondEx.toString() + " " + thirdEx.toString() + " " + original.toString()
    }
}

class Applicants(newSet: List[Applicant]) {
    val setAb = newSet

    def this(xname: String, xfirstEx: Int, xsecondEx: Int, xthirdEx: Int, xoriginal: Boolean) = {
        this(List(new Applicant(xname, xfirstEx, xsecondEx, xthirdEx, xoriginal)))
    }

    def + (sa: Applicants): Applicants = new Applicants(setAb:::sa.setAb)

    def / (filt: Int): Applicants = new Applicants(setAb.filter(a => a.sum > filt))

    def unary_! : Applicants = new Applicants(setAb.filter(a => a.original == true))

    override def toString (): String = setAb.map[String](a => a.toString()).mkString(", ")
}



object Program {
  def main(args: Array[String]) = {
    val test1 = new Applicants("Alexis", 100, 100, 100, false)
    val test2 = new Applicants("Alex", 0, 0, 0, true)
    val union = test1 + test2
    println(test1)
    println(test2)
    println(union)
    println(union/100)
    println(!union)
  }
}
