val revList: List[List[Int]] => List[List[Int]] = 
    lists => if (lists == Nil) Nil else {
        val x::xs = lists
        def revInt (ls: List[Int]): List[Int] = {
            ls match {
                case Nil => Nil
                case (f :: t) => revInt(t):::List(f)
            }
        }
        revList(xs):::List(revInt(x))
    }

val res = revList(List(List(1, 2), List(2, 3), List(3, 4), List(4, 5, 6)))
