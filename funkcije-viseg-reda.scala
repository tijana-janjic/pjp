object FunkcijeVisegReda{
    def map[T,R](f:(T => R), list:List[T]) : List[R] = {
        for(x <- list)
            yield f(x)
    }

    def filter[T](f: T => Boolean, list : List[T]) : List[T] = {
        for(x <- list if f(x))
            yield x
    }

    def foldl[T](f: (T, T) => T, poc: T, list : List[T]) : T = {
        var res : T = poc
        for(i <- 0 until list.length)
            res = f(res, list(i))
        res
    }

    def foldr[T](f: (T, T) => T, poc: T, list : List[T]) : T = {
        l match {
            case Nil => poc
            case x :: xs => f(x, foldr(f, poc, list))
        }
    }

    def main(args: Array[String]): Unit = {
        val f = (x: Int) => x * x
        println(map(f, List(1,2,3,4)))

        val f2 = (x: String) => x.toString > "Kako"
        println(filter(f2, List("ana", "sara", "lara", "Ana", "zaklina", "Bisa")))
        
        val f3 = (x: String, y: String) => x + y
        println(foldl(f3, "", List("ana", "sara", "lara", "Ana", "zaklina", "Bisa")))
    }
}