object Liste { 
    
    def insertionSort(list : List[Int]) = {

        def insert(list : List[Int], x : Int) : List[Int] = {
            list match {
                case Nil => List(x)
                case h::t => if (x>=h) x::h::t
                       else h::insert(t, x)
            }
        }

        list match {
            case Nil => Nil
            case h::t => insert(t, h)
        }
    }

    def quickSort(l : List[Int]) : List[Int] = {
        if (l.isEmpty || l.tail.isEmpty) return l
        var l1l2 = partition(l)
        quickSort(l1l2._1) ::: l.head :: quickSort(l1l2._2) 
    }

    def partition(l : List[Int]) : (List[Int], List[Int]) = {
        var pivot = l.head
        var lt : List[Int] =  Nil
        var gt : List[Int] =  Nil
        var list = l drop 1
        var h = list.head

        while(!list.isEmpty) {
            if(h < pivot)
                lt = h::lt 
            else 
                gt = h::gt
            list = list drop 1
            if (!list.isEmpty)
                h = list.head
        }

        (lt, gt)            
    }

    def mergeSort(l : List[Int]) : List[Int] = {

        def merge(l1 : List[Int], l2 : List[Int]) : List[Int] = {
            (l1, l2) match {
                case (Nil, l2) => l2
                case (l1, Nil) => l1
                case (h1::t1, h2::t2) =>  if(h1<h2) return h1::merge(t1, h2::t2) else h2::merge(h1::t1, t2)
            }
        }

        if (l == Nil || l.tail == Nil) l
        else {
            var k = l.length / 2
            var l1 = l take k
            var l2 = l drop k
            merge(mergeSort(l1), mergeSort(l2))
        }
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        var list = List(2,4,1,78,6,1,3,5)
        println(list.mkString(","))
        val list2 = Liste.quickSort(list)
        println(list2.mkString(","))
        println(list.head)
    }
}