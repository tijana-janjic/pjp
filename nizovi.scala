object Test{
    def main(args: Array[String]): Unit = {
        var arr = Array(2,4,1,78,6,1,3,5)
        println(arr.mkString(","))
        Nizovi.quickSort(arr)
        println(arr.mkString(","))
        val x = args(0).toInt
        val y = Nizovi.binSearchRec(arr, x)

        println(y)

        println()
        println(x)
        println(arr(y))
    }
}

object Nizovi{

    // insertion sort
    def insertionSort(arr : Array[Int]) = {
        for(i <- 1 until arr.length) {
            for(j <- i-1 to 0 by -1 if arr(j) > arr(j+1)) {
                swap(arr, j, j+1)
            }
        }    
    } 

    // selection sort
    def selectionSort(arr : Array[Int]) = {
        for(i <- 0 until arr.length) {
            var min = arr(i)
            var minIndex = i
            for(j <- i until arr.length) {
                if (arr(j) < min) {
                    min = arr(j)
                    minIndex = j
                }
            }
            if(minIndex!=i) swap(arr, i, minIndex)
        } }

    // bubble sort
    def bubbleSort(arr: Array[Int]) = {
        var sorted = false
        do {
            sorted = true
            for(j <- 0 until arr.length-1) {
                if(arr(j) > arr(j+1)){
                    swap(arr, j, j+1)
                    sorted = false
                }
            }
        } while (!sorted) }

    
    // quick sort
    def quickSort(arr: Array[Int]): Unit = {
        quickSort(arr, 0, arr.length-1) }
    def quickSort(arr: Array[Int], l: Int, h: Int): Unit = { 
        if(l<h) { 
            var p = partition(arr, l, h)
            quickSort(arr, l, p)
            quickSort(arr, p+1, h)  
        }
    }
    def partition(arr: Array[Int], l: Int, h: Int) = {
        val pivot = arr(l)
        var i = l + 1
        var j = h
        while (i <= j) {
            while (i <= h && arr(i) < pivot) i += 1
            while (j >= l+1 && arr(j) > pivot) j -= 1
            if (i <= j) {
                swap(arr, i, j)
                i += 1
                j -= 1
            }
        }
        swap(arr, l, j)
        j
    }

    def swap(arr : Array[Int], i : Int, j : Int) = {
        var temp = arr(j)
        arr(j) = arr(i)
        arr(i) = temp
    }
    
    
    /* Sekvencijalno (linearno) pretraživanje niza*/
    def searchLinear(arr: Array[Int], x: Int) : Int = {
        for(i <- 0 until arr.length) {
            if(arr(i) == x) return i
        }
        -1
    }
    
    // Binarno pretraživanje sortiranog niza
    def binSearch(arr: Array[Int], x: Int): Int = {
        var l = 0
        var r = arr.length - 1
        while (l <= r) {
            val mid = (l + r) / 2
            if (x == arr(mid)) return mid
            else if (x < arr(mid)) r = mid - 1
            else l = mid + 1
        }
        -1
    }

    def binSearchRec(arr: Array[Int], x: Int): Int = {
    // ugnjezdena funkcija
        def search(l: Int, r: Int): Int = {
            if (l <= r) {
                val mid = (l + r) / 2
                if (x == arr(mid)) mid
                else if (x < arr(mid)) search(l, mid - 1)
                else search(mid + 1, r)
            } else -1
        }
        search(0, arr.length - 1)
    }
}




