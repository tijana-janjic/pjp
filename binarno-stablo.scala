sealed abstract class IntBST
case object Empty extends IntBST
case class Node(v: Int, l: IntBST, r: IntBST) extends IntBST

def insert(t: IntBST, info: Int): IntBST =
    t match {
        case Empty => Node(info, Empty, Empty)
        case Node(v, l, r) =>
            if (info < v) Node(v, insert(l, info), r)
            else Node(v, l, insert(r, info))
    }

def search(t: IntBST, info: Int): Boolean =
    t match {
        case Empty => false
        case Node(v, l, r) =>
            if (v == info) true
            else if (info < v) search(l, info)
            else search(r, info)
    }

def inOrder(t: IntBST): String
    t match {
        case Empty => " "
        case Node(v, l, r)  = inOrder(l) + " " + v + " " + inOrder(r)
    }
