import scala.annotation.tailrec

object TestList {
    @tailrec
    def last[A](lst: List[A]): A = lst match {
        case h :: Nil => h
        case _ :: t => last(t)
        case _ => throw new NoSuchElementException
    }

    @tailrec
    def lastButOne[A](lst: List[A]): A = lst match {
        case a :: _ :: Nil => a
        case _ :: tail => lastButOne(tail)
        case _ => throw new NoSuchElementException
    }

    def kthElement[A](lst: List[A], k: Int): A = {
        @tailrec
        def helper(cl: List[A], i: Int): A = {
            if (cl.isEmpty) throw new NoSuchElementException
            else if (i == k) cl.head
            else helper(cl.tail, i + 1)
        }

        helper(lst, 1)
    }

    def numberOfElements[A](lst: List[A]): Int = {
        @tailrec
        def helper(cl: List[A], s: Int): Int = {
            if (cl.isEmpty) s
            else helper(cl.tail, s + 1)
        }

        helper(lst, 0)
    }
    def reverse[A](lst: List[A]): List[A] = {
        @tailrec
        def helper(cl: List[A], r: List[A]): List[A] = cl match {
            case h :: _ => helper(cl.tail, h :: r)
            case _ => r
        }

        helper(lst, List.empty[A])
    }

    def palindrome[A](lst: List[A]): Boolean = {
        val mid = numberOfElements(lst) / 2
        if (mid == 0) return true
        @tailrec
        def helper(rev: List[A], n: List[A], count: Int): Boolean = {
            if (count > mid) true
            else if (rev.head == n.head) helper(rev.tail, n.tail, count + 1)
            else false
        }

        val rl = reverse(lst)
        helper(rl, lst, 1)
    }

    def flatten1(lst: List[Any]): List[Any] = {
        def unwrap(ls: List[Any]): List[Any] = ls match {
            case (ls1: List[_]) :: tail => unwrap(ls1) ::: unwrap(tail)
            case x :: _ => x :: unwrap(ls.tail)
            case Nil => Nil
        }
        unwrap(lst)
    }
    def flatten(lst: List[Any]):List[Any] = lst flatMap {
        case l:List[_] => flatten(l)
        case d => List(d)
    }
    def  compress[A](lst: List[A]) : List[A] =  lst match {
        case Nil => Nil
        case h :: tail => h :: compress(tail.dropWhile( _ == h))
    }
    def compresstail[A](lst:List[A]) : List[A] = {
        @tailrec
        def helper(ls:List[A],acc:List[A]):List[A] = ls match {
            case Nil => acc
            case h :: tail => helper(tail.dropWhile(_ == h), h :: acc)
        }
        helper(lst,List.empty)
    }
    def pack[A](lst:List[A]): List[List[A]] = {
        if(lst.isEmpty) List(List())
        else {
            val s = lst.takeWhile( _ == lst.head)
            val d = lst.dropWhile(_ == lst.head)
            if (d.isEmpty) List(s)
            else s :: pack(d)
        }
    }
    def runLength[A](lst:List[A]) : List[(Int,A)] = {
        if(lst.isEmpty) List()
        else {
            val h = lst.head
            val d = lst.dropWhile(_ == h)
            (lst.length - d.length ,h) :: runLength(d)
        }
    }
    def runLengthT[A](lst:List[A]) : List[(Int,A)] = {
        @tailrec
        def helper[A](ls:List[A],acc:List[(Int,A)]): List[(Int,A)] = {
            if(ls.isEmpty) acc.reverse
            else {
                val h = ls.head
                val d = ls.dropWhile(_ == h)
                helper(d, (ls.length- d.length,h) :: acc)
            }
        }
        helper(lst,List())
    }
    def runLengthF[A](lst:List[A]) : List[(Int,A)] = {
        lst.foldRight(List[(Int,A)]()) {
            (a,acc) => {
                if (acc.isEmpty) (1,a) :: acc
                else {
                    val h = acc.head
                    if(h._2 == a) (h._1+1,a) :: acc.tail
                    else (1,a) :: acc
                }
            }
        }
    }
    def runLengthFL[A](lst:List[A]) : List[(Int,A)] = {
        lst.foldLeft(List[(Int,A)]()) {
            (acc,a) => {
                if (acc.isEmpty) (1,a) :: acc
                else {
                    val h = acc.head
                    if(h._2 == a) (h._1+1,a) :: acc.tail
                    else (1,a) :: acc
                }
            }
        }.reverse
    }

    def main(args: Array[String]): Unit = {
        var l = last(List(1,2,3,4,5))
        println("last element of list is " + l)
        l = lastButOne(List(1,2,3,4,5))
        println("last but one element of list is " + l)
        l = kthElement(List(1,2,3,4,5),3)
        println("kth element of list is " + l)
        l = numberOfElements(List(1,2,3,4,5))
        println("numbe of elements in list is " + l)
        val r = reverse(List(1,2,3,4,5))
        println("reversed list is " + r)
        var b = palindrome(List(1,2,3,1,1))
        println("is list palindrome " + b)
        b = palindrome(List(1,2,3,2,1))
        println("is list palindrome " + b)
        var f = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
        println("flattend list is " + f)
        f = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("compress " + f)
        f = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("pack" + f)
        f = runLength(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("runLength " + f)
        f = runLengthT(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("runLengthT " + f)
        f = runLengthF(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("runLengthF " + f)
        f = runLengthFL(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("runLengthF " + f)
    }
}
