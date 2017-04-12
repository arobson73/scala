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
    //would be easy to make this tailrec
    def runLengthMod(lst:List[Any]) : List[Any] = {
        if(lst.isEmpty) List()
        else {
            val h = lst.head
            val d = lst.dropWhile(_ == h)
            val df = lst.length - d.length
            if(df > 1) (df ,h) :: runLengthMod(d)
            else h :: runLengthMod(d)
        }

    }
    def decode[A](lst:List[(Int,A)]) : List[A] = {
        lst.flatMap{case (n,c) => List.fill(n)(c)}
    }

    def duplicate[A](lst:List[A]): List[A] = {
        @tailrec
        def helper(ls:List[A],acc:List[A]): List[A] = {
            if(ls.isEmpty) acc.reverse
            else {
                val h = ls.head
                val d = ls.dropWhile(_ == h)
                val len = ls.length - d.length
                helper(d, List.fill(len*2)(h) ::: acc)
            }
        }
        helper(lst,List())
    }
    def duplicate2[A](lst:List[A]):List[A] = lst flatMap(d => List(d,d))
    def duplicateN[A](n: Int, lst:List[A]):List[A] = lst flatMap(d => {
        List.fill(n)(d)
    })
    //drop every nth element from the list
    def drop[A](lst:List[A], n:Int):List[A] = {
        @tailrec
        def helper(ls:List[A],count: Int, acc:List[A]):List[A] = {
            if(ls.isEmpty) acc.reverse
            else
            {
                if(count == 1) helper(ls.tail,n,acc)
                else helper(ls.tail,count -1,ls.head :: acc)
            }
        }
        helper(lst,n,List.empty)
    }
    def split[A](lst:List[A], n:Int): (List[A],List[A]) = {
        @tailrec
        def helper(ls:List[A],c:Int, acc:List[A]): (List[A], List[A]) = {
            if (ls.isEmpty || c == 0 ) (acc,ls)
            else helper(ls.tail,c-1,ls.head :: acc)
        }
        if(n >= lst.length) (lst,List[A]())
        else helper(lst,n, List.empty)
    }
    def slice[A](lst:List[A],s:Int,e:Int) : List[A] = {
        @tailrec
        def helper(ls: List[A], from:Int, to:Int,acc:List[A]): List[A] = {
            if(ls.isEmpty) acc
            else {
                if(from != 1) helper(ls.tail,from-1,to-1,acc)
                else if(from == 1 && to != 1) helper(ls.tail,from,to-1,ls.head :: acc)
                else ls.head :: acc
            }
        }
        helper(lst,s,e,List.empty).reverse
    }
    def rotate[A](r: Int, lst: List[A]): List[A] = {
        val len = lst.length
        val d = r % len
        if (d == 0 || lst.isEmpty) return lst
        // could of used take / drop here instead
        if (d > 0) slice(lst,len-d+1,len) ::: slice(lst,1,len-d)
        else slice(lst,(-d)+1,len) ::: slice(lst,1,(-d))
    }
    def removeKth[A](lst:List[A],k:Int) : (List[A],A) = {
        @tailrec
        def helper(ls:List[A], c:Int, acc:List[A]):(List[A],A) = {
            if(c == 1) (acc.reverse ::: ls.tail,ls.head)
            else helper(ls.tail,c-1, ls.head :: acc)
        }
        if (lst.isEmpty || k > lst.length) throw new NoSuchElementException
        else helper(lst,k,List.empty)
    }
    //could use splitAt here instead
    def insertKth[A](lst:List[A],k:Int,elem:A) : List[A] = {
        @tailrec
        def helper(ls:List[A], c:Int, acc:List[A]):List[A] = {
            if (c == 1) (elem :: acc).reverse ::: ls
            else helper(ls.tail,c-1,ls.head :: acc)
        }
        if(lst.isEmpty) elem :: lst
        else if (k > lst.length) lst ::: List(elem)
        else helper(lst,k,List.empty)
    }
    def insertKth2[A](lst:List[A], k:Int, elem:A):List[A] = lst.splitAt(k) match {
        case (a,b) => a ::: (elem :: b)
    }
    //could use a fold here also
    def range(from:Int, to:Int) :List[Int] = {
        @tailrec
        def helper(f:Int, t:Int, acc:List[Int]): List[Int] = {
            if(f == t) (f::acc).reverse
            else helper(f+1,t,f::acc)
        }
        if(from < to) helper(from,to,List.empty)
        else helper(to,from,List.empty)
    }
    def extract[A](n:Int,lst:List[A]):List[A] = {
        val r = new scala.util.Random(4356)
        @tailrec
        def helper(ls:List[A],count:Int,acc:List[A]):List[A] = {
            if(count == 0) acc
            else if(ls.length == 1) ls.head :: acc
            else {
                val pos = r.nextInt(ls.length-1) + 1
                val (nl,i) = removeKth(ls,pos)
                helper(nl,count-1,i :: acc)
            }
        }
        helper(lst,n,List.empty)
    }
    def lotto(n:Int,s:Int): List[Int] =
    {
        val r = range(1,s)
        extract(n,r)
    }
    def randPerm[A](lst:List[A]): List[A] = extract(lst.length,lst)

    def powerset[A](lst: List[A]): List[List[A]] = {
        def pwr(lst: List[A], acc: List[List[A]]): List[List[A]] = lst match {
            case Nil => acc
            case x :: xs => pwr(xs, acc ::: (acc map (x :: _)))
        }
        pwr(lst,Nil::Nil)
    }
    def combinationsK[A](lst:List[A],k:Int): List[List[A]] = {
        val ps = powerset(lst)
        ps.filter(ls => ls.length == k)
    }
    //note sublist is the whole list
    def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
        ls match {
            case Nil => Nil
            case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
        }

    def combinations[A](n: Int, ls: List[A]): List[List[A]] =
        if (n == 0) List(Nil)
        else flatMapSublists(ls) { sl =>
            combinations(n - 1, sl.tail) map {sl.head :: _}
        }
    def group3[A](ls: List[A]): List[List[List[A]]] =
        for {
            a <- combinations(2, ls)
            noA = ls.filterNot(x => a.contains(x))
            b <- combinations(3, noA)
        } yield List(a, b, noA.filterNot(x => b.contains(x)))

    def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
        case Nil     => List(Nil)
        case n :: ns => combinations(n, ls) flatMap { c =>
            group(ns, ls.filterNot(x => c.contains(x))) map {c :: _}
        }
    }
    //insert into a list of list  based on its size
    def insert[A](lst:List[List[A]], i:List[A]): List[List[A]] = {
        if (lst.isEmpty || i.length <= lst.head.length) i :: lst
        else lst.head :: insert(lst.tail,i)
    }
    def isort[A](lst:List[List[A]]) : List[List[A]] =
    {
        if (lst.isEmpty) Nil
        else insert(isort(lst.tail),lst.head)
    }
    // note here we create a Map, first get the length of each sub list, sort them
    //in ascending order, then get the runLength of those so we have a list of (frequencies , length)
    //then swap the order to be (length,frequency), then initialise the immutable Map.
    //the :_* is use to tell it the number of args is variable

    def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val freqs = Map(runLengthT(ls map { _.length } sorted ) map { _.swap }:_*)
    ls sortWith { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
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
        println("runLengthFL " + f)
        f = runLengthMod(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("runLengthMod " + f)
        f = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
        println("decode " + f)
        f = duplicate(List('a, 'b, 'c, 'c, 'd))
        println("duplicate " + f)
        f = duplicate2(List('a, 'b, 'c, 'c, 'd))
        println("duplicate2 " + f)
        f = duplicateN(3,List('a, 'b, 'c, 'c, 'd))
        println("duplicateN " + f)
        f = drop(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),3)
        println("drop 3" + f)
        val tt = split(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),3)
        println("split 3" + tt)
        f = slice(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),3,7)
        println("slice 3 7 " + f)
        f = rotate(3, List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("rotate 3  " + f)
        f = rotate(-3, List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("rotate -3  " + f)
        f = rotate(1, List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("rotate 1  " + f)
        f = rotate(-1, List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("rotate -1  " + f)
        val (ls,g) = removeKth(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),1)
        println("remove Kth  " + ls + " " + g)
        val (ls2,g2) = removeKth(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),6)
        println("remove Kth  " + ls2 + " " + g2)
        val (ls3,g3) = removeKth(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),14)
        println("remove Kth  " + ls3 + " " + g3)
        f = insertKth(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),1,'z)
        println("insert Kth  " + f)
        f = insertKth(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),100,'z)
        println("insert Kth  " + f)
        f = insertKth(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),5,'z)
        println("insert Kth  " + f)
        f = range(2,7)
        println("range 2 7 " + f)
        f = range(-22,-7)
        println("range -22 -7 " + f)
        f = extract(3,List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        println("extract 3 " + f)
        f = extract(1,List('a))
        println("extract 1 " + f)
        f = lotto(6,49)
        println("lotto " + f)
        f = randPerm(List('a,'b,'c,'d,'e,'f))
        println("randPerm " + f)
        val ps = powerset(List('a,'b,'c,'d))
        println("powerset " + ps)
        var comb = combinationsK(List('a,'b,'c,'d),2)
        println("combinations " + comb)
        comb = combinations(2,List('a,'b,'c,'d))
        println("combinations 2 " + comb)
        comb = combinations(3,List('a,'b,'c,'d,'e))
        println("combinations 3 " + comb)
        val gr3 = group3(List(1,2,3,4,5,6,7,8,9))
       // println("group3 " + gr3) this is a big lits
        println("group3 number of possibilities = " + gr3.length)
        val sl = isort(List(List(1,2,3,5),List(3,2),List(4),List(4,5,6)))
        println("sorted list of list = " + sl)
        //sort by frequency of occurance
        val ll = lsortFreq(List(List(1,2),List(2),List(3,4,5),List(4),List(4,8),List(8),List(3),List(4,5)))
        println("sorted by freq of occurance " + ll)
    }
}
