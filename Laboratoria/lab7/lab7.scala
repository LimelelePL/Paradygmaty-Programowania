object lab7 extends App {
  
  def lwybierz [A] (lxs: LazyList[A], n : Int, m:Int) : LazyList[A] = {

    def getMth (lxs: LazyList[A], m : Int): LazyList[A] = {
        (lxs,m) match
            case (LazyList(), _) => LazyList()
            case (_,0) => lxs
            case (h#::rest, _) => getMth(rest, m-1)
    }
    
    def helper [A] (lxs: LazyList[A], idx: Int) : LazyList[A] = {
            lxs match
                case LazyList() => LazyList() 
                case h #:: rest =>
                     if idx % n == 0 then h #:: helper (rest, idx + 1)
                     else helper (rest, idx + 1)
        }
        helper(getMth(lxs,m-1), 0)
  }

  println(lwybierz(LazyList(5,6,3,2,1),2,1).force.toString())
}
