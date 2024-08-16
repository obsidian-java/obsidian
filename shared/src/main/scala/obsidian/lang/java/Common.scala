package obsidian.lang.java

object Common {
  /** 
   *  update or insert the pair of key and value in a list of key-value pairs.
   * */
  def upsert[K,A](m:List[(K,A)], p:(K,A)):List[(K,A)] = {
    val keys = m.map(_._1)
    if (keys.contains(p._1)) {
      m.map( i => i match {
        case (k,v) if k == p._1 => p
        case (k,v)              => (k,v)
      })
    } else {
      m ++ List(p)
    }
  }

  def lookup[K,A](m:List[(K,A)], key:K):Option[A] = m match {
    case Nil => None
    case ((k,v)::l) if k == key => Some(v)
    case ((k,v)::l)             => lookup(l,key)
  }


  def allSame[E](m:List[E]):Boolean = m match {
    case Nil => true
    case (x::xs) => xs.forall(_ == x)
  }

}