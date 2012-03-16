
object MyIteratee{
  sealed trait Input[E] {
  }

  object Input {
    case class El[E](e: E) extends Input[E]
  }

  trait Iteratee[E, A] {
    def invoke(in:Input[E]): Iteratee[E,A]
  }
}

object Main extends App{
  import MyIteratee._

  val l = List(1,2,3).map(Input.El(_))

  val it:Iteratee[Int,Int] = new Iteratee[Int,Int]{
    def step(state:Int)(in:Input[Int]):Iteratee[Int,Int] = in match{
      case Input.El(e) => {println(e);
			   new Iteratee[Int, Int]{
			     def invoke(in:Input[Int]):Iteratee[Int,Int] = 
			       step(state + e)(in)
			   }
			 }
    }

    def invoke(in:Input[Int]):Iteratee[Int,Int] = 
      step(0)(in)
  }
    
  l.foldLeft(it)((res, e) => res.invoke(e))
}
