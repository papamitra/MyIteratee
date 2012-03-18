
object MyIteratee{
  sealed trait Input[+E] {
  }

  object Input {
    case class El[+E](e: E) extends Input[E]
    case object EOF extends Input[Nothing]
  }

  trait Iteratee[E, A] {
    def invoke(in:Input[E]): Iteratee[E,A]
    def run(): A
  }
}

object Main extends App{
  import MyIteratee._

  val it:Iteratee[Int,Int] = new Iteratee[Int,Int]{
    def step(state:Int)(in:Input[Int]):Iteratee[Int,Int] = in match{
      case Input.El(e) => {println(e);
			   new Iteratee[Int, Int]{
			     def invoke(in:Input[Int]):Iteratee[Int,Int] = 
			       step(state + e)(in)
			     def run() = this.invoke(Input.EOF).run()
			   }
			 }
      case Input.EOF => {
			   new Iteratee[Int, Int]{
			     def invoke(in:Input[Int]):Iteratee[Int,Int] = 
			       sys.error("diverging iteratee after Input.EOF")
			     def run() = state
			   }
      }
    }

    def invoke(in:Input[Int]):Iteratee[Int,Int] = 
      step(0)(in)

    def run() = this.invoke(Input.EOF).run()
  }

  val l = List(1,2,3).map(Input.El(_))
  println ((l.foldLeft(it)((res, e) => res.invoke(e))).run())
}
