
object MyIteratee{
  sealed trait Input[+E] {
  }

  object Input {
    case class El[+E](e: E) extends Input[E]
    case object EOF extends Input[Nothing]
  }

  trait Iteratee[E, A] {
    def fold[B](done:(A, Input[E]) => B,
		cont:(Input[E] => Iteratee[E,A]) => B): B

    def invoke(in:Input[E]): Iteratee[E,A] =
      fold( (a,e) => Done(a,e),
	   k => k(in))

    def run():A =
      fold( (a, _) => a,
	   k => k(Input.EOF).fold( (a1, _) => a1,
				  _ => sys.error("diverging iteratee after Input.EOF")))
  }

  trait Enumerator[E]{
    def apply[A](i: Iteratee[E, A]): Iteratee[E, A]
    def |>>[A](i: Iteratee[E, A]): Iteratee[E, A] = apply(i)
  }

  object Enumerator{
    def apply[E](in: E*): Enumerator[E] = new Enumerator[E] {
      def apply[A](i: Iteratee[E, A]): Iteratee[E, A] = enumerate(in, i)
    }

    private def enumerate[E, A]: (Seq[E], Iteratee[E, A]) => Iteratee[E, A] = { (l, i) =>
      l.foldLeft(i)((it, e) => it.invoke(Input.El(e)))
    }
  }

  object Cont{
    def apply[E, A](k: Input[E] => Iteratee[E, A]): Iteratee[E, A] = new Iteratee[E, A] {
      def fold[B](done: (A, Input[E]) => B,
		  cont: (Input[E] => Iteratee[E, A]) => B
		  ): B = cont(k)
    }
  }

  object Done {

    def apply[E, A](a: A, e: Input[E]): Iteratee[E, A] = new Iteratee[E, A] {
      def fold[B](done: (A, Input[E]) => B,
		  cont: (Input[E] => Iteratee[E, A]) => B): B = done(a, e)
    }
  }

}

object Main extends App{
  import MyIteratee._

  val it:Iteratee[Int,Int] = {
    def step(state:Int)(in:Input[Int]):Iteratee[Int,Int] = in match{
      case Input.El(e) => {println(e); Cont(i => step(state + e)(i))}
      case Input.EOF => { Done(state, Input.EOF)}
    }
    Cont(i => step(0)(i))
  }

  println ((Enumerator(1,2,3) |>> it).run())
}
