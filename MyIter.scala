
object MyIteratee{
  sealed trait Input[E] {
  }

  object Input {
    case class El[E](e: E) extends Input[E]
  }
  
  def step(state:Int)(in:Input[Int]):Int = in match{
    case Input.El(e) => state + e
  } 
}

object Main extends App{
  import MyIteratee._

  val l = List(1,2,3).map(Input.El(_))

  println(l.foldLeft(0)((res, e) => step(res)(e)))
}
