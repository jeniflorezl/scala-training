object MatchTest1 {
    def matchTest(x: Int) :String = x match {
      case 1 => "one"
      case 2 => "two"
      case _ => "many"
    }
}
