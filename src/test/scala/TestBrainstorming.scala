object TestBrainstorming extends App {

  //getRandomIntFromZeroToMaxExceptInSeq should return None if ExceptSeq contains all possible values
  assert(
    Brainstorming.getRandomIntFromZeroUntilMaxExceptInList(10, List(0,1,2,3,4,5,6,7,8,9)) == None
  )
  //getRandomIntFromZeroToMaxExceptInSeq should return 4 if ExceptSeq contains all other possible values
  assert(
    Brainstorming.getRandomIntFromZeroUntilMaxExceptInList(10, List(0,1,2,3,5,6,7,8,9)) == Some(4)
  )

  //getRandomIntFromZeroToMaxExceptInSeq should return a random value except those defined in Seq
  assert(
    List(4,5,6) contains Brainstorming.getRandomIntFromZeroUntilMaxExceptInList(10, List(0,1,2,3,7,8,9)).get
  )


  //getNextTurn should create the next turn with unique pairs
  val result = Brainstorming.getNextTurn(Seq(Seq(1, 3, 0, 2)))

  result match {
    case None => println("error")
    case Some(r) => assert(
      r == Vector(0, 2, 1, 3) ||
      r == Vector(2, 0, 3, 1) ||
      r == Vector(3, 1, 2, 0)
    )
  }




}
