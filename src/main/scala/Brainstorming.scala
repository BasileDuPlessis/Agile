import scala.util.{Try, Random}

/*
Each Post-IT must have n notes given by n different person
 */
object Brainstorming extends App {

  val people = Seq(
    "people A",
    "people B",
    "people C",
    "people D",
    "people E",
    "people F",
    "people G",
    "people H",
    "people I",
    "people J",
    "people K",
    "people L"
  )

  //Get max name length to align printing
  val maxLength = people.maxBy(_.length).length

  //First column is randomly built
  val peopleMatrix:Seq[Seq[Int]] = Seq(
    Seq(Random.shuffle((0 until people.length).toList):_*)
  )

  //Build a 3 times matrix (3 notes by Post-IT)
  buildMatrix(peopleMatrix, 3) match {
    case None => println("An error occurs, please try again...")
    case Some(s) => {
      for (j <- 0 until s(0).length) yield {
        for ( i <- 0 until s.length; p = people(s(i)(j)) ) yield " " * (maxLength - p.length) + p
      } mkString " - > "
    } foreach println
  }

  /*
  Build matrix by adding n turn to the first column
   */
  def buildMatrix(sourceMatrix: Seq[Seq[Int]], turnNumber: Int): Option[Seq[Seq[Int]]] = {
    if (sourceMatrix.length <= turnNumber) {
      Try {getNextTurn(sourceMatrix)} getOrElse None match {
        case None => None
        case Some(s) => buildMatrix(sourceMatrix :+ s, turnNumber)
      }
    } else {
      Some(sourceMatrix)
    }
  }

  /*
  From a matrix get next turn
   */
  def getNextTurn(sourceMatrix: Seq[Seq[Int]], result: Map[Int, Int] = Map()): Option[Seq[Int]] = {
    //Source matrix should contains 1 column
    require(sourceMatrix.length > 0)

    val length = sourceMatrix(0).length

    //Get next available key
    val key = getRandomIntFromZeroUntilMaxExceptInList(length, result.keys.toList)

    key match {
      //Return result as a Seq of values
      case None => Some(result.toSeq.sortBy(_._1).map(_._2).toIndexedSeq)
      //Create a pair for next available key
      case Some(k) =>
        //get line
        val line = (0 until sourceMatrix.length).map(sourceMatrix(_)(k))

        //get value except from line and column
        val value = getRandomIntFromZeroUntilMaxExceptInList(length, result.values.toList ++ line)

        value match {
          case None => None
          case Some(v) =>
            //add pair
            val valuePair = sourceMatrix.last(k)
            val keyPair = sourceMatrix.last.indexOf(v)

            keyPair match {
              case -1 => None
              case _ => getNextTurn(sourceMatrix, result ++ Map(k -> v, keyPair -> valuePair))
            }


      }
    }

  }

  /*
  Get a random value from 0 until max except from a given Seq
   */
  def getRandomIntFromZeroUntilMaxExceptInList(max: Int, except: List[Int]): Option[Int] = {
    (0 until max).diff(except) match {
      case Vector() => None
      case l:Vector[Int] => Some(l(Random.nextInt(l.size)))
    }
  }
}