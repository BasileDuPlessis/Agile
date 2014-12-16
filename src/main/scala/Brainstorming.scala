import scala.util.{Random}

/*
Distribute turn for an Agile brainstorming exercice
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

  val turnNumber = 4

  val peopleMatrix:Seq[Seq[Int]] = Seq(
    Seq(Random.shuffle((0 until people.length).toList):_*)
  )

  buildMatrix(peopleMatrix) match {
    case None => println("Try again")
    case Some(s) => for (j <- 0 until s(0).length) {
      println( (for (i <- 0 until s.length) yield people(s(i)(j))) mkString (" -> ") )
    }
  }

  /*
  Build matrix by adding n turn to the first column
   */
  def buildMatrix(sourceMatrix: Seq[Seq[Int]]): Option[Seq[Seq[Int]]] = {
    if (sourceMatrix.length <= turnNumber) {
      getNextTurn(sourceMatrix) match {
        case None => None
        case Some(s) => buildMatrix(sourceMatrix :+ s)
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
      case None => {
        Some(result.toSeq.sortBy(_._1).map(_._2).toIndexedSeq)
      }
      case Some(k) => {

        //get line
        val line = (0 until sourceMatrix.length).map(sourceMatrix(_)(k))

        //get value except from line and column
        val value = getRandomIntFromZeroUntilMaxExceptInList(length, result.values.toList ++ line)

        value match {
          case None => None
          case Some(v) => {
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