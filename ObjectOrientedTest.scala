import java.util
import scala.collection.JavaConversions._
import scala.collection.immutable.Stream.Empty
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by synerzip on 22/9/15.
 */
object ObjectOrientedTest
{
  //type [Database] >: mutable.MutableList[Database]
  val examquestions :scala.collection.mutable.MutableList[Database] = mutable.MutableList[Database]()
  val totalobj :scala.collection.mutable.MutableList[Database] = mutable.MutableList[Database]()

  case class Database(qno:Int, examc:String , typec:String, question:String, answer:List[String],rightChIndex :Int)
  {
    override def toString = "Question number -> "+qno+"\n"+"exam code -> "+examc+"\n"+"type code -> "+typec+"\n"+"question-> "+question+"\n"+"Choices -> "+(answer mkString " | ")+"\n"+"Correct choice index -> "+rightChIndex+"\n"
  }

  def main(args: Array[String])
  {

    addData() //adds records to MAIN DATABASE

    println("Enter EXAM CODE TO GET CORRESPONDING RECORD")
    getData(Console.readLine().toLowerCase()) //Filter by Exam CODE => By Exam DIFFICULTY => build final EXAM QUESTIONS

    conductExam()
    /*val index =examquestions.size
    var count = 0

    println("Want to start exam?(y/n)")
    if(Console.readChar() == 'y')
    do{
      println(s"  RECORD $count  ")
      println(examquestions(count))
        count = count +1
      println("GO TO NEXT RECORD??(y/n)")

    }while(Console.readChar() == 'y' & count < index)
    else println("EXAM TERMINATED")*/



  }


  def addData() =
  {
    print("HOW MANY RECORDS?")
    val limit = Console.readInt()
    var i = 1;
    while(i <= limit)
    {
      println("ENTER QUESTION  NUMBER")
      val qno = Console.readInt()
      println("Enter exam code")
      val ecode = Console.readLine().toLowerCase()
      println("Enter type code")
      val tcode  = Console.readLine().toLowerCase()
      println("Enter question")
      val q = Console.readLine().toLowerCase()
      println("Enter answer choices (4 choices)")
      val ch1 = Console.readLine().toLowerCase()
      val ch2 = Console.readLine().toLowerCase()
      val ch3 = Console.readLine().toLowerCase()
      val ch4 = Console.readLine().toLowerCase()
      val ansch = List(ch1,ch2,ch3,ch4)
      println("Enter correct choice INDEX(from 0 to 3) ")
      val correctch = Console.readInt()
      val obj:Database =new Database(qno,ecode,tcode,q,ansch,correctch)
      //totalobj.add(obj)
      totalobj.+=:(obj)
      i += 1
    }

    //totalobj foreach(x =>println(x))

  }

  def getData(examc:String) =
  {
    /*var count =0
    examc match
    {
        case "java" =>
        {
          val filteredbyexam :scala.collection.mutable.MutableList[Database] = totalobj filter(_.examc == "java")
          val filterbytype_EASY :scala.collection.mutable.MutableList[Database] = filteredbyexam filter(_.typec == "easy")
          val filterbytype_MEDIUM :scala.collection.mutable.MutableList[Database] = filteredbyexam filter(_.typec == "medium")
          val filterbytype_HARD :scala.collection.mutable.MutableList[Database] = filteredbyexam filter(_.typec == "hard")

         println("TOTAL EASY QUESTIONS ARE-->\n")
          print(filterbytype_EASY mkString "")

          println("TOTAL MEDIUM QUESTIONS ARE-->\n")
          print(filterbytype_MEDIUM mkString "")

          println("TOTAL HARD QUESTIONS ARE-->\n")
          print(filterbytype_HARD mkString "")
          println("TOTAL QUESTIONS OF $examc -> "+filteredbyexam.size)
          println("TOTAL EASY QUESTIONS OF JAVA -> "+ filterbytype_EASY.size)
          println("TOTAL MEDIUM QUESTIONS OF JAVA -> "+ filterbytype_MEDIUM.size)
          println("TOTAL HARD QUESTIONS OF JAVA -> "+ filterbytype_HARD.size)}
        //case "scala" =>println("HELLO SCALA")
        case _ => throw  new Error("INVALID EXAM CODE... ObjectOrientedTest.getData()")
      }*/
    val filteredbyexam :scala.collection.mutable.MutableList[Database] = totalobj filter(_.examc == examc)
    val filterbytype_EASY :scala.collection.mutable.MutableList[Database] = filteredbyexam filter(_.typec == "easy")
    val filterbytype_MEDIUM :scala.collection.mutable.MutableList[Database] = filteredbyexam filter(_.typec == "medium")
    val filterbytype_HARD :scala.collection.mutable.MutableList[Database] = filteredbyexam filter(_.typec == "hard")



    objbyType(filterbytype_MEDIUM,1,"MEDIUM") // want 3 record of MEDIUM type
    objbyType(filterbytype_EASY,1,"EASY")  //Want 3 records from EASY type
    objbyType(filterbytype_HARD,1,"HARD")  //want 4 record of HARD type


    println(s"TOTAL QUESTIONS OF $examc -> "+filteredbyexam.size)
    println(s"TOTAL EASY QUESTIONS OF $examc -> "+ filterbytype_EASY.size)
    println(s"TOTAL MEDIUM QUESTIONS OF $examc -> "+ filterbytype_MEDIUM.size)
    println(s"TOTAL HARD QUESTIONS OF $examc -> "+ filterbytype_HARD.size)
    println("EXAM CONTAINS "+ examquestions.size +" QUESTIONS")

    /* println("TOTAL EASY QUESTIONS ARE-->\n")
     print(filterbytype_EASY mkString "")

     println("TOTAL MEDIUM QUESTIONS ARE-->\n")
     print(filterbytype_MEDIUM mkString "")

     println("TOTAL HARD QUESTIONS ARE-->\n")
     print(filterbytype_HARD mkString "")

    //Take 2 records from EASY type, 2 from MEDIUM and 2 from HARD type sequentially & PREPEND to examQuestions List so that HARD comes first and EASY comes last

    val examQuestions:mutable.MutableList[Database] =(filterbytype_EASY.slice(0,2))

    examQuestions +=:(filterbytype_HARD.slice(0,2))

    println("QUESTION SET IS \n")
    println(examQuestions toString())



    println("FINAL EXAM QUESTIONS ARE \n")
    println(examquestions mkString "")*/

  }

  def objbyType(xs:mutable.MutableList[Database],c:Int,difficulty:String) =
  {
    if(xs.size < c) throw new Error(s"TOO FEW RECORDS FOR $difficulty... INSERT ATLEAST $c RECORDS FOR GIVEN DIFFICULTY")
    for(i <- 0 until  c) {examquestions +=(xs(i))}

  }

  def conductExam() =
  {
    var totalRight = 0
    var totalEasyRight =0
    var totalMedRight =0
    var totalHardRight =0

    val index =examquestions.size
    var count = 0
    println("START EXAM??(y / n)")
    if(Console.readChar() =='y')
      do
      {

        println(s"Question $count :-> ")
        println(examquestions(count).question+"\n")

        println("OPTIONS ARE :-> ")
        println(examquestions(count).answer.mkString (" | ") )


        println("Enter choice (1 -> 4)")
        if((Console.readInt() - 1) == examquestions(count).rightChIndex)
        {
          println("CORRECT ANSWER")
          totalRight +=1
          if(examquestions(count).typec =="easy") totalEasyRight +=1
          else if(examquestions(count).typec =="medium") totalMedRight +=1
          else totalHardRight +=1
        }
        else println("WRONG ANSWER")

        count += 1

        if(count < index) println("Go to next question? (y/n)")
        else print("Exam finished..press any key ")

      }while(Console.readChar() == 'y' & count < index)

    else println("EXAM TERMINATED")

    println("PRESS 'y' to get STATUS")
    if(Console.readChar() == 'y')
    {
      println("TOTAL QUESTIONS -> "+examquestions.size)
      println("TOTAL EASY QUESTIONS -> "+examquestions.count(_.typec == "easy")+"| CORRECT -> "+totalEasyRight + "| WRONG -> "+(examquestions.count(_.typec=="easy") - totalEasyRight))
      println("TOTAL MEDIUM QUESTIONS -> "+examquestions.count(_.typec=="medium")+"| CORRECT -> "+totalMedRight + "| WRONG -> "+(examquestions.count(_.typec=="medium") - totalMedRight))
      println("TOTAL HARD QUESTIONS -> "+examquestions.count(_.typec == "hard")+"| CORRECT -> "+totalHardRight + "| WRONG -> "+(examquestions.count(_.typec=="hard") - totalHardRight))
    }
  }

}