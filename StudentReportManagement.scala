package com.knoldus.list.assignment1

/**
  * Created by ashish on 20/1/17.
  */
class StudentReportManagement {

   case class Student(id: Int,name: String)
   case class Marks(subjectId: Int,studentId: Int,marksObtained: Int)

   val studentList = List(Student(1,"Anuj"),Student(2,"Ramandeep"),Student(3,"Simarpreet"),Student(4,"Gitika"),Student(5,"Jatin"),Student(6,"Kunal"))
   val subjectList = List(Marks(1,1,23),Marks(2,1,34),Marks(3,1,40),Marks(4,1,40),Marks(5,2,23),Marks(1,2,23),Marks(2,2,23),Marks(3,2,23))

  def passFail(subjectId:Int, percentage:Double,result: String) : String = {
     result.toLowerCase match {
       case "pass" => s"$result count: ${ subjectList.flatMap(x=> {if(x.subjectId==subjectId && x.marksObtained>=percentage) Some(x) else None}).length.toString}"

       case "fail" => s"$result count: ${subjectList.flatMap(x=> {if(x.subjectId==subjectId && x.marksObtained<percentage) Some(x) else None}).length.toString}"

       case _      => "N/A"
     }
   }

  def generateReportCard() = {
    val marksList = subjectList.groupBy(_.studentId)
    studentList.map(x =>x.id ==(marksList.mapValues(x => print(x))))

  }
}

object StudentReportManagement {
  def main(args: Array[String]) {
    val obj=new StudentReportManagement()
    println(obj.passFail(1,30,"pass"))
    obj.generateReportCard()
  }
}