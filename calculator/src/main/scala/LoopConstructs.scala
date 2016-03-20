/** Optional exercise suggested in video to implement a REPEAT .. UNTIL .. loop construct */
object LoopConstructs {

 def REPEAT(command : => Unit) = new Repeat(command)
 class Repeat(command : => Unit)  {
   def UNTIL(condition : => Boolean) :Unit = {
     command
     if (!condition)
       UNTIL(condition)
   }
 }

  def main(args:Array[String]) : Unit ={
    var sum =0
    var x =0
    REPEAT{
      sum +=x
      x += 1
    } UNTIL (x > 10)
    println(sum)
  }



}
