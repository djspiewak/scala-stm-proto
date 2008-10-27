import com.codecommit.stm._

object EasyTest {
  import Transaction._
  
  val count = new Ref[Int]
  
  def main(args: Array[String]) {
    val left = new Thread {
      override def run() {
        atomic(countImpl(_))
      }
    }
    
    val right = new Thread {
      override def run() {
        atomic(countImpl(_))
      }
    }
    
    left.start()
    right.start()
    
    left.join()
    right.join()
    
    val result: Int = count      // reading from outside transaction, how 'bout them apples?
    printf("Result: %d%n", result)
  }
  
  private def countImpl(implicit t: Transaction) {
    for (i <- 0 until 10) {
      val i: Int = count
      Thread.sleep((Math.random * 50).intValue)    // randomly force collision
      
      count := i + 1
    }
  }
}
