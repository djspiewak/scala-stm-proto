import org.specs._
import org.scalacheck._

import com.codecommit.stm._

object STMSpecs extends Specification with Scalacheck {
  import Prop._
  import Transaction._
  
  "type-system magic should compile" in {
    val ref = new Ref(42)
    
    // covariance
    val subSource: Source[Int] = ref
    val superSource: Source[AnyVal] = ref
    
    // contravariance
    val objRef = new Ref[Any]
    val subSink: Sink[Any] = objRef
    val superSink: Sink[String] = objRef
    
    true mustBe true    // tautology
  }
  
  "refs" should {
    "retrieve values" in {
      val ref = new Ref(42)
      
      ref.get mustEqual 42
      !ref mustEqual 42
      (ref:Int) mustEqual 42
    }
    
    "mutate values" in {
      val ref = new Ref(42)
      !ref mustEqual 42
      
      def modify(implicit t: Transaction) {
        ref := 6
      }
      atomic(modify(_))
      
      !ref mustEqual 6
    }
  }
  
  "transactions" should {
    "isolate changes" in {
      val ref = new Ref(42)
      
      def check() {
        !ref mustEqual 42
      }
      
      def modify(implicit t: Transaction) {
        !ref mustEqual 42
        
        ref := 12
        
        !ref mustEqual 12
        check()
      }
      atomic(modify(_))
      
      !ref mustEqual 12
    }
    
    "detect conflicts" in {
      val ref = new Ref(42)
      
      def modify(implicit t: Transaction) {
        ref := ref + 10
      }
      
      val right = thread {
        atomic(modify(_))
      }
      
      var runs = 0
      def modify2(implicit t: Transaction) {
        runs += 1
        
        val x = !ref
        Thread.sleep(250)    // create conflict
        
        ref := x + 10
      }
      
      val left = thread {
        atomic(modify2(_))
      }
      
      left.start()
      right.start()
      
      left.join()
      right.join()
      
      !ref mustEqual 62
      runs mustEqual 2
    }
    
    "maintain separate versions" in {
      val ref = new Ref(42)
      
      def modify1(implicit t: Transaction) {
        ref := 21
      }
      
      val left = thread {
        atomic(modify1(_))
      }
      
      var runs = 0
      def modify2(implicit t: Transaction) {
        runs += 1
        
        Thread.sleep(250)
        
        if (runs == 1) {
          !ref mustEqual 42     // changed in global context, but not here!
        } else {
          !ref mustEqual 21
        }
        
        ref := ref + 10
      }
      
      val right = thread {
        atomic(modify2(_))
      }
      
      right.start()
      left.start()
      
      right.join()
      left.join()
      
      !ref mustEqual 31
      runs mustEqual 2
    }
  }
  
  def thread(f: =>Unit): Thread = new Thread {
    override def run() {
      f
    }
  }
}
