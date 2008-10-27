import com.codecommit.stm._
import com.codecommit.collection._

object BankFrenzy {
  import Transaction._
  
  type Account = Ref[Long]
  
  private val fees = new Account
  private val log: Ref[Vector[Transfer]] = new Ref(EmptyVector)
  
  def main(args: Array[String]) {
    import Thread.sleep
    
    val business1 = new Account(15000)
    val business2 = new Account(20000)
    val business3 = new Account(50000)
    
    val people = (0 until 100).foldLeft(Vector[Account]()) { (vec, i) =>
      vec + new Account(1000)
    }
    
    val market = people + business1 + business2 + business3 + fees
    var running = true
    
    val secActor = thread {
      while (running) {
        val total = atomic(sum(market)(_))
        println("Market value: $" + total)
        
        sleep(10)
      }
    }
    
    val businessActor = thread {
      while (running) {
        atomic(transfer(250, business1, business2)(_))    // transfer rent
        
        sleep(200)
      }
    }
    
    val peopleActors = for {
      i <- 0 until people.length
      val p = people(i)
    } yield thread {
      atomic(transfer(50, p, business3)(_))       // payoff the mob
      atomic(transfer(i * 10, p, business1)(_))   // purchase from business1
      atomic(transfer(i * 3, business2, p)(_))    // refund from business2
    }
    
    println("Starting market value: $" + atomic(sum(market)(_)))
    
    businessActor.start()
    secActor.start()
    
    for (pa <- peopleActors) pa.start()
    for (pa <- peopleActors) pa.join()
    running = false
    
    businessActor.join()
    secActor.join()
    
    println("Total fees: $" + fees)
    println("Final market value: $" + atomic(sum(market)(_)))
  }
  
  def transfer(amount: Long, from: Account, to: Account)(implicit t: Transaction) {
    log := log + Transfer(amount, from, to)
    
    val less = Math.round(amount * 0.075)
    
    from := from - amount
    to := to + (amount - less)
    fees := fees + less
  }
  
  def sum(portfolio: Vector[Account])(implicit t: Transaction) = {
    portfolio.foldRight(0:Long) { _ + _ }
  }
  
  def thread(f: =>Unit) = new Thread {
    override def run() {
      f
    }
  }
  
  case class Transfer(amount: Long, from: Account, to: Account)
}
