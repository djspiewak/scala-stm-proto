package com.codecommit.stm

import scala.collection._

final class Transaction private[stm] (val rev: Int) extends Context {
  private[stm] val world = mutable.Map[Ref[Any], Any]()
  
  private var reads = immutable.TreeSet[Ref[Any]]()
  private val writes = mutable.Set[Ref[Any]]()
  
  private val version = mutable.Map[Ref[Any], Int]()
  
  private[stm] def retrieve[T](ref: Ref[T]) = {
    val castRef = ref.asInstanceOf[Ref[Any]]
    reads += castRef
    
    world.synchronized {
      if (!world.contains(castRef)) {
        val (value, refRev) = ref.contents
        
        world(castRef) = value
        version(castRef) = refRev
      }
      
      world(castRef).asInstanceOf[T]
    }
  }
  
  private[stm] def store[T](ref: Ref[T])(v: T) {
    val castRef = ref.asInstanceOf[Ref[Any]]
    writes += castRef
    
    world.synchronized {
      if (!version.contains(castRef)) {
        version(castRef) = ref.rev
      }
      
      world(castRef) = v
    }
  }
  
  private[Transaction] def preserve[T](ref: Ref[T]) {
    val castRef = ref.asInstanceOf[Ref[Any]]
    
    world.synchronized {
      if (!world.contains(castRef)) {
        val (v, rev) = ref.contents
        
        world(castRef) = v
        version(castRef) = rev
      }
    }
  }
  
  private[stm] def commit() = {
    if (world.size > 0) {
      writes foreach { _.lock() }
      try {
        val f = { ref: Ref[Any] => ref.rev == version(ref) }
        val back = reads.forall(f) && writes.forall(f)
        
        if (back) {
          for (ref <- writes) {
            for (t <- Transaction.active) {
              if (t != this) t.preserve(ref)
            }
            
            ref.contents = (world(ref), rev)
          }
        }
        
        back
      } finally {
        writes.foldRight(()) { (r, u) => r.unlock() }		// trick to get the elements in reverse
      }
      
    } else true
  }
  
  private implicit def convertOrdered[A](ref: Ref[A]): Ordered[Ref[A]] = new Ordered[Ref[A]] {
    def compare(that: Ref[A]) = this.hashCode compare that.hashCode
  }
}

object Transaction {
  private var _rev = 1
  private val revLock = new AnyRef
  
  private val _active = new mutable.HashSet[Transaction] with mutable.SynchronizedSet[Transaction]
  
  private def rev = revLock.synchronized {
    val back = _rev
    _rev += 1
    back
  }
  
  private[Transaction] def active = _active
  
  private def activate(t: Transaction) {
    _active += t
  }
  
  private def deactivate(t: Transaction) {
    _active -= t
  }
  
  implicit def sourceToValue[T](src: Source[T])(implicit c: Context) = src.get(c)
  
  def atomic[A](f: (Transaction)=>A): A = atomic(true)(f)
  
  def atomic[A](cond: =>Boolean)(f: (Transaction)=>A) = {
    def attemptTransact(): A = {
      if (cond) {
        val trans = new Transaction(rev)
        activate(trans)
        
        try {
          val result = f(trans)
          
          if (trans.commit()) result else {
            deactivate(trans)
            attemptTransact()
          }
        } catch {
          case RetryMessage => {      // on retry(), wait for a change and then try again
            val block = new AnyRef
            
            deactivate(trans)
            
            for ((ref, _) <- trans.world) {
              ref.registerBlock(block)
            }
            
            block.wait()
            
            for ((ref, _) <- trans.world) {
              ref.deregisterBlock(block)
            }
            
            attemptTransact()
          }
        }
      } else null.asInstanceOf[A]
    }
    
    attemptTransact()
  }
  
  def retry()(implicit t: Transaction) {
    throw RetryMessage
  }
  
  def check(c: Boolean)(implicit t: Transaction) {
    if (c) retry()
  }
  
  private case object RetryMessage extends RuntimeException
}
