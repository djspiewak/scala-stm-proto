package com.codecommit.stm

import scala.collection._

final class Transaction private[stm] (val rev: Int) extends Context {
  private[stm] val world = mutable.Map[Ref[Any], Any]()
  
  private val writes = mutable.Set[Ref[Any]]()
  private val version = mutable.Map[Ref[Any], Int]()
  
  private[stm] def retrieve[T](ref: Ref[T]) = {
    val castRef = ref.asInstanceOf[Ref[Any]]
    
    if (!world.contains(castRef)) {
      val (value, refRev) = ref.contents
      
      world(castRef) = value
      
      if (!version.contains(castRef)) {
        version(castRef) = refRev
      }
    }
    
    world(castRef).asInstanceOf[T]
  }
  
  private[stm] def store[T](ref: Ref[T])(v: T) {
    val castRef = ref.asInstanceOf[Ref[Any]]
    
    if (!version.contains(castRef)) {
      version(castRef) = ref.rev
    }
    
    world(castRef) = v
    writes += castRef
  }
  
  private[stm] def commit() = {
    if (world.size > 0) {
      CommitLock.synchronized {
        val back = world.foldLeft(true) { (success, tuple) =>
          val (ref, _) = tuple
          success && ref.rev == version(ref)
        }
        
        if (back) {
          for (ref <- writes) {
            ref.contents = (world(ref), rev)
          }
        }
        
        back
      }
    } else true
  }
}

object Transaction {
  private var rev_ = 1
  private val revLock = new AnyRef
  
  private def rev = revLock.synchronized {
    val back = rev_
    rev_ += 1
    back
  }
  
  implicit def refToValue[T](ref: Ref[T])(implicit c: Context) = ref.get(c)
  
  def atomic[A](f: (Transaction)=>A): A = atomic(true)(f)
  
  def atomic[A](cond: =>Boolean)(f: (Transaction)=>A) = {
    def attemptTransact(): A = {
      if (cond) {
        val trans = new Transaction(rev)
        
        try {
          val result = f(trans)
          
          if (trans.commit()) result else attemptTransact()
        } catch {
          case RetryMessage => {      // on retry(), wait for a change and then try again
            val block = new AnyRef
            
            for ((ref, _) <- trans.world) {
              ref.registerBlock(block)
            }
            
            block.wait()
            
            for ((ref, _) <- trans.world) {
              ref.deregisterBlock(block)
            }
            
            attemptTransact()
          }
          
          case FailureException(e) => {     // propagate exception 
            throw e
          }
          
          case _ => attemptTransact()    // if exception, assume conflict and retry
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
  
  def fail(e: Throwable)(implicit t: Transaction) {
    throw FailureException(e)
  }
  
  private case object RetryMessage extends RuntimeException
  private case class FailureException(e: Throwable) extends RuntimeException
}

private[stm] object CommitLock
