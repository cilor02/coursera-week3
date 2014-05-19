package objsets

object week3 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(75); 
  println("Welcome to the Scala worksheet")
  

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet =
   filterAcc(p,new Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def maxAcc( max: Tweet): Tweet
  
  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = filterAcc((tweet) =>true,that)

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList


  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def descendingByRetweet: TweetList = Nil
  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException ()
  
  def maxAcc( max: Tweet): Tweet = max
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def descendingByRetweet2: TweetList =
  {
  def sortDescendingByRetweet(tweetSet:TweetSet): TweetList =
  {
    tweetSet match
    {
      case empty : Empty => Nil
      case _ =>   val tweet:Tweet = tweetSet.mostRetweeted;
                 new Cons(tweet,sortDescendingByRetweet(tweetSet.remove(tweet)))
    }

  }
    sortDescendingByRetweet(this)
  }

  
  def descendingByRetweet: TweetList =
  {
    val tweet = mostRetweeted
    new Cons(tweet, remove(tweet).descendingByRetweet)
  }

  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =

  {
    if (p(elem))
      right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
    else
      right.filterAcc(p, left.filterAcc(p, acc))
  }
  
  def mostRetweeted():Tweet =
  {
    maxAcc(elem)
  }
  
  def maxAcc( max: Tweet): Tweet =
  {
    if (elem.retweets > max.retweets)
     right.maxAcc(left.maxAcc( elem))
    else
    {
      right.maxAcc(left.maxAcc(max))
    }
      
  }
  
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
};$skip(6473); 


val tweet1 = new Tweet("u1","text1",1);System.out.println("""tweet1  : objsets.week3.Tweet = """ + $show(tweet1 ));$skip(39); 
val tweet2 = new Tweet("u2","text2",2);System.out.println("""tweet2  : objsets.week3.Tweet = """ + $show(tweet2 ));$skip(39); 
val tweet3 = new Tweet("u3","text3",3);System.out.println("""tweet3  : objsets.week3.Tweet = """ + $show(tweet3 ));$skip(39); 
val tweet4 = new Tweet("u4","text4",4);System.out.println("""tweet4  : objsets.week3.Tweet = """ + $show(tweet4 ));$skip(81); 

val tweetSet1 = new Empty().incl(tweet1).incl(tweet2).incl(tweet3).incl(tweet4);System.out.println("""tweetSet1  : objsets.week3.TweetSet = """ + $show(tweetSet1 ));$skip(37); 

val twt1 = new Tweet("u1","txt1",1);System.out.println("""twt1  : objsets.week3.Tweet = """ + $show(twt1 ));$skip(36); 
val twt2 = new Tweet("u2","txt2",2);System.out.println("""twt2  : objsets.week3.Tweet = """ + $show(twt2 ));$skip(36); 
val twt3 = new Tweet("u3","txt3",3);System.out.println("""twt3  : objsets.week3.Tweet = """ + $show(twt3 ));$skip(37); 
val twt4 = new Tweet("u4","txt4", 4);System.out.println("""twt4  : objsets.week3.Tweet = """ + $show(twt4 ));$skip(71); 

val twtSet1 = new Empty().incl(twt1).incl(twt2).incl(twt3).incl(twt4);System.out.println("""twtSet1  : objsets.week3.TweetSet = """ + $show(twtSet1 ));$skip(33); 
println(tweetSet1.mostRetweeted);$skip(51); 

tweetSet1.descendingByRetweet.foreach(println(_));$skip(98); 
//tweetSet1.foreach(println(_))
tweetSet1.filter((tweet)=>tweet.user != "u3").foreach(println(_));$skip(52); 
tweetSet1.filter((tweet)=>true).foreach(println(_));$skip(129); 
//twtSet1.foreach(println(_))
                                                  
 (twtSet1.union(tweetSet1)).foreach(println(_));$skip(32); 
new Empty().foreach(println(_));$skip(54); 
new Empty().filter((tweet)=>true).foreach(println(_));$skip(54); 

new Empty().descendingByRetweet.foreach(println(_));$skip(51); 
(new Empty().union(tweetSet1)).foreach(println(_));$skip(51); 
(tweetSet1.union(new Empty())).foreach(println(_))}

}
