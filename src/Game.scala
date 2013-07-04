import java.lang._

/**
 * <b>Game</b> represents a single away game in the roster,
 * and takes 3 parameters:
 * 	name, firsts, seconds
 *  
 *  Has an auxiliary constructor for creating an empty game
 *  that only takes the name parameter and sets the firsts and seconds to Nil
 *  
 * @param n: String, name of the game
 * @param f: List[Choice], current roster for firsts
 * @param s: List[Choice], current roster for seconds
 *
 * @author Gene Kim
 * June 16, 2013
 */
class Game(name: String, f: List[Choice], s: List[Choice]) {
  var firsts = f
  var seconds = s
  
  /**
   * Auxiliary constructor
   * @param n: String, name of the game
   */
  def this(n: String) = this(n, Nil, Nil)
  
  /**
   * @return Boolean, true if the game's roster is full, else false
   */
  def isFull(): Boolean = isFull(1) && isFull(2) 
  
  /**
   * Checks if the Game is full
   * @param part: Int, the part of the section.  Must be 1 or 2
   * @throws IllegalArgumentException if isFull is passed an Int other than 1 or 2
   * @return Boolean, true if the game's roster for the given part id full,
   * 			else false
   */
  def isFull(part: Int): Boolean = {
    part match {
      case 1 => firsts.length == 3
      case 2 => seconds.length == 3
      case _ => throw new IllegalArgumentException("part must be 1 or 2 in Game.isFull(part)")
    }
  }
  
  /**
   * Adds a choice to the Game roster
   * @param mem: Choice, choice to be added to the roster
   * @throws IllegalStateException if the part of the member in the choice object is not 1 or 2
   */
  def add(mem: Choice) = {
    mem.member.part match {
      case 1 => mem::firsts
      case 2 => mem::seconds
      case _ => throw new IllegalStateException("member's part must be either 1 or 2")
    }
  }
  
  /**
   * Default string representation of Game
   * @return String, string form of Game
   */
  override def toString() = {
    "Game:\t" + name + "\n1sts:\t" + firsts + "\n2nds:\t" + seconds
  }
}