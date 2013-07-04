/**
 * <b>Choice</b> represents one choice in this years member and games,
 * and takes 3 parameters:
 * 	member, priority, game
 *  
 * Have increasing sort based on priority with ChoiceOrdering
 * 
 * @param m:	Member, member associated with the choice
 * @param prt: 	Int, part of the member (1 or 2); redundant from member to simplify code
 * @param y: 	Int, years the member has been in the band; redundant to simplify code
 *
 * @author Gene Kim
 * June 16, 2013
 */
class Choice(m: Member, p: Int, g: Int) {
  val priority = p
  val member = m
  val game = g
  
    /**
   * Default string representation of Choice
   * @return String, string form of Choice
   */
  override def toString() = {
    member.name + "\t" + priority
  }
}

/**
 * <b>ChoiceOrdering</b> is the basic ordering for Choice.
 * Increasing based on the priority of the Choice
 */
object ChoiceOrdering extends Ordering[Choice] {
  def compare(a:Choice, b:Choice) = a.priority compare b.priority
}