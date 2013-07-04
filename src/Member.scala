/**
 * <b>Member</b> represents one member in the section,
 * and takes 5 parameters:
 * 	name, part, year, gigs, and priorities
 * 
 * @param n:	String, name of the member
 * @param prt: 	Int, part of the member (1 or 2)
 * @param y: 	Int, years the member has been in the band
 * @param g: 	Int, number of gigs the member has done in the past year
 * @param p: 	List[Int], List of length equal to the number of away games, 
 *  	 			each value corresponding to the priority of one game  
 *       
 * @author Gene Kim
 * June 16, 2013
 */
class Member(n: String, prt: Int, y: Int, g: Int, p: List[Int]) {
	val name = n
	val part = prt
	val year = y
	val gigs = g
	val priorities = p
	
	/**
	 * Default string representation of Member
	 * @returns String, representation of Member
	 */
	override def toString() = {
		"[name: " + name + "\tpart: " + part + "\tyear: " + year + "\tgigs: " + gigs + "\tpriorities: [" + priorities.fold("")((x, y) => x + " " + y) + " ]]"
	}
}