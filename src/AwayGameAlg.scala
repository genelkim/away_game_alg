import java.io._
import scala.io.Source
import scala.collection.mutable.PriorityQueue

/**
 * <b>AwayGameAlg</b> has a main method that determines the roster for the away trips
 * in the Husky Marching Band trombone section.
 * 
 * The roster is determined by each individual's priority for the games, seniority, and gig participation.
 *
 * @author Gene Kim
 * June 16, 2013
 */
object AwayGameAlg {
	
	var members: List[Member] = Nil
	var heap = new Array[PriorityQueue[Choice]](GAME_NUM)
	
	// generalize games by file input
	val games = Array[String]("Arizona", "Stanford", "OSU")
	val GAME_NUM = 3
	val yrMap = Map(2 -> 10, 3 -> 14, 4 -> 14, 5 -> 13)
	
	/**
	  * @param args: 1 argument at index 0 of filename
	  */
	def main(args: Array[String]) {
		val file = Source.fromFile(args(0)).getLines
		initPeople(file)
		//printMemList
		buildHeap()
		//printHeap()
		var roster = new Array[Game](GAME_NUM)
		for(i <- 0 until GAME_NUM) { roster(i) = new Game(games(i), Nil, Nil) }
		
		// TODO: add cases where no choices are left for a specific game, or something else...
		while(!roster.forall(x => x.isFull)) {
			chooseSingleRound(roster)
		}		
		roster.foreach(x => println(x + "\n"))
	}
	
	
	/**
	 * @requires cur has size GAME_NUM (1 for each game)
	 */
	def chooseSingleRound(cur: Array[Game]) = {
	  var current = cur
	  var mems = members.toSet
	  var saved: List[Choice] = Nil
	  while((!mems.isEmpty) && (!gamesFull(current)) && getNext() != -1) {
	    
	    // TODO: refactor-inefficient, repeating call to getNext
	    val index = getNext()
	    
	    // next Choice object
	    val next = heap(index).dequeue
	    val game = current(index)
	    
	    // if checks, adds to roster.
	    // else places in saved list
	    if (mems.contains(next.member) && !game.isFull(next.member.part)) {
	      game.add(next)
	      mems = mems - next.member
	    } else {
	      saved = next::saved
	    }
	  }
	  rebuildHeap(saved)
	}
	
	/**
	 * Checks if the entire roster is full
	 * @return true if all games are full, else false
	 */
	def gamesFull(games: Array[Game]) = games.forall(game => game.isFull)
	
	
	/**
	 * Gets next index in heap to remove
	 */
	def getNext(): Int = {
	  var max = 0
	  var index = -1
	  for(i <- 0 to GAME_NUM - 1) {
	    // TODO: add tie-breaker
	    if(!heap(i).isEmpty && heap(i).max(ChoiceOrdering).priority > max) {
	      max = heap(i).max(ChoiceOrdering).priority
	      index = i
	    }
	  }
	  return index
	}
	
	/**
	 * Builds an array of priority queues (1 for each game) from the members list
	 */
	def buildHeap() = {
	  for(i <- 0 to GAME_NUM - 1) {
	    var pq = new PriorityQueue[Choice]()(ChoiceOrdering) 
	    for(mem <- members if mem.priorities(i) != 0) {
	        pq += new Choice(mem, mem.priorities(i), i)
	    }
	    heap(i) = pq
	  }
	}
	
	def rebuildHeap(saved: List[Choice]) = saved.foreach(x => heap(x.game) += x)
	
	/**
	 * Initializes the members list to have all the necessary data:
	 * 	name, part, seniority, gigs, priorities
	 */
	// Expected line form:  Name seniority gigs G0-G5
	private def initPeople(iter: Iterator[String]) = {
		for(p <- iter) {
		  //println(p)
		  // get values from string
		  val line = p.trim.split(" +")
		  val name = line(0)
		  val part = line(1).toInt
		  val sen = line(2).toInt
		  val gigs = line(3).toInt
		  val rawPriorities = line.slice(4, line.length).map(a => a.toInt) 
			
			// modify priorities
			val priorities = normalizeSum(rawPriorities.toList).map((value: Int) => calcPriority(value, sen, gigs))
			val newMem = new Member(name, part, sen, gigs, priorities)
			members = (newMem)::members
		}
	}
	
	// calculates the priority from the raw score, seniority, and gigs
	private def calcPriority(value: Int, sen: Int, gigs: Int) = {
	  if(value == 0) 0 
	  else value * yrMap(sen) + ((gigs * value) / 100)
	}	
	  
	private def normalizeSum(prefs: List[Int]): List[Int] = {
		val sum = prefs.fold(0)((x, y) => x + y)
		var r = prefs
		if(sum > 100) {
			r = prefs.map(x => (x*100) / sum) 
		}
		return r
	}
	
	////////////////////////////////////////////////////////////////////////////////////////////////
	////// Helper Functions
	////////////////////////////////////////////////////////////////////////////////////////
	
	
	// TODO: Useless, remove after done referencing
	// splits iterator in each word by whitespace.
	private def testFile(iter: Iterator[String]) = {
		for(a <- iter) {
			for(word <- a.trim.split(" +")) {
				println(word)
			}
		}
	}
	
	// Helper function to see progress
	private def printMemList() = {
	  for(mem <- members) {
	    println(mem)
	  }
	}
	// Helper function to see progress
	private def printHeap() = {
	  for(pq <- heap) {
	    for(s <- pq.clone().dequeueAll.iterator) {
	      println(s)
	    }
	    println()
	  }
	}
}