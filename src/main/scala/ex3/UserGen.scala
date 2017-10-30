package ex3

import java.sql.Timestamp
import java.time.Instant
import java.util.UUID

import ex2.Prob


object UserGen extends App {


  val idProb: Prob[String] = Prob.fromGet(() => UUID.randomUUID().toString)


  val ipProb: Prob[String] = {
    val ipPartProb: Prob[Int] = Prob.range(100, 255)
    for {
      i1 <- ipPartProb
      i2 <- ipPartProb
      i3 <- ipPartProb
      i4 <- ipPartProb
    } yield s"$i1.$i2.$i3.$i4"
  }

  val firstTsProb: Prob[Timestamp] = {
    val i = Instant.now()
    Prob.range(0, 24 * 3600).map(s => new Timestamp((i.getEpochSecond + s) * 1000))
  }


  {
    "i = 20171025 : 23:00:45"
    "12h"

  }

  case class User(id: String, ip: String, firstInteraction: Timestamp)

  val userProp: Prob[User] = for {
    userId <- idProb
    ip <- ipProb
    ts <- firstTsProb

  } yield {
    User(userId, ip, ts)
  }

  userProp.samples(10).foreach(println)


  val nbInteractionProb: Prob[Int] = for {
    more <- Prob.uniform.map(_ < 0.05)
    nbNext <- Prob.range(2, 4)
  } yield if (more) nbNext else 1


  val topics: Prob[String] = Prob.choose(
    """Active
     Drag to reorder
     Top Stories
     World
     U.S.
     Business
     Entertainment
     Sports
     Health
     Hide
     Technology
     Hide
     Science""".split("\n").map(_.trim))

  val clicTypeProb: Prob[String] = Prob.choose(Seq("Unfold", "CloseAd", "Share"))

  case class Clic(userId: String, ip: String, pageId: String, clicType: String, ts: Timestamp)

  def clicProb(user: User, ts: Timestamp): Prob[Clic] = for {
    clicT <- clicTypeProb
    t <- topics
  } yield {
    Clic(userId = user.id, ip = user.ip, pageId = t, clicType = clicT, ts = ts)
  }

  val clicProb: Prob[Seq[Clic]] = for {
    user <- userProp
    nbInt <- nbInteractionProb
  } yield {
    clicProb(user, user.firstInteraction).samples(nbInt)
  }


  clicProb.samples(100).map(_.toVector).foreach(println)


  def flattenList[T](from: Prob[Seq[T]]): Prob[T] = new Prob[T] {
    private var xs: List[T] = Nil

    override def get: T = {
      if (xs.isEmpty) {
        xs = from.get.toList
        this.get
      } else {
        val x :: rest = xs
        xs = rest
        x
      }
    }
  }


  println(flattenList(Prob.choose[Seq[String]](List(List("a", "a"), List("b"), Nil))).density())


}


/** *
  * *
  * package prj.rng
  * *
  * import java.sql.Timestamp
  * import java.time.Instant
  * import java.util.UUID
  * *
  * import org.apache.spark.rdd.RDD
  * import org.apache.spark.sql.{Dataset, SaveMode, SparkSession}
  * *
  * import scala.collection.immutable
  * import scala.util.Random
  * *
  * object RngUtils {
  * *
  * lazy val random = Random
  * *
  * def rndIp: String = {
  * *
  * def ippart = random.nextInt(256)
  * *
  * s"$ippart.$ippart.$ippart.$ippart"
  * }
  * *
  * def main(args: Array[String]): Unit = {
  * *
  * println(rndIp)
  * }
  * *
  * }
  * *
  *
  * object Values {
  * *
  * val topics: Seq[String] =
  * """Active
  * Drag to reorder
  * Top Stories
  * World
  *U.S.
  * Business
  * Entertainment
  * Sports
  * Health
  * Hide
  * Technology
  * Hide
  * Science""".split("\n").map(_.trim)
  * *
  *
  * def nextTopic: String = topics(Random.nextInt(topics.length))
  * *
  *
  * val clicTypes:Seq[String] = Seq("Unfold", "CloseAd", "Share")
  * *
  *
  * def randomFromSeq[T](seq:Seq[T]): T = seq(Random.nextInt(seq.length))
  * }
  * *
  *
  *
  *
  * case class User(id: String = UUID.randomUUID().toString, ip: String = RngUtils.rndIp, currentTimestamp:Timestamp = Timestamp.from(Instant.now())) {
  * *
  *
  * def next(topic:String = Values.nextTopic, pageInfo:(Boolean,Int) = (true,0)):(User, Seq[Search], Seq[Clic]) = {
  * *
  * val (newSearch, indexPage) = pageInfo
  * *
  * val pageId = UUID.randomUUID().toString
  * val page = Search(userId = id,ip = ip,topic = topic,ts = currentTimestamp,pageId = pageId, newSearch = newSearch, indexPage = indexPage)
  * *
  *
  * def nextClics(ts:Timestamp, chance:Double = 0.3):List[Clic] = {
  * if(Random.nextDouble() < chance) {
  * val newTs = new Timestamp(ts.getTime + Random.nextInt(10 * 1000))
  * Clic(userId = id,ip = ip, pageId, Values.randomFromSeq(Values.clicTypes), newTs) :: nextClics(newTs, 0.5)
  * } else Nil
  * }
  * *
  * val clics = nextClics(currentTimestamp)
  * *
  *
  * val nextInteraction = Random.nextDouble()
  * val tsAd = if(nextInteraction < 0.5) {
  * 15 + Random.nextInt(600)
  * } else if (nextInteraction < 0.9) {
  * 600 + Random.nextInt(2 * 3600)
  * } else {
  *Random.nextInt(36 * 3600)
  * }
  * *
  * val extraBall = Random.nextDouble()
  * val newUser = copy(currentTimestamp = new Timestamp(currentTimestamp.getTime + tsAd * 1000))
  * *
  * if(extraBall < 0.2) {
  * *
  * val (_, pages, newClics) = newUser.next(topic, (false, indexPage + 1))
  * *
  * (newUser, page :: pages.toList, clics ++ newClics )
  * *
  * } else {
  * *
  *
  * (newUser, Seq(page), clics)
  * }
  * *
  *
  *
  * }
  * *
  *
  * }
  * *
  *
  *
  * case class Search(userId: String, ip: String, ts:Timestamp, pageId:String, newSearch:Boolean, indexPage:Int, topic: String)
  * *
  * case class Clic(userId: String, ip:String, pageId:String, clicType:String, ts:Timestamp)
  * *
  *
  * object Test {
  * *
  * def main(args: Array[String]): Unit = {
  * *
  * val values: Vector[(User, Seq[Search], Seq[Clic])] = (0 to 10000).toVector.flatMap(_ => Stream.iterate( User().next())(_._1.next()).takeWhile(_._2.exists(_.ts.toString < "2017-09-16")))
  * *
  * val ss = SparkSession.builder().master("local").appName("test").getOrCreate()
  * *
  * import ss.implicits._
  * *
  * val sc = ss.sparkContext
  * val rdd1: RDD[Clic] = sc.makeRDD(values.map(_._3)).flatMap(identity)
  * val ds1: Dataset[Clic] = rdd1.toDS()
  * *
  *
  * val ds2: Dataset[Clic] = ds1.filter("ts > 10")
  * *
  * ds2.coalesce(1).sort("ts").write.option("header", "true")
  * .mode(SaveMode.Overwrite).csv("clic-data.csv")
  * *
  *ss.createDataFrame(sc.makeRDD(values.map(_._2)).flatMap(identity)).coalesce(1).sort("ts").write.option("header", "true").mode(SaveMode.Overwrite).csv("search-data.csv")
  * *
  *
  * }
  * *
  * }
  *
  *
  * **/