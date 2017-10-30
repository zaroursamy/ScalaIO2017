import java.sql.Timestamp
import java.time.Instant
import java.util.{Calendar, UUID}

import ex2.Prob
import ex3.UserGen.{firstTsProb, idProb, ipProb}

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

val i: Instant = Instant.now()

val firstTsProb: Prob[Timestamp] = {
  Prob.range(0, 24 * 3600).map(s => new Timestamp((i.getEpochSecond + s) * 1000))
}

case class User(id: String, ip: String, firstInteraction: Timestamp)

val userProp: Prob[User] = for {
  userId <- idProb
  ip <- ipProb
  ts <- firstTsProb

} yield {
  User(userId, ip, ts)
}

val probUser = idProb.flatMap(x => ipProb.flatMap(y => firstTsProb.flatMap(z => Prob.fromGet(()=>User(x,y,z)))))

userProp.samples(10).foreach(println)
