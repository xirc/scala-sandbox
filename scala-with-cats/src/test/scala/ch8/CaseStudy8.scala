package ch8

import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import testing.BaseSpec

import scala.concurrent.Future

final class CaseStudy8 extends BaseSpec {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  final class TestUptimeClient(hosts: Map[String, Int])
      extends UptimeClient[Id] {
    override def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }

  final class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  "total uptime" in {

    val hosts = Map("host1" -> 1, "host2" -> 2)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val uptime = service.getTotalUptime(hosts.keys.toList)
    val expectedUptime = hosts.values.sum
    uptime shouldBe expectedUptime

  }

}
