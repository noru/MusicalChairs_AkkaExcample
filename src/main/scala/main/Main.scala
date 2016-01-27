package main

import main.Umpire._
import scala.io.StdIn._
import akka.actor._

object Main extends App{

    println("How many chairs before start? ")
    val total = 3//readInt()
    println("Ok.")

    val system = ActorSystem("MusicalChairs")
    val umpire = system.actorOf(Props(classOf[Umpire]), "Umpire")
    umpire ! Prepare(total)

    Thread.sleep(1000)
    println("game starts in ... ")
    (0 to 3).reverse.foreach(i => {
      println(i + " ")
//      Thread.sleep(1000)
    })
    umpire ! Start


}