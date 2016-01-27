package main

import main.Umpire._
import scala.io.StdIn._
import akka.actor._

object Main extends App{

    println("How many chairs before start? (2-11) ")
    val total = readInt().ensuring(i => i > 1 && i < 12)
    println("Got it.")

    val system = ActorSystem("MusicalChairs")
    val umpire = system.actorOf(Props(classOf[Umpire]), "Umpire")
    umpire ! Prepare(total)

    Thread.sleep(1000)
    println("Game starts in ... ")
    (0 to 3).reverse.foreach(i => {
      println(i + " ")
      Thread.sleep(1000)
    })

    umpire ! Start

}