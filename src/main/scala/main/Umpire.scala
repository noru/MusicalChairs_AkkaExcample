package main

import akka.actor.{Props, Actor}
import Umpire._
import scala.util.Random

object Umpire {
  case class Prepare(chairs: Int)
  case object Start
  case class Ready(round: Int)
  case class Acquire(round: Int)
  case class Failed(round: Int)
  case class Pass(round: Int)
}

class Umpire extends Actor{

  var chairs = 0
  var round = 0
  var available = 0

  val AllPlayers = Vector("John", "Mike", "Sarah", "Amy", "Carry", "Sam", "Dick", "Fred", "Cam", "Fiona", "Steve", "Chris")

  def receive = {

    /** Setup initial state for the game: chairs, player actors
      */
    case Prepare(i) => {
      chairs = i
      available = chairs
      val players = AllPlayers.take( if (i > AllPlayers.length - 1) AllPlayers.length else i + 1)
      println(players.mkString(", ") + " have join the game.")
      players.foreach( p => context.actorOf(Props(classOf[Player]), p))
    }

    /** Start each round
      */
    case Start => {
      available = chairs - round
      round = round + 1
      if (round <= chairs){
        println("Round " + round + ", Go!")
        context.children.foreach(_ ! Ready(round))
      }
    }

    /** One of the player requests acquiring a chair
      * Note: Acquire(r) -> r is like a token to mark the corresponding round that the players are in,
      * if not match current round, failed the player directly
      */
    case Acquire(r) => available match {

      // Plenty of Seats!
      case i if i > 1 && r == round => {
        available = available - 1
        sender() ! Pass(r)
      }
      // Last Seat!
      case 1 if r == round => {
        available = 0
        sender() ! Pass(r)
        if (round == chairs) {
          println("We have a winner! " + sender().path.name)
        } else {
          self ! Start
        }
      }
      // No seats! You're out!
      case _ => {
        sender() ! Failed(r)
      }
    }
  }
}

class Player extends Actor {

  var out = false
  var busy = false
  def receive = {
    case Ready(i) => {
      if (!out && !busy){
        Thread.sleep(Random.nextInt(1000))
        sender() ! Acquire(i)
        busy = true
      }
    }
    case Pass(r) => {
      busy = false
//      println(self.path.name + " got a chair in round " + r)
    }
    case Failed(i) => {
      if (!out){
        println(self.path.name + " is out of the game in round " + i)
        out = true
      }
    }
  }

}