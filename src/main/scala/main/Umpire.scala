package main

import akka.actor.{Props, Actor}

import Umpire.{Success, _}

object Umpire {
  case class Prepare(i: Int)
  case object Start
  case object Stop
  case object Ready
  case object Acquire
  case object Success
  case class Failed(round: Int)
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
      println(players.mkString(", ") + "have join the game.")
      players.foreach( p => context.actorOf(Props(classOf[Player]), p))
    }

    case Start => {
      available = chairs - round
      round = round + 1
      if (round <= chairs){
        println("Round " + round + ", Go!")
        context.children.foreach(_ ! Ready)
      }
    }

    case Stop => {
      context.stop(self)
    }

    case Acquire => available match {

      case i if i > 1 => {
        available = available - 1
        sender() ! Success
      }
      case 1 => {
        available = 0
        if (round == chairs) {
          println("We have a winner! " + sender().path.name)
          self ! Stop
        } else {
          self ! Start
        }

      }
      case _ => {
        sender() ! Failed(round)
      }
    }
  }


}


class Player extends Actor {

  var out = false
  def receive = {
    case Ready => {
      if (!out){
        Thread.sleep(500)
        sender() ! Acquire
      }
    }
    case Success => {
//      println(self.path.name + " got a chair")
    }
    case Failed(i) => {
      println(self.path.name + " is out of the game in round " + i)
      out = true
    }
  }

}