package main.scala 

import collection.mutable
import main.scala._ 

class Maze {

  private val maze = Array(
    Array(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 1, 0, 0, 1, 0, 1, 0, 1, 0),
    Array(0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
    Array(0, 1, 0, 0, 1, 0, 1, 0, 1, 0),
    Array(0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    Array(0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
    Array(0, 1, 1, 1, 1, 1, 1, 0, 1, 1),
    Array(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 1, 0, 1, 1, 1, 1, 0, 1, 0),
    Array(0, 0, 0, 1, 0, 0, 0, 0, 1, 0))

  private val gridWidth = 20
  private val offsets = Seq((0, -1), (1, 0), (0, 1), (-1, 0))

  private var visited = Set[(Int, Int)]()
  private var startX = 0
  private var startY = 0
  private var endX = 9
  private var endY = 9


  override def toString = "Maze"

  def breadthFirstShortestPath(): Option[List[(Int, Int)]] = {
    val queue = new ArrayQueue[List[(Int, Int)]]
    queue.enqueue(List(startX -> startY))
    var solution: Option[List[(Int, Int)]] = None
    val visited = mutable.Set[(Int, Int)]()
    while (!queue.isEmpty && solution.isEmpty) {
      val steps @ ((x, y) :: _) = queue.dequeue
      for ((dx, dy) <- offsets) {
        val nx = x + dx
        val ny = y + dy
        if (nx >= 0 && nx < maze(0).length && ny >= 0 && ny < maze.length && maze(ny)(nx) == 0 && !visited(nx -> ny)) {
          if (nx == endX && ny == endY) {
            solution = Some((nx -> ny) :: steps)
          }
          visited += nx -> ny
          queue.enqueue((nx -> ny) :: steps)
        }
      }
    }
    solution
  }

}
