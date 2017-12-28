package ru.yudnikov.genetic

import genetic._

object Main extends App {
  val p = Population(10, 10)
  p.individuals foreach println
  println(p.select())
  var currentGeneticAlgorithm = GeneticAlgorithm(p, 0.95, 0.1, 0)
  val genetartionLimit = 1000
  var currentGeneration = 1
  while (!currentGeneticAlgorithm.isSolved && currentGeneration <= genetartionLimit) {
    currentGeneticAlgorithm = currentGeneticAlgorithm.evaluate
    currentGeneration = currentGeneration + 1
  }
}