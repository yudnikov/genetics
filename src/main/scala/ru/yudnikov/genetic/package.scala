import scala.util.Random
import scala.math.BigDecimal

import Function.tupled

package object genetic {

  private val r = new Random()
  
  object Chromosome {
    def fatum(percent: Int = 50): Boolean = {
      r.nextInt(100) < percent
    }
    def random(length: Int): Seq[Int] = {
      1 to length map { _ =>
        if (fatum()) 1 else 0
      }
    }
    def crossover(a: Seq[Int], b: Seq[Int]): Seq[Int] = {
      a.zip(b) map tupled { (a, b) => 
        if (fatum()) a else b
      }
    }
    def mutate(chromosome: Seq[Int], rate: BigDecimal) = {
      chromosome map { gene =>
        if (fatum((rate * 100).toInt)) 1 else 0
      }
    }
  }

  case class Population(individuals: Seq[Individual]) {
    lazy val fitness: BigDecimal = individuals.map(_.fitness).sum
    lazy val ordered: Population = {
      Population(individuals.sortBy(_.fitness).reverse)
    }
    def crossovered(rate: BigDecimal, eliteSize: Int): Population = {
      val crossoveredIndividuals = individuals map { p1 => 
        val p2 = select()
        Individual(Chromosome.crossover(p1.chromosome, p2.chromosome))
      }
      Population(crossoveredIndividuals)
    }
    def select(
      individuals: Seq[Individual] = ordered.individuals, 
      target: Int = r.nextInt((fitness * 100).toInt), 
      agg: BigDecimal = 0
    ): Individual = {
      println(s"target = $target\nselecting from: ${individuals.length}")
      individuals match {
        case Nil => individuals.last
        case h :: Nil => h
        case list if agg >= target => list.head
        case list => select(list.tail, target, agg + list.head.fitness * 100)  
        
      }
    }
  }

  object Population {
    def apply(populationSize: Int, chromosomeLength: Int): Population = {
      val individuals = 1 to populationSize map { _ =>
        Individual(chromosomeLength)
      }
      new Population(individuals)
    }
  }

  case class Individual(chromosome: Seq[Int]) {
    val fitness: BigDecimal = {
      BigDecimal(chromosome.filter(_ == 1).length) / chromosome.length
    }
  }

  object Individual {
    def apply(length: Int): Individual = new Individual(Chromosome.random(length))
    //def apply(chromosome: Seq[Int]): Individual = ???
    def crossover(a: Individual, b: Individual): Individual = {
      Individual(Chromosome.crossover(a.chromosome, b.chromosome))
    }
  }

  case class GeneticAlgorithm(
    population: Population, 
    crossoverRate: BigDecimal, 
    mutationRate: BigDecimal, 
    eliteSize: Int) {
      def isSolved: Boolean = population.individuals.exists(_.fitness == 1)
      def evaluate: GeneticAlgorithm = {
        val p = population.ordered.crossovered(crossoverRate, eliteSize)
        this.copy(population = p)
      }
    }

}