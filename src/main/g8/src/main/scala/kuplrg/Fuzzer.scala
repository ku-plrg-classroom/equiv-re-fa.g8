package kuplrg

import scala.math.*
import scala.util.Random

object Fuzzer {

  def fuzzDFA(
    numStates: Int,
    symbols: String,
  ): DFA =
    val n: Int = numStates
    val symbolSet: Set[Char] = symbols.toSet
    val m: Int = symbolSet.size
    val trans: Int = randInt(n, n * m)
    val finalStates: Int = randInt(2, n)
    DFA(numStates, symbols, trans, finalStates)

  def fuzzRE(depth: Int, symbols: String): RE =
    fuzzRE(depth, symbols.toSet.toList)
  def fuzzRE(depth: Int, symbols: List[Symbol]): RE =
    if (depth == 0 || randBool(0.1)) fuzzREBase(symbols)
    else fuzzREInd(depth, symbols)

  def fuzzREBase(symbols: List[Symbol]): RE = randInt(3) match
    case 0 => REEmpty()
    case 1 => REEpsilon()
    case 2 => RESymbol(symbols(randInt(symbols.length)))

  def fuzzREInd(depth: Int, symbols: List[Symbol]): RE = randInt(4) match
    case 0 => REUnion(fuzzRE(depth - 1, symbols), fuzzRE(depth - 1, symbols))
    case 1 => REConcat(fuzzRE(depth - 1, symbols), fuzzRE(depth - 1, symbols))
    case 2 => REStar(fuzzRE(depth - 1, symbols))
    case 3 => REParen(fuzzRE(depth - 1, symbols))

  def randBool: Boolean = rand.nextBoolean
  def randBool(probForTrue: Double): Boolean = rand.nextDouble < probForTrue
  def randInt(base: Int, exp: Int): Int = randInt(pow(base, exp).toInt)
  def randInt(bound: Int): Int = rand.nextInt(bound)

  val rand = new scala.util.Random

  fuzzDFA(5, "aba")
}
