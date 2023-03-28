package kuplrg

object Implementation extends Template {

  // Convert a regular expression to a epsilon-NFA
  def re2enfa(re: RE): ENFA =
    val SimpleENFA(from, trans, to) = re2senfa(re, 1)
    val states = (from to to).toSet
    val symbols = trans.flatMap((_, aOpt, _) => aOpt)
    val map = trans.groupMap((i, aOpt, _) => (i, aOpt))((_, _, j) => j)
    val enfaTrans = states
      .flatMap(q => symbols.map(a => q -> Option(a)) + (q -> None))
      .map(pair => pair -> map.getOrElse(pair, Set()))
      .toMap
    ENFA(
      states = states,
      symbols = symbols,
      trans = enfaTrans,
      initState = from,
      finalStates = Set(to),
    )

  // Convert a regular expression `re` to a simplified epsilon-NFA with an
  // initial state `i`.
  def re2senfa(re: RE, i: State): SimpleENFA = re match
    case REEmpty() => ???
    case REEpsilon() => ???
    case RESymbol(symbol) => ???
    case REUnion(re1, re2) => ???
    case REConcat(re1, re2) => ???
    case REStar(re) => ???
    case REParen(re) => ???

  // Convert a DFA to a regular expression
  def dfa2re(givenDFA: DFA, debug: Boolean = false): RE =
    val dfa = givenDFA.normalized

    // Show details in the conversion from a DFA to a regular expression
    if (debug)
      show("* Details in the conversion from a DFA to a regular expression:")
      val n = dfa.states.size
      for (k <- 0 to n; i <- 1 to n; j <- 1 to n)
        val re = reForPaths(dfa)(i, j, k)
        println(s"  - ($i, $j, $k) -> ${re.stringForm}")

    ???

  // A regular expression accepting paths from `i` to `j` with intermediate
  // states bounded by `k` in a given DFA `dfa`. Assume that the given DFA
  // `dfa` is already normalized (i.e., the states of DFA are 1, 2, ..., n).
  def reForPaths(dfa: DFA)(i: State, j: State, k: State): RE = k match
    case 0 => ???
    case _ => ???
}
