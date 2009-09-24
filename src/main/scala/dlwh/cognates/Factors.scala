package dlwh.cognates;

trait Factors {
  // Either a marginal or a message
  trait Marginal {
    def *(m: Marginal): Marginal;
  }

  trait EdgeFactor {
    def childMarginalize(c: Marginal): Marginal;
    def parentMarginalize(p: Marginal): Marginal;
  }

  def edgeFor(parent: String, child: String): EdgeFactor;
  def initialBeliefs(c: String): Marginal;

}
