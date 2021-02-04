package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait EqTests[A] extends Laws {
  def laws: EqLaws[A]

  def eqv(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A]): RuleSet = {
    implicit val eqA: Eq[A] = laws.E

    new DefaultRuleSet(
      "eq",
      None,
      // "reflexivity eq" -> forAll(laws.reflexivityEq _),
      // "symmetry eq" -> forAll(laws.symmetryEq _),
      "antisymmetry eq" -> forAll(laws.antiSymmetryEq _),
      // "transitivity eq" -> forAll(laws.transitivityEq _)
    )
  }
}

object EqTests {
  def apply[A: Eq]: EqTests[A] =
    new EqTests[A] { def laws: EqLaws[A] = EqLaws[A] }
}
