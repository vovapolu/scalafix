/*
rules = DisableSelect

DisableSelect.symbols = [
  {
    qualType = "scala.Array"
    name = "equals"
  }
]
 */
package test

object DisableSelect {
  def f(is: Int*): Array[Int] = is.toArray

  f(1, 2, 3).equals(f(4, 5, 6))
}
