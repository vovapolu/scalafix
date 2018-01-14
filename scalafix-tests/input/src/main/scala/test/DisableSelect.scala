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
  Array(1, 2, 3).equals(Array(4, 5, 6))
}
