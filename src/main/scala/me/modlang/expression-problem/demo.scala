package me
package modlang
package expression_problem

// Read source [9] from "The extension problem revisited" by Mads Torgensen and
// add functional approaches here. Solutions for immutable objects only are
// allowed.
// More mentioned in paper:
// - “deep subtyping” [8]
// - “classgroups” [9]
// - “exten-sible algebraic datatypes” [10]

def demo() =
  println("Extension problem")
  class_model.test()
  union_model.test()
  visitor_model.test()
  traits_model.test()
  tfi_model.test()

