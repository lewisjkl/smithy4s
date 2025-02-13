package smithy4s.example

import smithy4s.Schema
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.schema.Schema.struct
import smithy4s.schema.Schema.recursive
import smithy4s.ShapeTag

case class TestTrait(orderType: Option[OrderType] = None)
object TestTrait extends ShapeTag.Companion[TestTrait] {
  val id: ShapeId = ShapeId("smithy4s.example", "testTrait")

  val hints : Hints = Hints(
    smithy.api.Trait(selector = None, structurallyExclusive = None, conflicts = None, breakingChanges = None),
  )

  implicit val schema: Schema[TestTrait] = recursive(struct(
    OrderType.schema.optional[TestTrait]("orderType", _.orderType),
  ){
    TestTrait.apply
  }.withId(id).addHints(hints))
}