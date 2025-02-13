package smithy4s.example

import smithy4s.Schema
import smithy4s.Hints
import smithy4s.schema.Schema.string
import smithy4s.ShapeId
import smithy4s.schema.Schema.struct
import smithy4s.ShapeTag

case class PutStreamedObjectInput(key: String)
object PutStreamedObjectInput extends ShapeTag.Companion[PutStreamedObjectInput] {
  val id: ShapeId = ShapeId("smithy4s.example", "PutStreamedObjectInput")

  val hints : Hints = Hints.empty

  implicit val schema: Schema[PutStreamedObjectInput] = struct(
    string.required[PutStreamedObjectInput]("key", _.key).addHints(smithy.api.Required()),
  ){
    PutStreamedObjectInput.apply
  }.withId(id).addHints(hints)
}