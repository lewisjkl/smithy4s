package smithy4s.example.imp

import smithy4s.Errorable
import smithy4s.example.import_test.OpOutput
import smithy4s.Schema
import smithy4s.schema.Schema.unit
import smithy4s.Transformation
import smithy4s.Monadic
import smithy4s.Service
import smithy4s.ShapeTag
import smithy4s.schema.Schema.bijection
import smithy4s.example.error.NotFoundError
import smithy4s.schema.Schema.union
import smithy4s.schema.Schema.UnionSchema
import smithy4s.Hints
import smithy4s.StreamingSchema
import smithy4s.ShapeId
import smithy4s.Endpoint

trait ImportServiceGen[F[_, _, _, _, _]] {
  self =>

  def importOperation() : F[Unit, ImportServiceGen.ImportOperationError, OpOutput, Nothing, Nothing]

  def transform[G[_, _, _, _, _]](transformation : Transformation[F, G]) : ImportServiceGen[G] = new Transformed(transformation)
  class Transformed[G[_, _, _, _, _]](transformation : Transformation[F, G]) extends ImportServiceGen[G] {
    def importOperation() = transformation[Unit, ImportServiceGen.ImportOperationError, OpOutput, Nothing, Nothing](self.importOperation())
  }
}

object ImportServiceGen extends Service[ImportServiceGen, ImportServiceOperation] {

  def apply[F[_]](implicit F: Monadic[ImportServiceGen, F]): F.type = F

  val id: ShapeId = ShapeId("smithy4s.example.imp", "ImportService")

  val hints : Hints = Hints(
    alloy.SimpleRestJson(),
  )

  val endpoints: List[Endpoint[ImportServiceOperation, _, _, _, _, _]] = List(
    ImportOperation,
  )

  val version: String = "1.0.0"

  def endpoint[I, E, O, SI, SO](op : ImportServiceOperation[I, E, O, SI, SO]) = op match {
    case ImportOperation() => ((), ImportOperation)
  }

  object reified extends ImportServiceGen[ImportServiceOperation] {
    def importOperation() = ImportOperation()
  }

  def transform[P[_, _, _, _, _]](transformation: Transformation[ImportServiceOperation, P]): ImportServiceGen[P] = reified.transform(transformation)

  def transform[P[_, _, _, _, _], P1[_, _, _, _, _]](alg: ImportServiceGen[P], transformation: Transformation[P, P1]): ImportServiceGen[P1] = alg.transform(transformation)

  def asTransformation[P[_, _, _, _, _]](impl : ImportServiceGen[P]): Transformation[ImportServiceOperation, P] = new Transformation[ImportServiceOperation, P] {
    def apply[I, E, O, SI, SO](op : ImportServiceOperation[I, E, O, SI, SO]) : P[I, E, O, SI, SO] = op match  {
      case ImportOperation() => impl.importOperation()
    }
  }
  case class ImportOperation() extends ImportServiceOperation[Unit, ImportServiceGen.ImportOperationError, OpOutput, Nothing, Nothing]
  object ImportOperation extends Endpoint[ImportServiceOperation, Unit, ImportServiceGen.ImportOperationError, OpOutput, Nothing, Nothing] with Errorable[ImportOperationError] {
    val id: ShapeId = ShapeId("smithy4s.example.import_test", "ImportOperation")
    val input: Schema[Unit] = unit.addHints(smithy4s.internals.InputOutput.Input.widen)
    val output: Schema[OpOutput] = OpOutput.schema.addHints(smithy4s.internals.InputOutput.Output.widen)
    val streamedInput : StreamingSchema[Nothing] = StreamingSchema.nothing
    val streamedOutput : StreamingSchema[Nothing] = StreamingSchema.nothing
    val hints : Hints = Hints(
      smithy.api.Http(method = smithy.api.NonEmptyString("GET"), uri = smithy.api.NonEmptyString("/test"), code = 200),
    )
    def wrap(input: Unit) = ImportOperation()
    override val errorable: Option[Errorable[ImportOperationError]] = Some(this)
    val error: UnionSchema[ImportOperationError] = ImportOperationError.schema
    def liftError(throwable: Throwable) : Option[ImportOperationError] = throwable match {
      case e: NotFoundError => Some(ImportOperationError.NotFoundErrorCase(e))
      case _ => None
    }
    def unliftError(e: ImportOperationError) : Throwable = e match {
      case ImportOperationError.NotFoundErrorCase(e) => e
    }
  }
  sealed trait ImportOperationError extends scala.Product with scala.Serializable {
    @inline final def widen: ImportOperationError = this
  }
  object ImportOperationError extends ShapeTag.Companion[ImportOperationError] {
    val id: ShapeId = ShapeId("smithy4s.example.imp", "ImportOperationError")

    val hints : Hints = Hints.empty

    case class NotFoundErrorCase(notFoundError: NotFoundError) extends ImportOperationError

    object NotFoundErrorCase {
      val hints : Hints = Hints.empty
      val schema: Schema[NotFoundErrorCase] = bijection(NotFoundError.schema.addHints(hints), NotFoundErrorCase(_), _.notFoundError)
      val alt = schema.oneOf[ImportOperationError]("NotFoundError")
    }

    implicit val schema: UnionSchema[ImportOperationError] = union(
      NotFoundErrorCase.alt,
    ){
      case c : NotFoundErrorCase => NotFoundErrorCase.alt(c)
    }
  }
}

sealed trait ImportServiceOperation[Input, Err, Output, StreamedInput, StreamedOutput]
