package apbufgen

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.functional.syntax._

object APBufGen {

  case class Location(lat: Double, long: Double)
  case class Resident(name: String, age: Int, role: Option[String])
  case class Place(name: String, location: Location, residents: Seq[Resident])

  implicit val locationReads: Reads[Location] = (
    (JsPath \ "lat").read[Double](DoubleReads) and
      (JsPath \ "long").read[Double](DoubleReads)
  )(Location.apply _)

  implicit val residentReads: Reads[Resident] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "age").read[Int] and
      (JsPath \ "role").readNullable[String]
  )(Resident.apply _)

  implicit val placeReads: Reads[Place] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "location").read[Location] and
      (JsPath \ "residents").read[Seq[Resident]]
  )(Place.apply _)

  def intReads() = { IntReads }

  def intWrites() = { IntWrites }

  case class CircleInfo[Cnum, Rnum](center: Position[Cnum], radius: Rnum)

  def circleInfoReads[Cnum, Rnum](local_param_cnum: Reads[Cnum], local_param_rnum: Reads[Rnum]) = { (
    (JsPath \ "center").read[Position[Cnum]](positionReads(local_param_cnum)) and
      (JsPath \ "radius").read[Rnum](local_param_rnum)
  )(CircleInfo.apply[Cnum, Rnum] _) }


  def circleInfoWrites[cnum, rnum](local_param_cnum: Writes[cnum], local_param_rnum: Writes[rnum]): Writes[CircleInfo[cnum, rnum]] = { ((JsPath \ "center").write[Position[cnum]](positionWrites(local_param_cnum)) and (JsPath \ "radius").write[rnum](local_param_rnum))(unlift(CircleInfo.unapply[cnum, rnum])) }


  sealed trait Geometry
case class Circle(arg: CircleInfo[Int, Rational]) extends Geometry
case class Rectangle(arg: RectangleInfo[Int]) extends Geometry


  def geometryReads(): Reads[Geometry] = { (JsPath \ "label").read[String].flatMap { (label: String) => label match { case "Circle" => (JsPath \ "arg").read[CircleInfo[Int, Rational]](circleInfoReads(intReads(), rationalReads())).flatMap { (x) => Reads.pure(Circle(x)) } case "Rectangle" => (JsPath \ "arg").read[RectangleInfo[Int]](rectangleInfoReads(intReads())).flatMap { (x) => Reads.pure(Rectangle(x)) } }} }


  def geometryWrites(): Writes[Geometry] = {
    (Writes.apply { (x: Geometry) =>
      x match {
        case Circle(arg) =>
          Json.obj(
            "label" -> JsString("Circle"),
            "arg" -> Json.toJson(arg)(circleInfoWrites(intWrites(), rationalWrites()))
          )
/*
          val writer =
            (
              (JsPath \ "label").write[String]("Circle")(StringWrites) and
                (JsPath \ "arg").write[CircleInfo[Int, Rational]](arg)(circleInfoWrites(intWrites(), rationalWrites()))
            )(unlift(CircleInfo.unapply[Int, Rational]));
          Json.toJson(x)(writer)
 */
        case Rectangle(arg) =>
          Json.obj(
            "label" -> JsString("Circle"),
            "arg" -> Json.toJson(arg)(rectangleInfoWrites(intWrites()))
          )
/*
          val writer =
            (
              (JsPath \ "label").write[String]("Rectangle")(StringWrites) and
                (JsPath \ "arg").write[RectangleInfo[Int]](arg)(rectangleInfoWrites(intWrites()))
            )(unlift(RectangleInfo.unapply[Int]));
          Json.toJson(x)(writer)
 */
      }
    })
  }


  case class Position[num](x: num, y: num)


  def positionReads[num](local_param_num: Reads[num]): Reads[Position[num]] = { ((JsPath \ "x").read[num](local_param_num) and (JsPath \ "y").read[num](local_param_num))(Position.apply[num] _) }


  def positionWrites[Num](local_param_num: Writes[Num]): Writes[Position[Num]] = { ((JsPath \ "x").write[Num](local_param_num) and (JsPath \ "y").write[Num](local_param_num))(unlift(Position.unapply[Num])) }


  case class Rational(denominator: Int, numerator: Int)


  def rationalReads(): Reads[Rational] = { ((JsPath \ "denominator").read[Int](intReads()) and (JsPath \ "numerator").read[Int](intReads()))(Rational.apply _) }


  def rationalWrites(): Writes[Rational] = { ((JsPath \ "denominator").write[Int](intWrites()) and (JsPath \ "numerator").write[Int](intWrites()))(unlift(Rational.unapply)) }


  case class RectangleInfo[num](lowerRight: Position[num], upperLeft: Position[num])


  def rectangleInfoReads[num](local_param_num: Reads[num]): Reads[RectangleInfo[num]] = { ((JsPath \ "lowerRight").read[Position[num]](positionReads(local_param_num)) and (JsPath \ "upperLeft").read[Position[num]](positionReads(local_param_num)))(RectangleInfo.apply[num] _) }


  def rectangleInfoWrites[num](local_param_num: Writes[num]): Writes[RectangleInfo[num]] = { ((JsPath \ "lowerRight").write[Position[num]](positionWrites(local_param_num)) and (JsPath \ "upperLeft").write[Position[num]](positionWrites(local_param_num)))(unlift(RectangleInfo.unapply[num])) }


}
