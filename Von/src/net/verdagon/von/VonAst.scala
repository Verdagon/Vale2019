package net.verdagon.von

import net.verdagon.vale.metal.FullNameH
import net.verdagon.vale.vassert

sealed trait IVonData

case class VonInt(value: Int) extends IVonData
case class VonFloat(value: Float) extends IVonData
case class VonBool(value: Boolean) extends IVonData
case class VonStr(value: String) extends IVonData

case class VonReference(id: String) extends IVonData

case class VonObject(
  tyype: String,
  id: Option[String],
  members: Vector[VonMember]
) extends IVonData
case class VonMember(
    memberIndex: Option[Int],
    fieldName: Option[String],
    value: IVonData) {
  vassert(memberIndex.nonEmpty || fieldName.nonEmpty)
}

case class VonArray(
  id: Option[String],
  members: Vector[IVonData]
) extends IVonData

case class VonListMap(
  id: Option[String],
  members: Vector[VonMapEntry]
) extends IVonData

case class VonMap(
  id: Option[String],
  members: Vector[VonMapEntry]
) extends IVonData

case class VonMapEntry(
  key: IVonData,
  value: IVonData,
)
