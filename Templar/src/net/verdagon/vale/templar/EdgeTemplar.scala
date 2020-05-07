package net.verdagon.vale.templar

import net.verdagon.vale.astronomer.{GlobalFunctionFamilyNameA, IImpreciseNameStepA, INameA}
import net.verdagon.vale.templar.templata.{FunctionBanner2, Override2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail}

object EdgeTemplar {
  case class NeededOverride(name: GlobalFunctionFamilyNameA, paramFilters: List[ParamFilter])

  def assembleEdges(
    functions: List[net.verdagon.vale.templar.Function2],
    interfaces: List[InterfaceDefinition2],
    impls: List[Impl2]):
  List[NeededOverride] = {

    val abstractFunctionsByInterfaceWithoutEmpties =
      functions.flatMap(function => {
        function.header.getAbstractInterface match {
          case None => List()
          case Some(abstractInterface) => List(abstractInterface -> function)
        }
      })
        .groupBy(_._1)
        .mapValues(_.map(_._2))
    // Some interfaces would be empty and they wouldn't be in
    // abstractFunctionsByInterfaceWithoutEmpties, so we add them here.
    val abstractFunctionsByInterface =
    abstractFunctionsByInterfaceWithoutEmpties ++
      interfaces.map(i => {
        (i.getRef -> abstractFunctionsByInterfaceWithoutEmpties.getOrElse(i.getRef, Set()))
      })

    val interfaceEdgeBlueprints =
      abstractFunctionsByInterface
        .map({ case (interfaceRef2, functions2) =>
          InterfaceEdgeBlueprint(
            interfaceRef2,
            // This is where they're given order and get an implied index
            functions2.map(_.header.toBanner).toList)
        })

    val overrideFunctionsAndIndicesByStructAndInterface =
      functions.flatMap(overrideFunction => {
        overrideFunction.header.getOverride match {
          case None => List()
          case Some((struct, superInterface)) => {
            // Make sure that the struct actually overrides that function
            if (!impls.exists(impl => impl.struct == struct && impl.interface == superInterface)) {
              vfail("Struct " + struct + " doesn't extend " + superInterface + "!")
            }

            val virtualIndex = overrideFunction.header.getVirtualIndex.get
            vassert(virtualIndex >= 0)
            val overrideFunctionParamTypes =
              overrideFunction.header.params.map(_.tyype)
            val needleSuperFunctionParamTypes =
              overrideFunctionParamTypes.zipWithIndex.map({ case (paramType, index) =>
                if (index != virtualIndex) {
                  paramType
                } else {
                  paramType.copy(referend = superInterface)
                }
              })

            // Make sure that there's an abstract function in that edge that this
            // overrides
            val edgeBlueprint =
            interfaceEdgeBlueprints.find(_.interface == superInterface) match {
              case None => {
                // every interface should have a blueprint...
                vfail("wot")
              }
              case Some(ieb) => ieb
            }
            val matchesAndIndices =
              edgeBlueprint.superFamilyRootBanners.zipWithIndex
                .filter({ case (possibleSuperFunction, index) =>
                  val namesMatch =
                    (possibleSuperFunction.fullName.last, overrideFunction.header.fullName.last) match {
                      case (FunctionName2(possibleSuperFunctionHumanName, _, _), FunctionName2(overrideFunctionHumanName, _, _)) => {
                        possibleSuperFunctionHumanName == overrideFunctionHumanName
                      }
                      case _ => false
                    }
                  namesMatch && possibleSuperFunction.paramTypes == needleSuperFunctionParamTypes
                })
            matchesAndIndices match {
              case Nil => {
                vfail("Function " + overrideFunction.header.toSignature + " doesn't override anything in " + superInterface)
              }
              case List((_, indexInEdge)) => {
                List((struct, superInterface) -> (overrideFunction, indexInEdge))
              }
              case _ => {
                vfail("Function " + overrideFunction.header.toSignature + " overrides multiple things in " + superInterface + ": " + matchesAndIndices.map(_._1.toSignature).mkString(", "))
              }
            }
          }
        }
      })
        .groupBy(_._1)
        .mapValues(_.map(_._2))

    val neededBanners =
      overrideFunctionsAndIndicesByStructAndInterface
        .flatMap({ case ((struct, superInterface), overrideFunctionsAndIndices) =>
          val blueprint = interfaceEdgeBlueprints.find(_.interface == superInterface).get
          val overrideFunctionsByIndex =
            overrideFunctionsAndIndices.groupBy(_._2).mapValues(_.map(_._1).toList)
          // overrideIndex is the index in the itable
          blueprint.superFamilyRootBanners.zipWithIndex.flatMap({ case (superFunction, overrideIndex) =>
            overrideFunctionsByIndex.get(overrideIndex) match {
              case None => {
                val overrideParamFilters =
                  superFunction.paramTypes.zipWithIndex.map({
                    case (Coord(ownership, _), index) if index == superFunction.getVirtualIndex.get => {
                      ParamFilter(Coord(ownership, struct), Some(Override2(superInterface)))
                    }
                    case (tyype, _) => ParamFilter(tyype, None)
                  })
                superFunction.fullName.last match {
                  case FunctionName2(humanName, _, _) => List(NeededOverride(GlobalFunctionFamilyNameA(humanName), overrideParamFilters))
                  case _ => List()
                }
              }
              case Some(List()) => vfail("wot")
              case Some(List(_)) => List()
              case Some(multipleOverrides) => {
                vfail("Multiple overrides for struct " + struct + " for interface " + superInterface + ": " + multipleOverrides.map(_.header.toSignature).mkString(", "))
              }
            }
          })
        })

    neededBanners.toList
  }
}
