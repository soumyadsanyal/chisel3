// See LICENSE for license details.

package chisel3.core

import chisel3.internal.Builder.pushCommand
import chisel3.internal.firrtl.{Attach, Connect}
import chisel3.internal.throwException
import scala.language.experimental.macros
import chisel3.internal.sourceinfo._

/**
* BiConnect.connect executes a bidirectional connection element-wise.
*
* Note that the arguments are left and right (not source and sink) so the
* intent is for the operation to be commutative.
*
* The connect operation will recurse down the left Data (with the right Data).
* An exception will be thrown if a movement through the left cannot be matched
* in the right (or if the right side has extra fields).
*
* See elemConnect for details on how the root connections are issued.
*
*/

object BiConnect {
  // These are all the possible exceptions that can be thrown.
  case class BiConnectException(message: String) extends Exception(message)
  // These are from element-level connection
  def BothDriversException =
    BiConnectException(": Both Left and Right are drivers")
  def NeitherDriverException =
    BiConnectException(": Neither Left nor Right is a driver")
  def UnknownDriverException =
    BiConnectException(": Locally unclear whether Left or Right (both internal)")
  def UnknownRelationException =
    BiConnectException(": Left or Right unavailable to current module.")
  // These are when recursing down aggregate types
  def MismatchedVecException =
    BiConnectException(": Left and Right are different length Vecs.")
  def MissingLeftFieldException(field: String) =
    BiConnectException(s".$field: Left Bundle missing field ($field).")
  def MissingRightFieldException(field: String) =
    BiConnectException(s": Right Bundle missing field ($field).")
  def MismatchedException(left: String, right: String) =
    BiConnectException(s": Left ($left) and Right ($right) have different types.")

  /** This function is what recursively tries to connect a left and right together
  *
  * There is some cleverness in the use of internal try-catch to catch exceptions
  * during the recursive decent and then rethrow them with extra information added.
  * This gives the user a 'path' to where in the connections things went wrong.
  */
  def connect(sourceInfo: SourceInfo, connectCompileOptions: CompileOptions, left: Data, right: Data, context_mod: Module): Unit =
    (left, right) match {
      // Handle element case (root case)
      case (left_a: Analog, right_a: Analog) => {
        analogAttach(sourceInfo, left_a, right_a, context_mod)
      }
      case (left_e: Element, right_e: Element) => {
        elemConnect(sourceInfo, connectCompileOptions, left_e, right_e, context_mod)
        // TODO(twigg): Verify the element-level classes are connectable
      }
      // Handle Vec case
      case (left_v: Vec[Data @unchecked], right_v: Vec[Data @unchecked]) => {
        if(left_v.length != right_v.length) { throw MismatchedVecException }
        for(idx <- 0 until left_v.length) {
          try {
            connect(sourceInfo, connectCompileOptions, left_v(idx), right_v(idx), context_mod)
          } catch {
            case BiConnectException(message) => throw BiConnectException(s"($idx)$message")
          }
        }
      }
      // Handle Bundle case
      case (left_b: Bundle, right_b: Bundle) => {
        // Verify right has no extra fields that left doesn't have
        for((field, right_sub) <- right_b.elements) {
          if(!left_b.elements.isDefinedAt(field)) {
            if (connectCompileOptions.connectFieldsMustMatch) {
              throw MissingLeftFieldException(field)
            }
          }
        }
        // For each field in left, descend with right
        for((field, left_sub) <- left_b.elements) {
          try {
            right_b.elements.get(field) match {
              case Some(right_sub) => connect(sourceInfo, connectCompileOptions, left_sub, right_sub, context_mod)
              case None => {
                if (connectCompileOptions.connectFieldsMustMatch) {
                  throw MissingRightFieldException(field)
                }
              }
            }
          } catch {
            case BiConnectException(message) => throw BiConnectException(s".$field$message")
          }
        }
      }
      // Left and right are different subtypes of Data so fail
      case (left, right) => throw MismatchedException(left.toString, right.toString)
    }

  // These functions (finally) issue the connection operation
  // Issue with right as sink, left as source
  private def issueConnectL2R(left: Element, right: Element)(implicit sourceInfo: SourceInfo): Unit = {
    pushCommand(Connect(sourceInfo, right.lref, left.ref))
  }
  // Issue with left as sink, right as source
  private def issueConnectR2L(left: Element, right: Element)(implicit sourceInfo: SourceInfo): Unit = {
    pushCommand(Connect(sourceInfo, left.lref, right.ref))
  }

  // This function checks if element-level connection operation allowed.
  // Then it either issues it or throws the appropriate exception.
  def elemConnect(implicit sourceInfo: SourceInfo, connectCompileOptions: CompileOptions, left: Element, right: Element, context_mod: Module): Unit = {
    import Direction.{Input, Output} // Using extensively so import these
    // If left or right have no location, assume in context module
    // This can occur if one of them is a literal, unbound will error previously
    val left_mod: Module  = left.binding.location.getOrElse(context_mod)
    val right_mod: Module = right.binding.location.getOrElse(context_mod)

    val left_direction: Option[Direction] = left.binding.direction
    val right_direction: Option[Direction] = right.binding.direction
    // None means internal

    // CASE: Context is same module as left node and right node is in a child module
    if( (left_mod == context_mod) &&
        (right_mod._parent.map(_ == context_mod).getOrElse(false)) ) {
      // Thus, right node better be a port node and thus have a direction hint
      ((left_direction, right_direction): @unchecked) match {
        //    CURRENT MOD   CHILD MOD
        case (Some(Input),  Some(Input))  => issueConnectL2R(left, right)
        case (None,         Some(Input))  => issueConnectL2R(left, right)

        case (Some(Output), Some(Output)) => issueConnectR2L(left, right)
        case (None,         Some(Output)) => issueConnectR2L(left, right)

        case (Some(Input),  Some(Output)) => throw BothDriversException
        case (Some(Output), Some(Input))  => throw NeitherDriverException
        case (_,            None)         => throw UnknownRelationException
      }
    }

    // CASE: Context is same module as right node and left node is in child module
    else if( (right_mod == context_mod) &&
             (left_mod._parent.map(_ == context_mod).getOrElse(false)) ) {
      // Thus, left node better be a port node and thus have a direction hint
      ((left_direction, right_direction): @unchecked) match {
        //    CHILD MOD     CURRENT MOD
        case (Some(Input),  Some(Input))  => issueConnectR2L(left, right)
        case (Some(Input),  None)         => issueConnectR2L(left, right)

        case (Some(Output), Some(Output)) => issueConnectL2R(left, right)
        case (Some(Output), None)         => issueConnectL2R(left, right)

        case (Some(Input),  Some(Output)) => throw NeitherDriverException
        case (Some(Output), Some(Input))  => throw BothDriversException
        case (None, _)                    => throw UnknownRelationException
      }
    }

    // CASE: Context is same module that both left node and right node are in
    else if( (context_mod == left_mod) && (context_mod == right_mod) ) {
      ((left_direction, right_direction): @unchecked) match {
        //    CURRENT MOD   CURRENT MOD
        case (Some(Input),  Some(Output)) => issueConnectL2R(left, right)
        case (Some(Input),  None)         => issueConnectL2R(left, right)
        case (None,         Some(Output)) => issueConnectL2R(left, right)

        case (Some(Output), Some(Input))  => issueConnectR2L(left, right)
        case (Some(Output), None)         => issueConnectR2L(left, right)
        case (None,         Some(Input))  => issueConnectR2L(left, right)

        case (Some(Input),  Some(Input))  => {
          if (connectCompileOptions.dontAssumeDirectionality) {
            throw BothDriversException
          } else {
            (left.binding, right.binding) match {
              case (PortBinding(_, _), PortBinding(_, _)) => throw BothDriversException
              case (PortBinding(_, _), _) => issueConnectL2R(left, right)
              case (_, PortBinding(_, _)) => issueConnectR2L(left, right)
              case _ => throw BothDriversException
            }
          }
        }
        case (Some(Output), Some(Output)) => {
          if (connectCompileOptions.dontAssumeDirectionality) {
            throw BothDriversException
          } else {
            (left.binding, right.binding) match {
              case (PortBinding(_, _), PortBinding(_, _)) => throw BothDriversException
              case (PortBinding(_, _), _) => issueConnectR2L(left, right)
              case (_, PortBinding(_, _)) => issueConnectL2R(left, right)
              case _ => throw BothDriversException
            }
          }
        }
        case (None,         None)         => {
          if (connectCompileOptions.dontAssumeDirectionality) {
            throw UnknownDriverException
          } else {
            issueConnectR2L(left, right)
          }
        }
      }
    }

    // CASE: Context is the parent module of both the module containing left node
    //                                        and the module containing right node
    //   Note: This includes case when left and right in same module but in parent
    else if( (left_mod._parent.map(_ == context_mod).getOrElse(false)) &&
             (right_mod._parent.map(_ == context_mod).getOrElse(false))
    ) {
      // Thus both nodes must be ports and have a direction hint
      ((left_direction, right_direction): @unchecked) match {
        //    CHILD MOD     CHILD MOD
        case (Some(Input),  Some(Output)) => issueConnectR2L(left, right)
        case (Some(Output), Some(Input))  => issueConnectL2R(left, right)

        case (Some(Input),  Some(Input))  => throw NeitherDriverException
        case (Some(Output), Some(Output)) => throw BothDriversException
        case (_, None)                    =>
          if (connectCompileOptions.dontAssumeDirectionality) {
            throw UnknownRelationException
          } else {
            issueConnectR2L(left, right)
          }
        case (None, _)                    =>
          if (connectCompileOptions.dontAssumeDirectionality) {
            throw UnknownRelationException
          } else {
            issueConnectR2L(left, right)
          }
      }
    }

    // Not quite sure where left and right are compared to current module
    // so just error out
    else throw UnknownRelationException
  }

  // Issue attach for Analog types
  private def issueAttach(source: Element, exprs: Seq[Analog])(implicit sourceInfo: SourceInfo): Unit = {
    pushCommand(Attach(sourceInfo, source.lref, exprs.map(_.ref)))
  }

  // This function checks if analog element-level attaching is allowed
  // Then it either issues it or throws the appropriate exception.
  def analogAttach(implicit sourceInfo: SourceInfo, left: Analog, right: Analog, context_mod: Module): Unit = {
    // TODO check widths?

    // Analogs can only be stitched up, thus they cannot be literals and must be bound to a Module
    val leftMod: Module  = left.binding.location.getOrElse(
      throwException(s"Cannot determine Module containing Left!"))
    val rightMod: Module = right.binding.location.getOrElse(
      throwException(s"Cannot determine Module containing Right!"))

    println(s"Attach analog $left:${left.binding.direction} and $right:${right.binding.direction}")

    // CASE: Context is same module as left node and right node is in a child module
    if ((leftMod == context_mod) &&
        (rightMod._parent.map(_ == context_mod).getOrElse(false))) {
      (left.binding, right.binding) match {
        //    PARENT MOD         CHILD MOD
        case (PortBinding(_, _), PortBinding(_,_)) => issueAttach(left, Seq(right))
        case (WireBinding(_), PortBinding(_,_)) => issueAttach(left, Seq(right))
        case _ => throw UnknownRelationException
      }
    }
    // CASE: Context is same module as right node and left node is in child module
    else if ((rightMod == context_mod) &&
             (leftMod._parent.map(_ == context_mod).getOrElse(false))) {
      (left.binding, right.binding) match {
        //    CHILD MOD          PARENT MOD
        case (PortBinding(_, _), PortBinding(_,_)) => issueAttach(right, Seq(left))
        case (PortBinding(_,_), WireBinding(_)) => issueAttach(right, Seq(left))
        case _ => throw UnknownRelationException
      }
    }
    // CASE: Context is same module that both left node and right node are in
    // TODO This whole case doesn't really work in Firrtl
    else if ((context_mod == leftMod) && (context_mod == rightMod)) {
      (left.binding, right.binding) match {
        //    CURRENT MOD        CURRENT MOD
        case (PortBinding(_, _), PortBinding(_,_)) =>
          //throwException("Attaching two analog ports of the same Module is not currently supported")
          issueAttach(left, Seq(right))
        case (WireBinding(_), PortBinding(_,_)) => issueAttach(left, Seq(right))
        case (PortBinding(_,_), WireBinding(_)) => issueAttach(left, Seq(right))
        case (WireBinding(_), WireBinding(_)) => issueAttach(left, Seq(right))
        case _ => throw UnknownRelationException
      }
    }
    // CASE: Context is the parent module of both the module containing left node
    //                                        and the module containing right node
    //   Note: This includes case when left and right in same module but in parent
    else if ((leftMod._parent.map(_ == context_mod).getOrElse(false)) &&
             (rightMod._parent.map(_ == context_mod).getOrElse(false))) {
      (left.binding, right.binding) match {
        //    CHILD MOD          CHILD MOD
        case (PortBinding(_, _), PortBinding(_,_)) =>
          val wire = Wire(left)
          issueAttach(wire, Seq(left, right))
        // If left and right are not ports, they must be some internal wire
        case (_,_) => throw UnknownRelationException
      }
    }
    else throw UnknownRelationException
  }
}
