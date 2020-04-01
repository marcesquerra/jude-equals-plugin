package jude.plugins.equals

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins._
import nsc.transform._

class JudeEquals(val global: Global) extends Plugin {
  import global._

  val name = "equals"
  val description =
    "renames the == and != so that the default implementations can't be used"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with TypingTransformers {
    val global: JudeEquals.this.global.type = JudeEquals.this.global
    val runsAfter = List[String]("parser")
    val phaseName = JudeEquals.this.name
    def newPhase(_prev: Phase) = new JudeEqualsPhase(_prev)

    val EQ = "$eq$eq"
    val EQJ = "'$eq$eq'"
    val NEQ = "$bang$eq"
    val NEQJ = "'$bang$eq'"

    val EQUALS = "equals"
    val EQUALSJ = "'equals'"
    val SCALA_EQUALS = "extern$u0020equals"

    class JudeEqualsTransformer(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree) = tree match {
        case Apply(Select(lhs, TermName(EQ)), rhs) =>
          Apply(
            Select(transform(lhs), TermName(EQJ)),
            rhs.map(transform)
          )
        case Apply(Select(lhs, TermName(NEQ)), rhs) =>
          Apply(
            Select(transform(lhs), TermName(NEQJ)),
            rhs.map(transform)
          )
        case Apply(Select(lhs, TermName(EQUALS)), rhs) =>
          Apply(
            Select(transform(lhs), TermName(EQUALSJ)),
            rhs.map(transform)
          )
        case Apply(Select(lhs, TermName(SCALA_EQUALS)), rhs) =>
          Apply(
            Select(transform(lhs), TermName(EQUALS)),
            rhs.map(transform)
          )
        case Ident(TermName(EQ)) =>
          Ident(TermName(EQJ))
        case Ident(TermName(NEQ)) =>
          Ident(TermName(NEQJ))
        case Ident(TermName(EQUALS)) =>
          Ident(TermName(EQUALSJ))
        case Ident(TermName(SCALA_EQUALS)) =>
          Ident(TermName(EQUALS))
        case DefDef(
            modifiers,
            TermName(EQ),
            tparams,
            params,
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName(EQJ),
            tparams,
            params,
            retType,
            transform(rhs)
          )
        case DefDef(
            modifiers,
            TermName(NEQ),
            tparams,
            params,
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName(NEQJ),
            tparams,
            params,
            retType,
            transform(rhs)
          )
        case DefDef(
            modifiers,
            TermName(EQUALS),
            tparams,
            params,
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName(EQUALSJ),
            tparams,
            params,
            retType,
            transform(rhs)
          )
        case DefDef(
            modifiers,
            TermName(SCALA_EQUALS),
            tparams,
            params,
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName(EQUALS),
            tparams,
            params,
            retType,
            transform(rhs)
          )
        case _ =>
          // I'll keep this in here. It's good to run experiments
          // println(s"""|
          //   |=================
          //   |$tree
          //   |-----------------
          //   |${showRaw(tree)}
          //   |=================
          //   |""".stripMargin)
          super.transform(tree)
      }
    }

    def newTransformer(unit: CompilationUnit) =
      new JudeEqualsTransformer(unit)

    class JudeEqualsPhase(prev: Phase) extends StdPhase(prev) {

      type PublicCompilationUnit = CompilationUnit
      override def name = JudeEquals.this.name

      override def apply(unit: CompilationUnit): Unit =
        unit.body = new JudeEqualsTransformer(unit).transform(unit.body)

    }
  }
}
