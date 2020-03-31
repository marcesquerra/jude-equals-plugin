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

    class JudeEqualsTransformer(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree) = tree match {
        case Apply(Select(lhs, TermName("$eq$eq")), List(rhs)) =>
          Apply(
            Select(transform(lhs), TermName("$eq$eq$")),
            List(transform(rhs))
          )
        case Apply(Select(lhs, TermName("$bang$eq")), List(rhs)) =>
          Apply(
            Select(transform(lhs), TermName("$bang$eq$")),
            List(transform(rhs))
          )
        case DefDef(
            modifiers,
            TermName("$eq$eq"),
            tparams,
            List(List(param)),
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName("$eq$eq$"),
            tparams,
            List(List(param)),
            retType,
            transform(rhs)
          )
        case DefDef(
            modifiers,
            TermName("$bang$eq"),
            tparams,
            List(List(param)),
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName("$bang$eq$"),
            tparams,
            List(List(param)),
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
