package jude.plugins.renamer

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins._
import nsc.transform._

class JudeRenamer(val global: Global) extends Plugin {
  import global._

  val name = "Renamer"
  val description =
    "renames the == and != so that the default implementations can't be used"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with TypingTransformers {
    val global: JudeRenamer.this.global.type = JudeRenamer.this.global
    val runsAfter = List[String]("parser")
    val phaseName = JudeRenamer.this.name
    def newPhase(_prev: Phase) = new JudeRenamerPhase(_prev)

    def quote(s: String): (String, String) =
      s -> s"'$s'"

    def extern(s: String): (String, String) =
      ("extern$u0020" + s) -> s

    val mappings: Map[String, String] = Map(
      quote("$eq$eq"),
      quote("$bang$eq"),
      quote("equals"),
      quote("toString"),
      extern("equals"),
      extern("toString")
    )

    class JudeRenamerTransformer(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree) = tree match {
        case Select(lhs, TermName(id)) if mappings.contains(id) =>
          Select(transform(lhs), TermName(mappings(id)))

        case Ident(TermName(id)) if mappings.contains(id) =>
          Ident(TermName(mappings(id)))
        case DefDef(
            modifiers,
            TermName(id),
            tparams,
            params,
            retType,
            rhs
            ) if mappings.contains(id) =>
          DefDef(
            modifiers,
            TermName(mappings(id)),
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
      new JudeRenamerTransformer(unit)

    class JudeRenamerPhase(prev: Phase) extends StdPhase(prev) {

      type PublicCompilationUnit = CompilationUnit
      override def name = JudeRenamer.this.name

      override def apply(unit: CompilationUnit): Unit = {
        unit.body = new JudeRenamerTransformer(unit).transform(unit.body)
      }

    }
  }
}
