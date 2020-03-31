package jude.plugins.literals

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins._
import nsc.transform._

class JudeLiterals(val global: Global) extends Plugin {
  import global._

  val name = "literals"
  val description = "converts scala literal values into jude literal values"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with TypingTransformers {
    val global: JudeLiterals.this.global.type = JudeLiterals.this.global
    // val runsAfter = List("parser")
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    val runsAfter = List[String]("parser")
    val phaseName = JudeLiterals.this.name
    def newPhase(_prev: Phase) = new JudeLiteralsPhase(_prev)

    class JudeLiteralsTransformer(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree) = tree match {
        case Literal(Constant(null)) =>
          global.reporter.error(
            tree.pos,
            "literal 'null' values are not supported. Instead use 'unsafe.nullValue'"
          )
          tree
        case Ident(TermName("_root_$u002Ejude$u002Eunsafe$u002EnullValue")) =>
          Literal(Constant(null))
        case Literal(Constant(_: Boolean)) =>
          q"""_root_.jude.Boolean($tree)"""
        case Literal(Constant(_: Float)) =>
          q"""_root_.jude.f32($tree)"""
        case Literal(Constant(_: Double)) =>
          q"""_root_.jude.f64($tree)"""
        case Literal(Constant(_: Long)) =>
          q"""_root_.jude.i64($tree)"""
        case Literal(Constant(_: Int)) =>
          q"""_root_.jude.i32($tree)"""
        case Literal(Constant(_: String)) =>
          q"""_root_.jude.String($tree)"""

        case If(condition, thenPart, elsePart) =>
          val newCondition = q"""(${transform(condition)}).toScalaPrimitive"""
          If(newCondition, transform(thenPart), transform(elsePart))
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
      new JudeLiteralsTransformer(unit)

    class JudeLiteralsPhase(prev: Phase) extends StdPhase(prev) {

      type PublicCompilationUnit = CompilationUnit
      override def name = JudeLiterals.this.name

      override def apply(unit: CompilationUnit): Unit =
        unit.body = new JudeLiteralsTransformer(unit).transform(unit.body)

    }
  }
}
