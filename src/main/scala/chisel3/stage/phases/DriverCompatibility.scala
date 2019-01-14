// See LICENSE for license details.

package chisel3.stage.phases

import firrtl.{AnnotationSeq, ExecutionOptionsManager, HasFirrtlOptions}
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{OutputAnnotationFileAnnotation, Phase}
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation

import chisel3.HasChiselExecutionOptions
import chisel3.stage.{NoRunFirrtlCompilerAnnotation, ChiselOutputFileAnnotation}

/** This provides components of a compatibility wrapper around Chisel's deprecated [[chisel.Driver Driver]].
  *
  * Primarily, this object includes [[firrtl.options.Phase Phase]]s that generate [[firrtl.annotation.Annotation
  * Annotation]]s derived from the deprecated [[firrtl.options.phases.DriverCompatibility.TopNameAnnotation
  * TopNameAnnotation]].
  */
object DriverCompatibility {

  /** Adds a [[ChiselOutputFileAnnotation]] derived from a [[TopNameAnnotation]] if no [[ChiselOutputFileAnnotation]]
    * already exists. If no [[TopNameAnnotation]] exists, then no [[OutputFileAnnotation]] is added. ''This is not a
    * replacement for [[chisel3.stage.phases.AddImplicitOutputFile AddImplicitOutputFile]] as this only adds an output
    * file based on a discovered top name and not on a discovered elaborated circuit.'' Consequently, this will provide
    * the correct behavior before a circuit has been elaborated.
    * @note the output suffix is unspecified and will be set by [[chisel3.stage.phases.EmitCircuit EmitCircuit]]
    */
  private [chisel3] class AddImplicitOutputFile extends Phase {

    def transform(annotations: AnnotationSeq): AnnotationSeq = {
      val hasOutputFile = annotations
        .collectFirst{ case a: ChiselOutputFileAnnotation => a }
        .isDefined
      lazy val top = annotations.collectFirst{ case TopNameAnnotation(a) => a }

      if (!hasOutputFile && top.isDefined) {
        ChiselOutputFileAnnotation(top.get) +: annotations
      } else {
        annotations
      }
    }
  }

  /** If a [[firrtl.options.OutputAnnotationFileAnnotation OutputAnnotationFileAnnotation]] does not exist, this adds one
    * derived from a [[TopNameAnnotation]]. ''This is not a replacement for
    * [[chisel3.stage.phases.AddImplicitOutputAnnotationFile AddImplicitOutputAnnotationFile]] as this only adds an
    * output annotation file based on a discovered top name.'' Consequently, this will provide the correct behavior
    * before a circuit has been elaborated.
    * @note the output suffix is unspecified and will be set by [[firrtl.options.phases.WriteOutputAnnotations
    * WriteOutputAnnotations]]
    */
  private [chisel3] class AddImplicitOutputAnnotationFile extends Phase {

    def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
      .collectFirst{ case a: OutputAnnotationFileAnnotation => annotations }
      .getOrElse{
        val top = annotations.collectFirst{ case TopNameAnnotation(top) => top}

        if (top.isDefined) {
          OutputAnnotationFileAnnotation(top.get) +: annotations
        } else {
          annotations
        }
      }
  }

  private [chisel3] case object RunFirrtlCompilerAnnotation extends NoTargetAnnotation

  /** Disables the execution of [[firrtl.stage.FirrtlStage FirrtlStage]]. This can be used to call
    * [[chisel3.stage.ChiselStage ChiselStage]] and guarantee that the FIRRTL compiler will not run. This is necessary
    * for certain [[chisel3.Driver Driver]] compatibility situations where you need to do something between Chisel
    * compilation and FIRRTL compilations, e.g., update a mutable data structure.
    */
  private [chisel3] class DisableFirrtlStage extends Phase {

    def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
      .collectFirst { case NoRunFirrtlCompilerAnnotation => annotations                              }
      .getOrElse    { Seq(RunFirrtlCompilerAnnotation, NoRunFirrtlCompilerAnnotation) ++ annotations }
  }

  private [chisel3] class ReEnableFirrtlStage extends Phase {

    def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
      .collectFirst { case RunFirrtlCompilerAnnotation =>
        val a: AnnotationSeq = annotations.filter {
          case NoRunFirrtlCompilerAnnotation | RunFirrtlCompilerAnnotation => false
          case _                                                           => true
        }
        a
      }
      .getOrElse{ annotations }

  }

  /** Mutate an input [[firrtl.ExecutionOptionsManager ExecutionOptionsManager]] based on information encoded in an
    * [[firrtl.AnnotationSeq AnnotationSeq]]. This is intended to be run between [[chisel3.stage.ChiselStage
    * ChiselStage]] and [[firrtl.stage.FirrtlStage FirrtlStage]] if you want to have backwards compatibility with an
    * [[firrtl.ExecutionOptionsManager ExecutionOptionsManager]].
    */
  private [chisel3] class MutateOptionsManager(
    optionsManager: ExecutionOptionsManager with HasChiselExecutionOptions with HasFirrtlOptions) extends Phase {

    def transform(annotations: AnnotationSeq): AnnotationSeq = {

      val firrtlCircuit = annotations.collectFirst{ case FirrtlCircuitAnnotation(a) => a }
      optionsManager.firrtlOptions = optionsManager.firrtlOptions.copy(
        firrtlCircuit = firrtlCircuit,
        annotations = optionsManager.firrtlOptions.annotations ++ annotations,
        customTransforms = optionsManager.firrtlOptions.customTransforms ++
          annotations.collect{ case RunFirrtlTransformAnnotation(a) => a } )

      annotations

    }

  }

}
