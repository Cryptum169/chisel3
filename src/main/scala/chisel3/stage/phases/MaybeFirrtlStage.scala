// See LICENSE for license details.

package chisel3.stage.phases

import chisel3.stage.NoRunFirrtlCompilerAnnotation

import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.stage.FirrtlStage

/** Run [[firrt.stage.FirrtlStage FirrtlStage]] if a [[firrtl.stage.NoRunFirrtlCompilerAnnotation
  * NoRunFirrtlCompilerAnnotation]] is not present.
  */
class MaybeFirrtlStage extends Phase {

  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
    .collectFirst { case NoRunFirrtlCompilerAnnotation => annotations }
    .getOrElse    { (new FirrtlStage).transform(annotations)          }

}
