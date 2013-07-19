(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.composites )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol CompositeAPI)
(defprotocol Block)
(defprotocol BlockStep)


(deftype Composite [ cache ] Activity CompositeAPI

abstract class Composite extends Activity {

  private val _children= mutable.ArrayBuffer[Activity]()

  def size() = _children.size()

  protected def add(a:Activity ) {
    _children.add(a)
    onAdd(a)
  }

  protected def onAdd(a:Activity ) {}

  protected def reifyInnerSteps(outer:FlowStep) = new IterWrapper(outer, _children.toSeq )

}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composites-eof nil)


