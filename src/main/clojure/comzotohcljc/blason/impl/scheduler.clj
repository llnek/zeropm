
(ns ^{ :doc ""
       :author "kenl" }

(import '(java.util Properties Timer TimerTask))
(import '(org.apache.commons.lang3 StringUtils))


(defprocotol SchedulerAPI
  ())


  ;;protected val _holdQ= mutable.HashMap[Long,Runnable]()
  ;;protected val _runQ= mutable.HashMap[Long,Runnable]()
  ;;protected var _core:TCore= null
  ;;private var _timer:JTimer=null
(deftype Scheduler [core] SchedulerAPI

  (dispose [_]
    (when-not (nil? core) (.dispose core)))

  ;; called by a *running* task to remove itself from the running queue
  (dequeue [this w]
    (let [ pid (xrefPID w) ]
      (.remove runQ pid)))

  (run [this w]
    (do
      (preRun w)
      (.schedule core w)) )

  (delay [this w delayMillis]
    (cond
      (= delayMillis 0) (run  w)
      (< delayMillis 0) (hold w)
      :else
      (do
        (addTimer (proxy [TimerTask] []
                    (run [_] (.wakeup this w))) delayMillis)
        (debug "Delaying eval on process: " w ", wait: " delayMillis "millisecs"))))

  def hold( w:Runnable) {
    xrefPID(w) match {
      case pid:Long if pid >= 0L =>  hold(pid,w)
      case _ =>
    }
  }

  def hold(pid:Long, w:Runnable) {
    _runQ.remove(pid)
    _holdQ.put(pid, w)
    tlog.debug("Moved to pending wait, process: {}", w)
  }

  def wakeup(w:Runnable) {
    xrefPID(w) match {
      case pid:Long if (pid >= 0L) =>  wakeAndRun( pid,w)
      case _ =>
    }
  }

  def wakeAndRun( pid:Long, w:Runnable) {
    _holdQ.remove(pid)
    _runQ.put(pid, w)
    run( w)
    tlog.debug("Waking up process: {}", w)
  }

  def reschedule(w:Runnable) {
    if (w != null) {
      tlog.debug("Restarting runnable: {}" , w)
      run( w)
    }
  }

  private def preRun(w:Runnable) {
    xrefPID(w) match {
      case n:Long if n >= 0L  =>
        _holdQ.remove( n )
        _runQ += n -> w
      case _ =>
    }
  }

  def start() { _core.start }

  def stop() {}

  def addCore(id:String, threads:Int) {
    _core= new TCore(id,threads)
  }

  def configure(c:Configuration) {
    addCore( uid(), c.getLong("threads", 4L).toInt )
    _timer= new JTimer("scheduler-timer", true)
  }

  def addTimer(t:JTTask, delay:Long) {
    _timer.schedule(t, delay)
  }

  private def xrefPID(w:Runnable) = {
    w match {
      case s:Identifiable => s.id()
      case _ => -1L
    }
  }

}

