(cl:defpackage #:site.djei.bar.impl
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum)
                    (:cass :org.shirakumo.classowary))
  (:use #:site.djei.bar #:cl))

(in-package #:site.djei.bar.impl)

(defclass bar-pane (clim:application-pane) ())

(defmethod clim:note-sheet-grafted :after ((sheet bar-pane))
  (clime:schedule-event sheet (make-instance 'clim:timer-event :sheet sheet) 5))

(defmethod clim:handle-event ((sheet bar-pane) (e clim:timer-event))
  (declare (ignore e))
  (when (clim:sheet-viewable-p sheet)
    (clim:redisplay-frame-pane clim:*application-frame* sheet)
    (clime:schedule-event sheet (make-instance 'clim:timer-event :sheet sheet) 5)))

(clim:define-application-frame bar ()
  ()
  (:pane (clim:make-clim-stream-pane :type 'bar-pane
                                     :display-function 'display
                                     :scroll-bar nil
                                     :background clim:+black+
                                     :foreground clim:+white+))
  (:menu-bar nil)
  (:pointer-documentation nil)
  (:geometry :height 30 :width 1920))

(defmacro hrack ((stream) &body body)
  `(clim:formatting-table (,stream)
     (clim:formatting-row (,stream)
       ,@body)))
(defmacro hrack* ((stream) &body body)
  `(hrack (,stream)
     ,@(loop :for c :in body :collect
             `(clim:formatting-cell (,stream :align-y :center)
                ,c))))

(defun invoke-with-bar-layout (stream left-cont center-cont right-cont)
  (let ((left (clim:with-output-recording-options (stream :draw nil
                                                          :record t)
                (clim:with-room-for-graphics (stream)
                  (funcall left-cont stream))))
        (center (clim:with-output-recording-options (stream :draw nil
                                                            :record t)
                  (clim:with-room-for-graphics (stream)
                    (funcall center-cont stream))))
        (right (clim:with-output-recording-options (stream :draw nil
                                                           :record t)
                 (clim:with-room-for-graphics (stream)
                   (funcall right-cont stream))))
        (solver (cass:make-solver)))
    (clim:with-bounding-rectangle* (:height lh :width lw) left
      (clim:with-bounding-rectangle* (:height ch :width cw) center
        (clim:with-bounding-rectangle* (:height rh :width rw) right
          (cass:with-variables (ly cx cy rx ry) solver
            (cass:constrain-with solver `(= ,(/ 1920 2) (+ ,(/ cw 2) cx))
                                 :strength cass:+strong+)
            (cass:constrain-with solver `(= (+ ,(/ lh 2) ,ly) (+ ,(/ ch 2) ,cy)))
            (cass:constrain-with solver `(= (+ ,(/ lh 2) ,ly) (+ ,(/ rh 2) ,ry)))
            (cass:constrain-with solver `(<= ,lw ,cx))
            (cass:constrain-with solver `(<= (+ ,cx ,cw) ,rx))
            (cass:constrain-with solver `(= 1910 (+ ,rx ,rw)))
            (cass:update-variables solver)
            (setf (clim:output-record-position left) (values 0 (cass:value ly))
                  (clim:output-record-position center) (values (cass:value cx) (cass:value cy))
                  (clim:output-record-position right) (values (cass:value rx) (cass:value ry)))))))
    (when (clim:stream-drawing-p stream)
      (clim:replay left stream)
      (clim:replay center stream)
      (clim:replay right stream))))

(defmacro with-bar-layout ((stream) &body (left center right))
  `(invoke-with-bar-layout
    ,stream
    (lambda (,stream) ,left)
    (lambda (,stream) ,center)
    (lambda (,stream) ,right)))

(defun display (frame bar)
  (declare (ignore frame))
  (with-bar-layout (bar)
    (clim:surrounding-output-with-border (bar :background clim:+dark-blue+
                                              :outline-ink clim:+black+
                                              :shape :rounded
                                              :padding-x 10
                                              :padding-y 3)
      (format bar "Left"))
    (format bar "Center")
    (hrack* (bar)
      (format bar "Foo")
      (clim:surrounding-output-with-border (bar :background clim:+dark-red+
                                                :outline-ink clim:+black+
                                                :shape :rounded
                                                :padding-x 20
                                                :padding-y 3)
        (local-time:format-timestring bar (local-time:now)
                                      :format local-time:+asctime-format+)))))

(defmethod clim:note-frame-enabled :before ((fm clim-clx::clx-frame-manager) (frame bar))
  (let ((window (clim-clx::window (clim:sheet-mirror (clim:frame-top-level-sheet frame)))))
    (xlib:change-property window
                          :_net_wm_state
                          (list
                           (xlib:intern-atom (xlib:window-display window) :_net_wm_state_sticky)
                           (xlib:intern-atom (xlib:window-display window) :_net_wm_state_above))
                          :atom
                          32)
    (xlib:change-property window
                          :_net_wm_window_type
                          (list (xlib:intern-atom (xlib:window-display window) :_NET_WM_WINDOW_TYPE_DOCK))
                          :atom
                          32)
    (xlib:change-property window :_net_wm_strut (list 0 0 30 0) :cardinal 32)))

(defun run-bar ()
  (let* ((fm (clim:find-frame-manager :port (clim:find-port :server-path :clx-ttf)))
         (frame (clim:make-application-frame 'bar :frame-manager fm)))
    (flet ((run ()
             (unwind-protect
                  (clim-debugger:with-debugger () (clim:run-frame-top-level frame))
               (clim:disown-frame fm frame))))
      (values (clim-sys:make-process #'run)
              frame))))

(defun main ()
  (run-bar))
