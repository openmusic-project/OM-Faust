(in-package :om)


(defvar *faust-automations-to-fill* nil)

;===========================================================
;FAUST AUTOMATION
;===========================================================

(defclass! faust-automation (simple-container BPF) 
           ((c-action :initform nil :accessor c-action); :initarg :c-action)
            (faust-control :initform nil 
                           :accessor faust-control 
                           :initarg :faust-control 
                           :documentation "a faust-fx/synth, or a list of a faust-fx/synth and a name of a parameter (e.g : (<faust-synth> \"freq\"))")
            (paraminfos :initform nil :accessor paraminfos)
            (paramnum :initform nil :accessor paramnum)
            (faustfun :initform nil :accessor faustfun)
            (faustname :initform nil :accessor faustname))
           (:icon '(233))
           (:documentation "Faust Automation.

Faust-Automation is defined with :
<x-points> As for the BPF object, a list of x coordinates.
<y-points> As for the BPF object, a list of y coordinates.
<decimals> The precision of the coordinates.
<faust-control> It can be :
- A Faust-FX (then a pop-up will ask you for which parameter you want to automate),
- A Faust-Synth,
- A list of a Faust-FX/Synth and a parameter name.

Faust-Automation builds a default curve if no coordinates are specified."))


(defmethod players-for-object ((self faust-automation)) '(:bpfplayer))

(defmethod make-faust-automation-from-bpf ((self bpf) faustobj faustparam)                
  (let* ((res (make-instance 'faust-automation
                             :faust-control (list faustobj (string faustparam))))
         (inf (get-infos-from-faust-control (faust-control res))))
    (setf (x-points res) (x-points self)
          (decimals res) (decimals self)
          (paraminfos res) (car inf)
          (paramnum res) (cadr inf)
          (faustfun res) (get-function-from-faust-control res)
          (y-points res) (y-points self))
    res))

(defmethod make-one-instance ((self faust-automation) &rest slots-vals)
  (let ((bpf (call-next-method))
        fullinf xl1 xl2 yl1 yl2 xlist ylist)
    (setf (faust-control bpf) (nth 3 slots-vals))
    (when (and (faust-control bpf)
               (or
                (and
                 (listp (faust-control bpf)) 
                 (or (typep (car (faust-control bpf)) 'faust-fx) (typep (car (faust-control bpf)) 'faust-synth)) 
                 (typep (cadr (faust-control bpf)) 'string))
                (and
                 (not (listp (faust-control bpf)))
                 (or (typep (faust-control bpf) 'faust-fx) (typep (faust-control bpf) 'faust-synth)))))
      (setq fullinf (get-infos-from-faust-control (faust-control bpf)))
      (setf (faustname bpf) (cond ((typep (faust-control bpf) 'faust-fx)
                                   (effect-name (faust-control bpf)))
                                  ((typep (faust-control bpf) 'faust-synth)
                                   (synth-name (faust-control bpf)))
                                  ((listp (faust-control bpf))
                                   (cond ((typep (car (faust-control bpf)) 'faust-fx)
                                          (effect-name (car (faust-control bpf))))
                                         ((typep (car (faust-control bpf)) 'faust-synth) 
                                          (synth-name (car (faust-control bpf))))))
                                  (t nil))
            (paraminfos bpf) (car fullinf))
      (if (paraminfos bpf)
          (progn
            (setf (paramnum bpf) (cadr fullinf)
                  (faustfun bpf) (get-function-from-faust-control bpf))
            ;(if (and (= (nth 2 slots-vals) 0) (<= (- (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf))) 100))
            ;    (if (<= (- (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf))) 3)
            ;        (if (= (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf)))
            ;            (setf (decimals bpf) 0)
            ;          (setf (decimals bpf) 3))
            ;      (setf (decimals bpf) 2))
            ;  (setf (decimals bpf) (nth 2 slots-vals)))
            (setf (decimals bpf) (nth 2 slots-vals))
            (when (and (equal '(0 100) (nth 1 slots-vals)) (equal '(0 100) (nth 0 slots-vals)))
              (if (/= (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf)))
                  (setq xl1 (interpolate (list 0 500) (list 0 500) 10)
                        xl2 (interpolate (list 9500 10000) (list 9500 10000) 10)
                        yl1 (interpolate (list 0 500) (list (nth 1 (paraminfos bpf)) (nth 3 (paraminfos bpf))) 10)
                        yl2 (interpolate (list 9500 10000) (list (nth 3 (paraminfos bpf)) (nth 2 (paraminfos bpf))) 10))
                (setq xl1 (list 0 999 1000)
                      xl2 (list 2999 3000 10000)
                      yl1 (list 0 0 1)
                      yl2 (list 1 0 0)))
              (setq xlist (append xl1 xl2)
                    ylist (append yl1 yl2))
              (setf (y-points bpf) ylist
                    (x-points bpf) xlist)))
        (print "I cannot build a FAUST-AUTOMATION with these parameters"))
      bpf)))

(defmethod omng-copy ((self faust-automation))
  (let ((bpf (eval (call-next-method))))
    (setf (c-action bpf) (c-action self)
          (faust-control bpf) (faust-control self)
          (paraminfos bpf) (paraminfos self)
          (paramnum bpf) (paramnum self)
          (faustfun bpf) (faustfun self)
          (decimals bpf) (decimals self)
          (x-points bpf) (x-points self)
          (y-points bpf) (y-points self))
    bpf))


(defmethod omng-save ((self faust-automation) &optional (values? nil))
  (let ((action (c-action self))
        (parnum (paramnum self))
        (xp (x-points self))
        (yp (y-points self))
        (pinfos (paraminfos self))
        (fname (faustname self))
        (dec (decimals self)))
    `(let ((rep (make-instance ',(type-of self))))
       (setf (c-action rep) ,(omng-save action)
             (paramnum rep) ',parnum
             (decimals rep) ',dec
             (x-points rep) ',xp
             (y-points rep) ',yp
             (paraminfos rep) ',pinfos
             (faustname rep) ',fname)
       (push rep *faust-automations-to-fill*)
       rep)))

(defun fill-automations-infos ()
  (if *faust-automations-to-fill*
      (om-run-process "faust automations compiler"
                      #'(lambda ()
                          (while (not *faust-consoles-compiled?*) (sleep 0.5)) ;;;;pas propre
                          (mapcar 
                           #'(lambda (autom) 
                               (let ((found 0)
                                     (i 0)
                                     indx)
                                 (if *faust-console-register*
                                     (progn
                                       (while (= found 0)
                                         (if (string= (car (nth i *faust-console-register*)) (faustname autom))
                                             (progn (setf found 1) (setf indx i)))
                                         (incf i)
                                         (if (>= i (length *faust-console-register*))
                                             (setf found 1)))
                                       (when indx
                                         (setf (faust-control autom) (list (cadr (nth indx *faust-console-register*)) (car (paraminfos autom))))
                                         (setf (faustfun autom) (get-function-from-faust-control autom)))))))
                           *faust-automations-to-fill*)
                          (setf *faust-automations-to-fill* nil)))))

(defmethod draw-control-info ((self t) (object faust-automation))
  (when (and (faust-control object) (paraminfos object))
  (om-with-focused-view self
    (om-with-fg-color self (om-make-color 1 0 0)
      (om-draw-string 80 (om-point-y (point2pixel self (om-make-point 0 (car (last (paraminfos object)))) (get-system-etat self)))
                      (format nil (if (paraminfos object) (car (paraminfos object)))))))))

(defmethod copy-container ((self faust-automation) &optional (pere ()))
   "Builds a copy of a bpf control"
   (let ((bpf (eval (call-next-method))))
    (setf (c-action bpf) (c-action self)
          (faust-control bpf) (faust-control self)
          (paraminfos bpf) (paraminfos self)
          (paramnum bpf) (paramnum self)
          (faustfun bpf) (faustfun self)
          (decimals bpf) (decimals self)
          (x-points bpf) (x-points self)
          (y-points bpf) (y-points self))
    bpf))

(defmethod print-object ((self faust-automation) stream)
  (call-next-method))

(defmethod play-obj? ((self faust-automation)) t)
(defmethod allowed-in-maq-p ((self faust-automation)) t)
(defmethod get-obj-dur ((self faust-automation)) (last-elem (x-points self)))

(defmethod prepare-to-play ((self (eql :bpfplayer)) (player omplayer) (object faust-automation) at interval params)
  ;(player-unschedule-all self)
  (let ((faustfun (faustfun object)))
    (when (or (c-action object) (faust-control object))
      (if interval
          (mapcar #'(lambda (point)
                      (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                          (progn
                            (if (c-action object) 
                                (schedule-task player 
                                               #'(lambda () (funcall (c-action object) (cadr point))) 
                                               (+ at (car point))))
                            (if (faust-control object) 
                                (schedule-task player
                                               #'(lambda () (funcall faustfun (cadr point))) 
                                               (+ at (car point)))))))
                  (point-pairs object))
        (mapcar #'(lambda (point)
                    (if (c-action object)
                        (schedule-task player 
                                       #'(lambda () (funcall (c-action object) (cadr point))) 
                                       (+ at (car point))))
                    (if (faust-control object) 
                        (schedule-task player
                                       #'(lambda () (funcall faustfun (cadr point))) 
                                       (+ at (car point)))))
                (point-pairs object))))))

(defmethod default-edition-params ((self faust-automation)) 
  (pairlis '(player) '(:bpfplayer)))

(defmethod get-editor-class ((self faust-automation)) 'bpfcontroleditor)

(defmethod draw-automation-info ((self t) (object faust-automation))
  (let ((name (nth 0 (paraminfos object)))
        (min (nth 1 (paraminfos object)))
        (max (nth 2 (paraminfos object)))) (if (= min max) (setf max (1+ min)))
    (om-with-focused-view self
      (om-with-fg-color self (om-make-color 1 0 0)
        (om-draw-string 80 (+ (om-point-y (point2pixel self (om-make-point 0 (* min (expt 10 (decimals object)))) (get-system-etat self))) 9)
                        (format nil "~A MIN = ~A" name (round min)))
        (om-draw-line 0 (om-point-y (point2pixel self (om-make-point 0 (* min (expt 10 (decimals object)))) (get-system-etat self))) 1000000 (om-point-y (point2pixel self (om-make-point 0 (* min (expt 10 (decimals object)))) (get-system-etat self))))
        (om-draw-string 80 (om-point-y (point2pixel self (om-make-point 0 (* max (expt 10 (decimals object)))) (get-system-etat self)))
                        (format nil "~A MAX = ~A" name (round max)))
        (om-draw-line 0 (om-point-y (point2pixel self (om-make-point 0 (* max (expt 10 (decimals object)))) (get-system-etat self))) 1000000 (om-point-y (point2pixel self (om-make-point 0 (* max (expt 10 (decimals object)))) (get-system-etat self))))
        ))))

(defun get-function-from-faust-control (bpf) 
  (let* ((faust-control (faust-control bpf))
         (console (if (listp faust-control) (car faust-control) faust-control))
         (ptr (if (typep console 'faust-fx) (effect-ptr console) (synth-ptr console)))
         (infos (paraminfos bpf))
         (found (paramnum bpf))
         minval maxval range paramtype)
    (when infos
      (if (= (nth 1 infos) (nth 2 infos))
          (setq minval 0
                maxval 1)
        (setq minval (nth 1 infos)
              maxval (nth 2 infos)))
      (setq range (- maxval minval)
            paramtype (param-type (nth found (params-ctrl console))))
      #'(lambda (val)
          (setq val (max minval (min val maxval)))
          (las-faust-set-control-value ptr found (float val))
          (cond ((string= paramtype "checkbox")
                 (if (display (nth found (params-ctrl console)))
                     (om-set-check-box (paramgraph (display (nth found (params-ctrl console)))) (if (>= val 1) t))))
                ((string= paramtype "numentry")
                 (if (display (nth found (params-ctrl console)))
                     (progn
                       (om-set-dialog-item-text (paramval (display (nth found (params-ctrl console)))) (number-to-string (float val)))
                       (set-value (paramgraph (display (nth found (params-ctrl console)))) (* 100 (/ (- val minval) range))))))
                ((string= paramtype "button")
                 nil)
                (t 
                 (if (display (nth found (params-ctrl console)))
                     (progn
                       (om-set-dialog-item-text (paramval (display (nth found (params-ctrl console)))) (number-to-string (float val)))
                       (om-set-slider-value (paramgraph (display (nth found (params-ctrl console)))) (* 100 (/ (- val minval) range)))))))))))

(defun get-infos-from-faust-control (faust-control)
  (let* ((name (if (listp faust-control) (cadr faust-control)))
         (console (if (listp faust-control) (car faust-control) faust-control))
         (ptr (if (typep console 'faust-fx) (effect-ptr console) (synth-ptr console)))
         maxnum found)
    (if (and ptr (not (las-faust-null-ptr-p ptr)))
        (progn
          (setq maxnum (las-faust-get-control-count ptr))
          (if name
              (dotimes (i maxnum)
                (if (string-equal name (car (las-faust-get-control-params ptr i)))
                    (setf found i))))
          (if found
              (list (las-faust-get-control-params ptr found) found)
            (let ((param-n (make-param-select-window 
                            (loop for i from 0 to (- maxnum 1) collect
                                  (car (las-faust-get-control-params ptr i))))))
              (if param-n 
                  (list (las-faust-get-control-params ptr param-n) param-n))))))))

