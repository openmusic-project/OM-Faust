;;;================================================================================================================================================================
;;;                                                                            faust
;;;================================================================================================================================================================

(in-package :om)

(defvar paramnum 0) ;For graphic purpose
(defvar *faust-effects-to-compile* nil)
(defvar *faust-synths-to-compile* nil)
(defvar *faust-console-register* nil)
(defvar *faust-consoles-compiled?* nil)

;======================================================
;===      SINGLE FAUST PARAMETER CONTROLLER         ===
;=== a single parameter of the general faust effect ===
;======================================================

(defclass faust-effect-parameter-controller () 
  ((param-type :initform 'vslider :initarg :param-type :accessor param-type :type t)
   (label :initform nil :initarg :label :accessor label :type t)
   (index :initform nil :initarg :index :accessor index)
   (defval :initform nil :initarg :defval :accessor defval :type float)
   (minval :initform nil :initarg :minval :accessor minval :type float)
   (maxval :initform nil :initarg :maxval :accessor maxval :type float)
   (stepval :initform nil :initarg :stepval :accessor stepval :type float)
   (effect-ptr :initform nil :initarg :effect-ptr :accessor effect-ptr)
   (tracknum :initform 0 :initarg :tracknum :accessor tracknum)
   (display :initform nil :initarg :display :accessor display)))


;=======================================
;===     PARAMETERS CONTROLLER       ===
;=== a set of parameters controllers ===
;=======================================

(defclass! faust-fx (simple-score-element)
   ((effect-txt :initform nil :initarg :effect-txt :accessor effect-txt :documentation "a textfile written in Faust language")
    (effect-ptr :initform nil :accessor effect-ptr)
    (effect-name :initform nil :initarg :effect-name :accessor effect-name :documentation "a name")
    (tracknum :initform 0 :initarg :tracknum :accessor tracknum :documentation "a track on which the effect will be plugged (0 = no specific track)")
    (effect-dsp :initform nil :accessor effect-dsp)
    (effect-svg :initform nil :accessor effect-svg)
    (nbparams :initform 0 :accessor nbparams :type t)
    (params-ctrl :initform nil :accessor params-ctrl :type t)
    (ui-tree :initform nil :accessor ui-tree))
   (:icon 1)
   (:documentation "Faust Effect.

Faust-FX is defined with :
<synth-txt> A Textfile containing your Faust code.
<synth-name> A name to register your effect (optional).
<tracknum> A track to plug your effect on (optional).

Faust-FX compiles Faust effect code, registers it in the OpenMusic LibAudioStream architecture and provides a graphic user interface to control it in real-time.
It also provides a block-diagram SVG file describing the signal processor, which can be opened in any web browser."))


;=======================================
;;; MEMORY MANAGEMENT

;Add the faust cleanup to garbage functions
(hcl::add-special-free-action 'faust-cleanup)

(defmethod faust-cleanup ((self faust-fx))
  (las-faust-effect-cleanup (effect-ptr self)))

(defmethod faust-cleanup ((self t)) nil)

(defmethod initialize-instance ((self faust-fx) &rest l)
  (let ((rep (call-next-method)))
    (hcl::flag-special-free-action rep)
    rep))

;=======================================


(defmethod initialize-instance :after ((self faust-fx) &rest l)
  (declare (ignore l))
  (when (effect-txt self)
    (build-faust-fx self))
  self)

(defmethod build-faust-fx ((self faust-fx))
  ;;Check if user plugged a Faust code to the box. If yes, build, if no, exit.
  (if (and (effect-txt self) (typep (effect-txt self) 'textfile))
      (let ((parlist (list-of-lines (buffer-text (effect-txt self))))
            name effect-string effect-result)
        ;;Set name, or add a default name
        (setq name (set-effect-name self))
        ;;Check if the name  is already used. If yes, exit. If no, build effect.
        (let ((namesearch (las-faust-search-effect-name-in-register name)))
          (if (car namesearch)
              (progn
                (om-message-dialog (format nil "An effect called \"~A\" already exists. It has been replaced by the new one.~%"  name))
                (if (car (gethash (cadr namesearch) *faust-effects-register*))
                    (let* ((pointer (car (gethash (cadr namesearch) *faust-effects-register*)))
                           (track (- (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*)) 1)))
                      (las-faust-effect-cleanup pointer)
                      (if (>= track 0) (las-faust-pack-track-effects track))))
                (if *general-mixer-window*
                    (update-general-mixer-effects-lists (car (om-subviews *general-mixer-window*)))))
            (print "WARNING : If this box already contains a Faust FX, it won't be deleted, but you won't be able to access to its console anymore.")))
        ;;Build string from textfile
        (loop for line in parlist do
              (setq effect-string (concatenate 'string effect-string (format nil "~%") line)))
        ;;Save as a dsp file
        (save-data (list (list effect-string)) (format nil "OM-Faust_effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))
        ;;Get result from the compilation with the faust-api.
        (setq effect-result (las-faust-make-effect 
                             (concatenate 'string (directory-namestring *om-outfiles-folder*) 
                                          (format nil "OM-Faust_effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))
                             *om-outfiles-folder*))
        (setf (effect-ptr self) (nth 1 effect-result))
        ;;Save code as DSP and set some slots for SVG display
        (set-effect-dsp-and-svg self)
        ;;Check if faust-api returned a compilation error. If yes, exit, if no, build
        (if (/= (car effect-result) 1)
            (om-message-dialog (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (nth 2 effect-result)))
          ;;Get tree from Json, init params, register effect, plug if a track is specified.
          (let ((param-list (finalize-effect-building self name)))
            (when (and param-list (> (nbparams self) 0))
              (setf (params-ctrl self)
                    (loop for param from 0 to (- (nbparams self) 1) collect (make-instance 'faust-effect-parameter-controller
                                                                                           :param-type (param-type (nth param param-list))
                                                                                           :label (label (nth param param-list))
                                                                                           :index param
                                                                                           :defval (string-to-number (init-val (nth param param-list)))
                                                                                           :minval (string-to-number (min-val (nth param param-list)))
                                                                                           :maxval (string-to-number (max-val (nth param param-list)))
                                                                                           :stepval (string-to-number (step-val (nth param param-list)))
                                                                                           :effect-ptr (effect-ptr self)
                                                                                           :tracknum (tracknum self)
                                                                                           )))))) self)
    (print "I cannot build a faust-fx with these parameters")))


(defmethod set-effect-name ((self faust-fx))
  (or (effect-name self)
      (let ((name (format nil "Faust-FX-~A" (+ 1 (las-get-number-faust-effects-register)))))
        (print (format nil "WARNING : You didn't give a name to the effect. It's now called ~A." name))
        (setf (effect-name self) name)
        name)))

(defmethod set-effect-dsp-and-svg ((self faust-fx))
  (setf (effect-dsp self) (format nil "OM-Faust_effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))) 
        (effect-svg self) (format nil "./OM-Faust_effect~A-svg/process.svg" (+ 1 (las-get-number-faust-effects-register)))))

(defun faust-show-svg (pathname dsp svg)
  (declare (ignore dsp))
  (om-cmd-line (format nil "open ~A" svg) nil t pathname))

(defun las-clean-faust-files ()
  (om-cmd-line "rm -Rf OM-Faust_*" nil t *om-outfiles-folder*))

(defmethod finalize-effect-building ((self faust-fx) name)
  (let (param-list)
    (print "Faust-FX successfully built.")
    (setf (ui-tree self) (las-faust-parse (las-faust-get-json (effect-ptr self)))
          param-list (las-faust-translate-tree (ui-tree self)))
    (las-faust-add-effect-to-register (effect-ptr self) (tracknum self) name)
    (if (and (tracknum self) (> (tracknum self) 0))
        (las-faust-add-effect-to-track (effect-ptr self) name (- (tracknum self) 1)))
    (if *general-mixer-window*
        (update-general-mixer-effects-lists (car (om-subviews *general-mixer-window*))))
    (setf (nbparams self) (length param-list))
    param-list))


(defmethod allowed-in-maq-p ((self faust-fx))  nil)

(defmethod draw-mini-view ((self t) (value faust-fx)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value faust-fx)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self faust-fx) x x1 y y1 edparams view)
  (if (effect-txt self)
      (draw-obj-in-rect (effect-txt self) x x1 y (- y1 3) edparams view)
    (om-draw-string 2 (- y1 40) "!NO CODE!"))
  (om-with-focused-view view
    (om-with-fg-color view (om-make-color 1 0.5 0) 
      (om-draw-string 2 (- y1 5) (or (effect-name self) "!NO NAME!")))))

(defmethod omNG-copy ((self faust-fx))
   "Cons a Lisp expression that return a copy of self when it is valuated."
   `(let ((rep (make-instance ',(type-of self))))
      rep))

(defmethod copy-container  ((self faust-fx) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self))))
    rep))


(defmethod omNG-save ((self faust-fx) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((text (effect-txt self))
        (name (effect-name self))
        (track (tracknum self)))
    (if (and (effect-ptr self) (not (las-faust-null-ptr-p (effect-ptr self))))
          `(when (find-class ',(type-of self) nil)
             (let ((rep (make-instance ',(type-of self))))
               (setf (effect-txt rep) ,(omng-save text)
                     (effect-name rep) ',name
                     (tracknum rep) ',track)
               (push rep *faust-effects-to-compile*)
               rep))
        `(when (find-class ',(type-of self) nil)
           (make-instance ',(type-of self))))))



(defun compile-faust-objects ()
  (om-run-process "faust objects compiler"
                  #'(lambda ()
                      (setq *faust-consoles-compiled?* nil)
                      (mapcar 
                       #'(lambda (fx) 
                           (cond ((typep fx 'faust-fx)
                                  (progn
                                    (build-faust-fx fx)
                                    (push (list (effect-name fx) fx) *faust-console-register*)))
                                 ((typep fx 'faust-synth)
                                  (progn
                                    (build-faust-synth fx)
                                    (push (list (synth-name fx) fx) *faust-console-register*)))
                                 (t nil)))
                       (append *faust-effects-to-compile* *faust-synths-to-compile*))
                      (setq *faust-effects-to-compile* nil
                            *faust-synths-to-compile* nil
                            *faust-consoles-compiled?* t))))


(defmethod load-abstraction-attributes :after ((self ompatch) currentpersistent)
  (compile-faust-objects)
  (fill-automations-infos)) 

   
(defmethod get-obj-dur ((self faust-fx)) 0)

(defmethod object-remove-extra ((self faust-fx) box)
  (let* ((ptr (effect-ptr self)))
    (if ptr
        (las-faust-effect-cleanup ptr))
    (if *general-mixer-window*
        (update-general-mixer-effects-lists (car (om-subviews *general-mixer-window*))))))


;;;==================
;;; EDITOR
;;;==================

(defmethod Class-has-editor-p  ((self faust-fx)) t)

(defmethod get-editor-class ((self faust-fx)) 'faustcontrollerEditor)

(defclass faustcontrollerEditor (EditorView) 
  ((params-panels :initform nil :accessor params-panels :type list)
   (tree :initform nil :accessor tree :type nil)
   (bottom-bar :initform nil :accessor bottom-bar :type t)))


(defmethod make-editor-window ((class (eql 'faustcontrollerEditor)) object name ref &key 
                               winsize winpos (close-p t) (winshow t) 
                               (resize nil) (maximize nil))
  (declare (ignore winsize close-p winshow resize maximize))
  (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                               :close-p t :winshow t :bg-color *om-light-gray-color*
                               )))
    win))


(defmethod get-win-ed-size ((self faust-fx)) 
  (if (ui-tree self)
     (om-make-point (min 500 (if (> (+ 75 (cadr (las-faust-get-group-size (ui-tree self)))) 500)
                                 (+ (car (las-faust-get-group-size (ui-tree self))) 14)
                               (car (las-faust-get-group-size (ui-tree self))))) (min 500 (+ 75 (cadr (las-faust-get-group-size (ui-tree self))))))
    (om-make-point 75 75)))


(defmethod get-panel-class ((self faustcontrollerEditor)) 'faustcontrollerPanel)

(defmethod update-subviews ((self faustcontrollerEditor))
   (om-set-view-size (panel self) (om-make-point (min 500 (w self)) (min 500 (h self)))))


;=== MAIN PANEL ===
(defclass faustcontrollerPanel (om-scroller) ())

(defmethod get-object ((Self faustcontrollerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self faustcontrollerPanel))
  (report-modifications (om-view-container self)))


;======== Parameters controllers panels =====

(defclass faustparamPanel () 
  ((paramctr :initform nil :initarg :paramctr :accessor paramctr)
   (paramText :initform nil :accessor paramText :type t)
   (paramVal :initform nil :accessor paramVal :type t)
   (paramGraph :initform nil :accessor paramGraph :type t)
   (paramReset :initform nil :accessor paramReset :type t)))


(defclass faustparamPanelview (faustparamPanel om-view) ())

(defmethod update-subviews ((Self faustparamPanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self faustparamPanel))
   (call-next-method))



(defmethod get-object ((Self faustparamPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self faustparamPanel))
  (report-modifications (om-view-container self)))


(defmethod get-parampanel-class ((self faustcontrollerPanel)) 'faustparamPanelview)



;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod metaobj-scrollbars-params ((self faustcontrollerEditor))  '(:h nil))

(defmethod initialize-instance :after ((self faustcontrollerEditor) &rest l)
  (declare (ignore l))
  (let ((x (if (ui-tree (object self))
               (max 75 (car (las-faust-get-group-size (ui-tree (object self)))))
             75))
        (y (if (ui-tree (object self))
               (+ 75 (cadr (las-faust-get-group-size (ui-tree (object self)))))
             75)))

    (if (and (effect-ptr (object self)) (ui-tree (object self)))
        (let (group-type groups params)
          (setf (tree self) (ui-tree (object self)))
          (setq group-type (las-faust-get-group-type (tree self))
                groups (las-faust-get-groups-only (tree self))
                params (las-faust-get-params-only (tree self)))
          (setf (panel self) (om-make-view (get-panel-class self) 
                                           :owner self
                                           :position (om-make-point 0 0) 
                                           :scrollbars (cond ((and (>= x 500) (>= y 500)) t)
                                                             ((and (>= x 500) (< y 500)) :h)
                                                             ((and (< x 500) (>= y 500)) :v)
                                                             (t nil))
                                           :retain-scrollbars nil
                                           :field-size  (om-make-point x y)
                                           :size (om-make-point (w self) (h self))
                                           :bg-color *om-light-gray-color*))
     
          (make-faust-group-view self (tree self))
          (setq paramnum 0)
          (setf (bottom-bar self) (om-make-view (get-panel-class self)
                                                :owner (panel self)
                                                :bg-color *om-dark-gray-color*
                                                :position (om-make-point 0 (- y 50))
                                                :size (om-make-point (+ 20 x) 50)))
          (om-add-subviews (bottom-bar self) 
                           (om-make-dialog-item 'om-button (om-make-point (- (round x 2) 30) 5) (om-make-point 60 24)
                                                "SVG"
                                                :di-action (om-dialog-item-act item
                                                             (faust-show-svg *om-outfiles-folder* (effect-dsp (object self)) (effect-svg (object self)))))))
      (progn
        (setf (panel self) (om-make-view (get-panel-class self) 
                                         :owner self
                                         :position (om-make-point 0 0) 
                                         :scrollbars nil
                                         :retain-scrollbars t
                                         :field-size  (om-make-point x (- y 50))
                                         :size (om-make-point (w self) (h self))
                                         :bg-color *om-dark-gray-color*))
        (om-add-subviews (panel self) (om-make-dialog-item 'om-static-text
                                                           (om-make-point 17 0)
                                                           (om-make-point 75 75)
                                                           "X"
                                                           :fg-color *om-white-color*
                                                           :font (om-make-font oa::*om-def-bold-font-face* 48 :style '(:bold))))
        ))))

(defmethod make-faust-param-view ((self faustcontrollerEditor) paractrl x y size)
  (setf (display paractrl) (om-make-view (get-parampanel-class (panel self))
                                         :paramctr paractrl
                                         :owner (panel self)
                                         :bg-color *om-light-gray-color*
                                         :position (om-make-point x y)
                                         :size (om-make-point (car size) (cadr size)))))

(defmethod make-faust-group-view ((self faustcontrollerEditor) group &optional (x 0) (y 0))
  (let* ((grouptype (las-faust-get-group-type group))
         (children (las-faust-get-group-items group))
         (numchildren (length children))
         (size (las-faust-get-group-size group)))
    ;;///////////////////ON CREE L'ESPACE DU GROUPE
    (om-make-view 'om-view
                  :owner (panel self)
                  :bg-color *om-light-gray-color*
                  :position (om-make-point x y)
                  :size (om-make-point (car size) (cadr size)))
    ;;///////////////////ON CREE LES VIEW DES ENFANTS
    (loop for i from 0 to (- numchildren 1) do
          (if (typep (nth i children) 'faust-group)
              (progn
                (make-faust-group-view self (nth i children) x y)
                (if (string= grouptype "hgroup")
                    (incf x (+ 5 (car (las-faust-get-group-size (nth i children)))))
                  (incf y (+ 5 (cadr (las-faust-get-group-size (nth i children)))))))
            (let* ((type (param-type (nth i children)))
                   (disable 0)
                   (size (cond ((string= type "hslider") hsliderSize)
                               ((string= type "vslider") vsliderSize)
                               ((string= type "checkbox") checkboxSize)
                               ((string= type "numentry") numentrySize)
                               ((string= type "button") buttonSize)
                               (t (progn (setf disable 1) buttonsize)))))
              (if (= disable 0)
                  (make-faust-param-view self (nth paramnum (params-ctrl (object self))) x y size))
              (incf paramnum)
              (if (string= grouptype "hgroup") 
                  (incf x (car size))
                (incf y (cadr size))))))))


(defmethod update-editor-after-eval ((self faustcontrollerEditor) val)
  (setf (object self) val)
  (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
  (om-set-view-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
  (om-set-field-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
  (loop for parampan in (params-panels self) do 
        (om-remove-subviews (panel self) parampan))
  (if (ui-tree (object self))
      (make-faust-group-view self (ui-tree (object self))))
  (setq paramnum 0))


(defmethod initialize-instance :after ((self faustparamPanel) &rest l)
   (declare (ignore l))
   (do-initialize-param self))


(defmethod do-initialize-param ((self faustparamPanel))  
  (let* ((ptr (effect-ptr (paramctr self)))
         (number (index (paramctr self)))
         (color (om-make-color 0.9 0.9 0.9))
         (orange (om-make-color 1 0.5 0))
         (type (param-type (paramctr self)))
         (name (label (paramctr self)))
         (min (minval (paramctr self)))
         (max (maxval (paramctr self)))
         (range 0)
         (val 0)
         (editor (om-view-container (om-view-container self)))
         (curval (las-faust-get-control-value ptr number)))
    (if (= min max) (let () (setf min 0) (setf max 1)) nil)
    (setf range (/ (- max min) 1.0))
    (setf val (* 100.0 (/ (- curval min) range)))
    (om-set-bg-color self color) 

    (cond ((string= type "hslider")
           (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                       (om-make-point 5 0) 
                                                       (om-make-point (- (car hsliderSize) 5) 40)
                                                       (format nil "~D" name)
                                                       :font *om-default-font1*
                                                       :bg-color color)
                 (paramVal self) (om-make-dialog-item 'om-static-text 
                                                      (om-make-point (- (round (car hsliderSize) 2) 30) (- (cadr hsliderSize) 31 20)) 
                                                      (om-make-point 60 20)
                                                      (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                      :font *om-default-font1*
                                                      :fg-color orange
                                                      :bg-color color)
                 (paramGraph self) (om-make-dialog-item 'om-slider  
                                                        (om-make-point 10 (- (cadr hsliderSize) 36)) 
                                                        (om-make-point 94 31) ""
                                                        :di-action 
                                                        (om-dialog-item-act item
                                                          (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                                            (las-faust-set-control-value ptr number valeur)
                                                            (om-set-dialog-item-text (paramVal self) 
                                                                                     (if (<= range 100) 
                                                                                         (format nil "~$" valeur) 
                                                                                       (format nil "~D" (round valeur))))))
                                                        :increment 1
                                                        :range '(0 100)
                                                        :value val
                                                        :direction :horizontal
                                                        :tick-side :none)))
          ((string= type "vslider")
           (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                       (om-make-point 5 0) 
                                                       (om-make-point (- (car vsliderSize) 15) 50)
                                                       (format nil "~D" name)
                                                       :font *om-default-font1*
                                                       :bg-color color)
                 (paramVal self) (om-make-dialog-item 'om-static-text 
                                                      (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                      (om-make-point 60 20)
                                                      (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                      :font *om-default-font1*
                                                      :fg-color orange
                                                      :bg-color color)
                 (paramGraph self)
                 (om-make-dialog-item 'om-slider  
                                      (om-make-point 10 (- (cadr vsliderSize) 104)) 
                                      (om-make-point 31 94) ""
                                      :di-action 
                                      (om-dialog-item-act item
                                        (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                          (las-faust-set-control-value ptr number valeur)
                                          (om-set-dialog-item-text (paramVal self) 
                                                                   (if (<= range 100) 
                                                                       (format nil "~$" valeur) 
                                                                     (format nil "~D" (round valeur))))))
                                      :increment 1
                                      :range '(0 100)
                                      :value val
                                      :direction :vertical
                                      :tick-side :none)))
          ((string= type "checkbox")
           (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                       (om-make-point 5 0) 
                                                       (om-make-point (- (car checkboxSize) 5) 50)
                                                       (format nil "~D" name)
                                                       :font *om-default-font1*
                                                       :bg-color color)
                 (paramVal self) (om-make-dialog-item 'om-static-text 
                                                      (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                      (om-make-point 60 20)
                                                      ""
                                                      :font *om-default-font1*
                                                      :fg-color *om-blue-color*
                                                      :bg-color color)
                 (paramGraph self)
                 (om-make-dialog-item 'om-check-box  
                                      (om-make-point 20 5) 
                                      (om-make-point 31 94) ""
                                      :di-action 
                                      (om-dialog-item-act item 
                                        (if (= (las-faust-get-control-value ptr number) 1.0) 
                                            (las-faust-set-control-value ptr number 0.0) 
                                          (las-faust-set-control-value ptr number 1.0))
                                        (om-set-dialog-item-text (paramVal self) (format nil "~D" (round (las-faust-get-control-value ptr number)))))
                                      :font *om-default-font1*
                                      :checked-p (if (= (las-faust-get-control-value ptr number) 1.0) t nil))))
          ((string= type "button")
           (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                       (om-make-point 5 0) 
                                                       (om-make-point (- (car checkboxSize) 5) 50)
                                                       ""
                                                       :font *om-default-font1*
                                                       :bg-color color)
                 (paramVal self) (om-make-dialog-item 'om-static-text 
                                                      (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                      (om-make-point 60 20)
                                                      ""
                                                      :font *om-default-font1*
                                                      :fg-color *om-blue-color*
                                                      :bg-color color)
                 (paramGraph self)
                 (om-make-dialog-item 'om-button  
                                      (om-make-point 20 5) 
                                      (om-make-point 80 30) 
                                      (format nil "~D" name)
                                      :di-action 
                                      (om-dialog-item-act item
                                        (if (= (las-faust-get-control-value ptr number) max) 
                                            (progn
                                              (las-faust-set-control-value ptr number 0.0)
                                              (las-faust-set-control-value ptr number (+ 0.000001 (las-faust-get-control-value ptr number))))
                                          (las-faust-set-control-value ptr number (+ 0.000001 (las-faust-get-control-value ptr number)))
                                          )
                                        (om-set-dialog-item-text (paramVal self) (format nil "~D" (round (las-faust-get-control-value ptr number)))))
                                      :font *om-default-font1*)))
          (t 
           (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                       (om-make-point 5 0) 
                                                       (om-make-point (- (car vsliderSize) 15) 50)
                                                       (format nil "~D" name)
                                                       :font *om-default-font1*
                                                       :bg-color color)
                 (paramVal self) (om-make-dialog-item 'om-static-text 
                                                      (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                      (om-make-point 60 20)
                                                      (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                      :font *om-default-font1*
                                                      :fg-color orange
                                                      :bg-color color)
                 (paramGraph self)
                 (om-make-view 'graphic-numbox :position (om-make-point 10 45) 
                               :size (om-make-point 31 94)
                               :pict (om-load-and-store-picture "fader" 'di)
                               :nbpict 77
                               :pict-size (om-make-point 31 94)
                               :di-action (om-dialog-item-act item
                                            (let ((valeur (+ (* (/ (value item) 100.0) range) min)))
                                              (las-faust-set-control-value ptr number valeur)
                                              (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                               :font *om-default-font2*
                               :value val
                               :min-val 0
                               :max-val 100))))

    (om-add-subviews self
                     (paramText self)
                     (paramVal self)
                     (paramGraph self)
                    ;(paramReset self)
                     )))


