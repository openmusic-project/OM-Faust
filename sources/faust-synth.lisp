(in-package :om)


;;;================================================================================================================================================================
;;;                                                                            faust SYNTH
;;;================================================================================================================================================================

(defclass* faust-synth-parameter-controller () 
  ((param-type :initform 'vslider :initarg :param-type :accessor param-type :type t)
   (label :initform nil :initarg :label :accessor label :type t)
   (index :initform nil :initarg :index :accessor index)
   (defval :initform nil :initarg :defval :accessor defval :type float)
   (minval :initform nil :initarg :minval :accessor minval :type float)
   (maxval :initform nil :initarg :maxval :accessor maxval :type float)
   (stepval :initform nil :initarg :stepval :accessor stepval :type float)
   (synth-ptr :initform nil :initarg :synth-ptr :accessor synth-ptr)
   (tracknum :initform 0 :initarg :tracknum :accessor tracknum)
   (display :initform nil :initarg :display :accessor display)))


;=======================================
;===     PARAMETERS CONTROLLER       ===
;=== a set of parameters controllers ===
;=======================================

(defclass! faust-synth (simple-score-element)
   ((synth-txt :initform nil :initarg :synth-txt :accessor synth-txt :documentation "a textfile written in Faust language")
    (synth-ptr :initform nil :accessor synth-ptr)
    (synth-name :initform nil :initarg :synth-name :accessor synth-name :documentation "a name")
    (tracknum :initform 0 :initarg :tracknum :accessor tracknum :documentation "a track on which the synth will be plugged (0 = no specific track)")
    (duration :initform 10 :initarg :duration :accessor duration :documentation "a duration in seconds (default = 10 sec)")
    (nullsnd :initform nil :accessor nullsnd)
    (synth-dsp :initform nil :accessor synth-dsp)
    (synth-svg :initform nil :accessor synth-svg)
    (nbparams :initform 0 :accessor nbparams :type t)
    (params-ctrl :initform nil :accessor params-ctrl :type t)
    (ui-tree :initform nil :accessor ui-tree)
    (is-copy :initform nil :accessor is-copy))
   (:icon 2)
   (:documentation "Faust Synth.

Faust-Synth is defined with :
<synth-txt> A Textfile containing your Faust code.
<synth-name> A name to register your synthesizer (optional).
<tracknum> A track to plug your synthesizer on (optional).
<duration> A length in seconds for your synthesizer (optional).

Faust-synth compiles Faust synthesizer code, registers it in the OpenMusic LibAudioStream architecture and provides a graphic user interface to control it in real-time.
It also provides a block-diagram SVG file describing the signal processor, which can be opened in any web browser."))
(defmethod print-faust-synth ((self faust-synth))
  (print (list "Text" (synth-txt self)))
  (print (list "Pointer" (synth-ptr self)))
  (print (list "Name" (synth-name self)))
  (print (list "Track" (tracknum self)))
  (print (list "Duration" (duration self)))
  (print (list "Null Sound" (nullsnd self)))
  (print (list "DSP" (synth-dsp self)))
  (print (list "SVG" (synth-svg self))))  

(defmethod faust-cleanup ((self faust-synth))
  ;(print (list "GC-FAUST_SYNTH_CLEANUP" self))
  (las-faust-synth-cleanup (synth-ptr self)))

(defmethod play-obj? ((self faust-synth)) t)

(defmethod players-for-object ((self faust-synth)) '(:libaudiostream))

;/Redefinition of transport functions for this kind of box
(defmethod player-play-object ((engine (eql :libaudiostream)) (object faust-synth) &key interval)
  (om-synth-preview-play object))

(defmethod player-stop-object ((engine (eql :libaudiostream)) (object faust-synth) &key interval)
  (om-synth-preview-stop object))

(defmethod default-edition-params ((self faust-synth)) 
  (pairlis '(player) '(:libaudiostream) (call-next-method)))

(defmethod initialize-instance ((self faust-synth) &rest l)
  (let ((rep (call-next-method)))
    ;(print (list "GC-REGISTERING_CONSOLE" rep))
    (hcl::flag-special-free-action rep)
    rep))

(defmethod initialize-instance :after ((self faust-synth) &rest l)
  (declare (ignore l))
  (when (synth-txt self)
    (build-faust-synth self))
  self)

(defmethod build-faust-synth ((self faust-synth))
  ;Check whether it's a maquette copy. If no, build, if yes, don't do anything.
  ;(if (not (is-copy self))
      ;;Check if user plugged a Faust code to the box. If yes, build, if no, exit.
      (if (and (synth-txt self) (typep (synth-txt self) 'textfile))
          (let ((parlist (list-of-lines (buffer-text (synth-txt self))))
                (nullptr (las-faust-make-null-sound (duration self)))
                name synth-string synth-result)
            ;;Set name, or add a default name
            (setf name (set-synth-name self))
            ;;Check if the name is already used. If yes, exit. If no, build synth.
            (let ((namesearch (las-faust-search-synth-name-in-register name)))
              (if (car namesearch)
                  (progn
                    (om-message-dialog (format nil "A Synth called \"~A\" already exists. It has been replaced by the new one.~%"  name))
                    (if (car (gethash (cadr namesearch) *faust-synths-register*))
                        (las-faust-synth-cleanup (car (gethash (cadr namesearch) *faust-synths-register*))))
                    (if *general-mixer-window*
                        (update-general-mixer-synths-lists (car (om-subviews *general-mixer-window*)))))
                (print "WARNING : If this box already contains a Faust Synth, it won't be deleted, but you won't be able to access to its console anymore.")))
            ;;Build string from textfile
            (loop for line in parlist do
                  (setf synth-string (concatenate 'string synth-string (format nil "~%") line)))
            ;;Save as a dsp file
            (save-data (list (list synth-string)) (format nil "OM-Faust_synth~A.dsp" (+ 1 (las-get-number-faust-synths-register))))
            ;;Get result from the compilation with the faust-api.
            (setf synth-result (las-faust-make-effect 
                                (concatenate 'string (directory-namestring *om-outfiles-folder*) (format nil "OM-Faust_synth~A.dsp" (+ 1 (las-get-number-faust-synths-register)))) 
                                *om-outfiles-folder*))
            (setf (synth-ptr self) (nth 1 synth-result))
            ;;Set a null snd object for this synth
            (setf (nullsnd self) (make-instance 'internalsound
                                                :number-of-channels 2
                                                :player-data (make-instance 'las-player-sound
                                                                            :loaded t   ;; fake
                                                                            :sndlasptr nullptr
                                                                            :sndlasptr-current nullptr
                                                                            :sndlasptr-current-save nullptr)))
            (las-sound-set-sndlasptr-to-play (print (player-data (nullsnd self))) nullptr)
            ;;Save code as DSP and set some slots for SVG display
            (set-synth-dsp-and-svg self)
            ;;Check if faust-api returned a compilation error. If yes, exit, if no, build
            (if (/= (car synth-result) 1)
                (om-message-dialog (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (nth 2 synth-result)))
              ;;Get tree from Json, init params, register synth, plug if a track is specified.
              (let (param-list)
                (setf param-list (finalize-synth-building self name))
                (if (> (nbparams self) 0)
                    (setf (params-ctrl self)
                          (loop for param from 0 to (- (nbparams self) 1) collect (make-instance 'faust-synth-parameter-controller
                                                                                                 :param-type (param-type (nth param param-list))
                                                                                                 :label (label (nth param param-list))
                                                                                                 :index param
                                                                                                 :defval (string-to-number (init-val (nth param param-list)))
                                                                                                 :minval (string-to-number (min-val (nth param param-list)))
                                                                                                 :maxval (string-to-number (max-val (nth param param-list)))
                                                                                                 :stepval (string-to-number (step-val (nth param param-list)))
                                                                                                 :synth-ptr (synth-ptr self)
                                                                                                 :tracknum (tracknum self)
                                                                                                 )))))) self)
        (print "I cannot build a faust-synth with these parameters")));)


(defmethod set-synth-name ((self faust-synth))
  (or (synth-name self)
      (let ((name (format nil "Faust-Synth-~A" (+ 1 (las-get-number-faust-synths-register)))))
        (print (format nil "WARNING : You didn't give a name to the synth. It's now called ~A." name))
        (setf (synth-name self) name)
        name)))

(defmethod set-synth-dsp-and-svg ((self faust-synth))
  (setf (synth-dsp self) (format nil "OM-Faust_synth~A.dsp" (+ 1 (las-get-number-faust-synths-register))))
  (setf (synth-svg self) (format nil "./OM-Faust_synth~A-svg/process.svg" (+ 1 (las-get-number-faust-synths-register)))))

(defmethod finalize-synth-building ((self faust-synth) name)
  (let (param-list)
    (print "Faust-Synth successfully built.")
    (setf (ui-tree self) (las-faust-parse (las-faust-get-json (synth-ptr self))))
    (setf param-list (las-faust-translate-tree (ui-tree self)))
    (if (and (tracknum self) (> (tracknum self) 0))
        (if (not (gethash 0 (gethash (- (tracknum self) 1) *faust-synths-by-track*)))
            (progn
              (las-faust-add-synth-to-track (synth-ptr self) name (- (tracknum self) 1))
              (las-faust-add-synth-to-register (synth-ptr self) (tracknum self) name))
          (progn
            (print (format nil "A synth is already plugged on channel ~A. Your synth has been created but not plugged" (tracknum self)))
            (las-faust-add-synth-to-register (synth-ptr self) 0 name)))
      (progn
        (las-faust-add-synth-to-register (synth-ptr self) 0 name)))
    (las-faust-add-synth-console-to-register self (synth-ptr self) (nullsnd self))
    (if *general-mixer-window*
        (update-general-mixer-synths-lists (car (om-subviews *general-mixer-window*))))
    (setf (nbparams self) (length param-list))
    param-list))


(defmethod allowed-in-maq-p ((self faust-synth))  t)

(defmethod Class-has-editor-p ((self faust-synth)) t)

(defmethod get-editor-class ((self faust-synth)) 'faustSynthcontrollerEditor)

(defmethod draw-mini-view  ((self t) (value faust-synth)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value faust-synth)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self faust-synth) x x1 y y1 edparams view)
  (let ((w (w view)))
    (draw-obj-in-rect (synth-txt self) x x1 y (- y1 3) edparams view)
    (om-with-focused-view view
      (om-draw-string 2 (- y1 20) (or (synth-name self) "!NO NAME!")))))

(defmethod omNG-copy ((self faust-synth))
  (call-next-method))

(defmethod copy-container  ((self faust-synth) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance 'faust-synth))
        (nullptr (las-faust-make-null-sound (duration self))))
    (setf (synth-txt rep) (synth-txt self)
          (synth-ptr rep) (synth-ptr self)
          (synth-name rep) (synth-name self)
          (tracknum rep) (or (nth 1 (gethash (las-faust-find-synth-in-register (synth-ptr self)) *faust-synths-register*)) (tracknum self))
          (duration rep) (duration self)
          (nullsnd rep) (nullsnd self)
          (synth-dsp rep) (synth-dsp self)
          (synth-svg rep) (synth-svg self)
          (nbparams rep) (nbparams self)
          (params-ctrl rep) (params-ctrl self)
          (ui-tree rep) (ui-tree self)
          (is-copy rep) t)
    (las-faust-add-synth-console-to-register rep (synth-ptr rep) (nullsnd rep))
    rep))


(defmethod omNG-save ((self faust-synth) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((text (synth-txt self))
        (name (synth-name self))
        (dur (duration self))
        (track (tracknum self))
        (copy-state (is-copy self)))
    (if (and (synth-ptr self) (not (las-faust-null-ptr-p (synth-ptr self))))
        (progn
          `(let ((rep (make-instance ',(type-of self))))
             (setf (synth-txt rep) ,(omng-save text)
                   (synth-name rep) ',name
                   (tracknum rep) ',track
                   (duration rep) ',dur
                   (is-copy rep) ',copy-state)
             (push rep *faust-synths-to-compile*)
             rep))
      (progn
        `(let ((rep (make-instance ',(type-of self))))
           rep)))))


(defun compile-faust-synths ()
  (om-run-process "faust synths compiler" 
                  #'(lambda ()
                      (mapcar 
                       #'(lambda (fx) (build-faust-synth fx))
                       *faust-synths-to-compile*)
                      (setf *faust-synths-to-compile* nil))))


(defmethod get-obj-dur ((self faust-synth)) (* 1000 (duration self)))
(defmethod real-duration ((self faust-synth) time) (values time (+ time (get-obj-dur self))))


(defmethod object-remove-extra ((self faust-synth) box)
  (let* ((ptr (synth-ptr self)))
    (if ptr
        (las-faust-synth-cleanup ptr))
    (if *general-mixer-window*
        (update-general-mixer-synths-lists (car (om-subviews *general-mixer-window*))))))


;================ CONTROLLER EDITOR ===================

(defclass faustSynthcontrollerEditor (EditorView) 
  ((params-panels :initform nil :accessor params-panels :type list)
   (tree :initform nil :accessor tree :type nil)
   (bottom-bar :initform nil :accessor bottom-bar :type t)))


(defmethod make-editor-window ((class (eql 'faustSynthcontrollerEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                :close-p t :winshow t :bg-color *om-dark-gray-color*)))
    win))


(defmethod get-win-ed-size ((self faust-synth)) 
  (if (ui-tree self)
      (om-make-point (min 500 (if (> (+ 75 (cadr (las-faust-get-group-size (ui-tree self)))) 500)
                                  (+ (car (las-faust-get-group-size (ui-tree self))) 14)
                                (car (las-faust-get-group-size (ui-tree self))))) (min 500 (+ 75 (cadr (las-faust-get-group-size (ui-tree self))))))
    (om-make-point 75 75)))

(defmethod get-panel-class ((self faustSynthcontrollerEditor)) 'faustSynthcontrollerPanel)

(defmethod update-subviews ((self faustSynthcontrollerEditor))
   (om-set-view-size (panel self) (om-make-point (min 500 (w self)) (min 500 (h self)))))


;=== MAIN PANEL ===
(defclass faustSynthcontrollerPanel (om-scroller) ())

(defmethod get-object ((Self faustSynthcontrollerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self faustSynthcontrollerPanel))
  (report-modifications (om-view-container self)))


;===== Parameters controllers panels =====

(defclass faustSynthparamPanel () 
  ((paramctr :initform nil :initarg :paramctr :accessor paramctr)
   (paramText :initform nil :accessor paramText :type t)
   (paramVal :initform nil :accessor paramVal :type t)
   (paramGraph :initform nil :accessor paramGraph :type t)
   (paramReset :initform nil :accessor paramReset :type t)))

(defclass faustSynthparamPanelview (faustSynthparamPanel om-view) ())

(defmethod update-subviews ((Self faustSynthparamPanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self faustSynthparamPanel))
   (call-next-method))



(defmethod get-object ((Self faustSynthparamPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self faustSynthparamPanel))
  (report-modifications (om-view-container self)))


(defmethod get-parampanel-class ((self faustSynthcontrollerPanel)) 'faustSynthparamPanelview)



;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod metaobj-scrollbars-params ((self faustSynthcontrollerEditor))  '(:h nil))

(defmethod initialize-instance :after ((self faustSynthcontrollerEditor) &rest l)
  (set-edit-param self 'player :libaudiostream)
  (declare (ignore l))
  (let ((x (if (ui-tree (object self))
               (max 75 (car (las-faust-get-group-size (ui-tree (object self)))))
             75))
        
        (y (if (ui-tree (object self))
               (+ 75 (cadr (las-faust-get-group-size (ui-tree (object self)))))
             75))
        (xwin (om-point-h (get-win-ed-size (object self))))
        (ywin (om-point-v (get-win-ed-size (object self))))
        (orange (om-make-color 1 0.5 0))
        group-type
        groups
        (xgrp 0)
        (ygrp 0)
        (xpars 0)
        (ypars 0)
        (offset 0))
    (if (and (synth-ptr (object self)) (ui-tree (object self)))
        (progn
          (setf (tree self) (ui-tree (object self)))
          (setf group-type (las-faust-get-group-type (tree self)))
          (setf groups (las-faust-get-groups-only (tree self)))
          (setf params (las-faust-get-params-only (tree self)))

          (setf (panel self) (om-make-view (get-panel-class self) 
                                           :owner self
                                           :position (om-make-point 0 0) 
                                           :scrollbars (cond ((and (>= x 500) (>= y 500)) t)
                                                             ((and (>= x 500) (< y 500)) :h)
                                                             ((and (< x 500) (>= y 500)) :v)
                                                             (t nil))
                                           :retain-scrollbars t
                                           :field-size  (om-make-point x y)
                                           :size (om-make-point (w self) (h self))
                                           :bg-color *om-light-gray-color*))
     
          (make-faust-group-view self (tree self))
          (setf paramnum 0)
     
          (setf (bottom-bar self) (om-make-view (get-panel-class self)
                                                :owner (panel self)
                                                :bg-color *om-dark-gray-color*
                                                :position (om-make-point 0 (- y 50))
                                                :size (om-make-point (+ 20 x) 50)))
     
          (om-add-subviews (bottom-bar self) (om-make-dialog-item 'om-button (om-make-point (- (round x 2) 30) 5) (om-make-point 60 24)
                                                                  "SVG"
                                                                  :di-action (om-dialog-item-act item
                                                                               (faust-show-svg *om-outfiles-folder* (synth-dsp (object self)) (synth-svg (object self)))))))
      (progn
        (setf (panel self) (om-make-view (get-panel-class self) 
                                         :owner self
                                         :position (om-make-point 0 0) 
                                         :scrollbars nil
                                         :retain-scrollbars t
                                         :field-size  (om-make-point x (- y 50))
                                         :size (om-make-point (w self) (h self))
                                         :bg-color orange))
        (om-add-subviews (panel self) (om-make-dialog-item 'om-static-text
                                                           (om-make-point 17 0)
                                                           (om-make-point 75 75)
                                                           "X"
                                                           :fg-color *om-white-color*
                                                           :font (om-make-font oa::*om-def-bold-font-face* 48 :style '(:bold))))
        ))))

(defmethod make-faust-param-view ((self faustSynthcontrollerEditor) paractrl x y size)
  (let ((res (om-make-view (get-parampanel-class (panel self))
                :paramctr paractrl
                :owner (panel self)
                :bg-color *om-light-gray-color*
                :position (om-make-point x y)
                :size (om-make-point (car size) (cadr size)))))
    (setf (display paractrl) res)
    res))

(defmethod make-faust-group-view ((self faustSynthcontrollerEditor) group &optional (x 0) (y 0))
  (let* ((grouptype (las-faust-get-group-type group))
         (orange (om-make-color 1 0.5 0))
         (children (las-faust-get-group-items group))
         (numchildren (length children))
         (size (las-faust-get-group-size group))
         childlist)
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


(defmethod update-editor-after-eval ((self faustSynthcontrollerEditor) val)
  (setf (object self) val)
  (progn
    (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-view-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-field-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (loop for parampan in (params-panels self) do 
          (om-remove-subviews (panel self) parampan))
    (if (ui-tree (object self))
        (make-faust-group-view self (ui-tree (object self))))
    (setf paramnum 0)))


(defmethod initialize-instance :after ((self faustSynthparamPanel) &rest l)
   (declare (ignore l))
   (do-initialize-param self))


(defmethod do-initialize-param ((self faustSynthparamPanel))  
  (let* ((ptr (synth-ptr (paramctr self)))
         (number (index (paramctr self)))
         (color (om-make-color 0.9 0.9 0.9))
         (orange (om-make-color 1 0.5 0))
         (type (param-type (paramctr self)))
         (name (label (paramctr self)))
         (min (minval (paramctr self)))
         (max (maxval (paramctr self)))
         (def (defval (paramctr self)))
         (range 0)
         (val 0)
         (tracknum (tracknum (paramctr self)))
         (editor (om-view-container (om-view-container self)))
         (curval (las-faust-get-control-value ptr number)))
    (if (= min max) (let () (setf min 0) (setf max 1)) nil)
    (setf range (/ (- max min) 1.0))
    (setf val (* 100.0 (/ (- curval min) range)))
    (om-set-bg-color self color) 

    (cond ((string= type "hslider")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car hsliderSize) 5) 40)
                                                         (format nil "~D" name)
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point (- (round (car hsliderSize) 2) 30) (- (cadr hsliderSize) 31 20)) 
                                                        (om-make-point 60 20)
                                                        (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                        :font *om-default-font1*
                                                        :fg-color orange
                                                        :bg-color color))
             (setf (paramGraph self) (om-make-dialog-item 'om-slider  
                                                          (om-make-point 10 (- (cadr hsliderSize) 36)) 
                                                          (om-make-point 94 31) ""
                                                          :di-action 
                                                          (om-dialog-item-act item
                                                            (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                                              (las-faust-set-control-value ptr number valeur)
                                                              (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                                          :increment 1
                                                          :range '(0 100)
                                                          :value val
                                                          :direction :horizontal
                                                          :tick-side :none))))
          ((string= type "vslider")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car vsliderSize) 15) 50)
                                                         (format nil "~D" name)
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                        (om-make-point 60 20)
                                                        (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                        :font *om-default-font1*
                                                        :fg-color orange
                                                        :bg-color color))
             (setf (paramGraph self)
                   (om-make-dialog-item 'om-slider  
                                        (om-make-point 10 (- (cadr vsliderSize) 104)) 
                                        (om-make-point 31 94) ""
                                        :di-action 
                                        (om-dialog-item-act item
                                          (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                            (las-faust-set-control-value ptr number valeur)
                                            (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                        :increment 1
                                        :range '(0 100)
                                        :value val
                                        :direction :vertical
                                        :tick-side :none))))
          ((string= type "checkbox")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car checkboxSize) 5) 50)
                                                         (format nil "~D" name)
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                        (om-make-point 60 20)
                                                        ""
                                                        :font *om-default-font1*
                                                        :fg-color *om-blue-color*
                                                        :bg-color color))
             (setf (paramGraph self)
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
                                        :checked-p (if (= (las-faust-get-control-value ptr number) 1.0) t nil)))))
          ((string= type "button")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car checkboxSize) 5) 50)
                                                         ""
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                        (om-make-point 60 20)
                                                        ""
                                                        :font *om-default-font1*
                                                        :fg-color *om-blue-color*
                                                        :bg-color color))
             (setf (paramGraph self)
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
                                        :font *om-default-font1*))))
          (t (progn
               (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                           (om-make-point 5 0) 
                                                           (om-make-point (- (car vsliderSize) 15) 50)
                                                           (format nil "~D" name)
                                                           :font *om-default-font1*
                                                           :bg-color color))
               (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                          (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                          (om-make-point 60 20)
                                                          (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                          :font *om-default-font1*
                                                          :fg-color orange
                                                          :bg-color color))
               (setf (paramGraph self)
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
                                   :max-val 100)))))

    (om-add-subviews self
                     (paramText self)
                     (paramVal self)
                     (paramGraph self)
                    ;(paramReset self)
                     )))


;;;==========================
;;; SYNTH PLAYER FEATURES

(defmethod prepare-to-play ((engine (eql :libaudiostream)) (player omplayer) (object faust-synth) at interval params)
  (schedule-task player
                 #'(lambda () (player-play-object engine object)) at))


(defmethod player-loop ((self (eql :libaudiostream)) player &optional (play-list faust-synth))
  (mapcar #'(lambda (a) (if (typep a 'faust-synth)
                            (progn
                              (om-synth-preview-stop a)
                              (om-synth-preview-play a))
                          (progn
                            (las-stop a (tracknum a))
                            (las-loop-play a (tracknum a)))))
          play-list))


;/SYNTH PREVIEW PLAY FUNCTION
;This function plays a preview of a selected synth, on the track which it's plugged or on the hidden player if it's not plugged.
(defmethod om-synth-preview-play ((obj faust-synth))
  (let ((search-res (las-faust-search-synth-console-in-register obj)))
    (if (car search-res)
        (let* ((info (cadr search-res))
               (synth-ptr (nth 1 info))
               (nullsnd (nth 2 info))
               (actual-track (las-sound-tracknum-sys (player-data nullsnd)))
               (res (las-faust-synth-already-plugged-? synth-ptr))
               chan)
          (if res
              (progn
                (om-smart-play nullsnd nil nil (+ (car res) 1)))
            (if (not (las-faust-synth-hidden-already-plugged-? synth-ptr))
                (progn
                  (if (/= -1 (or actual-track -1))
                      (setf chan actual-track)
                    (setf chan (get-free-channel *audio-player-hidden*)))
                  (setf (gethash chan *faust-synths-by-track-hidden*) synth-ptr)
                  (las::AddAudioEffect (gethash chan *effects-lists-hidden*) synth-ptr)
                  (om-smart-play nullsnd)))))
      (om-beep-msg (format nil "Error synth ~A not found in register..." obj)))))

;/SYNTH PREVIEW STOP FUNCTION
;This function stops a preview of a selected synth.
(defmethod om-synth-preview-stop  ((obj faust-synth))
  (let ((search-res (las-faust-search-synth-console-in-register obj)))
    (if (car search-res)
        (let* ((info (cadr search-res))
               (synth-ptr (nth 1 info))
               (nullsnd (nth 2 info))
               (res (las-faust-synth-already-plugged-? synth-ptr)))
          (if res
              (om-smart-stop-visible nullsnd (car res))
            (let ((chan1 (las-faust-synth-hidden-already-plugged-? synth-ptr)))
              (if chan1
                  (progn
                    (remove-faust-effect-from-list synth-ptr (gethash chan1 *effects-lists-hidden*))
                    (setf (gethash chan1 *faust-synths-by-track-hidden*) nil)))
              (om-smart-stop-hidden nullsnd)))))))

(defmethod om-synth-preview-play ((obj list))
  (mapcar 'om-synth-preview-play obj))

(defmethod om-synth-preview-stop ((obj list))
  (mapcar 'om-synth-preview-stop obj))
 
