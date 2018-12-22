(in-package :om)

;======================================================
;FAUST Effects 
;======================================================

(setf *faust-folder* (om-make-pathname :directory (append (pathname-directory (lib-pathname (find-library "OM-Faust"))) '("resources" "libs"))))


(defmethod! set-faust-folder ((path string))
            :icon '(186)
            (setf *faust-folder* path))

(defmethod! set-faust-folder ((path pathname))
            :icon '(186)
            (setf *faust-folder* (namestring path)))

(defmethod! set-faust-folder ((path null))
            :icon '(186)
            (setf *faust-folder* (namestring (om-choose-directory-dialog))))

(defmethod! faust-file ((path string) &optional (unix nil))
            :icon '(186)
            (let ((pa (string+ (namestring *faust-folder*) path)))
              (if unix (namestring pa) pa)))

(defmethod! faust-file ((path null) &optional (unix nil))
            :icon '(186)
            (let ((pa (namestring *faust-folder*)))
              (if unix (namestring pa) pa)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;REDIRECT TO METHOD WITH SOUND DATA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! apply-faust-fx ((s sound) effect &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 1
            :initvals '(nil nil 1.0 nil nil nil "faust-fx-out" 'aiff)
            :indoc '("A sound object" "A Faust-Fx or a list of Faust-Automation" "A gain factor between 0 and 1" "A list of values, to set the Faust-Fx parameters values" "A class-array containing BPFs with parameter names (used to build automations)" "A boolean to know if the automations need to be rescaled to the sound length" "A filename" "An output format (wav, aif or aiff)")
            :outdoc '("A sound object")
            :doc "Apply-Faust-FX outputs a sound object, which is the result of an off-time record of the s sound object, going through the effect"
            (apply-faust-fx (get-las-sound-data s) effect :gain gain :settings settings :auto-array auto-array :auto-rescale auto-rescale :filename filename :format format))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;APPLY FAUST FX WITH A STATIC FAUST FX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! apply-faust-fx ((s las-sound-data) (effect faust-fx) &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 1
            (if (or (not auto-array) (not (typep auto-array 'class-array)))
                (let* ((file (or filename 
                                 (om-choose-new-file-dialog :directory (def-save-directory) 
                                                            :prompt (om-str "Save as...")s
                                                            :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                         ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                         (t nil)))))
                       (faustptr (effect-ptr effect))
                       gaineffect nparams)
                  (when (and file faustptr (not (las::las-null-ptr-p faustptr)))
                    (setq *last-saved-dir* (make-pathname :directory (pathname-directory file)))
                    (let* ((sndformat (or format *def-snd-format*))
                           (resolution (case *audio-res*
                                         (8 las::SF_FORMAT_PCM_S8)
                                         (16 las::SF_FORMAT_PCM_16)
                                         (24 las::SF_FORMAT_PCM_24)
                                         (32 las::SF_FORMAT_PCM_32)              
                                         (otherwise las::SF_FORMAT_PCM_16)))
                           (efflist (las::AddAudioEffect (las::MakeAudioEffectList) faustptr))
                           (sndr (las::MakeRendererSound 
                                  (las::MakeWriteSound
                                   (namestring file) (las::MakeTransformSound (ptr s) efflist 0 0) 
                                   (if (equal sndformat 'aiff)
                                       (logior las::SF_FORMAT_AIFF resolution)
                                     (logior las::SF_FORMAT_WAV resolution)))))
                           (buffer-size 256)
                           (buffer (om-make-pointer (* 4 buffer-size 2) :clear t))
                           (res buffer-size))
                      (when (and gain (numberp gain)) 
                        (setq gain (max 0 (min gain 1)))
                        (setq gaineffect (las::MakeVolAudioEffect (float gain)))
                        (las::AddAudioEffect efflist gaineffect))
                      (when (and settings (lisp settings))
                        (setq nparams (las::getcontrolcount faustptr))
                        (loop for i from 0 to (- nparams 1) 
                              for v in settings do
                              (when (numberp v)
                                (las::SetControlValue faustptr i (float v))
                                (print (format nil "Setting ~d to ~d..." (car (las::getcontrolparam faustptr i)) (las::GetControlValue faustptr i))))))
                      (loop while (= res buffer-size) do
                            (setq res (las::ReadSound sndr buffer buffer-size (las::GetChannelsSound sndr))))
                      (om-free-pointer buffer))
                    (probe-file file)))
              (let ((infolist (lcontrols auto-array)) l)
                (setq l (remove nil (loop for elt in infolist collect
                                          (when (if (listp (cadr elt)) (caadr elt) (cadr elt))
                                            (make-faust-automation-from-bpf (if (listp (cadr elt)) (caadr elt) (cadr elt)) effect (car elt))))))
                (loop for aut in l do 
                      (setf (x-points aut) (om* (x-points aut) (expt 10 (decimals aut)))))
                (apply-faust-fx s l :gain gain :settings settings :auto-array auto-array :auto-rescale auto-rescale :filename filename :format format))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;APPLY FAUST FX WITH ONE OFF TIME AUTOMATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! apply-faust-fx ((s las-sound-data) (effect faust-automation) &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 1
            (let* ((file (or filename 
                             (om-choose-new-file-dialog :directory (def-save-directory) 
                                                        :prompt (om-str "Save as...")
                                                        :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                     ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                     (t nil)))))
                   (faustptr (if (listp (faust-control effect)) 
                                 (if (typep (car (faust-control effect)) 'faust-fx)
                                     (effect-ptr (car (faust-control effect)))
                                   (om-beep-msg "Wrong FX."))
                               (if (typep (faust-control effect) 'faust-fx)
                                   (effect-ptr (faust-control effect))
                                 (om-beep-msg "Wrong FX."))))
                   gaineffect save-x-points)
              (when auto-rescale 
                (push (x-points effect) save-x-points)
                (setf (x-points effect) (om-scale (x-points effect) 0 (/ (las::getlengthsound (ptr s)) (/ las-srate 1000.0)))))
              (when (and file faustptr (not (las::las-null-ptr-p faustptr)) (faustfun effect))
                (setq *last-saved-dir* (make-pathname :directory (pathname-directory file)))
                (let* ((sndformat (or format *def-snd-format*))
                       (resolution (case *audio-res*
                                     (8 las::SF_FORMAT_PCM_S8)
                                     (16 las::SF_FORMAT_PCM_16)
                                     (24 las::SF_FORMAT_PCM_24)
                                     (32 las::SF_FORMAT_PCM_32)              
                                     (otherwise las::SF_FORMAT_PCM_16)))
                       (defvalue (las-faust-get-control-value faustptr (paramnum effect)))
                       (efflist (las::AddAudioEffect (las::MakeAudioEffectList) faustptr))
                       (sndr (las::MakeRendererSound 
                              (las::MakeWriteSound 
                               (namestring file) (las::MakeTransformSound (ptr s) efflist 0 0) 
                               (if (equal sndformat 'aiff)
                                   (logior las::SF_FORMAT_AIFF resolution)
                                 (logior las::SF_FORMAT_WAV resolution)))))
                       (buffer-size 256)
                       (buffer (om-make-pointer (* 4 buffer-size 2) :clear t))
                       (buffertime (float (* (/ buffer-size las-srate) 1000)))
                       (res buffer-size)
                       (indx 0)
                       (current-time 0))
                  (when (and gain (numberp gain)) 
                    (setq gain (max 0 (min gain 1))) 
                    (setq gaineffect (las::MakeVolAudioEffect (float gain)))
                    (las::AddAudioEffect efflist gaineffect))
                  (if settings
                      (print "The \"settings\" slot will be ignored, since you are using an automation."))
                  (loop while (= res buffer-size) do
                        (when (and (< indx (- (length (x-points effect)) 1)) (>= current-time (nth indx (x-points effect))))
                          (las-faust-set-control-value faustptr (paramnum effect) (nth indx (y-points effect)))
                          (incf indx))
                        (setq res (las::ReadSound sndr buffer buffer-size (las::GetChannelsSound sndr)))
                        (las-faust-set-control-value faustptr (paramnum effect) (nth indx (y-points effect)))
                        (incf current-time buffertime))
                  (when auto-rescale (setf (x-points effect) (car save-x-points)))
                  (om-free-pointer buffer)
                  (las-faust-set-control-value faustptr (paramnum effect) defvalue))
                (print (probe-file file)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;APPLY FAUST FX WITH A LIST OF OFF TIME AUTOMATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! apply-faust-fx ((s las-sound-data) (effect list) &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 1
            (let ((tester1 t) (tester2 t) (tester3 t)
                  effect-ptr-list paramnum-list
                  xpoints-list ypoints-list save-x-points)
              (loop for elem in effect do
                    (setq tester1 (and tester1 (typep elem 'faust-automation))))
              (when tester1
                (setq effect-ptr-list (loop for elem in effect collect
                                            (if (listp (faust-control elem)) 
                                                (if (typep (car (faust-control elem)) 'faust-fx)
                                                    (effect-ptr (car (faust-control elem)))
                                                  (om-beep-msg "Wrong FX."))
                                              (if (typep (faust-control elem) 'faust-fx)
                                                  (effect-ptr (faust-control elem))
                                                (om-beep-msg "Wrong FX.")))))
                (loop for i from 1 to (1- (length effect-ptr-list)) do
                      (setq tester2 (and tester2 (las::las-eql-ptr (nth i effect-ptr-list) (nth (1- i) effect-ptr-list)))))
                (when tester2
                  (when auto-rescale
                    (loop for aut in effect 
                          for i from 0 do
                          (push (x-points aut) save-x-points)
                          (when (and (listp auto-rescale) (nth i auto-rescale))
                            (setf (x-points aut) (om* (x-points aut) (expt 10 (decimals aut))))
                            (setf (x-points aut) (om-scale (x-points aut) 0 (/ (las::getlengthsound (ptr s)) (/ las-srate 1000.0)))))))
                  (let* ((file (or filename 
                                   (om-choose-new-file-dialog :directory (def-save-directory) 
                                                              :prompt (om-str "Save as...")
                                                              :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                           ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                           (t nil)))))
                         (faustptr (car effect-ptr-list))
                         gaineffect)
                    (loop for e in effect do (setq tester3 (and tester3 (faustfun e))))
                    (when (and file faustptr (not (las::las-null-ptr-p faustptr)) tester3)
                      (setq *last-saved-dir* (make-pathname :directory (pathname-directory file))
                            paramnum-list (loop for elem in effect collect (paramnum elem))
                            xpoints-list (loop for elem in effect collect (x-points elem))
                            ypoints-list (loop for elem in effect collect (y-points elem)))
                      (let* ((sndformat (or format *def-snd-format*))
                             (resolution (case *audio-res*
                                           (8 las::SF_FORMAT_PCM_S8)
                                           (16 las::SF_FORMAT_PCM_16)
                                           (24 las::SF_FORMAT_PCM_24)
                                           (32 las::SF_FORMAT_PCM_32)              
                                           (otherwise las::SF_FORMAT_PCM_16)))
                             (defvalue-list (loop for i from 0 to (1- (length effect)) collect (las-faust-get-control-value faustptr (nth i paramnum-list))))
                             (efflist (las::AddAudioEffect (las::MakeAudioEffectList) faustptr))
                             (sndr (las::MakeRendererSound 
                                    (las::MakeWriteSound 
                                     (namestring file) (las::MakeTransformSound (ptr s) efflist 0 0) 
                                     (if (equal sndformat 'aiff)
                                         (logior las::SF_FORMAT_AIFF resolution)
                                       (logior las::SF_FORMAT_WAV resolution)))))
                             (buffer-size 256)
                             (buffer (om-make-pointer (* 4 buffer-size 2)  :clear t))
                             (buffertime (float (* (/ buffer-size las-srate) 1000)))
                             (res buffer-size)
                             (index-list (loop for i for elem in effect collect 0))
                             (current-time 0))

                        (when (and gain (numberp gain))
                          (setq gain (max 0 (min gain 1)))
                          (setq gaineffect (las::MakeVolAudioEffect (float gain)))
                          (las::AddAudioEffect efflist gaineffect))
                        (when settings
                          (print "The \"settings\" slot will be ignored, since you are using an automation."))
                        (loop while (= res buffer-size) do
                              (loop for i from 0 to (1- (length effect)) do
                                    (when  (and (< (nth i index-list) (- (length (nth i xpoints-list)) 1)) (>= current-time (nth (nth i index-list) (nth i xpoints-list))))
                                      (las-faust-set-control-value faustptr (nth i paramnum-list) (float (nth (nth i index-list) (nth i ypoints-list))))
                                      (incf (nth i index-list))))
                              (setq res (las::ReadSound sndr buffer buffer-size (las::GetChannelsSound sndr)))
                              (incf current-time buffertime))
                        (when auto-rescale
                          (setq save-x-points (reverse save-x-points))
                          (loop for aut in effect
                                for x in save-x-points do
                                (setf (x-points aut) x)))
                        (om-free-pointer buffer)
                        (loop for i from 0 to (1- (length effect)) do
                              (las-faust-set-control-value faustptr (nth i paramnum-list) (nth i defvalue-list))))
                      (probe-file file)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;