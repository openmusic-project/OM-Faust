(in-package :om)

;======================================================
;FAUST Synths 
;======================================================


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FLATTEN A FAUST SYNTH WITH A STATIC SYNTH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! flatten-faust-synth ((synth faust-synth) duration &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 2
            (if (or (not auto-array) (not (typep auto-array 'class-array)))
                (let* ((file (or filename 
                                 (om-choose-new-file-dialog :directory (def-save-directory) 
                                                            :prompt (om-str "Save as...")
                                                            :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                         ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                         (t nil)))))
                       (faustptr (synth-ptr synth))
                       gaineffect nparams)
                  (if (and duration (numberp duration))
                      (setf duration (max 0.1 duration))
                    (progn
                      (print "The duration slot is invalid, it will be set to 10 seconds")
                      (setf duration 10)))
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
                                   (namestring file) (las::MakeTransformSound (las-faust-make-null-sound duration) efflist 0 0) 
                                   (if (equal sndformat 'aiff)
                                       (logior las::SF_FORMAT_AIFF resolution)
                                     (logior las::SF_FORMAT_WAV resolution)))))
                           (buffer-size 512)
                           (buffer (om-make-pointer (* 4 buffer-size 2)  :clear t))    
                           (res buffer-size))
                      (when (and gain (numberp gain)) 
                        (setq gain (max 0 (min gain 1)))
                        (setq gaineffect (las::MakeVolAudioEffect (float gain)))
                        (las::AddAudioEffect efflist gaineffect))
                      (when (and settings (listp settings))
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
                                            (make-faust-automation-from-bpf (if (listp (cadr elt)) (caadr elt) (cadr elt)) synth (car elt))))))
                (loop for aut in l do 
                      (setf (x-points aut) (om* (x-points aut) (expt 10 (decimals aut)))))
                (flatten-faust-synth l duration :gain gain :settings settings :auto-array auto-array :auto-rescale auto-rescale :filename filename :format format))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FLATTEN A FAUST SYNTH WITH AN OFF TIME AUTOMATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! flatten-faust-synth ((synth faust-automation) duration &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 2
            (let* ((file (or filename 
                             (om-choose-new-file-dialog :directory (def-save-directory) 
                                                        :prompt (om-str "Save as...")
                                                        :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                     ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                     (t nil)))))
                   (faustptr (if (listp (faust-control synth)) 
                                 (if (typep (car (faust-control synth)) 'faust-synth)
                                     (synth-ptr (car (faust-control synth)))
                                   (om-beep-msg "Wrong FX."))
                               (if (typep (faust-control synth) 'faust-synth)
                                   (synth-ptr (faust-control synth))
                                 (om-beep-msg "Wrong FX."))))
                   gaineffect save-x-points)
              (when auto-rescale
                (push (x-points synth) save-x-points)
                (setf (x-points synth) (om-scale (x-points synth) 0 (* 1000 duration))))

              (if (and duration (numberp duration))
                  (setf duration (max 0.1 duration))
                (progn
                  (print "The duration slot is invalid, it will be set to 10 seconds")
                  (setf duration 10)))

              (when (and file faustptr (not (las::las-null-ptr-p faustptr)))

                (setf *last-saved-dir* (make-pathname :directory (pathname-directory file)))

                (let* ((sndformat (or format *def-snd-format*))
                       (resolution (case *audio-res*
                                     (8 las::SF_FORMAT_PCM_S8)
                                     (16 las::SF_FORMAT_PCM_16)
                                     (24 las::SF_FORMAT_PCM_24)
                                     (32 las::SF_FORMAT_PCM_32)              
                                     (otherwise las::SF_FORMAT_PCM_16)))
                       (defvalue (las-faust-get-control-value faustptr (paramnum synth)))
                       (efflist (las::AddAudioEffect (las::MakeAudioEffectList) faustptr))
                       (sndr (las::MakeRendererSound 
                              (las::MakeWriteSound 
                               (namestring file) (las::MakeTransformSound (las-faust-make-null-sound duration) efflist 0 0) 
                               (if (equal sndformat 'aiff)
                                   (logior las::SF_FORMAT_AIFF resolution)
                                 (logior las::SF_FORMAT_WAV resolution)))))
                       (buffer-size 256)
                       (buffer (om-make-pointer (* 4 buffer-size 2)  :clear t))
                       (buffertime (float (* (/ buffer-size las-srate) 1000)))
                       (res buffer-size)
                       (indx 0)
                       (current-time 0))

                  (when (and gain (numberp gain)) 
                    (setq gain (max 0 (min gain 1)))
                    (setq gaineffect (las::MakeVolAudioEffect (float gain)))
                    (las::AddAudioEffect efflist gaineffect))
                  (when settings
                    (print "The \"settings\" slot will be ignored, since you are using an automation."))
                  (loop while (= res buffer-size) do
                        (when (and (< indx (- (length (x-points synth)) 1)) (>= current-time (nth indx (x-points synth))))
                          (las-faust-set-control-value faustptr (paramnum synth) (float (nth indx (y-points synth))))
                          (incf indx))
                        (setq res (las::ReadSound sndr buffer buffer-size (las::GetChannelsSound sndr)))
                        (las-faust-set-control-value faustptr (paramnum synth) (float (nth indx (y-points synth))))
                        (incf current-time buffertime))
                  (when auto-rescale (setf (x-points synth) (car save-x-points)))
                  (om-free-pointer buffer)
                  (las-faust-set-control-value faustptr (paramnum synth) defvalue))
                (probe-file file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FLATTEN A FAUST SYNTH WITH A LIST OF OFF TIME AUTOMATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! flatten-faust-synth (synth duration &key (gain 1.0) (settings nil) auto-array auto-rescale filename (format 'aiff))
            :icon 2
            :initvals '(nil 10 1.0 nil nil nil "faust-synth-out" 'aiff)
            :indoc '("A Faust-Synth" "A duration in seconds" "A gain factor between 0 and 1" "A list of values, to set the Faust-Synth parameters values" "A class-array containing BPFs with parameter names (used to build automations)" "A boolean to know if the automations need to be rescaled to the choosen duration" "A filename" "An output format (wav, aif or aiff)")
            :outdoc '("A sound object")
            :doc "Flatten-Faust-Synth outputs a sound object, which is the result of an off-time record of the synthesizer."

            (let ((tester1 t) (tester2 t)
                  synth-ptr-list paramnum-list
                  xpoints-list ypoints-list save-x-points)
              (loop for elem in synth do
                    (setq tester1 (and tester1 (typep elem 'faust-automation))))
              (when tester1
                (setf synth-ptr-list (loop for elem in synth collect
                                           (if (listp (faust-control elem)) 
                                               (if (typep (car (faust-control elem)) 'faust-synth)
                                                   (synth-ptr (car (faust-control elem)))
                                                 (om-beep-msg "Wrong FX."))
                                             (if (typep (faust-control elem) 'faust-synth)
                                                 (synth-ptr (faust-control elem))
                                               (om-beep-msg "Wrong FX.")))))
                (loop for i from 1 to (1- (length synth-ptr-list)) do
                      (setq tester2 (and tester2 (las::las-eql-ptr (nth i synth-ptr-list) (nth (1- i) synth-ptr-list)))))
                (when tester2
                  (when auto-rescale
                    (loop for aut in synth 
                          for i from 0 do
                          (push (x-points aut) save-x-points)
                          (when (and (listp auto-rescale) (nth i auto-rescale)) (print "rescale")
                            (setf (x-points aut) (om* (x-points aut) (expt 10 (decimals aut))))
                            (setf (x-points aut) (om-scale (x-points aut) 0 (* 1000 duration))))))
                  (let* ((file (or filename 
                                   (om-choose-new-file-dialog :directory (def-save-directory) 
                                                              :prompt (om-str "Save as...")
                                                              :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                           ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                           (t nil)))))
                         (faustptr (car synth-ptr-list))
                         gaineffect)

                    (if (and duration (numberp duration))
                        (setf duration (max 0.1 duration))
                      (progn
                        (print "The duration slot is invalid, it will be set to 10 seconds")
                        (setf duration 10)))
                    (when (and file faustptr (not (las::las-null-ptr-p faustptr)))
                      (setq *last-saved-dir* (make-pathname :directory (pathname-directory file))
                            paramnum-list (loop for elem in synth collect (paramnum elem))
                            xpoints-list (loop for elem in synth collect (x-points elem))
                            ypoints-list (loop for elem in synth collect (y-points elem)))
                      (let* ((sndformat (or format *def-snd-format*))
                             (resolution (case *audio-res*
                                           (8 las::SF_FORMAT_PCM_S8)
                                           (16 las::SF_FORMAT_PCM_16)
                                           (24 las::SF_FORMAT_PCM_24)
                                           (32 las::SF_FORMAT_PCM_32)              
                                           (otherwise las::SF_FORMAT_PCM_16)))
                             (defvalue-list (loop for i from 0 to (1- (length synth)) collect (las-faust-get-control-value faustptr (nth i paramnum-list))))
                             (efflist (las::AddAudioEffect (las::MakeAudioEffectList) faustptr))
                             (sndr (las::MakeRendererSound 
                                    (las::MakeWriteSound 
                                     (namestring file) (las::MakeTransformSound (las-faust-make-null-sound duration) efflist 0 0) 
                                     (if (equal sndformat 'aiff)
                                         (logior las::SF_FORMAT_AIFF resolution)
                                       (logior las::SF_FORMAT_WAV resolution)))))
                             (buffer-size 256)
                             (buffer (om-make-pointer (* 4 buffer-size 2) :clear t))
                             (buffertime (float (* (/ buffer-size las-srate) 1000)))
                             (res buffer-size)
                             (index-list (loop for i for elem in synth collect 0))
                             (current-time 0))

                        (when (and gain (numberp gain)) 
                          (setq gain (max 0 (min gain 1))) 
                          (setq gaineffect (las::MakeVolAudioEffect (float gain)))
                          (las::AddAudioEffect efflist gaineffect))
                        (if settings
                            (print "The \"settings\" slot will be ignored, since you are using an automation."))
                        (loop while (= res buffer-size) do
                              (loop for i from 0 to (1- (length synth)) do
                                    (when (and (< (nth i index-list) (- (length (nth i xpoints-list)) 1)) (>= current-time (nth (nth i index-list) (nth i xpoints-list))))
                                      (las-faust-set-control-value faustptr (nth i paramnum-list) (float (nth (nth i index-list) (nth i ypoints-list))))
                                      (incf (nth i index-list))))
                              (setq res (las::ReadSound sndr buffer buffer-size (las::GetChannelsSound sndr)))
                              (incf current-time buffertime))
                        (when auto-rescale
                          (setq save-x-points (reverse save-x-points))
                          (loop for aut in synth
                                for x in save-x-points do
                                (setf (x-points aut) x)))
                        (om-free-pointer buffer)
                        (loop for i from 0 to (1- (length synth)) do
                              (las-faust-set-control-value faustptr (nth i paramnum-list) (nth i defvalue-list))))
                      (probe-file file)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;