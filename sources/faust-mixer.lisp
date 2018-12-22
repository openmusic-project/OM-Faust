(in-package :om)

(defvar *more-mixer-items-displayed-once?* nil)

;;; REDEFINITION
(defun mixer-track-size () 400)

;;; REDEFINITION
(defun make-more-mixer-items (panel channel pos)
  (let* ((effectlist (build-faust-effect-list channel))
         (synthlist (build-faust-synth-list channel))
         (effectlist1 effectlist) (effectlist2 effectlist) (effectlist3 effectlist) 
         (effectlist4 effectlist) (effectlist5 effectlist)
         bar3 synth-text synth bar4 faust-text effect1 effect2 effect3 effect4 effect5)

    (setf bar3 (om-make-view 'bar-item 
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))
    (incf pos 4)
    (setf synth-text (om-make-dialog-item 'om-static-text 
                                          (om-make-point 17 pos) 
                                          (om-make-point 75 16)
                                          "SYNTH"
                                          :font *om-default-font1*))
    (incf pos 20)
    (setf synth (om-make-dialog-item 'om-pop-up-dialog-item 
                                     (om-make-point 0 pos) 
                                     (om-make-point 75 12)
                                     ""
                                     :di-action (om-dialog-item-act item
                                                  (pop-up-las-synth-plug panel item channel (om-get-item-list item)))
                                     :font *om-default-font1*
                                     :range synthlist
                                     :value (cadr (gethash 0 (gethash channel *faust-synths-by-track*)))))
    (incf pos 25)
    (setf bar4 (om-make-view 'bar-item
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))
    (incf pos 4)
    (setf faust-text (om-make-dialog-item 'om-static-text 
                                          (om-make-point 27 pos) 
                                          (om-make-point 75 16)
                                          "FX"
                                          :font *om-default-font1*))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 0)
              (setf effectlist1 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist1 :test #'string=))))
    (setf effect1 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 0 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist1
                                       :value (cadr (gethash 0 (gethash channel *faust-effects-by-track*)))))
    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 1)
              (setf effectlist2 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist2 :test #'string=))))
    (setf effect2 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 1 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist2
                                       :value (cadr (gethash 1 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 2)
              (setf effectlist3 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist3 :test #'string=))))
    (setf effect3 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 2 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist3
                                       :value (cadr (gethash 2 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 3)
              (setf effectlist4 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist4 :test #'string=))))
    (setf effect4 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 3 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist4
                                       :value (cadr (gethash 3 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 4)
              (setf effectlist5 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist5 :test #'string=))))
    (setf effect5 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 4 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist5
                                       :value (cadr (gethash 4 (gethash channel *faust-effects-by-track*)))))

    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5)
    (setf *more-mixer-items-displayed-once?* t)
    (list bar3
          synth-text
          synth
          bar4
          faust-text
          effect1
          effect2
          effect3
          effect4
          effect5)))


;/UPDATE AVAILABLE EFFECTS SLOTS
;This function makes effects pop-up dialogs available or not according to the number of plugged effect on each channel
(defun update-available-effects-slots (effect1 effect2 effect3 effect4 effect5)
  (if (= (om-get-selected-item-index effect1) 0)
      (progn
        (om-enable-dialog-item effect2 nil)
        (om-enable-dialog-item effect3 nil)
        (om-enable-dialog-item effect4 nil)
        (om-enable-dialog-item effect5 nil))
    (progn
      (om-enable-dialog-item effect2 t)
      (if (= (om-get-selected-item-index effect2) 0)
          (progn
            (om-enable-dialog-item effect3 nil)
            (om-enable-dialog-item effect4 nil)
            (om-enable-dialog-item effect5 nil))
        (progn
          (om-enable-dialog-item effect3 t)
          (if (= (om-get-selected-item-index effect3) 0)
              (progn
                (om-enable-dialog-item effect4 nil)
                (om-enable-dialog-item effect5 nil))
            (progn
              (om-enable-dialog-item effect4 t)
              (if (= (om-get-selected-item-index effect4) 0)
                  (progn
                    (om-enable-dialog-item effect5 nil))
                (om-enable-dialog-item effect5 t)))))))))


;/BUILD FAUST EFFECT LIST FUNCTION
;This function returns a list of availables effects for each channel.
(defun build-faust-effect-list (channel)
  (let ((n (- (las-get-number-faust-effects-register) 1))
        (final-list (list "-------"))) 
    (loop for i from 0 to n do
          (if (or (= 0 (nth 1 (gethash i *faust-effects-register*))) (= (+ channel 1) (nth 1 (gethash i *faust-effects-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-effects-register*)))))))
    final-list))

;/BUILD FAUST SYNTH LIST FUNCTION
;This function returns a list of availables effects for each channel.
(defun build-faust-synth-list (channel)
  (let ((n (- (las-get-number-faust-synths-register) 1))
        (final-list (list "-------"))) 
    (loop for i from 0 to n do
          (if (or (= 0 (nth 1 (gethash i *faust-synths-register*))) (= (+ channel 1) (nth 1 (gethash i *faust-synths-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-synths-register*)))))))
    final-list))

;/POP UP LAS EFFECT PLUG FUNCTION
;This function plug and/or unplug effect according to the user action on a pop-up effect dialog.
;It also update effect lists on each pop-up
(defun pop-up-las-effect-plug (panel item channel effect-number effectlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-effect-name-in-register (om-get-selected-item item))) *faust-effects-register*)))
        (name (nth (om-get-selected-item-index item) effectlist))
        newlist
        )
    ;unplug old one
    (if (gethash effect-number (gethash channel *faust-effects-by-track*))
        (progn
          (las-faust-remove-effect-from-track (car (gethash effect-number (gethash channel *faust-effects-by-track*))) channel)
          (las-faust-pack-track-effects channel)))
    ;plug new one
    (if pointer
        (las-faust-add-effect-to-track pointer name channel))
    ;update effects lists
    (update-general-mixer-effects-lists panel)))

;/POP UP LAS SYNTH FUNCTION
;This function plug and/or unplug a synth according to the user action on a pop-up synth dialog.
;It also update synth lists on each channel.
(defun pop-up-las-synth-plug (panel item channel synthlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-synth-name-in-register (om-get-selected-item item))) *faust-synths-register*)))
        (name (nth (om-get-selected-item-index item) synthlist))
        newlist)
    ;unplug old one
    (if (gethash 0 (gethash channel *faust-synths-by-track*))
        (las-faust-remove-synth-from-track (car (gethash 0 (gethash channel *faust-synths-by-track*))) channel))
    ;plug new one
    (if pointer
        (las-faust-add-synth-to-track pointer name channel))
    ;update effects lists
    (update-general-mixer-synths-lists panel)))

;/UPDATE GENMIXER EFFECTS LISTS FUNCTION
;This function updates the effect lists on each pop-up effect dialog.
(defun update-general-mixer-effects-lists (panel) 
  (when *more-mixer-items-displayed-once?*
    (let ((newlistn (make-hash-table))
          newlist
          (effectn (make-hash-table)))
      (loop for i from 0 to (- las-channels 1) do
            (setf newlist (build-faust-effect-list i))
            (loop for k from 0 to 4 do
                  (setf (gethash k newlistn) newlist))
            (loop for j from 0 to 4 do
                  (loop for k from 0 to 4 do
                        (if (/= j k)
                            (setf (gethash j newlistn) (remove (cadr (gethash k (gethash i *faust-effects-by-track*))) (gethash j newlistn) :test #'string=))))
                  (setf (gethash j effectn) (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))))
                  (om-set-item-list (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))) (gethash j newlistn))
                  (om-set-selected-item (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))) (cadr (gethash j (gethash i *faust-effects-by-track*)))))
            (update-available-effects-slots (gethash 0 effectn) 
                                            (gethash 1 effectn) 
                                            (gethash 2 effectn) 
                                            (gethash 3 effectn) 
                                            (gethash 4 effectn))))))

;/UPDATE GENMIXER SYNTHS LISTS FUNCTION
;This function updates the synth lists on each pop-up synth dialog.
(defun update-general-mixer-synths-lists (panel)
  (when *more-mixer-items-displayed-once?*
    (let (newlist)
      (loop for i from 0 to (- las-channels 1) do
            (setf newlist (build-faust-synth-list i))
            (om-set-item-list (nth 11 (om-subviews (nth i (om-subviews panel)))) newlist)
            (om-set-selected-item (nth 11 (om-subviews (nth i (om-subviews panel)))) (cadr (gethash 0 (gethash i *faust-synths-by-track*))))))))
