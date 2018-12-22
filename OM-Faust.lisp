;;;===================================================
;;;
;;; OM-Faust
;;;
;;;===================================================


(in-package :om)

;;;LOAD FAUST API FIRST (*faust-folder* needed in faust-effect-fun file).
(compile&load (namestring (om-relative-path '("sources") "las-dsp")))
(compile&load (namestring (om-relative-path '("sources") "faust-api")))
(compile&load (namestring (om-relative-path '("sources") "faust-parser-api")))
(compile&load (namestring (om-relative-path '("sources") "faust-effect")))
(compile&load (namestring (om-relative-path '("sources") "faust-synth")))
(compile&load (namestring (om-relative-path '("sources") "faust-mixer")))
(compile&load (namestring (om-relative-path '("sources") "faust-automation")))
(compile&load (namestring (om-relative-path '("sources") "faust-effect-fun")))
(compile&load (namestring (om-relative-path '("sources") "faust-synth-fun")))


(om::fill-library '((nil (
                          ("LibAudioStream DSP" nil nil 
                                                (las-sound-cut
                                                 las-sound-fade
                                                 las-sound-loop
                                                 las-sound-silence
                                                 las-sound-mix 
                                                 las-sound-seq 
                                                 las-sound-vol
                                                 las-sound-effect
                                                 las-vol-effect
                                                 las-pan-effect
                                                 las-stereo-pan-effect
                                                 las-record-sound) 
                                                nil) 
                          ("Faust" nil 
                                   (faust-fx 
                                    faust-synth 
                                    faust-automation) 
                                   (apply-faust-fx 
                                    flatten-faust-synth)
                                   nil)
                          ) nil nil nil))
                         
                  (om::find-library "OM-Faust"))


(om::set-lib-release 1.0)

(las-faust-init-system)
(om-add-exit-cleanup-func 'las-clean-faust-files t)


(doc-library "OM-Faust is a library which allows you to write and compile Faust code from OpenMusic.
The current version is compatible with OM 6.8.

<br><br>
Faust (Yann Orlarey, Dominique Fober, Stéphane Letz, GRAME - 2009) is a functionnal programming DSP language.
<br><br>
With this library, you can build both Faust effects and Faust Synthesizers. These objects are integrated into OpenMusic through the LibAudioStream audio library, which is provided with OpenMusic.
<br><br>
Effects and Synthesizers can be plugged on player channels using the \"General Mixer\", and Synthesizers can also be used as specific audio objects.
<br><br>
You can also build automations of these objects parameters thanks to the faust-automation object.
<br><br>
For more information, and Faust code examples, see : <a href=\"http://faust.grame.fr/\">Faust Website</a>
" (find-library "OM-Faust"))


(om-print "
;;;============================================================
;;; OM-Faust
;;; (c) D. Bouche - IRCAM - Representations Musicales - 2013
;;;============================================================
")

;;; (gen-lib-reference (find-library "OM-Faust"))