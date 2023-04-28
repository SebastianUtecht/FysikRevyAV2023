;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; Hjælpefunktioner til at lave html-filer ud fra nanus-filer.

;; insert-song-from-tex-buffer:

;; 

;; sed '1,/\\begin{song}/d; /\\end{song}/,$d; s/^[[:space:]]*//g; s/\\sings{[^}]*}[[:space:]]*//g; s/\\.*//g; s/[[:space:]]*%.*//g; s/\\%/%/g; s/\(\\texttrademark\|\$\^{TM}\$\)/&trade;/g; /^$/d; s/\(.*\)/<p>\1<\/p>/g' .\Du_skal_regne_kvant.tex > Du_skal.html

(defun song-buffer-to-html (buffer)
  "Konverterer indholdet af buffer 'buffer' fra latex-fil, der bruger
revyens sang-skabelon, til html."
  
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "\\`\\(.\\|\n\\)*?\\\\begin{song}.*" nil t)
      (replace-match "") )
    (when (re-search-forward "\\\\end{song}\\(.\\|\n\\)*" nil t)
      (replace-match "") )
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]+" nil t)
      (replace-match "") )
    (goto-char (point-min))
    (while (re-search-forward "\\\\sings{[^}]*}[[:space:]]*" nil t)
      (replace-match "") )
    (goto-char (point-min))
    (while (re-search-forward "^\\\\.*" nil t)
      (replace-match "") )
    (goto-char (point-min))
    (while (re-search-forward "[[:space:]]*%.*" nil t)
      (replace-match "") )
    (goto-char (point-min))
    (flush-lines "^[[:space:]]*$")
    (goto-char (point-min))
    (while (re-search-forward "\\\\%" nil t)
      (replace-match "%") )
    (goto-char (point-min))
    (while (re-search-forward "\\(\\\\texttrademark\\)\\|\\(\\$\\^{TM}\\$\\)" nil t)
      (replace-match "&trade;" t) )
    (while (re-search-forward "\n[[:space:]]*\\'" nil t)
      (replace-match "") )
    (goto-char (point-min))
    (while (re-search-forward "^\\(.*\\)" nil t)
      (replace-match "<p>\\1</p>") )
    ) )

(defun insert-song-from-tex-buffer (buffer)
  "Indsætter resultatet af at køre song-buffer-to-html på buffer 'buffer'
ved point. 'buffer' bliver ikke ændret."

  (let ((t-buffer (generate-new-buffer "*song-temp")))
    (with-current-buffer t-buffer
      (insert-buffer-substring-no-properties buffer) )
    (song-buffer-to-html t-buffer)
    (push-mark)
    (insert-buffer-substring-no-properties t-buffer)
    (indent-region (mark) (point))
    (kill-buffer t-buffer) ) )

(defun insert-songs-in-plan-file (plan-buffer)
  "Konverter (tex-)filer opremset i en aktoversigt (.plan) fil åben i 
buffer 'buffer' til html med insert-song-from-tex-buffer. Vi går ud fra,
at stier til filer med sange starter med 'sang'. Insætter også et
pauseskilt ved aktovergange."
  
  (let ((target-buffer (current-buffer))
        (pause-counter 0) )
    (with-current-buffer plan-buffer
      (goto-char (point-min))
      (when (re-search-forward "akt 1" nil t)
        (forward-line) )
      (while (progn
               (cond
                ((looking-at "sang")
                 (let (song-temp-buffer
                       (song-file-name (buffer-substring-no-properties
                                        (point)
                                        (line-end-position) )))
                   (with-temp-buffer
                     (insert-file-contents song-file-name)
                     (setq song-temp-buffer (current-buffer))
                     (with-current-buffer target-buffer
                       (insert "<article class=\"build lyr\" id =\""
                               (progn
                                 (string-match "sange/\\(.*\\)\\.tex"
                                               song-file-name )
                                 (replace-match "\\1" nil nil song-file-name) )
                               "\">\n" )
                       (insert-song-from-tex-buffer song-temp-buffer)
                       (insert "\n</article>")
                       (funcall indent-line-function)
                       (insert "\n\n") ) ) ) )
                ((looking-at "[Aa]kt")
                 (with-current-buffer target-buffer
                   (push-mark (point) t)
                   (insert (format "<article class=\"build\" id=\"PAUSE %d\">\n"
                                   (setq pause-counter (1+ pause-counter)) )
                           "<div class=\"pagediv\">\n"
                           "<p style=\"font-size:50px;margin-top:50px;\">PAUSE</p>\n"
                           "<p style=\"margin-top:5px;\">Revyen&trade; fortsætter</p>\n"
                           "<p style=\"margin-top:2px;\">om 20 minutter</p>\n"
                           "</div>\n"
                           "</article>\n\n" )
                   (indent-region (mark) (point))
                   (pop-mark) ) ) )
               (= 0 (forward-line)) )) ) ) )
