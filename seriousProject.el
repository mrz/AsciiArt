(setq letters '(
            (A . "
 XX
X  X
XXXX
X  X
X  X")
            (B . "
XXX
X  X
XXXX
X  X
XXX")
            (C . "
XXXX
X
X
X
XXXX")
            (D . "
XXX
X  X
X  X
X  X
XXX")
            (E . "
XXXX
X
XXXX
X
XXXX")
            (F . "
XXXX
X
XXXX
X
X")
            (G . "
XXXX
X
X XX
X  X
XXXX")
            (H . "
X  X
X  X
XXXX
X  X
X  X")
            (I . "
XXXXX
  X
  X
  X
XXXXX")
            (J . "
   X
   X
   X
  xX
XXX")
            (K . "
X  X
X X
XX
X X
X  X")
            (L . "
X
X
X
X
XXXX")
            (M . "
X   X
X X X
X x X
X   X
X   X")
            (N . "
X   X
XX  X
X X X
X  XX
X   X")
            (O . "
XXXX
X  X
X  X
X  X
XXXX")
            (P . "
XXXX
X  X
XXXX
X
X  ")
            (Q . "
XXXX
X  X
X  X
X  X
XXXXXX")
            (R . "
XXXX
X  X
XXXX
X  X
X  X")
            (S . "
XXXX
X
XXXX
   X
XXXX")
            (T . "
XXXXXX
  XX
  XX
  XX
  XX")
            (U . "
X  X
X  X
X  X
X  X
XXXX")
            (V . "
X   X
X   X
X   X
 X X
  X ")
            (W . "
X   X   X
X   X   X
X   X   X
 X X X X
  X   X ")
            (X . "
X   X
 X X
  X
 X X
X   X")
            (Y . "
X   X
 X X
  X
  X
  X")
            (Z . "
XXXXX
   X
  X
 X
XXXXX")))

(defun transform (letter)
    (cdr (assoc letter letters)))

(defun foo(mystr transform)
  (mapconcat transform (coerce mystr 'list) "\n"))

(defun for-skype (start end smiley)
  (interactive "r\nsSmiley:")
  (let ((m (copy-marker end)))
    (replace-regexp " " "     " nil start m)
    (replace-regexp "X" smiley nil start m)))

(defun string-for-skype(s smiley)
  (replace-regexp-in-string "X" smiley
                            (replace-regexp-in-string " " "     " s)))

(defun art->skype (art char smiley)
  (replace-regexp-in-string char  smiley
                            (replace-regexp-in-string " " "     " art)))

(defun string->art (str)
  (foo str (lambda (ch)
             (cdr (assoc (intern (upcase (string ch))) letters)))))

(defun string->skype (str smiley)
  (art->skype (string->art str)
              "X"
              smiley))

(defun skype-art (&optional start end smiley)
  (interactive "r\nsSmiley:")
  (let ((str (cond ((region-active-p)
                    (buffer-substring start end))
                   (t (read-from-minibuffer "Sentence: ")))))
    (kill-new (string->skype str smiley))))


(defun number-of-lines (art)
  (length (s-split "\n" art)))

(defun art-width (art)
  (apply #'max (mapcar 'length (s-split "\n" art))))

(defun pad-art (art)
  (let ((n (art-width art)))
    (s-join "\n" (mapcar (lambda (l) (s-pad-right n " " l))
                         (s-split "\n" art)))))

(defun art-concat (art-1 art-2)
  (s-join "\n"
          (mapcar* (lambda (l1 l2)
                     (concat l1 "  " l2))
                   (s-split "\n" (pad-art art-1))
                   (s-split "\n" (pad-art art-2)))))

(defun string->horizontal-art (str)
  (reduce 'art-concat (mapcar (lambda (ch)
                                (pad-art (cdr (assoc (intern (upcase (string ch))) letters))))
                              str)))

(defun skype-horizontal-art (sentence smiley)
  (interactive "sSentence: \nsSmiley: ")
  (kill-new (art->skype (string->horizontal-art sentence) "X" smiley)))
