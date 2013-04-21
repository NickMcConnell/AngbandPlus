
;
; Translate an old-style ``mixed'' vault definition to a new-style
; ``separated'' definition. This assumes that a copy of a vault defintion
; is currently selected.
;
; Warning: This function will convert old monster symbols (a-h) to new
; ones! (0-9)
;
; Therefore, don't run on a vault that has buildings.

(defun fix-vault-monst ()
  (interactive)
  
  (subst-char-in-region (point) (mark) ?# ?-)
  (subst-char-in-region (point) (mark) ?% ?-)
  (subst-char-in-region (point) (mark) ?+ ?-)
  (subst-char-in-region (point) (mark) ?\; ?-)
  (subst-char-in-region (point) (mark) ?< ?-)
  (subst-char-in-region (point) (mark) ?> ?-)
  (subst-char-in-region (point) (mark) ?^ ?-)
  (subst-char-in-region (point) (mark) ?. ?-)

  (subst-char-in-region (point) (mark) ?T ?.)
  (subst-char-in-region (point) (mark) ?, ?:)
  (subst-char-in-region (point) (mark) ?8 ?^)
  (subst-char-in-region (point) (mark) ?9 ?#)
  (subst-char-in-region (point) (mark) ?@ ?\;)
  (subst-char-in-region (point) (mark) ?P ?@)

  (subst-char-in-region (point) (mark) ?a ?0)
  (subst-char-in-region (point) (mark) ?b ?1)
  (subst-char-in-region (point) (mark) ?c ?2)
  (subst-char-in-region (point) (mark) ?d ?3)
  (subst-char-in-region (point) (mark) ?e ?4)
  (subst-char-in-region (point) (mark) ?f ?5)
  (subst-char-in-region (point) (mark) ?g ?6)
  (subst-char-in-region (point) (mark) ?h ?7)
  (subst-char-in-region (point) (mark) ?i ?8)
  (subst-char-in-region (point) (mark) ?j ?9)

  (subst-char-in-region (point) (mark) ?A ?-)
  (subst-char-in-region (point) (mark) ?B ?-)
  (subst-char-in-region (point) (mark) ?C ?-)
  (subst-char-in-region (point) (mark) ?D ?-)
  (subst-char-in-region (point) (mark) ?E ?-)
  (subst-char-in-region (point) (mark) ?F ?-)
  (subst-char-in-region (point) (mark) ?G ?-)
  (subst-char-in-region (point) (mark) ?H ?-)
  (subst-char-in-region (point) (mark) ?I ?-)
  (subst-char-in-region (point) (mark) ?J ?-)
  (subst-char-in-region (point) (mark) ?K ?-)
  (subst-char-in-region (point) (mark) ?L ?-)
  (subst-char-in-region (point) (mark) ?M ?-)
  (subst-char-in-region (point) (mark) ?O ?-)
  (subst-char-in-region (point) (mark) ?Q ?-)
  (subst-char-in-region (point) (mark) ?S ?-)
  (subst-char-in-region (point) (mark) ?U ?-)
  (subst-char-in-region (point) (mark) ?V ?-)
  (subst-char-in-region (point) (mark) ?W ?-)
  (subst-char-in-region (point) (mark) ?X ?-)
  (subst-char-in-region (point) (mark) ?Y ?-))


;
; Replace the monster/object symbols on the terrain part of the
; vault specification with a symbol of your choice.
;

(defun fix-vault-feat (chr)
  (interactive "cReplace with what:")

  (subst-char-in-region (point) (mark) ?T chr)
  (subst-char-in-region (point) (mark) ?& chr)
  (subst-char-in-region (point) (mark) ?, chr)
  (subst-char-in-region (point) (mark) ?8 chr)
  (subst-char-in-region (point) (mark) ?9 chr)
  (subst-char-in-region (point) (mark) ?@ chr)
  (subst-char-in-region (point) (mark) ?P chr)

  (subst-char-in-region (point) (mark) ?a chr)
  (subst-char-in-region (point) (mark) ?b chr)
  (subst-char-in-region (point) (mark) ?c chr)
  (subst-char-in-region (point) (mark) ?d chr)
  (subst-char-in-region (point) (mark) ?e chr)
  (subst-char-in-region (point) (mark) ?f chr)
  (subst-char-in-region (point) (mark) ?g chr)
  (subst-char-in-region (point) (mark) ?h chr)
  (subst-char-in-region (point) (mark) ?i chr)
  (subst-char-in-region (point) (mark) ?j chr))
