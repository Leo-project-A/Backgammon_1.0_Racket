(load "backgammon.rkt")
(load "graphics.rkt")
(load "pvp.rkt")
(load "pvc.rkt")
(load "minimax.rkt")

;--------------------------------------------------
;--------------------- Main -----------------------

;disaplys the board in the viewport window, with all the buttons, but no pieces.
;display a message for the player, to choose if he wants to play against a friend, or the computer.
(define (game viewport)
  ((draw-pixmap viewport) g-board (make-posn 0 0))
  (((draw-pixmap-posn g-choise 'unknown/mask) viewport) (make-posn 40 160))
  (make-choise viewport (mouse-click-posn (get-mouse-click viewport))))

;this func checks where the player clicked, on the PVP button, or the PVC button, and atcs accordingly.
(define (make-choise viewport posn)
  (cond
    ((and
      (> (posn-x posn) 60)(< (posn-x posn) 260)(> (posn-y posn) 310)(< (posn-y posn) 410))
     (PVP-intro viewport backgammon-board))
    ((and
      (> (posn-x posn) 420)(< (posn-x posn) 620)(> (posn-y posn) 310)(< (posn-y posn) 410))
     (PVC-intro viewport backgammon-board))
    (else
     (make-choise viewport (mouse-click-posn (get-mouse-click viewport))))))


;--------------------------------------------------
;-------------- Mouse Operations ------------------

;recives a posn of the player's mouse click on the board, and reutrns the place he presed it, according to the listed board, that backgammon.rkt uses.
;if the player click on the penalty zone -> reutnrs 'penalty-[player].
;if the player click on the dead zone -> reutnrs 'dead-[player].
;if the player click on the 'restart' option -> restarts the game, from the func (game).
;if the player click on the 'rules' option -> opens a new viewport window with all the rules.
;if the player click on the 'exit' option -> closes the viewport and the game.
(define (board-mouse-click viewport posn stage)
  (cond
    ((and
      (> (posn-x posn) 40)(< (posn-x posn) 280)(> (posn-y posn) 40)(< (posn-y posn) 280))
     (+ 12 (quotient (posn-x posn) 40)))
    ((and
      (> (posn-x posn) 360)(< (posn-x posn) 600)(> (posn-y posn) 40)(< (posn-y posn) 280))
     (+ 12 (quotient (- (posn-x posn) 80) 40)))
    ((and
      (> (posn-x posn) 40)(< (posn-x posn) 280)(> (posn-y posn) 320)(< (posn-y posn) 560))
     (- 13 (quotient (posn-x posn) 40)))
    ((and
      (> (posn-x posn) 360)(< (posn-x posn) 600)(> (posn-y posn) 320)(< (posn-y posn) 560))
     (- 13 (quotient (- (posn-x posn) 80) 40)))
    ;----------------------!!! BLU PENALTY ZONE
    ((and
      (> (posn-x posn) 300)(< (posn-x posn) 340)(> (posn-y posn) 40)(< (posn-y posn) 280))
     'penalty-blue)
    ;----------------------!!! RED PENALTY ZONE
    ((and
      (> (posn-x posn) 300)(< (posn-x posn) 340)(> (posn-y posn) 320)(< (posn-y posn) 560))
     'penalty-red)
    ;----------------------!!! BLU dead ZONE
    ((and
      stage (> (posn-x posn) 600)(< (posn-x posn) 640)(> (posn-y posn) 360)(< (posn-y posn) 560))
     'dead-blue)
    ;----------------------!!! RED dead ZONE
    ((and
      stage (> (posn-x posn) 600)(< (posn-x posn) 640)(> (posn-y posn) 40)(< (posn-y posn) 240))
     'dead-red)
    ;------------------------- OPTIONS
    ((and
       (> (posn-x posn) 660)(< (posn-x posn) 780)(> (posn-y posn) 40)(< (posn-y posn) 80))
     (game w))
    ((and
       (> (posn-x posn) 660)(< (posn-x posn) 780)(> (posn-y posn) 120)(< (posn-y posn) 160))
     (show-rules viewport stage))
    ((and
       (> (posn-x posn) 660)(< (posn-x posn) 780)(> (posn-y posn) 200)(< (posn-y posn) 240))
     (close-viewport viewport))
    (else 
     (board-mouse-click viewport (mouse-click-posn (get-mouse-click viewport)) stage))))

;--------------------------------------------------
;--------------------- Rules ----------------------

;displays the rules for the player.
(define (show-rules viewport stage)
  (copy-viewport viewport buffer)
  (show-rules-help viewport 1 stage))
;according to the page, forwards the right image.
(define (show-rules-help viewport page stage)
  (cond
    ((= page 1)(show-rules-help2 viewport page 'g-rules-1 stage))
    ((= page 2)(show-rules-help2 viewport page 'g-rules-2 stage))
    ((= page 3)(show-rules-help2 viewport page 'g-rules-3 stage))
    ((= page 4)(show-rules-help2 viewport page 'g-rules-4 stage))
    ((= page 5)(show-rules-help2 viewport page 'g-rules-5 stage))))
;displays the page, and forwards a mouse click from the player.
(define (show-rules-help2 viewport page image stage)
  (((draw-pixmap-posn (eval image) 'unknown/mask) viewport) (make-posn 0 0))
  (show-rules2 viewport page (mouse-click-posn (get-mouse-click viewport)) stage))
;according to the mouse click, switchs to next plage/previous page, or returns to the game.
(define (show-rules2 viewport page posn stage)
  (cond
    ((and
       (> (posn-x posn) 40)(< (posn-x posn) 160)(> (posn-y posn) 520)(< (posn-y posn) 560)
       (> page 1))
     (show-rules-help viewport (sub1 page) stage))
    ((and
       (> (posn-x posn) 240)(< (posn-x posn) 320)(> (posn-y posn) 520)(< (posn-y posn) 560)
       (< page 5))
     (show-rules-help viewport (add1 page) stage))
    ((and
       (> (posn-x posn) 520)(< (posn-x posn) 640)(> (posn-y posn) 520)(< (posn-y posn) 560))
     (exit-rules viewport stage))
    (else
     (show-rules2 viewport page (mouse-click-posn (get-mouse-click viewport)) stage))))
;exits the rules, re-draws the board, and continues playing.
(define (exit-rules viewport stage)
  (copy-viewport buffer viewport)
  (board-mouse-click viewport (mouse-click-posn (get-mouse-click viewport)) stage))



;THIS FUNC STARTS THE GAME.
(game w)