;--------------------------------------------------
;------------------- Graphics ---------------------

(require graphics/graphics)
(open-graphics)

;--------------------------------------------------
;----------------- Image Defines ------------------

;the board
(define g-board "graphics/board.png")
;the pieces
(define g-blue "graphics/blue-piece.png")
(define g-red "graphics/red-piece.png")
;the dice
(define g-1 "graphics/dice-1.png")
(define g-2 "graphics/dice-2.png")
(define g-3 "graphics/dice-3.png")
(define g-4 "graphics/dice-4.png")
(define g-5 "graphics/dice-5.png")
(define g-6 "graphics/dice-6.png")
;the rules
(define g-rules-1 "graphics/rules-1.png")
(define g-rules-2 "graphics/rules-2.png")
(define g-rules-3 "graphics/rules-3.png")
(define g-rules-4 "graphics/rules-4.png")
(define g-rules-5 "graphics/rules-5.png")
;notification:
;-player's turn
(define g-red-turn "graphics/red-turn.png")
(define g-blue-turn "graphics/blue-turn.png")
;-computer's turn
(define g-AI-turn "graphics/AI-turn.png")
;-player's start roll
(define g-red-try "graphics/red-try.png")
(define g-blue-try "graphics/blue-try.png")
(define g-AI-try "graphics/AI-try.png")
;-player's start roll win
(define g-red-win "graphics/red-win-roll.png")
(define g-blue-win "graphics/blue-win-roll.png")
(define g-dice-tie "graphics/dice-tie.png")
;-no more moves avaiable
(define g-no-more-moves "graphics/no-more-moves.png")
;the dead area
(define g-red-dead "graphics/dead-red.png")
(define g-blue-dead "graphics/dead-blue.png")
;the help-pieces
(define g-help-piece "graphics/help-piece.png")
(define g-dead-help "graphics/dead-help.png")
;GAME WINNER
(define g-red-winner "graphics/red-winner.png")
(define g-blue-winner "graphics/blue-winner.png")
;player's choise
(define g-choise "graphics/players-choise.png")

;the viewport window
(define w (open-viewport "Backgammon 2.0" 800 600))

(define buffer (open-pixmap "buffer" 800 600))
(define original (open-pixmap "buffer" 800 600))
((draw-pixmap original) g-board (make-posn 0 0) "white")

;-------------------------------------------------------------
;------------------------ Drawing ----------------------------

;display the board on the viewport window. with all pieces.
(define (display-board board viewport)
  (copy-viewport original buffer)
  (draw-board board buffer)
  (copy-viewport buffer viewport))

;draws the board in the viewport window
(define (draw-board board viewport)
  (draw-game-board (rest (board-game board)) 1 viewport)
  (draw-penalty (board-penalty board) viewport)
  (draw-dead (first (reverse (board-game board)))(first (board-game board)) viewport))

;draws the game-board in the viewport window
(define (draw-game-board board place viewport)
  (cond
    ((= place 25) (newline))
    ((empty? (rest board)) (newline))
    ((= (list-ref (first board) 0) 0)(draw-game-board (rest board) (add1 place) viewport))
    (else
     (draw-pieces (list-ref (first board) 0)(list-ref (first board) 1) 0 board place viewport))))
;draws the pieces on place in the board
(define (draw-pieces num color placed-pieces board place viewport)
  (cond
    ((= num 0)(draw-game-board (rest board) (add1 place) viewport))
    (else
     (draw-piece-help num color placed-pieces board place viewport))))
(define (draw-piece-help num color placed-pieces board place viewport)
  (draw-piece color placed-pieces place viewport)
  (draw-pieces (sub1 num) color (add1 placed-pieces) board place viewport))

;returns a posn according to the place on the board
;this will help in drawing the images of pieces
(define (place->posn place)
  (cond
    ((and
      (>= place 1)
      (<= place 6))
     (make-posn (+ 80 (* (- 13 place) 40)) 520))
    ((and
      (>= place 7)
      (<= place 12))
     (make-posn (* (- 13 place) 40) 520))
    ((and
      (>= place 13)
      (<= place 18))
     (make-posn (* (- place 12) 40) 40))
    ((and
      (>= place 19)
      (<= place 24))
     (make-posn (+ 80 (* (- place 12) 40)) 40))))

;draws 1 piece on the board
;identifys where the piece should be drawn, and forwards coordinates accordingly.
(define (draw-piece color placed-pieces place viewport)
  (cond
    ((and
      (>= place 1)
      (<= place 12))
     (draw-piece2 color placed-pieces 0 (place->posn place) -40 viewport))
    (else
     (draw-piece2 color placed-pieces 0 (place->posn place) +40 viewport))))
;counter = helps prevent over flowing the places on the board. when the counter reachs 5, counter is zeroed, and the x posn moves slightly. in order to give a nice stacking effect.
;placed-pieces = the number of pieces already in the place, so that the new piece could be added on top of them.
(define (draw-piece2 color placed-pieces counter posn add-y viewport)
  (cond
    ((= counter 5)
     (draw-piece2 color placed-pieces 0 (make-posn (+ (posn-x posn) 5)(- (posn-y posn)(* 5 add-y))) add-y viewport))
    ((not (= placed-pieces 0))
     (draw-piece2 color (sub1 placed-pieces) (add1 counter) (make-posn (posn-x posn)(+ (posn-y posn) add-y)) add-y viewport))
    (else
     (((draw-pixmap-posn (eval color) 'unknown/mask) viewport) posn))))
     
;draw the penalty zone on the board
(define (draw-penalty penalty-zone viewport)
  (draw-penalty2 (list-ref penalty-zone 0) 'g-red 0 viewport)
  (draw-penalty2 (list-ref penalty-zone 1) 'g-blue 0 viewport))
;draw all the pieces in the penalty zone
(define (draw-penalty2 num color placed-pieces viewport)
  (cond
    ((= num 0) (newline))
    ((equal? color 'g-red)
     (draw-penalty3 num color (make-posn 300 520) -40 placed-pieces viewport))
    ((equal? color 'g-blue)
     (draw-penalty3 num color (make-posn 300 40) +40 placed-pieces viewport))
    ))
(define (draw-penalty3 num color posn add-y placed-pieces viewport)
  (draw-piece2 color placed-pieces 0 posn add-y viewport)
  (draw-penalty2 (sub1 num) color (add1 placed-pieces) viewport))

;draw all the dead pieces on the board
(define (draw-dead red-dead blue-dead viewport)
  (draw-dead2 (list-ref red-dead 0) 'g-red-dead 0 viewport)
  (draw-dead2 (list-ref blue-dead 0) 'g-blue-dead 0 viewport))
(define (draw-dead2 num color placed-pieces viewport)
  (cond
    ((= num 0) (newline))
    ((equal? color 'g-red-dead)
     (draw-dead3 num color (make-posn 605 40) +13 placed-pieces viewport))
    ((equal? color 'g-blue-dead)
     (draw-dead3 num color (make-posn 605 547) -13 placed-pieces viewport))))
(define (draw-dead3 num color posn add-y placed-pieces viewport)
  (draw-dead4 color posn add-y placed-pieces viewport)
  (draw-dead2 (sub1 num) color (add1 placed-pieces) viewport))
(define (draw-dead4 color posn add-y placed-pieces viewport)
  (cond
    ((not (= placed-pieces 0))(draw-dead4 color (make-posn (posn-x posn) (+ (posn-y posn) add-y)) add-y (sub1 placed-pieces) viewport))
    (else
     (((draw-pixmap-posn (eval color) 'unknown/mask) viewport) posn))))

;draws the dice on the baord
(define (draw-dice dice-list viewport)
  (cond
    ((empty? dice-list) (newline))
    ((= (length dice-list) 1)
     (draw-dice2 dice-list viewport 680 360))
    (else
     (draw-dice2 dice-list viewport 680 480))))
(define (draw-dice2 dice-list viewport x y)
  (cond
    ((= (first dice-list) 1) (draw-dice3 'g-1 x y dice-list viewport))
    ((= (first dice-list) 2) (draw-dice3 'g-2 x y dice-list viewport))
    ((= (first dice-list) 3) (draw-dice3 'g-3 x y dice-list viewport))
    ((= (first dice-list) 4) (draw-dice3 'g-4 x y dice-list viewport))
    ((= (first dice-list) 5) (draw-dice3 'g-5 x y dice-list viewport))
    ((= (first dice-list) 6) (draw-dice3 'g-6 x y dice-list viewport))))
(define (draw-dice3 dice x y dice-list viewport)
  (((draw-pixmap-posn (eval dice) 'unknown/mask) viewport) (make-posn x y))
  (draw-dice (rest dice-list) viewport))
