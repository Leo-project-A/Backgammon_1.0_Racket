;--------------------------------------------------
;-------------- Player VS Player ------------------


;------------- Choosing the starter ---------------

;after the player has chosen to player against a friend, this func prints the board, and calls 'PVP-intro2' to see which player starts.
(define (PVP-intro viewport board)
  (display-board board viewport)
  (PVP-intro2 (press-to-start-red viewport) board viewport))

;draw a message for the red player to roll to see who starts.
(define (press-to-start-red viewport)
  (((draw-pixmap-posn g-red-try 'unknown/mask) viewport) (make-posn 360 240))
  (roll-random-dice 'g-red viewport (mouse-click-posn (get-mouse-click viewport))))
;draw a message for the blue player to roll to see who starts.
(define (press-to-start-blue viewport)
  (((draw-pixmap-posn g-blue-try 'unknown/mask) viewport) (make-posn 40 240))
  (roll-random-dice 'g-blue viewport (mouse-click-posn (get-mouse-click viewport))))

;recieves a mouse click, that indicates that the player wants to roll the dice.
(define (roll-random-dice player viewport posn)
  (cond
    ((and
      (equal? player 'g-red)
      (> (posn-x posn) 435)(< (posn-x posn) 522)(> (posn-y posn) 310)(< (posn-y posn) 340))
     (add1 (random 6)))
    ((and
      (equal? player 'g-blue)
      (> (posn-x posn) 115)(< (posn-x posn) 202)(> (posn-y posn) 310)(< (posn-y posn) 340))
     (add1 (random 6)))
    (else
     (roll-random-dice player viewport (mouse-click-posn (get-mouse-click viewport))))))
;after reciving the red's dice roll, continues to recieve the blue's one.
(define (PVP-intro2 red board viewport)
  (display-board board viewport)
  (draw-dice (list red) viewport)
  (sleep 1)
  (PVP-intro3 red (press-to-start-blue viewport) board viewport))
;after the 2 players rolled, this func checks who won.
;if red or blue win, there is a message accordingly.
;else, [meaning its a tie] calls 'its-dice-tie'.
(define (PVP-intro3 red blue board viewport)
  (display-board board viewport)
  (draw-dice (list blue) viewport)
  (sleep 1)
  (cond
    ((> red blue)
     (PVP-intro4 'g-red-win 'g-red board viewport))
    ((= red blue)
     (its-dice-tie board viewport))
    (else
     (PVP-intro4 'g-blue-win 'g-blue board viewport))))
;this func draws in the window a message saying there is a tie, and calls 'who-start?' to reRoll.
(define (its-dice-tie board viewport)
  (display-board board viewport)
  (((draw-pixmap-posn g-dice-tie 'unknown/mask) viewport) (make-posn 200 240))
  (sleep 1)
  (display-board board viewport)
  (PVP-intro2 (press-to-start-red viewport) board viewport))
;once there is a winner at the rolling, this func draw the appropriate message and starts the game.
(define (PVP-intro4 image player board viewport)
  (display-board board viewport)
  (((draw-pixmap-posn (eval image) 'unknown/mask) viewport) (make-posn 200 240))
  (sleep 1)
  (PVP board player viewport))

;--------------------------------------------------
;-------------- Starting the game -----------------

;this func checks who's turn it is, for later use.
(define (PVP board player viewport)
  (cond
    ((equal? player 'g-red)(PVP2 board player viewport 'g-red-turn))
    (else
     (PVP2 board player viewport 'g-blue-turn))))
;this func draw a message for the player, to roll the dice, so he could start his turn.
(define (PVP2 board player viewport image)
  (((draw-pixmap-posn (eval image) 'unknown/mask) viewport) (make-posn 200 240))
  (PVP3 board player viewport (mouse-click-posn (get-mouse-click viewport))))
;after the message was showen, the player must press "roll" to start.
(define (PVP3 board player viewport posn)
  (cond
    ((and (> (posn-x posn) 275)(< (posn-x posn) 361)(> (posn-y posn) 310)(< (posn-y posn) 341))
     (PVP-start-turn board player (random-dice-list) viewport 'PVP))
    (else
     (PVP3 board player viewport (mouse-click-posn (get-mouse-click viewport))))))

;this func starts the turn of the player. 
;it displays the board and dice the player has.
;if the player have won, it calls 'winner' for the proper congratz.
;if the dice-list is empty, meaning the player is done with his moves, it calls 'PVP' for the next player to play.
;ready-for-stage2-? returns #t if all of the player's pieces are at his base, and he has no pieces at penalty. this means hes ready to remove his pieces.
(define (PVP-start-turn board player dice-list viewport func)
  (display-board board viewport)
  (draw-dice dice-list viewport)
  (cond
    ((victory? board player)(winner board player viewport))
    ((victory? board (next-player player))(winner board (next-player player) viewport))
    ((empty? dice-list)((eval func) board (next-player player) viewport))
    ((ready-for-stage2? board player)
     (PVP-start-turn-part2 board player dice-list (all-possible-moves board player dice-list #t) viewport #t func))
    (else
     (PVP-start-turn-part2 board player dice-list (all-possible-moves board player dice-list #f) viewport #f func))))

;this func is the second part of the turn-start. after getting all the possible moves according to the stage, it continues.
;all-moves = contain a list with all the possible moves the player can do. -not appended yet!
;all-moves is empty = means the func didnt find any legal mvoes the player could do this turn.
(define (PVP-start-turn-part2 board player dice-list all-moves viewport stage func)
  (cond
    ((empty? (append-list all-moves))(no-more-moves board player viewport func))
    (else
     (turn1 board player dice-list (board-mouse-click viewport (mouse-click-posn (get-mouse-click viewport)) stage) all-moves viewport stage func))))

;if there are no more moves the player can do, a message will appear announcing it, and then it will be the next player's turn.
(define (no-more-moves board player viewport func)
  (((draw-pixmap-posn g-no-more-moves 'unknown/mask) viewport) (make-posn 200 240))
  (sleep 1)
  (display-board board viewport)
  ((eval func) board (next-player player) viewport))

;after reciving the start-x click of the player, this func calls 'turn1-help-out' with the list with all the possible moves the player can do.
;all-moves = list with all the possible moves the player can make. -> no more need to check every move if its legal. =]
(define (turn1 board player dice-list start-x all-moves viewport stage func)
  (turn1-help-out board player dice-list start-x all-moves (where-can-I-go (append-list all-moves) start-x) viewport stage func))

;this func checks if the start-x is legal.
;help-pieces = contain a list with all the possible places the player can move to from start-x.
;if help-pieces is empty, meaning the func 'where-can-I-go' didnt find any legal moves. the player must click again.
(define (turn1-help-out board player dice-list start-x all-moves help-pieces viewport stage func)
  (cond
    ((empty? help-pieces)
     (turn1 board player dice-list (board-mouse-click viewport (mouse-click-posn (get-mouse-click viewport)) stage) all-moves viewport stage func))
    (else
     (display-help-on-board board player dice-list start-x all-moves help-pieces help-pieces viewport stage func))))
;this func takes the help-pieces and display them on the board, so that the player would know, where he can place a piece.
;WHEN this func is done display help-pieces, it calls 'turn2'.
;all-end-x = is the same as help-pieces, just its not changing, so its can be used later on in turn2.
(define (display-help-on-board board player dice-list start-x all-moves all-end-x help-pieces viewport stage func)
  (cond
    ((empty? help-pieces)
     (turn2 board player dice-list start-x (board-mouse-click viewport (mouse-click-posn (get-mouse-click viewport)) stage) all-moves all-end-x viewport stage func))
    (else
     (display-help-on-board2 board player dice-list start-x all-moves all-end-x help-pieces viewport stage func))))
;displays one help piece on the board.
(define (display-help-on-board2 board player dice-list start-x all-moves all-end-x help-pieces viewport stage func)
  (cond
    ((equal? (first help-pieces) 'dead-red)
     (display-dead-help-on-board 'g-dead-help (make-posn 605 40) +13 25 board player dice-list start-x all-moves all-end-x help-pieces viewport stage func))
    ((equal? (first help-pieces) 'dead-blue)
     (display-dead-help-on-board 'g-dead-help (make-posn 605 547) -13 0 board player dice-list start-x all-moves all-end-x help-pieces viewport stage func))
    (else
     (display-help-on-board3 board player dice-list start-x all-moves all-end-x help-pieces viewport stage func))))
;if this is notmal condition, draw the help piece on the board as normal.
(define (display-help-on-board3 board player dice-list start-x all-moves all-end-x help-pieces viewport stage func)
  (draw-piece 'g-help-piece (get-value (board-game board) (first help-pieces) 0) (first help-pieces) viewport)
  (display-help-on-board board player dice-list start-x all-moves all-end-x (rest help-pieces) viewport stage func))
;if this is special condition[dead zone], draw the help piece in dead zone.
(define (display-dead-help-on-board image posn add-y place board player dice-list start-x all-moves all-end-x help-pieces viewport stage func)
  (draw-dead4 (eval image) posn add-y (get-value (board-game board) place 0) viewport)
  (display-help-on-board board player dice-list start-x all-moves all-end-x (rest help-pieces) viewport stage func))

;this func checks if the end-x is legal, on its own and togther with start-x.
;if start-x and end-x are equal, meaning the player changed his mind, this will return to 'PVP-start-turn', in order to recive new start-x.
;if all is legal, this func calls 'PVP-start-turn' with the updated board[after the move], and without the dice the player jsut used.
(define (turn2 board player dice-list start-x end-x all-moves all-end-x viewport stage func)
  (cond
    ((equal? start-x end-x)(PVP-start-turn board player dice-list viewport func))
    ((not (in-list? all-end-x end-x))
     (turn2 board player dice-list start-x (board-mouse-click viewport (mouse-click-posn (get-mouse-click viewport)) stage) all-moves all-end-x viewport stage func))
    (else
     (PVP-start-turn (make-move board start-x end-x player) player (remove-dice dice-list start-x end-x all-moves) viewport func))))

;--------------------------------------------------
;------------------- After Win --------------------

;when a player wins the game, a celebrative message apear to announce it!
;and then the player can choose to restart.
(define (winner board player viewport)
  (display-board board viewport)
  (cond
    ((equal? player 'g-red)(winner2 'g-red-winner board viewport))
    (else
     (winner2 'g-blue-winner board viewport))))
(define (winner2 image board viewport)
  (((draw-pixmap-posn (eval image) 'unknown/mask) viewport) (make-posn 0 0))
  (sleep 2)
  (new-game? (mouse-click-posn (get-mouse-click viewport))))

;gets mouse click. if presed on restart.. start a new game, exit - closes the game.
(define (new-game? posn)
  (cond
    ((and
       (> (posn-x posn) 660)(< (posn-x posn) 780)(> (posn-y posn) 40)(< (posn-y posn) 80))
     (game w))
    ((and
       (> (posn-x posn) 660)(< (posn-x posn) 780)(> (posn-y posn) 200)(< (posn-y posn) 240))
     (close-viewport viewport))
    (else
     (new-game? (mouse-click-posn (get-mouse-click viewport))))))
  

  