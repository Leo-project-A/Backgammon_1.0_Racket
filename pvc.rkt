;--------------------------------------------------
;------------- Choosing the starter ---------------

;after the player chose to player VS the computer, this func is choosing who is going to start.
;'press-to-start-red' is displaying a message to the player, so hell roll the dice.
(define (PVC-intro viewport board)
  (display-board board viewport)
  (PVC-intro2 (press-to-start-red viewport) board viewport))
;afther recieving the roll from the player, calls 'PVC-intro3' with a roll from the AI.
(define (PVC-intro2 red board viewport)
  (display-board board viewport)
  (draw-dice (list red) viewport)
  (sleep 1)
  (PVC-intro3 red (press-to-start-AI (add1 (random 6)) viewport) board viewport))
;this func makes a roll for the AI and displays a message.
(define (press-to-start-AI dice viewport)
  (((draw-pixmap-posn g-AI-try 'unknown/mask) viewport) (make-posn 40 240))
  (sleep 2)
  (draw-dice (list dice) viewport)
  (sleep 1)
  dice)
;checks who won the dice roll.
(define (PVC-intro3 red blue board viewport)
  (cond
    ((> red blue)
     (PVC-intro4 'g-red-win 'g-red board viewport))
    ((= red blue)
     (roll-again board viewport))
    (else
     (PVC-intro4 'g-blue-win 'g-blue board viewport))))
;in case of a tie, reCalls 'PVC-intro2' to get new rolls.
(define (roll-again board viewport)
  (display-board board viewport)
  (((draw-pixmap-posn g-dice-tie 'unknown/mask) viewport) (make-posn 200 240))
  (sleep 1)
  (display-board board viewport)
  (PVC-intro2 (press-to-start-red viewport) board viewport))
;after choosing who starts, this func display the message[who starts], and calls 'PVC' to start the game.
(define (PVC-intro4 image player board viewport)
  (display-board board viewport)
  (((draw-pixmap-posn (eval image) 'unknown/mask) viewport) (make-posn 200 240))
  (sleep 1)
  (PVC board player viewport))

;--------------------------------------------------
;--------------- Starting the Game ----------------

;this func checks who's turn it is, for later use.
(define (PVC board player viewport)
  (cond
    ((equal? player 'g-red)(PVC2 board player viewport 'g-red-turn))
    (else
     (AI-turn board player viewport (random-dice-list)))))
;this func draw a message for the player, to roll the dice, so he could start his turn.
(define (PVC2 board player viewport image)
  (((draw-pixmap-posn (eval image) 'unknown/mask) viewport) (make-posn 200 240))
  (PVC3 board player viewport (mouse-click-posn (get-mouse-click viewport))))
;after the message was showen, the player must press "roll" to start.
(define (PVC3 board player viewport posn)
  (cond
    ((and (> (posn-x posn) 275)(< (posn-x posn) 361)(> (posn-y posn) 310)(< (posn-y posn) 341))
     (PVP-start-turn board player (random-dice-list) viewport 'PVC))
    (else
     (PVC3 board player viewport (mouse-click-posn (get-mouse-click viewport))))))

;--------------------------------------------------
;------------------- AI's Turn --------------------

;this func starts the turn of the AI. 
;it displays the board and dice the AI has.
;if the player have won, it calls 'winner' for the proper congratz, same for AI.
;if the dice-list is empty, meaning the AI is done with his moves, it calls 'PVC' for the next player to play.
;ready-for-stage2-? returns #t if all of the AI's pieces are at his base, and he has no pieces at penalty. this means hes ready to remove his pieces.
(define (AI-turn board player viewport dice-list)
  (display-board board viewport)
  (draw-dice dice-list viewport)
  (cond
    ((victory? board player)(winner board player viewport))
    ((victory? board (next-player player))(winner board (next-player player) viewport))
    ((empty? dice-list)(PVC board (next-player player) viewport))
    ((ready-for-stage2? board player)
     (AI-turn-help board player viewport dice-list #t))
    (else
     (AI-turn-help board player viewport dice-list #f))))

;this func displays a "thinking..." message so that the player know the player is thinking.
(define (AI-turn-help board player viewport dice-list stage)
  (((draw-pixmap-posn g-AI-turn 'unknown/mask) viewport) (make-posn 240 280))
  (AI-turn2 board player viewport dice-list stage))

;--------------------------------------------------
;----------------- The Wiehgts --------------------
;---- Default        : (1 1 1 1 1 1)           ----
;---- Atfer Learning : (3 6 5 2 6 5)           ----
;--------------------------------------------------

;this func calls AI-turn3 with the best node possible, he does it by using the Minimax algorithm to choose the best possible move.
(define (AI-turn2 board player viewport dice-list stage)
  (AI-turn3 (minimax (make-node (calc-value board player '(2 5 5 2 7 5)) board player dice-list) player 1 '(2 5 5 2 7 5)) board player viewport))
;if the node-board is empty, this means that there were no moves the AI could make, so a proper message will show.
(define (AI-turn3 node board player viewport)
  (cond
    ((empty? (node-board node))
     (no-more-moves board player viewport 'PVC))
    (else
     (AI-turn4 node player viewport))))

;this func displays the new updated board[after the AI's move], and calls PVC with the next player.
(define (AI-turn4 node player viewport)
  (display-board (node-board node) viewport)
  (PVC (node-board node) (next-player player) viewport))
