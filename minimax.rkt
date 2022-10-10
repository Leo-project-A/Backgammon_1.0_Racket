
;this is the struct for a node. - every node carries data about a state.
;value = the value of the board.
;board = the board of the game, with all the pieces.
;turn = the turn of the player who is about to play.
;dice-list = the dice the player has.
(define-struct node (value board turn dice-list))

;main minimax
;this function starts the minimax process to find the best move to make.
;node = the state of the game right now.
;player = the player who is about to make the move.
;depth = the depth the func is in right now. [when calling minimax for the first time, you can decide what depth you want. the deeper, the better]
;if reached victory or depth 0, returns the node
(define (minimax node player depth weights)
  (cond
    ((victory? (node-board node) player) node)
    ((<= depth 0) node)
    (else
     (minimax2 node player depth (node-turn node) weights))))
;if its the player's turn in the minimax, calls chance with max, else with min.
(define (minimax2 node player depth turn weights)
  (cond
    ((equal? turn player)(chance node player depth turn 'max weights))
    (else
     (chance node player depth turn 'min weights))))

;returns the node with the highest node value
;currentMax starts with -1000000
(define (maximum node player depth turn sons currentMax weights)
  (cond
    ((empty? sons) currentMax)
    (else
     (maximum node player depth turn (rest sons) (calc-max currentMax (minimax (first sons) player (sub1 depth) weights)) weights))))
;returns which node[node or currentMax] is higher
(define (calc-max currentMax node)
  (cond
    ((> (node-value node) (node-value currentMax)) node)
    (else currentMax)))

;reutns the node with the lowest node value
;currentMax starts with +1000000
(define (minimum node player depth turn sons currentMin weights)
  (cond
    ((empty? sons) currentMin)
    (else
     (minimum node player depth turn (rest sons) (calc-min currentMin (minimax (first sons) player (sub1 depth) weights)) weights))))
;returns which node[node or currnetMin] is lower
(define (calc-min currentMax node)
  (cond
    ((< (node-value node) (node-value currentMax)) node)
    (else currentMax)))

;a list with all the possible dice rolls.`
(define all-possible-dice '((1 1)(1 2)(1 3)(1 4)(1 5)(1 6)
                            (2 2)(2 3)(2 4)(2 5)(2 6)
                            (3 3)(3 4)(3 5)(3 6)
                            (4 4)(4 5)(4 6)
                            (5 5)(5 6)
                            (6 6)))

;creates sons with chance!
;if its the first time in minimax[monde has dice already built-in], its goes stright to maximum, without doing chance
(define (chance node player depth turn min/max weights)
  (cond
    ((equal? (node-dice-list node) null)(chance2 node player depth turn min/max all-possible-dice weights))
    ((equal? min/max 'max)(maximum node player depth turn (sons node weights)(make-node -1000000 null null null) weights))
    (else
     (minimum node player depth turn (sons node weights)(make-node +1000000 null null null) weights))))
;this func will return the node, with updated value.
;the updated value will be the average of all the values of the different sons that are created with the diferent dice rolls.
(define (chance2 node player depth turn min/max all-dice weights)
  (make-node 
   (/ (chance3 node player depth turn min/max all-dice weights) (length all-dice))
   (node-board node)(node-turn node) null))
;this func will return the sum of all the values of diferent sons of diferent dice rolls.
(define (chance3 node player depth turn min/max all-dice weights)
  (cond
    ((empty? all-dice) 0)
    (else
     (+ (node-value (minimax (make-node (node-value node)(node-board node)(node-turn node)(first all-dice)) player depth weights))
        (chance3 node player depth turn min/max (rest all-dice) weights)))))

;--------------------------------------------------
;--------- Creating All Possible Moves ------------

;these functions return a well orgenized list with all the possible combination of moves the player can do on the board.
;example- ( ((start-x1 end-x1)(start-x2 end-x2)) ((start-x1 end-x2)(start-x2 end-x3)) ......)
;this list will help constructing the sons in Minimax.
;-- listen closly now! its a bit tricky..
;the func makes every possible move with the first dice, use them on the board, and then calls the func again with the next dice.
;once out of dice, it returns the last move, that is applyd to the moves that brought the player here.

;this func 'applys' the move to the list of moves that brought the player here.
(define (play-book board player dice-list move)
  (cond
    ((empty? dice-list) (list move))
    (else
     (apply-move move (play-book2 (make-move board (first move)(second move) player) player dice-list)))))
;the func take the board player and dice-list, first it checks what stage the player is on, and calls play-book3 with all the possible moves.
(define (play-book2 board player dice-list)
  (cond
    ((ready-for-stage2? board player)
     (play-book3 board player dice-list (first (all-possible-moves board player (list (first dice-list)) #t))))
    (else
     (play-book3 board player dice-list (first (all-possible-moves board player (list (first dice-list)) #f))))))
;this func makes a list with all the moves that have been used. 
(define (play-book3 board player dice-list all-moves)
  (cond
    ((empty? all-moves) '())
    (else
     (append (play-book board player (rest dice-list) (first all-moves))
             (play-book3 board player dice-list (rest all-moves))))))

;this func applys the move to every full-move on the list.
(define (apply-move move L)
  (cond
    ((empty? L) (list (list move)))
    (else
     (apply-move2 move L))))
(define (apply-move2 move L)
  (cond
    ((empty? L) '())
    ((not (list? (first (first L))))(cons (cons move (list (first L))) (apply-move2 move (rest L))))
    (else
     (append (list (cons move (first L))) (apply-move2 move (rest L))))))

;this function takes a pack of moves and applys them on the board.
(define (moves-on-board board player full-move)
  (cond
    ((empty? full-move) board)
    (else
     (moves-on-board (make-move board (first (first full-move))(second (first full-move)) player) player (rest full-move)))))

;this function returns #t, if the 2 board are identical, else #f.
(define (same-board? board1 board2)
  (cond
    ((and
      (equal? (board-game board1)(board-game board2))
      (equal? (board-penalty board1)(board-penalty board2)))
     #t)
    (else #f)))

;when using the 'play-book' with a double roll, there is a lot of useless information coming up. this function searches the list, and removes that info.
(define (filter-moves board player list)
  (cond
    ((empty? list) '())
    (else
     (cons (first list)
           (filter-moves board player (filter-moves2 board player (first list) (rest list)))))))
(define (filter-moves2 board player filter-move moves)
  (cond
    ((empty? moves) '())
    ((same-board? (moves-on-board board player filter-move)
                  (moves-on-board board player (first moves)))
     (filter-moves2 board player filter-move (rest moves)))
    (else
     (cons (first moves)
           (filter-moves2 board player filter-move (rest moves))))))

;this func combains 2 move-lists, that used the dice diferently.
;example, 1 list used dice (1 4), the other used dice (4 1).
(define (combain-move list1 list2)
  (cond
    ((empty? list1) list2)
    ((empty? list2) list1)
    (else
     (append (append (list (first list1))(list (first list2)))
           (combain-move (rest list1)(rest list2))))))

;--------------------------------------------------
;--------------- Building the Sons ----------------

;build a list with all the possible sons
(define (sons node weights)
  (sons2 (node-board node)(node-turn node)(node-dice-list node) weights))
;decides whether the player is ready for stage2 or not.
(define (sons2 board turn dice-list weights)
  (cond
    ((same-values? dice-list)
     (sons3 board turn (filter-moves board turn (play-book2 board turn dice-list)) weights))
    (else
     (sons3 board turn (filter-moves board turn (combain-move (play-book2 board turn dice-list)(play-book2 board turn (reverse dice-list)))) weights))))

;checks if the dice list is consisted of the same value.
(define (same-values? dice-list)
  (cond
    ((= (length dice-list) 1) #t)
    (else
     (same-values?-2 (first dice-list) (rest dice-list)))))
(define (same-values?-2 value dice-list)
  (cond
    ((empty? dice-list) #f)
    ((equal? value (first dice-list)) #t)
    (else
     (same-values?-2 value (rest dice-list)))))
    
;after recieving all the possible combinations of movement, creates the sons one by one, and places them in a list.
(define (sons3 board turn list weights)
  (cond
    ((empty? list) '())
    (else
     (cons
      (sons4 (moves-on-board board turn (first list)) turn weights)
      (sons3 board turn (rest list) weights)))))
;creats the node of the new boards
(define (sons4 newBoard turn weights)
  (make-node (calc-value newBoard turn weights) newBoard (next-player turn) null))

;--------------------------------------------------
;--------------- Value Calculating ----------------

;calculating the value of the board for the player
(define (calc-value board player weights)
  (- (+ (* (first weights)(distance-from-base board player))
        (* (second weights)(penalty-pieces board player))
        (* (third weights)(alone-pieces board player))
        (* (fourth weights)(threat-count board player))
        (* (fifth weights)(dead-pieces-count board player))
        (* (sixth weights)(pieces-in-enemy-base board player)))
        
     (+ (* (first weights)(distance-from-base board (next-player player)))
        (* (second weights)(penalty-pieces board (next-player player)))
        (* (third weights)(alone-pieces board (next-player player)))
        (* (fourth weights)(threat-count board (next-player player)))
        (* (fifth weights)(dead-pieces-count board (next-player player)))
        (* (sixth weights)(pieces-in-enemy-base board (next-player player)))
        )))

;calculates the distance of the pieces of the player from base.
(define (distance-from-base board player)
  (cond
    ((equal? player 'g-red)(distance-from-base2 board player 1 +1 1))
    (else
     (distance-from-base2 board player 24 -1 1))))
(define (distance-from-base2 board player place add-place num)
  (cond
    ((> num 24) 0)
    ((equal? (get-value (board-game board) place 1) player)
     (+ (* (get-value (board-game board) place 0) num)
        (distance-from-base2 board player (+ place add-place) add-place (add1 num))))
    (else
     (distance-from-base2 board player (+ place add-place) add-place (add1 num)))))

;calculates how many pieces the player has in penalty. 5^[num of pieces]X number of blocks in the enemy base. 
(define (penalty-pieces board player)
  (* -1 (expt 3 (penalty-num board player))(blocking-num board (next-player player) 1)))

;calculates how many blocks the enemy has in his base. block = 2 pieces in one palce.
(define (blocking-num board player cost)
  (cond
    ((equal? player 'g-blue)(blocking-num2 board player 6 -1 6 cost))
    (else
     (blocking-num2 board player 19 +1 6 cost))))
(define (blocking-num2 board player place add-place num cost)
  (cond
    ((= num 0) 0)
    ((and 
      (equal? (get-value (board-game board) place 1) player)
      (> (get-value (board-game board) place 0) 1))
     (+ cost (blocking-num2 board player (+ place add-place) add-place (sub1 num) cost)))
    (else
     (+ cost (blocking-num2 board player (+ place add-place) add-place (sub1 num) cost)))))

;calculates how many pieces the player has that are alone.
(define (alone-pieces board player)
  (alone-pieces2 (board-game board) player 1))
(define (alone-pieces2 board player place)
  (cond
    ((>= place 24) 0)
    ((and
      (equal? (get-value board place 1) player)
      (= (get-value board place 0) 1))
     (+ -20 (alone-pieces2 board player (add1 place))))
    (else
     (alone-pieces2 board player (add1 place)))))

;calculates how many dead pieces the player has.
(define (dead-pieces-count board player)
  (* 25 (dead-count (board-game board) player)))
(define (dead-count board player)
  (cond
    ((equal? player 'g-red) (get-value board 25 0))
    (else
     (get-value board 0 0))))

;counts how many pieces are in possible danger compare to the enemy.
(define (threat-count board player)
  (cond
    ((equal? player 'g-red)(threat-count2 (board-game board) player 1 +1 1 0 0))
    (else
     (threat-count2 (board-game board) player 24 -1 1 0 0))))
(define (threat-count2 board player place add-place num my-count enemy-count)
  (cond
    ((> num 24) (* 5 (- my-count enemy-count)))
    ((equal? (get-value board place 1) player)
     (threat-count2 board player (+ place add-place) add-place (add1 num) (add1 my-count) enemy-count))
    ((and
      (equal? (get-value board place 1) (next-player player))
      (> my-count 0))
     (threat-count2 board player (+ place add-place) add-place (add1 num) my-count (add1 enemy-count)))
    (else
     (threat-count2 board player (+ place add-place) add-place (add1 num) my-count enemy-count))))

;calculates how many pieces of the player are still in the enemys base.
(define (pieces-in-enemy-base board player)
  (cond
    ((equal? player 'g-red)(* -15 (pieces-in-enemy-base2 (board-game board) player 1 +1 1)))
    (else
     (* -15 (pieces-in-enemy-base2 (board-game board) player 24 -1 1)))))
(define (pieces-in-enemy-base2 board player place add-place num)
  (cond
    ((> num 6) 0)
    ((equal? (get-value board place 1) player)
     (+ (get-value board place 0)
        (pieces-in-enemy-base2 board player (+ place add-place) add-place (add1 num))))
    (else
     (pieces-in-enemy-base2 board player (+ place add-place) add-place (add1 num)))))

