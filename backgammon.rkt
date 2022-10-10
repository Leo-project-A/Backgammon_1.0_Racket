;--------------------------------------------------
;------------------- Backgammon -------------------

;This list contains the game board. places 1-24 are the playing zone, 0 and 25 are for removing pieces at the end of the game.
;1-24 places are each a list with 2 values.
;first value = number of pieces.
;second value = the player that the pieces belong to.
(define game-board '((0 g-blue)(2 g-red)(0 null)(0 null)(0 null)(0 null)(5 g-blue)(0 null)(3 g-blue)(0 null)(0 null)(0 null)(5 g-red)(5 g-blue)(0 null)(0 null)(0 null)(3 g-red)(0 null)(5 g-red)(0 null)(0 null)(0 null)(0 null)(2 g-blue)(0 g-red)))

;This list contains the Penalty zone. where pieces that have been eaten are kept.
;first value = number of Red eaten pieces.
;second value = number of Blue eaten pieces.
(define penalty-zone '(0 0))

;this struct contains both the board and the penalty-zone, for easier access.
(define-struct board (game penalty))

;This is the starting board - which we will use throughout the game.
(define backgammon-board (make-board game-board penalty-zone))

;--------------------------------------------------
;------------- Usefull Functions ------------------

;returns the value in (place,x) on the board.
;[just to make it easier]
(define (get-value board place x)
  (list-ref (list-ref board place) x))

;set the value in (place,x) on the board.
(define (set-list board place x value)
  (cond
    ((= place 0)(cons (set-subList (first board) x value)(rest board)))
    (else
     (cons (first board)(set-list (rest board) (sub1 place) x value)))))
;sets the value in the subList in (x).
(define (set-subList board x value)
  (cond
    ((= x 0)(cons value (rest board)))
    (else
     (cons (first board)(set-subList (rest board) (sub1 x) value)))))

;returns the next player to play.
(define (next-player player)
  (cond
    ((equal? player 'g-red) 'g-blue)
    (else 'g-red)))

;returns the number of pieces the player has in the penalty zone on the board.
(define (penalty-num board player)
  (cond
    ((equal? player 'g-red)(list-ref (board-penalty board) 0))
    (else
     (list-ref (board-penalty board) 1))))

;returns #t if the value is in the list, else #f.
(define (in-list? list value)
  (cond
    ((empty? list) #f)
    ((equal? (first list) value) #t)
    (else
     (in-list? (rest list) value))))

;returns a new list, only with the num of first values of the original list.
(define (get-sub-list list num)
  (cond
    ((= num 0) '())
    (else
     (cons (first list)(get-sub-list (rest-list) (sub1 num))))))

;returns penalty[player] according to the player.
(define (penalty-player player)
  (cond
    ((equal? player 'g-red) 'penalty-red)
    (else 'penalty-blue)))

;returns dead[player] according to the player.
(define (dead-player player)
  (cond
    ((equal? player 'g-red) 'dead-red)
    (else 'dead-blue)))

;returns the direction of the player's movement on the board.
(define (dir-player player)
  (cond
    ((equal? player 'g-red) +)
    (else -)))

;creates a random list, with each value is 1-6.
(define (random-dice-list)
  (list (add1 (random 6))(add1 (random 6))))

;removes value from the list.
(define (remove-from-list list value)
  (cond
    ((empty? list) '())
    ((equal? value (first list)) (rest list))
    (else
     (cons (first list)(remove-from-list (rest list) value)))))

;--------------------------------------------------
;----------------- Moving Pieces ------------------

;returns an upadted board. doing it by adding 1 to the pieces count in the place x, and setting the player in the player value in x.
;that way, whether there is a piece there already or not, it will do its job.
(define (add-piece board x player)
  (make-board (add-piece2 (board-game board) x player) (board-penalty board)))
(define (add-piece2 board x player)
  (set-list (set-list board x 0 (add1 (get-value board x 0))) x 1 player))

;returns an updated board. doing it by removing 1 from the pieces count in the place x.
;if there is olny 1 piece in x, sets the player value to null. else, keeps the value as it is.
(define (remove-piece board x)
  (make-board (remove-piece2 (board-game board) x) (board-penalty board)))
(define (remove-piece2 board x)
  (cond
    ((= (get-value board x 0) 1)(set-list (set-list board x 0 0) x 1 'null))
    (else
     (set-list board x 0 (sub1 (get-value board x 0))))))

;returns an updated board after a piece has been moved to/from the penalty zone.
;action = + to add, and - to remove.
(define (move-penalty board player action)
  (cond
    ((equal? player 'g-red)(make-board (board-game board)(move-penalty2 (board-penalty board) action 0)))
    (else
     (make-board (board-game board)(move-penalty2 (board-penalty board) action 1)))))
(define (move-penalty2 penalty action num)
  (set-subList penalty num (action (list-ref penalty num) 1)))

;returns an updated board, after a piece has been eaten by the player.
;changes the player value in x, to the player, and moves one of the enemies pieces to the penalty zone.
(define (eat-piece board x player)
  (eat-piece2 (move-penalty board (next-player player) +) x player))
(define (eat-piece2 board x player)
  (make-board (set-list (board-game board) x 1 player) (board-penalty board)))

;returns an updated board, after a piece was added to the player's dead zone.
(define (add-to-dead-zone board player)
  (cond
    ((equal? player 'g-red)(add-to-dead-zone2 board 25))
    (else
     (add-to-dead-zone2 board 0))))
(define (add-to-dead-zone2 board num)
  (make-board (set-list (board-game board) num 0 (add1 (get-value (board-game board) num 0)))(board-penalty board)))

;returns an updated board after a move have been made
;using the functions above, checks what is the scenario of the movement, and acts accordingly.
;start-x = 'penalty[player], means the player has chosen to move one of his pieces from the penalty zone.
;end-x = 'dead[player], means the player has chosen to remove one of his pieces to the dead zone.
(define (make-move board start-x end-x player)
  (cond
    ((and
      (or
       (equal? start-x 'penalty-red)
       (equal? start-x 'penalty-blue))
      (equal? (get-value (board-game board) end-x 1) (next-player player)))
     (eat-piece (move-penalty board player -) end-x player))
    ((or
      (equal? start-x 'penalty-red)
      (equal? start-x 'penalty-blue))
     (add-piece (move-penalty board player -) end-x player))
    ((or
      (equal? end-x 'dead-red)
      (equal? end-x 'dead-blue))
     (add-to-dead-zone (remove-piece board start-x) player))
    ((equal? (get-value (board-game board) end-x 1) (next-player player))
     (eat-piece (remove-piece board start-x) end-x player))
    (else
     (add-piece (remove-piece board start-x) end-x player))))

;--------------------------------------------------
;------------------ Legal tests -------------------

;this func returns #t if the starting move is legal.
;start-x = must be between 1-24, and there must be a player's piece in there.
(define (legal-normal-start? board start-x player)
  (cond
    ((or
      (> start-x 24)(< start-x 1))
     #f)
    ((not (equal? (get-value (board-game board) start-x 1) player)) #f)
    (else #t)))
;this func returns #t if the ending move is legal.
;start-x = must be between 1-24, and there cant be more than 1 enemy piece there.
(define (legal-normal-end? board end-x player)
  (cond
    ((or
      (> end-x 24)(< end-x 1))
     #f)
    ((and
      (equal? (get-value (board-game board) end-x 1) (next-player player))
      (> (get-value (board-game board) end-x 0) 1))
     #f)
    (else #t)))
  
;--------------------------------------------------
;--------------- All Possible Moves ---------------

;this func returns in a list, all of the possible moves, that the player can make with his dice-list.
;IMPORTENT! - the list is divided to the diferent dice in the dice-list, so that later functions could know which moves are from which dice.
;stage = #t/#f according to if the player is ready for stage2 or not.
(define (all-possible-moves board player dice-list stage)
  (cond
    ((> (penalty-num board player) 0)
     (possible-moves-penalty board player dice-list))
    (stage (combain-dead-normal
            (possible-moves-dead board player dice-list)
            (possible-moves-normal board player dice-list)))
    (else
     (possible-moves-normal board player dice-list))))

;this func returns an organised list of the dead and normal lists.
;this is done so it will be easier to use later on. 
;new list => ((all the moves with first dice)(all moves with second dice) etc..)
(define (combain-dead-normal dead-list normal-list)
  (cond
    ((empty? dead-list) normal-list)
    ((empty? normal-list) dead-list)
    (else
     (cons (append (first dead-list) (first normal-list))
           (combain-dead-normal (rest dead-list)(rest normal-list))))))

;this func returns a list with all the possible moves the player can do to the dead zone, with the dice-list.
;if the player enter this func it means that it is STAGE-2.
;NOTE- this func cares ONLY for the direct movement from board to dead zone, meaning these are not the only moves a player can do in stage2.
(define (possible-moves-dead board player dice-list)
  (cond
    ((empty? dice-list) '())
    ((equal? player 'g-red)
     (cons (possible-moves-dead2 board player 'dead-red 19 +1 6 (first dice-list) 0)
             (possible-moves-dead board player (rest dice-list))))
    (else
     (cons (possible-moves-dead2 board player 'dead-blue 6 -1 6 (first dice-list) 0)
             (possible-moves-dead board player (rest dice-list))))))
;this func returns the possible move of the player with the dice.
;place = the place on the board were checking.
;add-place = whether to add or sub place, acording to the movement of the player.
;num = the distance of the place from the dead zone.
;counter = this is so that there wont be any legal mistakes.
(define (possible-moves-dead2 board player end-x place add-place num dice counter)
  (cond
    ((= num 0) '())
    ((and
      (= num dice)
      (equal? (get-value (board-game board) place 1) player))
     (list (list place end-x)))
    ((and
      (equal? (get-value (board-game board) place 1) player)
      (<= (- num dice) 0)
      (= counter 0))
     (list (list place end-x)))
    ((equal? (get-value (board-game board) place 1) player)
     (possible-moves-dead2 board player end-x (+ place add-place) add-place (sub1 num) dice (add1 counter)))
    (else
     (possible-moves-dead2 board player end-x (+ place add-place) add-place (sub1 num) dice counter))))

;if there are pieces at player's penalty zone, this func will return a list with all the possible moves the player can do with his dice.
;if the player enter this func it means he has atleast 1 piece in his penalty zone.
(define (possible-moves-penalty board player dice-list)
  (cond
    ((empty? dice-list) '())
    ((equal? player 'g-red)
     (cons (possible-moves-penalty2 board player 'penalty-red (first dice-list))
             (possible-moves-penalty board player (rest dice-list))))
    (else
     (cons (possible-moves-penalty2 board player 'penalty-blue (- 25 (first dice-list)))
             (possible-moves-penalty board player (rest dice-list))))))
(define (possible-moves-penalty2 board player start-x end-x)
  (cond
    ((legal-normal-end? board end-x player)
     (list (list start-x end-x)))
    (else '())))

;this func returns all the possible "normal" moves the player can make. meaning on the board 1-24, without including dead/penalty zones.
;if the player entered this func, its means there are no pieces in penalty zone.
(define (possible-moves-normal board player dice-list)
  (cond
    ((empty? dice-list) '())
    (else
     (cons (possible-moves-normal2 board player (first dice-list) (dir-player player) 1)
           (possible-moves-normal board player (rest dice-list))))))
(define (possible-moves-normal2 board player dice dir place)
  (cond
    ((> place 24) '())
    ((and
      (legal-normal-start? board place player)
      (legal-normal-end? board (dir place dice) player))
     (cons (list place (dir place dice))
           (possible-moves-normal2 board player dice dir (add1 place))))
    (else
     (possible-moves-normal2 board player dice dir (add1 place)))))

;this func orginisze the all-moves list, for use in 'where-can-I-go'.
(define (append-list all-moves)
  (cond
    ((empty? all-moves) '())
    (else
     (append (first all-moves)(append-list (rest all-moves))))))

;this func is meant to help out the player, by displaying on the board all the places he can go to from the start-x he chose.
;this func returns all the places the player can move to. this by comparing the start-x that the player made, and the start-x in the all-moves list.
(define (where-can-I-go all-moves start-x)
  (cond
    ((empty? all-moves) '())
    ((equal? start-x (first (first all-moves)))
     (cons (second (first all-moves))(where-can-I-go (rest all-moves) start-x)))
    (else
     (where-can-I-go (rest all-moves) start-x))))

;--------------------------------------------------
;------ Removing a Dice from the Dice List --------

;removes the dice the player used, from start-x to end-x, using all moves.
;all-moves = (all-possible-moves board player dice-list stage2)
; -VERY IMPORTENT, because 'all-possible-moves' returns a list with the diferent dice saparated.
(define (remove-dice dice-list start-x end-x all-moves)
  (remove-dice2 dice-list dice-list start-x end-x all-moves))
(define (remove-dice2 original-dice-list test-dice-list start-x end-x all-moves)
  (cond
    ((remove-dice3 start-x end-x (first all-moves)) (remove-from-list original-dice-list (first test-dice-list)))
    (else
     (remove-dice2 original-dice-list (rest test-dice-list) start-x end-x (rest all-moves)))))
;this func checks if the move from start-x to end-x is in moves, -the first of all-moves. 
;if it is, it means the player used the first dice from the dice list.
(define (remove-dice3 start-x end-x moves)
  (cond
    ((empty? moves) #f)
    ((and
      (equal? start-x (first (first moves)))
      (equal? end-x (second (first moves))))
     #t)
    (else
     (remove-dice3 start-x end-x (rest moves)))))
;removes value from the list.
(define (remove-from-list list value)
  (cond
    ((empty? list) '())
    ((equal? value (first list)) (rest list))
    (else
     (cons (first list)(remove-from-list (rest list) value)))))

;--------------------------------------------------
;------------- Conditions in the Game -------------

;returns #t if the player is ready for stage2, -removing pieces. else #f.
;ready for stage2 = meaning the player has all of his pieces in his base [also no pieces in penalty].
(define (ready-for-stage2? board player)
  (cond
    ((> (penalty-num board player) 0) #f)
    ((equal? player 'g-red)(ready-for-stage2?-part2 board player 1 +1 1))
    (else
     (ready-for-stage2?-part2 board player 24 -1 1))))
(define (ready-for-stage2?-part2 board player place add-place num)
  (cond
    ((>= num 19) #t)
    ((equal? (get-value (board-game board) place 1) player) #f)
    (else
     (ready-for-stage2?-part2 board player (+ place add-place) add-place (add1 num)))))

;returns #t if player have won.
;winning means the player has all 15 pieces in dead zone.
(define (victory? board player)
  (cond
    ((equal? player 'g-red) (victory?-2 (get-value (board-game board) 25 0)))
    (else
     (victory?-2 (get-value (board-game board) 0 0)))))
(define (victory?-2 num)
  (cond
    ((= num 15) #t)
    (else #f)))

