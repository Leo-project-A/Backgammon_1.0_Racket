(load "backgammon.rkt")
(load "minimax.rkt")
(load "pvp.rkt")

;--------------------------------------------------
;------------------- Learning ---------------------

;starts the process of learning. 
;input = the current population and the number of generation[gen] you want to develope
;   ->first time, population->(make-random-population [num of weights] [num of cells])
;output = the latest population [best so far]
(define (learning population gen depth)
  (display-stats population gen depth)
  (cond
    ((= gen 0) (newline))
    (else
     (learning (tournament (make-player-list population) depth) (sub1 gen) depth))))

;displays the stat of the current population.
(define (display-stats population gen depth)
  (display "----- current population is: ------")
  (newline)
  (display population)
  (newline))

;returns a list of the players in the population. each player is [num ,weight list and 0{=number of wins}]
(define (make-player-list population)
  (cond
    ((empty? population) '())
    (else
     (cons (make-player (first population) 0) (make-player-list (rest population))))))

;struct of a player.
;weights = the weight list of the player
;wins = number of time won
(define-struct player (weights wins))

;creates initial random population
;length = length of each cell
;num = number of cells in the population
(define (make-random-population length num)
  (cond
    ((= num 0) '())
    (else
     (cons (make-random-population2 length) (make-random-population length (sub1 num))))))
;creates a cell with random [length] numbers
(define (make-random-population2 length)
  (cond
    ((= length 0) '())
    (else
     (cons (add1 (random 10)) (make-random-population2 (sub1 length))))))

;--------------------------------------------------
;------------------- Tournament -------------------

;makes a tournament between all the cells
;players = list of players
(define (tournament players depth)
  (tournament2 players players depth))
;when length = 1, meaning only last player left[hes not going to play agaisnt himself], so tournament is over, and its time to re-populate.
;original-players = for later use, after tournament is over.
;players = to use during the tournament -> chagnes while tournamnet is running.
(define (tournament2 original-players players depth)
  (cond
    ((= (length players) 1) (show-winners (mutate (repopulate original-players) 1) original-players))
    (else
     (tournament2 (tournament3 original-players (first players) (rest players) depth) (rest players) depth))))

;displays all the player's weights and winnings.
(define (show-winners new-population players)
  (cond
    ((not (empty? players))
     (display-player-to-me (first players) new-population players))
    (else
     new-population)))
(define (display-player-to-me player new-population players)
  (display "player's weights are: ")(display (player-weights player))(newline)
  (display "player's winning count is: ")(display (player-wins player))(newline)(newline)
  (show-winners new-population (rest players)))

;updates the win-count acording to the games player playes against opponents
(define (tournament3 original-players player opponents depth)
  (cond
    ((empty? opponents) original-players)
    (else
     (tournament3 (update-players original-players (learning-game 10 player (first opponents) depth)) player (rest opponents) depth))))

;upadtes the win-count-adds 1 [1 victory] to the player num in the list
(define (update-players players winner)
  (cond
    ((equal? (player-weights winner)(player-weights (first players)))
     (cons (make-player (player-weights (first players)) (add1 (player-wins (first players))))(rest players)))
    (else
     (cons (first players)(update-players (rest players) winner)))))

;takes the mother and the father, and creates a new population, the size of num
(define (repopulate players)
  (repopulate-help (sortList players) (length players)))
(define (repopulate-help best num)
  (repopulate2 (player-weights (first best))(player-weights (second best)) (list (player-weights (first best))(player-weights (second best))) (- num 2)))
;creates a new population in newList using mother and father
(define (repopulate2 mother father newList num)
  (cond
    ((= num 0) newList)
    (else
     (repopulate2 mother father (append newList (list (randomize mother father (length mother)(random 2))))(sub1 num)))))
;creates a new son for the population
(define (randomize mother father length ran)
  (cond
    ((= length 0) '())
    ((= ran 0) (cons (first mother)(randomize (rest mother)(rest father)(sub1 length)(random 2))))
    (else
     (cons (first father)(randomize (rest mother)(rest father)(sub1 length)(random 2))))))

;sorts the list of players by number of wins, and returns the best 2 parents
(define (sortList list)
  (sortList1 list 0 (- (length list) 1)))
(define (sortList1 list index length)
  (cond
    ((= 1 length) (reverse list))
    (else
     (sortList1 (sortList2 list 0  length) 0 (- length 1)))))
(define (sortList2 list index length)
  (cond
    ((= index length) list)
    ((> (player-wins (list-ref list index))(player-wins (list-ref list (+ 1 index))))(sortList2 (switch list index) (+ index 1) length))
    (else
     (sortList2 list (+ 1 index) length))))
(define (switch list index)
  (cond
    ((= index 0) (cons (second list) (cons (first list)(rest (rest list)))))
    (else
     (cons (first list) (switch (rest list) (- index 1))))))

;returns the population after its been throught num of mutations
;num = number of mutations wanted
(define (mutate list num)
  (cond
    ((= num 0) list)
    (else
     (mutate (mutation list (+ 2 (random (- (length list) 2))) (random (length (first list))) (choose-change (random 2))) (sub1 num)))))
;returns the population after a mutation has accured in x y
(define (mutation list x y change)
  (cond
    ((= x 0)(cons (mutation2 (first list) y change)(rest list)))
    (else
     (cons (first list) (mutation (rest list)(sub1 x) y change)))))
;returns the mutated cell in the population
(define (mutation2 subList y change)
  (cond
    ((= y 0)(cons (mutation3 (first subList) change)(rest subList)))
    (else
     (cons (first subList)(mutation2 (rest subList) (sub1 y) change)))))
;returns the weight mutation in the cell
(define (mutation3 num change)
  (cond
    ((or 
      (> (+ num change) 10)
      (< (+ num change) 1))
     (- num change))
    (else
     (+ num change))))
;chooses what kind of mutation to do
;if random did 0, returns -1, else +1
(define (choose-change num)
  (cond
    ((= num 0) -1)
    (else +1)))

;--------------------------------------------------
;-------------- The Learning Game -----------------

;the learning-game makes the games between 2 players.
(define (learning-game num player opponant depth)
  (learning-game2 player opponant (learning-game3 num player opponant depth '(0 0))))
;this func recieves the count of wins. and the player with more wins get the point in the tournament.
(define (learning-game2 player opponant count)
  (cond
    ((> (first count)(second count))
     player)
    (else
     opponant)))
;while count != 0, the 2 players play against each other, and every win counts in count.
(define (learning-game3 num player opponant depth count)
  (cond
    ((= num 0) count)
    (else
     (learning-game3 (sub1 num) player opponant depth (add-to-count count player opponant (CVC backgammon-board 'g-red player opponant depth))))))

;according to winner, updates the count.
(define (add-to-count count player opponant winner)
  (cond
    ((equal? winner player)(list (add1 (first count))(second count)))
    (else
     (list (first count)(add1 (second count))))))

;AIvsAI gmae. if any player wins, the func return him.
(define (CVC board turn player opponant depth)
  (cond
    ((victory? board 'g-red) player)
    ((victory? board 'g-blue) opponant)
    (else
     (CVC-turn board turn (random-dice-list) player opponant depth))))

;this is like a normal game, except here there are 2 AIs. each using the minimax algorithm and his own weights.
(define (CVC-turn board turn dice-list player opponant depth)
  (cond
    ((equal? turn 'g-red)
     (CVC-turn2 
      (minimax (make-node 0 board turn dice-list) turn depth (player-weights player))
      board turn player opponant depth))
    (else
     (CVC-turn2 
      (minimax (make-node 0 board turn dice-list) turn depth (player-weights opponant)) 
      board turn player opponant depth))))
;if node-board empty, the player has no moves. the board is left untouched, and the game continues with the next player.
(define (CVC-turn2 node board turn player opponant depth)
  (cond
    ((empty? (node-board node))
     (CVC board (next-player turn) player opponant depth))
    (else
     (CVC (node-board node) (next-player turn) player opponant depth))))
