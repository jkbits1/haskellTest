
--  "(define spt (lambda () (led_show 15 8 1 1 5)))",
--  "(define sptt (lambda () (led_show 15 8 1 0 5)))",
--  "(define ledd (lambda () (list (led_data '( 6 6 5 5 ) 0) (led_show 4 0 0 0 5) (led_show 1 0 0 0 5) (led_show 2 0 0 0 5) (sptt) )))",
--  "(ledd)",
--  "(define nth (lambda (n xs) (cond ((eq n 1) (car xs)) (t (nth (- n 1) (cdr xs))))))",
--  "(define drop (lambda (x xs) (cond ((eq x 0) xs) (t (drop (- x 1) (cdr xs))))))",
--  "(define take (lambda (x xs) (cond ((eq x 0) nil) (t (cons (car xs) (take (- x 1) (cdr xs)))))))",
--  "(define append (lambda (xs ys) (if (= (car xs) nil) ys (cons (car xs) (append (cdr xs) ys) ))))",
--  "(define rotate (lambda (n xs) (if (= (car xs) nil) nil (append (drop n xs) (take n xs)))))",
--  "(define incf (lambda (m) (let ((xx (+ (eval m) 1))) (set m xx))))",
--  "(define wheels 1)",
--  "(set! wheels '( ( 6 6 5 5 ) ( 2 2 4 3 ) ( 2 3 3 5 ) (10 11 12 13) ))",
--  "(define curWheel 1)",
--  "(define rotCount '(0 0 0 0))",
--  "(define srcHelper (lambda (n v) (append (take (- n 1) rotCount) (cons v (drop n rotCount)))))",
--  "(define setRotCount (lambda (n v) (let ((xx (cond ((eq n 1) (cons v (drop 1 rotCount))) ((eq n 2) (srcHelper n v)) ((eq n 3) (srcHelper n v)) (t (append (take 3 rotCount) (cons v nil))) ))) (set 'rotCount xx))))",
--  "(define loopRotDisp (lambda () (cond ((eq (nth curWheel rotCount) 3) (setRotCount curWheel 0)) (t (setRotCount curWheel (+ (nth curWheel rotCount) 1))))))",
--  "(define loopCurWheel (lambda () (cond ((eq curWheel 4) (set 'curWheel 1)) (t (incf 'curWheel)))))",
--  "(define rotDisp (lambda () (loopRotDisp)))",
--  "(define wheelDisp (lambda () (nth curWheel wheels)))",
--  "(define showDisp (lambda () (list (led_data (rotate (nth curWheel rotCount) (wheelDisp)) 0) (ans) (sptt))))",
--  "(interrupt 2 2)",
--  "(interrupt 4 2)",
--  "(define (int02 pin clicks count ms) (list (rotDisp) (showDisp)))",
--  "(define (int04 pin clicks count ms) (list (loopCurWheel) (showDisp)))",
--  "(define wheelShow (lambda (n) (rotate (nth n rotCount) (nth n wheels))))",
--  "(define zip2 (lambda (xs ys zs) (cond ((eq (car xs) nil) nil) ((eq (car ys) nil) nil) ((eq (car zs) nil) nil) (t (cons (list (car xs) (car ys) (car zs)) (zip2 (cdr xs) (cdr ys) (cdr zs) ) )) ) ))",
--  "(define sum3 (lambda (t) (+ (+ (car t) (nth 2 t)) (nth 3 t))))",
--  "(define ans (lambda () (led_data (mapcar sum3 (zip2 (wheelShow 1) (wheelShow 2) (wheelShow 3))) 4) ))",

--words = [ ( "goat" 6 5 5 ) ( "goad" 2 4 3 ) ( "gold" 3 3 5 ) (10 11 12 13) ]
words = [ [ "fall", "fall", "fall", "fall" ], [ "ball", "fill", "tall", "fell" ], [ "tell", "toll", "till", "tail" ], ["told", "roll", "loll", "poll"], ["cold", "cold", "cold", "cold"] ]

rotCount = [0, 0, 0, 0]

srcHelper n v = take (n - 1) rotCount ++ v : (drop n rotCount)

setRotCount n v =
  let xx = if n == 1
            then v : drop 1 rotCount
            else if n == 2
              then srcHelper n v
              else if n == 3
                then srcHelper n v
                else (take 3 rotCount) ++ (v : [])
  in
    --    set 'rotCount
    xx


-- russian roulette
    -- twnety one simple version

-- one suit deck
deck = [1..13]

-- ignore random issues
deal = ran $ length deck

-- return (player hand, new deck)
newDeck player deck humanMove =
  let
    card = deal
    humanMove != humanMove
  in
    (player ++ [card], take (card - 1) deck ++ drop (card) deck)

humanDeal human deck =
  let
    (p, d) = newDeck human deck
  in



-- binary test
binary = [ [ "0101", "1010", "1110", "0001" ], [ "ball", "fill", "tall", "fell" ], [ "tell", "toll", "till", "tail" ], ["told", "roll", "loll", "poll"], ["cold", "cold", "cold", "cold"] ]
bin_ans = [ "10", [ "ball", "fill", "tall", "fell" ], [ "tell", "toll", "till", "tail" ], ["told", "roll", "loll", "poll"], ["cold", "cold", "cold", "cold"] ]

