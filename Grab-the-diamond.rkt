;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Grab-the-diamond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; ★★★ CSC 104 2016 Winter Project I : Maze Generator ★★★

; This program:
;
;   I.  Creates a random maze.
;
;   II. Visualizes the maze being built and allows the user to move a character
;        around the maze.


; ★ The Grid, Maze, and Locations ★
;
; A “maze” means a set of *unblocked* squares inside a square grid of squares.
; All other squares in the grid are considered blocked.
;
; A “location” is the x-y co-ordinates of a square inside the grid.


; ★ Algorithm to Generate the Maze ★
;
;   Start with a maze containing only location (0, 0).
;
;   Repeat the following process to expand the maze:
;
;     #1. Randomly pick a location that is already in the maze.
;     #2. Randomly pick a location from the four locations left/right/up/down
;          relative to the location that was picked from the maze in #1.
;     #3. If that location is:
;           • not already in the maze, and
;           • within the bounds of the grid, and
;           • next to exactly one location that is already in the maze
;         then add the location to the maze.


; ★ Data Represention of the Maze ★
;
; A location is represented as a list of two numbers.
; The two numbers are the x and y co-ordinates of the location.
;
; A maze is represented as a list of locations.
;
; So a maze is represented as a list of two-element lists of numbers.
;
; Run this program now to see a list of ten mazes: each maze is just the maze containing
;  the location (0, 0)
;
; When you finish Part I, that list should have ten different mazes, with each maze in the
;  list extending the previous maze by one location (except sometimes two consecutive mazes
;  will be the same, due to some extra randomness in the function ‘try’ that can “give up”
;  temporarily if #3 in the algorithm fails: that's a quick way to fix a technical problem
;  that can occur in Part II).


; ★ Part I: Maze Generation ★
;
; Do the numbered steps below, then do the steps in Part II further down in this file.
;
; HOW TO AVOID THE DREADED “ENDLESS DEBUGGING NIGHTMARE”:
;
;   Do not remove any of the provided ‘check-expect’s.
;
;   Run this program now, and look at the test window that lists “Check failures:”.
;    The first line under it shows you the two values produced by the two expressions
;    in the first failing ‘check-expect‘, followed by a link to that ‘check-expect’.
;    Click on that link, and think about what the two values represent.
;
;   Do the steps one by one, in order.
;
;   Add partial design ‘check-expect’s whenever you have any trouble making a full design.
;    Look at the course's lecture notes for many examples of the process.
;
;   Run the program EACH TIME you add a ‘check-expect’ or fix a function.
;
;     If there is an error reported in the Interactions area: scroll through the code
;      looking for a pink highlight which shows where the error was triggered.
;
;     Otherwise, look at the first “Check Failure” and note the two values, then click
;      on the associated link and study the ‘check-expect’.
;
;   Use the menu item “Reindent All" under “Racket” in DrRacket EVERY TIME you make a change.
;
;
;  1. Fix ‘random-element’ according to its contract, purpose statement, and tests.
;
;  2. Make a full design ‘check-expect’ for ‘offset’.
;  3. Fix ‘offset’ according to its contract, purpose statement, and tests.
;     Make sure you follow your design ‘check-expect’.
;
;  4. Make a full design ‘check-expect’ for ‘neighbours’.
;     Use ‘offset’ in the design.
;  5. Fix ‘neighbours’ according to its contract, purpose statement, and tests.
;     Make sure you follow your design ‘check-expect’.
;
;  6. Fix ‘random-neighbour’ according to its contract, purpose statement, and test.
;     Use ‘neighbours’ and ‘random-element’ in the body.
;
;  7. Fix ‘count-in’ according to its contract, purpose statement, and tests.
;
;  8. Make a full design ‘check-expect’ for ‘count-neighbours’.
;     Use ‘count-in’ and ‘neighbours’.
;  9. Fix ‘count-neighbours’ according to its contract, purpose statement, and tests.
;     Make sure you follow your design ‘check-expect’.
;
; 10. Make a full design ‘check-expect’ for ‘add-location?’.
;     Use ‘count-neighbours’.
; 11. Fix ‘add-location?’ according to its contract, purpose statement, and tests.
;     Make sure you follow your design ‘check-expect’.
;
; 12. Run the program and check the result in the Interactions.
;     If your functions are correct then the result should be a list of valid mazes,
;      each extending the previous one.
;     There is nothing to write for this step.
;     Now move on to Part II below.


; ★ Start of the Implementation ★

; The constant ‘grid-size’ determines the width and height of the grid.
;
; The possible locations in the gride are all (x, y) such that:
;   • 0 ≤ x < grid-size
;   • 0 ≤ y < grid-size
;
(define grid-size 15)
; ★ 1. ★

; random-element :  list  →  any

(check-expect (<= 0 (random-element (range 0 1000 1)) 999)
              #true)
(check-expect (= (random-element (range 0 1000 1))
                 (random-element (range 0 1000 1)))
              #false)
(check-expect (member? (random-element (list "programming" "is" "fun"))
                       (list "programming" "is" "fun"))
              #true)

; Produce a random element from the non-empty list ‘a-list’.
(define (random-element a-list)
  (list-ref a-list (random (length a-list))))


; ★ 2 and 3. ★

; offset :  location number number  →  location
(check-expect (offset (list 104 123) 2 -3)
              (list 106 120))

; A partial design for ‘offset’:
(check-expect (offset (list 104 123) 2 -3)
              (list (+ 104 2) (+ 123 -3)))

; Full Design for ‘offset’:
(check-expect (offset (list 104 123) 2 -3)
              (list (+ 104 2) (+ 123 -3)))

(check-expect (equal? (offset (list 104 123) 2 -3)
                      (list (+ 104 2) (+ 123 0))) #false)

; Produce a location like ‘a-location’, with ‘x’ and ‘y’ added to the ‘x’ and ‘y’ of ‘a-location’.
(define (offset a-location x y)
  (list (+ (list-ref a-location 0) x) (+ (list-ref a-location 1) y)))


; ★ 4 and 5. ★

; neighbours :  location  →  list-of-locations

(check-expect (neighbours (list 123 104))
              (list (list 123 105)
                    (list 123 103)
                    (list 122 104)
                    (list 124 104)))

; Full Design for ‘neighbours’:
(check-expect (neighbours (list 123 104))
              (list (list 123 (+ 104 1))
                    (list 123 (+ 104 -1))
                    (list (+ 123 -1) 104)
                    (list (+ 123  1) 104)))




; Produce a list of the four locations below, above, left, and right of ‘a-location’.
(define (neighbours a-location)
  (list (list (list-ref a-location 0) (+ (list-ref a-location 1) 1))
        (list (list-ref a-location 0) (+ (list-ref a-location 1) -1))
        (list (+ (list-ref a-location 0) -1) (list-ref a-location 1))
        (list (+ (list-ref a-location 0) 1) (list-ref a-location 1))))


; ★ 6. ★

; random-neighbour :  list-of-locations  →  location

(check-expect (member? (random-neighbour (list (list 0 0) (list 123 104)))
                       (list (list 0 1)
                             (list 0 -1)
                             (list -1 0)
                             (list 1 0)
                             (list 123 105)
                             (list 123 103)
                             (list 122 104)
                             (list 124 104)))
              #true)

; Pick a random location from the non-empty list ‘locations’ and produce a random neighbour
;  of that location.
(define (random-neighbour locations)
  (random-element (neighbours (random-element locations))))


; ★ 7. ★

; count-in :  location list-of-locations  →  number

(check-expect (count-in (list 104 123)
                        (list (list 104 123)
                              (list 45 67)))
              1)
(check-expect (count-in (list 100 123)
                        (list (list 104 123)
                              (list 45 67)))
              0)

; A full design for ‘count-in’:
(check-expect (count-in (list 100 123)
                        (list (list 104 123)
                              (list 45 67)))
              (cond [(member? (list 100 123) (list (list 104 123)
                                                   (list 45 67)))
                     1]
                    [else 0]))

; Produce 1 if ‘a-location’ is in ‘locations’, otherwise produce 0.
(define (count-in a-location locations)
  (cond [(member? a-location locations) 1]
        [else 0]))


; ★ 8 and 9. ★

; count-neighbours :  location list-of-locations  →  number

(check-expect (count-neighbours (list 2 30)
                                (list (list 2 31)
                                      (list 1 29)
                                      (list 1 30)
                                      (list 2 30)))
              2)

; A partial design for ‘count-neighbours’:
(check-expect (count-neighbours (list 2 30)
                                (list (list 2 31)
                                      (list 1 29)
                                      (list 1 30)
                                      (list 2 30)))
              (+ (count-in
                  (list 2 31)
                  (list (list 2 31) (list 1 29) (list 1 30) (list 2 30)))
                 (count-in
                  (list 2 29)
                  (list (list 2 31) (list 1 29) (list 1 30) (list 2 30)))
                 (count-in
                  (list 1 30)
                  (list (list 2 31) (list 1 29) (list 1 30) (list 2 30)))
                 (count-in
                  (list 3 30)
                  (list (list 2 31) (list 1 29) (list 1 30) (list 2 30)))))


; Produce the number of neighbours of ‘a-location’ that are in ‘locations’.
(define (count-neighbours a-location locations)
  (+ (count-in (list-ref  (neighbours a-location) 0) locations)
     (count-in (list-ref  (neighbours a-location) 1) locations)
     (count-in (list-ref  (neighbours a-location) 2) locations)
     (count-in (list-ref  (neighbours a-location) 3) locations)))


; ★ 10 and 11. ★

; add-location? :  location maze  →  boolean

(check-expect (add-location? (list 0 0)
                             (list (list 0 0)))
              #false)
(check-expect (add-location? (list 0 1)
                             (list (list 0 0)))
              #true)
(check-expect (add-location? (list 0 -1)
                             (list (list 0 0)))
              #false)
(check-expect (add-location? (list 0 1)
                             (list (list 0 0) (list 0 2)))
              #false)

; A full design for ‘add-location?’:




; Produce #true if ‘a-location’ should be added to ‘a-maze’, otherwise produce #false.
; See the algorithm at the beginning of this file for a description of the conditions
;  required for this function to produce #true.
(define (add-location? a-location a-maze)
  (cond [(member? a-location a-maze) #false]
        [ (>= (list-ref a-location 0) grid-size) #false]
        [ (>= (list-ref a-location 1) grid-size) #false]
        [ (< (list-ref a-location 0) 0) #false]
        [ (< (list-ref a-location 1) 0) #false]
        [ else (cond  [(= (count-neighbours a-location a-maze) 1) #true ]
                      [else #false])]))




; ★★★

; try :  location maze  →  maze
;
; If ‘a-location’ should be added to ‘a-maze’ then produce ‘a-maze’ with ‘a-location’ included.
; Otherwise, with a small probability give up and produce ‘a-maze‘ unchanged.
; Otherwise, try to extend ‘a-maze’ again.
(define (try a-location a-maze)
  (cond [(add-location? a-location a-maze) (list* a-location a-maze)]
        [(zero? (random (sqr grid-size))) a-maze]
        [else (extend a-maze)]))

; extend :  maze  →  maze
;
; Produce the version of ‘a-maze’ from trying to include a random neighbour of one of the locations
; in ‘a-maze’.
(define (extend a-maze)
  (try (random-neighbour a-maze) a-maze))

(define small-maze (extend (list (list 0 0))))

(check-expect (and (= (length small-maze) 2)
                   (member? (list 0 0) small-maze)
                   (or (member? (list 0 1) small-maze)
                       (member? (list 1 0) small-maze)))
              #true)

;   repeated :  function any number  →  list
; ‘(repeated f a n)’ produces ‘(list a (f a) (f (f a)) ...)’ with ‘n’ elements.
(define (repeated f a n)
  (cond [(= n 1) (list a)]
        [else (list* a (repeated f (f a) (- n 1)))]))



; ★ 12. ★

; A list of the intermediate mazes created by extending the initial maze ten times.
(repeated extend (list (list 0 0)) 10)


; ★ Part II: Maze Visualization ★

; 13. Make a full design ‘check-expect’ for ‘trim’.
; 14. Fix ‘trim’ according to its contract, purpose statement, and tests.
;     Make sure you follow your design ‘check-expect’.
;
; 15. Uncomment the ‘big-bang’ at the *end* of this file and run this program.
;     You should see your maze growing: removing shrubs and rocks.
;     You should also be able to move the player right by pressing the right arrow key.
;
; 16. Make some documentation/test ‘check-expect’s for ‘try-move’.
; 17. Make a full design ‘check-expect’ for ‘try-move’.
; 18. Fix ‘try-move’ according to its contract, purpose statement, and tests.
;     Make sure you follow your design ‘check-expect’.
;
; 19. Fix ‘key-react’ so that the player can move in other directions.

(require picturing-programs)

; The state of the animation is a two element list with the current maze and the location of the player.

; tick :  state  →  state
; Produce a new state of the world by extending the maze, but leaving the player in the same place.

(define (tick state)
  
  (cond [(and (= (remainder (fourth state) 100) 0) (>= (fourth state) 100)) (list (extend (first state)) (second state) (third state) (+ (fourth state) 1) (random-element (first state)))]
        [else (list (extend (first state)) (second state) (third state) (+ (fourth state) 1) (fifth state))]
        ))
; ★ 13 and 14. ★

; trim :  image  →  image
(check-expect (trim (above (square 100 "solid" "red")
                           (square 100 "solid" "green")))
              (above (rectangle 100 55 "solid" "red")
                     (rectangle 100 90 "solid" "green")))

; A full design for ‘trim’:
(check-expect (trim (above (square 100 "solid" "red")
                           (square 100 "solid" "green")))
              (above (rectangle 100 (- 100 45) "solid" "red")
                     (rectangle 100 (- 100 10) "solid" "green")))




; Produce ‘an-image’ with 45 pixels cropped off the top and 10 pixels cropped off the bottom.
(define (trim an-image)
  (crop  0 45 (image-width an-image) (- (image-height an-image) 55)  an-image))


; ★★★

(require 2htdp/planetcute)

; The image of the player.
(define kat (trim character-horn-girl))
(define bomb (trim gem-orange))
(define bug  (trim enemy-bug))
(define bug-width (image-width bug))
(define  bug-height (image-height bug))
; The dimensions of a square in the grid, based on the player's dimensions.
(define cell-width (image-width kat))
(define cell-height (image-height kat))

; The image of grass occupying one square in the grid.
(define grass (rectangle cell-width cell-height "solid" "darkgreen"))

; The image of a shrub growing out of the earth.
(define shrub (overlay/align "middle" "bottom"
                             (trim tree-short)
                             (ellipse (floor (* 3/4 cell-width)) (floor (* 1/2 cell-height))
                                      
                                      "solid" "brown")))

; shrub-or-rock :  any  →  image
; Randomly produce the image of a shrub or rock on top of grass, using a shrub 3 out of 4 times.
(define (shrub-or-rock _)
  (overlay (random-element (list shrub shrub shrub (trim rock)))
           grass))

; row :  any  →  image
; Produce a row of shrubs and rocks, containing ‘grid-size’ many of them.
(define (row _) (apply beside (map shrub-or-rock (range 0 grid-size 1))))

; A square grid of shrubs and rocks, according to ‘grid-size’.
(define background (freeze (apply above (map row (range 0 grid-size 1)))))

; grass-at :  location  →  image
; Produce a mostly transparent image except for ‘grass’ placed in the square at ‘a-location’.
(define (grass-at a-location)
  (place-image/align grass
                     (* cell-width (first a-location)) (* cell-height (second a-location))
                     "left" "top"
                     (rectangle (* cell-width grid-size) (* cell-height grid-size)
                                "outline" (make-color 0 0 0 0))))

; draw :  state  →  image
; Produce an image of the maze and the player.
(define bug-win (text "bug win!" 250 "red"))
(define girl-win (text "girl win!" 250 "green"))
(define (draw state)
  (cond
    [(and (= (first (second state)) (first (fifth state))) (= (second (second state)) (second (fifth state))))  (scale 1/2 (place-image/align girl-win  600 600 "center" "center" (underlay/align/offset "left" "top" (underlay/align/offset "left" "top" (underlay/align/offset "left" "top"
                                                                                                            (apply underlay background
                                                                                                                   (map grass-at (first state)))
                                                                                                            (* cell-width (first (second state)))
                                                                                                            (* cell-height (second (second state)))
                                                                                                            kat) (* cell-width (first  (fifth state)))
                                                                                                                 (* cell-height (second (fifth state))) bomb)  (* bug-width (first (third state)))  (* bug-height (second (third state ))) bug )))]


     [(and (= (first (third state)) (first (fifth state))) (= (second (third state)) (second (fifth state))))  (scale 1/2 (place-image/align bug-win  800 800 "center" "center" (underlay/align/offset "left" "top" (underlay/align/offset "left" "top" (underlay/align/offset "left" "top"
                                                                                                            (apply underlay background
                                                                                                                   (map grass-at (first state)))
                                                                                                            (* cell-width (first (second state)))
                                                                                                            (* cell-height (second (second state)))
                                                                                                            kat) (* cell-width (first  (fifth state)))
                                                                                                                 (* cell-height (second (fifth state))) bomb)  (* bug-width (first (third state)))  (* bug-height (second (third state ))) bug )))]





    
    [else   (scale 1/2 (underlay/align/offset "left" "top" (underlay/align/offset "left" "top" (underlay/align/offset "left" "top"
                                                                                                            (apply underlay background
                                                                                                                   (map grass-at (first state)))
                                                                                                            (* cell-width (first (second state)))
                                                                                                            (* cell-height (second (second state)))
                                                                                                            kat) (* cell-width (first  (fifth state)))
                                                                                                                 (* cell-height (second (fifth state))) bomb)  (* bug-width (first (third state)))  (* bug-height (second (third state ))) bug ))]))


; try-adjust-place :  state number number  →  state
(define (try-adjust-place state x y)
  (list (first state)
        (try-move (second state)
                  (offset (second state) x y)
                  (first state)) (third state) (fourth state) (fifth state)))

(define (try-adjust-place-bug state x y)
  (list (first state) (second state)
        (try-move (third state)
                  (offset (third state ) x y)
                  (first state)) (fourth state) (fifth state)))

(define (place-bomb state)
  (list (first state) (second state)))

; ★ 16, 17, and 18. ★

; try-move :  location location maze  →  location

; Documentation/testing ‘check-expect's for ‘try-move’:




; A full design ‘check-expect' for ‘try-move’:





; Produce ‘to-location’ if it's in ‘a-maze’, otherwise produce ‘from-location’.
(define (try-move from-location to-location a-maze)
  (cond [(member? to-location a-maze) to-location]
        [else from-location]))


; ★ 19. ★

; key-react :  state KeyEvent  →  state
; Produce a new state with the player moved according to the user pressing the
;  left, right, up, or down arrow keys.
; This follows the Computer Graphics convention that "down" is the *positive* direction.
(define (key-react state a-key)
  (cond [(key=? a-key "right") (try-adjust-place state 1 0)]
        [(key=? a-key "left")  (try-adjust-place state -1 0)]
        [(key=? a-key "up")    (try-adjust-place state 0 -1)]
        [(key=? a-key "down")  (try-adjust-place state 0 1)]
        [(key=? a-key "d")     (try-adjust-place-bug state 1 0)]
        [(key=? a-key "a")     (try-adjust-place-bug state -1 0)]
        [(key=? a-key "w")     (try-adjust-place-bug state 0 -1)]
        [(key=? a-key "s")     (try-adjust-place-bug state 0 1)]
        [else state]))

(define (gameend state)
        (cond [(and (= (first (second state)) (first (fifth state))) (= (second (second state)) (second (fifth state)))) true]
              [(and (= (first (third state)) (first (fifth state))) (= (second (third state)) (second (fifth state)))) true]
              [else false]))

; ★ 15. ★
(big-bang (list (list (list 0 0) (list 14 14)) (list 0 0) (list 14 14) 0 (list 7 7))
          [on-tick tick]
          [to-draw draw]
          [on-key key-react]
          [stop-when gameend])
