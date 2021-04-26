; ****************** BEGIN INITIALIZATION FOR ACL2s MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);
(make-event
 (er-progn
  (set-deferred-ttag-notes t state)
  (value '(value-triple :invisible))))

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/ccg/ccg" :uncertified-okp nil :dir :system :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/base-theory" :dir :system :ttags :all)


#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/custom" :dir :system :ttags :all)

;; guard-checking-on is in *protected-system-state-globals* so any
;; changes are reverted back to what they were if you try setting this
;; with make-event. So, in order to avoid the use of progn! and trust
;; tags (which would not have been a big deal) in custom.lisp, I
;; decided to add this here.
;; 
;; How to check (f-get-global 'guard-checking-on state)
;; (acl2::set-guard-checking :nowarn)
(acl2::set-guard-checking :all)

;Settings common to all ACL2s modes
(acl2s-common-settings)
;(acl2::xdoc acl2s::defunc) ;; 3 seconds is too much time to spare -- commenting out [2015-02-01 Sun]

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/acl2s-sigs" :dir :system :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s mode.") (value :invisible))

(acl2::xdoc acl2s::defunc) ; almost 3 seconds

; Non-events:
;(set-guard-checking :none)

(set-inhibit-warnings! "Invariant-risk" "theory")

(in-package "ACL2")
(redef+)
(defun print-ttag-note (val active-book-name include-bookp deferred-p state)
  (declare (xargs :stobjs state)
	   (ignore val active-book-name include-bookp deferred-p))
  state)

(defun print-deferred-ttag-notes-summary (state)
  (declare (xargs :stobjs state))
  state)

(defun notify-on-defttag (val active-book-name include-bookp state)
  (declare (xargs :stobjs state)
	   (ignore val active-book-name include-bookp))
  state)
(redef-)

(acl2::in-package "ACL2S")

; ******************* END INITIALIZATION FOR ACL2s MODE ******************* ;
;$ACL2s-SMode$;ACL2s

(defdata hand nat) ;;represents one of a player's hands (the number of fingers active)
(defdata player (list hand hand)) ;;represents a player's two hands
(defdata game-state (list player player)) ;;The first player represents the player whose turn it is currently.
(defdata side (oneof 'left 'right)) ;;represents one of a player's hands, either the first in the list (left)
;;or the second (right)
(defdata tap (list side side)) ;;represents a tap move, where the player taps, with their hand on the
;;first side, the opponent's hand (on the second side).
(defdata transfer (list nat side)) ;;represents a transfer move, where the player transfers the given
;;number of fingers from their hand on the given side to their other hand.
(defdata move (oneof tap transfer)) ;;represents a player's move, either a tap or a transfer.

;;takes in a game-state and determines whether it can exist (meaning, there are
;;less than 5 fingers per hand).
(definec game-state-ic (s :game-state) :boolean
  (and (> 5 (caar s)) (> 5 (cadar s)) (> 5 (caadr s)) (> 5 (cadadr s))))

(check= (game-state-ic (list (list 0 0) (list 0 0))) t)
(check= (game-state-ic (list (list 1 0) (list 0 0))) t)
(check= (game-state-ic (list (list 0 1) (list 0 0))) t)
(check= (game-state-ic (list (list 0 0) (list 1 0))) t)
(check= (game-state-ic (list (list 0 0) (list 0 1))) t)
(check= (game-state-ic (list (list 1 1) (list 1 1))) t)
(check= (game-state-ic (list (list 4 4) (list 4 4))) t)
(check= (game-state-ic (list (list 5 5) (list 5 5))) nil)
(check= (game-state-ic (list (list 5 0) (list 0 0))) nil)
(check= (game-state-ic (list (list 27 1) (list 3 4))) nil)
(check= (game-state-ic (list (list 13 92) (list 1000 69))) nil)

;;takes in a game-state and a tap and checks whether this tap is valid
(definec tap-ic (s :game-state tap :tap) :boolean
  :ic (game-state-ic s)
  (cond ((and (equal 'left (car tap))
              (equal 'left (cadr tap))) (and (not (zp (caar s)))
                                             (not (zp (caadr s)))))
        ((and (equal 'left (car tap))
              (equal 'right (cadr tap))) (and (not (zp (caar s)))
                                              (not (zp (cadadr s)))))
        ((and (equal 'right (car tap))
              (equal 'left (cadr tap))) (and (not (zp (cadar s)))
                                             (not (zp (caadr s)))))
        ((and (equal 'right (car tap))
              (equal 'right (cadr tap))) (and (not (zp (cadar s)))
                                              (not (zp (cadadr s)))))))

(check= (tap-ic (list (list 0 0) (list 0 0)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 1) (list 0 0)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 0) (list 0 1)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 1) (list 0 1)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 1 1) (list 0 1)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 1) (list 1 1)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 1 0) (list 1 0)) (list 'left 'left)) t)
(check= (tap-ic (list (list 0 0) (list 0 0)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 4) (list 0 0)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 0) (list 0 4)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 4) (list 0 4)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 4 3) (list 0 2)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 0 2) (list 4 3)) (list 'left 'left)) nil)
(check= (tap-ic (list (list 4 0) (list 4 0)) (list 'left 'left)) t)
(check= (tap-ic (list (list 3 0) (list 2 0)) (list 'left 'left)) t)
(check= (tap-ic (list (list 2 0) (list 2 0)) (list 'left 'left)) t)
(check= (tap-ic (list (list 1 4) (list 3 4)) (list 'left 'left)) t)
(check= (tap-ic (list (list 2 3) (list 2 2)) (list 'left 'left)) t)

;;takes in a game-state and a transfer and checks whether this transfer is valid
(definec transfer-ic (s :game-state tran :transfer) :boolean
  :ic (game-state-ic s)
  (cond ((equal 'left (cadr tran))
         (and (> (car tran) 0)
              (<= (car tran) (caar s))
              (< (+ (car tran) (cadar s)) 5)
              (not (and (= (+ (car tran) (cadar s)) (caar s))
                        (= (- (caar s) (car tran)) (cadar s))))))
        ((equal 'right (cadr tran))
         (and (> (car tran) 0)
              (<= (car tran) (cadar s))
              (< (+ (car tran) (caar s)) 5)
              (not (and (= (+ (car tran) (caar s)) (cadar s))
                        (= (- (cadar s) (car tran)) (caar s))))))))

(check= (transfer-ic (list (list 0 0) (list 0 0)) (list 0 'left)) nil)
(check= (transfer-ic (list (list 0 0) (list 0 0)) (list 0 'right)) nil)
(check= (transfer-ic (list (list 1 0) (list 0 0)) (list 0 'right)) nil)
(check= (transfer-ic (list (list 0 1) (list 0 0)) (list 0 'right)) nil)
(check= (transfer-ic (list (list 0 1) (list 0 0)) (list 1 'right)) nil)
(check= (transfer-ic (list (list 1 0) (list 0 0)) (list 1 'left)) nil)
(check= (transfer-ic (list (list 3 2) (list 0 0)) (list 3 'left)) nil)
(check= (transfer-ic (list (list 3 2) (list 0 0)) (list 2 'right)) nil)
(check= (transfer-ic (list (list 1 2) (list 0 0)) (list 3 'right)) nil)
(check= (transfer-ic (list (list 2 2) (list 0 0)) (list 2 'left)) t)
(check= (transfer-ic (list (list 2 2) (list 0 0)) (list 2 'right)) t)
(check= (transfer-ic (list (list 2 0) (list 0 0)) (list 3 'left)) nil)
(check= (transfer-ic (list (list 2 0) (list 0 0)) (list 1 'left)) t)

;;takes in a game-state and a move and checks whether this game move is valid
(definec chopsticks-ic (s :game-state m :move) :boolean
  :ic (game-state-ic s)
  (cond ((tapp m) (tap-ic s m))
        ((transferp m) (transfer-ic s m))))

(check= (chopsticks-ic (list (list 0 0) (list 0 0)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 1) (list 0 0)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 0) (list 0 1)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 1) (list 0 1)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 1 1) (list 0 1)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 1) (list 1 1)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 1 0) (list 1 0)) (list 'left 'left)) t)
(check= (chopsticks-ic (list (list 0 0) (list 0 0)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 4) (list 0 0)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 0) (list 0 4)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 4) (list 0 4)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 4 3) (list 0 2)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 0 2) (list 4 3)) (list 'left 'left)) nil)
(check= (chopsticks-ic (list (list 4 0) (list 4 0)) (list 'left 'left)) t)
(check= (chopsticks-ic (list (list 3 0) (list 2 0)) (list 'left 'left)) t)
(check= (chopsticks-ic (list (list 2 0) (list 2 0)) (list 'left 'left)) t)
(check= (chopsticks-ic (list (list 1 4) (list 3 4)) (list 'left 'left)) t)
(check= (chopsticks-ic (list (list 2 3) (list 2 2)) (list 'left 'left)) t)
(check= (chopsticks-ic (list (list 0 0) (list 0 0)) (list 0 'left)) nil)
(check= (chopsticks-ic (list (list 0 0) (list 0 0)) (list 0 'right)) nil)
(check= (chopsticks-ic (list (list 1 0) (list 0 0)) (list 0 'right)) nil)
(check= (chopsticks-ic (list (list 0 1) (list 0 0)) (list 0 'right)) nil)
(check= (chopsticks-ic (list (list 0 1) (list 0 0)) (list 1 'right)) nil)
(check= (chopsticks-ic (list (list 1 0) (list 0 0)) (list 1 'left)) nil)
(check= (chopsticks-ic (list (list 3 2) (list 0 0)) (list 3 'left)) nil)
(check= (chopsticks-ic (list (list 3 2) (list 0 0)) (list 2 'right)) nil)
(check= (chopsticks-ic (list (list 1 2) (list 0 0)) (list 3 'right)) nil)
(check= (chopsticks-ic (list (list 2 2) (list 0 0)) (list 2 'left)) t)
(check= (chopsticks-ic (list (list 2 2) (list 0 0)) (list 2 'right)) t)
(check= (chopsticks-ic (list (list 2 0) (list 0 0)) (list 3 'left)) nil)
(check= (chopsticks-ic (list (list 2 0) (list 0 0)) (list 1 'left)) t)

;;takes in two hands, where the first is added to the second, and outputs the resulting second hand.
(definec tap-help (h1 :hand h2 :hand) :hand
  :ic (and (< 0 h2) (< 0 h1) (< h1 5) (< h2 5))
  (if (> (+ h1 h2) 4)
      0
      (+ h1 h2)))

(check= (tap-help 3 1) 4)
(check= (tap-help 2 4) 0)
(check= (tap-help 1 2) 3)



;;takes in a game-state and a tap and outputs the resulting game-state.
(definec tap (s :game-state tap :tap) :game-state
  :ic (and (game-state-ic s) (tap-ic s tap))
  (cond ((and (equal 'left (car tap))
              (equal 'left (cadr tap))) (list (car s)
                                              (list (tap-help (caar s)
                                                              (caadr s))
                                                    (cadadr s))))
        ((and (equal 'left (car tap))
              (equal 'right (cadr tap))) (list (car s)
                                               (list (caadr s)
                                                     (tap-help (caar s)
                                                               (cadadr s)))))
        ((and (equal 'right (car tap))
              (equal 'left (cadr tap))) (list (car s)
                                              (list (tap-help (cadar s)
                                                              (caadr s))
                                                    (cadadr s))))
        ((and (equal 'right (car tap))
              (equal 'right (cadr tap))) (list (car s)
                                               (list (caadr s)
                                                     (tap-help (cadar s)
                                                               (cadadr s)))))))

(check= (tap (list (list 1 0) (list 1 0)) (list 'left 'left)) (list (list 1 0) (list 2 0)))
(check= (tap (list (list 4 0) (list 4 0)) (list 'left 'left)) (list (list 4 0) (list 0 0)))
(check= (tap (list (list 3 0) (list 2 0)) (list 'left 'left)) (list (list 3 0) (list 0 0)))
(check= (tap (list (list 2 0) (list 2 0)) (list 'left 'left)) (list (list 2 0) (list 4 0)))
(check= (tap (list (list 1 4) (list 3 4)) (list 'left 'left)) (list (list 1 4) (list 4 4)))
(check= (tap (list (list 2 3) (list 2 2)) (list 'left 'left)) (list (list 2 3) (list 4 2)))

;;takes in a game-state and a transfer and outputs the resulting game-state.
(definec transfer (s :game-state tran :transfer) :game-state
  :ic (and (game-state-ic s) (transfer-ic s tran))
  (cond ((equal 'left (cadr tran)) (list (list (- (caar s) (car tran))
                                               (+ (cadar s) (car tran)))
                                         (cadr s)))
        ((equal 'right (cadr tran)) (list (list (+ (caar s) (car tran))
                                                (- (cadar s) (car tran)))
                                          (cadr s)))))
                                          
(check= (transfer '((1 3) (2 3)) '(1 right)) '((2 2) (2 3)))
(check= (transfer '((3 2) (2 3)) '(2 left)) '((1 4) (2 3)))

;;takes in a game-state and a move and outputs the resulting game-state, with the player order
;;reversed to indicate that the player whose turn it is has switched.
(definec chopsticks (s :game-state m :move) :game-state
  :ic (and (game-state-ic s) (chopsticks-ic s m))
  (cond ((tapp m) (rev (tap s m)))
        ((transferp m) (rev (transfer s m)))))

(check= (chopsticks '((1 4) (3 2)) '(left right)) '((3 3) (1 4)))
(check= (chopsticks '((1 4) (3 2)) '(right left)) '((0 2) (1 4)))
(check= (chopsticks '((2 2) (3 2)) '(1 left)) '((3 2) (1 3)))
(check= (chopsticks '((1 4) (3 2)) '(2 right)) '((3 2) (3 2)))



;;determines if player one has won given game state
(definec win? (s :game-state) :bool
  :ic (game-state-ic s)
  (and (equal (caadr s) 0) (equal (cadadr s) 0)))


(check= (win? '((1 4) (3 2))) nil)
(check= (win? '((0 0) (3 2))) nil)
(check= (win? '((1 4) (0 0))) t)

;;determines if player one has lost given game state
(definec lose? (s :game-state) :bool
  :ic (game-state-ic s)
  (and (equal (caar s) 0) (equal (cadar s) 0)))

(check= (lose? '((1 4) (3 2))) nil)
(check= (lose? '((0 0) (3 2))) t)
(check= (lose? '((1 4) (0 0))) nil)
        

;;tie together three functions so that it chopsticks function repeats till
;;win or lose is true


(definec same-state (s1 :game-state s2 :game-state) :boolean
  (and (= (caar s1) (caar s2))
       (= (cadar s1) (cadar s2))
       (= (caadr s1) (caadr s2))
       (= (cadadr s1) (cadadr s2)))
)

(check= (same-state '((0 0) (0 0)) '((0 0) (0 0))) t)
(check= (same-state '((2 0) (0 0)) '((0 0) (0 0))) nil)
(check= (same-state '((0 5) (0 0)) '((0 0) (0 0))) nil)
(check= (same-state '((0 0) (5 0)) '((0 0) (0 0))) nil)
(check= (same-state '((0 0) (0 5)) '((0 0) (0 0))) nil)
(check= (same-state '((1 2) (3 4)) '((1 2) (3 4))) t)


;;functino step specific for testing just one move
(definec chopsticks-no-rev (s :game-state m :move) :game-state
  :ic (and (game-state-ic s) (chopsticks-ic s m))
  (cond ((tapp m) (tap s m))
        ((transferp m) (transfer s m))))

(check= (chopsticks-no-rev '((1 4) (3 2)) '(left right)) '((1 4) (3 3)))
(check= (chopsticks-no-rev '((1 4) (3 2)) '(right left)) '((1 4) (0 2)))
(check= (chopsticks-no-rev '((2 2) (3 2)) '(1 left)) '((1 3) (3 2)))
(check= (chopsticks-no-rev '((1 4) (3 2)) '(2 right)) '((3 2) (3 2)))

#|;;specific for when n = 1
(definec chopsticks-solver-one (s :game-state e :game-state) :boolean
  :ic (and (game-state-ic s) (game-state-ic e))
  :timeout 1000
  (cond ((same-state s e) t)
        ((or (win? s) (lose? s)) nil)
        ((and (tap-ic s '(left left)) (same-state (chopsticks-no-rev s '(left left)) e)) t)
        ((and (tap-ic s '(left right)) (same-state (chopsticks-no-rev s '(left right)) e)) t)
        ((and (tap-ic s '(right left)) (same-state (chopsticks-no-rev s '(right left)) e)) t)
        ((and (tap-ic s '(right right)) (same-state (chopsticks-no-rev s '(right right)) e)) t)
        ((and (transfer-ic s '(1 left)) (same-state (chopsticks-no-rev s '(1 left)) e)) t)
        ((and (transfer-ic s '(2 left)) (same-state (chopsticks-no-rev s '(2 left)) e)) t)
        ((and (transfer-ic s '(3 left)) (same-state (chopsticks-no-rev s '(3 left)) e)) t)
        ((and (transfer-ic s '(4 left)) (same-state (chopsticks-no-rev s '(4 left)) e)) t)
        ((and (transfer-ic s '(1 right)) (same-state (chopsticks-no-rev s '(1 right)) e)) t)
        ((and (transfer-ic s '(2 right)) (same-state (chopsticks-no-rev s '(2 right)) e)) t)
        ((and (transfer-ic s '(3 right)) (same-state (chopsticks-no-rev s '(3 right)) e)) t)
        ((and (transfer-ic s '(4 right)) (same-state (chopsticks-no-rev s '(4 right)) e)) t)
        (t nil)));;could replace this to implement recursion

(check= (chopsticks-solver-one '((1 1) (1 1)) '((1 1) (1 1))) t)
(check= (chopsticks-solver-one '((1 1) (1 1)) '((1 1) (1 3))) nil)
(check= (chopsticks-solver-one '((1 4) (1 1)) '((1 1) (1 4))) t)
|#

;;proving a win with starting game state and n number of moves
;;first method of proving satisfiability 
(definec chopsticks-win-solver (s :game-state n :int) :boolean
  :ic (and (game-state-ic s) (> 2 n) (<= 0 n))
  :timeout 10000
  (cond ((and (win? s)) t)
        ((or (zp n) (lose? s)) nil)
        (t (or (if (tap-ic s '(left left))
               (chopsticks-win-solver (chopsticks s '(left left)) (- n 1)) nil)
               (if (tap-ic s '(left right))
               (chopsticks-win-solver (chopsticks s '(left right)) (- n 1)) nil)
               (if (tap-ic s '(right left))
               (chopsticks-win-solver (chopsticks s '(right left)) (- n 1)) nil)
               (if (tap-ic s '(right right))
               (chopsticks-win-solver (chopsticks s '(right right)) (- n 1)) nil)
               (if (transfer-ic s '(1 left))
               (chopsticks-win-solver (chopsticks s '(1 left)) (- n 1)) nil)
               (if (transfer-ic s '(2 left))
               (chopsticks-win-solver (chopsticks s '(2 left)) (- n 1)) nil)
               (if (transfer-ic s '(3 left))
               (chopsticks-win-solver (chopsticks s '(3 left)) (- n 1)) nil)
               (if (transfer-ic s '(4 left))
               (chopsticks-win-solver (chopsticks s '(4 left)) (- n 1)) nil)
               (if (transfer-ic s '(1 right))
               (chopsticks-win-solver (chopsticks s '(1 right)) (- n 1)) nil)
               (if (transfer-ic s '(2 right))
               (chopsticks-win-solver (chopsticks s '(2 right)) (- n 1)) nil)
               (if (transfer-ic s '(3 right))
               (chopsticks-win-solver (chopsticks s '(3 right)) (- n 1)) nil)
               (if (transfer-ic s '(4 right))
               (chopsticks-win-solver (chopsticks s '(4 right)) (- n 1)) nil)))))


;;tests for zero moves
(check= (chopsticks-win-solver '((1 1) (1 1)) 0) nil)
(check= (chopsticks-win-solver '((1 2) (1 2)) 0) nil)
(check= (chopsticks-win-solver '((3 3) (2 2)) 0) nil)
(check= (chopsticks-win-solver '((2 2) (2 3)) 0) nil)
(check= (chopsticks-win-solver '((1 1) (1 3)) 0) nil)
(check= (chopsticks-win-solver '((4 4) (4 4)) 0) nil)
(check= (chopsticks-win-solver '((3 1) (3 1)) 0) nil)
(check= (chopsticks-win-solver '((1 2) (4 1)) 0) nil)

;;tests for zero moves but already in a winning state
(check= (chopsticks-win-solver '((0 0) (3 3)) 0) nil);;represents a loss
(check= (chopsticks-win-solver '((2 4) (0 0)) 0) t)


;;function to find ending game state that will guarentee a win:
;;can be used to determine end state for second method of proving satisfiability
(definec chopsticks-win-state (s :game-state n :int) :game-state
  :ic (and  (game-state-ic s) (> 1 n) (<= 0 n) (chopsticks-win-solver s n))
  :timeout 1000
  (cond ((and (win? s) (== (mod n 2) 0)) s)
        (t (or (if (tap-ic s '(left left))
               (chopsticks-win-state (chopsticks s '(left left)) (- n 1)) nil)
               (if (tap-ic s '(left right))
               (chopsticks-win-state (chopsticks s '(left right)) (- n 1)) nil)
               (if (tap-ic s '(right left))
               (chopsticks-win-state (chopsticks s '(right left)) (- n 1)) nil)
               (if (tap-ic s '(right right))
               (chopsticks-win-state (chopsticks s '(right right)) (- n 1)) nil)
               (if (transfer-ic s '(1 left))
               (chopsticks-win-state (chopsticks s '(1 left)) (- n 1)) nil)
               (if (transfer-ic s '(2 left))
               (chopsticks-win-state (chopsticks s '(2 left)) (- n 1)) nil)
               (if (transfer-ic s '(3 left))
               (chopsticks-win-state (chopsticks s '(3 left)) (- n 1)) nil)
               (if (transfer-ic s '(4 left))
               (chopsticks-win-state (chopsticks s '(4 left)) (- n 1)) nil)
               (if (transfer-ic s '(1 right))
               (chopsticks-win-state (chopsticks s '(1 right)) (- n 1)) nil)
               (if (transfer-ic s '(2 right))
               (chopsticks-win-state (chopsticks s '(2 right)) (- n 1)) nil)
               (if (transfer-ic s '(3 right))
               (chopsticks-win-state (chopsticks s '(3 right)) (- n 1)) nil)
               (if (transfer-ic s '(4 right))
               (chopsticks-win-state (chopsticks s '(4 right)) (- n 1)) nil)))))

(check= (chopsticks-win-state '((2 4) (0 0)) 0) '((2 4) (0 0)))


;;can compare this function by plugging in the ending game state generated by previous function
;;would input result of chopsticks-win-state to prove satisfiability of a win with n moves
(definec chopsticks-move-solver (s :game-state e :game-state n :int) :boolean
  :ic (and (game-state-ic s) (game-state-ic e) (> 1 n) (<= 0 n))
  :timeout 1000
  (cond ((same-state s e) t)
        ((or (win? s) (lose? s) (zp n)) nil)
        (t (or (if (tap-ic s '(left left))
               (chopsticks-move-solver (chopsticks s '(left left)) (rev e) (- n 1)) nil)
               (if (tap-ic s '(left right))
               (chopsticks-move-solver (chopsticks s '(left right)) (rev e) (- n 1)) nil)
               (if (tap-ic s '(right left))
               (chopsticks-move-solver (chopsticks s '(right left)) (rev e) (- n 1)) nil)
               (if (tap-ic s '(right right))
               (chopsticks-move-solver (chopsticks s '(right right)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(1 left))
               (chopsticks-move-solver (chopsticks s '(1 left)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(2 left))
               (chopsticks-move-solver (chopsticks s '(2 left)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(3 left))
               (chopsticks-move-solver (chopsticks s '(3 left)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(4 left))
               (chopsticks-move-solver (chopsticks s '(4 left)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(1 right))
               (chopsticks-move-solver (chopsticks s '(1 right)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(2 right))
               (chopsticks-move-solver (chopsticks s '(2 right)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(3 right))
               (chopsticks-move-solver (chopsticks s '(3 right)) (rev e) (- n 1)) nil)
               (if (transfer-ic s '(4 right))
               (chopsticks-move-solver (chopsticks s '(4 right)) (rev e) (- n 1)) nil)))))

(check= (chopsticks-move-solver '((1 2) (3 4))'((0 4) (1 2)) 0) nil)
(check= (chopsticks-move-solver '((1 2) (3 4))'((3 3) (4 2)) 0) nil)
;;(check= (chopsticks-move-solver '((1 2) (3 4))'((0 4) (1 2)) 1) t)
;;(check= (chopsticks-move-solver '((1 2) (3 4))'((0 4) (1 2)) 2) t)
;;(check= (chopsticks-move-solver '((1 1) (1 1)) '((1 2) (1 1)) 1) t)
;;(check= (chopsticks-move-solver '((1 1) (1 1)) '((1 2) (1 1)) 2) t)
;;(check= (chopsticks-move-solver '((1 1) (1 2)) '((1 1) (1 1)) 1) nil)
;;(check= (chopsticks-move-solver '((1 1) (1 1)) '((1 1) (1 1)) 1) t)


;;bad way of doing it: accumulate everything into a list of list
;;when first called input an empty list of list for l. each list in list should be of length n or less if ending game state is reached

(check= (chopsticks-win-solver '((1 1) '(0 0)) 0) 
        (chopsticks-move-solver '((0 0) (2 2)) (chopsticks-win-state '((1 1) (0 0)) 0) 0))
