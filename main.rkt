;#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-

; Title :
; Final PF1 Project

; Authors :
; Lorenzo Spoleti
; Thomas Del Prete

;--------------------------------- Libraries -----------------------------------

(require 2htdp/image)
(require 2htdp/universe)

(require mrlib/gif)
(require rsound)


;####################=- TO DO LIST -=#########################
;
; - draw the world   ## DONE
;
; - movement : make the player move  ## DONE
; -- right ## DONE
; -- left ## DONE
; -- jump ## DONE
;
; - Physics :
; -- Gravity for player and enemy ## DONE
; -- Collisions of player with walls,enemies
; -- Collision of enemy with walls and projectiles
;
; - Destroy out of screen
;
; - Random Spawn the enemies from top of the screen
; - Increasingly difficult
;
; - Enemy simple AI
;
; - Weapons :
; -- pistol
; -- double pistol
; -- pump shotgun
; -- machinegun
;
; - Score
;
; - Screens :
; -- Main menu
; --- Select Player
; --- How to Play
; -- Game over
;
; - Change game difficulty
;

; Optional :
; - LANCIAFIAMMEEEEE !!
;
; Very optional :
; - Multiplayer
;----------------------------------- Main --------------------------------------


;======================= Definitions ==========================-----------------

;============= Structs ==================---------------------------------------

; A world is a :
; (make-world (List<player> list<Enemy> list<Projectile> list<Blocks> number))
; Interpretation : the player , the list of enemies, a list of the porjectiles,
; the layout of the current level and the score.
(define-struct world [player enemies projectiles blocks score])


; A player is a :
; (make-player (number number number number number image))
; Interpretation : the coordinates  , the values of the velocity , the life count
; and the current sprite.

(define-struct player [x y dx dy hp sprite])


; An enemy is a :
; (make-enemy (number number number number number image))
; Interpretation : the coordinates  , the values of the velocity , the life count
; and the current sprite.
; The same as a player but not controlled by the user.

(define-struct enemy [x y dx dy hp sprite])

; An projectile is a :
; (make-projectile (number number number number image))
; Interpretation : the coordinates  , the values of the velocity, the sprite.

(define-struct projectile [x y dx dy sprite])

; An Block is a :
; (make-block (number number image))
; Interpretation : the blocks that compose the layout of the level.

(define-struct block [x y sprite])

;============= Variables ================---------------------------------------

;Scene :
(define WIDTH 512)
(define HEIGHT 288)

(define SCALE 2)

; Background of the game : Image
(define BACKGROUND (empty-scene WIDTH HEIGHT))


(define G 6)





(define JUMP -52)
(define SPEED 11)
(define POLLO .)
;(write-gif  "pollo.gif")

(define PLAYER .)

(define PROJECTILE (scale 2  .))

(define PROJECTILE-SPEED 25)


;########

;======================= Functions ============================-----------------


;============= Drawing ==================---------------------------------------

; draw-world : world -> world
; draws the current world

(define (draw-world world)
  (draw-list (list-to-draw world) BACKGROUND SCALE))


; list-to-draw : world -> list
; creates a list of all the elements that are needed to draw in the scene.

(define (list-to-draw world)
        (append (world-player world)
                (world-enemies world)
                (world-projectiles world)
                (world-blocks world)))


(check-expect (list-to-draw
               (make-world
                (list (make-player 40 40 0 0 10 .))
                (list
                 (make-enemy 40 40 0 0 10 .)
                 (make-enemy 90 90 0 0 10 .))
                (list (make-projectile 80 40 0 0 .))
                (list (make-block 0 300 .))
                0))
              (list
               (make-player 40 40 0 0 10 .)
               (make-enemy 40 40 0 0 10 .)
               (make-enemy 90 90 0 0 10 .)
               (make-projectile 80 40 0  0 .)
               (make-block 0 300 .)))


; draw-list : list empty-scene number -> scene
; draws all the elements of the list-to-draw on the scene

(define (draw-list list background Xsize)
  (cond [(empty? list) background]
        [(cons? list)
         (cond [(player? (first list))
                (scale Xsize (place-image/align (player-sprite (first list))
                         (player-x (first list))
                         (player-y (first list))
                         "left" "top"
                         (draw-list (rest list) background Xsize)))]
               [(projectile? (first list))
                (place-image/align (projectile-sprite (first list))
                         (projectile-x (first list))
                         (projectile-y (first list))
                         "left" "top"
                         (draw-list (rest list) background Xsize))]
               [(enemy? (first list))
                (place-image/align (enemy-sprite (first list))
                         (enemy-x (first list))
                         (enemy-y (first list))
                         "left" "top"
                         (draw-list (rest list) background Xsize))]
               [(block? (first list))
                (place-image/align (block-sprite (first list))
                         (block-x (first list))
                         (block-y (first list))
                         "left" "top"
                         (draw-list (rest list) background Xsize))])
         ]))



(check-expect (draw-list (list
               (make-player 40 40 0 0 10 .)
               (make-enemy 40 40 0 0 10 .)
               (make-enemy 90 90 0 0 10 .)
               (make-projectile 80 40 0 0 .)) BACKGROUND 0.2)
              .)


(define (purge-the-evil list)
  (filter inside-canvas? list))

(check-expect (purge-the-evil
               (list
                (make-enemy 400 216 0 18 10 .)
                (make-enemy 300 216 0 18 10 .)
                (make-enemy 200 228 0 18 10 .)
                (make-enemy 320 216 0 18 10 .)
                (make-projectile 1250 40 30 0 .)
                (make-projectile 860 40 20 0 .)
                (make-projectile 470 40 10 0 .)))
              (list
  (make-enemy 400 216 0 18 10 .)
  (make-enemy 300 216 0 18 10 .)
  (make-enemy 200 228 0 18 10 .)
  (make-enemy 320 216 0 18 10 .)
  (make-projectile 470 40 10 0 .)))

(check-expect (purge-the-evil (list
                               (make-player 40 40 0 0 10 .)
                               (make-enemy 40 40 0 0 10 .)
                               (make-enemy 90 90 0 0 10 .)
                               (make-projectile 80 40 0  0 .)
                               (make-block 0 300 .)))
              (list
               (make-player 40 40 0 0 10 .)
               (make-enemy 40 40 0 0 10 .)
               (make-enemy 90 90 0 0 10 .)
               (make-projectile 80 40 0  0 .)
               (make-block 0 300 .)))



(define (inside-canvas? X)
  (cond [(player? X) (if (or (< (player-x X) 0)
                             (> (player-x X) WIDTH)
                             (< (player-y X) 0)
                             (> (player-y X) HEIGHT))
                         #false
                         #true)]
        [(enemy? X) (if (or (< (enemy-x X) 0)
                             (> (enemy-x X) WIDTH)
                             (< (enemy-y X) 0)
                             (> (enemy-y X) HEIGHT))
                         #false
                         #true)]
        [(projectile? X) (if (or (< (projectile-x X) 0)
                             (> (projectile-x X) WIDTH)
                             (< (projectile-y X) 0)
                             (> (projectile-y X) HEIGHT))
                         #false
                         #true)]
        [else #true]))

(check-expect (inside-canvas? (make-projectile 16921 154 25 0 .)) #false)
(check-expect (inside-canvas? (make-player 20 30 25 0 100 .)) #true)

;============= Movement ==================--------------------------------------


(define (world-handle-key world key)
  (cond [(key=? key "left")
       (make-world
        (add-player-velocity (first (world-player world)) (- SPEED) 0 (flip-horizontal PLAYER))
        (world-enemies world)
        (world-projectiles world)
        (world-blocks world)
        (world-score world))]

        [(key=? key "right")
       (make-world
        (add-player-velocity (first (world-player world)) SPEED 0 PLAYER)
        (world-enemies world)
        (world-projectiles world)
        (world-blocks world)
        (world-score world))]

        [(key=? key "up")
       (make-world
        (if (= (player-y (first (world-player world))) (- HEIGHT 40 (image-height (player-sprite (first (world-player world))))))
           (add-player-velocity (first (world-player world)) 0 JUMP (player-sprite (first (world-player world))))
           (world-player world)
            )
        (world-enemies world)
        (world-projectiles world)
        (world-blocks world)
        (world-score world))]

        [(key=? key "x")
       (make-world
        (world-player world)
        (world-enemies world)
        (if (flipped-player? (player-sprite (first (world-player world))))
        (add-projectile (first (world-player world)) (world-projectiles world) (- PROJECTILE-SPEED))
        (add-projectile (first (world-player world)) (world-projectiles world) PROJECTILE-SPEED))
        (world-blocks world)
        (world-score world))]

        [else world]))

(define (world-handle-key-release world key)
  (cond [(key=? key "left")
       (make-world
        (add-player-velocity (first (world-player world)) SPEED 0 (player-sprite (first (world-player world))))
        (world-enemies world)
        (world-projectiles world)
        (world-blocks world)
        (world-score world))]

        [(key=? key "right")
       (make-world
        (add-player-velocity (first (world-player world)) (- SPEED) 0 (player-sprite (first (world-player world))))
        (world-enemies world)
        (world-projectiles world)
        (world-blocks world)
        (world-score world))]
        [else world]))


(define (add-player-velocity player new-dx new-dy new-sprite)
  (list (make-player
   (player-x player)
   (player-y player)
   (if (or (> (+ (player-dx player) new-dx) (+ SPEED (/ SPEED 2)))
           (< (+ (player-dx player) new-dx) (- (+ SPEED (/ SPEED 2)))))
       (player-dx player)
       (+ (player-dx player) new-dx))
   (+ (player-dy player) new-dy)
   (player-hp player)
   new-sprite)))

(check-expect (add-player-velocity (make-player 40 40 0 0 10 .) 40 0 .)
              (list (make-player 40 40 0 0 10 .)))
(check-expect (add-player-velocity (make-player 40 40 0 0 10 .) 6 0 .)
              (list (make-player 40 40 6 0 10 .)))

(define (add-player-xy player new-x new-y)
  (list (make-player
   (+ (player-x player) new-x)
   (+ (player-y player) new-y)
   (player-dx player)
   (player-dy player)
   (player-hp player)
   (player-sprite player))))

(check-expect (add-player-xy (make-player 40 40 0 0 10 .) 20 0)
              (list (make-player 60 40 0 0 10 .)))

(define (flipped-player? image)
 (not (equal? image PLAYER)))

(check-expect (flipped-player? PLAYER) #f)
(check-expect (flipped-player? (flip-horizontal PLAYER)) #t)

;============= Weapons ==================---------------------------------------

(define (add-projectile player projectiles SPEED)
  (cons
   (make-projectile (+ (player-x player) (pinhole-x (center-pinhole (player-sprite player))))
                    (+ (player-y player) (pinhole-y (center-pinhole (player-sprite player))))
                    SPEED 0 PROJECTILE)
   projectiles))


;============= Spawning ==================--------------------------------------


;(define (enemy-AI world)
;  (map
;)

 (define (wall-turn X)
  (make-enemy
   (enemy-x X)
   (enemy-y X)
   (cond [(<= (enemy-x X) 20) (- (enemy-dx X))]
         [(>= (enemy-x X) (- WIDTH 20)) (- (enemy-dx X))]
         [else (enemy-dx X)])
   (enemy-dy X)
   (enemy-hp X)
   (enemy-sprite X)))


;============= Physics ===================--------------------------------------

;====== Gravity ==============--------------------------------------------------


;(define (world-update world)


(define (world-move world)
      (make-world (map add-movement (purge-the-evil (world-player world)))
              (map wall-turn (map add-movement (purge-the-evil (world-enemies world))))
              (map add-movement (purge-the-evil (world-projectiles world)))
              (world-blocks world)
              (world-score world)))


; ; list-to-drop : world -> list
; ; creates
; (define (list-to-move world)
;   (cons (world-player world)
;         (append (world-enemies world)
;                 (world-projectiles world))))
;
; (check-expect (list-to-move
;               (make-world
;                 (make-player 40 40 0 0 10 .)
;                 (list
;                  (make-enemy 40 40 0 0 10 .)
;                  (make-enemy 90 90 0 0 10 .))
;                 (list (make-projectile 80 40 0 0 .))
;                 '()
;                 0))
;               (list
;                (make-player 40 40 0 0 10 .)
;                (make-enemy 40 40 0 0 10 .)
;                (make-enemy 90 90 0 0 10 .)
;                (make-projectile 80 40 0 0 .)))



; world-from-list is one option


; add-gravity : struct -> struct
; add gravity to a player or enemy

(define (add-movement X)
  (cond [(player? X)
         (if (>= (+ (player-y X) (player-dy X)) (- HEIGHT 40 (image-height (player-sprite X))))
             (make-player (+ (player-x X) (player-dx X))
                          (- (+ (player-y X) (- (- HEIGHT 40) (player-y X))) (image-height (player-sprite X)))
                          (player-dx X)
                          (player-dy X)
                          (player-hp X)
                          (player-sprite X))
             (make-player (+ (player-x X) (player-dx X))
                          (+ (player-y X) (player-dy X))
                          (player-dx X)
                          (if (> (player-dy X) 20)
                              (player-dy X)
                              (+ (player-dy X) G))
                          (player-hp X)
                          (player-sprite X)))]
        [(enemy? X)
         (if (>= (+ (enemy-y X) (enemy-dy X)) (- HEIGHT 40 (image-height (enemy-sprite X))))
             (make-enemy (+ (enemy-x X) (enemy-dx X))
                         (- (+ (enemy-y X) (- (- HEIGHT 40) (enemy-y X))) (image-height (enemy-sprite X)))
                         (enemy-dx X)
                         (enemy-dy X)
                         (enemy-hp X)
                         (enemy-sprite X))
             (make-enemy (+ (enemy-x X) (enemy-dx X))
                         (+ (enemy-y X) (enemy-dy X))
                         (enemy-dx X)
                         (if (> (enemy-dy X) 15)
                              (enemy-dy X)
                              (+ (enemy-dy X) G))
                         (enemy-hp X)
                         (enemy-sprite X)))]
        [(projectile? X)
             (make-projectile (+ (projectile-x X) (projectile-dx X))
                         (+ (projectile-y X) (projectile-dy X))
                         (projectile-dx X)
                         (projectile-dy X)
                         (projectile-sprite X))]
        ))

(check-expect (add-movement (make-player 40 40 0 0 10 .))
              (make-player 40 40 0 6 10 .))



;====== Collisions ===========--------------------------------------------------

;(define (entity-hits-floor world)
;  (ormap (lambda (X)
;           (cond [(player? X) (= (player-y X) (- HEIGHT 40))]
;                 [(enemy? X)
;)))


;########

;----------------------------------- Big Bang ----------------------------------

(define (main _)
  (big-bang (make-world (list (make-player 40 50 0 0 10 PLAYER))
                        (list (make-enemy 400 0 10 0 10 POLLO)
                              (make-enemy 300  50 10 0 10 POLLO)
                              (make-enemy 200 10 0 0 10 (rectangle 20 20 255 "red"))
                              (make-enemy 320 0 0 0 10 POLLO))
                        (list (make-projectile 80 40 30 0 (rectangle 10 10 255 "gold"))
                              (make-projectile 80 40 20 0 (rectangle 10 10 255 "gold"))
                              (make-projectile 80 40 10 0 PROJECTILE))
                        (list (make-block 0 (- HEIGHT 40)  (rectangle WIDTH 40 100 "grey"))
                              (make-block 0 0  (rectangle 20 HEIGHT 100 "grey"))
                              (make-block (- WIDTH 20) 0 (rectangle 20 HEIGHT 100 "grey")))
                        0
                        )
            [to-draw draw-world]
            [on-tick world-move]
            [on-key world-handle-key]
            [on-release world-handle-key-release]
         ;   [stop-when world-is-full?]
            ))

(main 420)

;########


;(make-world (make-player 40 40 0 0 10 (rectangle 10 10 255 "red"))
;                        (list (make-enemy 40 40 0 0 10 (rectangle 10 10 255 "red")))
;                        (list (make-projectile 80 40 0 0 (rectangle 10 10 255 "red")))
;                        )


(overlay/offset (rectangle 6 6 255 "white") -8 8 (overlay/offset (rectangle 10 10 255 "white") 8 6 (rectangle 40 40 255 "black")))

(define (height enemy)
  (image-height (enemy-sprite enemy)))



;#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-
