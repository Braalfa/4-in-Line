#lang racket
(require racket/gui)

;librerias que usaremos
(require (lib "graphics.ss" "graphics"))
(open-graphics)

;-----------------------------Pantalla Juego---------------------------------------------------------------------------------------------------
(define (par? a)
  (cond
    [(not (and (number? a) (not (= a 0)))) #f]
    [ (even? a) #t]
    [else #f]))

(define ventana1 (open-viewport "4LINE" 100 100))
;(define oculta (open-pixmap "oculta" 800 800))



(define (form-matrix m n)
  (set! ventana1 (open-viewport "4LINE" (* m 50) (* n 50)))
  (for ([i m])
    (for ([j n])
      ;(printf "i=~a\n" i)
      ;(printf "i=~a\n" j)
      (cond ((par? (+ i 1))
            ((draw-solid-rectangle ventana1) (make-posn (* i 50) (* j 50)) 50 50 "gray"))
            (else
             ((draw-solid-rectangle ventana1) (make-posn (* i 50) (* j 50)) 50 50 "white")
             ))
      ))
  ((draw-rectangle ventana1) (make-posn 0 0) (* m 50) (* n 50) "black")
  ;(copy-viewport oculta ventana1)
  
  )




(define (cual_columna? x m)
  (cond ((= m 0) #f)
        ((> x (* (- m 1) 50) ) m)
        (else (cual_columna? x (- m 1))))
  )
  

  
(define (dibujarP x y t)
  (cond((= t 0)
        (cond ((= x 0) (((draw-pixmap-posn "Racket/azul.png" ) ventana1)(make-posn x y )))
        ((par? (/ x 50)) (((draw-pixmap-posn "Racket/azul.png" ) ventana1)(make-posn x y )))
        (else (((draw-pixmap-posn "Racket/azul2.png" ) ventana1)(make-posn x y )))))
       ((= t 1)
        (cond ((= x 0) (((draw-pixmap-posn "Racket/verde.png" ) ventana1)(make-posn x y )))
        ((par? (/ x 50)) (((draw-pixmap-posn "Racket/verde.png" ) ventana1)(make-posn x y )))
        (else (((draw-pixmap-posn "Racket/verde2.png" ) ventana1)(make-posn x y )))))
       (else
        (cond ((= x 0) (((draw-pixmap-posn "Racket/negro.png" ) ventana1)(make-posn x y )))
        ((par? (/ x 50)) (((draw-pixmap-posn "Racket/negro.png" ) ventana1)(make-posn x y )))
        (else (((draw-pixmap-posn "Racket/negro2.png" ) ventana1)(make-posn x y )))))))
  
(provide dibujarP)

(define (dibujarC x y )
  (cond ((= x 0) (((draw-pixmap-posn "Racket/com.png" ) ventana1)(make-posn x y )))
        ((par? (/ x 50)) (((draw-pixmap-posn "Racket/com.png" ) ventana1)(make-posn x y )))
        (else (((draw-pixmap-posn "Racket/com-2.png" ) ventana1)(make-posn x y )))))

(provide dibujarC)

;(copy-viewport oculta ventana1)

;-----------------------------Mouse------------------------------------------------------------------------------------------------------------
(define click (get-mouse-click ventana1))

(define (poss mouse m color)
  (printf "posx=~a\n" (posn-x (mouse-click-posn mouse)))
  (printf "posy=~a\n" (posn-y (mouse-click-posn mouse)))
  (cual_columna? (posn-x (mouse-click-posn mouse)) m)
)


(define (juego m color)
  (set! click (get-mouse-click ventana1))
  (poss click m color))

(provide juego)

;(juego)
;-----------------------------Pantalla Menu----------------------------------------------------------------------------------------------------

(define frame (new frame% [label "Menú de Juego 4Line"]
                   [width 500]	 
                   [height 500]))


(define msg (new message% [parent frame]
                 [label "Menú de Juego 4Line"]))

(define panel (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(left top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))





(define color (new radio-box%
                   [parent panel]
                   [label "Color de ficha      "]
                   [choices (list "Azul" "Verde" "Negro")]))

(define panel4 (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(left top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))



(define indicaciones (new message% [parent panel4]
                 [label "Indicaciones:      Si un jugador coloca cuatro o más fichas en una línea "]))
(define indicaciones1 (new message% [parent panel4]
                 [label "                             continua vertical, horizontal o diagonalmente."]))
(define indicaciones2 (new message% [parent panel4]
                 [label "                             Este jugador gana la partida."]))

;(define lista_matrix (new choice%	 
 ;  	 	[label "Matrix del Juego      "]	 
  ; 	 	[choices (list "8x8" "8x16" "16x16")]	 
   ;	 	[parent panel2]	 ))

(define panel2 (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define msg1 (new message% [parent panel2]
                 [label "Matrix del Juego el mínimo del tablero es 8 X 8 y el máximo de 16 X 16    "]))
(define msg2 (new message% [parent panel2]
                 [label "(M X N) "]))
(define lista_matrix1 (new text-field%	 
   	 	[label "M     "]	 
   	 	[parent panel2]	 
                [init-value "8"]
                [min-width 0]	 
   	 	[min-height 0]
                ))
(define lista_matrix2 (new text-field%	 
   	 	[label "N      "]	 
   	 	[parent panel2]	 
                [init-value "8"]
                [min-width 0]	 
   	 	[min-height 0]
                ))

(define panel3 (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))


(define m -1)
(define n -1)
(define c 0)

(define (colorJ)
  c)
(provide colorJ)

(define (tamano)
  (sleep 0.5)
  (cond((equal? m -1) (tamano))
       (else (list m n))))
(provide tamano)

(define iniciar (new button%
                   [parent panel3]
                   [label "Iniciar Juego"]
                   [callback (lambda (button event)
                               (cond ((and (entre_rango (send lista_matrix1 get-value)) (entre_rango (send lista_matrix2 get-value)))
                                      (printf "color=~a\n" (send color get-selection))
                                      (printf "M=~a\n"   (send lista_matrix1 get-value))
                                      (printf "N=~a\n" (send lista_matrix2 get-value))
                                      (set! c (send color get-selection))
                                      (set! m (string->number (send lista_matrix1 get-value)))
                                      (set! n  (string->number (send lista_matrix2 get-value)))
                                      (form-matrix (string->number (send lista_matrix1 get-value)) (string->number (send lista_matrix2 get-value)))
                                      (send frame show #f)
                                      (juego (string->number (send lista_matrix1 get-value)) (send color get-selection))
                                      )
                                      
                                     (else (printf "El número digitado no esta en el rango prestablecido")))
                               )]))


(define (entre_rango num)
  (cond ((integer? (string->number num))
         (cond ((and (< (string->number num) 17) (> (string->number num) 7)) #t)
               (else #f)))
        (else #f)))


(define p11(new frame% [label "Ganador"]
                   [width 100]	 
                   [height 100]
                   ))
(define p12 (new vertical-pane%
                   [parent p11]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define msg13 (new message% [parent p12]
                  [label "Has ganado la partida"]))

(define p2(new frame% [label "Perdedor"]
                   [width 100]	 
                   [height 100]
                   ))
(define p21 (new vertical-pane%
                   [parent p2]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define msg21 (new message% [parent p21]
                  [label "Has perdido la partida"]))


   
(define p3(new frame% [label "Empate"]
                   [width 100]	 
                   [height 100]
                   ))
(define p31 (new vertical-pane%
                   [parent p3]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define msg31 (new message% [parent p31]
                  [label "Has empatado la partida"]))

(provide p11)
(provide p2)
(provide p3)

(send frame show #t)   