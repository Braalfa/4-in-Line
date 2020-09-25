#lang racket
(require racket/gui)

;librerias que usaremos
(require (lib "graphics.ss" "graphics"))
(open-graphics)

;-----------------------------Pantalla Juego---------------------------------------------------------------------------------------------------
;Funcion que determina si un numero es par o no retornando un true o un false
(define (par? a)
  (cond
    [(not (and (number? a) (not (= a 0)))) #f]
    [ (even? a) #t]
    [else #f]))

;Se necesita una ventana para iniciar el juego la cual tiene el mensaje de inciar partida 
(define ventana1 (open-viewport "4LINE" 200 100))
((draw-solid-rectangle ventana1) (make-posn 0 0) 200 100 "black")
((draw-string ventana1) (make-posn 50 50) "Iniciar Partida" "white" )
;(define oculta (open-pixmap "oculta" 800 800))


;Funcion que se encarga de revisar dos parametros con los cuales se crea una matriz de cuadros que se dibuja en la ventana1, y esta ventana se reajusta al tanaño de la matriz de juego
(define (form-matrix m n)
  (set! ventana1 (open-viewport "4LINE" (* m 50) (* n 50)))
  (for ([i m])
    (for ([j n])
      (cond ((par? (+ i 1))
            ((draw-solid-rectangle ventana1) (make-posn (* i 50) (* j 50)) 50 50 "gray"))
            (else
             ((draw-solid-rectangle ventana1) (make-posn (* i 50) (* j 50)) 50 50 "white")
             ))
      ))
  ((draw-rectangle ventana1) (make-posn 0 0) (* m 50) (* n 50) "black")
  
  
  )



;Determina cual columna esta clickeando el usuario, recibe la coordenada x y el tamaño horizontal de la matriz y con comparaciones determina en cual columna esta
(define (cual_columna? x m)
  (cond ((= m 0) #f)
        ((> x (* (- m 1) 50) ) m)
        (else (cual_columna? x (- m 1))))
  )
  

;Esta funcion recibe las coordenadas x y y una variable t la cual determina el color de la ficha se que utilizara en el juego, y con estos aspectos dibuja en ventana1 la imagen que corresponde
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

;Con este comando se puede acceder a la funcion dibujarP desde la logica en otro archivo
(provide dibujarP)

;Funcion que recibe las coordenadas x y para dibujar la imagen de la ficha de la maquina en las posiciones indicadas
(define (dibujarC x y )
  (cond ((= x 0) (((draw-pixmap-posn "Racket/com.png" ) ventana1)(make-posn x y )))
        ((par? (/ x 50)) (((draw-pixmap-posn "Racket/com.png" ) ventana1)(make-posn x y )))
        (else (((draw-pixmap-posn "Racket/com-2.png" ) ventana1)(make-posn x y )))))

;Con este comando se puede acceder a la funcion dibujarC desde la logica en otro archivo
(provide dibujarC)



;-----------------------------Mouse------------------------------------------------------------------------------------------------------------
;Funcion que obtiene el click del mouse cuando este en ventana1
(define click (get-mouse-click ventana1))

;Funcion que recibe el click del mouse el horizontal de la matriz y el color de la ficha del jugador y envia los datos a la funcion cual_columa?
(define (poss mouse m color)
  (cual_columna? (posn-x (mouse-click-posn mouse)) m)
)

;Funcion que recibe el horizontal de la matriz y el color de la ficha del jugador a usar, le da un nuevo valor a las coordeadas del click cuando se clickee en la ventana1 y envia los parametros a la funcion poss
(define (juego m color)
  (set! click (get-mouse-click ventana1))
  (poss click m color))


;Con este comando se puede acceder a la funcion juego desde la logica en otro archivo
(provide juego)


;-----------------------------Pantalla Menu----------------------------------------------------------------------------------------------------
;Define un frame para el menu del juego
(define frame (new frame% [label "Menú de Juego 4Line"]
                   [width 500]	 
                   [height 500]))

;Inserta un label en el frame para indicar el nombre de la ventana
(define msg (new message% [parent frame]
                 [label "Menú de Juego 4Line"]))


;Crea un panel para agrupar los elementos que recibe
(define panel (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(left top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))




;Menu de seleccion para identificar cual color desea el usuario usar 
(define color (new radio-box%
                   [parent panel]
                   [label "Color de ficha      "]
                   [choices (list "Azul" "Verde" "Negro")]))


;Crea un panel para agrupar los elementos que recibe
(define panel4 (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(left top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))


;Inserta un label en el frame para indicar las indicaciones del juego 
(define indicaciones (new message% [parent panel4]
                 [label "Indicaciones:      Si un jugador coloca cuatro o más fichas en una línea "]))
(define indicaciones1 (new message% [parent panel4]
                 [label "                             continua vertical, horizontal o diagonalmente."]))
(define indicaciones2 (new message% [parent panel4]
                 [label "                             Este jugador gana la partida."]))


;Crea un panel para agrupar los elementos que recibe
(define panel2 (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))


;Inserta un label en el frame para indicar las indicaciones de lo que debe ingresar en la parte inferior a este mensaje 
(define msg1 (new message% [parent panel2]
                 [label "Matrix del Juego el mínimo del tablero es 8 X 8 y el máximo de 16 X 16    "]))
(define msg2 (new message% [parent panel2]
                 [label "(M X N) "]))

;Recibe un numero el cual sera el valor del tamaño horixontal
(define lista_matrix1 (new text-field%	 
   	 	[label "M     "]	 
   	 	[parent panel2]	 
                [init-value "8"]
                [min-width 0]	 
   	 	[min-height 0]
                ))

;Recibe un numero el cual sera el valor del tamaño vertical
(define lista_matrix2 (new text-field%	 
   	 	[label "N      "]	 
   	 	[parent panel2]	 
                [init-value "8"]
                [min-width 0]	 
   	 	[min-height 0]
                ))

;Crea un panel para agrupar los elementos que recibe
(define panel3 (new vertical-pane%
                   [parent frame]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

;Define variables con datos que se establecen en el menu de juego m=tamaño horizontal ,n=tamaño vertical, c= color
(define m -1)
(define n -1)
(define c 0)


;Funcion que retorna la variable del color
(define (colorJ)
  c)

;Con este comando se puede acceder a la funcion coloJ desde la logica en otro archivo
(provide colorJ)

;Funcion que hace recursion hasta que las variables locales sean cambiadas en el menu de juego
(define (tamano)
  (sleep 0.5)
  (cond((equal? m -1) (tamano))
       (else (list m n))))

;Con este comando se puede acceder a la funcion tamano desde la logica en otro archivo
(provide tamano)

;Crea un boton iniciar para como su nombre lo dice iniciar el juego este verifica que lo ingresado en los espacios para la matriz este entre los rangos prestablecidos, cambia las variables globales
;Envia el tamaño de la matriz a la funcion form-matrix para crear la matriz del juego en la pantalla e inicia la funcion juego con los parametros del tamaño horixontal y el color , tambien muestra la ventana1 donde esta el juego
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

;Funcion valora si un numero esta entre el rango de mayor a 7 y menor a 17, retorna true y se cumple y false si no se cumple
(define (entre_rango num)
  (cond ((integer? (string->number num))
         (cond ((and (< (string->number num) 17) (> (string->number num) 7)) #t)
               (else #f)))
        (else #f)))

;Define un frame para el mensaje de Ganador
(define p11(new frame% [label "Ganador"]
                   [width 100]	 
                   [height 100]
                   ))

;Crea un panel para agrupar los elementos que recibe
(define p12 (new vertical-pane%
                   [parent p11]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

;Inserta un label en el frame para indicar que se ha ganado la partida
(define msg13 (new message% [parent p12]
                  [label "Has ganado la partida"]))

;Define un frame para el mensaje de Perdedor
(define p2(new frame% [label "Perdedor"]
                   [width 100]	 
                   [height 100]
                   ))

;Crea un panel para agrupar los elementos que recibe
(define p21 (new vertical-pane%
                   [parent p2]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

;Inserta un label en el frame para indicar que se perdio la partida
(define msg21 (new message% [parent p21]
                  [label "Has perdido la partida"]))


;Define un frame para el mensaje de Empate   
(define p3(new frame% [label "Empate"]
                   [width 100]	 
                   [height 100]
                   ))

;Crea un panel para agrupar los elementos que recibe
(define p31 (new vertical-pane%
                   [parent p3]
                   [vert-margin 20]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))


;Inserta un label en el frame para indicar que se empato la partida
(define msg31 (new message% [parent p31]
                  [label "Has empatado la partida"]))

;Con este comando se puede acceder a la funcion p11 desde la logica en otro archivo
(provide p11)
;Con este comando se puede acceder a la funcion p2 desde la logica en otro archivo
(provide p2)
;Con este comando se puede acceder a la funcion p3 desde la logica en otro archivo
(provide p3)

;Al iniciar la aplicion muestra el frame del menu de juego
(send frame show #t)   