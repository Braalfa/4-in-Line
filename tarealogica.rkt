#lang racket

(require "tareagrafica.rkt")

;------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion para crear las columnas

(define (crearColumnas mCont)
  (cond ((= 0 mCont) '())
       (else
        (cons 0 (crearColumnas (- mCont 1))))))

;Funcion para crear la matriz del tamano mxn llena de ceros
(define(crearMatriz mCont nCont)
  (cond((= 0 nCont) '())
       (else
        (append (cons (crearColumnas mCont) '()) (crearMatriz mCont (- nCont 1))))))

;-------------------------------------------------------------------------------------------------------------------------------------------------
;Insertar Ficha

; Encuentra el valor de X,Y en la matriz, esta recive la posX y posY y una Matriz  
;la funcion devuelve el valor del entero en esas possiciones

(define (encontrar x y matriz)
  (posicion x (posicion y matriz)))

;El recive una lista y una posicion esta funcion devuelve el valor en la posicion
(define (posicion i lista)
  (cond((null? lista) '())
       ((equal? 0 i) (car lista))
       (else (posicion (- i 1) (cdr lista)))))

;Recive una columna un valor que sera un contador y una matriz 
;el hace un contador sobre la matriz donde devulve la ultima fila donde existe un cero o una poscion vacia en esa columna 
(define(posY? nColum valor matriz)
  (cond((or (null? matriz) (> (encontrar nColum 0 matriz) 0))
      (- valor 1))
        (else
         (posY? nColum (+ valor 1) (cdr matriz)))))

;Funcion para insertar un valor en una lista  la cual recive una columna , el valor y la lista
;Este intercambia el valor que se encuntra en la lista en la poscion en  X e introduce el nuevo parametro
(define(valorLista posX valor lista)
  (cond((zero? posX)
        (cons valor (cdr lista)))
       (else
        (cons (car lista) (valorLista (- posX 1) valor (cdr lista))))))
;Funcion para insertar un valor en una matriz la cual recive una columna y una fila el valor y la matriz
;Este intercambia el valor que se encuntra en la matriz en las posciones X, Y e introduce el nuevo parametro
(define(valorMatriz posX posY valor matriz)
  (cond((zero? posY)
        (cons (valorLista posX valor (car matriz))(cdr matriz)))
        (else
         (cons (car matriz) (valorMatriz posX (- posY 1) valor (cdr matriz))))))

;Funcion para insertar un valor en una matriz la cual recive una columna el valor y la matriz
(define(insertarMatriz posX valor matriz)
  (valorMatriz posX (posY? posX 0 matriz) valor matriz))  


;-------------------------------------------------------------------------------------------------------------------------------------------------
;Comprobaciones

;Comprueba si la primera fila de la matriz esta completa para
;definir la matriz
(define(empate? lista)
  (cond ((null? lista)#t)
       ((equal? 0 (car lista)) #f)
       (else
        (empate? (cdr lista)))))

;Funcion utilizada para calcular el alto de la matriz
;retorna la cantida de filas que tiene la matriz
(define(alto? matriz)
  (cond((null? matriz)0)
       (else
        (+ 1 (alto?(cdr matriz))))))

;Funcion auxiliar para calacular el largo de la matriz
(define (largoAux lista)
  (cond((null? lista)0)
       (else
        (+ 1 (largoAux (cdr lista))))))

;Funcion utilizada para calcular el largo de la matriz
;retorna la cantida de columnas que tiene la matriz
(define (largo? matriz)
  (largoAux (car matriz)))

;Cuenta la cantidad de numeros horizontales iguales hay de manera consecutiva
;en el momento que se encuente un numero que no sea igual al anterior para el conteo
(define(contadorHorizontal lista valor)
  (cond((or(null? lista)(not (equal? (car lista) valor))) 0)
       (else
        (+ 1 (contadorHorizontal (cdr lista) valor)))))

;Hace un true o false segun los calculos de contadorHorizontal sobre toda la matriz
(define(victoriaHorizontalPlayer? lista)
  (cond((null? lista) #f)
       ((>= (contadorHorizontal lista 1) 4) #t)
       (else
         (victoriaHorizontalPlayer? (cdr lista)))))

;Hace un true o false segun los calculos de contadorHorizontal sobre toda la matriz
(define(victoriaHorizontalComputadora? lista)
  (cond((null? lista) #f)
       ((>= (contadorHorizontal lista 2) 4) #t)
       (else
         (victoriaHorizontalComputadora? (cdr lista)))))

;Revisa si el jugador gano de forma horizontal en caso de que no se cumpla devuelve falso
(define(victoriaHMPlayer? matriz)
  (cond((null? matriz)#f)
       ((victoriaHorizontalPlayer? (car matriz))#t)
       (else
        (victoriaHMPlayer? (cdr matriz)))))

;Revisa si el computador gano de forma horizontal en caso de que no se cumpla devuelve falso
(define(victoriaHMCompuatadora? matriz)
  (cond((null? matriz)#f)
       ((victoriaHorizontalComputadora? (car matriz))#t)
       (else
        (victoriaHMCompuatadora? (cdr matriz)))))

;Cuenta la cantidad de numeros verticales iguales hay de manera consecutiva
;en el momento que se encuente un numero que no sea igual al anterior para el conteo
(define(contadorVertical posX posY valor matriz)
  (cond((> posY (- (alto? matriz) 1) )0)
       ((not(equal?(encontrar posX posY matriz) valor))0)
       (else
            (+ 1 (contadorVertical posX (+ posY 1) valor matriz) ))))

;Hace un true o false segun los calculos de contadorVertical sobre toda la matriz
(define(victoriaVerticalPlayer? posX posY matriz)
  (cond((> posX (- (largo? matriz) 1))#f)
       ((>= (contadorVertical posX (+ posY 1) 1 matriz)4)#t)
       ((> posY (- (alto? matriz) 1))(victoriaVerticalPlayer? (+ posX 1) 0 matriz))
       (else
        (victoriaVerticalPlayer? posX (+ posY 1) matriz))))

;Hace un true o false segun los calculos de contadorVertical sobre toda la matriz
(define(victoriaVerticalCompatadora? posX posY matriz)
  (cond((> posX (- (largo? matriz) 1))#f)
       ((>= (contadorVertical posX (+ posY 1) 2 matriz)4)#t)
       ((> posY (- (alto? matriz) 1))(victoriaVerticalCompatadora? (+ posX 1) 0 matriz))
       (else
        (victoriaVerticalCompatadora? posX (+ posY 1) matriz))))

;Funcion para calacular largo de la diagonal parceando desde
;arriba hacia la derecha 
(define (contadorDiagonalAD posX posY valor matriz)
  (cond((or(> posY (- (alto? matriz) 1)) (> posX (- (largo? matriz) 1))) 0) 
       ((not(equal?(encontrar posX posY matriz) valor))0)
       (else
        (+ 1 (contadorDiagonalAD (+ posX 1) (+ posY 1) valor matriz)))))

;Define si el jugador gano de forma diagonal de arriba hacia la derecha
(define(victoriaDiagonalADPlayer posX posY matriz)
  (cond
    ((and(> posY (- (alto? matriz) 1)) (> posX (- (largo? matriz) 1)))#f)
       ((>= (contadorDiagonalAD posX posY 1 matriz) 4) #t)
       ((> posX (- (largo? matriz) 1))(victoriaDiagonalADPlayer 0 (+ 1 posY) matriz))
       (else
        (victoriaDiagonalADPlayer (+ posX 1) posY matriz))))

;Define si el computador gano de forma diagonal de arriba hacia la derecha
(define(victoriaDiagonalADComputadora posX posY matriz)
  (cond
    ((and(> posY (- (alto? matriz) 1)) (< posX (- (largo? matriz) 1)))#f)
       ((>= (contadorDiagonalAD posX posY 2 matriz) 4) #t)
       ((> posX (- (largo? matriz) 1))(victoriaDiagonalADComputadora 0 (+ 1 posY) matriz))
       (else
        (victoriaDiagonalADComputadora (+ posX 1) posY matriz))))

;Funcion para calacular largo de la diagonal parceando desde
;arriba hacia la izquierda
(define(contadorDiagonalAI posX posY valor matriz)
  (cond((or(> posY (- (alto? matriz) 1))(< posX 0))0)
       ((> posX (- (largo? matriz) 1))0)
       ((not(equal?(encontrar posX posY matriz) valor))0)
       (else
        (+ 1 (contadorDiagonalAI (- posX 1) (+ posY 1) valor matriz)))))

;Define si el jugador gano de forma diagonal de arriba hacia la izquierda
(define(victoriaDiagonalAIPlayer posX posY matriz)
  (cond((and(> posY (- (alto? matriz) 1)) (> posX (- (largo? matriz) 1)))#f)
       ((>= (contadorDiagonalAI posX posY 1 matriz) 4) #t)
       ((> posX (- (largo? matriz) 1)) (victoriaDiagonalAIPlayer 0 (+ 1 posY) matriz))
       (else
       (victoriaDiagonalAIPlayer (+ posX 1) posY matriz))))

;Define si el computador gano de forma diagonal de arriba hacia la izquierda
(define(victoriaDiagonalAIComputadora posX posY matriz)
  (cond((and(> posY (- (alto? matriz) 1)) (> posX (- (largo? matriz) 1)))#f)
       ((>= (contadorDiagonalAI posX posY 2 matriz) 4) #t)
       ((> posX (- (largo? matriz) 1)) (victoriaDiagonalAIComputadora 0 (+ 1 posY) matriz))
       (else
       (victoriaDiagonalAIComputadora (+ posX 1) posY matriz))))

;Revisa si el jugador gano
(define(hayGanadorPlayer? matriz)
  (cond((or(victoriaDiagonalAIPlayer 0 0 matriz)(victoriaDiagonalADPlayer 0 0 matriz)(victoriaVerticalPlayer? 0 0 matriz)(victoriaHMPlayer? matriz))#t)
       (else
        (equal? 1 2))))

;Revisa si la computadora gano
(define(hayGanadorComputadora? matriz)
  (cond((or(victoriaDiagonalAIComputadora 0 0 matriz) (victoriaDiagonalADComputadora 0 0 matriz) (victoriaVerticalCompatadora? 0 0 matriz)(victoriaHMCompuatadora? matriz))#t)
       (else
        (equal? 1 2))))


;---------------------------------------------------------------------------------------------------------------------------------------------
;b

;; Algoritmo Greedy
  

;;Esta funcion da una lista de las elementos en los indices dados por la lista pos, de la lista
(define (posiciones pos lista)
  (cond((null? pos) '())
       (else (cons (posicion (car pos) lista) (posiciones (cdr pos) lista)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten a la derecha del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarHorizontalDerecha i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar (+ x 1) y matriz) numjugador)) 0)
       (else (+ 1 (contarHorizontalDerecha (+ i 1) numjugador (+ x 1) y matriz)))))


;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten a la izquierda del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarHorizontalIzquierda i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar (- x 1) y matriz) numjugador)) 0)
       (else (+ 1 (contarHorizontalIzquierda (+ i 1) numjugador (- x 1) y matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten abajo del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarVerticalAbajo i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar x (+ y 1) matriz) numjugador)) 0)
       (else (+ 1 (contarVerticalAbajo (+ i 1) numjugador x (+ y 1) matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten arriba del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarVerticalArriba i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar x (- y 1) matriz) numjugador)) 0)
       (else (+ 1 (contarVerticalArriba (+ i 1) numjugador x (- y 1) matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten diagonalmente arriba a la derecha del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarDiagArrDer i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar (+ x 1) (- y 1) matriz) numjugador)) 0)
       (else (+ 1 (contarDiagArrDer (+ i 1) numjugador (+ x 1) (- y 1) matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten diagonalmente abajo a la izquierda del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarDiagAbaIzq i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar (- x 1) (+ y 1) matriz) numjugador)) 0)
       (else (+ 1 (contarDiagAbaIzq (+ i 1) numjugador (- x 1) (+ y 1) matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten diagonalmente arriba a la izquierda del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarDiagArrIzq i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar (- x 1) (- y 1) matriz) numjugador)) 0)
       (else (+ 1 (contarDiagArrIzq (+ i 1) numjugador (- x 1) (- y 1) matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten diagonalmente abajo a la derecha del punto (x,y) en la matriz.
;El indice inicial debe de ser 0
(define (contarDiagAbaDer i numjugador x y matriz)
  (cond((equal? 3 i) 0)
       ((not (miembro? (encontrar (+ x 1) (+ y 1) matriz) numjugador)) 0)
       (else (+ 1 (contarDiagAbaDer (+ i 1) numjugador (+ x 1) (+ y 1) matriz)))))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten horizontales del punto (x,y) en la matriz.
(define (puntosHorizontal numjugador x y matriz)
  (cond ((< 2 (+ (contarHorizontalDerecha 0 (list numjugador 0) x y matriz) (contarHorizontalIzquierda 0 (list numjugador 0) x y matriz)))
              (+ (contarHorizontalDerecha 0 (list numjugador) x y matriz) (contarHorizontalIzquierda 0 (list numjugador) x y matriz)))
        (else 0)))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten verticales del punto (x,y) en la matriz.            
(define (puntosVerticales numjugador x y matriz)
  (cond ((< 2 (+ (contarVerticalAbajo 0 (list numjugador) x y matriz) (contarVerticalArriba 0 (list 0) x y matriz)))
              (contarVerticalAbajo 0 (list numjugador) x y matriz))
        (else 0)))


;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten diagonales con derivada positiva del punto (x,y) en la matriz.
(define (puntosDiagonalesPos numjugador x y matriz)
  (cond ((< 2 (+ (contarDiagArrDer 0 (list numjugador 0) x y matriz) (contarDiagAbaIzq 0 (list numjugador 0) x y matriz)))
              (+ (contarDiagArrDer 0 (list numjugador) x y matriz) (contarDiagAbaIzq 0 (list numjugador) x y matriz)))
        (else 0)))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten diagonales con derivada negativa del punto (x,y) en la matriz.
(define (puntosDiagonalesNeg numjugador x y matriz)
  (cond ((< 2 (+ (contarDiagAbaDer 0 (list numjugador 0) x y matriz) (contarDiagArrIzq 0 (list numjugador 0) x y matriz)))
              (+ (contarDiagAbaDer 0 (list numjugador) x y matriz) (contarDiagArrIzq 0 (list numjugador) x y matriz)))
        (else 0)))

;;Esta funcion se encarga de contar los elementos de tipo numjugador que esten cercanos del punto (x,y) en la matriz,
;y los devuelve en una lista de acuerdo a su tipo
(define (listaPuntosTotal numjugador x y matriz)
  (cond((equal? y -1) '(-1 -1 -1 -1))
       (else
           (quicksort (list (puntosHorizontal numjugador x y matriz)
                            (puntosVerticales numjugador x y matriz)
                            (puntosDiagonalesPos numjugador x y matriz)
                            (puntosDiagonalesNeg numjugador x y matriz))))))

;;Esta funcion se encarga de generar una matriz con el resumen de las listas de puntos para cada columna
(define (generarMatrizDePuntos i numjugador posicionesY matriz)
  (cond ((null? posicionesY) '())
        (else (cons (listaPuntosTotal numjugador i (car posicionesY) matriz)
                    (generarMatrizDePuntos (+ i 1) numjugador (cdr posicionesY) matriz)))))

;;Esta funcion devuelve una lista de los primeros elementos de cada fila de una matriz
(define (obtenerPrimeros matriz)
  (cond ((null? matriz) '())
        (else (cons (caar matriz) (obtenerPrimeros (cdr matriz))))))

;;Esta funcion elimina los primeros elementos de cada fila de una matriz
(define (eliminarPrimeros matriz)
  (cond ((null? matriz) '())
        (else (cons (cdar matriz) (eliminarPrimeros (cdr matriz))))))

;;Esta funcion calcula la transpuesta de una matriz
(define (transpuesta matriz)
  (cond ((null? (car matriz)) '())
        (else (cons (obtenerPrimeros matriz) (transpuesta (eliminarPrimeros matriz))))))

;;Esta funcion busca el indice de un elemento (ele) en una lista
; El indice i debe ser 0
(define (buscarIndices i ele lista)
  (cond((null? lista) '())
       ((equal? ele (car lista)) (cons i (buscarIndices (+ i 1) ele (cdr lista))))
       (else (buscarIndices (+ i 1) ele (cdr lista)))))


;;Esta funcion devuelve los indices de las columnas que tienen el mayor sencillo(el puntaje de su mejor cualidad (diagonal, horizontal, vertical))
(define (mayorPosicionSimple listaDeMayores)
  (buscarIndices 0 (car (quicksort listaDeMayores)) listaDeMayores))


;;Esta funcion selecciona el correcto indice de columna al cual se debe ir de acuerdo a la funcion que califica
(define (selector numAI numjugador matriz)
  (selectorAux 
               (generarMatrizDePuntos 0 numAI (posicionesY 0 (car matriz) matriz) matriz)
               (generarMatrizDePuntos 0 numjugador (posicionesY 0 (car matriz) matriz) matriz)))

;;Esta funcion es auxiliar del selector y se encarga de llamar al selector real con los resumenes de puntos
(define (selectorAux puntosAI puntosJugador)
  (selectorAux2 (transpuesta puntosAI) (transpuesta puntosJugador) ))

;;Esta funcion es auxiliar del selector y se encarga de llamar al selector real con los resumenes de puntos y con los indices de las mejores posiciones
(define (selectorAux2 transpuestaAI transpuestaJugador)
   (selectorAux3 transpuestaAI
                 (mayorPosicionSimple (car transpuestaAI))
                 (posicion (car (mayorPosicionSimple (car transpuestaAI))) (car transpuestaAI))
                 transpuestaJugador
                 (mayorPosicionSimple (car transpuestaJugador))
                 (posicion (car (mayorPosicionSimple (car transpuestaJugador))) (car transpuestaJugador))))

;;Esta funcion es auxiliar del selector y se encarga de seleccionar a cual columna ir
(define (selectorAux3 transpuestaAI mayoresAI mayorAI transpuestaJugador mayoresJugador mayorJ)
  (cond ((> mayorAI 2) (car mayoresAI)) 
        ((> mayorJ 2) (posicion (selectorAux4Ju transpuestaAI mayoresAI transpuestaJugador mayoresJugador) mayoresJugador))
        ((equal? mayorAI 2) (posicion (selectorAux4AI transpuestaAI mayoresAI transpuestaJugador mayoresJugador) mayoresAI))
        ((equal? mayorJ 2) (posicion (selectorAux4Ju transpuestaAI mayoresAI transpuestaJugador mayoresJugador) mayoresJugador))
        ((equal? mayorAI 1) (posicion (selectorAux4AI transpuestaAI mayoresAI transpuestaJugador mayoresJugador) mayoresAI))
        ((equal? mayorJ 1) (posicion (selectorAux4Ju transpuestaAI mayoresAI transpuestaJugador mayoresJugador) mayoresJugador))
        (else 0)))

;;Esta funcion es auxiliar del selector y se encarga de llamar a una segunda comparacion debido a que hubo empate(cuando se decidio por atacar)
(define (selectorAux4AI transpuestaAI mayoresAI transpuestaJugador mayoresJugador)
  (cond ((null? (cdr mayoresAI)) 0)
        (else (selectorAux2 (reducirPrimera (transpuesta (posiciones mayoresAI (transpuesta transpuestaAI))))
                            (transpuesta (posiciones mayoresAI (transpuesta transpuestaJugador)))))))
;;Esta funcion es auxiliar del selector y se encarga de llamar a una segunda comparacion debido a que hubo empate(cuando se decidio por defender)
(define (selectorAux4Ju transpuestaAI mayoresAI transpuestaJugador mayoresJugador)
  (cond ((null? (cdr mayoresJugador)) 0)
        (else (selectorAux2 (transpuesta (posiciones mayoresJugador (transpuesta transpuestaAI)))
                            (reducirPrimera (transpuesta (posiciones mayoresJugador (transpuesta transpuestaJugador))))))))

;;Esta funcion se encarga de mandar al fondo las posiciones que fueron descartadas
(define (reducirPrimera matrix)
  (append (cdr matrix) (list (copiarceros(car matrix)))))

;;Esta funcion se encarga de modificar las posiciones que fueron descartadas por ceros
(define (copiarceros lista)
  (cond ((null? lista) '())
        (else (cons 0 (copiarceros (cdr lista))))))

;;Esta funcion se encarga de devolver una lista con el indice de la mayor posicion desocupada en cada columna
(define (posicionesY i linea0 matriz)
  (cond ((null? linea0) '())
        (else (cons (posY? i 0 matriz) (posicionesY (+ i 1) (cdr linea0) matriz)))))

;;Esta funcion es el algoritmo greedy para seleccionar la columna escogida por la computadora
(define (seleccionarColAI matriz)
  (selector 2 1 matriz))

;---------------------------------------------------------------------------------------------------------;
  
;; Funcion miembro?: Devuelve un valor booleano que indica si el elemento dado esta en la lista dada.
(define (miembro? elemento lista)
     (cond ((null? lista) #f)
           ((equal? (car lista) elemento) #t)
           (else (miembro? elemento (cdr lista)))))


;---------------------------------------------------------------------------------------------------------;


;; Funcion quicksort: Ordena la lista dada en orden ascendente, de acuerdo al algoritmo de quicksort.
;Esto lo hace calculando recursivamente el quicksort en la lista partida (la mitad mas pequena se agrega a la lista izquierda
;y la mitad mas grande a la lista derecha)

(define (quicksort lista)
  (cond   ( (null? lista) lista)
          (else(append
               (quicksort (mayores (cdr lista) (car lista))) 
               (list (car lista)) 
               (quicksort (menores (cdr lista) (car lista))))))) 

;Funcion menores: Es una funcion auxiliar del quicksort, devuelve una lista con los elementos
;menores que el pivot.

(define (menores lista pivot)
  (cond ((null? lista) '())
     ((> pivot (car lista)) (cons (car lista) (menores (cdr lista) pivot)))
     (else (menores (cdr lista) pivot))))

;Funcion mayores: Es una funcion auxiliar del quicksort, devuelve una lista con los elementos
;mayores que el pivot.

(define (mayores lista pivot)
  (cond ((null? lista) '())
     ((<= pivot (car lista)) (cons (car lista) (mayores (cdr lista) pivot)))
     (else (mayores (cdr lista) pivot))))

;---------------------------------------------------------------------------------------------------------;



;Funcion que revice si en la matriz existe un GanadorJugador o Empate si no enviar a jugar pc

(define(mainAux1 matriz)
  (pretty-print matriz)
  (cond((empate? (car matriz))(send p3 show #t))
       ((hayGanadorPlayer? matriz) (send p11 show #t))
       (else
        (sleep 0.5)
        (dibujarC (* 50 (seleccionarColAI matriz)) (* 50 (posY? (seleccionarColAI matriz) 0 matriz)) )
        (mainAux2 (insertarMatriz (seleccionarColAI matriz) 2 matriz)))))

;;Esta funcion se encarga de manejar el turno del computador
(define(mainAux2 matriz)
  (pretty-print matriz)
  (cond((empate? (car matriz))(send p3 show #t))
       ((hayGanadorComputadora? matriz)(send p2 show #t))
       (else (mainAux3 matriz (- (juego (car (tamano)) (colorJ)) 1) (colorJ)))))


;;Esta funcion se encarga de manejar el turno del jugador
(define(mainAux3 matriz x color)
  (cond ((equal? (posY? x 0 matriz) -1) (mainAux2 matriz)) 
        (else
            (dibujarP (* 50 x ) (* 50 (posY? x 0 matriz)) color )
            (mainAux1 (insertarMatriz x 1 matriz)))))
        
;Esta funcion llama a la creacion de la matriz y al inicio del juego
(define(main)
  (mainAux2 (crearMatriz (car (tamano)) (cadr (tamano)))))

(thread main)
