#lang racket
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------

;---------------------------------------------------------------- LEXER ANALIZER V2.0 ------------------------------------------------------------------------------

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------


(define html-samp-start
  "
<html>
    <title> LexerScheme </title>
        <head>

            <link href = \"styles/style.css\" rel = \"stylesheet\" type = \"text/css\">

        </head>
            <body>
                <h2> LexerScheme V2.0 </h2>
                <h4>Informacion de color y de estilos</h4>
                <br>
                <span class = 'box_rounded_1'> Identificadores &nbsp; </span> <span class = 'box_rounded_2'> Palabras Reservadas &nbsp;</span> <span class = 'box_rounded_3'> Constantes Decimales &nbsp; </span> <span class = 'box_rounded_4'> Constantes Enteras Punto Flotante &nbsp; </span> <span class = 'box_rounded_5'> Simbolos Especiales &nbsp; </span> <span class = 'box_rounded_6'> Comentarios &nbsp; </span> <span class = 'box_rounded_7'> Errores &nbsp; </span> <br><br>
")

(define html-samp-end

  "
        </body>

</html>

")

;Automata

(define verifica-reservada
  (lambda (str)
    (cond
      ((equal? str "def") "<span class = 'reserved'> def </span>")
      ((equal? str "if") "<span class = 'reserved'> if </span>")
      ((equal? str "cond") "<span class = 'reserved'> cond </span>")
      ((equal? str "defn") "<span class = 'reserved'> defn </span>")
      ((equal? str "fn") "<span class = 'reserved'> fn </span>")
      ((equal? str "else") "<span class = 'reserved'> else </span>")
      ((equal? str "true") "<span class = 'reserved'> true </span>")
      ((equal? str "flase") "<span class = 'reserved'> flase </span>")
      ((equal? str "nil") "<span class = 'reserved'> nil </span>")
      ((equal? str "cons") "<span class = 'reserved'> cons </span>")
      ((equal? str "first") "<span class = 'reserved'> first </span>")
      ((equal? str "second") "<span class = 'reserved'> second </span>")
      ((equal? str "rest") "<span class = 'reserved'> rest </span>")
      ((equal? str "next") "<span class = 'reserved'> next </span>")
      ((equal? str "concat") "<span class = 'reserved'> concat </span>")
      ((equal? str "map") "<span class = 'reserved'> map </span>")
      ((equal? str "apply") "<span class = 'reserved'> apply </span>")
      ((equal? str "filter") "<span class = 'reserved'> filter </span>")
      ((equal? str "reduce") "<span class = 'reserved'> reduce </span>")
      ((equal? str "pmap") "<span class = 'reserved'> pmap </span>")
      ((equal? str "let") "<span class = 'reserved'> let </span>")
      ((equal? str "seq") "<span class = 'reserved'> seq </span>")
      ((equal? str "conj") "<span class = 'reserved'> conj </span>")
      ((equal? str "do") "<span class = 'reserved'> do </span>")
      ((equal? str "and") "<span class = 'reserved'> and </span>")
      ((equal? str "or") "<span class = 'reserved'> or </span>")
      ((equal? str "not") "<span class = 'reserved'> not </span>")
      ((equal? str "not=") "<span class = 'reserved'> not= </span>")
      ((equal? str "quote") "<span class = 'reserved'> quote </span>")
      ((equal? str "empty") "<span class = 'reserved'> empty? </span>")
      ((equal? str "take") "<span class = 'reserved'> take </span>")
      ((equal? str "range") "<span class = 'reserved'> range </span>")
      ((equal? str "doall") "<span class = 'reserved'> doall </span>")
      ((equal? str "time") "<span class = 'reserved'> time </span>")
      ((equal? str "future") "<span class = 'reserved'> future </span>")
      ((equal? str "delay") "<span class = 'reserved'> delay </span>")
      ((equal? str "promise") "<span class = 'reserved'> promise </span>")
      (else 0))))

(define reserved '("def" "if" "cond" "defn" "fn" "else" "true" "false" "nil" "cons" "first" "second" "rest" "next" "concat" "map" "filter" "apply" "reduce" "pmap"
                   "let" "seq" "conj" "do" "and" "or" "not" "not=" "quote" "empty?" "take" "range" "doall" "time" "future" "delay" "promise"))




;----------------------------------------- VARIABLES GLOBALES --------------------------------------

(define secuencia "")

(define character-number 1)
(define regresion #f)
(define check #f)
(define comment #f)
(define filepath "code.clj")
(define end-of-file #f)

(define filepath-html "index.html")
(define html-file (open-output-file filepath-html #:exists `replace))
(display html-samp-start html-file)

;----------------------------------------- VARIABLES GLOBALES --------------------------------------

;Funcion para accesar a un elemento especifico por su posicion
(define access-n
  (lambda (lista element)
  (define n-elements characters-in-file)
  (cond
    ((null? lista) 0)
    ((= element 0) 0)
    ((= element 1) (car lista))
    ((> element n-elements) 0)
    (else (access-n (cdr lista) (- element 1))))))

;Funcion para saber cuando ya se tiene un elemento del archivo
(define get-token
  (lambda (chars-number chars)

    (define char (access-n chars character-number))
    
    (cond
      ((equal? character-number characters-in-file) (set! check "EOF") (set! end-of-file #t))
      
      ((equal? char " ")
       (cond
         ((equal? comment #t) (set! secuencia (string-append secuencia char)) (set! character-number (+ 1 character-number)) (set! check #f) check)
         (else
          (set! character-number (+ 1 character-number)) (set! check #t) (display "&nbsp;&nbsp;" html-file) check)))

      ((or (equal? char "\n") (equal? char "\r"))
       (set! character-number (+ 1 character-number)) (set! comment #f) (set! check #t) (display "<br>" html-file) check)

      ((equal? regresion #t)
       (set! regresion #f) (set! secuencia (string-append secuencia char)) (set! character-number (+ 1 character-number)) (set! check #t) check)

      ((and (or (equal? char "+") (equal? char "-") (equal? char "*") (equal? char "/") (equal? char "=") (equal? char "<") (equal? char ">") (equal? char "#") (equal? char "%") (equal? char ":")
                (equal? char "(") (equal? char ")") (equal? char "[") (equal? char "]") (equal? char "{") (equal? char "}") (equal? char "'")) (equal? regresion #f))
       (cond
         ((and (or (equal? (access-n chars (+ 1 character-number)) " ") (equal? (access-n chars (+ 1 character-number)) "\n")) (or (equal? (access-n chars (+ 1 character-number)) " ") (equal? (access-n chars (+ 1 character-number)) "\n")))
           (cond
             ((equal? comment #t) (set! character-number (+ 1 character-number)) (set! check #f) check)
             (else
              (set! regresion #t) (set! check #t) check)))
         (else
          (cond
           ((or (equal? char "<") (equal? char ">") (equal? char "=") (equal? char "-") (equal? char "+")) (set! secuencia (string-append secuencia char)) (set! character-number (+ 1 character-number)) (set! check #f) check)
           (else
            (set! character-number (+ 1 character-number)) (set! check #f) check)))))

      (else
       (set! secuencia (string-append secuencia char))
       (cond
         ((equal? (string-ref secuencia 0) #\;) (set! comment #t)))
       (set! character-number (+ 1 character-number)) (set! check #f) check))))


;CONJUNTOS DE CARACTERES

(define letra '( "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "x" "y" "z"
                 "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "X" "Y" "Z"))

(define sim-especial '( "+" "-" "*" "/" "=" "<" ">" "#" "%" ":" "(" ")" "[" "]" "{" "}" "'" ))

(define digitos '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

(define punto '("."))

(define delimitador '(" " "\n" "\r"))

(define comentario '(";"))


;Checa si un elemento esta en una lista
(define is_in
  (lambda (elemento lista)
  (cond
    ((null? lista) #f)
    ((equal? (car lista) elemento) #t)
    (else (is_in elemento (cdr lista))))))

;Funcion para checar el tipo de elemento que es un string
(define type
  (lambda (elemento)
  (cond
    ((is_in elemento digitos) "digito")
    ((is_in elemento punto) "punto")
    ((is_in elemento letra) "letra")
    ((is_in elemento delimitador) "delimitador")
    ((is_in elemento sim-especial) "sim-esp")
    ((is_in elemento comentario) "comment")
    (else "Sin-tipo"))))


;------------------------------------------------------------------ AUTOMATA SECTION -------------------------------------------------------------

;FSA
(define FSA '(((1 "letra" 2) (2 "letra" 2) (1 "digito" 3) (3 "punto" 4) (4 "digito" 5) (5 "digito" 5) (3 "digito" 3) (1 "sim-esp" 6) (1 "comment" 7) ) (2 3 5 6 7)))


;Funcion helper para verificar los estados aceptores
(define miembro
  (lambda (elemento lista)
  (cond
    ((null? lista) 0)   
    ((equal? elemento (car lista)) elemento)
    (else (miembro elemento (cdr lista))))))

;Funcion helper para realizar la comrpobacion con el automata para el usuario
(define (token secuencia)
  ;Transiciones del automata
  (define transiciones (car FSA))
  ;Estado inicial del automata
  (define e-inicial (caaar FSA))
  ;Estado(s) Final(es) del automata
  (define e-finales (cadr FSA))

  ;Llamada a la funcion principal del automata para reconocer las transiciones, estado incial y estado(s) final(es)
  (cond
    ((= (automata transiciones secuencia e-inicial e-finales) 2) "Identificador")
    ((= (automata transiciones secuencia e-inicial e-finales) 3) "Constante Decimal")
    ((= (automata transiciones secuencia e-inicial e-finales) 5) "Constante Entera Punto Flotante")
    ((= (automata transiciones secuencia e-inicial e-finales) 6) "Simbolo Especial")
    ((= (automata transiciones secuencia e-inicial e-finales) 7) "Comentario")
    (else "Secuencia Invalida")))
  

;Funcion helper para recorrer las transiciones con el simbolo dado para generar su siguiente estado
(define comprobar-estados-simbolo
  (lambda (transiciones estado caracter)
  (with-handlers ([exn:fail? (lambda (exn) 0)])
    (if
     (and (eq? estado (caar transiciones)) (eq? caracter (car(cdr(car transiciones)))))
     (car(cddr(car transiciones)))
     (comprobar-estados-simbolo (cdr transiciones) estado caracter)))))

;Funcion helper para recorrer la cadena de caracteres y obtener el siguiente estado 
(define automata
  (lambda (transiciones cadena s-comp e-finales)
  (cond
    ((null? cadena) (miembro s-comp e-finales))
    (else
    (automata transiciones (cdr cadena) (comprobar-estados-simbolo transiciones s-comp (car cadena)) e-finales)))))

;------------------------------------------------------------------ AUTOMATA SECTION -------------------------------------------------------------


;----------------------------------------------------------------ACCIONES PARA LEER----------------------------------------------------------

;Funcion helper para leer lineas de un archivo
(define read-lines-from-port-impl
        (lambda (file-input-port)
                (let* ((char (read-char file-input-port)))
                      (if (eof-object? char)
                          (list)
                          (cons char (read-lines-from-port-impl file-input-port))))))

;Funcion helper para leer caracteres de un archivo
(define read-chars-from-port
        (lambda (file-port)
                (if (input-port? file-port)
                    (read-lines-from-port-impl file-port)
                    (list))))

;FUNCION MAIN PARA LEER EL ARCHIVO
(define read-chars-from-file
        (lambda (filename)
                (call-with-input-file filename read-chars-from-port)))

;----------------------------------------------------------------ACCIONES PARA LEER----------------------------------------------------------

;-------------------------------------------------------------- ACCIONES PARA PROCESAR --------------------------------------------------------

;Funcion helper para eliminar elementos de una lista
(define delete
  (lambda (element list)
  (filter (negate (curry equal? element)) list)))


;Funcion helper para convertir una lista de caracteres a una lista de strings
(define char2string
  (lambda (list)
    (if (null? list)
        '()
        (delete "\r" (cons (string (car list)) (char2string (cdr list)))))))

;Funcion helper para convertir lista de strings en lista de tokens
(define convierte-lista
  (lambda (lista)
    (cond
      ((null? lista) null)
      ((equal? (car lista) ";") '("comment"))
      (else
       (append (list (type (car lista))) (convierte-lista (cdr lista)))))))

;Funcion helper para convertir una lista de strings en un string general
(define list2string
  (lambda (list)
    (if (null? list)
        ""
        (apply string-append list))))

;Funcion helper para connvertir un string en una lista caracter por caracter
(define secuencia2list
  (lambda (str)
    (define secuencia-traducida (string->list str))
    secuencia-traducida))

;Simbolo que contiene todos los caracteres del archivo
(define characters (char2string (read-chars-from-file filepath)))

;Simbolo que contiene el numero de caracteres que tiene el archivo
(define characters-in-file (length characters))

;-------------------------------------------------------------- ACCIONES PARA PROCESAR --------------------------------------------------------



;MAIN DEL LEXER

;Variable que almacena el tipo de token actual
(define ver-tkn #f)

;Funcion principal del programa (el argumento es indiferente)
(define start
  (lambda (pass)
    
    (define tk (get-token characters-in-file characters))

    (if (equal? end-of-file #t)
        (display "Analisis terminado con exito!\n\n")
        (cond
          ((equal? tk #t)
           (set! ver-tkn (token (convierte-lista (char2string (secuencia2list secuencia)))))

           (cond
             ((equal? ver-tkn "Identificador")
              (cond
                ((equal? (verifica-reservada secuencia) 0)
                 (display "<span class = 'identifier'>" html-file)
                 (display secuencia html-file)
                 (display "</span>" html-file) (set! secuencia "") (start 1))
                
                (else
                 (display (verifica-reservada secuencia) html-file) (start 1))))

             ((equal? ver-tkn "Constante Decimal")
              (display "<span class = 'cons_int'>" html-file)
              (display secuencia html-file)
              (display "</span>" html-file) (set! secuencia "") (start 1))

             ((equal? ver-tkn "Constante Entera Punto Flotante")
              (display "<span class = 'cons_float'>" html-file)
              (display secuencia html-file)
              (display "</span>" html-file) (set! secuencia "") (start 1))

             ((equal? ver-tkn "Simbolo Especial")
              (display "<span class = 'special'>" html-file)
              (display secuencia html-file)
              (display "</span>" html-file) (set! secuencia "") (start 1))

             ((equal? ver-tkn "Comentario")
              (display "<span class = 'comment'>" html-file)
              (display secuencia html-file)
              (display "</span>" html-file) (set! secuencia "") (start 1))

             ((equal? ver-tkn "Secuencia Invalida")
              (display "<span class = 'notvalid'>" html-file)
              (display secuencia html-file)
              (display "</span>" html-file) (set! secuencia "") (start 1))))

          (else
           (start 1))))))



;Inicio del programa
(time (start 1))

;Escribir la ultima parte del archivo html
(display html-samp-end html-file)

;Cerrar archivo html
(close-output-port html-file)


;FIN DEL PROGRAMA








        




