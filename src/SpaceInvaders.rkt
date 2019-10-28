;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname SpaceInvaders) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; SPACE INVADERS
; RACKET

; Constantes => SNAKE_CASE
; Funcoes => snake_case
; Variaveis => camelCase



; ### CONSTANTES ###

(define BG_HEIGHT 400)
(define BG_WIDTH 200)
(define BG (empty-scene BG_WIDTH BG_HEIGHT))
(define MIN_WIDTH 5)
(define MAX_WIDTH 195)
(define Y_CHAO 380)

(define NAVE (circle 10 "solid" "blue"))
(define VELOCIDADE_NAVE 3)

(define TIRO_WIDTH 3)
(define TIRO_HEIGHT 16)
(define TIRO (rectangle TIRO_WIDTH TIRO_HEIGHT "solid" "green"))
(define Y_INICIAL_TIRO 380)
(define VELOCIDADE_TIRO 5)

(define INIMIGO_WIDTH 16)
(define INIMIGO_HEIGHT 16)
(define INIMIGO (rectangle INIMIGO_WIDTH INIMIGO_HEIGHT "solid" "red"))
(define VELOCIDADE_INIMIGO 1)
(define PROB_INIMIGO 0.03) ; entre 0.01 e 0.04 para melhores resultados



; ### STRUCTS ###

; struct posicao
; x:Numero posicao horizontal
; y:Numero posicao vertical
(define-struct posicao (x y))

; struct jogo
; x:Numero posicao horizontal da nave
; listaTiros:list lista de structs posicao representando tiros
; listaInimigos:list lista de structs posicao representando inimigos
(define-struct jogo (x listaTiros listaInimigos))


; ######## FUNCOES ########



; --- DESENHA ---

; render: jogo -> None
;    desenha tiros, nave e inimigos
(define (render estadoAtual)
  (desenha_tiros (desenha_inimigos (place-image NAVE (jogo-x estadoAtual) 380 BG) (jogo-listaInimigos estadoAtual))
                 (jogo-listaTiros estadoAtual)))

; desenha_tiros: imagem, lista -> imagem
;      desenha todos os tiros da lista de tiros
(define (desenha_tiros imagem lista)
  (cond
    [(empty? lista) imagem]
    [else (desenha_tiros (place-image TIRO
                                      (posicao-x (first lista))
                                      (posicao-y (first lista))
                                      imagem)
                         (rest lista))]))

; desenha_inimigos: imagem, lista -> imagem
;      desenha todos os inimigos da lista de inimigos
(define (desenha_inimigos imagem lista)
  (cond
    [(empty? lista) imagem]
    [else (desenha_inimigos (place-image INIMIGO
                                         (posicao-x (first lista))
                                         (posicao-y (first lista))
                                         imagem)
                            (rest lista))]))







; --- LOOP ---

; loop: jogo -> jogo
;     chama atualiza_tiros e atualiza_inimigos
(define (loop estadoAtual)
  (remove_colisoes2
   (make-jogo (jogo-x estadoAtual)
              (atualiza_tiros (jogo-listaTiros estadoAtual))
              (cria_inimigo (atualiza_inimigos (jogo-listaInimigos estadoAtual))))))

; cria_inimigos: jogo -> jogo
(define (cria_inimigo lista)
  (if (> (random) (- 1 PROB_INIMIGO))
      (cons (make-posicao (random_num 0 MAX_WIDTH) 0) lista)
      lista))

(define (random_num min max)
  (floor (+
          (* (random)
             (- max min))
          min)))

; atualiza_inimigos: lista -> lista
;     atualiza posicao dos inimigos
;     botando y = y+1
;     e removendo inimigos que tocam o chao
(define (atualiza_inimigos lista)
  (cond
    [(empty? lista) empty]
    [(>= (posicao-y (first lista)) Y_CHAO) (atualiza_inimigos (rest lista))]
    [else (cons (make-posicao (posicao-x (first lista)) (+ (posicao-y (first lista)) VELOCIDADE_INIMIGO))
                (atualiza_inimigos (rest lista)))]))
;test
(check-expect (atualiza_inimigos
               (cons (make-posicao 10 10) (cons (make-posicao 25 30) (cons (make-posicao 2 380) empty))))
              (cons (make-posicao 10 (+ 10 VELOCIDADE_INIMIGO))
                    (cons (make-posicao 25 (+ 30 VELOCIDADE_INIMIGO)) empty)))

; atualiza_tiros: lista -> lista
;     atualiza posicao dos tiros
;     botando y = y-1
;     e removendo tiros que estao fora da tela
(define (atualiza_tiros lista)
  (cond
    [(empty? lista) empty]
    [(<= (posicao-y (first lista)) 0) (atualiza_tiros (rest lista))]
    [else (cons (make-posicao (posicao-x (first lista)) (- (posicao-y (first lista)) VELOCIDADE_TIRO))
                (atualiza_tiros (rest lista)))]))
;test
(check-expect (atualiza_tiros (cons (make-posicao 10 10)
                                    (cons (make-posicao 25 30)
                                          (cons (make-posicao 2 0)
                                                empty))))
              (cons (make-posicao 10 (- 10 VELOCIDADE_TIRO))
                    (cons (make-posicao 25 (- 30 VELOCIDADE_TIRO)) empty)))








; --- COLISOES ---

; remove_colisoes2: jogo -> jogo
;     dado um jogo,
;     percorre a lista de tiros e inimigos
;     e retorna o jogo sem os inimigos em que houve colisao
(define (remove_colisoes2 ea)
  (cond
    [(empty? (jogo-listaTiros ea)) ea]
    [(empty? (jogo-listaInimigos ea)) ea]
    [(colide_com_lista? (first (jogo-listaInimigos ea)) (jogo-listaTiros ea))
     (remove_colisoes2 (make-jogo
                        (jogo-x ea)
                        (jogo-listaTiros ea)
                        (rest (jogo-listaInimigos ea))))]
    [else (make-jogo (jogo-x ea)
                     (jogo-listaTiros ea)
                     (cons (first (jogo-listaInimigos ea))
                           (jogo-listaInimigos (remove_colisoes2 (make-jogo (jogo-x ea)
                                                                            (jogo-listaTiros ea)
                                                                            (rest (jogo-listaInimigos ea)))))))]))

; colide_com_lista?: posicao lista -> Boolean
;     dadas uma posicao e uma lista de posicoes
;     retorna #true se a posicao colide com alguma posicao da lista
;             #false caso contrario
(define (colide_com_lista? posicao lista)
  (cond
    [(empty? lista) #false]
    [(colide? posicao (first lista)) #true]
    [else (colide_com_lista? posicao (rest lista))]))

; colide?: posicao_inimigo posicao_tiro -> Boolean
;     dadas as posicoes de inimigo e de tiro,
;     retorna #true se colidem
;             #false caso contrario
(define (colide? tiro inimigo)
  (if (in_range? (posicao-x tiro)
                 (- (posicao-x inimigo) (/ INIMIGO_WIDTH 2))
                 (+ (posicao-x inimigo) (/ INIMIGO_WIDTH 2)))
      (if (in_range? (posicao-y tiro)
                     (- (posicao-y inimigo) (/ INIMIGO_HEIGHT 2))
                     (+ (posicao-y inimigo) (/ INIMIGO_HEIGHT 2)))
          #true
          #false)
      #false))
          
; in_range?: numero numero numero -> Boolean
;     dados um valor x e os valores de minimo e maximo
;     retorna #true caso x esteja entre esses valores
;             #false caso contrario
(define (in_range? x min max)
  (if (<= x max)
      (if (>= x min)
          #true
          #false)
      #false))








; ## ACOES ##

; atira: jogo -> jogo
(define (atira estadoAtual)
  (make-jogo (jogo-x estadoAtual)
             (cond
               [(empty? (jogo-listaTiros estadoAtual)) (cons (make-posicao (jogo-x estadoAtual) Y_INICIAL_TIRO) empty)]
               [else (cons (make-posicao (jogo-x estadoAtual) Y_INICIAL_TIRO) (jogo-listaTiros estadoAtual))])
             (jogo-listaInimigos estadoAtual)))
;test
(check-expect (atira (make-jogo 20 (cons (make-posicao 20 20) empty) empty))
              (make-jogo 20 (cons (make-posicao 20 Y_INICIAL_TIRO) (cons (make-posicao 20 20) empty)) empty))

; ativa_super_poder: jogo -> jogo
(define (ativa_super_poder ea)
  (make-jogo (jogo-x ea)
             (jogo-listaTiros ea)
             empty))

; move_esquerda: jogo -> jogo
;   Dado um jogo,
;   retorna um jogo com x = (x - 1)
(define (move_esquerda estadoAtual)
  (make-jogo
   (cond
     [(<= (- (jogo-x estadoAtual) VELOCIDADE_NAVE) MIN_WIDTH) MIN_WIDTH]
     [else (- (jogo-x estadoAtual) VELOCIDADE_NAVE)])
   (jogo-listaTiros estadoAtual)
   (jogo-listaInimigos estadoAtual)))

; move_direita: jogo -> jogo
;   Dado um jogo,
;   retorna um jogo com x = (x + 1)
(define (move_direita estadoAtual)
  (make-jogo
   (cond
     [(>= (+ (jogo-x estadoAtual) VELOCIDADE_NAVE) MAX_WIDTH) MAX_WIDTH]
     [else (+ (jogo-x estadoAtual) VELOCIDADE_NAVE)])
   (jogo-listaTiros estadoAtual)
   (jogo-listaInimigos estadoAtual)))








; ## MAIN ##

; key_pressed: jogo, tecla -> jogo
;   Dado um jogo e uma tecla,
;   gerencia a tecla pressionada, chama a funcao necessaria
;   retorna um jogo
(define (key_pressed estadoAtual tecla)
  (cond
    [(key=? tecla "left") (move_esquerda estadoAtual)]
    [(key=? tecla "right") (move_direita estadoAtual)]
    [(key=? tecla " ") (atira estadoAtual)]
    [(key=? tecla "s") (ativa_super_poder estadoAtual)]
    [else estadoAtual]))

(big-bang
    (make-jogo (/ BG_WIDTH 2) empty (cons (make-posicao 20 0) (cons (make-posicao 40 10) empty)))
  (on-key key_pressed)
  (to-draw render)
  (on-tick loop))
