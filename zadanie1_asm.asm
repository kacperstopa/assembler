DATA1 segment

ARGS db 200 dup(0)
INVALINUMBER db "Niepoprawna ilosc argumentow ", 10, 13, '$'
INVALIDLENGTH db "Argumenty sa zlej dlugosci ", 10, 13, '$'
INVALIDARGFORM db "Nieprawidłowe znaki w argumentach", 10, 13, '$'
ASCIART db 0, '.', 'o', '+', '=', '*', 'B', 'O', 'X', '@', '%', '&', '#', '/', '^'
CHESSBOARD db 153 dup (0)

DATA1 ends


CODE1 segment

START:

        mov ax, seg top1
        mov ss, ax
        mov sp, offset top1 ; inicjalizacja stosu

        mov ax, ds
        mov es, ax  ;linia komend w es

        mov si, 82h ; początek argumentow z linii komend w si

        mov ax, seg ARGS
        mov ds, ax

        mov di, offset ARGS ; w di ustawiam poczatek tablicy

        call parseArguments
        ;call printargs
        mov al, 0
        call Validate_args
        cmp al, 0
        jg koniec
        call do_alhorythm
        call print_chessboard

        koniec:
        mov ah, 4ch
        int 21h



parseArguments:

        push si
        push di
        push ax
        push bx
        push bp
        pushf


        xor al, al


        mov bx, di ; bx wskazuje na poczatek tablicy args, pierwsza pozycja
        ; bedzie mowila ile jest argumentow, kolejne 16 mowily ich odpowiednia
        ;dlugosc a reszta ciąg argumentow

        mov byte ptr [bx], 0

        add di, 17

        saveArguments:

                        call skip_white ; pomiń białe znakiem


                        cmp al, 1 ; jesli flaga=1 to znaczy że skonczyła się linia komend
                        je end_parseArguments
                                    ; w przeciwnym razie:
                        inc byte ptr [bx] ; zwiekszam licznik argumentow


                        call save_word ; trafiłem na argument wiec go zapisuje

                        push bx ;
                        push ax

                            mov al, byte ptr [bx] ; ax==al
                            xor ah, ah

                            add bx, ax

                            mov byte ptr [bx], cl ; bo cx == cl

                        pop ax
                        pop bx

                        jmp saveArguments

                    end_parseArguments:
                        popf
                        pop bp
                        pop bx
                        pop ax
                        pop di
                        pop si
                        ret


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip_white:

        pushf

        skip_white_start:

        mov ah, byte ptr es:[si] ; zapisuję dany znak do ah

        cmp ah, 13 ; jesli koniec linii komend to ustawiam odpowiednią flagę i
        je set_flag_white ; kończę procedurę

        cmp ah, ' ' ; jesli znak biały ( mniejszy od spacji w ASCII )
        jle inc_si ; to zwiększam indeks i powtarzam

        jmp end_skip_white ; nie jest znakiem bialym ani CR więc koncze funkcje

        inc_si:
              inc si
              jmp skip_white_start

        set_flag_white:
              mov al, 1

        end_skip_white:

        popf
        ret

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


save_word:
        pushf
        push ax
        mov cx, 0 ; licznik dlugosci argumentu

        save_word_start:

              mov ah, es:[si] ; zapisuję dany znak do ah

              cmp ah, ' '
              jle end_save_word ; jesli znak jest biały to koncze zapisywanie

              mov byte ptr[di], ah ; zapisuje znak do tablicy argumentow
              inc di ; zwiekszam indeks w tablicy argumentow
              inc si ; biore kolejny znak
              inc cx ; zwiekszam licznik dlugosci argumentow

              jmp save_word_start

        end_save_word:
              pop ax
              popf
              ret

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  printargs: ; wypisuje kazdy argument w nowej linii
          push cx
          push si
          push di
          push ax
          push dx
          pushf

          mov cl, byte ptr[di]
          xor ch, ch
          mov si, di
          inc si
          add di, 17
          slowo:
                push cx
                mov cl,byte ptr [si]
                xor ch, ch
                inc si
                znak:
                      mov dl,byte ptr [di]
                      mov ah, 2
                      int 21h
                      inc di
                      loop znak
                pop cx
                mov dl, 10
                mov ah, 2
                int 21h
                loop slowo

          popf
          pop dx
          pop ax
          pop di
          pop si
          pop cx
          ret

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Validate_args: ; sprawdza poprawność wprowadzonych elementow
                    push dx
                    push di
                    push cx

                    mov ah, byte ptr [di]
                    cmp ah, 2         ; liczba argumentow rozna od 2 -> błąd
                    jne invalid_arg_number

                    inc di
                    mov ah, byte ptr [di]
                    cmp ah, 1       ; długość pierwszego argumentu rozna 1 ->błąd
                    jne invalid_arg_length

                    inc di
                    mov ah, byte ptr [di]
                    cmp ah, 32 ; dlugosc drugiego argumentu rozna 31 -> błąd
                    jne invalid_arg_length

                    add di, 15
                    mov ah, byte ptr [di]
                    cmp ah, '1'
                    je validate_next
                    cmp ah, '0'
                    jne invalid_arg_form

            validate_next:

                    mov cx, 32
            validate_hexkey:
                    inc di
                    mov ah, byte ptr[di]
                    cmp ah, 'f'
                    jg invalid_arg_form
                    cmp ah, 'a'
                    jge next_char
                    cmp ah, '9'
                    jg invalid_arg_form
                    cmp ah, '0'
                    jl invalid_arg_form
  next_char:        loop validate_hexkey

                    validate_end:
                      pop cx
                      pop di
                      pop dx
                      ret

                    invalid_arg_length:
                      inc al
                      mov dx, offset INVALIDLENGTH
                      mov ah, 9
                      int 21h
                      jmp validate_end

                    invalid_arg_number:
                      inc al
                      mov dx, offset INVALINUMBER
                      mov ah, 9
                      int 21h
                      jmp validate_end

                    invalid_arg_form:
                      inc al
                      mov dx, offset INVALIDARGFORM
                      mov ah, 9
                      int 21h
                      jmp validate_end


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_alhorythm:
                  push cx
                  push di
                  push bx
                  push si
                  push ax

                  mov cx, 16 ; 16 bajtow do przetworzenia
                  add di, 18 ; ds:[di] wskazuje na poczatek drugiego argumentu
                  mov bx, 76 ; srodek tablicy
                  mov si, offset CHESSBOARD ; poczatek tablicy

            do_algorythm_start:

                  mov ah, byte ptr ds:[di] ; starsza czesc bajtu do ah
                  inc di
                  mov al, byte ptr ds:[di] ; mlodsza czesc bajdu do al
                  inc di
                  call Byte_to_bin ; zamieniam caly bajt (ah, al) na postac binarna i zwracam wynik w dl

                  push di
                  mov di, offset ARGS
                  add di, 17
                  cmp byte ptr ds:[di], '0'; jesli modyfikacja to neguje bajt
                  je next
                  not dl
            next:
                  pop di
                  call Move_bishop ; ruszam goncem 4 razy

                  loop do_algorythm_start ; powtarzaj dla pozostałych bajtow

                  call ascii_art ; zamieniam na ascii art
                  mov byte ptr ds:[si+76], 'S' ; 'S' na poczatek
                  mov byte ptr ds:[si+bx], 'E' ; 'E' na koniec

                  pop ax
                  pop si
                  pop bx
                  pop di
                  pop cx
                  ret





;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Byte_to_bin:
                  pushf
                  push bx
                  push cx
                  push ax
                  xor dh, dh

                  call Hex_to_bin ; zamieniam ah szesnastkowo na binarna
                  mov dl, ah ; przechowuje to w dl
                  mov cl, 4
                  shl dl, cl ; przesuwam o 4 bity w prawo
                  mov ah, al ; powtarzam to dla al
                  call Hex_to_bin
                  add dl, ah

                  pop ax
                  pop cx
                  pop bx
                  popf
                  ret


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




Hex_to_bin:       ; zamienia to co jest w ah na postac binarna i zwraca do ah
                pushf

                cmp ah, 'a' ; sprawdza czy litera czy cyfra
                jge letter_to_bin
                jmp number_to_bin

      letter_to_bin: ; jesli litera to odejmuje 'A' i dodaje 10
                sub ah, 'a'
                add ah, 10
                jmp Hex_to_bin_end

      number_to_bin:
                sub ah, '0' ; jesli cyfra to odejmuje '0'
                jmp Hex_to_bin_end

      Hex_to_bin_end:

                popf
                ret


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Move_bishop: ; w dl mam 8 bitow (4 ruchy ) do przetworzenia
              pushf
              push cx
              mov cx, 4
              one_move:

                      shr dl, 1 ; przesuwam o jeden bit w prawo
                      jc move_right ; jesli flaga carry==1 to znaczy ze byla 1
                      jmp move_left ; w przeciwnym razie 0

              up_down:

                      shr dl, 1
                      jc move_down
                      jmp move_up

              next_pair:
                      loop one_move

                      pop cx
                      popf
                      ret

              move_right:
              pushf
              push ax
              push cx

              mov ax, bx ; ax == dzielna
              mov cl, 17 ; cl = dzielnik
              div cl
              cmp ah, 16 ; ah = reszta z dzielenia, jesli jest rowna 16 to nie moge isc w prawo
              je move_right_end

              inc bx ; poszedlem w prawo wiec zwiekszam bx o 1

              move_right_end:
              pop cx
              pop ax
              popf
              jmp up_down ; probuje wykonac ruch gora/dol

              move_left:
              pushf
              push ax
              push cx
              mov ax, bx
              mov cl, 17
              div cl
              cmp ah, 0 ; jesli ah == 0 to nie moge isc w lewo
              je move_left_end

              dec bx ; ide o jedno w lewo

              move_left_end:
              pop cx
              pop ax
              popf
              jmp up_down


              move_down:
              pushf
              push ax
              push cx
              mov ax, bx
              mov cl, 17
              div cl
              cmp al, 8 ; jesli czesc calkwita dzielenia == 8 to nie moge isc w dol
              je move_down_end

              add bx, 17

              move_down_end:
              inc byte ptr ds:[si+bx] ; zwiekszam licznik odwiedzen pola
              pop cx
              pop ax
              popf

              jmp next_pair

              move_up:
              pushf
              push ax
              push cx
              mov ax, bx
              mov cl, 17
              div cl
              cmp al, 0 ; jesli czesc calkowita dzielenia == 0 to nie moge isc w gore
              je move_down_end

              sub bx, 17

              move_up_end:
              inc byte ptr ds:[si+bx] ; zwiekszam licznik odwiedzen pola
              pop cx
              pop ax
              popf
              jmp next_pair



;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



print_chessboard:
              pushf
              push cx
              push si
              mov si, offset CHESSBOARD
              mov dl, 201
              mov ah, 2h
              int 21h
              mov cx, 17
              print_frame_up:
              mov dl, 205
              mov ah, 2h
              int 21h
              loop print_frame_up
              mov dl, 187
              mov ah, 2h
              int 21h
              mov dl, 10
              mov ah, 2h
              int 21h
              mov cx, 9
              print_row:
                    mov dl, 186
                    mov ah, 2h
                    int 21h
                    push cx
                    mov cx, 17
                    print_sign:
                          mov dl, byte ptr ds:[si]
                          mov ah, 2
                          int 21h
                          inc si
                          loop print_sign
                    pop cx
                    mov dl, 186
                    mov ah, 2h
                    int 21h
                    mov dl, 10
                    mov ah, 2
                    int 21h
                    loop print_row
            mov dl, 200
            mov ah, 2h
            int 21h
            mov cx, 17
            print_frame_down:
            mov dl, 205
            mov ah, 2h
            int 21h
            loop print_frame_down
            mov dl, 188
            mov ah, 2h
            int 21h
            pop si
            pop cx
            popf
            ret


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ascii_art:
          pushf
          push bp
          push si
          push ax
          push bx
          push di
          push cx
          mov si, offset CHESSBOARD
          mov bp, offset ASCIART
          mov cx, 153

          ascii_art_start:
            mov al, byte ptr ds:[si]
            xor ah, ah
            mov di, ax
            cmp di, 14
            jle insert_sign
            mov byte ptr ds:[si], '^'
            jmp loop_art

insert_sign:
            mov bh, byte ptr ds:[bp+di]
            mov byte ptr ds:[si], bh

loop_art: inc si
          loop ascii_art_start

          pop cx
          pop di
          pop bx
          pop ax
          pop si
          pop bp
          popf
          ret

CODE1 ends


STOS1 segment STACK

      dw    200   dup(?)
      top1   dw    ?

STOS1 ends

end START
