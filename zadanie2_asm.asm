DATA1 segment

ARGS db 200 dup(0)
handler1 dw ?
handler2 dw ?
ok db 'Udalo sie otworzyc plik$'
error db 'Wystapil blad$'
invalid_arg_number db 'Nieprawidlowa ilosc argumentow$'
cant_overwrite db 'Nie mozna nadpisac/stworzyc piku wyjsciowego$'
invalid_syntax db 'Zla skladania przy wywolaniu$'
invalid_first db 'Nie mozna otworzyc pierwszego pliku$'
cant_get_char db 'Nie mozna zaladowac danych do bufora wejsciowego$'
cant_decompress db 'Nie mozna zdekompresowac pliku'
input_buffor db 16000 dup(?)
output_buffor db 16000 dup(?)
input_pointer dw 16000
output_pointer dw 0
bytes_saved dw 16000


DATA1 ends

CODE1 segment

START:

  mov ax, seg top1
  mov ss, ax
  mov sp, offset top1 ; ustawiam stos

  mov ax, ds
  mov es, ax ; linia komend w es

  mov si, 82h ; poczatek linii argumentow w si

  mov ax, seg ARGS
  mov ds, ax ; segment danych w ds

  mov di, offset ARGS ; poczatek tablicy w di

  call parseArguments
  ;call printargs
  call validate_args
  call is_d
  call close_files



  mov ah, 4ch
  int 21h

;##########################################################################


close_files:
      mov ah, 3eh
      mov bx, word ptr ds:[handler1]
      int 21h
      mov ah, 3eh
      mov bx, word ptr ds:[handler2]
      int 21h
      ret



fill_bufor:
        push ax
        push bx
        push cx
        push dx
        pushf
                    mov ah, 3fh ; numer przerwania
                    mov bx, word ptr ds:[handler1] ; do bx uchwyt pliku wejsciowego
                    mov cx, 16000 ; do cx ilosc bajtow do zapisania;
                    mov dx, offset input_buffor ; do dx gdzie mamy zapisac
                    int 21h
                    mov word ptr ds:[input_pointer], 0 ; wskaznik bufora wejsciowego na 0
                    mov word ptr ds:[bytes_saved], ax

        popf
        pop dx
        pop cx
        pop bx
        pop ax
        ret ; wypełnia buffor wejciowy

;###########################################################################

empty_bufor:
      pushf
      push ax
      push cx
      push dx
      push bx
                mov ah, 42h ; przesuwam wskaznik na koniec pliku
                mov cx, 0
                mov dx, 0
                mov al, 2 ; tryb zapis/odczyt
                mov bx, word ptr ds:[handler2]
                int 21h ; ustawiony wskaznik
                mov ah, 40h
                mov bx, word ptr ds:[handler2]
                mov cx, word ptr ds:[output_pointer]
                mov dx, offset output_buffor
                int 21h
                mov word ptr ds:[output_pointer], 0

      pop bx
      pop dx
      pop cx
      pop ax
      popf
      ret; zapisuje bajty z bufora wyjsciowego do pliku, opróżnia bufor wyjsciowy


;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

  validate_args:


          mov si, di
          cmp byte ptr [di], 2 ; sprawdzam ile argumentow
          je two_args ; jesli dwa to sprawdzam czy poprawne
          cmp byte ptr [di], 3
          je three_args
          mov ah, 9
          mov dx, offset invalid_arg_number
          int 21h
          ret

                two_args:
                add si, 17 ; poczaetek pierwszego argumentu w si
                mov cx, 0 ; cx jako flaga
                push di
                    mov di, offset handler1 ; w 'handler1' zapisze uchwyt pierwszego pliku
                    mov dx, si ; dx poczatek argumentu
                    call open_file ; probuje otworzyc plik
                    cmp cx, 0 ; jesli flaga = 0 to nie otworzylem
                    je first_arg_error
                    mov cx, 0 ; ustawiam flage spowrotem
                    sub si, 16 ; si pokazuje dlugosc pierwszego argumentu
                    mov al, byte ptr [si] ; ax = dlugosc pierwszego argumentu
                    xor ah, ah
                    add si, ax ; do si dodaje dlugosc pierwszego i 16, wiec teraz wskazuje na poczatek drugiego argumentu
                    add si, 16
                    mov di, offset handler2
                    mov dx, si ; dx = poczatek argumentu drugiego
                    call overwrite_file
               pop di
               ret


              three_args:
              push di
              inc si

              cmp byte ptr [si], 3 ; sprawdzam czy pierwszy argument ma 3 znaki (-d + 0 ktore dodal parser)
              jne syntax_error

              add si, 16 ; sprawdzam czy peirwszy argument to -d
              cmp word ptr [si], 'd-' ; 'd-' poniewaz jako slowo jest na odwrot
              jne syntax_error


              add si, 3 ; poczatek drugiego argumentu
              mov cx, 0
              mov di, offset handler1 ; w tym miejscu zapisze uchwyt pierwszego pliku
              mov dx, si ; poczatek drugiego arg w dx
              call open_file ; probuje otworzyc
              cmp cx, 0
              je first_arg_error

              sub si, 18 ; teraz si wskazuje dlugosc druiego argumentu
              mov al, byte ptr [si]
              xor ah, ah
              add si, ax
              add si, 18
              mov di, offset handler2
              mov dx, si
              call overwrite_file



              pop di
              ret

                arg_error:
            mov ah, 9
            mov dx, offset error
            int 21h
            pop di
            ret

            first_arg_error:
            mov ah, 9
            mov dx, offset invalid_first
            int 21h
            pop di
            ret

            syntax_error:
            mov ah, 9
            mov dx, offset invalid_syntax
            int 21h
            pop di
            ret


;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4444



  open_file:
          mov ah, 3dh
          mov al, 0
          int 21h
          jc blad
          mov cx, 1
          mov word ptr ds:[di], ax
            blad:     ret

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4

  overwrite_file:
          mov ah, 3ch ; tworze nowy plik, jesli taki istnial to go nadpisze
          xor cx, cx
          int 21h
          jc overwrite_error ; jesli flaga carry to nie udalo sie otworzyc
          mov word ptr ds:[di], ax ; jesli sie udalo to zapisauje uchwyt do pamieci
          ret

      overwrite_error:
            mov ah, 9
            mov dx, offset cant_overwrite
            int 21h
            ret


;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


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
                mov byte ptr[di], 0
                inc cx
                inc di
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





  get_char:     ; pobiera bajty z bufora wejsciowego do AL, jesli buffor przepełniony to go ładuje
                mov si, word ptr ds:[input_pointer]

                cmp si, word ptr ds:[bytes_saved]
                jne get_char_continue

                cmp si, 16000
                jne cant_char

                call fill_bufor
        get_char_continue:

                mov si, word ptr ds:[input_pointer]
                mov al, byte ptr ds:[input_buffor+si]
                mov dl, 0
                inc word ptr ds:[input_pointer]

                ret

        cant_char:

                mov dl, 1
                ret

  ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ;________________________________________________________________________
  ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  put_char: ; zapisuje AL do bufora, jesli trzeba to zwalnia ten bufor
            push cx
                mov si, word ptr ds:[output_pointer]

                cmp si, 16000
                jne put_char_continue

                call empty_bufor

      put_char_continue:

                mov si, word ptr ds:[output_pointer]
                mov byte ptr ds:[output_buffor+si], al
                inc word ptr ds:[output_pointer]
                pop cx
                ret



;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;________________________________________________________________________
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  compress:
            call get_char
            mov cx, 1

              compress_start:
                    mov ah, al
                    call get_char

                    cmp dl, 1
                    je compress_end

                    cmp ah, 0
                          je compress_0

                    cmp al, ah
                          je compress_continue

    compress_new_byte:       call new_byte
                    mov cx, 1
                    jmp compress_start

      compress_continue:
            cmp cx, 255
            je compress_new_byte
            inc cx
            jmp compress_start

      compress_0:
            push ax
                      mov al, 0
                      call put_char
                      mov al, 0
                      call put_char
                      mov cx, 1
            pop ax
            jmp compress_start

      compress_end:
            cmp ah, 0
            je compress_end_0


            call new_byte
            call empty_bufor
            ret

      compress_end_0:
            mov al, 0
            call put_char
            call put_char
            call empty_bufor
            ret



new_byte:
      push ax
      push cx
      push bx
      push dx
      pushf

          cmp cx, 0
          je new_byte_end
          cmp cx, 3
          jle new_byte_loop

                        mov al, 0
                        call put_char
                        mov al, cl
                        call put_char
                        mov al, ah
                        call put_char
                        jmp new_byte_end

              new_byte_loop:
                mov al, ah
                call put_char
                loop new_byte_loop

      new_byte_end:
      popf
      pop dx
      pop bx
      pop cx
      pop ax
      ret



decompress:


            decompress_start:
                call get_char
                cmp dl, 1
                je decompress_end




                cmp al, 0
                je byte_0

                call put_char
                jmp decompress_start



            byte_0:
                call get_char
                cmp dl, 1
                je decompress_error
                cmp al,0
                  jne decompress_continue
                mov al, 0
                call put_char
                jmp decompress_start
            decompress_continue:
                xor cx, cx
                mov cl, al
                call get_char
                cmp dl, 1
                je decompress_error
                decompress_loop:
                    call put_char
                    loop decompress_loop
                jmp decompress_start

            decompress_end:
                 call empty_bufor
                 ret


      decompress_error:
            mov ah, 9
            mov dx, offset cant_decompress
            int 21h
            jmp decompress_end


is_d:
    mov di, offset ARGS
    cmp byte ptr ds:[di], 2
    je comp
    call decompress
    ret
    comp:
    call compress
    ret


CODE1 ends


STOS1 segment STACK

dw    200   dup(?)
top1  dw    ?

STOS1 ends

end START
