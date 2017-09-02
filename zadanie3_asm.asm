
.387
DATA1 segment

ARGS db 200 dup(0) ; pierwsza pozycja ilosc argumentow, 16 kolejnych dlugosc poszczegolnych, reszta argumenty
itnum db 0
len db 0

pi_rad dw 180d
delta_rad dw 60d

x dw 0
y dw 479

error db "Zle argumenty $"
success db "Poprawne argumenty$"

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
        call validate_args
        call convert_args

        call init_graphic
        call fpu_init
        call write_triangle
        ;call write_line
        ;call calsincos

        ;all loadxy
        ;call write_pixel
      mov   ah,0  ;oczekiwanie na dowolny klawisz
      int   16h

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
              popf
              pop ax
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

validate_args:
            push si
            push di
            push ax
            push bx
            push bp
            pushf


            xor cx, cx ; w cx bede trzymal laczna dlugosc argumentow

            cmp byte ptr ds:[di], 2
            jne error_exit
            inc di
            add cl, byte ptr ds:[di]
            inc di
            add cl, byte ptr ds:[di]
            add di, 15
        validate_numbers:
            cmp byte ptr ds:[di], '9'
            jg error_exit
            cmp byte ptr ds:[di], '0'
            jl error_exit
            sub byte ptr ds:[di], '0'
            inc di
            loop validate_numbers

            mov dx, offset success
            mov ah, 9
            int 21h

            popf
            pop bp
            pop bx
            pop ax
            pop di
            pop si
            ret

            error_exit:
                mov dx, offset error
                mov ah, 9
                int 21h
                mov ah, 4ch
                int 21h

convert_args:
      push di
      push cx
      push ax
      push bx
      pushf

      inc di
      mov al, byte ptr ds:[di] ; w al dlugosc pierwszego argumentu
      inc di
      mov ah, byte ptr ds:[di] ; w ah dlugosc drugiego argumentu

      add di, 15
      mov bl, al ; bl dlugosc pierwszego
      call convert_one_arg ; w bh otrzymam argument jako liczbe
      mov byte ptr ds:[itnum], bh
      mov bl, ah ; w bl dlugosc drugiego argumentu
      xor ah, ah ; al == ah
      add di, ax ; do di dodaje dlugosc pierwszego
      call convert_one_arg ;
      mov byte ptr ds:[len], bh



      popf
      pop bx
      pop ax
      pop cx
      pop di


convert_one_arg:
      push di
      push ax
      ; w bl dlugosc argumentu, w ds:[di] poczatek , zwracam w bh
      xor bh, bh
      cmp bl, 1
      je convert_1
      cmp bl, 2
      je convert_2
      jmp convert_3


      convert_1:
            mov bh, byte ptr ds:[di]
            pop ax
            pop di
            ret
      convert_2:
            mov al, 10
            mul byte ptr ds:[di]
            mov byte ptr ds:[di], al
            add bh, byte ptr ds:[di]
            inc di
            add bh, byte ptr ds:[di]
            pop ax
            pop di
            ret
      convert_3:
            mov al, 100
            mul byte ptr ds:[di]
            mov byte ptr ds:[di], al
            add bh, byte ptr ds:[di]
            inc di
            mov al, 10
            mul byte ptr ds:[di]
            mov byte ptr ds:[di], al
            add bh, byte ptr ds:[di]
            inc di
            add bh, byte ptr ds:[di]
            pop ax
            pop di
            ret


init_graphic:
      push ax
      xor ah, ah
      mov al, 11h
      int 10h
      pop ax
      ret


write_pixel: ; na pozycji x,y rysuje jeden pixel
      push ax
      push bx
      push cx
      push dx
      mov al, 1
      mov ah, 0Ch
      mov cx, word ptr ds:[x]
      mov dx, word ptr ds:[y]
      xor bh, bh
      int 10h
      pop dx
      pop cx
      pop bx
      pop ax
      ret


fpu_init:
      finit ; inicjalizacja koprocesora
      fild word ptr ds:[delta_rad]
      fild word ptr ds:[pi_rad]
      fdivp st(1), st(0); dziele 60/180 i mam to w st(0)
      fldpi
      fmulp st(1), st(0) ;mnoze st(1)*st(0) i sciagam st(0)
      fldz

      fild word ptr ds:[y]
      fild word ptr ds:[x]
      ; st(0) = x
      ; st(1) = y
      ; st(2) = 0  aktualny kierunek 'zolwia'
      ; st(3) = pi/3 kat ktory bede odejmowal/dodawał
      ret


write_triangle:
      push bx
      push ax
      xor bx, bx
      mov bl, byte ptr ds:[itnum]
      inc bl
      mov al, bl
      shr al, 1
      jc write_a
      call write_b_rec
      pop ax
      pop bx
      ret
      write_a:
      call write_a_rec
      pop ax
      pop bx
      ret


write_a_rec:
      push bx
      dec bl
      cmp bl, 0
      je a_rec_end

      call write_b_rec
      call rotate_right
      call write_a_rec
      call rotate_right
      call write_b_rec

      pop bx
      ret

      a_rec_end:
            call write_line
            pop bx
            ret


write_b_rec:
      push bx
      dec bl
      cmp bl, 0
      je b_rec_end

      call write_a_rec
      call rotate_left
      call write_b_rec
      call rotate_left
      call write_a_rec

      pop bx
      ret

      b_rec_end:
          call write_line
          pop bx
          ret

write_line:
      push cx
      xor cx, cx
      mov cl, byte ptr ds:[len]
      call calsincos
      write_line_start:
            call addsincos
            call loadxy
            call write_pixel
      loop write_line_start
      call delsincos
      pop cx
      ret



calsincos: ; x, y, kat, pi
      fldz ; 0, x, y, kat, pi
      fadd st(0), st(3) ; kat, x, y, kat, pi
      fsin ; sin, x, y, kat, pi
      fldz ; 0, sin, x, y, kat, pi
      fadd st(0), st(4) ; kat, sin, x , y, kat, pi
      fcos ; cos, sin, x, y, kat, pi
              ; st(0) = cosinus
              ; st(1) = sinus
      ret


addsincos: ; cos, sin, x, y, kat, pi/3
      fxch  st(2) ; x, sin, cos, y, kat, pi/3
      fadd st(0), st(2) ; x+cos, sin, cos, y, kat, pi/3
      fxch st(2) ; cos, sin, x, y
      fxch st(3) ; y, sin, x, cos
      fsub st(0), st(1) ; y-sin, sin, x, cos
      fxch st(3) ; cos, sin, x, y
      ret

loadxy: ; cos, sin, x, y, kat, pi/3
      fxch  st(2) ; x, sin, cos, y, kat, pi/3
      fist word ptr ds:[x]
      fxch  st(2) ; cos, sin, x , y, kat, pi/3
      fxch  st(3) ; y, sin, x, cos, kat, pi/3
      fist word ptr ds:[y]
      fxch  st(3) ; cos, sin, x, y, kat, pi/3
      ret



delsincos:
      fstp st(0)
      fstp st(0)
      ret

rotate_left: ; do aktualnego kata dodaje pi/3
      fxch st(2)
      fadd st(0), st(3)
      fxch st(2)
      ret

rotate_right: ; od aktualnego kata odejmuje pi/3
      fxch st(2)
      fsub st(0), st(3)
      fxch st(2)
      ret

CODE1 ends


STOS1 segment STACK

      dw    4000   dup(?)
      top1   dw    ?

STOS1 ends

end START
