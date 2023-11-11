TITLE MASM32_SnakeGame

; lib
INCLUDE Irvine32.inc
INCLUDE Macros.inc
INCLUDELIB user32.lib

; define
VK_LEFT		EQU		000000025h
VK_UP		EQU		000000026h
VK_RIGHT	EQU		000000027h
VK_DOWN		EQU		000000028h
maxCol      EQU     60
maxRow      EQU     20
maxWait     EQU     20

GetKeyState PROTO, nVirtKey:DWORD

;.STACK 4096
.STACK 4096

; Position <Struct>
Position STRUCT
    x BYTE   10D
    y BYTE   20D
Position ENDS

.data
    ; Object
    food        Position  <>
    address     Position  <>
    ; Flag
    _direction  BYTE    4
    _wait       BYTE    0
    _round      DWORD   0
    _keyed      BYTE    0
    eated       BYTE    0
    ; Position
    newX        BYTE    0
    newY        BYTE    0
    ; Count
    score       DWORD   0
    ; Text
    scoreText   BYTE    "Score:" , 0
    loseText    BYTE    "You are Loser!" , 0

.code
; draw the map edge
drawEdge PROC

    ;------------------- 4 corner
    MOV  AL, '+'    
    MOV  DH, 0          ; row = 0 ,        col = 0 
    MOV  DL, 0          ; leftUpper
    CALL Gotoxy         ;
    CALL WriteChar      ;

    MOV  DL, maxCol     ; row = 0 ,        col = maxCol+1
    INC DL              ; rightUpper
    CALL Gotoxy         ;
    CALL WriteChar      ;

    MOV  DH, maxRow     ; row = maxRow+1 , col = maxCol+1
    INC DH              ; rightLower
    CALL Gotoxy         ;
    CALL WriteChar      ;

    MOV  DL, 0          ; row = maxRow+1 , col = 0
    CALL Gotoxy         ; leftLower
    CALL WriteChar      ;
    
    ;------------------- horizontal edge
    MOV  AL, '-'  

    MOV  DH, 0          ; top
    MOV  DL, 1          ;
    CALL Gotoxy         ;
    MOV  ECX, maxCol    ;
    L1:                 ;
        CALL WriteChar  ;
        LOOP L1         ;

    MOV  DH, maxRow     ; bottom
    INC  DH             ;
    MOV  DL, 1          ;
    CALL Gotoxy         ;
    MOV  ECX, maxCol    ;
    L2:                 ;
        CALL WriteChar  ;
        LOOP L2         ;
        
    ;------------------- vertical edge
    MOV  AL, '|'   
    
    MOV  DH, 0          ; lift
    MOV  ECX, maxRow    ;
    L3:                 ;
        INC DH          ;
        MOV  DL, 0      ;
        CALL Gotoxy     ;
        CALL WriteChar  ;
        LOOP L3         ;

    MOV  DH, 0          ; right
    MOV  ECX, maxRow    ;
    L4:                 ;
        INC DH          ;
        MOV  DL, maxCol ;
        INC DL          ;
        CALL Gotoxy     ;
        CALL WriteChar  ;
        LOOP L4         ;

    RET
drawEdge ENDP

; Create new food
newFood PROC  sBottom:DWORD, sTop:DWORD, jmpAddress:DWORD ; パ┕オ块把计
LOCAL   same:BYTE
    ADD sTop , 4

    L1:
        ; get random position
        CALL randomize
        MOV  EAX , maxCol
        CALL RandomRange
        MOV food.x , AL

        CALL randomize
        MOV  EAX , maxRow
        CALL RandomRange
        MOV food.y , AL

        MOV  DL, food.x     ; column
        MOV  DH, food.y     ; row
        INC  DL
        INC  DH
        CALL Gotoxy         ; Change position according to new input
        MOV  AL, 'X'          
        CALL WriteChar      ; Write point on new place

        ; check the food no overlapping to snake body
        MOV EAX , sBottom   ; calculate STACK_lenght
        SUB EAX , sTop      ; Subtract STACK_TOP from STACK_BOTTOM
        SUB EAX , 12        ; Since the height of STACK_BOTTOM will be 12 more, subtract 12 more to get BIT_lenght

        MOV EDX , 0         ; Clear the remainder first to avoid Overflow
        MOV EBX , 2         ; byte to index
        DIV EBX             ;

        MOV ECX , EAX       ; login counter
        L2:             
            MOV EDI , sTop  ; login Locator at Stack head
            MOV EAX , ECX   ; get count
            DEC EAX         ; [0 ~ n] 
            MOV EBX , 2     ;
            MUL EBX         ; index to byte

            MOV AX, [EDI+EAX]     ; get STACK_Value and move
            ; if overlapping. reCreate
            .IF food.x == AL && food.y == AH
                MOV same, 1 ; reCreate flag set true
            .ENDIF
        LOOP L2

        ; goBack reCreate food
        .IF same == 1
            MOV same, 0
            JMP L1
        .ENDIF

    RET
newFood ENDP

; that snake move at Stack
moveSnake PROC  sBottom:DWORD, sTop:DWORD, jmpAddress:DWORD ; パ┕オ块把计
LOCAL displacement:DWORD
    ADD sTop , 4
    ; moving body with position
    MOV EAX , sBottom   ; calculate STACK_lenght
    SUB EAX , sTop      ; Subtract STACK_TOP from STACK_BOTTOM
    SUB EAX , 12        ; Since the height of STACK_BOTTOM will be 12 more, subtract 12 more to get BIT_lenght

    MOV EDX , 0         ; Clear the remainder first to avoid Overflow
    MOV EBX , 2         ; byte to index
    DIV EBX             ;

    MOV ECX , EAX       ; login counter
    .IF ECX >= 1
        SUB EAX , 1     ; skip snake head
        L2:             
            MOV EDI , sTop      ; login Locator at Stack head
            MOV EAX , ECX       ; get count
            DEC EAX             ; [0 ~ n] 
            MOV EBX , 2         ;
            MUL EBX             ; index to byte

            MOV displacement , EAX     ; get STACK_Value and move
            MOV AX , [EDI+EAX]
            ADD EDI , displacement
            MOV [EDI+2] , AX

            LOOP L2
    .ENDIF

    ; get now the head position and to move 1 step
    MOV EDI , sTop
    MOV AX , [EDI]
    ; up
    .IF _direction == 0
        .IF AH == 0
            mov AH , maxRow
        .ENDIF 
        DEC AH
    .ENDIF 
    ; right
    .IF _direction == 1
        INC AL
        .IF AL == maxCol
            mov AL , 0
        .ENDIF  
    .ENDIF  
    ; down
    .IF _direction == 2
        INC AH
        .IF AH == maxRow
            mov AH , 0
        .ENDIF
    .ENDIF   
    ; left
    .IF _direction == 3
        .IF AL == 0
            mov AL , maxCol
        .ENDIF  
        DEC AL
    .ENDIF  
    ; updata new snake head
    MOV [EDI] , AX
        
    RET
moveSnake ENDP

; change direction
changeDirection PROC sTop:DWORD, jmpAddress:DWORD
    ADD sTop , 4
    MOV EDI, sTop
    MOV AX , [EDI]
    MOV address.x, AL
    MOV address.Y, AH
    
    ; down
    mov AH, 0
    INVOKE GetKeyState, VK_DOWN
    .IF AH && _keyed == 0 && _direction != 0
        MOV _keyed , 1
        MOV _direction , 2
        MOV EAX,'v'
  	.ENDIF
    ; up
	INVOKE GetKeyState, VK_UP
   .IF AH && _keyed == 0 && _direction != 2
        MOV _keyed , 1
        MOV _direction , 0
        MOV EAX,'^'
    .ENDIF     
    ; left
    INVOKE GetKeyState, VK_LEFT
    .IF AH && _keyed == 0 && _direction != 1
        MOV _keyed , 1
        MOV _direction , 3
        MOV EAX,'<'
    .ENDIF  
    ; right
    INVOKE GetKeyState, VK_RIGHT
    .IF AH && _keyed == 0 && _direction != 3
        MOV _keyed , 1
        MOV _direction , 1
        MOV EAX,'>' 
	.ENDIF   
        
    RET
changeDirection ENDP

; Drawing using snake position in Std
drawInStd PROC char:DWORD, sIndex:DWORD, jmpAddress:DWORD
    ADD sIndex, 4
    ; get position
    MOV EDI, sIndex
    MOV AX , [EDI]
    ; Move cursor
    MOV address.x, AL
    MOV address.Y, AH
    MOV DH, address.y
    MOV DL, address.x
    INC DH
    INC DL
    CALL Gotoxy
    ; Draw char
    MOV EAX , char
    CALL WriteChar

    RET
drawInStd ENDP

; main
main PROC

    ; init snake position and push to Stack
    CALL randomize
    MOV  EAX , maxCol
    CALL RandomRange
    MOV address.x , AL

    CALL randomize
    MOV  EAX , maxRow
    CALL RandomRange
    MOV address.y , AL

    PUSH address

    ; init snake direction
    CALL randomize
    MOV  EAX , 4                  ;Keeps the range 0 - 9
    CALL RandomRange
    MOV _direction , AL

    ; call create new food
    PUSH OFFSET newFood   ; jmpAddress  
    PUSH ESP              ; sTop 
    PUSH EBP              ; sBottom
    CALL newFood

    ; draw map edge
    CALL drawEdge
    
    ; print score text
    MOV DH, 22
    MOV DL, 10
    CALL Gotoxy
    MOV EDX , OFFSET scoreText
    CALL WriteString
    ; print score
    MOV DH, 22
    MOV DL, 17
    CALL Gotoxy
    MOV EAX ,score
    CALL WriteDec

    ; game loop
    looop:
        ; Have not changed direction yet
        ; To avoid conflicts,
        ; you can only change direction once before taking a step
        .IF _keyed == 0                 ;
            PUSH OFFSET changeDirection ; jmpAddress
            PUSH ESP                    ; sTop 
            CALL changeDirection        ;
        .ENDIF

        loopSleep:                  ; Timer
            INC _round              ;
            .IF _round < 262143     ;
                JMP loopSleep       ;
	        .ENDIF                  ;
            .IF _round == 262143    ;
                MOV _round , 0      ;
	        .ENDIF                  ;

        ; Refresh one frame after every 255 detections
        INC _wait
        .IF _wait == 255
            MOV _keyed , 0          ; reload variable
            MOV _wait , 0           ;
            MOV eated , 0           ;

            ; call output std draw snake head. Before moving
            PUSH OFFSET drawInStd   ; jmpAddress
            PUSH ESP                ; sTop
            .IF _direction == 0     ; char
                PUSH '^'            ;
            .ENDIF                  ;
            .IF _direction == 1     ;
                PUSH '>'            ;
            .ENDIF                  ;
            .IF _direction == 2     ;
                PUSH 'v'            ;
            .ENDIF                  ;
            .IF _direction == 3     ;
                PUSH '<'            ;
            .ENDIF                  ;
            CALL drawInStd          

            ; Determine what is in front of the head
            MOV AX , [ESP]              ; get snake head position
            MOV newX , AL
            MOV newY , AH               
            .IF _direction == 0         ; ^
                .IF newY == 0           ;
                    MOV newY , maxRow   ;
                .ENDIF                  ;
                DEC newY                ;
            .ENDIF                      ;

            .IF _direction == 1         ; >
                INC newX                ;
                .IF newX == maxCol      ;
                    MOV newX , 0        ;
                .ENDIF                  ;
            .ENDIF                      ;

            .IF _direction == 2         ; v
                INC newY                ;
                .IF newY == maxRow      ;
                    MOV newY , 0        ;
                .ENDIF                  ;
            .ENDIF                      ;

            .IF _direction == 3         ; <
                .IF newX == 0           ;
                    MOV newX , maxCol   ;
                .ENDIF                  ;
                DEC newX                ;
            .ENDIF                      ;

            ; Determine whether the body has been hit
            MOV EAX , EBP   ; calculate STACK_lenght
            SUB EAX , ESP   ; Subtract STACK_TOP from STACK_BOTTOM
            SUB EAX , 12    ; Since the height of STACK_BOTTOM will be 12 more, subtract 12 more to get BIT_lenght

            MOV EDX, 0      ; Clear the remainder first to avoid Overflow
            MOV EBX, 2      ; byte to index
            DIV EBX         ;

            ; Stack not only snake head
            .IF EAX > 0
                MOV ECX , EAX   ; login counter
                MOV EDI , ESP   ; login Locator at Stack head
                SUB EAX , 1     ; skip snake head
                ADD EDI , 2     ; skip snake head
                L1:             
                    MOV EAX , ECX   ; get count
                    DEC EAX         ; [0 ~ n]
                    MOV EBX , 2     ; 
                    MUL EBX         ; index to byte

                    MOV AX, [EDI+EAX]      ; get STACK_Value
                    ; if hit self body
                    .IF AL == newX && AH == newY
                        MOV DH, 22
                        MOV DL, 0
                        CALL Gotoxy
                        ; go lose process
                        JMP lose
                    .ENDIF
                    LOOP L1
            .ENDIF

            ; Turn the snake head into a body
            ; call output std
            PUSH OFFSET drawInStd   ; jmpAddress  
            PUSH ESP                ; sTop
            PUSH 'O'                ; char
            CALL drawInStd

            ; Determine if food has been eaten
            MOV AL , newX
            MOV AH , newY
            .IF AL == food.x && AH == food.y    ; if eaten
                MOV address.x , AL
                MOV address.y , AH
                PUSH address    ; push a new snake point in stack head
                MOV eated , 1   ; set flag been true
            .ENDIF
            
            ; if eated
            .IF eated == 1
                INC score             ; Scoring add 1
                ; call create new food
                PUSH OFFSET newFood   ; jmpAddress  
                PUSH ESP              ; sTop 
                PUSH EBP              ; sBottom
                CALL newFood
                ; show score
                MOV EAX , score
                MOV DH, 22
                MOV DL, 17
                CALL Gotoxy
                CALL WriteDec
            .ENDIF

            ; if no eated
            .IF eated == 0
                ; calculate Stack position
                MOV EAX , EBP
                SUB EAX , 18
                ; call output std
                PUSH OFFSET drawInStd   ; jmpAddress  
                PUSH EAX                ; sBottom
                PUSH ' '                ; char
                CALL drawInStd
                ; that snake move at Stack
                PUSH OFFSET moveSnake   ; jmpAddress   
                PUSH ESP                ; sTop 
                PUSH EBP                ; sBottom
                CALL moveSnake
            .ENDIF

            ; call output std draw snake head. After moving
            PUSH OFFSET drawInStd   ; jmpAddress
            PUSH ESP                ; sTop
            .IF _direction == 0
                PUSH '^'
            .ENDIF
            .IF _direction == 1
                PUSH '>'
            .ENDIF
            .IF _direction == 2
                PUSH 'v'
            .ENDIF
            .IF _direction == 3
                PUSH '<'
            .ENDIF
            CALL drawInStd
	    .ENDIF   

        ; place the cursor outside the map
        MOV DH, 22
        MOV DL, 0
        CALL Gotoxy

    ; loop game
    JMP looop

    ; show lose title
    lose:
        MOV DH, 22
        MOV DL, 38
        CALL Gotoxy
        MOV EDX , OFFSET loseText
        CALL WriteString

    ; Prevent users from exiting the program due to accidentally pressing keys
    ; and place the cursor outside the map
    endloop:
        MOV DH, 22
        MOV DL, 0
        CALL Gotoxy
    JMP endloop

    EXIT
main ENDP
END main