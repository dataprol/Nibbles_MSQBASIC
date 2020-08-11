'$INCLUDE: 'sblaster.bi'
'
'                         Q B a s i c   N i b b l e s
'
'                   Copyright (C) Microsoft Corporation 1990
'
' Nibbles is a game for one or two players.  Navigate your snakes
' around the game board trying to eat up numbers while avoiding
' running into walls or other snakes.  The more numbers you eat up,
' the more points you gain and the longer your snake becomes.
'
' To run this game, press Shift+F5.
'
' To exit QBasic, press Alt, F, X.
'
' To get help on a BASIC keyword, move the cursor to the keyword and press
' F1 or click the right mouse button.
'

'Set default data type to integer for faster game play
DEFINT A-Z

'User-defined TYPEs
TYPE snakeBody
    row AS INTEGER
    col AS INTEGER
END TYPE

'This type defines the player's snake
TYPE snaketype
    head      AS INTEGER
    length    AS INTEGER
    row       AS INTEGER
    col       AS INTEGER
    direction AS INTEGER
    lives     AS INTEGER
    score     AS INTEGER
    scolor    AS INTEGER
    alive     AS INTEGER
END TYPE

'This type is used to represent the playing screen in memory
'It is used to simulate graphics in text mode, and has some interesting,
'and slightly advanced methods to increasing the Speed! of operation.
'Instead of the normal 80x25 text graphics using chr$(219) "€", we will be
'using chr$(220)"‹" and chr$(223) "ﬂ" and chr$(219) "€" to mimic an 80x50
'pixel screen.
'Check out sub-programs SET and POINTISTHERE to see how this is implemented
'feel free to copy these (as well as arenaType and the DIM ARENA stmt and the
'initialization code in the DrawScreen subprogram) and use them in your own
'programs
TYPE arenaType
    realRow     AS INTEGER        'Maps the 80x50 point into the real 80x25
    acolor      AS INTEGER        'Stores the current color of the point
    sister      AS INTEGER        'Each char has 2 points in it.  .SISTER is
END TYPE                          '-1 if sister point is above, +1 if below

'Sub Declarations
DECLARE SUB SpacePause (text$)
DECLARE SUB PrintScore (NumPlayers%, score1%, score2%, lives1%, lives2%)
DECLARE SUB Intro ()
DECLARE SUB GetInputs (NumPlayers, Speed!, diff$, monitor$)
DECLARE SUB DrawScreen ()
DECLARE SUB PlayNibbles (NumPlayers, Speed!, diff$)
DECLARE SUB Set (row, col, acolor)
DECLARE SUB Center (row, text$)
DECLARE SUB DoIntro ()
DECLARE SUB Initialize ()
DECLARE SUB SparklePause ()
DECLARE SUB Level (WhatToDO, Sammy() AS snaketype)
DECLARE SUB InitColors ()
DECLARE SUB EraseSnake (snake() AS ANY, snakeBod() AS ANY, snakeNum%)
DECLARE FUNCTION StillWantsToPlay ()
DECLARE FUNCTION PointIsThere (row, col, backColor)

'Constants
CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST MAXSNAKELENGTH = 1000
CONST STARTOVER = 1             ' Parameters to 'Level' SUB
CONST SAMELEVEL = 2
CONST NEXTLEVEL = 3

'Global Variables
DIM SHARED arena(1 TO 50, 1 TO 80) AS arenaType
DIM SHARED CurLevel, colorTable(10)

    GOSUB ClearKeyLocks
    RANDOMIZE TIMER
    
    ON KEY(10) GOSUB Sair
    KEY(10) ON

    Intro
    GetInputs NumPlayers, Speed!, diff$, monitor$
    GOSUB SetColors
    DrawScreen

    DO
      PlayNibbles NumPlayers, Speed!, diff$
    LOOP WHILE StillWantsToPlay

    GOSUB RestoreKeyLocks
    COLOR 15, 0
    CLS
END

ClearKeyLocks:
    DEF SEG = 0                     ' Turn off CapLock, NumLock and ScrollLock
    KeyFlags = PEEK(1047)
    POKE 1047, &H0
    DEF SEG
    RETURN

RestoreKeyLocks:
    DEF SEG = 0                     ' Restore CapLock, NumLock and ScrollLock states
    POKE 1047, KeyFlags
    DEF SEG
    RETURN

SetColors:
    IF monitor$ = "M" THEN
        RESTORE mono
    ELSE
        RESTORE normal
    END IF

    FOR a = 1 TO 6
        READ colorTable(a)
    NEXT a
    RETURN

           'snake1     snake2   Walls  Background  Dialogs-Fore  Back
mono:   DATA 15,         7,       7,     0,          15,            0
normal: DATA 12,         13,      14,    1,          15,            4
END

Sair:
  PCOPY 0, 1
  CLS

  Center 5, "N I B B L E S"
  Center 12, "Deseja sair do jogo? (S ou N)"

  DO
      I$ = INKEY$
      IF UCASE$(I$) = "S" THEN
         COLOR 7, 0
         CLS
         GOSUB RestoreKeyLocks
         END
      ELSEIF UCASE$(I$) = "N" THEN
         PCOPY 1, 0
         RETURN
      END IF
  LOOP

'Center:
'  Centers text on given row
SUB Center (row, text$)
    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;
END SUB

'DrawScreen:
'  Draws playing field
SUB DrawScreen ()

    'initialize screen
    VIEW PRINT
    COLOR colorTable(1), colorTable(4)
    CLS

    'Print title & message
    Center 1, "Nibbles!"
    Center 11, "Aguarde um momento..."
    
    'Initialize arena array
    FOR row = 1 TO 50
        FOR col = 1 TO 80
            arena(row, col).realRow = INT((row + 1) / 2)
            arena(row, col).sister = (row MOD 2) * 2 - 1
        NEXT col
    NEXT row
END SUB

'EraseSnake:
'  Erases snake to facilitate moving through playing field
SUB EraseSnake (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)

    FOR c = 0 TO 9
        FOR b = snake(snakeNum).length - c TO 0 STEP -10
            tail = (snake(snakeNum).head + MAXSNAKELENGTH - b) MOD MAXSNAKELENGTH
            Set snakeBod(tail, snakeNum).row, snakeBod(tail, snakeNum).col, colorTable(4)
        NEXT b
    NEXT c
    
END SUB

'GetInputs:
'  Gets player inputs
SUB GetInputs (NumPlayers, Speed!, diff$, monitor$)

    COLOR 7, 0
    CLS

    DO
        LOCATE 5, 47: PRINT SPACE$(34);
        LOCATE 5, 20
        INPUT "1 ou 2 Jogadores? ", num$
    LOOP UNTIL VAL(num$) = 1 OR VAL(num$) = 2
    NumPlayers = VAL(num$)

    LOCATE 9, 22: PRINT "1 = F†cil"
    LOCATE 10, 22: PRINT "2 = Normal"
    LOCATE 11, 22: PRINT "3 = Dif°cil"
    DO
        LOCATE 8, 21: PRINT SPACE$(35);
        LOCATE 8, 21: INPUT "Dificuldade: ", gameSpeed$
    LOOP UNTIL VAL(gameSpeed$) >= 1 AND VAL(gameSpeed$) <= 3
    Speed! = VAL(gameSpeed$)

    SELECT CASE Speed!
        CASE 1
            Speed! = .2
        CASE 2
            Speed! = .1
        CASE 3
            Speed! = .01
    END SELECT

END SUB

'InitColors:
'Initializes playing field colors
SUB InitColors ()
    
    FOR row = 1 TO 50
        FOR col = 1 TO 80
            arena(row, col).acolor = colorTable(4)
        NEXT col
    NEXT row

    CLS
   
    'Set (turn on) pixels for screen border
    FOR col = 1 TO 80
        Set 3, col, colorTable(3)
        Set 50, col, colorTable(3)
    NEXT col

    FOR row = 4 TO 49
        Set row, 1, colorTable(3)
        Set row, 80, colorTable(3)
    NEXT row

END SUB

'Intro:
'  Displays game introduction
SUB Intro ()
    SCREEN 0
    WIDTH 80, 25
    COLOR 11, 0
    PALETTE 3, 33
    PALETTE 11, 22
    CLS


    Center 3, "N i b b l e s"
    COLOR 15
    Center 5, "Copyright (C) Microsoft Corporation 1990"
    Center 6, "1997 Luiz Carlos Costa Rodrigues"
    COLOR 3
    Center 8, "Nibbles Ç jogo para um ou dois jogadores. Navegando sua cobra"
    Center 9, "em todas as direá‰es de um plano tentando comer n£meros enquanto"
    Center 10, "esqu°va-se rapidamente de muros ou outra cobra. Quanto mais n£meros"
    Center 11, "vocà come, mais pontos vocà ganha e maior se torna sua cobra."
    COLOR 11
    Center 14, "Controles do Jogo:"
    Center 16, "  Geral               Jogador 1              Jogador 2   "
    Center 18, "P - Pausar               " + CHR$(24) + "                      W       "
    Center 19, "F10 Sair               " + CHR$(27) + "   " + CHR$(26) + "                  A   D     "
    Center 20, "                         " + CHR$(25) + "                      S       "
    COLOR 14
    Center 24, "Para sair tecle F10."
    COLOR 3
    Center 25, "Pressione qualquer tecla para comeáar..."

    PLAY "MBT160O1L8CDEDCDL4ECC"
    SparklePause

END SUB

'Level:
'Sets game level
STATIC SUB Level (WhatToDO, Sammy() AS snaketype)

    PALETTE 12, 59
    PALETTE 13, 44

    SELECT CASE (WhatToDO)

        CASE STARTOVER
            CurLevel = 1
            TempoDecorrido# = TIMER
            ScoreVidas = 45


        CASE NEXTLEVEL
            IF TIMER - TempoDecorrido# < 24 THEN
                Sammy(1).lives = Sammy(1).lives + 1
                Sammy(2).lives = Sammy(2).lives + 1
                Sammy(1).score = Sammy(1).score + ScoreVidas / 2
                Sammy(2).score = Sammy(2).score + ScoreVidas / 2
                ScoreVidas = ScoreVidas + ScoreVidas * 2
                Sammy(1).score = Sammy(1).score + ScoreVidas
                Sammy(2).score = Sammy(2).score + ScoreVidas
                SBTom 200, 2
                SBTom 500, 2
                SBTom 800, 2
                SpacePause "   Vocà ganhou uma vida!"

            ELSEIF Sammy(1).score >= ScoreVidas OR Sammy(2).score >= ScoreVidas THEN
                Sammy(1).lives = Sammy(1).lives + 1
                Sammy(2).lives = Sammy(2).lives + 1
                ScoreVidas = ScoreVidas + ScoreVidas * 2
                SBTom 100, 2
                SBTom 400, 2
                SBTom 800, 2
                SpacePause "   Vocà ganhou uma vida!"
            END IF
            CurLevel = CurLevel + 1
            TempoDecorrido# = TIMER
            
    END SELECT

    Sammy(1).head = 1                       'Initialize Snakes
    Sammy(1).length = 2
    Sammy(1).alive = TRUE
    Sammy(2).head = 1
    Sammy(2).length = 2
    Sammy(2).alive = TRUE

    InitColors
    
    SELECT CASE CurLevel
    CASE 1
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 4: Sammy(2).direction = 3
        PALETTE 1, 1
        
    CASE 2
        FOR I = 20 TO 60
            Set 25, I, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 60: Sammy(2).col = 20
        Sammy(1).direction = 3: Sammy(2).direction = 4
        PALETTE 1, 16
        
    CASE 3
        FOR I = 10 TO 40
            Set I, 20, colorTable(3)
            Set I, 60, colorTable(3)
        NEXT I
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 1: Sammy(2).direction = 2
        PALETTE 1, 8
        
    CASE 4
        FOR I = 4 TO 30
            Set I, 20, colorTable(3)
            Set 53 - I, 60, colorTable(3)
        NEXT I
        FOR I = 2 TO 40
            Set 38, I, colorTable(3)
            Set 15, 81 - I, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 60: Sammy(2).col = 20
        Sammy(1).direction = 3: Sammy(2).direction = 4
        PALETTE 1, 33
   
    CASE 5
        FOR I = 13 TO 39
            Set I, 21, colorTable(3)
            Set I, 59, colorTable(3)
        NEXT I
        FOR I = 23 TO 57
            Set 11, I, colorTable(3)
            Set 41, I, colorTable(3)
        NEXT I
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 1: Sammy(2).direction = 2
        PALETTE 1, 13

    CASE 6
        FOR I = 4 TO 49
            IF I > 30 OR I < 23 THEN
                Set I, 10, colorTable(3)
                Set I, 20, colorTable(3)
                Set I, 30, colorTable(3)
                Set I, 40, colorTable(3)
                Set I, 50, colorTable(3)
                Set I, 60, colorTable(3)
                Set I, 70, colorTable(3)
            END IF
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 42

    CASE 7
        FOR I = 4 TO 49 STEP 2
            Set I, 40, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 9

    CASE 8
        FOR I = 4 TO 40
            Set I, 10, colorTable(3)
            Set 53 - I, 20, colorTable(3)
            Set I, 30, colorTable(3)
            Set 53 - I, 40, colorTable(3)
            Set I, 50, colorTable(3)
            Set 53 - I, 60, colorTable(3)
            Set I, 70, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 3

    CASE 9
        FOR I = 6 TO 47
            Set I, I, colorTable(3)
            Set I, I + 28, colorTable(3)
        NEXT I
        Sammy(1).row = 40: Sammy(2).row = 15
        Sammy(1).col = 75: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2
        PALETTE 1, 1
   
    CASE 10
        FOR I = 2 TO 78 STEP 3
            Set 8, I, colorTable(3)
            Set 14, I + 1, colorTable(3)
            Set 20, I, colorTable(3)
            Set 26, I + 1, colorTable(3)
            Set 32, I, colorTable(3)
            Set 38, I + 1, colorTable(3)
            Set 44, I, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 3: Sammy(2).direction = 4
        PALETTE 1, 52

    CASE 11
        FOR I = 5 TO 49 STEP 1
            Set I - 1, 10, colorTable(3)
            Set I, 20, colorTable(3)
            Set I - 1, 30, colorTable(3)
            Set I, 40, colorTable(3)
            Set I - 1, 50, colorTable(3)
            Set I, 60, colorTable(3)
            Set I - 1, 70, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 57

    CASE 12
        FOR I = 7 TO 46 STEP 1
            Set I, 12, colorTable(3)
            Set I, 68, colorTable(3)
            Set I, 40, colorTable(3)
        NEXT I
        FOR I = 14 TO 66 STEP 1
            Set 7, I, colorTable(3)
            Set 46, I, colorTable(3)
            Set 18, I, colorTable(3)
            Set 35, I, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 40

    CASE 13
        FOR I = 15 TO 38 STEP 1
            Set I, I + 13, colorTable(3)
            Set 40 - I + 13, I + 13, colorTable(3)
            Set 8, I * 2 - 13, colorTable(3)
            Set 45, I * 2 - 13, colorTable(3)
            Set I * 1.6 - 16, 17, colorTable(3)
            Set I * 1.6 - 16, 63, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 49

    CASE 14
        FOR I = 4 TO 49 STEP 2
            Set I, 10, colorTable(3)
            Set I + 1, 20, colorTable(3)
            Set I, 30, colorTable(3)
            Set I + 1, 40, colorTable(3)
            Set I, 50, colorTable(3)
            Set I + 1, 60, colorTable(3)
            Set I, 70, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 52

    CASE 15
        FOR I = 4 TO 49 STEP 2
            Set I, 10, colorTable(3)
            Set I + 1, 20, colorTable(3)
            Set I, 30, colorTable(3)
            Set I + 1, 40, colorTable(3)
            Set I, 50, colorTable(3)
            Set I + 1, 60, colorTable(3)
            Set I, 70, colorTable(3)
        NEXT I
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
        PALETTE 1, 52
        
    CASE ELSE
        COLOR colorTable(5), colorTable(6)
        Center 11, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
        Center 12, "€ ParabÇns! Vocà conseguiu chegar ao final! €"
        Center 13, "€ Este jogo foi atualizado por Luiz Carlos. €"
        Center 14, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
        WHILE INKEY$ <> "": WEND
        WHILE INKEY$ = "": WEND
        Intro
        GetInputs NumPlayers, Speed!, diff$, monitor$
        DrawScreen
        PlayNibbles NumPlayers, Speed!, diff$

    END SELECT

END SUB

'PlayNibbles:
'  Main routine that controls game play
SUB PlayNibbles (NumPlayers, Speed!, diff$)

    'Initialize Snakes
    DIM sammyBody(MAXSNAKELENGTH - 1, 1 TO 2) AS snakeBody
    DIM Sammy(1 TO 2) AS snaketype
    Sammy(1).lives = 5
    Sammy(1).score = 0
    Sammy(1).scolor = colorTable(1)
    Sammy(2).lives = 5
    Sammy(2).score = 0
    Sammy(2).scolor = colorTable(2)
                 
    Level STARTOVER, Sammy()
    startRow1 = Sammy(1).row: startCol1 = Sammy(1).col
    startRow2 = Sammy(2).row: startCol2 = Sammy(2).col

    curSpeed! = Speed!

    'play Nibbles until finished

    SpacePause "    N°vel" + STR$(CurLevel) + ", Tecle Espaáo"
    gameOver = FALSE
    DO
        IF NumPlayers = 1 THEN
            Sammy(2).row = 0
        END IF

        number = 1          'Current number that snakes are trying to run into
        nonum = TRUE        'nonum = TRUE if a number is not on the screen

        playerDied = FALSE
        PrintScore NumPlayers, Sammy(1).score, Sammy(2).score, Sammy(1).lives, Sammy(2).lives
        PLAY "T160O1>L20CDEDCDL10ECC"

        DO
            'Print number if no number exists
            IF nonum = TRUE THEN
                DO
                    numberRow = INT(RND(1) * 47 + 3)
                    NumberCol = INT(RND(1) * 78 + 2)
                    sisterRow = numberRow + arena(numberRow, NumberCol).sister
                LOOP UNTIL NOT PointIsThere(numberRow, NumberCol, colorTable(4)) AND NOT PointIsThere(sisterRow, NumberCol, colorTable(4))
                numberRow = arena(numberRow, NumberCol).realRow
                nonum = FALSE
                COLOR colorTable(1), colorTable(4)
                LOCATE numberRow, NumberCol
                PRINT RIGHT$(STR$(number), 1);
                count = 0
            END IF


            'Velocidade do jogo
            t# = TIMER
            DO WHILE TIMER - t# < curSpeed!: LOOP
            
            'Get keyboard input & Change direction accordingly
            kbd$ = INKEY$
            SELECT CASE kbd$
                CASE "w", "W": IF Sammy(2).direction <> 2 THEN Sammy(2).direction = 1
                CASE "s", "S": IF Sammy(2).direction <> 1 THEN Sammy(2).direction = 2
                CASE "a", "A": IF Sammy(2).direction <> 4 THEN Sammy(2).direction = 3
                CASE "d", "D": IF Sammy(2).direction <> 3 THEN Sammy(2).direction = 4
                CASE CHR$(0) + "H": IF Sammy(1).direction <> 2 THEN Sammy(1).direction = 1
                CASE CHR$(0) + "P": IF Sammy(1).direction <> 1 THEN Sammy(1).direction = 2
                CASE CHR$(0) + "K": IF Sammy(1).direction <> 4 THEN Sammy(1).direction = 3
                CASE CHR$(0) + "M": IF Sammy(1).direction <> 3 THEN Sammy(1).direction = 4
                CASE "p", "P": SpacePause " Jogo Pausado... Tecle Espaáo "
                CASE ELSE
            END SELECT

            FOR a = 1 TO NumPlayers
                'Move Snake
                SELECT CASE Sammy(a).direction
                    CASE 1: Sammy(a).row = Sammy(a).row - 1
                    CASE 2: Sammy(a).row = Sammy(a).row + 1
                    CASE 3: Sammy(a).col = Sammy(a).col - 1
                    CASE 4: Sammy(a).col = Sammy(a).col + 1
                END SELECT

                'If snake hits number, respond accordingly
                IF numberRow = INT((Sammy(a).row + 1) / 2) AND NumberCol = Sammy(a).col THEN
                    PLAY "MBO0L16>CCCE"
                    IF Sammy(a).length < (MAXSNAKELENGTH - 30) THEN
                        Sammy(a).length = Sammy(a).length + number * 4
                    END IF
                    Sammy(a).score = Sammy(a).score + number
                    PrintScore NumPlayers, Sammy(1).score, Sammy(2).score, Sammy(1).lives, Sammy(2).lives
                    number = number + 1
                    IF number = 10 THEN
                        EraseSnake Sammy(), sammyBody(), 1
                        EraseSnake Sammy(), sammyBody(), 2
                        LOCATE numberRow, NumberCol: PRINT " "
                        Level NEXTLEVEL, Sammy()
                        PrintScore NumPlayers, Sammy(1).score, Sammy(2).score, Sammy(1).lives, Sammy(2).lives
                        SpacePause "    N°vel" + STR$(CurLevel) + ", Tecle Espaáo"
                        IF NumPlayers = 1 THEN Sammy(2).row = 0
                        number = 1
                    END IF
                    nonum = TRUE
                END IF
            NEXT a

            FOR a = 1 TO NumPlayers
                'If player runs into any point, or the head of the other snake, it dies.
                IF PointIsThere(Sammy(a).row, Sammy(a).col, colorTable(4)) OR (Sammy(1).row = Sammy(2).row AND Sammy(1).col = Sammy(2).col) THEN
                    PLAY "MBO0L32EFGEFDC"
                    COLOR , colorTable(4)
                    LOCATE numberRow, NumberCol
                    PRINT " "
                   
                    playerDied = TRUE
                    Sammy(a).alive = FALSE
                    Sammy(a).lives = Sammy(a).lives - 1

                'Otherwise, move the snake, and erase the tail
                ELSE
                    Sammy(a).head = (Sammy(a).head + 1) MOD MAXSNAKELENGTH
                    sammyBody(Sammy(a).head, a).row = Sammy(a).row
                    sammyBody(Sammy(a).head, a).col = Sammy(a).col
                    tail = (Sammy(a).head + MAXSNAKELENGTH - Sammy(a).length) MOD MAXSNAKELENGTH
                    Set sammyBody(tail, a).row, sammyBody(tail, a).col, colorTable(4)
                    sammyBody(tail, a).row = 0
                    Set Sammy(a).row, Sammy(a).col, Sammy(a).scolor
                END IF
            NEXT a

        LOOP UNTIL playerDied

        curSpeed! = Speed!                ' reset Speed! to initial value
       
        FOR a = 1 TO NumPlayers
            EraseSnake Sammy(), sammyBody(), a

            'If dead, then erase snake in really cool way
            IF Sammy(a).alive = FALSE THEN
                'Update score
                '**** DIMINUI PONTOS **** Sammy(a).score = Sammy(a).score - 10
                PrintScore NumPlayers, Sammy(1).score, Sammy(2).score, Sammy(1).lives, Sammy(2).lives
                
                IF Sammy(1).lives > 1 AND Sammy(2).lives > 1 THEN
                    IF a = 1 THEN
                        SpacePause "Sammy Morreu! Tecle Espaáo ->"
                    ELSE
                        SpacePause " <- Jake Morreu! Tecle Espaáo "
                    END IF
                END IF
            END IF
        NEXT a

        Level SAMELEVEL, Sammy()
        PrintScore NumPlayers, Sammy(1).score, Sammy(2).score, Sammy(1).lives, Sammy(2).lives
     
    'Play next round, until either of snake's lives have run out.
    LOOP UNTIL Sammy(1).lives = 0 OR Sammy(2).lives = 0

END SUB

'PointIsThere:
'  Checks the global  arena array to see if the boolean flag is set
FUNCTION PointIsThere (row, col, acolor)
    IF row <> 0 THEN
        IF arena(row, col).acolor <> acolor THEN
            PointIsThere = TRUE
        ELSE
            PointIsThere = FALSE
        END IF
    END IF
END FUNCTION

'PrintScore:
'  Prints players scores and number of lives remaining
SUB PrintScore (NumPlayers, score1, score2, lives1, lives2)
    COLOR 15, colorTable(4)

    IF NumPlayers = 2 THEN
        LOCATE 1, 1
        PRINT USING "#,###,#00  Vidas: #  <- JAKE"; score2; lives2
    END IF

    LOCATE 1, 49
    PRINT USING "SAMMY ->  Vidas: #     #,###,#00"; lives1; score1
END SUB

'Set:
'  Sets row and column on playing field to given color to facilitate moving
'  of snakes around the field.
SUB Set (row, col, acolor)
    IF row <> 0 THEN
        arena(row, col).acolor = acolor             'assign color to arena
        realRow = arena(row, col).realRow           'Get real row of pixel
        topFlag = arena(row, col).sister + 1 / 2    'Deduce whether pixel
                                                    'is on topﬂ, or bottom‹
        sisterRow = row + arena(row, col).sister    'Get arena row of sister
        sisterColor = arena(sisterRow, col).acolor  'Determine sister's color

        LOCATE realRow, col

        IF acolor = sisterColor THEN                'If both points are same
            COLOR acolor, acolor                           'Print chr$(219) "€"
            PRINT CHR$(219);
        ELSE
            IF topFlag THEN                         'Since you cannot have
                IF acolor > 7 THEN                  'bright backgrounds
                    COLOR acolor, sisterColor       'determine best combo
                    PRINT CHR$(223);                'to use.
                ELSE
                    COLOR sisterColor, acolor
                    PRINT CHR$(220);
                END IF
            ELSE
                IF acolor > 7 THEN
                    COLOR acolor, sisterColor
                    PRINT CHR$(220);
                ELSE
                    COLOR sisterColor, acolor
                    PRINT CHR$(223);
                END IF
            END IF
        END IF
    END IF

END SUB

'SpacePause:
'  Pauses game play and waits for space bar to be pressed before continuing
SUB SpacePause (text$)

    COLOR colorTable(5), colorTable(6)
    Center 11, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
    Center 12, "€ " + LEFT$(text$ + SPACE$(29), 29) + " €"
    Center 13, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
    WHILE INKEY$ <> "": WEND
    WHILE INKEY$ <> " ": WEND
    COLOR 15, colorTable(4)

    FOR I = 21 TO 26            ' Restore the screen background
        FOR j = 24 TO 56
            Set I, j, arena(I, j).acolor
        NEXT j
    NEXT I

END SUB

'SparklePause:
'  Creates flashing border for intro and game over screens
SUB SparklePause ()

  COLOR 9, 0
  a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
  WHILE INKEY$ <> "": WEND 'Clear keyboard buffer

  WHILE INKEY$ = ""
    FOR a = 1 TO 5
      LOCATE 1, 1                             'print horizontal sparkles
      PRINT MID$(a$, a, 80);
      LOCATE 22, 1
      PRINT MID$(a$, 6 - a, 80);

      FOR b = 2 TO 21                         'Print Vertical sparkles
        c = (a + b) MOD 5
        IF c = 1 THEN
          LOCATE b, 80
          PRINT "*";
          LOCATE 23 - b, 1
          PRINT "*";
        ELSE
          LOCATE b, 80
          PRINT " ";
          LOCATE 23 - b, 1
          PRINT " ";
        END IF
      NEXT b
      t# = TIMER
      DO WHILE TIMER - t# < .001: LOOP
      PALETTE 9, a
    NEXT a
  WEND
END SUB

'StillWantsToPlay:
'  Determines if users want to play game again.
FUNCTION StillWantsToPlay ()

    COLOR colorTable(5), colorTable(6)
    Center 10, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
    Center 11, "€       G A M E   O V E R       €"
    Center 12, "€                               €"
    Center 13, "€      Tentar de Novo? (S/N)    €"
    Center 14, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"

    WHILE INKEY$ <> "": WEND
    DO
        kbd$ = UCASE$(INKEY$)
    LOOP UNTIL kbd$ = "S" OR kbd$ = "N"

    COLOR 15, colorTable(4)
    Center 10, "                                 "
    Center 11, "                                 "
    Center 12, "                                 "
    Center 13, "                                 "
    Center 14, "                                 "

    IF kbd$ = "S" THEN
        StillWantsToPlay = TRUE
    ELSE
        Intro
        GetInputs NumPlayers, Speed!, diff$, monitor$
        DrawScreen
        PlayNibbles NumPlayers, Speed!, diff$
    END IF

END FUNCTION

