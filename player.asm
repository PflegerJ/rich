; i want to just have all the player related stuff here to it is easier to check. probably just a .h file type structure

; player variables:
    ; x pos
    ; x pos float
    ; y pos
    ; y pos float
    ; state
    ; facing direction
    ; animation frame
    ; animation timer


    ; these are all at least like... needed to draw the player. i guess flaot isn't but i need to implement it since im overhauling movement
    ; if there are any other variables, they will be like inventory or health. idk what 
    ; ima just use space until i realize i don't have any

    ; player speed





PlayerLogic:
    lda playerState         ; Drinking - Walking - Peeing - Smoking - interacting - X - Facing Direction ( 0: Down    1: Left     2: Up   3: Right )
    asl 
    bcc @NotDrinking        
    jsr DrinkingLogic
    jmp @DoneState
@NotDrinking:
    asl 
    bcc @NotWalking
    jsr WalkingLogic
    jmp @DoneState
@NotWalking:
    asl ; peeing
    asl ; smoking
    jsr StandingLogic
    jmp @DoneState

@DoneState:
    rts 

StandingLogic:

    lda playerAnimationCounter       ; load animation counter: used to determine where in the animation cycle the player is                             
    and #%00001111                   ; animation cycles every 16 frames i guess?
    clc 
    adc #$01                         ; i did it this way cause i was doing other shit that should have been done elsewhere. might have to edit this code i hope it still works for now
    cmp #$10
    sta playerAnimationCounter          
    bne @ControllerChecking          ; not updating sprite
  ;  lda playerTile                   ; flipping between sprites 0 and 1 for now
  ;  eor #%00000001
  ;  sta playerTile

  ; ok using some placeholder bs to make it more complex and show facing angle
    ; i am under the assumption that we can just use whatever register
   ; lda playerState
   ; and #$03            ; isolates facing angle

    ; ok this is a really bad way to do this
        ; BUT
    ; I think that it doesn't fucking matter. cause like. i will have to redo all this shit when i have actual
        ; animation cycles. so who cares. lets get it barebones and gogogo
    
    ; i don't even need the facing angle. all i care about is changing the first 4 bits from 1 to 2 or 2 to 1
        ; based on how the tile map is right now
        ; should I have code that is hyper dependant on the layout of the tilemap? 
            ; probably not but again i'll fix it later
    lda playerTile

    eor #%00110000
    sta playerTile
   ; and #$F0
    ;cmp #$10
    ;beq @changeItToTwo
    ;lda playerTile
    ;sec 
    ;sbc #$10
    ;jmp @ControllerChecking

;@changeItToTwo:
   ; lda playerTile
   ; clc 
   ; adc #$01

@ControllerChecking:                    ; check input
    lda controller1Pressed
    asl     ; a
    bcs @StandingAPressed
    asl     ; b
    asl     ; select
    asl     ; start i need to code more often cause i should remember the fucking order of user inputs. i looked at reference this should be correct now
    asl ; up
    bcs @StandingUpPressed      ; right now i only read one input. also need to incorperate the actual player movement into this instead of having it be in this old function that sucks balls
    asl ; down
    bcs @StandingDownPressed
    asl ; left
    bcs @StandingLeftPressed
    asl ; right
    bcs @StandingRightPressed
    jmp @DoneStanding
@StandingAPressed:
    jsr StandingAPressedTest
    jmp @DoneStanding
    ;jsr Interact   ; might be this call down the road. right now just making sure i can press A while standing and something happens
@StandingUpPressed:
    lda #%01000010
    sta playerState
    jsr WalkingLogicStart
    jmp @DoneStanding 
@StandingDownPressed:
    lda #%01000000
    sta playerState
    jsr WalkingLogicStart
    jmp @DoneStanding
@StandingRightPressed:
    lda #%01000011
    sta playerState
    jsr WalkingLogicStart
    jmp @DoneStanding
@StandingLeftPressed:
    lda #%01000001
    sta playerState
    lda playerAtt
    and #%10111111
    sta playerAtt
    jsr WalkingLogicStart
                                  ; standing animation sprite update
@DoneStanding:        
    rts 

StandingStateStart:
    lda #$00  
    sta playerAnimationCounter
   ; lda #$00                        ; first sprite of standing animation (obvi temp idk what ima do for animation but make it bad at first tand then better lets goooo)
    
    
    lda playerState
    and #$03        ; isolate facing angle
    cmp #$00
    bne @notFacingDown2
    lda #$10
    sta playerTile
    jmp @DoneStandingStateStart

@notFacingDown2:
    cmp #$01
    bne @notFacingLeft2
    lda #$11
    sta playerTile
    jmp @DoneStandingStateStart

@notFacingLeft2:
    cmp #$02
    bne @notFacingUp2
    lda #$12
    sta playerTile
    jmp @DoneStandingStateStart

@notFacingUp2:
    lda #$11
    sta playerTile
    lda playerAtt
    ora #%01000000
    sta playerAtt

@DoneStandingStateStart:
    rts 
DrinkingLogic:
    rts 
DrinkingLogicStart:
    rts 
WalkingLogic:

    ;; check a b start first. those actions take prio over moving
    jsr CheckWeStillWalking
    beq @NotWalkingAnymore
    jsr StillWalking
    jmp @DoneWalking

@NotWalkingAnymore:
    lda playerState     ; not moving so standing state starts
    and #%00000011
    sta playerState
    jsr StandingStateStart
    ;; read no movement input. go back to standing
@DoneWalking:
    rts 

;;;; could i just assume start - select - A - B are all not pressed? cause i was thinking of prioing them. so if they were pressed then i would be in a different state already. 
            ; wait unless they hit A with nothing to interact with. unless I want to make that a state
            ; well lets do it the safe way for now and if i notice that i its always going to be 0000XXXX at this point then yay more clock cycles for other bad code 

;; ok i optimized this funciton. it used to be (p and q) or (s and q). now its (p or s) and q. saved like 8 - 10 clock cycles per call nbd.
CheckWeStillWalking:            ; this function will return 1 if we still walking 0 if we have stopped in the A reg
         ; the value in A has to be 0 already so it works as the return value! yay optimization. im so good at saving 2 clock cycles and terrible at saving hundreds
    lda controller1Pressed    ; 4
    and #%00001111            ; 2
    ora controller1Held       ; 4
    and #%00001111            ; 2
    rts 

    ;; nothing is pressed so go back to standing

    lda playerState
    and #%00000011          ; set player state to standing while preserving facing direction
    sta playerState

@StoppedWalkingStartStanding:
    jsr StandingStateStart 
    rts 
    ; i think its best to check if im still holding the same direction as im facing. 

StillWalking:

    ;;; ok first ima check if any movement buttons were held. cause if so, i can just do the animation 
                ; right now the animation is don't change, but future obvi it will cont whatever.
                ; I think i can also just do the movement and call it a day.  wait. so. when am i changing direction? cause the direction is already set from standing state subroutine.
                    ; so like. and the animation counter has been reset from the startwalking function call right? i should make sure thats how i set it up.
                    ; but like don't i just need to make sure no non-movement buttons were pressed. change playerState accordingly if so, and then
                    ; just continue the animation, and apply the movement of the held and pressed buttons? so not even held or pressed right?
                    ; just like controller1PreviousInput. right? just read that and call it a day? ok lets fucking yolo it and pray.


    ;; ok so it mostly works -  the issue is if you are always holding a d-pad button down, you will never change your facing. so i guess that means you can walk backwards.
            ; and i kind of was doing this so you could straif... maybe it could be with a held button. but you only have a and b. so it would have to be b. maybe b isn't drink...
                ;; idk i only drank in my room. so A could be interact and just be at the computer passively drinking. doing an animation and decrementing inv. upping bladder scoring points.

    ; ok wait. i could just check if a button matches the facing. and if not, hmmm, first try to flip? or check pressed?
    ;; is this even neccisssarry? idk. i was trying to think of way a to not just walk backwards every where cause thats what i've been doing so far as i test..
    ;; maybe I would have to add a straiffing sub state in one of the empty bits in playerState...
    ; to have reduced movement speed. I guess i could just check if the move<Dirction> fucntion im about to call matches the direction.
    ;; if not reduce speed. but thats for another day for sure.



    ;; first animation.
    lda playerAnimationCounter
    clc 
    adc #$01
    sta playerAnimationCounter
    ;; this is where animation shit would go? maybe just have like
    ; jsr WalkingAnimation?
    ;; but I want to at least inc the counter cause i have it so might as well think about it.
        ;; wait maybe this should be at the end... if i want to swap directions I guess...


    ;; ok now just apply the movement?
    lda controller1PreviousInput
    ror ; right?
    bcc @NotMovingRight
    ;; ok im yolo trying to use the stack to save the controller1PreviousInput since im roring bits off it. yolo you know
    pha 
    jsr moveRight
    pla 

@NotMovingRight:   
    ror ; left?
    bcc @NotMovingLeft
    pha 
    jsr moveLeft 
    pla 

@NotMovingLeft:
    ror ; down?
    bcc @NotMovingDown
    pha 
    jsr moveDown
    pla 

@NotMovingDown:
    ror ; up?
    bcc @NotMovingUp
    pha 
    jsr moveUp
    pla 

@NotMovingUp: 
    rts 

WalkingLogicStart:
    lda #$00                    ; first reset animation counter
    sta playerAnimationCounter
    lda playerState             ; load in the player state and then AND it to get facing direction
    and #%00000011
    ; cmp #$00              ; I don't need to cmp here for 0. cause the AND will set the zero flag. and beq branches if the zero flag is 0. i think. or its the other way around and i'll figure out the bug eventually
    beq @WalkingDownStart ; walking down

    cmp #$01
    beq @WalkingLeftStart
    
    cmp #$02
    beq @WalkingUpStart

@WalkingRightStart:

    lda #$83
    sta playerTile
    jsr moveRight
    jmp @WalkingStartDone

@WalkingDownStart:
    lda #$80
    sta playerTile
    jsr moveDown
    jmp @WalkingStartDone

@WalkingLeftStart:
    lda #$81
    sta playerTile
    jsr moveLeft
    jmp @WalkingStartDone

@WalkingUpStart:
    lda #$82
    sta playerTile
    jsr moveUp
    jmp @WalkingStartDone

@WalkingStartDone:
    rts 

StandingAPressedTest:
    ; frist lets just set the sprite tile and position cause this is a test so im not going to initialize it elsewhere thats dumb
    lda aButtonTestTile
    cmp #$70
    beq @aButtonTestInit
    lda #$70
    sta aButtonTestTile
    lda #$90
    sta aButtonTestXpos
    lda #$80
    sta aButtonTestYpos
    lda #$00
    sta aButtonTestAtt

@aButtonTestInit:
    

    jsr CheckTileInFront
    rts 

CheckTileInFront:
    ; i want to check the tile in front of the player to see if that tile contains something the player can interact with
    ; so what do i need to do......
    ; First i need to get the player's position and reduce it into a tile. so what... just remove the last 3 bits? 
    ; where is the actual position? isn't it like 1 pixel above the sprite? 

    ; lets yolo
    ; lets grab players pos, adjust to reflect where the sprite is and remove last 3 bits.
    ; just store into temp1 and temp2 for now

    

    lda playerYpos
    clc 
    adc #$01
    and #$F8
    sta temp2   ; this gets the player y pos, adjusts cause its 1 pixel higher than the sprite, then clears the last 3 digits. 
                    ; i will have to change this cause it will def be weird when trying to interact with something either slightly above or below idk which but one of them for sure
    lda playerXpos
    
    

    and #$F8            ; don't need to adjust y? wait i only need to adjust y
    sta temp1

; this is taking player facing direction into account. so ima store the value as one tile over in the direction the player is facing
        ; again this will have to be updated if i don't AND the last 3 bits to 0 so that i can interact with things inbetween tiles
        ; Facing Direction ( 0: Down    1: Left     2: Up   3: Right )
    lda playerState
    and #$03
    cmp #$00        ; facing down
    bne @notFacingDown
    lda #$08
    clc 
    adc temp2
    sta temp2
    jmp @doneWithFacing

@notFacingDown:
    cmp #$01        ; facing left
    bne @notFacingLeft
    lda temp1
    sec 
    sbc #$08
    sta temp1
    jmp @doneWithFacing

@notFacingLeft:
    cmp #$02        ; facing up
    bne @notFacingUp
    lda temp2
    sec 
    sbc #$08
    sta temp2
    jmp @doneWithFacing

@notFacingUp:
    lda #$08        ; facing right
    clc 
    adc temp1
    sta temp1

@doneWithFacing:
    ldy roomIndex
    lda RoomInteractLo, y
    sta pointerLo
    lda RoomInteractHi, y
    sta pointerHi
    ldy #$00            ; y is now the loop counter thing
    lda (pointerLo), y  ; this grabs the count and can be used at the loop count limit
    tax                 ; so lets put it in x


@interactLoop:
    iny         
    lda (pointerLo), y  ; this is the x pos of the interactable obj
    cmp temp1
    bne @interactLoopXNotMatch
    iny 
    lda (pointerLo), y 
    cmp temp2
    bne @interactLoopYNotMatch
        ; if we here then both x and y pos match
        ; this is where we would call the code for that specific obj. probably store the pointers in the location table
    
    ; interactable object found in front of the player. gets the hi and then low - 1 byte of that objects interact function
    ; pushes it on to the stack and then rts will go to that function 
    iny 
    lda (pointerLo), y
    pha 
    iny  
    lda (pointerLo), y
    pha 
    jmp @interactLoopEnd
    ; ok so temp1 is player x pos rounded to nearest tile, temp2 is adjusted then rounded to nearest tile
@interactLoopXNotMatch:
    iny 
@interactLoopYNotMatch:
    iny 
    iny   
    dex   
    cpx #$00
    bne @interactLoop 
@interactLoopEnd:
    rts 


InteractTestFunction1:
    lda aButtonTestAtt
    eor #%10000000
    sta aButtonTestAtt
    rts 

InteractTestFunction2:
    lda #$02
    sta roomIndex
    jsr LoadRoom
    rts 

ToiletInteract:
                                                                ; so like how the fuck do i have text or something flash on the screen and then go away? 
                                                                ; cause like how do i keep track of how long its on the screen and when it knows to go away?
                                                                    ; couple ideas
                                                                        ; iterate through sprites somehow and do any code that pertains to them. like scott moving or something
                                                                        ; have the room be in charge? cause i think i go through the room code?

    ; so i figured this out ^^^^^^^^^^^^^^^^
    ; fuck all this shit code. it did it's job though so thank you!!!!

    ; ok so what I need to do. right now ima see if i can create the text when interacting with the toilet
            ; i still need to iterate through each game object each frame so ugh i'll set that up next
            ; lets just get it to display like 4 things and then after x time deletes 2, then y time deletes the other 2?
                ; well i guess it doesn't do the delete. 
                    ; I need to have a text box function that each text uses as its game object function
                        ; the generic version can have like, uses a pointer to get all the characters and placements and how long it will stay out/goes away from another source
                            ; wait thats more a generic create function
                        ; generic game loop function would just decrement the counter if it had it.

    ; ok focus on this function

    ; basically we will grab the tables ToiletLetters1 - 4? and make the game objects.

    ; returning here after a couple of days rotting away and wasting my life
        ; did I even do anything last time?
    
    ; ok all this is trash I think I can just remove. I might need to check the bathroom function to see if it breaks but ima have to edit that anyways probably at least who the fuck know idk what code i wrote yesterday so no way i know what the fuck is going on in that function 
   ; lda bathroomFlag
   ; cmp #$00
   ; bne @ToiletInteractDone  ; text is still there. i don't want to reset the text timer cause idc
   ; jsr ToiletInteractSetSprites

    ; so
    ; wait 
    ; that code might not be useless...
  ;  lda bathroomFlag        ; could make this room flag and just be used by whatever room is loaded. 
   ; cmp #$00
   ; bne @ToiletInteractDone  ; text is still there. i don't want to reset the text timer cause idc
    ; jsr ToiletInteractSetSprites
    ; fucking a man
        ; should the toilet be a game object? or should it be some static thing in the room. fuck
            ; thats for later but will have to change this depending on my choice but when do i not have to redo code when i learn more better
    
    ; i'll have the text set the bathroomflag back to 0 when they realize they about to be deleted. obvi that a shit way but again. also it isn't that bad when its just 1 game obj instead of 1 per char
  ;  inc bathroomFlag

    ; i need some loop to create the 4 characters
        ; it needs to set pointerLo and PointerHi to the right toiletLettersX
        ; i think thats it? then just make sure toiletLettersX table is formatted correctly
        
    ldx #$00    ; idk which but i haven't used any registers so yolo
    ldy #$00
    ; ok 

    ;; so. i have in ROM. a table with the label ToiletLetters1. that label is for readablitity. computer no see label.
        ; so that's why I have to have store the address of the label somewhere with the .word ToiletLetters1
        ; then when I want to get the data from that table. 
            ; I use < for lo ( left l low ) and > for hi
            ; but I use #. because
                ; because I want the data. right?
                    ; the data is an address which I think is why its confusing
                ; but example:
                    ; .word ToiletLetters1 ( this is storign the address of the label which is the start of the table. )
                                                        ; $A000:    $02     
                                                        ; $A001:    $AB   
                    ; ToiletLetters1: ( this is just a label for me, it is not stored anywhere in mem when assembled. thats why we have either the hi lo tables or the .word of the label somewhere )
                        ; .byte $80, $10, $00, $68
                                                        ; $02AB:    $80
                                                        ; $02AC:    $10
                                                        ; $02AD:    $00
                                                        ; $02AE:    $68
        ; the data at $A000 is the address of the table
            ; so we use # to get the data

        ; i'll fuck this up again but its starting to sink in
            ; refrences and pointers might be easy after this project who knows   
        
        ; literally isntantly i don't get it
            ; why can i just do <ToiletLetterGameLoop no problem?
                ; um
                    ; so. when its a label to a table. its not stored anywhere in mem actually
                        ; but in this case. we are trying to use the not stored memory address,
                            ; we are storing the memory address of the label. 
                                ; if we used # then it would be getting the opcode byte values of the code from the function ToiletLettersGameLoop
                            ; wow way to work through it :)

    lda #<(ToiletLetters1)
    sta pointerLo
    lda #>(ToiletLetters1)
    sta pointerHi
    jsr CreateGameObject

    lda #<(ToiletLetters2)
    sta pointerLo
    lda #>(ToiletLetters2)
    sta pointerHi
    jsr CreateGameObject
    

   
    ;; ok i still need to fix the data tables and create a gameloop function. but it seems like its working


@ToiletInteractDone:
    rts 

;; fuck ima need to make a more streamlined way to display text. cause this sucks.
    ;; def make defines for each tile for each letter cause this is annoying af
.word ToiletLetterGameLoop
.word ToiletLetters1
.word ToiletLetters2
.word ToiletLetters3
.word ToiletLetters4
.word ToiletLetters5
.word ToiletLetters6
.word ToiletLetters7
.word ToiletLetters8
.word ToiletLetters9
.word ToiletLetters10
ToiletLetters1: 
    ;      y  tile  att   x    hi  lo var ?
    .byte $A0, $D0, $00, $40, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, >DrawTextStatic2, <DrawTextStatic2 - 1, $FF, $00, $00, $00, $00

ToiletLetters2: 
    ;      y  tile  att   x    hi  lo var ?
    .byte $90, $D0, $00, $10, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, >DrawTextStatic2, <DrawTextStatic2 - 1, $80, $01, $01, $01, $01

TestNPC1:
   ; .byte $80, $D0, $00, $80, >TestNPCGameLoop, <TestNPCGameLoop - 1, >DrawTestNPC, DrawTestNPC - 1, $00, $00, $00, $00, $00
ToiletLetters22:
    .byte $10, $D8, $00, $90, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $40 
ToiletLetters3:
    .byte $10, $D9, $00, $A0, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters4:
    .byte $10, $D2, $00, $A8, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters5:
    .byte $10, $DC, $00, $B0, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters6:
    .byte $10, $DC, $00, $B8, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters7:
    .byte $18, $D0, $00, $90, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF 
ToiletLetters8:
    .byte $18, $D2, $00, $98, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF
ToiletLetters9:
    .byte $18, $DB, $00, $A0, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF
ToiletLetters10:
    .byte $18, $D5, $00, $A8, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF


;; i honestly need to write the game loop interation function first. cause idk how I'll access the object's variables. if some register should have the offset already
    ; or if i save the current offset to a variable or something.

; but the gist of this dummy version
    ; check var1 which is the timer 
    ; if 0 set bathroomflag to 0 and call to be deleted
    ; if > 0 dec var1 
        ; thats it
    
    ; also fuck im wondering if I should like queue up things that should be deleted
        ; one example:
                ; object A does it's code. it hit object B but also got hit and takes lethal dmg. it deletes itself.
                ; object b does it's code. it was hit by A but also hits A. but A is already deleted so it can't get any info from it
            ; maybe it would work where both objects are updated.
; but that update could affect other shit? this honestly is outside my current knowledge of all the systems required so i'll come back to this way later             

ToiletLetterGameLoop:
    dec objectVar1,x
    lda objectVar1,x 
    cmp #$00
    beq @DeleteThis
    sta objectVar1,x 
    rts 
@DeleteThis:
    lda #$01
    sta deleteFlag
    rts 
    ; so the offset is in x right now
    ; we following the logic in the comments above this function
    lda #$01
    sta bathroomFlag
    lda objectVar1,x
    sec 
    sbc #$01  
    cmp #$00
    bne @StillAlive
    sta bathroomFlag
    jsr DeleteGameObject
    rts 
@StillAlive:
    sta objectVar1,x
    rts 

ToiletInteractSetSprites:
    lda #$3C
    sta bathroomFlag

    ldy #$00


    rts 

Interact:
    ;check if there is something infront of the player that they can interact with
    ; what are ways we can do this?
    ; have a table of locations of interactable things in each room? I feel like that fits with how im setting up everything else?
    ;   so we'll have a table of tables (wow so original). this table will be indexed using the roomIndex to get the address of the table of interactable object locations for each room

    ; first using room index get interactable table address

    ; go through the interactable table for the specific room checking if any of them are in front of the player
        ; what is in front?
        ; i think ima keep everyting on 1 tile so i just check 1 tile in front.
                ; what if im in between tiles? i should have like a range. if im 1 pixel inside a tile, then no. so i guess 
                ; hmmm lets make it stupidly easy. i'll reduce my player pos to the tile. and check the tile in front. i think it will be kind of shitty but that is something for future me to fix
    
    ; so if we get to the end of the list we just rts and call it a day.

    ; if we can interact. how do we handle this?
        ; is the object in charge of handling what happens? hmmmm think simple at first. then when it inevitably is wrong i know why or i have a better understanding of all the things i need to account for
        ; computer
            ; does a menu pop up?
            ; do i do an animation?
            ; does it affect time? or score?
            ; does it change my state?
    
    ; after all that fun stuff i need to make sure to know if i can i need to end playerlogic cause im stuck in that interaction and for how long or if i can keep moving or something
        ; i think im getting ahead of myself. god im just so fried but im glad im at least thinking about it. i'll code it out tomorrow and it will be rough cause i'll have to remmeber everything but thats ok i got this
    rts 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Interact Tables
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; James Room
; x, y, hi, low - 1
JamesRoomInteract:
    .byte $02       ; count
    .byte $80, $80, >InteractTestFunction1, <InteractTestFunction1 - 1  ; location of the interactable object
    .byte $A0, $88, >InteractTestFunction2, <InteractTestFunction2 - 1

LivingRoomInteract:
    .byte $00

ScottRoomInteract:
    .byte $00

BathroomInteract:
    .byte $01
    .byte $90, $30, >ToiletInteract, <ToiletInteract - 1

    RoomInteractLo:
    .byte <JamesRoomInteract, <LivingRoomInteract, <ScottRoomInteract, <BathroomInteract
RoomInteractHi:
    .byte >JamesRoomInteract, >LivingRoomInteract, >ScottRoomInteract, >BathroomInteract




    moveUp:
    dec playerYpos
    lda playerYpos
    clc
    adc #$01
    tay
    ldx playerXpos
    jsr check_background_collision
    beq @checkRightPixel
    inc playerYpos

@checkRightPixel:
    lda playerYpos
    clc
    adc #$01
    tay
    lda playerXpos
    clc
    adc #$07
    tax
    jsr check_background_collision
    beq @noCollision
    inc playerYpos
    rts

@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
   ; jsr loadbackground
@NoLoadingZoneFound:
    rts

moveDown:
    inc playerYpos
    lda playerYpos
    clc
    adc #$08
    tay
    ldx playerXpos
    jsr check_background_collision
    beq @checkRightPixel
    DEC playerYpos

@checkRightPixel:
    lda playerYpos
    clc
    adc#$08
    tay
    lda playerXpos
    clc
    adc #$07
    tax
    jsr check_background_collision
    beq @noCollision
    dec playerYpos
    rts

@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
   ; jsr loadbackground
@NoLoadingZoneFound:
    rts

;; x + 7, y + 1 to deal with position being x (x, y - 1) of where the sprite is drawn
moveRight:
    inc playerXpos
    lda playerXpos
    clc
    adc #07
    tax 
    lda playerYpos
    clc
    adc #$01
    tay
    jsr check_background_collision
    beq @checkBottomPixel
    dec playerXpos

@checkBottomPixel:
    lda playerXpos
    clc
    adc #$07
    tax
    lda playerYpos
    clc
    adc #$08
    tay
    jsr check_background_collision
    beq @noCollision
    dec playerXpos
    rts 
@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
    ;jsr loadbackground
@NoLoadingZoneFound:
    rts 



moveLeft:
    dec playerXpos
    ldx playerXpos
    lda playerYpos
    clc
    adc #$01
    tay
    jsr check_background_collision
    beq @checkBottomPixel
    inc playerXpos

@checkBottomPixel:
    ldx playerXpos
    lda playerYpos
    clc
    adc #$08
    tay
    jsr check_background_collision
    beq @noCollision
    inc playerXpos
    rts

@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
   ; jsr loadbackground
@NoLoadingZoneFound:
    rts 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


sprites: ;  y  tile  att  x
    .byte $FE, $fe, $fe, $fe    ; 0 sprite off the screen (maybe status bar or something)
    .byte $80, $10, $00, $80 ; YCoord, tile number, attr, XCoord
    .byte $10, $00, $00, $10        ; 1's digit of timer sprite
    .byte $10, $00, $00, $08        ; 10s digit of timer sprite
    .byte $20, $00, $00, $10        ; 1s digit of timer2
    .byte $20, $00, $00, $08        ; 10s digit of timer 2

    .byte $20, $00, $00, $80        ; score 1s
    .byte $20, $00, $00, $78        ; score 10s
    .byte $20, $00, $00, $70        ; score 100s
    .byte $20, $09, $00, $68        ; score 1000s
    .byte $20, $00, $00, $60        ; score 10000s

    .byte $88, $00, $00, $A0
    .byte $80, $00, %00100000, $80        ; scott

        ;; weird idea. what if i just like yolo the sprites. like. is this what a buffer is? cause ive understood the conecpt but never the freaking impelmentation. 
                ;; so like. instead of writing directly to $02XX, is it better to write somewhere else... i guess as i write that out it seems like a no.. idk.
                ;; hmm let me think. I guess one thing thats semi related i guess. but like. right now every entity is hard coded. and for timers i guess that makes sense, and same for palyer.
                ;; but do I need every npc location known at all times. or at least taking up memory? 
                ;; so I'll need to write a better sprite and background shit probs. idk if I can with back ground, but at least... idk i think doing mapping can be saved for next proj
                ;; who knows though

loadSprites:
    ;lda spriteCount   ; this will be used when each map knows how many sprites it has on load
   ; asl
    ;asl
    ldx #$00
spriteLoop:
    lda sprites, X
    sta SPRITE_RAM, X
    inx
    cpx #$34
    bne spriteLoop
    rts