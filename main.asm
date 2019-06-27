 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:    main.asm 
;
; Access to the input pins of the ADC is avalible at connector J15. Here is the top
; view of the connector:
;
; +--+
; |  | <-- Red power button
; +--+
;
; +-----+-----+
; + GND | IN7 |
; +-----+-----+
; + IN6 | IN5 |
; +-----+-----+
; + IN4 | IN3 |
; +-----+-----+
; + IN2 | IN1 |
; ------+-----+
; + IN0 | 5V  |
; +-----+-----+
;      J15
;

$NOLIST
$MODDE1SOC
$LIST

; Bits used to access the LTC2308
LTC2308_MISO bit 0xF8 ; Read only bit
LTC2308_MOSI bit 0xF9 ; Write only bit
LTC2308_SCLK bit 0xFA ; Write only bit
LTC2308_ENN  bit 0xFB ; Write only bit

;TIMER0_RELOAD_L DATA 0xf2
;TIMER0_RELOAD_H DATA 0xf4


TIMER0_RATE   	EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD 	EQU ((65536-(CLK/(12*TIMER0_RATE))))
CLK 			EQU 33333333
;CLK_2 EQU 22118400 ; Microcontroller system crystal frequency in Hz
BAUD 			EQU 57600
TIMER2_RATE   	EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD 	EQU ((65536-(CLK/(12*TIMER2_RATE))))
TIMER_1_RELOAD 	EQU (256-((2*CLK)/(12*32*BAUD)))
;TIMER_0_1ms 	EQU (65536-(CLK/(12*1000)))
PULSE 			equ P1.2
SOUND_OUT     	equ P3.7

; Reset vector
org 0x0000
	ljmp MainProgram

; External interrupt 0 vector (not used in this code)
org 0x0003
	lcall ABORT_M
	reti

; Timer/Counter 0 overflow interrupt vector (not used in this code)
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	lcall Interrupt_1
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector (not used in this code)
org 0x002B
	ljmp Timer2_ISR
	;reti

$NOLIST     ;do we need this twice?
$include(math32.inc)
$include(LCD_4bit_DE1Soc.inc)
$include(set_values.asm)
$include(EEPROM_P1.asm)
$LIST

dseg at 0x30
x: 			ds 4
y: 			ds 4
bcd: 		ds 5
tc: 		ds 4
temp: 		ds 4
save: 		ds 4

;state machine variables
state: 		ds 1
temp_soak: 	ds 1 ;Set to 150C
Time_soak: 	ds 1 ;Set to 60s
Temp_refl: 	ds 1 ;Set to 217C
Temp_abrt: 	ds 1 ;Set to 230C
Time_refl: 	ds 1 ;Set to 45s
Temp_cool: 	ds 1 ;Set to 60C
pwm_on:	   	ds 2 ;Oven controller
pwm_off:   	ds 2 
sec: 	   	ds 1
seconds:	ds 1
minutes: 	ds 1
Count1ms: 	ds 2
CountBuzz:	ds 2
AB_TEMP:	ds 1
BuzzerTime: ds 2

bseg
mf: dbit 1
stateChanged: dbit 1
seconds_flag: dbit 1

cseg

ELCD_RS equ P0.0
ELCD_RW equ P0.1
ELCD_E  equ P0.2
ELCD_D4 equ P0.3
ELCD_D5 equ P0.4
ELCD_D6 equ P0.5
ELCD_D7 equ P0.6

MY_CLEAR: db '                ', 0
P_STATE: db 'S: ', 0
P_STATE_0: db 'Wait For Key3 ', 0
P_STATE_1: db 'Ramp to Soak  ', 0
P_STATE_2: db 'Soak/Preheat  ', 0
P_STATE_3: db 'Ramp to Peak  ', 0
P_STATE_4: db 'Reflow        ', 0
P_STATE_5: db 'Cooling       ', 0
ABORT_M1:  db 'Warning User         ', 0
ABORT_M2:  db 'Abort Detected   ', 0
ABORT_S1:  db 'Warning System',0
ABORT_S2:  db 'ABORT, CHECK TC ', 0
ABORT_T1:  db 'Warning System',0
ABORT_T2:  db 'Abort, Crit Temp', 0

ABORT_M:
	push acc
	mov state, #0
	WriteCommand(#0x80)
	Send_Constant_String(#ABORT_M1)
	WriteCommand(#0xC0)
	Send_Constant_String(#ABORT_M2)
	lcall Wait_2_sec
	WriteCommand(#0x80)
	Send_Constant_String(#MY_CLEAR)
	WriteCommand(#0xC0)
	Send_Constant_String(#MY_CLEAR)
	pop acc
	ret	

Interrupt_1:
	push acc
	push psw
	mov a, state
	cjne a, #0, SS
	lcall Set_Values
	lcall Save_Configuration
SS:
	pop psw
	pop acc
	reti

Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	mov TH0, #high(TIMER0_RELOAD) ; Timer 0 doesn't have autoreload in the CV-8052
	mov TL0, #low(TIMER0_RELOAD)
	jnb StateChanged, noSound
	cpl SOUND_OUT ; Connect speaker to P3.7!
noSound:
	reti
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

Timer2_ISR:   ;complement oven
	clr TF2
	;clr c
	push acc
	push psw
	; Increment the 16-bit one mili second counter
	inc CountBuzz+0
	mov a, CountBuzz
	jnz Inc_Buzz_Done
	inc CountBuzz+1
Inc_Buzz_Done:
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
Inc_Done:
	; Check if second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), PWM_Interrupt ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), PWM_Interrupt
;-----------second has passed------------------------------------------------------
	;send temperature to serial port every second 
	mov save, x 
	mov save+1, x+1 
	mov save+2, x+2 
	mov save+3, x+3
	mov x, temp         
	mov x+1, temp+1
	mov x+2, temp+2
	mov x+3, temp+3
	lcall hex2bcd
	lcall Display_BCD ; Display using the 7-segment displays
	lcall SendNumber  ; Send to serial port
	mov x+3, save+3 
	mov x+2, save+2
	mov x+1, save+1
	mov x, save 


	inc sec
	inc seconds
	
	clr PULSE
	clr TR2
	clr a
	mov Count1ms, a
	mov Count1ms+1, a
	setb TR2

	mov a, seconds
	cjne a, #60, Timer2_ISR_done
	inc minutes
	mov seconds, #0

	ljmp Timer2_ISR_done
PWM_Interrupt:
	mov a, Count1ms
	cjne a, pwm_on, Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, pwm_on+1, Timer2_ISR_done
	
	setb PULSE
Timer2_ISR_done:
	mov a, CountBuzz+0
	cjne a, BuzzerTime, StopBuzzer
	mov a, CountBuzz+1
	cjne a, BuzzerTime+1, StopBuzzer
	clr StateChanged
	mov countBuzz, #0
	mov countBuzz+1, #0
StopBuzzer:	
	pop psw
	pop acc
	reti


Initialize_Serial_Port:
	; Configure serial port and baud rate
	clr TR1 ; Disable timer 1
	anl TMOD, #0x0f ; Mask the bits for timer 1
	orl TMOD, #0x20 ; Set timer 1 in 8-bit auto reload mode
    orl PCON, #80H ; Set SMOD to 1
	mov TH1, #low(TIMER_1_RELOAD)
	mov TL1, #low(TIMER_1_RELOAD) 
	setb TR1 ; Enable timer 1
	mov SCON, #52H
	ret


putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret
	
getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret

; Receive a character using the serial port
getchar_1:
    jnb RI, getchar
    clr RI
    mov a, SBUF
    lcall putchar ; echo back what was received
    ret

SendString:
    clr a
    movc a, @a+dptr
    jz SendString_L1
    lcall putchar
    inc dptr
    sjmp SendString  
SendString_L1:
	ret
	
Initialize_LEDs:
    ; Turn off LEDs
	mov	LEDRA,#0x00
	mov	LEDRB,#0x00
	ret
	
Initialize_ADC:
	; Initialize SPI pins connected to LTC2308
	clr	LTC2308_MOSI
	clr	LTC2308_SCLK
	setb LTC2308_ENN
	ret

LTC2308_Toggle_Pins:
    mov LTC2308_MOSI, c
    setb LTC2308_SCLK
    mov c, LTC2308_MISO
    clr LTC2308_SCLK
    ret

; Bit-bang communication with LTC2308.  Check Figure 8 in datasheet (page 18):
; https://www.analog.com/media/en/technical-documentation/data-sheets/2308fc.pdf
; The VREF for this 12-bit ADC is 4.096V
; Warning: we are reading the previously converted channel! If you want to read the
; channel 'now' call this function twice.
;
; Channel to read passed in register 'b'.  Result in R1 (bits 11 downto 8) and R0 (bits 7 downto 0).
; Notice the weird order of the channel select bits!
LTC2308_RW:
    clr a 
	clr	LTC2308_ENN ; Enable ADC

    ; Send 'S/D', get bit 11
    setb c ; S/D=1 for single ended conversion
    lcall LTC2308_Toggle_Pins
    mov acc.3, c
    ; Send channel bit 0, get bit 10
    mov c, b.2 ; O/S odd channel select
    lcall LTC2308_Toggle_Pins
    mov acc.2, c 
    ; Send channel bit 1, get bit 9
    mov c, b.0 ; S1
    lcall LTC2308_Toggle_Pins
    mov acc.1, c
    ; Send channel bit 2, get bit 8
    mov c, b.1 ; S0
    lcall LTC2308_Toggle_Pins
    mov acc.0, c
    mov R1, a
    
    ; Now receive the lest significant eight bits
    clr a 
    ; Send 'UNI', get bit 7
    setb c ; UNI=1 for unipolar output mode
    lcall LTC2308_Toggle_Pins
    mov acc.7, c
    ; Send 'SLP', get bit 6
    clr c ; SLP=0 for NAP mode
    lcall LTC2308_Toggle_Pins
    mov acc.6, c
    ; Send '0', get bit 5
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.5, c
    ; Send '0', get bit 4
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.4, c
    ; Send '0', get bit 3
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.3, c
    ; Send '0', get bit 2
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.2, c
    ; Send '0', get bit 1
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.1, c
    ; Send '0', get bit 0
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.0, c
    mov R0, a

	setb LTC2308_ENN ; Disable ADC

	ret

; Converts the 16-bit hex number in [R1,R0] to a 
; 5-digit packed BCD in [R4,R3,R2] using the
; double-dabble algorithm.
hex2bcd16:
	clr a
	mov R4, a ; Initialize BCD to 00-00-00 
	mov R3, a
	mov R2, a
	mov R5, #16  ; Loop counter.

hex2bcd16_L1:
	; Shift binary left	
	mov a, R1
	mov c, acc.7 ; This way [R1,R0] remains unchanged!
	mov a, R0
	rlc a
	mov R0, a
	mov a, R1
	rlc a
	mov R1, a
    
	; Perform bcd + bcd + carry using BCD arithmetic
	mov a, R2
	addc a, R2
	da a
	mov R2, a
	mov a, R3
	addc a, R3
	da a
	mov R3, a
	mov a, R4
	addc a, R4
	da a
	mov R4, a

	djnz R5, hex2bcd16_L1

	ret



; Look-up table for the 7-seg displays. (Segments are turn on with zero) 
T_7seg:
    DB 40H, 79H, 24H, 30H, 19H, 12H, 02H, 78H, 00H, 10H

; Display the 4-digit bcd stored in [R3,R2] using the 7-segment displays
Display_BCD:
	mov dptr, #T_7seg
	
	mov a, bcd+1
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX4, a
	
	mov a, bcd+0
	swap a
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX3, a
	
	mov a, bcd+0
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX2, a
	mov HEX1, #0b0011100
	mov HEX0, #0b1000110

	
	ret

; Send a 4-digit BCD number stored in [R3,R2] to the serial port	
SendNumber:
	mov a, bcd+1
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	mov a, bcd
	swap a
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	mov a, bcd
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
	
	ret
	
	

MainProgram:
    mov sp, #0x7f
    mov P0MOD, #0b01111111
    mov P2MOD, #0b00001011
	mov P1MOD, #0b00001100
	mov P3MOD, #0b10001100
    lcall Initialize_LEDs
	lcall FT93C66_INIT_SPI
    lcall Initialize_Serial_Port
    lcall Initialize_ADC
    lcall Timer2_Init
    lcall Timer0_Init
    setb EA
    setb EX0
    setb EX1
    lcall ELCD_4bit

	;initialize state variables
	clr PULSE
	mov state, #0	

	lcall Load_Configuration
	lcall Save_Configuration
    clr seconds_flag
	clr StateChanged
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	mov BuzzerTime, #low(1000)
	mov BuzzerTime+1, #high(1000)
    mov pwm_on, #0
    mov pwm_on+1, #0
    mov AB_TEMP, #50
 

forever:
	;read cold junction
	mov b, #1
	lcall LTC2308_RW  ; Read the channel from the ADC
	Wait_Milli_Seconds(#1)
	mov b, #1
	lcall LTC2308_RW  ; Read the channel from the ADC
	mov x, R0
	mov x+1, R1
	mov x+2, #0
	mov x+3, #0	
	;convert cold junction voltage to temperature
	load_y(2730)
	lcall sub32
	load_y(152)
	lcall sub32
	load_y(1000)
	lcall mul32
	mov tc, x
	mov tc+1, x+1
	mov tc+2, x+2
	mov tc+3, x+3
	
	;read hot voltage and calculate temperature
	mov b, #0
	lcall LTC2308_RW  ; Read the channel from the ADC
	Wait_Milli_Seconds(#1)
	mov b, #0
	lcall LTC2308_RW  ; Read the channel from the ADC
	mov x, R0
	mov x+1, R1
	mov x+2, #0
	mov x+3, #0
	load_y(806)
	lcall mul32
	mov y, tc
	mov y+1, tc+1
	mov y+2, tc+2
	mov y+3, tc+3
	lcall add32       ;Th + Tc
	load_y(10000)
	lcall div32
	mov temp, x 
	mov temp+1, x+1
	mov temp+2, x+2
	mov temp+3, x+3

	Wait_Milli_Seconds(#250)
	
	WriteCommand(#0x80)
	Send_Constant_String(#P_STATE)
	WriteCommand(#0x89)
	WriteData(#' ')
	lcall Set_LEDS
	
	mov a, state
	cjne a, #0, next1
	sjmp FSM
next1:
	Set_Cursor(1,12)
	mov x, minutes
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
    lcall hex2bcd
	Display_BCD_DE1(bcd)
	WriteData(#':')
	mov x, seconds
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
    lcall hex2bcd
	Display_BCD_DE1(bcd)

	
;-------------------------STATE MACHINE ------------------------------------------------------
FSM:
	push ACC
	WriteCommand(#0x83)
	mov x, temp
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
    Display_BCD_DE1(bcd+1)
    Display_BCD_DE1(bcd)
    WriteData(#0b11011111)
    WriteData(#'C')
    WriteCommand(#0x83)
    WriteData(#' ')
	mov a, state
state0: ;This state just starts the solder reflow oven controller
    cjne a, #0, state1
    WriteCommand(#0x82)
    WriteData(#'0')
    WriteCommand(#0xC0)
    Send_Constant_String(#P_STATE_0)
    mov pwm_on+0, #low(1000)
	mov pwm_on+1, #high(1000)
    jb KEY.3, state0_done
    jnb KEY.3, $ ; Wait for key release
	;visually see in graph when FSM starts
	mov x, #0         
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
	lcall Display_BCD ; Display using the 7-segment displays
	lcall SendNumber  ; Send to serial port
	setb StateChanged
	mov CountBuzz, #0
	mov state, #1
state0_done:
	mov seconds, #0
	mov minutes, #0
	pop ACC
    ljmp forever
MOV_S2:
	ljmp state2
MOV_REG:
	ljmp REG
state1: ;This state is the Ramp to soak state
    cjne a, #1, MOV_S2
    WriteCommand(#0x82)
    WriteData(#'1')
    WriteCommand(#0xC0)
    Send_Constant_String(#P_STATE_1)
    ; 100% power
    mov pwm_on+1, #high(350) ;PWM of 100 is a diagonal line on the diagram (increasing temp)
	mov pwm_on+0, #low(350) ; goes on at 0
   	mov a, minutes
   	cjne a, #1, MOV_REG
   	mov a, seconds
   	cjne a, #0, MOV_REG
   	mov a, temp
   	clr c
   	subb a, AB_TEMP
   	jnc MOV_REG
   	WriteCommand(#0x80)
   	Send_Constant_String(#MY_CLEAR)
   	WriteCommand(#0x80)
   	Send_Constant_String(#ABORT_S1)
   	WriteCommand(#0xC0)
   	Send_Constant_String(#MY_CLEAR)
   	WriteCommand(#0xC0)
   	Send_Constant_String(#ABORT_S2)
	; warning beep
	setb StateChanged
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	mov BuzzerTime, #low(1000)
	mov BuzzerTime+1, #high(1000)
	lcall Wait_2_sec
   	WriteCommand(#0x80)
   	Send_Constant_String(#MY_CLEAR)
   	WriteCommand(#0xC0)
   	Send_Constant_String(#MY_CLEAR)

   	mov state, #0
   	sjmp state1_done
REG:
    mov a, temp_soak ; 150C
    clr c
    subb a, temp
    jnc state1_done
	setb StateChanged
	mov CountBuzz, #0
    mov state, #2
state1_done:
	pop ACC
	mov sec, #0
    ljmp forever
state2: ;This state is the preheat/soak state
    cjne a, #2, state3
    WriteCommand(#0x82)
    WriteData(#'2')
    WriteCommand(#0xC0)
    Send_Constant_String(#P_STATE_2)
    ; 20% pwm
    mov pwm_on+1, #high(775) ;PWM of 20 is a horizontal line on the diagram (const temp)
	mov pwm_on, #low(775)
    mov a, time_soak
    clr c
    subb a, sec
    jnc state2_done
	setb StateChanged
	Set_Cursor(2,1)
	Send_Constant_String(#CLEAR)
	mov CountBuzz, #0
    mov state, #3
state2_done:
	pop ACC
    ljmp forever
state3: ;This is the ramp to peak state
	cjne a, #3, state4
	WriteCommand(#0x82)
	WriteData(#'3')
	WriteCommand(#0xC0)
    Send_Constant_String(#P_STATE_3)
	; 100% power
    mov pwm_on+1, #high(315) ;PWM of 100 is a diagonal line on the diagram (increasing temp)
	mov pwm_on, #low(315)
	mov a, Temp_refl ; 217C
	clr c
	subb a, temp
	jnc state3_done
	setb StateChanged
	mov CountBuzz, #0
	Set_Cursor(2,1)
	Send_Constant_String(#CLEAR)
	mov state, #4
state3_done:
	pop ACC
	mov sec, #0
	ljmp forever
state5_C:
	ljmp state5
state4: ;This is the reflow state
	cjne a, #4, state5_C 
	WriteCommand(#0x82)
	WriteData(#'4')
	WriteCommand(#0xC0)
    Send_Constant_String(#P_STATE_4)
	; 20% pwm
    mov pwm_on+1, #high(705) ;PWM of 20 is a horizontal line on the diagram (const temp)
	mov pwm_on, #low(705)
	mov a, Time_refl
	clr c
	subb a, sec
	jnc state4_cont
	mov BuzzerTime, #low(5000)
	mov BuzzerTime+1, #high(5000)
	setb StateChanged
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	Set_Cursor(2,1)
	Send_Constant_String(#CLEAR)
	mov state, #5
	ljmp state4_done
state4_cont:
	clr c
	mov a, Temp_abrt ; 230C
	subb a, temp
	jnc next4
	sjmp hop
next4:
	ljmp state4_done
hop:
	WriteCommand(#0x80)
	Send_Constant_String(#MY_CLEAR)
	WriteCommand(#0x80)
	Send_Constant_String(#ABORT_T1)
	WriteCommand(#0xC0)
	Send_Constant_String(#MY_CLEAR)
	WriteCommand(#0xC0)
	Send_Constant_String(#ABORT_T2)
	lcall Wait_2_sec
	mov BuzzerTime, #low(5000)
	mov BuzzerTime+1, #high(5000)
	setb StateChanged
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	Set_Cursor(1,1)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2,1)
	Send_Constant_String(#CLEAR)
	mov state, #5
state4_done:
	pop ACC
	ljmp forever
state5: ; This is the cooling state
	; 0% power
	WriteCommand(#0x82)
	WriteData(#'5')
	WriteCommand(#0xC0)
    Send_Constant_String(#P_STATE_5)
	mov pwm_on+0, #low(1000)
	mov pwm_on+1,#high(1000)
	mov a, Temp_cool
	clr c
	subb a, temp
	cpl c
	jnc state5_done
	Set_Cursor(2,15)
	Send_Constant_String(#CLEAR)
	
	;first beep
	setb StateChanged
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	mov BuzzerTime, #low(1000)
	mov BuzzerTime+1, #high(1000)
	lcall Wait_2_sec
	
	;second beep
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	setb StateChanged
	lcall Wait_2_sec
	
	;third beep
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	setb StateChanged
	lcall Wait_2_sec
	
	;fourth beep
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	setb StateChanged
	lcall Wait_2_sec
	
	;fifth beep
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	setb StateChanged
	lcall Wait_2_sec
	
	;sixth beep
	mov CountBuzz, #0
	mov CountBuzz+1, #0
	setb StateChanged
	mov state, #0
state5_done:
	pop ACC
	ljmp forever
	
	
	
;-----------------Wait two seconds-----------------------------
Wait_2_sec:
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	ret
;-------------------------------------------------------------------------------

end