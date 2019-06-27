$NOLIST
$MODDE1SOC
$LIST

org 0000H
   ljmp MainProgram

; This 'equ' must match the hardware connection
FT93C66_CE   EQU P2.0 ; Connect to pin 1 of 93C66
FT93C66_MOSI EQU P2.1 ; Connect to pin 3 of 93C66 
FT93C66_MISO EQU P2.2 ; Connect to pin 4 of 93C66
FT93C66_SCLK EQU P2.3 ; Connect to pin 2 of 93C66
; Connect pins 5 and 6 of the 93C66 to ground.  Connect pin 8 to 5V.

$include(FT93C66_DE1SoC.inc)

CSEG

; This macro allows you to easily configure a pin as output.
; For example, to configure P2.0 as output call this macro like this:
; Configure_pin_as_output(P2, 0)
; Check the generated lst file to verify the expansion of this macro.
Configure_pin_as_output mac
orl %0MOD, #(1<<%1) ; Configure %0.%1 as output
endmac

MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov LEDRA, #0
    mov LEDRB, #0
    
    ; CE, MOSI, and SCLK configured as ouputs:  
    Configure_pin_as_output(P2,0)
    Configure_pin_as_output(P2,1)
    Configure_pin_as_output(P2,3)
    
    lcall FT93C66_INIT_SPI
        
    lcall FT93C66_Write_Enable
    
    mov dptr, #0x10 ; Random memory location to test
    mov a, #0x55 ; Value to write at location
    lcall FT93C66_Write
    lcall FT93C66_Read
    cjne a, #0x55, it_failed ; Read back and check if the location was written correctly

    ; Test another address
    mov dptr, #0x33 ; Random memory location to test
    mov a, #0xaa ; Value to write at location
    lcall FT93C66_Write
    lcall FT93C66_Read
    cjne a, #0xaa, it_failed ; Read back and check if the location was written correctly
 
 	mov HEX3, #00001100B ;  Letter P
 	mov HEX2, #00001000B ;  Letter A
 	mov HEX1, #00010010B ;  Letter S
 	mov HEX0, #00010010B ;  Letter S
	sjmp done
	
it_failed:
 	mov HEX3, #00001110B ;  Letter F
 	mov HEX2, #00001000B ;  Letter A
 	mov HEX1, #01111001B ;  Letter I
 	mov HEX0, #01000111B ;  Letter L
    
done:    
    sjmp $ ; This is equivalent to 'forever: sjmp forever'
    
END
