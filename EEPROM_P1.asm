; This 'equ' must match the hardware connection
FT93C66_CE   EQU P2.0 ; Connect to pin 1 of 93C66
FT93C66_MOSI EQU P2.1 ; Connect to pin 3 of 93C66 
FT93C66_MISO EQU P2.2 ; Connect to pin 4 of 93C66
FT93C66_SCLK EQU P2.3 ; Connect to pin 2 of 93C66 
; Connect pins 5 and 6 of the 93C66 to ground.  Connect pin 8 to 5V.
$include(FT93C66_DE1SoC.inc)

DSEG at 0x30
ASCII_Line:  ds 16

cseg

; This macro allows you to easily configure a pin as output.
; For example, to configure P2.0 as output call this macro like this:
; Configure_pin_as_output(P2, 0)
; Check the generated lst file to verify the expansion of this macro.
Configure_pin_as_output mac
orl %0MOD, #(1<<%1) ; Configure %0.%1 as output
endmac

Send_nl:
    mov a, #'\r'
    lcall putchar
    mov a, #'\n'
    lcall putchar
    ret

getbyte:
    push b
    ; Get most significant nibble
    lcall getchar_1
    anl a, #00011111B ; To deal with upercase/lowercase
    jb acc.4, getbyte_1
    add a, #9
getbyte_1:
    anl a, #00001111B
    swap a
    mov b, a
    ; Get least significant nibble
    lcall getchar_1
    anl a, #00011111B ; To deal with upercase/lowercase
    jb acc.4, getbyte_2
    add a, #9
getbyte_2:
    anl a, #00001111B
    orl a, b
    pop b
    ret

getaddress:
    push acc
    lcall getchar_1
    anl a, #00000001B
    mov dph, a
    lcall getbyte
    mov dpl, a
    pop acc
    ret

hex: db '0123456789abcdef',0

putbyte:
    ; Preserve used registers
    push acc
    push dpl
    push dph
    ; Display a hex byte 
    push acc
    mov dptr, #hex
    swap a
    anl a, #00001111B
    movc a,@a+dptr
    lcall putchar
    pop acc
    anl a, #00001111B
    movc a,@a+dptr
    lcall putchar
    ; Restore used registers
    pop dph
    pop dpl
    pop acc
    ret
    
putadd:
    ; Preserve used registers
    push acc
    push dpl
    push dph
    ; Display a hex address     
    mov a, dph
    push dpl
    mov dptr, #hex
    anl a, #00001111B
    movc a,@a+dptr
    lcall putchar
    pop acc
    lcall putbyte
    ; Restore used registers
    pop dph
    pop dpl
    pop acc
    ret
    
Add_msg: DB '\r\nAddress: ', 0
Val_msg: DB '\r\nValue: ', 0

Write_Byte:
    mov dptr, #Add_msg
    lcall SendString
    lcall getaddress
    push dpl
    push dph
    mov dptr, #Val_msg
    lcall SendString
    lcall getbyte
    pop dph
    pop dpl
    lcall FT93C66_Write_Enable
    lcall FT93C66_Write
    lcall FT93C66_Write_Disable
    lcall Send_nl
    ret

Display_memory:
    mov dptr, #0
Display_memory_0:
    mov a, dpl
    anl a, #0x0f
    jnz Display_memory_1
    lcall Send_nl
    lcall putadd
    mov a, #':'
    lcall putchar
    mov a, #' '
    lcall putchar
Display_memory_1:
    lcall FT93C66_Read
    mov b, a ; Make a copy
    
    ; Fill the line of ASCII characters
    mov a, dpl
    anl a, #00001111B
    add a, #ASCII_Line
    mov R0, a
    
    ; If the value is larger than 0x7e display a '.'
    mov a, b
    clr c
    subb a, #0x7e
    jnc adddot
    
    ; If the value is smaller than 0x20 display a '.'
    mov a, b
    clr c
    subb a, #0x20
    jc adddot
    ; The value read is betwee 0x20 and 0x7e, display as ASCII
    mov @R0, b
    sjmp good_ascii
adddot:
    mov @R0, #'.'
good_ascii:

    mov a, b
    lcall putbyte
    mov a, #' '
    lcall putchar
    inc dptr
    
    ; Display the line of ASCII characters
    mov a, dpl
    anl a, #00001111B
    jnz skip_ascii
    mov R0, #ASCII_Line
    mov R1, #16
out_ascii:
    mov a, @R0
    lcall putchar
    inc R0
    djnz R1, out_ascii
   
skip_ascii:
    mov a, dph
    jb acc.1, Display_memory_2
    sjmp Display_memory_0
Display_memory_2:      
    lcall Send_nl
    lcall Send_nl
    ret

Erase_memory:
    lcall FT93C66_Write_Enable
    lcall FT93C66_Erase_All
    lcall FT93C66_Write_Disable
    lcall Send_nl
    ret

Fill_memory:
    mov dptr, #Val_msg
    lcall SendString
    lcall getbyte
    lcall FT93C66_Write_Enable
    lcall FT93C66_Write_All
    lcall FT93C66_Write_Disable
    lcall Send_nl
    ret
    
test_msg: db '\r\nTest patern ', 0
error_msg: db ' error', 0
ok_msg: db 'Pass!', 0

test_byte mac
   mov b, #low(%0)
   lcall ?test_byte
endmac

?test_byte:
    mov dptr, #test_msg
    lcall SendString
    mov a, b
    lcall putbyte
    mov a, #':'
    lcall putchar
    mov a, #' '
    lcall putchar
    
	mov R6, #0
	mov R7, #0
	mov a, b
    lcall FT93C66_Write_All
    mov dptr, #0
next_byte:
    lcall FT93C66_Read
    clr c
    subb a, b
    jnz inc_error
    sjmp check_end
inc_error:
    mov R5, #1 ; Set error flag
    inc R6
    mov a, R6
    jnz check_end
    inc R7
check_end:
    inc dptr
    mov a, dph
    jnb acc.1, next_byte ; This checks if dptr is 0x0200
    
    mov a, R6
    orl a, R7
    jz all_ok
    
    mov a, #'0'
    lcall putchar
    mov a, #'x'
    lcall putchar
    mov dpl, R6
    mov dph, R7
    lcall putadd
    mov dptr, #error_msg
    lcall SendString
    cjne R6, #0x01, print_s
    cjne R7, #0x00, print_s
    ret
    
print_s:    
    mov a, #'s'
    lcall putchar
    ret
    
all_ok:
    mov dptr, #ok_msg
    lcall SendString
    ret

Warn_Msg: db '\r\n\r\nWARNING: MEMORY ERRORS DETECTED!\r\n', 0

test_memory:
    lcall FT93C66_Write_Enable
    
    mov R5, #0 ; Error flag

	test_byte(0x00)
	test_byte(0x55)
	test_byte(0xaa)
	test_byte(0x0f)
	test_byte(0xf0)
	test_byte(0x5a)
	test_byte(0xa5)
	
	test_byte(0x01)
	test_byte(0x02)
	test_byte(0x04)
	test_byte(0x08)
	test_byte(0x10)
	test_byte(0x20)
	test_byte(0x40)
	test_byte(0x80)
	
	test_byte(not(0x01))
	test_byte(not(0x02))
	test_byte(not(0x04))
	test_byte(not(0x08))
	test_byte(not(0x10))
	test_byte(not(0x20))
	test_byte(not(0x40))
	test_byte(not(0x80))

    lcall FT93C66_Write_Disable
    
    cjne R5, #0x01, test_memory_done
    mov dptr, #Warn_Msg
    lcall SendString
    
test_memory_done:
    lcall Send_nl
    lcall Send_nl
    ret

String_msg:  db '\r\nType string (Enter to finish): ', 0

Write_String:
    lcall FT93C66_Write_Enable
    mov dptr, #Add_msg
    lcall SendString
    lcall getaddress
    push dpl
    push dph
    mov dptr, #String_msg
    lcall SendString
    pop dph
    pop dpl
    
Write_String_loop:
    lcall getchar_1
    mov b, a
    xrl a, #'\r'
    jz Write_String_done
    xrl a, #'\n'
    jz Write_String_done
    mov a, b
    lcall FT93C66_Write
    inc dptr
    sjmp Write_String_loop
    
Write_String_done:
    lcall FT93C66_Write_Disable
    lcall Send_nl
    ret
             
menu:
    DB 'FT93C66A EEPROM test\r\n'
    DB '   1) Write byte\r\n'
    DB '   2) Display memory\r\n'
    DB '   3) Erase memory\r\n'
    DB '   4) Fill memory\r\n'
    DB '   5) Test memory\r\n'
    DB '   6) Write string\r\n'
    DB 'Option: '
    DB 0

Test_menu:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov LEDRA, #0
    mov LEDRB, #0
    
    ; CE, MOSI, and SCLK configured as ouputs:  
    Configure_pin_as_output(P2,0)
    Configure_pin_as_output(P2,1)
    Configure_pin_as_output(P2,3)

forever_1:
    mov dptr, #menu
    lcall SendString
    lcall getchar_1
    push acc
    lcall Send_nl
    pop acc
Option1:    
    cjne a, #'1', Option2
    lcall Write_Byte
    ljmp forever_1
Option2:    
    cjne a, #'2', Option3
    lcall Display_memory
    ljmp forever_1
Option3:    
    cjne a, #'3', Option4
    lcall Erase_memory
    ljmp forever_1
Option4:    
    cjne a, #'4', Option5
    lcall Fill_memory
    ljmp forever_1
Option5:    
    cjne a, #'5', Option6
    lcall Test_memory
    ljmp forever_1
Option6:    
    cjne a, #'6', done
    lcall Write_String

done:    
    ljmp forever_1

Load_Defaults: ; Load defaults if keys are incorrect
    mov temp_soak, #150
    mov Time_soak, #80
    mov Temp_refl, #219
    mov Temp_abrt, #240
    mov Time_refl, #45
    mov Temp_cool, #60
    ret


Save_Configuration:
    lcall FT93C66_Write_Enable
    mov DPTR, #0
    ; Save variables
    mov a, temp_soak
    lcall FT93C66_Write
    inc DPTR	
    mov a, Time_soak
    lcall FT93C66_Write 
    inc DPTR	
    mov a, Temp_refl
    lcall FT93C66_Write
    inc DPTR
    mov a, Temp_abrt
    lcall FT93C66_Write	
    inc DPTR	
    mov a, Time_refl
    lcall FT93C66_Write
    inc DPTR	
    mov a, Temp_cool
    lcall FT93C66_Write
    inc DPTR
    mov a, #0x55 ; First key value
    lcall FT93C66_Write
    inc DPTR	
    mov a, #0xAA ; Second key value
    lcall FT93C66_Write
    lcall FT93C66_Write_Disable
    ret

Load_Configuration:
    mov dptr, #0x0006 ;First key value location.  Must be 0x55
    lcall FT93C66_Read
    cjne a, #0x55, Load_Defaults
    inc dptr ; Second key value location.  Must be 0xaa
    lcall FT93C66_Read
    cjne a, #0xaa, Load_Defaults

    ; Keys are good.  Load saved values.
    mov dptr, #0
    lcall FT93C66_Read
    mov temp_soak, a
    inc dptr
    lcall FT93C66_Read
    mov Time_soak, a
    inc dptr
    lcall FT93C66_Read
    mov Temp_refl, a
    inc dptr
    lcall FT93C66_Read
    mov Temp_abrt, a
    inc dptr
    lcall FT93C66_Read
    mov Time_refl, a
    inc dptr
    lcall FT93C66_Read
    mov Temp_cool, a
    
    
    ret
    