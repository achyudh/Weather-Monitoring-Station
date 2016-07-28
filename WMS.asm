#make_bin#

#LOAD_SEGMENT=0100h#
#LOAD_OFFSET=0000h#

#CS=0000h#
#IP=1000h#

#DS=0200h#
#ES=0000h#
#SS=0000h#
#SP=FFFEh#

#AX=0000h#
#BX=0000h#
#CX=0000h#
#DX=0000h#
#SI=0000h#
#DI=0000h#
#BP=0000h#          

; Initialising IVT Entries
mov ax, offset ISR0
mov cs:[00200h], ax
mov ax, 0100h
mov cs:[00202h], ax

mov ax, offset ISR1
mov cs:[00204h], ax
mov ax, 0100h
mov cs:[00206h], ax

mov ax, offset ISR2
mov cs:[00208h], ax
mov ax, 0100h
mov cs:[0020Ah], ax

mov ax, offset ISR3
mov cs:[0020Ch], ax
mov ax, 0100h
mov cs:[0020Eh], ax

; Allocating Space for IVT
jmp START
db 512 dup(0)

; Initialising Data Segment
SelectTHP db 00h
ButtonFlag db 00h
NumReadings db 00h
TempOutBuffer db 40 dup(5)
HumiOutBuffer db 40 dup(6)
PresOutBuffer db 40 dup(7)

Remainder db 00h
Quotient db 00h
Divisor dw 00h

PortVal8255A db 00h
PortVal8255B db 00h

TempFirstHourFlag dw 00h
TempQueue db 12 dup(0)
TempInsertPointer dw 00h

HumiFirstHourFlag dw 00h
HumiQueue db 12 dup(0)
HumiInsertPointer dw 00h

PresFirstHourFlag dw 00h
PresQueue db 12 dup(0)
PresInsertPointer dw 00h

; Initialising Devices
START:
cli
Addr8259 equ 4000h
Addr8253 equ 4030h
Addr8255A equ 4010h
Addr8255B equ 4020h

AddrINT equ 4020h
AddrADCOP equ 4022h
AddrADCIP equ 4024h

Addr7447T equ 4010h
Addr7447P equ 4012h
Addr7447H equ 4014h

; Initialising PIC 8259
; Send ICW1
mov al, 00010011b               ; Single 8259, Edge Triggered
mov dx, Addr8259+00h            ; DX = 1st address of 8259
out dx, al

; Send ICW2
mov al, 10000000b               ; Starting address of interrupts is 80h
mov dx, Addr8259+02h            ; DX = 2nd address of 8259
out dx,al

; ICW3 is not required to be sent in a single 8259 configuration

; Send ICW4
mov al, 00000011b               ; Automatic EOI is enabled
out dx,al
 
; Send OCW1
mov al, 11111000b               ; Only IR0, IR1, IR2 are used
out dx, al  

; Initialising PPC 8255-A 
mov al, 10000000b
mov dx, Addr8255A+06h
out dx, al

; Initialising PPC 8255-B
mov al, 10010010b
mov dx, Addr8255B+06h
out dx, al  

; Initialising PIT 8253

; Program counter 0
mov al, 00010110b               ; Binary mode 3 - Square wave 
mov dx, Addr8253+06h
out dx, al
mov al, 02h                     ; Load value 02h into the counter to get 1 MHz
mov dx, Addr8253+00h     
out dx, al

; Program counter 1
mov al, 01110110b               ; Binary Mode 3 - Square wave
mov dx, Addr8253+06h
out dx, al
mov al, 24h                     ; Count = 62500 = 0F424h to get 16 Hz output
mov dx, Addr8253+02h
out dx, al
mov al, 0F4h
out dx, al

; Program counter 2
mov al, 10110100b               ; Binary Mode 2 - Rate generator
mov dx, Addr8253+06h            
out dx, al
; mov al, 0C0h                    ; Count = 4800 = 12C0h to get 300 secs output
mov al, 10h                    ; Count = 4800 = 12C0h to get 300 secs output
mov dx, Addr8253+04h
out dx, al
mov al, 00h
out dx, al

; Display Initial Values

jmp InitDisplay

mov SelectTHP,00h               ; Send SOC to ADC for temperature
int 81h

WaitForTempEOC1:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForTempEOC1

WaitForTempEOC2:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForTempEOC2

mov SelectTHP,00h               ; Store the value of Temperature
int 83h

mov SelectTHP,01h               ; Send SOC to ADC for humidity
int 81h

WaitForHumiEOC1:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForHumiEOC1

WaitForHumiEOC2:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForHumiEOC2

mov SelectTHP,01h               ; Store the value of Humidity
int 83h

mov SelectTHP,02h               ; Send SOC to ADC for pressure
int 81h

WaitForPresEOC1:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForPresEOC1

WaitForPresEOC2:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForPresEOC2

mov SelectTHP,02h               ; Store the value of Pressure
int 83h

mov SelectTHP,00h
int 82h

mov SelectTHP,01h
int 82h

mov SelectTHP,11h
int 82h


; Poll Port A of 8255-B to check for button or 5-min interrupts
Poll8255B:

mov al, ButtonFlag                  ; Check if button is pressed
cmp al, 01h
jnz CheckIntr                       ; If not, keep polling
mov ButtonFlag, 00h

; If button is pressed, send EOC to ADC

mov SelectTHP,00h               ; Send SOC to ADC for temperature
int 81h

WaitForTempEOC10:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForTempEOC10

WaitForTempEOC20:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForTempEOC20

mov SelectTHP,00h               ; Store the value of Temperature
int 83h

mov SelectTHP,01h               ; Send SOC to ADC for humidity
int 81h

WaitForHumiEOC10:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForHumiEOC10

WaitForHumiEOC20:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForHumiEOC20

mov SelectTHP,01h               ; Store the value of Humidity
int 83h

mov SelectTHP,02h               ; Send SOC to ADC for pressure
int 81h

WaitForPresEOC10:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForPresEOC10

WaitForPresEOC20:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForPresEOC20

mov SelectTHP,02h                   ; Store the value of Pressure
int 83h

mov SelectTHP,00h
int 82h

mov SelectTHP,01h
int 82h

mov SelectTHP,11h
int 82h

CheckIntr:
mov dx, AddrINT
in al, dx

mov bl, al
and bl, 02h
jz ButtonIntr

mov bl, al
and bl, 04h
jz FiveMinIntr

jmp Poll8255B

ButtonIntr:
in al, dx
and al, 01h
jz ButtonIntr

int 80h
jmp Poll8255B

FiveMinIntr:
in al, dx
and al, 02h
jz FiveMinIntr

mov SelectTHP,00h               ; Send SOC to ADC for temperature
int 81h

WaitForTempEOC11:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForTempEOC11

WaitForTempEOC21:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForTempEOC21

mov SelectTHP,00h               ; Store the value of Temperature
int 83h

mov SelectTHP,01h               ; Send SOC to ADC for humidity
int 81h

WaitForHumiEOC11:                ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForHumiEOC11

WaitForHumiEOC21:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForHumiEOC21

mov SelectTHP,01h               ; Store the value of Humidity
int 83h

mov SelectTHP,02h               ; Send SOC to ADC for pressure
int 81h

WaitForPresEOC11:               ; Check if EOC is already active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jnz WaitForPresEOC11

WaitForPresEOC21:                ; Wait for EOC to become active
mov dx, AddrINT
in al, dx
mov bl, al
and bl, 01h
jz WaitForPresEOC21

mov SelectTHP,02h               ; Store the value of Pressure
int 83h

inc NumReadings

cmp NumReadings,12h
jnz Poll8255B

; Call interrupts to display data
mov SelectTHP, 00h
int 82h                         
mov SelectTHP, 01h
int 82h
mov SelectTHP, 11h
int 82h

mov NumReadings, 00h             ; Reset the count of readings
jmp Poll8255B

jmp EndProgram

InitDisplay:
lea di, TempOutBuffer
mov ax, 0703h
stosw
mov ax, 0105h
stosw
lea di, HumiOutBuffer
mov ax, 0906h
stosw
mov ax, 0707h
stosw
lea di, PresOutBuffer
mov ax, 0001h
stosw
mov ax, 0204h
stosw
call OutputToDisplay


; Macro Definitions

PushAll macro
    push ax
    push bx
    push cx
    push dx
    push si
    push di
endm

PopAll macro
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
endm

SetPins8255A macro value
    PushAll
    mov al, value
    mov bl, PortVal8255A
    or  al, bl
    mov dx, Addr8255A+04h
    out dx, al
    mov bl, al
    mov PortVal8255A, bl
    PopAll            
endm

ResetPins8255A macro value
    PushAll
    mov al, value
    not al
    mov bl, PortVal8255A
    and al, bl
    mov dx, Addr8255A+04h
    out dx, al
    mov bl, al
    mov PortVal8255A, bl
    PopAll             
endm

SetPins8255B macro value
    PushAll
    mov al, value
    mov bl, PortVal8255B
    or  al, bl
    mov dx, AddrADCIP
    out dx, al
    mov bl, al
    mov PortVal8255B, bl
    PopAll            
endm

ResetPins8255B macro value
    PushAll
    mov al, value
    not al
    mov bl, PortVal8255B
    and al, bl
    mov dx, AddrADCIP
    out dx, al
    mov bl, al
    mov PortVal8255B, bl
    PopAll             
endm

; Subroutine Definitions

DivideBy proc near 
    PushAll
    mov cx, 00
    mov bx, Divisor
    
    RepeatSubtr:
    sub ax, bx
    inc cx
    cmp ax, 0
    jge RepeatSubtr

    dec cx
    add ax, bx
    
    mov Remainder, al
    mov Quotient, cl
    PopAll
    ret
DivideBy endp

Delay20ms proc near 
    push cx 
    mov cx,6667
    l2: nop 
    nop 
    loop l2 
    pop cx 
    ret 
Delay20ms endp

ConvertTemp proc near    
    ; Scale the temperature from 00h-FFh to (-20)-50 C
    mov ah, 00h
    mov al, Quotient
    mov bl, 70d
    mul bl
    mov bl, 0FFh
    div bl
    add ax, -20d
    
    ; TODO: Implement Negative Temperature LED

    ; Split AX to two digits in AL (Upper), AH (Lower)                    
    mov ah, 00h
    mov bl, 10d
    div bl
    
    lea si, TempOutBuffer
    mov [si], al
    mov [si+1], ah 
    
    mov al, Remainder
    mov ah, 00h
    
    mov bx, 100d
    mul bx
    mov bl, 12d                               ; Convert modulo-12 remainder to decimal
    div bl
    
    mov ah, 00h
    mov bl, 70d
    mul bl
    mov bl, 0FFh
    div bl
    add ax, -20d

    mov ah, 00h
    mov bl, 10d
    div bl
    
    mov [si+2], al
    mov [si+3], ah
   
    ret
ConvertTemp endp

ConvertHumi proc near
    ; Scale the humidity from 00h-FFh to 0-100%
    mov ah, 00h
    mov al, Quotient
    mov bl, 99d            
    mul bl
    mov bl, 0FFh         
    div bl
    
    ; TODO: Implement 100% Humidity LED

    ; Split AX to two digits in AL (Upper), AH (Lower)                    
    mov ah, 00h
    mov bl, 10d
    div bl
    
    lea si, HumiOutBuffer
    mov [si], al
    mov [si+1], ah 
    
    mov al, Remainder
    mov ah, 00h
    
    mov bx, 100d
    mul bx
    mov bl, 12d                         ; Convert modulo-12 remainder to decimal
    div bl
    
    mov ah, 00h
    mov bl, 99d
    mul bl
    mov bl, 0FFh
    div bl

    mov ah, 00h
    mov bl, 10d
    div bl
    
    mov [si+2], al    
    mov [si+3], ah
   
    ret
ConvertHumi endp

ConvertPres proc near
    ; Scale the pressure from 00h-FFh to 800 to 1100 millibar
    lea si, PresOutBuffer
    lea di, PresOutBuffer

    mov al, Remainder
    mov ah, 00h
    
    mov bx, 100d
    mul bx
    mov bl, 12d                          ; Convert modulo-12 remainder to decimal
    div bl
    
    mov ah, 00h
    mov bl, 110d            
    mul bl
    mov bl, 0FFh        
    div bl
    stosw

    mov ah, 00h
    mov al, Quotient
    mov bl, 110d            
    mul bl
    mov bl, 0FFh        
    div bl
    
    mov bx, ax
    lodsw
    add ax, bx

    ; Split AX to four digits               
    mov bx, ax
    mov ah, 00h
    mov cl, 08h
    rol bx, cl
    mov bh, 00h
    
    mov cl, 10d
    div cl
    mov [si+2], al
    mov [si+3], ah 

    mov ah, 00h
    mov al, bl
    mov bl, 10d
    div bl
    
    mov [si], al                    
    mov [si+1], ah
   
    ret
ConvertPres endp

OutputToDisplay proc near
Scan0:
    lea si, TempOutBuffer
    lea di, PresOutBuffer
    lea bx, HumiOutBuffer

    SetPins8255A 11100000b
    ResetPins8255A 00010000b
    mov dx, Addr7447T
    mov al, [si]  
    out dx, al
    mov dx, Addr7447P
    mov al, [di]
    out dx, al
    mov dx, Addr7447H
    mov al, [bx]
    ;out dx, al
    call Delay20ms

    SetPins8255A 11010000b
    ResetPins8255A 00100000b
    mov dx, Addr7447T
    mov al, [si+1]  
    out dx, al
    mov dx, Addr7447P
    mov al, [di+1]
    out dx, al
    mov dx, Addr7447H
    mov al, [bx+1]
    ;out dx, al
    call Delay20ms

    SetPins8255A 10110000b
    ResetPins8255A 01000000b
    mov dx, Addr7447T
    mov al, [si+2]  
    out dx, al
    mov dx, Addr7447P
    mov al, [di+2]
    out dx, al
    mov dx, Addr7447H
    mov al, [bx+2]
    ;out dx, al
    call Delay20ms

    SetPins8255A 01110000b
    ResetPins8255A 10000000b
    mov dx, Addr7447T
    mov al, [si+3]  
    out dx, al
    mov dx, Addr7447P
    mov al, [di+3]
    out dx, al
    mov dx, Addr7447H
    mov al, [bx+3]
    ;out dx, al
    call Delay20ms

    jmp Scan0
    ret

OutputToDisplay endp

; ISR Definitions

ISR0:                               ; Button Interrupt
    mov ButtonFlag, 01h
iret

ISR1:                               ; 5 minute Interrupt
    ; Enable Temperature Sensors
    ResetPins8255B 08h                ; Make OE high for ADC
    
    cmp SelectTHP, 00h
    jnz StartHumiditySensor

    ; Select Address 000h
    ResetPins8255B 01h
    ResetPins8255B 02h
    ResetPins8255B 04h

    SetPins8255B 20h                ; Give a high to low transition on ALE
    call Delay20ms
    ResetPins8255B 20h

    SetPins8255B 10h                ; Give a high to low transiton on SOC
    call Delay20ms
    ResetPins8255B 10h

    jmp EndOfISR1

    StartHumiditySensor:
    ; Enable Humidity Sensors
    cmp SelectTHP, 01h
    jnz StartPressureSensor

    ; Select Address 001h
    SetPins8255B 01h
    ResetPins8255B 02h
    ResetPins8255B 04h

    SetPins8255B 20h                ; Give a high to low transition on ALE
    call Delay20ms
    ResetPins8255B 20h

    SetPins8255B 10h                ; Give a high to low transiton on SOC
    call Delay20ms
    ResetPins8255B 10h

    jmp EndOfISR1

    StartPressureSensor:
    ; Enable Pressure Sensors
    ; Select Address 010h
    ResetPins8255B 01h
    SetPins8255B 02h
    ResetPins8255B 04h

    SetPins8255B 20h                ; Give a high to low transition on ALE
    call Delay20ms
    ResetPins8255B 20h

    SetPins8255B 10h                ; Give a high to low transiton on SOC
    call Delay20ms
    ResetPins8255B 10h

EndOfISR1:
iret

ISR2:                               ; 1 hour Interrupt
    cmp SelectTHP,00h
    jnz OutputHumiValue

    mov bx, 00h
    mov cx, 12d
    lea si, TempQueue

    TempSum:
    mov dl, [si]
    mov dh, 00h
    add bx, dx
    inc si
    dec cx
    jnz TempSum
    mov ax, bx

    mov dx, TempFirstHourFlag
    cmp dx, 1
    jnz TempFirstHour

    mov Divisor, 12d
    jmp TempAvg

    TempFirstHour:
    mov dx, TempInsertPointer
    mov Divisor, dx

    TempAvg:
    call DivideBy

    call ConvertTemp
    mov dx, Addr7447T
    call OutputToDisplay

    jmp EndOfISR2

    OutputHumiValue:
    cmp SelectTHP,01h
    jnz OutputPresValue

    mov bx, 00h
    mov cx, 12d
    lea si, HumiQueue

    HumiSum:
    mov dl, [si]
    mov dh, 00h
    add bx, dx
    inc si
    dec cx
    jnz HumiSum
    mov ax, bx

    mov dx, HumiFirstHourFlag
    cmp dx, 1
    jnz HumiAvg

    mov Divisor, 12d
    jmp HumiAvg

    HumiFirstHour:
    mov dx, HumiInsertPointer
    mov Divisor, dx

    HumiAvg:
    call DivideBy

    call ConvertHumi
    mov dx, Addr7447H
    call OutputToDisplay

    jmp EndOfISR2

    OutputPresValue:
    mov bx, 00h
    mov cx, 12d
    lea si, PresQueue

    PresSum:
    mov dl, [si]
    mov dh, 00h
    add bx, dx
    inc si
    dec cx
    jnz PresSum
    mov ax, bx

    mov dx, PresFirstHourFlag
    cmp dx, 1
    jnz PresFirstHour

    mov Divisor, 12d
    jmp PresAvg

    PresFirstHour:
    mov dx, PresInsertPointer
    mov Divisor, dx

    PresAvg:
    call DivideBy

    call ConvertPres
    mov dx, Addr7447P
    call OutputToDisplay

EndOfISR2:
iret

ISR3:                                   ; EOC Interrupt
    SetPins8255B 08h                    ; Set OE High

    cmp SelectTHP,00h
    jnz StoreHumidityVal

    mov dx, AddrADCOP
    in al, dx

    ; Check if it is the first hour
    ; If TempFirstHourFlag = 0, then it is the first hour.
    cmp TempFirstHourFlag, 0
    jnz TempNotFirstHour

    mov bx, TempInsertPointer
    lea si, TempQueue 
      
    mov [si+bx], al
    inc bx
    mov TempInsertPointer, bx
    cmp bx, 12
    jnz WriteTempInsPtr

    mov TempFirstHourFlag, 1
    mov TempInsertPointer, 0
    jmp EndOfISR3

    TempNotFirstHour: 
    mov bx, TempInsertPointer
    lea si, TempQueue
    mov [si+bx], al
    inc bx
    cmp bx, 12
    jnz WriteTempInsPtr
    mov bx, 0

    WriteTempInsPtr: 
    mov TempInsertPointer, bx
    jmp EndOfISR3

    StoreHumidityVal:      
    cmp SelectTHP,01h
    jnz StorePressureVal
    
    mov dx, AddrADCOP
    in al, dx

    ; Check if it is the first hour
    ; If HumiFirstHourFlag = 0, then it is the first hour.
    cmp HumiFirstHourFlag, 0
    jnz HumiNotFirstHour

    mov bx, HumiInsertPointer
    lea si, HumiQueue 
      
    mov [si+bx], al
    inc bx
    mov HumiInsertPointer, bx
    cmp bx, 12
    jnz WriteHumiInsPtr

    mov HumiFirstHourFlag, 1
    mov HumiInsertPointer, 0
    jmp EndOfISR3

    HumiNotFirstHour:
    mov bx, HumiInsertPointer
    lea si, HumiQueue
    mov [si+bx], al
    inc bx
    cmp bx, 12
    jnz WriteHumiInsPtr
    mov bx, 0

    WriteHumiInsPtr:   
    mov HumiInsertPointer, bx
    jmp EndOfISR3

    StorePressureVal:
    mov dx, AddrADCOP
    in al, dx

    ; Check if it is the first hour
    ; If PresFirstHourFlag = 0, then it is the first hour.
    cmp PresFirstHourFlag, 0
    jnz PresNotFirstHour
    
    mov bx, PresInsertPointer
    lea si, PresQueue 
      
    mov [si+bx], al
    inc bx
    mov PresInsertPointer, bx
    cmp bx, 12
    jnz WritePresInsPtr

    mov PresFirstHourFlag, 1
    mov PresInsertPointer, 0
    jmp EndOfISR3

    PresNotFirstHour:   
    mov bx, PresInsertPointer
    lea si, PresQueue
    mov [si+bx], al
    inc bx
    cmp bx, 12
    jnz WriteHumiInsPtr
    mov bx, 0

    WritePresInsPtr:   
    mov PresInsertPointer, bx

EndOfISR3:
    ResetPins8255B 08h                  ; Make OE low on ADC
iret

EndProgram:
hlt