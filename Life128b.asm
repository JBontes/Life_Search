FORMAT ELF64
;FORMAT WIN64

section '.data'
 ;start: dw 0xEEEE, 0xEEEE,0xEEEE, 0xEEEE,0xEEEE, 0xEEEE,0xEEEE, 0xEEEE 
  start:dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff;dw 0x3333,0x3333,0x3333,0x3333,0x3333,0x3333,0x3333,0x3333
  nw:   dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff
  n:    dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff;dw 0x5555,0x5555,0x5555,0x5555,0x5555,0x5555,0x5555,0x5555
  w:    dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff;dw 1,1,1,1,1,1,1,1
  se:   dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff
  s:    dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff;dw 0x5555,0x5555,0x5555,0x5555,0x5555,0x5555,0x5555,0x5555
  e:    dw 0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFff,0xFFfe;dw 1,1,1,1,1,1,1,1
  result:dw 0,0,0,0,0,0,0,0

section '.text' executable
public main
main:
    mov rbp, rsp; for correct debugging
    mov ebx,111         ;start marker bytes
    db 0x64,0x67,0x90   ;start marker bytes
    ;parameters
    ;rcx: main
    ;rdx: N
    ;r8: W
    ;r9: NW
    ; A cell looks like this:
    ;   BCD   123
    ;   AxA   405
    ;   BCD   678
    ; we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    movdqu xmm15,[rcx]          ;***** xmm15 8-D'
    movdqu xmm1,[rdx]           ;N
    movdqu xmm2,[r8]            ;W
    movdqu xmm3,[r9]            ;NW
    ;now create 9 planes, each one slightly shifted.
    ;xmm0 is the center plane.
    vpsrldq xmm3,xmm3,16-4       ;keep the bottom 2 rows of NW and shift them to the top
    vpsrldq xmm6,xmm1,16-2       ;N5           keep the bottom 1 rows of N and shift them to the top.
    vpsrldq xmm1,xmm1,16-4       ;N3           keep the bottom 2 rows of N and shift them to the top.
    vpsrlw xmm2,xmm2,14          ;W6           keep the 2 rightmost columns of W
    vpsrlw xmm3,xmm3,14          ;NW1          keep the 2 rightmost columns of NW
    vpslldq xmm5,xmm15,4         ;main3        remove the bottom 2 rows from main
    vpslldq xmm4,xmm15,2         ;main5        remove the bottom 1 row from main
    vpxor xmm14,xmm1,xmm5        ;***** xmm14 3 - D    2 rows N +14 rows main
    vpxor xmm13,xmm4,xmm6        ;***** xmm13 5 - A'   1 row N  +15 rows main
    ;we are done with N, xmm1 and xmm6
    vpsrlw xmm1,xmm2,1           ;W7           remove an extra column from W
    vpsllw xmm7,xmm15,1          ;main7        Shift main right
    vpsllw xmm8,xmm13,1          ;main0+N0     Shift main+N1 right
    vpsllw xmm9,xmm14,1          ;main2+N2     Shift mainn+N2 right
    vpxor xmm12,xmm7,xmm1        ;***** xmm12 7 - C Main7+W7
    vpsllw xmm7,xmm7,1           ;main6       Shift main right
    vpxor xmm11,xmm7,xmm2        ;***** xmm11 6 - B' Main6+W6
    vpslldq xmm10,xmm11,2        ;main4+W4    Shift Main6W6 down
    vpsrldq xmm7,xmm3,2          ;NW4         Shift NW1 up (only one row)
    vpsllw xmm6,xmm6,2           ;N4          Shift N3 right
    vpxor xmm10,xmm10,xmm7       ;main4+W4+NW4
    vpxor xmm10,xmm10,xmm6       ;***** xmm10 4 - A
    vpslldq xmm1,xmm1,2          ;W0          Shift W7 down 1 row
    vpsrlw xmm7,xmm7,1           ;NW0         Shift NW4 left (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         ;main0+N0+W0
    vpxor xmm0,xmm0,xmm7         ;***** xmm0 0 - x
    vpslldq xmm1,xmm2,4          ;W1          Shift W down 2 rows
    vpsllw xmm8,xmm9,1           ;main1+N1    Shift Main2N2 right 1 column
    vpxor xmm8,xmm8,xmm1         ;main1+N1+W1 Combine with W
    vpxor xmm8,xmm8,xmm3         ;**** xmm8 1 - B  Combine with the original NW
    vpsrlw xmm4,xmm1,1           ;W2          Shift W1 left 1 column
    vpsrlw xmm5,xmm3,1           ;NW2         Shift the original NW left 1 column
    vpxor xmm1,xmm4,xmm5         ;W2+NW2      combine w2 and NW2
    vpxor xmm9,xmm9,xmm1         ;**** xmm9 2 - C' main2+N2+W2+NW2
    ;register usage
    ;xmm0:   0-x
    ;xmm8:   1-B
    ;xmm9:   2-C'
    ;xmm10:  4-A
    ;xmm11:  6-B'
    ;xmm12:  7-C
    ;xmm13:  5-A'
    ;xmm14:  3-D
    ;xmm15:  8-D'
    ;First get all the counts
    vpxor xmm1,xmm12,xmm9        ;1's count of c
    vpand xmm2,xmm12,xmm9        ;2's count of c
    vpxor xmm3,xmm10,xmm13       ;1's count of a
    vpand xmm4,xmm10,xmm13       ;2's count of a
    vpxor xmm5,xmm8,xmm11        ;1's count of b
    vpand xmm6,xmm8,xmm11        ;2's count of b
    vpxor xmm7,xmm14,xmm15       ;1's count of d
    vpand xmm8,xmm14,xmm15       ;2's count of d
    ;Sow add the 1's together
    vpand xmm10,xmm1,xmm3        ;2's count of CA
    vpxor xmm1,xmm1,xmm3         ;combined ones of CA
    vpand xmm12,xmm5,xmm7        ;2's count of BD
    vpxor xmm5,xmm5,xmm7         ;combined ones of BD
    vpand xmm14,xmm1,xmm5        ;2's count of CABD
    vpxor xmm1,xmm1,xmm5         ;final count of the 1's
    ;now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         ;4's count of ca
    vpxor xmm2,xmm2,xmm4         ;2's count of ca
    vpand xmm5,xmm6,xmm8         ;4's count of bd
    vpxor xmm6,xmm6,xmm8         ;2's count of bd
    vpand xmm7,xmm10,xmm12       ;4's count of CABD
    vpxor xmm8,xmm10,xmm12       ;2's count of CABD
    vpand xmm9,xmm2,xmm6         ;4's count of cabd
    vpxor xmm4,xmm2,xmm6         ;2's count of cabd
    vpand xmm11,xmm8,xmm14       ;4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       ;2's count of CABD+abcd
    ;add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    ;add the 2's
    vpxor xmm2,xmm12,xmm4
    ;final add
    vpor xmm4,xmm14,xmm13
    ;now we have all the counts.
    ;register usage:
    ;xmm0 - original pattern
    ;xmm1 - count of the ones
    ;xmm2 - count of the twos
    ;xmm4 - count of the fours
    vpand xmm8,xmm0,xmm2         ;anything with a 2 stays the same
    vpand xmm3,xmm2,xmm1         ;anything with a 3 is alive
    vpor xmm1,xmm8,xmm3          ;add the alive cells
    ;We have now generated Q from P.
    ;We want to know the changes in key sectors of Q, and will check N,W,NW
    ;because this will become the S,E,SE of the next generation.
    ;bit 7 of al will be shifted into bit 16 of eax at a later stage and so on....
    ;The status bit work as follows
    ;xmm    8    7    6    5    4    3    2    1
    ;bit    7----6----5----4----3----2----1----0
    ;       +    same      |  dead             +
    ;       NW   W    N   all   NW   W    N   all
    vpandn xmm1,xmm1,xmm4        ;subtract the cells with 4 or more neighbors
    movdqu [rcx+2048],xmm1       ;store the result of the calculation
    vpxor xmm15,xmm15,xmm15      ;xmm15 holds a 0
    vpxor xmm9,xmm0,xmm1         ;see which cells have changed
    vpsrldq xmm2,xmm1,16-4       ;new N
    vpsrldq xmm6,xmm9,16-4       ;changes in N
    vpsrlw xmm3,xmm1,16-2        ;new W
    vpsrlw xmm7,xmm9,16-2        ;changes in W
    vpsrlw xmm4,xmm2,16-2        ;new NW
    vpsrlw xmm8,xmm6,16-2        ;changes in NW
    ; test the 2 qwords in each vector against zero
    vpcmpeqq xmm11, xmm1, xmm15
    vpcmpeqq xmm12, xmm3, xmm15
    vpcmpeqq xmm13, xmm5, xmm15
    vpcmpeqq xmm14, xmm7, xmm15

    ; blend the results down into xmm10   word origin
    vpblendw xmm10, xmm11, xmm12, 0xAA   ; 3131 3131
    vpblendw xmm13, xmm13, xmm14, 0xAA   ; 7575 7575
    vpblendw xmm10, xmm10, xmm13, 0xCC   ; 7531 7531

    ; test the 2 qwords in each vector against zero
    vpcmpeqq xmm11, xmm2, xmm15
    vpcmpeqq xmm12, xmm4, xmm15
    vpcmpeqq xmm13, xmm6, xmm15
    vpcmpeqq xmm14, xmm8, xmm15

    ; blend the results down into xmm11   word origin
    vpblendw xmm11, xmm11, xmm12, 0xAA   ; 4242 4242
    vpblendw xmm13, xmm13, xmm14, 0xAA   ; 8686 8686
    vpblendw xmm11, xmm11, xmm13, 0xCC   ; 8642 8642

    ;combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    ; xmm10 77553311 77553311
    ; xmm11 88664422 88664422   before shift
    ; xmm10 07050301 07050301
    ; xmm11 80604020 80604020   after shift
    ;result 87654321 87654321   combined
    vpsrlw xmm10,xmm10,8
    vpsllw xmm11,xmm11,8
    vpor xmm10,xmm10,xmm11

    ;combine the low and high dqword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm12,xmm10,8
    vpand xmm10,xmm10,xmm12
    vpmovmskb eax,xmm10
    ;P and Q are stored in the same memory block.
    ;first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    or al, ah
    ;xmm0 is the old unit
    ;xmm1 is the new unit
    ;al holds the status 
    mov ebx,222
    db 0x64, 0x67, 0x90    
    ret
