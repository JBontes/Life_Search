unit AVXGenerate;

{$mode delphiunicode}
{$optimization forcenostackframe}

interface
type
  TSymmetry = set of (sm_Horz, sm_Vert, sm_180);

function popcnt128(main: pointer): integer;

function GeneratePtoQ_AVX(main, N,W,NW: pointer): byte;
function GenerateQtoP_AVX(Main, S,E,SE: pointer): byte;

/// GeneratePtoQ with 32 units inside (i.e. with 64x64 pixel blocks).
function GeneratePtoQ_AVX_32(main, N,W,NW: pointer): byte;
function GenerateQtoP_AVX_32(Main, S,E,SE: pointer): byte;

function GeneratePtoQAVX2(main, N,W,NW: pointer): byte;
function GenerateQtoPAVX2(main, S,E,SE: pointer): byte;

function GeneratePtoQ_AVX256(main, N,W,NW: pointer): byte;
function GenerateQtoP_AVX256(main, S,E,SE: pointer): byte;

function NextCanocialNumber(var Continous: int64): int64;
procedure DivideStartingBlock(var NW, NE, SW, SE; const input: int64);
procedure ReverseBitsInAllBytes(ReverseMe: pointer);
function GenerateQtoP_AVX_plus_count(main, S,E,SE: pointer; ToDoMap128: pointer; previous: integer): integer;
function GeneratePtoQ_AVX_plus_count(main, N,W,NW: pointer; ToDoMap128: pointer; previous: integer): integer;


implementation


procedure ReverseBitsInAllBytes(ReverseMe: pointer);
const
  ReverseLowNibbles: array [0..15] of byte = ($00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0);
  ReverseHighNibbles:array [0..15] of byte = ($00,$08,$04,$0C,$02,$0A,$06,$0E,$01,$09,$05,$0D,$03,$0B,$07,$0F);
  NibbleMask:        array [0..15] of byte = ($0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F);
asm
  //register use:
  //RCX = input
  //RAX = output
  vmovdqu xmm4,[NibbleMask+rip]
  //vmovdqu xmm0,[ReverseMe]
  //vmovdqu xmm1,[ReverseMe]                     //low nibbles
  vmovdqu xmm2,[ReverseLowNibbles+rip]
  //movdqa xmm2,xmm1
  vmovdqu xmm3,[ReverseHighNibbles+rip]

  vpand xmm0,xmm4,[ReverseMe]       //keep the low nibbles
  vpandn xmm5,xmm4,[ReverseMe]             //keep the high nibbles
  vpsrlw xmm5,xmm5,4                //move the high nibbles in range for the mask
  vpshufb xmm0,xmm2,xmm0            //lookup the low nibbles
  vpshufb xmm5,xmm3,xmm5            //lookup the high nibbles
  //psllw xmm2,4                    //shift the high nibbles up
  vpor xmm0,xmm0,xmm5               //combine the two
  //movdqu [ReverseMe],xmm0
end;

(**)

function NextCanocialNumber(var Continous: int64): int64;
const
  NWMask = $0E0E0E7E7E7E00;
  NEMask = $7070707E7E7E00;
  SEMask = $7E7E7E70707000;
  SWMask = $7E7E7E0E0E0E00;
asm
  //parameters:
  //Previous: rcx
  //Result: rax
  mov rdx,[rcx]
  @next:
  inc rdx                     //Keep track of the continous count
  mov rax,rdx                 //get the next value
  //we want to manipulate an inner 6x6 block inside an 8x8 block.
  //convert the inner 6x6 to a 8x8 block
  //slice it up.
  mov r11d,$7E                //remember mask for later
  mov r8d,$7E00               //mask for slice 1
  shl rax,9                   //insert 1 empty row (8 bits) + 1 empty column (1 bit)
  and r8,rax                  //slice 1
  mov r9d,$7E0000             //mask slice 2
  shl rax,2                   //insert 2 empty columns
  and r9,rax
  mov r10d,$7E000000          //mask slice 3
  shl rax,2                   //insert 2 empty columns
  and r10,rax
  or r8,r9                    //combine slice 1 and 2
  mov r9,r11                  //Keep the mask
  shl r11,32                  //Mask slice 4
  shl rax,2                   //insert 2 empty columns
  and r11,rax
  or r8,r10                   //combine slice 123
  mov r10,r9
  shl r9,32+8                 //Mask slice 5
  shl rax,2                   //insert 2 empty columns
  and r9,rax                  //
  or r8,r11                   //combine slice 1234
  shl r10,32+16               //Mask 6
  shl rax,2                   //insert 2 empty columns
  and rax,r10                 //Slice 6
  or r8,r9                    //combine slice 12345
  or rax,r8                   //combine slice 123456

  mov r8,NWMask
  mov r9,NEMask
  mov r10,SEMask
  mov r11,SWMask
  and r8,rax
  and r9,rax
  and r10,rax
  and r11,rax
  popcnt r8,r8              //Get the weight per corner
  popcnt r9,r9
  popcnt r10,r10
  popcnt r11,r11
  cmp r8,r9
  js @next                    //NW corner, get next
  cmp r8,r10
  js @next
  cmp r8,r11
  js @next
  mov [rcx],rdx
end;

type
  TShuffleMask = array[0..15] of byte;

procedure DivideStartingBlock(var NW, NE, SW, SE; const input: int64);
const
  maskne = $00000000F0F0F0F0;
  masknw = $000000000F0F0F0F;
  maskse = $F0F0F0F000000000;
  masksw = $0F0F0F0F00000000;
  ShuffleNw: TShuffleMask = ($80,$80,$80,$80,$80,$80,$80,$80,$80,000,$80,001,$80,002,$80,003);
  ShuffleNe: TShuffleMask = ($80,$80,$80,$80,$80,$80,$80,$80,000,$80,001,$80,002,$80,003,$80);
  ShuffleSw: TShuffleMask = ($80,004,$80,005,$80,006,$80,007,$80,$80,$80,$80,$80,$80,$80,$80);
  ShuffleSe: TShuffleMask = (004,$80,005,$80,006,$80,007,$80,$80,$80,$80,$80,$80,$80,$80,$80);
  Full: TShuffleMask =      ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
asm
  //input:
  //input rcx
  //NW: rdx
  //NE: r8
  //SW: r9
  //SE: stack
  push rbx
  push rdi
  push rsi
  mov rbx,[input]
  mov r11,maskSE
  mov r10,maskSW
  mov rdi,maskNW
  mov rsi,maskNE
  and r11,rbx       //4 high bytes  high nibbles
  and r10,rbx       //4 high bytes  low nibbles
  and rdi,rbx       //4 low bytes  high nibbles
  and rsi,rbx       //4 low bytes  low nibbles
  vmovq xmm0,rdi   //NW
  vmovq xmm1,rsi   //NE
  vmovq xmm2,r10   //SW
  vmovq xmm3,r11   //SE
  //Now shuffle the bytes
  vmovdqu xmm4,[shuffleNW+rip]
  vmovdqu xmm5,[shuffleNE+rip]
  vmovdqu xmm6,[shuffleSW+rip]
  vmovdqu xmm7,[shuffleSE+rip]
  vpshufb xmm0, xmm0, xmm4
  vpshufb xmm1, xmm1, xmm5
  vpshufb xmm2, xmm2, xmm6
  vpshufb xmm3, xmm3, xmm7
  vpsllw xmm0,xmm0,4
  vpsrlw xmm1,xmm1,4
  vpsllw xmm2,xmm2,4
  vpsrlw xmm3,xmm3,4
  movdqu [NW], xmm0
  movdqu [NE], xmm1
  movdqu [SW], xmm2
  movdqu [SE], xmm3
  //movdqu xmm3,[Full+rip]
  //movdqu [SE],xmm3
  pop rsi
  pop rdi
  pop rbx
end;

{$define Get2GensBack}

function GeneratePtoQ_AVX(main, N,W,NW: pointer): byte;
asm
    //parameters
    //rcx: main
    //rdx: N
    //r8: W
    //r9: NW
    // A cell looks like this:
    //   BCD   123
    //   AxA   405
    //   BCD   678
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.

    vmovdqu xmm3,[r9]            //NW
    vmovdqu xmm1,[rdx]           //N
    vmovdqu xmm4,[r8]            //W
    vmovdqu xmm15,[rcx]          //***** xmm15 8-D'
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpsrldq xmm3,xmm3,16-4       //keep the bottom 2 rows of NW and shift them to the top
    vpsrldq xmm6,xmm1,16-2       //N5           keep the bottom 1 rows of N and shift them to the top.
    vpsrldq xmm1,xmm1,16-4       //N3           keep the bottom 2 rows of N and shift them to the top.
    vpsrlw xmm2,xmm4,14          //W6           keep the 2 rightmost columns of W
    vpsrlw xmm3,xmm3,14          //NW1          keep the 2 rightmost columns of NW
    vpslldq xmm5,xmm15,4         //main3        remove the bottom 2 rows from main
    vpslldq xmm7,xmm15,2         //main5        remove the bottom 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows N +14 rows main
    vpxor xmm13,xmm7,xmm6        //***** xmm13 5 - A'   1 row N  +15 rows main
    //we are done with N, xmm1 and xmm6
    vpsrlw xmm1,xmm2,1           //W7           remove an extra column from W
    vpsllw xmm7,xmm15,1          //main7        Shift main right
    vpsllw xmm8,xmm13,1          //main0+N0     Shift main+N1 right
    vpsllw xmm9,xmm14,1          //main2+N2     Shift mainn+N2 right
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+W7
    vpsllw xmm7,xmm7,1           //main6       Shift main right
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+W6
    vpslldq xmm10,xmm11,2        //main4+W4    Shift Main6W6 down
    vpsrldq xmm7,xmm3,2          //NW4         Shift NW1 up (only one row)
    vpsllw xmm6,xmm6,2           //N4          Shift N3 right
    vpxor xmm10,xmm10,xmm7       //main4+W4+NW4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpslldq xmm1,xmm1,2          //W0          Shift W7 down 1 row
    vpsrlw xmm7,xmm7,1           //NW0         Shift NW4 left (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+N0+W0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpslldq xmm1,xmm2,4          //W1          Shift W down 2 rows
    vpsllw xmm8,xmm9,1           //main1+N1    Shift Main2N2 right 1 column
    vpxor xmm8,xmm8,xmm1         //main1+N1+W1 Combine with W
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original NW
    vpsrlw xmm7,xmm1,1           //W2          Shift W1 left 1 column
    vpsrlw xmm5,xmm3,1           //NW2         Shift the original NW left 1 column
    vpxor xmm1,xmm7,xmm5         //W2+NW2      combine w2 and NW2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+N2+W2+NW2
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    {$ifdef Get2GensBack}
    vmovdqu xmm9,[rcx+2048]      //xmm9 is the value 2 generations back..
                                 //we use this to detect p2 oscillation.
    {$endif}
    vpand xmm8,xmm0,xmm2         //anything with a 2 stays the same
    //vmovdqu xmm0,[rcx+2048]        //Compare the new data with the data 2 gens back.
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm8,xmm3          //add the alive cells
    //We have now generated Q from P.
    //We want to know the changes in key sectors of Q, and will check N,W,NW
    //because this will become the S,E,SE of the next generation.
    //bit 7 of al will be shifted into bit 16 of eax at a later stage and so on....
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive P1+ alive
    //      corn all   EW   EW   NS  NS   all  all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx+2048],xmm1       //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    {$ifdef Get2GensBack}
    vpxor xmm9,xmm9,xmm1         //see which cells have changed 2 gens back.
    {$endif}
    vpxor xmm2,xmm0,xmm1         //see which cells have changed
    //vpsrldq xmm1,xmm1,16-4       //N3
    vpslldq xmm3,xmm1,16-4       //new N
    vpslldq xmm4,xmm9,16-6       //changes in N
    //vpsrlw xmm2,xmm4,14          //W6           keep the 2 rightmost columns of W
    vpsllw xmm5,xmm1,16-2        //new W
    vpsllw xmm6,xmm9,16-3        //changes in W
    vpsllw xmm7,xmm3,16-2        //new NW
    vpsllw xmm8,xmm4,16-3        //changes in NW
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm1, xmm1, xmm15  //all alive   1
    vpcmpeqq xmm3, xmm3, xmm15  //N           3
    vpcmpeqq xmm5, xmm5, xmm15  //W           5
    {$ifdef Get2GensBack}
    vpcmpeqq xmm7, xmm9, xmm15  //p2 all changed   7
    {$else}
    vpcmpeqq xmm7, xmm7, xmm15  //NW          7
    {$endif}

    // blend the results down into xmm10   byte origin
    vpblendw xmm1, xmm1, xmm3, $AA   // 3131 3131
    vpblendw xmm5, xmm5, xmm7, $AA   // 7575 7575
    vpblendw xmm1, xmm1, xmm5, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm2, xmm2, xmm15  //P1 All changes  2
    vpcmpeqq xmm4, xmm4, xmm15  //P2 N            4
    vpcmpeqq xmm6, xmm6, xmm15  //P2 W            6
    vpcmpeqq xmm8, xmm8, xmm15  //P2 NW           8

    // blend the results down into xmm11   byte origin
    vpblendw xmm2, xmm2, xmm4, $AA   // 4242 4242
    vpblendw xmm6, xmm6, xmm8, $AA   // 8686 8686
    vpblendw xmm2, xmm2, xmm6, $CC   // 8642 8642


    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm1,xmm1,8
    vpsllw xmm2,xmm2,8
    vpor xmm1,xmm1,xmm2

    //combine the low and high qword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm2,xmm1,8
    vpand xmm1,xmm1,xmm2
    vpmovmskb eax,xmm1
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //xmm0 is the old unit
    //xmm1 is the new unit
    //al holds the status
end;  (**)

const
  Offset64x64 = (64*64 div 8);

function GeneratePtoQ_AVX_32(main, N,W,NW: pointer): byte;
asm
    //parameters
    //rcx: main
    //rdx: N
    //r8: W
    //r9: NW
    // A cell looks like this:
    //   BCD   123
    //   AxA   405
    //   BCD   678
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.

    vmovdqu xmm3,[r9]            //NW
    vmovdqu xmm1,[rdx]           //N
    vmovdqu xmm4,[r8]            //W
    vmovdqu xmm15,[rcx]          //***** xmm15 8-D'
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpsrldq xmm3,xmm3,16-4       //keep the bottom 2 rows of NW and shift them to the top
    vpsrldq xmm6,xmm1,16-2       //N5           keep the bottom 1 rows of N and shift them to the top.
    vpsrldq xmm1,xmm1,16-4       //N3           keep the bottom 2 rows of N and shift them to the top.
    vpsrlw xmm2,xmm4,14          //W6           keep the 2 rightmost columns of W
    vpsrlw xmm3,xmm3,14          //NW1          keep the 2 rightmost columns of NW
    vpslldq xmm5,xmm15,4         //main3        remove the bottom 2 rows from main
    vpslldq xmm7,xmm15,2         //main5        remove the bottom 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows N +14 rows main
    vpxor xmm13,xmm7,xmm6        //***** xmm13 5 - A'   1 row N  +15 rows main
    //we are done with N, xmm1 and xmm6
    vpsrlw xmm1,xmm2,1           //W7           remove an extra column from W
    vpsllw xmm7,xmm15,1          //main7        Shift main right
    vpsllw xmm8,xmm13,1          //main0+N0     Shift main+N1 right
    vpsllw xmm9,xmm14,1          //main2+N2     Shift mainn+N2 right
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+W7
    vpsllw xmm7,xmm7,1           //main6       Shift main right
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+W6
    vpslldq xmm10,xmm11,2        //main4+W4    Shift Main6W6 down
    vpsrldq xmm7,xmm3,2          //NW4         Shift NW1 up (only one row)
    vpsllw xmm6,xmm6,2           //N4          Shift N3 right
    vpxor xmm10,xmm10,xmm7       //main4+W4+NW4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpslldq xmm1,xmm1,2          //W0          Shift W7 down 1 row
    vpsrlw xmm7,xmm7,1           //NW0         Shift NW4 left (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+N0+W0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpslldq xmm1,xmm2,4          //W1          Shift W down 2 rows
    vpsllw xmm8,xmm9,1           //main1+N1    Shift Main2N2 right 1 column
    vpxor xmm8,xmm8,xmm1         //main1+N1+W1 Combine with W
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original NW
    vpsrlw xmm7,xmm1,1           //W2          Shift W1 left 1 column
    vpsrlw xmm5,xmm3,1           //NW2         Shift the original NW left 1 column
    vpxor xmm1,xmm7,xmm5         //W2+NW2      combine w2 and NW2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+N2+W2+NW2
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    {$ifdef Get2GensBack}
    vmovdqu xmm9,[rcx+Offset64x64]      //xmm9 is the value 2 generations back..
                                 //we use this to detect p2 oscillation.
    {$endif}
    vpand xmm8,xmm0,xmm2         //anything with a 2 stays the same
    //vmovdqu xmm0,[rcx+2048]        //Compare the new data with the data 2 gens back.
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm8,xmm3          //add the alive cells
    //We have now generated Q from P.
    //We want to know the changes in key sectors of Q, and will check N,W,NW
    //because this will become the S,E,SE of the next generation.
    //bit 7 of al will be shifted into bit 16 of eax at a later stage and so on....
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive P1+ alive
    //      corn all   EW   EW   NS  NS   all  all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx+Offset64x64],xmm1       //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    {$ifdef Get2GensBack}
    vpxor xmm9,xmm9,xmm1         //see which cells have changed 2 gens back.
    {$endif}
    vpxor xmm2,xmm0,xmm1         //see which cells have changed
    //vpsrldq xmm1,xmm1,16-4       //N3
    vpslldq xmm3,xmm1,16-4       //new N
    vpslldq xmm4,xmm9,16-6       //changes in N
    //vpsrlw xmm2,xmm4,14          //W6           keep the 2 rightmost columns of W
    vpsllw xmm5,xmm1,16-2        //new W
    vpsllw xmm6,xmm9,16-3        //changes in W
    vpsllw xmm7,xmm3,16-2        //new NW
    vpsllw xmm8,xmm4,16-3        //changes in NW
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm1, xmm1, xmm15  //all alive   1
    vpcmpeqq xmm3, xmm3, xmm15  //N           3
    vpcmpeqq xmm5, xmm5, xmm15  //W           5
    {$ifdef Get2GensBack}
    vpcmpeqq xmm7, xmm9, xmm15  //p2 all changed   7
    {$else}
    vpcmpeqq xmm7, xmm7, xmm15  //NW          7
    {$endif}

    // blend the results down into xmm10   byte origin
    vpblendw xmm1, xmm1, xmm3, $AA   // 3131 3131
    vpblendw xmm5, xmm5, xmm7, $AA   // 7575 7575
    vpblendw xmm1, xmm1, xmm5, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm2, xmm2, xmm15  //P1 All changes  2
    vpcmpeqq xmm4, xmm4, xmm15  //P2 N            4
    vpcmpeqq xmm6, xmm6, xmm15  //P2 W            6
    vpcmpeqq xmm8, xmm8, xmm15  //P2 NW           8

    // blend the results down into xmm11   byte origin
    vpblendw xmm2, xmm2, xmm4, $AA   // 4242 4242
    vpblendw xmm6, xmm6, xmm8, $AA   // 8686 8686
    vpblendw xmm2, xmm2, xmm6, $CC   // 8642 8642


    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm1,xmm1,8
    vpsllw xmm2,xmm2,8
    vpor xmm1,xmm1,xmm2

    //combine the low and high qword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm2,xmm1,8
    vpand xmm1,xmm1,xmm2
    vpmovmskb eax,xmm1
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //xmm0 is the old unit
    //xmm1 is the new unit
    //al holds the status
end;  (**)


//function TDoMap.NextJumpLess(previous: integer): integer;
//asm
//  //register use
//  //RCX: self
//  //RDX: previous (0..127)
//  //return value in RAX
//  mov r10,[rcx]           //low part
//  mov r11,[rcx+8]         //high part
//  lea eax,[edx+1+(64*256)] //extract bits from position previous+1, length = 64
//  lea ecx,[edx+1-64+(64*256)] //extract bits from position previous+1-64 (high bits), length=64
//  lea r8d,[edx+1]
//  //bextr rdx,r10,rax       //extract low bits
//  //bextr rcx,r11,rcx       //extract high bits if needed
//  db $c4,$c2,$f8,$f7,$d2   //bextr  rdx,r10,rax
//  db $c4,$c2,$f0,$f7,$cb   //bextr  rcx,r11,rcx
//  rep bsf r9,rdx          //r9 = offset to previous if previous < 64
//  rep bsf r10,rcx         //r10 = offset to previous if previous > 64
//  rep bsf r11,r11         //r11 = result-64 if previous < 64 and low bits all 0
//  lea r9,[r9+r8]         //r9 = result if previous < 63  (ZF=0, CF=0)
//  lea r10,[r10+r8]       //r10 = result if previous >= 63  (CF=1, ZF= don't care)
//  lea r11,[r11+64]        //r11 = result if low bits all 0 and previous < 63)  (ZF=1, CF=0)
//  test rdx,rdx            //zf = low bits = 0.
//  bt eax,6                //cf = (previous+1) >= 64, ergo only look in high bits
//  //flags: CF = true
//  cmovz r9,r11           //select between the two options were CF should be 0.
//  cmovnc r10,r9           //if CF=0, choose the < 63 options, else select the > 63 option.
//  bt eax,7
//  mov eax,r10d
//  cmovc eax,r8d
//end;



//GeneratePtoQ is performed by the floating point unit.
//mix in integer code to get the next index to process.
//this is effectively free, because the integer unit is idling
//the integer code is spread out, so as not to overload the reorder buffer.
//Because quite a few AVX instructions have a latency > 1, this is essentially free
//even on CPU's that only execute 2 instructions per cycle.
function GeneratePtoQ_AVX_plus_count(main, N,W,NW: pointer; ToDoMap128: pointer; previous: integer): integer;
asm
    //parameters
    //rcx: main
    //rdx: N
    //r8: W
    //r9: NW
    // A cell looks like this:
    //   BCD   123
    //   AxA   405
    //   BCD   678
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.

    vmovdqu xmm3,[r9]            //NW
    vmovdqu xmm1,[rdx]           //N
    vmovdqu xmm4,[r8]            //W
    vmovdqu xmm15,[rcx]          //***** xmm15 8-D'
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpsrldq xmm3,xmm3,16-4       //keep the bottom 2 rows of NW and shift them to the top
    vpsrldq xmm6,xmm1,16-2       //N5           keep the bottom 1 rows of N and shift them to the top.
    vpsrldq xmm1,xmm1,16-4       //N3           keep the bottom 2 rows of N and shift them to the top.
    vpsrlw xmm2,xmm4,14          //W6           keep the 2 rightmost columns of W
    vpsrlw xmm3,xmm3,14          //NW1          keep the 2 rightmost columns of NW
    vpslldq xmm5,xmm15,4         //main3        remove the bottom 2 rows from main
    vpslldq xmm7,xmm15,2         //main5        remove the bottom 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows N +14 rows main
    vpxor xmm13,xmm7,xmm6        //***** xmm13 5 - A'   1 row N  +15 rows main
    //we are done with N, xmm1 and xmm6
mov rdx,[rsp+$28]           // rdx = @ToDoMap
mov r8d,[rsp+$30]           // r8d = previous

mov r10,[rdx]               // r10= low part
mov r11,[rdx+8]             // r11 = high part
lea eax,[r8d+1+(64*256)]    //extract bits from position previous+1, length = 64
lea r9d,[r8d+1-64+(64*256)] //extract bits from position previous+1-64 (high bits), length=64
inc r8d
    vpsrlw xmm1,xmm2,1           //W7           remove an extra column from W
    vpsllw xmm7,xmm15,1          //main7        Shift main right
    vpsllw xmm8,xmm13,1          //main0+N0     Shift main+N1 right
    vpsllw xmm9,xmm14,1          //main2+N2     Shift mainn+N2 right
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+W7
    vpsllw xmm7,xmm7,1           //main6       Shift main right
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+W6
    vpslldq xmm10,xmm11,2        //main4+W4    Shift Main6W6 down
    vpsrldq xmm7,xmm3,2          //NW4         Shift NW1 up (only one row)
    vpsllw xmm6,xmm6,2           //N4          Shift N3 right
    vpxor xmm10,xmm10,xmm7       //main4+W4+NW4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpslldq xmm1,xmm1,2          //W0          Shift W7 down 1 row
    vpsrlw xmm7,xmm7,1           //NW0         Shift NW4 left (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+N0+W0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpslldq xmm1,xmm2,4          //W1          Shift W down 2 rows
    vpsllw xmm8,xmm9,1           //main1+N1    Shift Main2N2 right 1 column
    vpxor xmm8,xmm8,xmm1         //main1+N1+W1 Combine with W
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original NW
    vpsrlw xmm7,xmm1,1           //W2          Shift W1 left 1 column
    vpsrlw xmm5,xmm3,1           //NW2         Shift the original NW left 1 column
    vpxor xmm1,xmm7,xmm5         //W2+NW2      combine w2 and NW2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+N2+W2+NW2
bextr rdx,r10,rax           //extract low bits
bextr r9, r11,r9            //extract high bits if needed
tzcnt r10,r9                //r10 = offset to previous if previous > 64
tzcnt r9,rdx                //r9 = offset to previous if previous < 64
tzcnt r11,r11               //r11 = result-64 if previous < 64 and low bits all 0
lea r9,[r9+r8]              //r9 = result if previous < 63  (ZF=0, CF=0)
lea r10,[r10+r8]            //r10 = result if previous >= 63  (CF=1, ZF= don't care)
lea r11,[r11+64]            //r11 = result if low bits all 0 and previous < 63)  (ZF=1, CF=0)
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
test rdx,rdx                //zf = low bits = 0.
bt eax,6                    //cf = (previous+1) >= 64, ergo only look in high bits
cmovz r9,r11                //select between the two options were CF should be 0.
cmovnc r10,r9               //if CF=0, choose the < 63 options, else select the > 63 option.
bt eax,7                    //if previous+1=128 then return that.
mov edx,eax                 //put next index in edx
cmovnc edx,r10d             //return next set bit.
shl edx,8                   //mov dl to dh
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    {$ifdef Get2GensBack}
    vmovdqu xmm9,[rcx+2048]      //xmm9 is the value 2 generations back..
                                 //we use this to detect p2 oscillation.
    {$endif}
    vpand xmm8,xmm0,xmm2         //anything with a 2 stays the same
    //vmovdqu xmm0,[rcx+2048]        //Compare the new data with the data 2 gens back.
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm8,xmm3          //add the alive cells
    //We have now generated Q from P.
    //We want to know the changes in key sectors of Q, and will check N,W,NW
    //because this will become the S,E,SE of the next generation.
    //bit 7 of al will be shifted into bit 16 of eax at a later stage and so on....
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive P1+ alive
    //      corn all   EW   EW   NS  NS   all  all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx+2048],xmm1       //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    {$ifdef Get2GensBack}
    vpxor xmm9,xmm9,xmm1         //see which cells have changed 2 gens back.
    {$endif}
    vpxor xmm2,xmm0,xmm1         //see which cells have changed
    //vpsrldq xmm1,xmm1,16-4       //N3
    vpslldq xmm3,xmm1,16-4       //new N
    vpslldq xmm4,xmm9,16-6       //changes in N
    //vpsrlw xmm2,xmm4,14          //W6           keep the 2 rightmost columns of W
    vpsllw xmm5,xmm1,16-2        //new W
    vpsllw xmm6,xmm9,16-3        //changes in W
    vpsllw xmm7,xmm3,16-2        //new NW
    vpsllw xmm8,xmm4,16-3        //changes in NW
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm1, xmm1, xmm15   //all alive   1
    vpcmpeqq xmm3, xmm3, xmm15   //N           3
    vpcmpeqq xmm5, xmm5, xmm15   //W           5
    {$ifdef Get2GensBack}
    vpcmpeqq xmm7, xmm9, xmm15  //p2 all changed   7
    {$else}
    vpcmpeqq xmm7, xmm7, xmm15  //NW          7
    {$endif}

    // blend the results down into xmm10   byte origin
    vpblendw xmm1, xmm1, xmm3, $AA   // 3131 3131
    vpblendw xmm5, xmm5, xmm7, $AA   // 7575 7575
    vpblendw xmm1, xmm1, xmm5, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm2, xmm2, xmm15   //P1 All changes  2
    vpcmpeqq xmm4, xmm4, xmm15   //P2 N            4
    vpcmpeqq xmm6, xmm6, xmm15   //P2 W            6
    vpcmpeqq xmm8, xmm8, xmm15   //P2 NW           8

    // blend the results down into xmm11   byte origin
    vpblendw xmm2, xmm2, xmm4, $AA   // 4242 4242
    vpblendw xmm6, xmm6, xmm8, $AA   // 8686 8686
    vpblendw xmm2, xmm2, xmm6, $CC   // 8642 8642


    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm1,xmm1,8
    vpsllw xmm2,xmm2,8
    vpor xmm1,xmm1,xmm2

    //combine the low and high qword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm2,xmm1,8
    vpand xmm1,xmm1,xmm2
    vpmovmskb eax,xmm1
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //xmm0 is the old unit
    //xmm1 is the new unit
    //al holds the status
or eax,edx                  //put next index in 2nd byte of result.
    //ah holds the next index.
end;  (**)



function GenerateQtoP_AVX_plus_count(main, S,E,SE: pointer; ToDoMap128: pointer; previous: integer): integer;
asm
    //parameters:
    //rcx - PMain
    //rdx - PS
    //r8 - PE
    //r9 - PSE
    //returns cellstate in AL.
    //and result in XMM0
    // A cell looks like this:
    //   DCB   876
    //   AxA   504
    //   DCB   321
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu xmm3,[r9]             //SE
    vmovdqu xmm1,[rdx]            //S
    vmovdqu xmm2,[r8]             //E
    vmovdqu xmm15,[rcx]           //***** xmm15 8-D'
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpslldq xmm3,xmm3,16-4       //keep the bottom 2 rows of SE and shift them to the top
    vpslldq xmm6,xmm1,16-2       //S5           keep the top 1 rows of S and shift them to the bottom.
    vpslldq xmm1,xmm1,16-4       //S3           keep the top 2 rows of S and shift them to the bottom.
    vpsllw xmm2,xmm2,14          //E6           keep the 2 leftmost columns of E
    vpsllw xmm3,xmm3,14          //SE1          keep the 2 leftmost columns of SE
    vpsrldq xmm5,xmm15,4         //main3        remove the top 2 rows from main
    vpsrldq xmm4,xmm15,2         //main5        remove the top 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows S +14 rows main
    vpxor xmm13,xmm4,xmm6        //***** xmm13 5 - A'   1 row S  +15 rows main
    //we are done with S, xmm1 and xmm6
mov rdx,[rsp+$28]           // rdx = @ToDoMap
mov r8d,[rsp+$30]           // r8d = previous

mov r10,[rdx]               // r10= low part
mov r11,[rdx+8]             // r11 = high part
lea eax,[r8d+1+(64*256)]    //extract bits from position previous+1, length = 64
lea r9d,[r8d+1-64+(64*256)] //extract bits from position previous+1-64 (high bits), length=64
inc r8d
    vpsllw xmm1,xmm2,1           //E7           remove an extra column from E
    vpsrlw xmm7,xmm15,1          //main7        Shift main left
    vpsrlw xmm8,xmm13,1          //main0+S0     Shift main+S1 left
    vpsrlw xmm9,xmm14,1          //main2+S2     Shift main+S2 left
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+E7
    vpsrlw xmm7,xmm7,1           //main6       Shift main left
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+E6
    vpsrldq xmm10,xmm11,2        //main4+E4    Shift Main6E6 up
    vpslldq xmm7,xmm3,2          //SE4         Shift SE1 down (only one row)
    vpsrlw xmm6,xmm6,2           //S4          Shift S3 left
    vpxor xmm10,xmm10,xmm7       //main4+E4+SE4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpsrldq xmm1,xmm1,2          //E0          Shift E7 up 1 row
    vpsllw xmm7,xmm7,1           //SE0         Shift SE4 right (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+S0+E0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpsrldq xmm1,xmm2,4          //E1          Shift E up 2 rows
    vpsrlw xmm8,xmm9,1           //main1+S1    Shift Main2S2 left 1 column
    vpxor xmm8,xmm8,xmm1         //main1+S1+E1 Combine with E
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original SE
    vpsllw xmm4,xmm1,1           //E2          Shift E1 right 1 column
    vpsllw xmm5,xmm3,1           //SE2         Shift the original SE right 1 column
    vpxor xmm1,xmm4,xmm5         //E2+SE2      combine E2 and SE2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+S2+E2+SE2
bextr rdx,r10,rax           //extract low bits
bextr r9, r11,r9            //extract high bits if needed
tzcnt r10,r9                //r10 = offset to previous if previous > 64
tzcnt r9,rdx                //r9 = offset to previous if previous < 64
tzcnt r11,r11               //r11 = result-64 if previous < 64 and low bits all 0
lea r9,[r9+r8]              //r9 = result if previous < 63  (ZF=0, CF=0)
lea r10,[r10+r8]            //r10 = result if previous >= 63  (CF=1, ZF= don't care)
lea r11,[r11+64]            //r11 = result if low bits all 0 and previous < 63)  (ZF=1, CF=0)
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
test rdx,rdx                //zf = low bits = 0.
bt eax,6                    //cf = (previous+1) >= 64, ergo only look in high bits
cmovz r9,r11                //select between the two options were CF should be 0.
cmovnc r10,r9               //if CF=0, choose the < 63 options, else select the > 63 option.
bt eax,7                    //if previous+1=128 then return that.
mov edx,eax                 //put next index in edx
cmovnc edx,r10d             //return next set bit.
shl edx,8                   //mov dl to dh
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    {$ifdef Get2GensBack}
    vmovdqu xmm9,[rcx-2048]      //xmm9 is the value 2 generations back..
                                 //we use this to detect p2 oscillation.
    {$endif}
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    vpand xmm7,xmm0,xmm2         //anything with a 2 stays the same
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm7,xmm3          //add the alive cells
    //Record the changes in S,E,SE.
    //This will influence the N,W,NW in the next generation.
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive P1+ alive
    //      corn all   EW   EW   NS  NS   all  all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx-2048],xmm1      //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    {$ifdef Get2GensBack}
    vpxor xmm9,xmm9,xmm1         //see which cells have changed 2 gens back.
    {$endif}
    vpxor xmm2,xmm0,xmm1         //see which cells have changed
    //vpslldq xmm1,xmm1,16-4       //S3
    vpsrldq xmm3,xmm1,16-4       //new S     //todo: see if this matches with the def of N above
    vpsrldq xmm4,xmm9,16-6       //P2 changes in S  (because we test p2, we need a border 1 px larger).
    //vpsllw xmm2,xmm4,14          //E6           keep the 2 leftmost columns of E
    vpsrlw xmm5,xmm1,16-2        //new E
    vpsrlw xmm6,xmm9,16-3        //P2 changes in E
    vpsrlw xmm7,xmm3,16-2        //new SE
    vpsrlw xmm8,xmm4,16-3        //P2 changes in SE
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm1, xmm1, xmm15  //all alive   1
    vpcmpeqq xmm3, xmm3, xmm15  //N           3
    vpcmpeqq xmm5, xmm5, xmm15  //W           5
    {$ifdef Get2GensBack}
    vpcmpeqq xmm7, xmm9, xmm15  //p2 all changed   7
    {$else}
    vpcmpeqq xmm7, xmm7, xmm15  //NW          7
    {$endif}

    // blend the results down into xmm10   byte origin
    vpblendw xmm1, xmm1, xmm3, $AA   // 3131 3131
    vpblendw xmm5, xmm5, xmm7, $AA   // 7575 7575
    vpblendw xmm1, xmm1, xmm5, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm2, xmm2, xmm15  //p1 All changes  2
    vpcmpeqq xmm4, xmm4, xmm15  //p2 S            4
    vpcmpeqq xmm6, xmm6, xmm15  //p2 E            6
    vpcmpeqq xmm8, xmm8, xmm15  //p2 SE           8

    // blend the results down into xmm11   byte origin
    vpblendw xmm2, xmm2, xmm4, $AA   // 4242 4242
    vpblendw xmm6, xmm6, xmm8, $AA   // 8686 8686
    vpblendw xmm2, xmm2, xmm6, $CC   // 8642 8642


    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm1,xmm1,8
    vpsllw xmm2,xmm2,8
    vpor xmm1,xmm1,xmm2

    //combine the low and high dqword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm2,xmm1,8
    vpand xmm1,xmm1,xmm2
    vpmovmskb eax,xmm1
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //xmm9 is the unit 2 gens back
    //xmm0 is the unit 1 gen back
    //xmm1 is the new unit
    //al holds the status
or eax,edx                  //put next index in 2nd byte of result.
end;


function GenerateQtoP_AVX(Main, S,E,SE: pointer): byte;
asm
    //parameters:
    //rcx - PMain
    //rdx - PS
    //r8 - PE
    //r9 - PSE
    //returns cellstate in AL.
    //and result in XMM0
    // A cell looks like this:
    //   DCB   876
    //   AxA   504
    //   DCB   321
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu xmm3,[r9]             //SE
    vmovdqu xmm1,[rdx]            //S
    vmovdqu xmm2,[r8]             //E
    vmovdqu xmm15,[rcx]           //***** xmm15 8-D'
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpslldq xmm3,xmm3,16-4       //keep the bottom 2 rows of SE and shift them to the top
    vpslldq xmm6,xmm1,16-2       //S5           keep the top 1 rows of S and shift them to the bottom.
    vpslldq xmm1,xmm1,16-4       //S3           keep the top 2 rows of S and shift them to the bottom.
    vpsllw xmm2,xmm2,14          //E6           keep the 2 leftmost columns of E
    vpsllw xmm3,xmm3,14          //SE1          keep the 2 leftmost columns of SE
    vpsrldq xmm5,xmm15,4         //main3        remove the top 2 rows from main
    vpsrldq xmm4,xmm15,2         //main5        remove the top 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows S +14 rows main
    vpxor xmm13,xmm4,xmm6        //***** xmm13 5 - A'   1 row S  +15 rows main
    //we are done with S, xmm1 and xmm6
    vpsllw xmm1,xmm2,1           //E7           remove an extra column from E
    vpsrlw xmm7,xmm15,1          //main7        Shift main left
    vpsrlw xmm8,xmm13,1          //main0+S0     Shift main+S1 left
    vpsrlw xmm9,xmm14,1          //main2+S2     Shift main+S2 left
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+E7
    vpsrlw xmm7,xmm7,1           //main6       Shift main left
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+E6
    vpsrldq xmm10,xmm11,2        //main4+E4    Shift Main6E6 up
    vpslldq xmm7,xmm3,2          //SE4         Shift SE1 down (only one row)
    vpsrlw xmm6,xmm6,2           //S4          Shift S3 left
    vpxor xmm10,xmm10,xmm7       //main4+E4+SE4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpsrldq xmm1,xmm1,2          //E0          Shift E7 up 1 row
    vpsllw xmm7,xmm7,1           //SE0         Shift SE4 right (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+S0+E0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpsrldq xmm1,xmm2,4          //E1          Shift E up 2 rows
    vpsrlw xmm8,xmm9,1           //main1+S1    Shift Main2S2 left 1 column
    vpxor xmm8,xmm8,xmm1         //main1+S1+E1 Combine with E
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original SE
    vpsllw xmm4,xmm1,1           //E2          Shift E1 right 1 column
    vpsllw xmm5,xmm3,1           //SE2         Shift the original SE right 1 column
    vpxor xmm1,xmm4,xmm5         //E2+SE2      combine E2 and SE2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+S2+E2+SE2
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    {$ifdef Get2GensBack}
    vmovdqu xmm9,[rcx-2048]      //xmm9 is the value 2 generations back..
                                 //we use this to detect p2 oscillation.
    {$endif}
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    vpand xmm7,xmm0,xmm2         //anything with a 2 stays the same
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm7,xmm3          //add the alive cells
    //Record the changes in S,E,SE.
    //This will influence the N,W,NW in the next generation.
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive P1+ alive
    //      corn all   EW   EW   NS  NS   all  all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx-2048],xmm1      //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    {$ifdef Get2GensBack}
    vpxor xmm9,xmm9,xmm1         //see which cells have changed 2 gens back.
    {$endif}
    vpxor xmm2,xmm0,xmm1         //see which cells have changed
    //vpslldq xmm1,xmm1,16-4       //S3
    vpsrldq xmm3,xmm1,16-4       //new S     //todo: see if this matches with the def of N above
    vpsrldq xmm4,xmm9,16-6       //P2 changes in S  (because we test p2, we need a border 1 px larger).
    //vpsllw xmm2,xmm4,14          //E6           keep the 2 leftmost columns of E
    vpsrlw xmm5,xmm1,16-2        //new E
    vpsrlw xmm6,xmm9,16-3        //P2 changes in E
    vpsrlw xmm7,xmm3,16-2        //new SE
    vpsrlw xmm8,xmm4,16-3        //P2 changes in SE
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm1, xmm1, xmm15  //all alive   1
    vpcmpeqq xmm3, xmm3, xmm15  //N           3
    vpcmpeqq xmm5, xmm5, xmm15  //W           5
    {$ifdef Get2GensBack}
    vpcmpeqq xmm7, xmm9, xmm15  //p2 all changed   7
    {$else}
    vpcmpeqq xmm7, xmm7, xmm15  //NW          7
    {$endif}

    // blend the results down into xmm10   byte origin
    vpblendw xmm1, xmm1, xmm3, $AA   // 3131 3131
    vpblendw xmm5, xmm5, xmm7, $AA   // 7575 7575
    vpblendw xmm1, xmm1, xmm5, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm2, xmm2, xmm15  //p1 All changes  2
    vpcmpeqq xmm4, xmm4, xmm15  //p2 S            4
    vpcmpeqq xmm6, xmm6, xmm15  //p2 E            6
    vpcmpeqq xmm8, xmm8, xmm15  //p2 SE           8

    // blend the results down into xmm11   byte origin
    vpblendw xmm2, xmm2, xmm4, $AA   // 4242 4242
    vpblendw xmm6, xmm6, xmm8, $AA   // 8686 8686
    vpblendw xmm2, xmm2, xmm6, $CC   // 8642 8642


    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm1,xmm1,8
    vpsllw xmm2,xmm2,8
    vpor xmm1,xmm1,xmm2

    //combine the low and high dqword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm2,xmm1,8
    vpand xmm1,xmm1,xmm2
    vpmovmskb eax,xmm1
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //xmm9 is the unit 2 gens back
    //xmm0 is the unit 1 gen back
    //xmm1 is the new unit
    //al holds the status
end;

function GenerateQtoP_AVX_32(Main, S,E,SE: pointer): byte;
asm
    //parameters:
    //rcx - PMain
    //rdx - PS
    //r8 - PE
    //r9 - PSE
    //returns cellstate in AL.
    //and result in XMM0
    // A cell looks like this:
    //   DCB   876
    //   AxA   504
    //   DCB   321
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu xmm3,[r9]             //SE
    vmovdqu xmm1,[rdx]            //S
    vmovdqu xmm2,[r8]             //E
    vmovdqu xmm15,[rcx]           //***** xmm15 8-D'
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpslldq xmm3,xmm3,16-4       //keep the bottom 2 rows of SE and shift them to the top
    vpslldq xmm6,xmm1,16-2       //S5           keep the top 1 rows of S and shift them to the bottom.
    vpslldq xmm1,xmm1,16-4       //S3           keep the top 2 rows of S and shift them to the bottom.
    vpsllw xmm2,xmm2,14          //E6           keep the 2 leftmost columns of E
    vpsllw xmm3,xmm3,14          //SE1          keep the 2 leftmost columns of SE
    vpsrldq xmm5,xmm15,4         //main3        remove the top 2 rows from main
    vpsrldq xmm4,xmm15,2         //main5        remove the top 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows S +14 rows main
    vpxor xmm13,xmm4,xmm6        //***** xmm13 5 - A'   1 row S  +15 rows main
    //we are done with S, xmm1 and xmm6
    vpsllw xmm1,xmm2,1           //E7           remove an extra column from E
    vpsrlw xmm7,xmm15,1          //main7        Shift main left
    vpsrlw xmm8,xmm13,1          //main0+S0     Shift main+S1 left
    vpsrlw xmm9,xmm14,1          //main2+S2     Shift main+S2 left
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+E7
    vpsrlw xmm7,xmm7,1           //main6       Shift main left
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+E6
    vpsrldq xmm10,xmm11,2        //main4+E4    Shift Main6E6 up
    vpslldq xmm7,xmm3,2          //SE4         Shift SE1 down (only one row)
    vpsrlw xmm6,xmm6,2           //S4          Shift S3 left
    vpxor xmm10,xmm10,xmm7       //main4+E4+SE4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpsrldq xmm1,xmm1,2          //E0          Shift E7 up 1 row
    vpsllw xmm7,xmm7,1           //SE0         Shift SE4 right (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+S0+E0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpsrldq xmm1,xmm2,4          //E1          Shift E up 2 rows
    vpsrlw xmm8,xmm9,1           //main1+S1    Shift Main2S2 left 1 column
    vpxor xmm8,xmm8,xmm1         //main1+S1+E1 Combine with E
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original SE
    vpsllw xmm4,xmm1,1           //E2          Shift E1 right 1 column
    vpsllw xmm5,xmm3,1           //SE2         Shift the original SE right 1 column
    vpxor xmm1,xmm4,xmm5         //E2+SE2      combine E2 and SE2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+S2+E2+SE2
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    {$ifdef Get2GensBack}
    vmovdqu xmm9,[rcx-Offset64x64]      //xmm9 is the value 2 generations back..
                                 //we use this to detect p2 oscillation.
    {$endif}
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    vpand xmm7,xmm0,xmm2         //anything with a 2 stays the same
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm7,xmm3          //add the alive cells
    //Record the changes in S,E,SE.
    //This will influence the N,W,NW in the next generation.
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive P1+ alive
    //      corn all   EW   EW   NS  NS   all  all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx-Offset64x64],xmm1      //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    {$ifdef Get2GensBack}
    vpxor xmm9,xmm9,xmm1         //see which cells have changed 2 gens back.
    {$endif}
    vpxor xmm2,xmm0,xmm1         //see which cells have changed
    //vpslldq xmm1,xmm1,16-4       //S3
    vpsrldq xmm3,xmm1,16-4       //new S     //todo: see if this matches with the def of N above
    vpsrldq xmm4,xmm9,16-6       //P2 changes in S  (because we test p2, we need a border 1 px larger).
    //vpsllw xmm2,xmm4,14          //E6           keep the 2 leftmost columns of E
    vpsrlw xmm5,xmm1,16-2        //new E
    vpsrlw xmm6,xmm9,16-3        //P2 changes in E
    vpsrlw xmm7,xmm3,16-2        //new SE
    vpsrlw xmm8,xmm4,16-3        //P2 changes in SE
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm1, xmm1, xmm15  //all alive   1
    vpcmpeqq xmm3, xmm3, xmm15  //N           3
    vpcmpeqq xmm5, xmm5, xmm15  //W           5
    {$ifdef Get2GensBack}
    vpcmpeqq xmm7, xmm9, xmm15  //p2 all changed   7
    {$else}
    vpcmpeqq xmm7, xmm7, xmm15  //NW          7
    {$endif}

    // blend the results down into xmm10   byte origin
    vpblendw xmm1, xmm1, xmm3, $AA   // 3131 3131
    vpblendw xmm5, xmm5, xmm7, $AA   // 7575 7575
    vpblendw xmm1, xmm1, xmm5, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm2, xmm2, xmm15  //p1 All changes  2
    vpcmpeqq xmm4, xmm4, xmm15  //p2 S            4
    vpcmpeqq xmm6, xmm6, xmm15  //p2 E            6
    vpcmpeqq xmm8, xmm8, xmm15  //p2 SE           8

    // blend the results down into xmm11   byte origin
    vpblendw xmm2, xmm2, xmm4, $AA   // 4242 4242
    vpblendw xmm6, xmm6, xmm8, $AA   // 8686 8686
    vpblendw xmm2, xmm2, xmm6, $CC   // 8642 8642


    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm1,xmm1,8
    vpsllw xmm2,xmm2,8
    vpor xmm1,xmm1,xmm2

    //combine the low and high dqword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm2,xmm1,8
    vpand xmm1,xmm1,xmm2
    vpmovmskb eax,xmm1
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //xmm9 is the unit 2 gens back
    //xmm0 is the unit 1 gen back
    //xmm1 is the new unit
    //al holds the status
end;


//Untested!!
function GenerateQtoP_AVX256(Main, S,E,SE: pointer): byte;
asm
    //parameters:
    //rcx - PMain
    //rdx - PS
    //r8 - PE
    //r9 - PSE
    //returns cellstate in AL.
    //and result in ymm0
    // A cell looks like this:
    //   DCB   876
    //   AxA   504
    //   DCB   321
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu ymm15,[rcx]           //***** ymm15 8-D'
    vmovdqu ymm1,[rdx]            //S
    vmovdqu ymm2,[r8]             //E
    vmovdqu ymm3,[r9]             //SE
    //now create 9 planes, each one slightly shifted.
    //ymm0 is the center plane.
    vpslldq ymm3,ymm3,32-4       //keep the bottom 2 rows of SE and shift them to the top
    vpslldq ymm6,ymm1,32-2       //S5           keep the top 1 rows of S and shift them to the bottom.
    vpslldq ymm1,ymm1,32-4       //S3           keep the top 2 rows of S and shift them to the bottom.
    vpsllw ymm2,ymm2,14          //E6           keep the 2 leftmost columns of E
    vpsllw ymm3,ymm3,14          //SE1          keep the 2 leftmost columns of SE
    vpsrldq ymm5,ymm15,4         //main3        remove the top 2 rows from main
    vpsrldq ymm4,ymm15,2         //main5        remove the top 1 row from main
    vpxor ymm14,ymm1,ymm5        //***** ymm14 3 - D    2 rows S +14 rows main
    vpxor ymm13,ymm4,ymm6        //***** ymm13 5 - A'   1 row S  +15 rows main
    //we are done with S, ymm1 and ymm6
    vpsllw ymm1,ymm2,1           //E7           remove an extra column from E
    vpsrlw ymm7,ymm15,1          //main7        Shift main left
    vpsrlw ymm8,ymm13,1          //main0+S0     Shift main+S1 left
    vpsrlw ymm9,ymm14,1          //main2+S2     Shift main+S2 left
    vpxor ymm12,ymm7,ymm1        //***** ymm12 7 - C Main7+E7
    vpsrlw ymm7,ymm7,1           //main6       Shift main left
    vpxor ymm11,ymm7,ymm2        //***** ymm11 6 - B' Main6+E6
    vpsrldq ymm10,ymm11,2        //main4+E4    Shift Main6E6 up
    vpslldq ymm7,ymm3,2          //SE4         Shift SE1 down (only one row)
    vpsrlw ymm6,ymm6,2           //S4          Shift S3 left
    vpxor ymm10,ymm10,ymm7       //main4+E4+SE4
    vpxor ymm10,ymm10,ymm6       //***** ymm10 4 - A
    vpsrldq ymm1,ymm1,2          //E0          Shift E7 up 1 row
    vpsllw ymm7,ymm7,1           //SE0         Shift SE4 right (keep only 1 pixel)
    vpxor ymm0,ymm8,ymm1         //main0+S0+E0
    vpxor ymm0,ymm0,ymm7         //***** ymm0 0 - x
    vpsrldq ymm1,ymm2,4          //E1          Shift E up 2 rows
    vpsrlw ymm8,ymm9,1           //main1+S1    Shift Main2S2 left 1 column
    vpxor ymm8,ymm8,ymm1         //main1+S1+E1 Combine with E
    vpxor ymm8,ymm8,ymm3         //**** ymm8 1 - B  Combine with the original SE
    vpsllw ymm4,ymm1,1           //E2          Shift E1 right 1 column
    vpsllw ymm5,ymm3,1           //SE2         Shift the original SE right 1 column
    vpxor ymm1,ymm4,ymm5         //E2+SE2      combine E2 and SE2
    vpxor ymm9,ymm9,ymm1         //**** ymm9 2 - C' main2+S2+E2+SE2
    //register usage
    //ymm0:   0-x
    //ymm8:   1-B
    //ymm9:   2-C'
    //ymm10:  4-A
    //ymm11:  6-B'
    //ymm12:  7-C
    //ymm13:  5-A'
    //ymm14:  3-D
    //ymm15:  8-D'
    //First get all the counts
    vpxor ymm1,ymm12,ymm9        //1's count of c
    vpand ymm2,ymm12,ymm9        //2's count of c
    vpxor ymm3,ymm10,ymm13       //1's count of a
    vpand ymm4,ymm10,ymm13       //2's count of a
    vpxor ymm5,ymm8,ymm11        //1's count of b
    vpand ymm6,ymm8,ymm11        //2's count of b
    vpxor ymm7,ymm14,ymm15       //1's count of d
    vpand ymm8,ymm14,ymm15       //2's count of d
    //Now add the 1's together
    vpand ymm10,ymm1,ymm3        //2's count of CA
    vpxor ymm1,ymm1,ymm3         //combined ones of CA
    vpand ymm12,ymm5,ymm7        //2's count of BD
    vpxor ymm5,ymm5,ymm7         //combined ones of BD
    vpand ymm14,ymm1,ymm5        //2's count of CABD
    vpxor ymm1,ymm1,ymm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand ymm3,ymm2,ymm4         //4's count of ca
    vpxor ymm2,ymm2,ymm4         //2's count of ca
    vpand ymm5,ymm6,ymm8         //4's count of bd
    vpxor ymm6,ymm6,ymm8         //2's count of bd
    vpand ymm7,ymm10,ymm12       //4's count of CABD
    vpxor ymm8,ymm10,ymm12       //2's count of CABD
    vpand ymm9,ymm2,ymm6         //4's count of cabd
    vpxor ymm4,ymm2,ymm6         //2's count of cabd
    vpand ymm11,ymm8,ymm14       //4's count of CABD+abcd
    vpxor ymm12,ymm8,ymm14       //2's count of CABD+abcd
    //add all 4's
    vpor ymm15,ymm3,ymm5
    vpor ymm13,ymm7,ymm9
    vpor ymm14,ymm11,ymm15
    //add the 2's
    vpxor ymm2,ymm12,ymm4
    //final add
    vpor ymm4,ymm14,ymm13
    //now we have all the counts.
    //register usage:
    //ymm0 - original pattern
    //ymm1 - count of the ones
    //ymm2 - count of the twos
    //ymm4 - count of the fours
    vpand ymm7,ymm0,ymm2         //anything with a 2 stays the same
    //movdqu ymm0,[rcx-2048]        //Compare the new data with the data 2 gens back.
    vpand ymm3,ymm2,ymm1         //anything with a 3 is alive
    vpor ymm1,ymm7,ymm3          //add the alive cells
    //Record the changes in S,E,SE.
    //This will influence the N,W,NW in the next generation.
    //The status bit work as follows
    //ymm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //       +    same         |  dead          +
    //       SE   E    S   all   SE   E    S   all
    vpandn ymm1,ymm4,ymm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx-2048],ymm1       //store the result of the calculation
    vpxor ymm15,ymm15,ymm15      //ymm15 holds a 0
    vpxor ymm2,ymm0,ymm1         //see which cells have changed
    vpslldq ymm3,ymm1,32-4       //new S
    vpslldq ymm4,ymm2,32-4       //changes in S
    vpsllw ymm5,ymm1,32-2        //new E
    vpsllw ymm6,ymm2,32-2        //changes in E
    vpsllw ymm7,ymm3,32-2        //new SE
    vpsllw ymm8,ymm4,32-2        //changes in SE
    // test the 2 qwords in each vector against zero
    vpcmpeqq ymm11, ymm1, ymm15  //all alive?
    vpcmpeqq ymm12, ymm3, ymm15  //S
    vpcmpeqq ymm13, ymm5, ymm15  //E
    vpcmpeqq ymm14, ymm7, ymm15  //SE

    // blend the results down into ymm10   word origin
    vpblendw ymm10, ymm11, ymm12, $AA   // 3131 3131
    vpblendw ymm13, ymm13, ymm14, $AA   // 7575 7575
    vpblendw ymm10, ymm10, ymm13, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq ymm11, ymm2, ymm15  //All changes
    vpcmpeqq ymm12, ymm4, ymm15  //S changes
    vpcmpeqq ymm13, ymm6, ymm15  //E changes
    vpcmpeqq ymm14, ymm8, ymm15  //SE changes

    // blend the results down into ymm11   word origin
    vpblendw ymm11, ymm11, ymm12, $AA   // 4242 4242
    vpblendw ymm13, ymm13, ymm14, $AA   // 8686 8686
    vpblendw ymm11, ymm11, ymm13, $CC   // 8642 8642

    //combine bytes of ymm10 and ymm11 together into ymm10, byte wise
    // ymm10 77553311 77553311
    // ymm11 88664422 88664422   before shift
    // ymm10 07050301 07050301
    // ymm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw ymm10,ymm10,8
    vpsllw ymm11,ymm11,8
    vpor ymm10,ymm10,ymm11

    //combine the low and high dqword to make sure both are zero.
    vpsrldq ymm12,ymm10,8
    vpand ymm10,ymm10,ymm12
    vpmovmskb eax,ymm10
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //or al, ah
    //ymm0 is the old unit
    //ymm1 is the new unit
    //al holds the status
end;

//Assumes 32 byte Units
//Needs to feed this into code that assumes 16 byte units
function GeneratePtoQAVX2(main, N, W, NW: pointer): byte;
asm
  lea N,[N+16]           //move from top to middle part
  lea NW,[N+16]
  call GeneratePtoQ_AVX
  mov r10,rax
  lea N,[main]
  lea NW,[W]
  lea main,[main+16]
  lea W,[W+16]
  call GeneratePtoQ_AVX
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //       +    same         |  dead          +
    //       NW   W    N   all   NW   W    N   all
  and eax,$55
  or rax,r10
end;

function GenerateQtoPAVX2(main, S, E, SE: pointer): byte;
asm
  mov r10,S
  mov r11,SE
  lea S,[main+16]
  lea SE,[E+16]
  call GenerateQtoP_AVX
  lea main,[main+16]
  lea E,[E+16]
  lea S,[r10]
  lea SE,[r11]
  mov r10,rax
  call GenerateQtoP_AVX
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //       +    same         |  dead          +
    //       SE   E    S   all   SE   E    S   all
  and eax,$55
  or rax,r10
end;

function popcnt128(main: pointer): integer;
asm
  //main is a 16 byte value.
  //register use: rcx: main
  popcnt rax,[rcx]
  popcnt rdx,[rcx+8]
  add rax,rdx
end;

function popcnt256(main: pointer): integer;
asm
  popcnt rax,[rcx]
  popcnt rdx,[rcx+8]
  popcnt r8,[rcx+16]
  popcnt r9,[rcx+24]
  add rax,rdx
  add r8,r9
  add rax,r8
end;

procedure difference128(const a,b: pointer; output: pointer);
asm
  vmovdqu xmm0,[rcx]
  vpxor xmm0,xmm0,[rdx]
  vmovdqu [r8],xmm0
end;

procedure difference256(const a,b: pointer; output: pointer);
asm
  vmovdqu ymm0,[rcx]
  vpxor ymm0,ymm0,[rdx]
  vmovdqu [r8],ymm0
end;

const
  ReverseLowNibbles: array [0..15] of byte = ($00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0);
  ReverseHighNibbles:array [0..15] of byte = ($00,$08,$04,$0C,$02,$0A,$06,$0E,$01,$09,$05,$0D,$03,$0B,$07,$0F);
  NibbleMask:        array [0..15] of byte = ($0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F);
  ShuffleBytetoWord: array [0..15] of byte = ($01,$00,$03,$02,$05,$04,$07,$06,$09,$08,$0B,$0A,$0D,$0C,$0F,$0E);


procedure ReverseBitsInAllWords;
asm
  //register use:
  //xmm0,xmm1: input
  //xmm0,xmm1: reversed output
  vmovdqu xmm4,[NibbleMask+rip]
  vmovdqu xmm2,[ReverseLowNibbles+rip]
  vmovdqu xmm3,[ReverseHighNibbles+rip]

  vpand  xmm5,xmm4,xmm0             //keep the low nibbles
  vpandn xmm6,xmm4,xmm0             //keep the high nibbles
  vpand  xmm7,xmm4,xmm1             //keep the low nibbles
  vpandn xmm8,xmm4,xmm1             //keep the high nibbles
  vmovdqu xmm4,[ShuffleByteToWord+rip]
  vpsrlw xmm6,xmm6,4                //move the high nibbles in range for the mask
  vpsrlw xmm8,xmm8,4
  vpshufb xmm0,xmm2,xmm5            //lookup the low nibbles
  vpshufb xmm2,xmm2,xmm7
  vpshufb xmm1,xmm3,xmm6            //lookup the high nibbles
  vpshufb xmm3,xmm3,xmm8
  //psllw xmm2,4                    //shift the high nibbles up
  vpor xmm0,xmm0,xmm2               //combine the two
  vpor xmm1,xmm1,xmm3
  vpshufb xmm0,xmm0,xmm4            //reverse the order of the byte per byte
  vpshufb xmm1,xmm1,xmm4            //to complete the reversal of words.
end;

const
  RowShuffleMask: array [0..15] of byte = (14,15,12,13,10,11,8,9,6,7,4,5,2,3,0,1);

procedure ReverseRows;
asm
  //register use
  //xmm2: input
  //xmm2: output with the rows reversed.
  vpshufb xmm2,xmm2,[RowShuffleMask+rip]
end;

//Here's how we test for symmetry.
//0. if the pattern as an odd width/height, remove the center column/row
//1. Center the pattern, so the middle point is nicely aligned.
//2. investigate the center of the pattern for the different symmetries.
//3. Validate if the other parts also have this symmetry.


//if we suspect a pattern to be symmetrical

const SymMaskHorz: array [0..15] of byte = ($FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00);


function SymmetryMiddleEven128(const a1,a2: pointer): TSymmetry;
asm
  vmovdqu xmm13,[rip+SymMaskHorz]
  vmovdqu xmm14,[rcx]       //16x8 slice 1
  vmovdqu xmm15,[rdx]       //16x8 slice 2
  //mask out the left most part
  vmovdqa xmm0,xmm14
  vmovdqa xmm1,xmm15
  call ReverseBitsInAllWords  //reverse the words

  vpandn xmm2,xmm13,xmm0      //mask off the rightmost part
  vpandn xmm3,xmm13,xmm1
  vpandn xmm4,xmm13,xmm14     //mask off the left most part
  vpandn xmm5,xmm13,xmm15
  //both parts should be equal now
  vpxor xmm2,xmm2,xmm4
  vpxor xmm3,xmm3,xmm5
  vpor xmm13,xmm0,xmm1         //if horz symmetrical then xmm13 will be zero.
  //Let's test vertical symmetry
  vmovdqa xmm2,xmm15
  call reverseRows
  vpxor xmm12,xmm2,xmm14        //if vert symmetrical then xmm0 will be zero.
  //A 180 rotation is a horizontal flip + a vertical flip.
  //the horizontally flipped parts are in xmm0 and xmm1
  //we need to flip these vertically.
  vpshufb xmm10,xmm0,[RowShuffleMask+rip]
  vpxor xmm11,xmm10,xmm15       //if 180 degree symm then should be zero.
  //let's get the test-results in the output
  vpxor xmm0,xmm0,xmm0         //xmm0 is zero
  vpcmpeqq xmm1, xmm13, xmm0  //Horz symm
  vpcmpeqq xmm2, xmm12, xmm0  //Vert symm
  vpcmpeqq xmm3, xmm11, xmm0  //180 symm

  // blend the results down into ymm11   word origin
  vpblendw xmm0, xmm0, xmm1, $AA   // 1010 1010
  vpblendw xmm1, xmm2, xmm3, $AA   // 3232 3232
  vpblendw xmm0, xmm0, xmm1, $CC   // 3210 3210

  //combine bytes of ymm10 and ymm11 together into ymm10, byte wise
  // ymm10 77553311 77553311
  // ymm11 88664422 88664422   before shift
  // ymm10 07050301 07050301
  // ymm11 80604020 80604020   after shift
  //result 87654321 87654321   combined
  vpsrlw ymm0,ymm0,8
  vpsllw ymm1,ymm0,8
  vpor xmm0,xmm0,xmm1


end;



//Untested
function GeneratePtoQ_AVX256(main, N,W,NW: pointer): byte;
asm
    //parameters
    //rcx: main
    //rdx: N
    //r8: W
    //r9: NW
    // A cell looks like this:
    //   BCD   123
    //   AxA   405
    //   BCD   678
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu ymm15,[rcx]          //***** ymm15 8-D'
    vmovdqu ymm1,[rdx]           //N
    vmovdqu ymm4,[r8]            //W
    vmovdqu ymm3,[r9]            //NW
    //now create 9 planes, each one slightly shifted.
    //ymm0 is the center plane.
    vpsrldq ymm3,ymm3,32-4       //keep the bottom 2 rows of NW and shift them to the top
    vpsrldq ymm6,ymm1,32-2       //N5           keep the bottom 1 rows of N and shift them to the top.
    vpsrldq ymm1,ymm1,32-4       //N3           keep the bottom 2 rows of N and shift them to the top.
    vpsrlw ymm2,ymm4,14          //W6           keep the 2 rightmost columns of W
    vpsrlw ymm3,ymm3,14          //NW1          keep the 2 rightmost columns of NW
    vpslldq ymm5,ymm15,4         //main3        remove the bottom 2 rows from main
    vpslldq ymm7,ymm15,2         //main5        remove the bottom 1 row from main
    vpxor ymm14,ymm1,ymm5        //***** ymm14 3 - D    2 rows N +14 rows main
    vpxor ymm13,ymm7,ymm6        //***** ymm13 5 - A'   1 row N  +15 rows main
    //we are done with N, ymm1 and ymm6
    vpsrlw ymm1,ymm2,1           //W7           remove an extra column from W
    vpsllw ymm7,ymm15,1          //main7        Shift main right
    vpsllw ymm8,ymm13,1          //main0+N0     Shift main+N1 right
    vpsllw ymm9,ymm14,1          //main2+N2     Shift mainn+N2 right
    vpxor ymm12,ymm7,ymm1        //***** ymm12 7 - C Main7+W7
    vpsllw ymm7,ymm7,1           //main6       Shift main right
    vpxor ymm11,ymm7,ymm2        //***** ymm11 6 - B' Main6+W6
    vpslldq ymm10,ymm11,2        //main4+W4    Shift Main6W6 down
    vpsrldq ymm7,ymm3,2          //NW4         Shift NW1 up (only one row)
    vpsllw ymm6,ymm6,2           //N4          Shift N3 right
    vpxor ymm10,ymm10,ymm7       //main4+W4+NW4
    vpxor ymm10,ymm10,ymm6       //***** ymm10 4 - A
    vpslldq ymm1,ymm1,2          //W0          Shift W7 down 1 row
    vpsrlw ymm7,ymm7,1           //NW0         Shift NW4 left (keep only 1 pixel)
    vpxor ymm0,ymm8,ymm1         //main0+N0+W0
    vpxor ymm0,ymm0,ymm7         //***** ymm0 0 - x
    vpslldq ymm1,ymm2,4          //W1          Shift W down 2 rows
    vpsllw ymm8,ymm9,1           //main1+N1    Shift Main2N2 right 1 column
    vpxor ymm8,ymm8,ymm1         //main1+N1+W1 Combine with W
    vpxor ymm8,ymm8,ymm3         //**** ymm8 1 - B  Combine with the original NW
    vpsrlw ymm7,ymm1,1           //W2          Shift W1 left 1 column
    vpsrlw ymm5,ymm3,1           //NW2         Shift the original NW left 1 column
    vpxor ymm1,ymm7,ymm5         //W2+NW2      combine w2 and NW2
    vpxor ymm9,ymm9,ymm1         //**** ymm9 2 - C' main2+N2+W2+NW2
    //register usage
    //ymm0:   0-x
    //ymm8:   1-B
    //ymm9:   2-C'
    //ymm10:  4-A
    //ymm11:  6-B'
    //ymm12:  7-C
    //ymm13:  5-A'
    //ymm14:  3-D
    //ymm15:  8-D'
    //First get all the counts
    vpxor ymm1,ymm12,ymm9        //1's count of c
    vpand ymm2,ymm12,ymm9        //2's count of c
    vpxor ymm3,ymm10,ymm13       //1's count of a
    vpand ymm4,ymm10,ymm13       //2's count of a
    vpxor ymm5,ymm8,ymm11        //1's count of b
    vpand ymm6,ymm8,ymm11        //2's count of b
    vpxor ymm7,ymm14,ymm15       //1's count of d
    vpand ymm8,ymm14,ymm15       //2's count of d
    //Now add the 1's together
    vpand ymm10,ymm1,ymm3        //2's count of CA
    vpxor ymm1,ymm1,ymm3         //combined ones of CA
    vpand ymm12,ymm5,ymm7        //2's count of BD
    vpxor ymm5,ymm5,ymm7         //combined ones of BD
    vpand ymm14,ymm1,ymm5        //2's count of CABD
    vpxor ymm1,ymm1,ymm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand ymm3,ymm2,ymm4         //4's count of ca
    vpxor ymm2,ymm2,ymm4         //2's count of ca
    vpand ymm5,ymm6,ymm8         //4's count of bd
    vpxor ymm6,ymm6,ymm8         //2's count of bd
    vpand ymm7,ymm10,ymm12       //4's count of CABD
    vpxor ymm8,ymm10,ymm12       //2's count of CABD
    vpand ymm9,ymm2,ymm6         //4's count of cabd
    vpxor ymm4,ymm2,ymm6         //2's count of cabd
    vpand ymm11,ymm8,ymm14       //4's count of CABD+abcd
    vpxor ymm12,ymm8,ymm14       //2's count of CABD+abcd
    //add all 4's
    vpor ymm15,ymm3,ymm5
    vpor ymm13,ymm7,ymm9
    vpor ymm14,ymm11,ymm15
    //add the 2's
    vpxor ymm2,ymm12,ymm4
    //final add
    vpor ymm4,ymm14,ymm13
    //now we have all the counts.
    //register usage:
    //ymm0 - original pattern
    //ymm1 - count of the ones
    //ymm2 - count of the twos
    //ymm4 - count of the fours
    vpand ymm8,ymm0,ymm2         //anything with a 2 stays the same
    //movdqu ymm0,[rcx+2048]        //Compare the new data with the data 2 gens back.
    vpand ymm3,ymm2,ymm1         //anything with a 3 is alive
    vpor ymm1,ymm8,ymm3          //add the alive cells
    //We have now generated Q from P.
    //We want to know the changes in key sectors of Q, and will check N,W,NW
    //because this will become the S,E,SE of the next generation.
    //bit 7 of al will be shifted into bit 16 of eax at a later stage and so on....
    //The status bit work as follows
    //ymm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //       +    same      |  dead             +
    //       NW   W    N   all   NW   W    N   all
    vpandn ymm1,ymm4,ymm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx+2048],ymm1       //store the result of the calculation
    vpxor ymm15,ymm15,ymm15      //ymm15 holds a 0
    vpxor ymm2,ymm0,ymm1         //see which cells have changed
    vpsrldq ymm3,ymm1,32-4       //new N     //todo: see if this matches with the def of N above
    vpsrldq ymm4,ymm2,32-4       //changes in N
    vpsrlw ymm5,ymm1,32-2        //new W
    vpsrlw ymm6,ymm2,32-2        //changes in W
    vpsrlw ymm7,ymm3,32-2        //new NW
    vpsrlw ymm8,ymm4,32-2        //changes in NW
    // test the 2 qwords in each vector against zero
    vpcmpeqq ymm11, ymm1, ymm15  //all alive
    vpcmpeqq ymm12, ymm3, ymm15  //N
    vpcmpeqq ymm13, ymm5, ymm15  //W
    vpcmpeqq ymm14, ymm7, ymm15  //NW

    // blend the results down into ymm10   word origin
    vpblendw ymm10, ymm11, ymm12, $AA   // 3131 3131
    vpblendw ymm13, ymm13, ymm14, $AA   // 7575 7575
    vpblendw ymm10, ymm10, ymm13, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq ymm11, ymm2, ymm15  //All changes
    vpcmpeqq ymm12, ymm4, ymm15  //N
    vpcmpeqq ymm13, ymm6, ymm15  //W
    vpcmpeqq ymm14, ymm8, ymm15  //NW

    // blend the results down into ymm11   word origin
    vpblendw ymm11, ymm11, ymm12, $AA   // 4242 4242
    vpblendw ymm13, ymm13, ymm14, $AA   // 8686 8686
    vpblendw ymm11, ymm11, ymm13, $CC   // 8642 8642

    //combine bytes of ymm10 and ymm11 together into ymm10, byte wise
    // ymm10 77553311 77553311
    // ymm11 88664422 88664422   before shift
    // ymm10 07050301 07050301
    // ymm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw ymm10,ymm10,8
    vpsllw ymm11,ymm11,8
    vpor ymm10,ymm10,ymm11

    //combine the low and high dqword to make sure both are set (indicating the orginal was zero).
    vpsrldq ymm12,ymm10,8
    vpand ymm10,ymm10,ymm12
    vpmovmskb eax,ymm10
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    //ymm0 is the old unit
    //ymm1 is the new unit
    //al holds the status
end;  (**)


(*
function GenerateQtoP_AVX(Main, S,E,SE: pointer): byte;
asm
    //parameters:
    //rcx - PMain
    //rdx - PS
    //r8 - PE
    //r9 - PSE
    //returns cellstate in AL.
    //and result in XMM0
    // A cell looks like this:
    //   DCB   876
    //   AxA   504
    //   DCB   321
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu xmm15,[rcx]           //***** xmm15 8-D'
    vmovdqu xmm1,[rdx]            //S
    vmovdqu xmm2,[r8]             //E
    vmovdqu xmm3,[r9]             //SE
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpslldq xmm3,xmm3,16-4       //keep the bottom 2 rows of SE and shift them to the top
    vpslldq xmm6,xmm1,16-2       //S5           keep the top 1 rows of S and shift them to the bottom.
    vpslldq xmm1,xmm1,16-4       //S3           keep the top 2 rows of S and shift them to the bottom.
    vpsllw xmm2,xmm2,14          //E6           keep the 2 leftmost columns of E
    vpsllw xmm3,xmm3,14          //SE1          keep the 2 leftmost columns of SE
    vpsrldq xmm5,xmm15,4         //main3        remove the top 2 rows from main
    vpsrldq xmm4,xmm15,2         //main5        remove the top 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows S +14 rows main
    vpxor xmm13,xmm4,xmm6        //***** xmm13 5 - A'   1 row S  +15 rows main
    //we are done with S, xmm1 and xmm6
    vpsllw xmm1,xmm2,1           //E7           remove an extra column from E
    vpsrlw xmm7,xmm15,1          //main7        Shift main left
    vpsrlw xmm8,xmm13,1          //main0+S0     Shift main+S1 left
    vpsrlw xmm9,xmm14,1          //main2+S2     Shift main+S2 left
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+E7
    vpsrlw xmm7,xmm7,1           //main6       Shift main left
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+E6
    vpsrldq xmm10,xmm11,2        //main4+E4    Shift Main6E6 up
    vpslldq xmm7,xmm3,2          //SE4         Shift SE1 down (only one row)
    vpsrlw xmm6,xmm6,2           //S4          Shift S3 left
    vpxor xmm10,xmm10,xmm7       //main4+E4+SE4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpsrldq xmm1,xmm1,2          //E0          Shift E7 up 1 row
    vpsllw xmm7,xmm7,1           //SE0         Shift SE4 right (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+S0+E0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpsrldq xmm1,xmm2,4          //E1          Shift E up 2 rows
    vpsrlw xmm8,xmm9,1           //main1+S1    Shift Main2S2 left 1 column
    vpxor xmm8,xmm8,xmm1         //main1+S1+E1 Combine with E
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original SE
    vpsllw xmm4,xmm1,1           //E2          Shift E1 right 1 column
    vpsllw xmm5,xmm3,1           //SE2         Shift the original SE right 1 column
    vpxor xmm1,xmm4,xmm5         //E2+SE2      combine E2 and SE2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+S2+E2+SE2
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now we have two sets of 4 counts.
    //In the odd registers we have all the 1's
    //In the even registers we have the 2's.
    //Count the two groups of 4's as follows:
    //A:xor 1=count=1,3, 0=count=0,2,4
    //B:and 1=count = 4, 0=count=0123
    //C:or 0=count=0, 1=count=1234
    //D:not(A)-C-B = count = 2
    //if count is 3 then 1and2 xor3and4 will always be 1.
    //Let's process the 1's first.
    vpxor xmm11,xmm1,xmm3         //odds 1 xor 3
    vpxor xmm12,xmm5,xmm7         //odds 5 xor 7
    vpand xmm13,xmm1,xmm3         //twos 1 and 3
    vpand xmm15,xmm5,xmm7         //twos 5 and 7
    vpand xmm10,xmm11,xmm12       //1:some 2's
    vpand xmm14,xmm13,xmm15       //1:count=4
    vpxor xmm5,xmm11,xmm12        //1:count=1
    vpxor xmm15,xmm13,xmm15       //1:count=3+the other 2's
    vpandn xmm12,xmm5,xmm15       //23 - 13 ->1:count = the other 2's
    vpor xmm12,xmm12,xmm10        //some 2's+the other 2's = all 2's
    vpand xmm13,xmm15,xmm5        //13 and 23 -> 1:count = 3
    vpandn xmm11,xmm15,xmm5       //13 - 23 ->1:count = 1

    //Now process the 2's
    vpxor xmm1,xmm2,xmm4          //odds A
    vpxor xmm2,xmm6,xmm8          //odds B
    vpand xmm3,xmm2,xmm4          //evens A
    vpand xmm5,xmm6,xmm8          //evens B
    vpand xmm6,xmm1,xmm2          //some of the 2's
    vpor xmm7,xmm2,xmm4           //
    vpor xmm8,xmm6,xmm8
    vpor xmm7,xmm7,xmm8           //count 1234




    //vpand xmm10,xmm1,xmm3        //2's count of CA
    //vpxor xmm1,xmm1,xmm3         //combined ones of CA
    //vpand xmm12,xmm5,xmm7        //2's count of BD
    //vpxor xmm5,xmm5,xmm7         //combined ones of BD
    //vpand xmm14,xmm1,xmm5        //2's count of CABD
    //vpxor xmm1,xmm1,xmm5         //final count of the 1's
    ////now we need to add all the 2's together.
    //vpand xmm3,xmm2,xmm4         //4's count of ca
    //vpxor xmm2,xmm2,xmm4         //2's count of ca
    //vpand xmm5,xmm6,xmm8         //4's count of bd
    //vpxor xmm6,xmm6,xmm8         //2's count of bd
    //vpand xmm7,xmm10,xmm12       //4's count of CABD
    //vpxor xmm8,xmm10,xmm12       //2's count of CABD
    //vpand xmm9,xmm2,xmm6         //4's count of cabd
    //vpxor xmm4,xmm2,xmm6         //2's count of cabd
    //vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    //vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    ////add all 4's
    //vpor xmm15,xmm3,xmm5
    //vpor xmm13,xmm7,xmm9
    //vpor xmm14,xmm11,xmm15
    ////add the 2's
    //vpxor xmm2,xmm12,xmm4
    //final add
    //vpor xmm4,xmm14,xmm13
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    vpand xmm7,xmm0,xmm12         //anything with a 2 stays the same
    //vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm7,xmm13          //add the alive cells
    //Record the changes in S,E,SE.
    //This will influence the N,W,NW in the next generation.
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //       +    same         |  dead          +
    //       SE   E    S   all   SE   E    S   all
    vpandn xmm1,xmm14,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx-2048],xmm1       //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    vpxor xmm9,xmm0,xmm1         //see which cells have changed
    vpslldq xmm2,xmm1,16-4       //new S
    vpslldq xmm6,xmm9,16-4       //changes in S
    vpsllw xmm3,xmm1,16-2        //new E
    vpsllw xmm7,xmm9,16-2        //changes in E
    vpsllw xmm4,xmm2,16-2        //new SE
    vpsllw xmm8,xmm6,16-2        //changes in SE
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm11, xmm1, xmm15
    vpcmpeqq xmm12, xmm3, xmm15
    vpcmpeqq xmm13, xmm5, xmm15
    vpcmpeqq xmm14, xmm7, xmm15

    // blend the results down into xmm10   word origin
    vpblendw xmm10, xmm11, xmm12, $AA   // 3131 3131
    vpblendw xmm13, xmm13, xmm14, $AA   // 7575 7575
    vpblendw xmm10, xmm10, xmm13, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm11, xmm2, xmm15
    vpcmpeqq xmm12, xmm4, xmm15
    vpcmpeqq xmm13, xmm6, xmm15
    vpcmpeqq xmm14, xmm8, xmm15

    // blend the results down into xmm11   word origin
    vpblendw xmm11, xmm11, xmm12, $AA   // 4242 4242
    vpblendw xmm13, xmm13, xmm14, $AA   // 8686 8686
    vpblendw xmm11, xmm11, xmm13, $CC   // 8642 8642

    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm10,xmm10,8
    vpsllw xmm11,xmm11,8
    vpor xmm10,xmm10,xmm11

    //combine the low and high dqword to make sure both are zero.
    vpsrldq xmm12,xmm10,8
    vpand xmm10,xmm10,xmm12
    vpmovmskb eax,xmm10
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    or al, ah
    //xmm0 is the old unit
    //xmm1 is the new unit
    //al holds the status
end;

function GeneratePtoQ_AVX(main, N,W,NW: pointer): byte;
asm
    //parameters
    //rcx: main
    //rdx: N
    //r8: W
    //r9: NW
    // A cell looks like this:
    //   BCD   123
    //   AxA   405
    //   BCD   678
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
    vmovdqu xmm15,[rcx]          //***** xmm15 8-D'
    vmovdqu xmm1,[rdx]           //N
    vmovdqu xmm2,[r8]            //W
    vmovdqu xmm3,[r9]            //NW
    //now create 9 planes, each one slightly shifted.
    //xmm0 is the center plane.
    vpsrldq xmm3,xmm3,16-4       //keep the bottom 2 rows of NW and shift them to the top
    vpsrldq xmm6,xmm1,16-2       //N5           keep the bottom 1 rows of N and shift them to the top.
    vpsrldq xmm1,xmm1,16-4       //N3           keep the bottom 2 rows of N and shift them to the top.
    vpsrlw xmm2,xmm2,14          //W6           keep the 2 rightmost columns of W
    vpsrlw xmm3,xmm3,14          //NW1          keep the 2 rightmost columns of NW
    vpslldq xmm5,xmm15,4         //main3        remove the bottom 2 rows from main
    vpslldq xmm4,xmm15,2         //main5        remove the bottom 1 row from main
    vpxor xmm14,xmm1,xmm5        //***** xmm14 3 - D    2 rows N +14 rows main
    vpxor xmm13,xmm4,xmm6        //***** xmm13 5 - A'   1 row N  +15 rows main
    //we are done with N, xmm1 and xmm6
    vpsrlw xmm1,xmm2,1           //W7           remove an extra column from W
    vpsllw xmm7,xmm15,1          //main7        Shift main right
    vpsllw xmm8,xmm13,1          //main0+N0     Shift main+N1 right
    vpsllw xmm9,xmm14,1          //main2+N2     Shift mainn+N2 right
    vpxor xmm12,xmm7,xmm1        //***** xmm12 7 - C Main7+W7
    vpsllw xmm7,xmm7,1           //main6       Shift main right
    vpxor xmm11,xmm7,xmm2        //***** xmm11 6 - B' Main6+W6
    vpslldq xmm10,xmm11,2        //main4+W4    Shift Main6W6 down
    vpsrldq xmm7,xmm3,2          //NW4         Shift NW1 up (only one row)
    vpsllw xmm6,xmm6,2           //N4          Shift N3 right
    vpxor xmm10,xmm10,xmm7       //main4+W4+NW4
    vpxor xmm10,xmm10,xmm6       //***** xmm10 4 - A
    vpslldq xmm1,xmm1,2          //W0          Shift W7 down 1 row
    vpsrlw xmm7,xmm7,1           //NW0         Shift NW4 left (keep only 1 pixel)
    vpxor xmm0,xmm8,xmm1         //main0+N0+W0
    vpxor xmm0,xmm0,xmm7         //***** xmm0 0 - x
    vpslldq xmm1,xmm2,4          //W1          Shift W down 2 rows
    vpsllw xmm8,xmm9,1           //main1+N1    Shift Main2N2 right 1 column
    vpxor xmm8,xmm8,xmm1         //main1+N1+W1 Combine with W
    vpxor xmm8,xmm8,xmm3         //**** xmm8 1 - B  Combine with the original NW
    vpsrlw xmm4,xmm1,1           //W2          Shift W1 left 1 column
    vpsrlw xmm5,xmm3,1           //NW2         Shift the original NW left 1 column
    vpxor xmm1,xmm4,xmm5         //W2+NW2      combine w2 and NW2
    vpxor xmm9,xmm9,xmm1         //**** xmm9 2 - C' main2+N2+W2+NW2
    //register usage
    //xmm0:   0-x
    //xmm8:   1-B
    //xmm9:   2-C'
    //xmm10:  4-A
    //xmm11:  6-B'
    //xmm12:  7-C
    //xmm13:  5-A'
    //xmm14:  3-D
    //xmm15:  8-D'
    //First get all the counts
    vpxor xmm1,xmm12,xmm9        //1's count of c
    vpand xmm2,xmm12,xmm9        //2's count of c
    vpxor xmm3,xmm10,xmm13       //1's count of a
    vpand xmm4,xmm10,xmm13       //2's count of a
    vpxor xmm5,xmm8,xmm11        //1's count of b
    vpand xmm6,xmm8,xmm11        //2's count of b
    vpxor xmm7,xmm14,xmm15       //1's count of d
    vpand xmm8,xmm14,xmm15       //2's count of d
    //Now add the 1's together
    vpand xmm10,xmm1,xmm3        //2's count of CA
    vpxor xmm1,xmm1,xmm3         //combined ones of CA
    vpand xmm12,xmm5,xmm7        //2's count of BD
    vpxor xmm5,xmm5,xmm7         //combined ones of BD
    vpand xmm14,xmm1,xmm5        //2's count of CABD
    vpxor xmm1,xmm1,xmm5         //final count of the 1's
    //now we need to add all the 2's together.
    vpand xmm3,xmm2,xmm4         //4's count of ca
    vpxor xmm2,xmm2,xmm4         //2's count of ca
    vpand xmm5,xmm6,xmm8         //4's count of bd
    vpxor xmm6,xmm6,xmm8         //2's count of bd
    vpand xmm7,xmm10,xmm12       //4's count of CABD
    vpxor xmm8,xmm10,xmm12       //2's count of CABD
    vpand xmm9,xmm2,xmm6         //4's count of cabd
    vpxor xmm4,xmm2,xmm6         //2's count of cabd
    vpand xmm11,xmm8,xmm14       //4's count of CABD+abcd
    vpxor xmm12,xmm8,xmm14       //2's count of CABD+abcd
    //add all 4's
    vpor xmm15,xmm3,xmm5
    vpor xmm13,xmm7,xmm9
    vpor xmm14,xmm11,xmm15
    //add the 2's
    vpxor xmm2,xmm12,xmm4
    //final add
    vpor xmm4,xmm14,xmm13
    //now we have all the counts.
    //register usage:
    //xmm0 - original pattern
    //xmm1 - count of the ones
    //xmm2 - count of the twos
    //xmm4 - count of the fours
    vpand xmm8,xmm0,xmm2         //anything with a 2 stays the same
    vpand xmm3,xmm2,xmm1         //anything with a 3 is alive
    vpor xmm1,xmm8,xmm3          //add the alive cells
    //We have now generated Q from P.
    //We want to know the changes in key sectors of Q, and will check N,W,NW
    //because this will become the S,E,SE of the next generation.
    //bit 7 of al will be shifted into bit 16 of eax at a later stage and so on....
    //The status bit work as follows
    //xmm    8    7    6    5    4    3    2    1
    //bit    7----6----5----4----3----2----1----0
    //       +    same      |  dead             +
    //       NW   W    N   all   NW   W    N   all
    vpandn xmm1,xmm4,xmm1        //subtract the cells with 4 or more neighbors
    vmovdqu [rcx+2048],xmm1       //store the result of the calculation
    vpxor xmm15,xmm15,xmm15      //xmm15 holds a 0
    vpxor xmm9,xmm0,xmm1         //see which cells have changed
    vpsrldq xmm2,xmm1,16-4       //new N
    vpsrldq xmm6,xmm9,16-4       //changes in N
    vpsrlw xmm3,xmm1,16-2        //new W
    vpsrlw xmm7,xmm9,16-2        //changes in W
    vpsrlw xmm4,xmm2,16-2        //new NW
    vpsrlw xmm8,xmm6,16-2        //changes in NW
    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm11, xmm1, xmm15
    vpcmpeqq xmm12, xmm3, xmm15
    vpcmpeqq xmm13, xmm5, xmm15
    vpcmpeqq xmm14, xmm7, xmm15

    // blend the results down into xmm10   word origin
    vpblendw xmm10, xmm11, xmm12, $AA   // 3131 3131
    vpblendw xmm13, xmm13, xmm14, $AA   // 7575 7575
    vpblendw xmm10, xmm10, xmm13, $CC   // 7531 7531

    // test the 2 qwords in each vector against zero
    vpcmpeqq xmm11, xmm2, xmm15
    vpcmpeqq xmm12, xmm4, xmm15
    vpcmpeqq xmm13, xmm6, xmm15
    vpcmpeqq xmm14, xmm8, xmm15

    // blend the results down into xmm11   word origin
    vpblendw xmm11, xmm11, xmm12, $AA   // 4242 4242
    vpblendw xmm13, xmm13, xmm14, $AA   // 8686 8686
    vpblendw xmm11, xmm11, xmm13, $CC   // 8642 8642

    //combine bytes of xmm10 and xmm11 together into xmm10, byte wise
    // xmm10 77553311 77553311
    // xmm11 88664422 88664422   before shift
    // xmm10 07050301 07050301
    // xmm11 80604020 80604020   after shift
    //result 87654321 87654321   combined
    vpsrlw xmm10,xmm10,8
    vpsllw xmm11,xmm11,8
    vpor xmm10,xmm10,xmm11

    //combine the low and high dqword to make sure both are set (indicating the orginal was zero).
    vpsrldq xmm12,xmm10,8
    vpand xmm10,xmm10,xmm12
    vpmovmskb eax,xmm10
    //P and Q are stored in the same memory block.
    //first an array of 128 P block (16 bytes each) and then an array of 128 Q blocks (16 bytes each).
    not eax
    or al, ah
    //xmm0 is the old unit
    //xmm1 is the new unit
    //al holds the status
end;  (**)


end.

