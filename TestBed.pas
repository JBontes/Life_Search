unit TestBed;

{$ALIGN 16}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Generics.Collections,
  Display;

type
  TForm34 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure ShowCell(Canvas: TCanvas; Data: Int64);
    procedure Test;
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TLifePointer = Integer;

  //LifeCells are records stored in a TList<TLifeCell>
  //pointers to cells to display are stored in a separate list

  TCoor = record
  case boolean of
    true:(x,y: smallint);
    false:(xy: integer);
  end;

const
  BitsPerUnit = 128;
  BytesPerUnit = BitsPerUnit div 8;
  MaxBytesPerUnit = BytesPerUnit - 1;
  XCount = 8;
  YCount = 16;
  MaxX = XCount -1;
  MaxY = YCount -1;
  UnitsPerBlock = 16*8;

type
  ///  The smallest unit in the universe.
  ///  Consists of one xmm/ymm register
  ///  Currently holds 128 bits in a box
  ///  16 bits wide and 8 bits high.
  PUnit = ^TUnit;
  TUnit = record
    procedure Display(const BitmapStart: pointer; const LineOffset: integer);
    case integer of
      1: (b: array[0..MaxBytesPerUnit] of byte);
      2: (w: array[0..(BytesPerUnit div 2)-1] of word);
      3: (i: array[0..(BytesPerUnit div 4)-1] of integer);
      4: (q: array[0..(BytesPerUnit div 8)-1] of int64);
  end;

  ///  Bitmap of active cells in a CellBlock
  ///  Relies on `tzcnt` to blaze through the units in need of calculation.
  PBorderStatus = ^TBorderStatus;
  TBitmap = record
    ///  Gets the index of the next set bit.
    ///  Feed an index of -1 to get the first set bit
    ///  Keep repeating until all bits have been processed.
    ///  if the result >= 128 then all bits have been processed.
    function Next(index: integer): integer;
    procedure TurnOn(index: integer);
    procedure TurnOff(index: integer);
    procedure ProcessNW(status: PBorderStatus);
    procedure ProcessSE(status: PBorderStatus);

    case integer of
      1:(b: array[0..((BitsPerUnit) div 32)-1] of integer);
      2:(q: array[0..((BitsPerUnit) div 64)-1] of int64);
  end;

  PCellBlock = ^TCellBlock;
  TCellBlock = record
    p: array[0..UnitsPerBlock-1] of TUnit;
    q: array[0..UnitsPerBlock-1] of TUnit;
  end;

  CellBlockHelper = record helper for PCellBlock
    class function New: PCellBlock; static;
    procedure Free;
  end;

  TUnitStat = (AllDead, NSDead, EWDead, CornerDead, AllStatic, NSStatic, EWStatic, CornerStatic);
  TUnitStatus = set of TUnitStat;

  ///  An array listing the status of bordering blocks.
  ///  0..7 : Status of N/S neighbors
  ///  8..23 : status of E/W neighbors
  ///  24: status of NW/SE neighbor
  TBorderStatus = array[0..8+16+1-1] of TUnitStatus;

  THalfLifeHeader = record
  public
    BorderStatus: TBorderStatus;
    ActiveBitmap: TBitmap;
    AliveCount: integer;        //count of the live units
  end;

  PLifeHeader = ^TLifeHeader;


  TLifeTree = record
  private const
    MaxDepth = 8;   //4^8 = 64k, which is the max X/Y coordinate of a CellBlock holding 128x128 bits x 2 states
  public type
    TNodeType = (ntNode, ntLeaf);
    PNode = ^TNode;
    TNode = record
      case TNodeType of
      ntNode: (Node: array[0..15] of PNode;
               NodeType: TNodeType);
      ntLeaf: (Leaf: array[0..15] of PLifeHeader);
    end;
  private
    FRoot: PNode;
    FNodeCount: Cardinal;
    FCellCount: UInt64;
    FCurrentDepth: integer;
  end;


  TLifeHeader = record
  private
    function North(index: integer): PUnit; inline;
    function West(index: integer): PUnit; inline;
    function NorthWest(index: integer): PUnit; inline;
    function South(index: integer): PUnit; inline;
    function East(index: integer): PUnit; inline;
    function SouthEast(index: integer): PUnit; inline;
  public
    procedure ProcessPtoQ;
    procedure ProcessQtoP;
    function GeneratePtoQ(const CellUnit: PUnit; index: integer): TUnitStatus;
    function GenerateQtoP(const CellUnit: PUnit; index: integer): TUnitStatus;
    procedure DisplayP(const Bitmap: TDIBits; const BoundingRect: TRect);
    procedure DisplayQ(const Bitmap: TDIBits; const BoundingRect: TRect);
  private
    p: THalfLifeHeader;
    q: THalfLifeHeader;
    CellBlock: PCellBlock;
    DisplayBitmap: TBitmap;
    Coordinate: TCoor;
    N,W,NW,S,E,SE: PLifeHeader;
    Parent: TLifeTree.PNode;
  end;


var
  Form34: TForm34;

implementation

{$R *.dfm}

uses
  System.Diagnostics, System.Math,
  HiResStopWatch,
  System.Types;


type
  TBitTotals = record
    total, bits: integer;
  end;


{ THalfLifeHeader }

procedure TestAdding;
var
  a,b,c: integer;
  total: integer;
  bitcount: integer;
  TotalCheck: array[0..19] of integer;

  function CountBits(a,b,c: Integer): integer;
  var
    i: Integer;
    input: integer;
  begin
    input:= a + (b shl 3) + (c shl 6);
    Result:= 0;
    for i := 0 to 31 do begin
      if Odd(input) then inc(Result);
      input:= input shr 1;
    end;
  end;

begin
  FillChar(TotalCheck, SizeOf(TotalCheck), #255); //-1
  for a := 0 to 7 do begin
    for b := 0 to 7 do begin
      for c:= 0 to 7 do begin
        if c in [2,3,6,7] then break;
        total:= a+b+c;
        bitcount:= CountBits(a,b,c);

        if TotalCheck[total] = -1 then TotalCheck[Total]:= bitcount
        else if TotalCheck[total] <> bitcount then begin
          Form34.memo1.Lines.Add(Format('Total = %d, bits = %d, check= %d',[total, bitcount,TotalCheck[Total]]));
        end;
      end;
    end;
  end;
end;

//    D D D D D D D D D D|C|C
//                       | |
//    D D D D D D D D D D|C|C
//    -------------------+ |
//    B B B B B B B B B B A|A
//    ---------------------+
//    B B B B B B B B B B A A

//157 instructions, 55 cycles = 2.85 instructies per cycle.

function Generate_QtoP_x64_Test2(A,B,C,D: Int64):Int64;
asm
  push rdi
  push rsi
  push rbx
  push r14
  push r15
  push r12
  push r13
  push rbp
  //rcx: A
  //rdx: B
  //r8:  C
  //r9:  D
  // Layout of a cell is
  //  DDD  C
  //  BBB  A

  mov rbp,$C0C0C0C0C0C0C0C0  //The leftmost 2 columns of A and C
  shr rdx,(64-16)            //Keep only the two top rows of B (and put them at the bottom)
  mov rsi,$1111111111111111  //counting mask
  and r8,rbp                 //leftmost 2 columns of C
  shr r8,(8-2)               //shift out the 6 columns we don't need
  //lea rdi,[rsi+rsi*4]        //$55555555  //fast counting mask
  mov rdi,$555555555555555
  and rcx,rbp                //The leftmost 2 columns of A
  shr rcx,(64-16+(8-2))      //shift out the bottom 6 rows and the 6 columns we don't need
  not rbp                    //mask for rightmost 6 columns of D/B to be combined with C/A

  //rcx: A
  //rdx: B
  //r8: C
  //r9,R10,rax: D
  //r8, r9, r10,r11: variables for C+D
  //r12,r13,r14,r15: variables for A+B
@CountNeighborsPerRow:      //count C+D, interleaved with count A+B
  ///////////Count the first 3 columns.
  ///
  //count 3 neighbors in 4 columns at once
  mov r11,rdi  //Cache D"
  mov rbx,r9  //Cache D'
  and r11,r9   //r11= bits in column2468
  shr rbx,1
  and rbx,rdi  //rbx= bits in column 3579
  mov rax,rdi
  and r9,rbp        //Mask off high (leftmost) 2 columns
  shl r9,2
  lea r10,[r8+r9]  //add colums 0+1 (D765432+C10)
  and rax,r10      //rax= bits in column 0246
  xor r10,rax        //r10=bits in column 1357
  shr r10,1
  //Add all the bits together
  lea r9,[r10+r11] //add bits 1357+2468 together
  lea r8,[r9+rax]  //r9 = NCount 1357  (1357+2468) +0246
  //////////////Count the next 3 columns
  //Do the same for the neighborhood counts in column
  //rax is live cells in 2468
  add r9,rbx    //column 2468+1357+3579 = NCount for columns 2468
  ///  register use:
  ///  r8: NCount D1357
  ///  r9: NCount D2468
  ///  r10: live bits in D1357
  ///  r11: live bits in D2468


  //mov r14,rcx
  //mov r15,rdx
  //xor r12,r12
  //xor r13,r13
  //or r14,rdx
  //jz @AandBareZero -> saves 3 cycles if a+b s always 0, not worth the bother

  ///////////Count the first 3 columns.
  mov r15,rdx   //count 3 neighbors in 4 columns at once
  mov r13,rdx   //break the dependency chain by caching B
  and r15,rdi   //r15= bits in column2468
  shr rdx,1
  and rdx,rdi   //rdx= bits in column 3579
  mov r12,rdi
  and r13,rbp   //Mask off leftmost 2 columns
  shl r13,2
  lea r14,[rcx+r13]   //add colums 0+1
  and r12,r14   //r12= bits in column 0246
  shr r14,1
  and r14,rdi   //r14=bitcount in column 1357
  //Add all the bitcounts together
  lea r13,[r14+r15] //1357+2468
  add r12,r13 //r12 = NCount 1357  (1357+2468)+0246
  //////////////Count the next 3 columns
  //Do the same for the neighborhood counts in colum

  add r13,rdx  //r13 = NCount 2468: (2468+1357) + 3579

@AandBareZero:

  //Now we're all done with the neighborhood counts in ABC and D
  ///  Register use
  ///  r12: NCount B1357
  ///  r13: NCount B2468
  ///  r14: live bits in B1357
  ///  r15: live bits in B2468
  lea rsi,[rsi+rsi*2] //Mask 333333333
  //mov rsi,$3333333333333333
  //mov rbp,rsi
  //not rbp             //Mask CCCCCCC
  lea rbp,[rsi*4]
  //mov rbp,$ccccccccccccccc

  @ExtractLiveCells:
  ///  We now need to extract just the middle part for the live cells
  shl r10,8     //middle part of D 1357
  shr r14,8     //middle part of B 1357
  add r10,r14   //Live cells 1357
  mov r14,r10   //copy

  shl r11,8     //middle part of D 2468
  shr r15,8     //middle part of B 2468
  add r11,r15   //Live cells 2468
  mov r15,r11   //copy

  and r10,rbp   //r10,r14 = Live cells 1357
  shr r10,2     //r10 = live cells 37
  and r14,rsi   //r8 = live cells 15


  and r11,rbp   //r11,r15 = Live cells 2468
  shr r11,2     //r11 = live cells 48
  and r15,rsi   //r9 = live cells

@AddCountsTogether:
  //input: r8:  Ncount for D1357
  //       r9:  Ncount for D2468    //rdx = copy
  //       r10: live cells for 1357 //r14 = copy
  //       r11: live cells for 2468 //r15 = copy
  //       r12: NCount for B1357
  //       r13: NCount for B2468    //rbx = copy
  //       rdi: mask 55555555
  //Push the live cells

  //Now we collect the counts in a 3x3 grid.
  // Start at the bottom and work our way up ??

  mov rdx, r9  //Neighborhood counts for D-2468
  mov rax,r9   //rax = D
  shl r9,8      //middle of D
  mov rbx, r13  //Neighborhood counts for B-2468, just the 2 rows
  //Get 3 counts: bottom-rcx, middle-rdx, top-rbx
  //the bottom is already in rcx
  shr r13,8     //bottom row of B
  shl rax,16    //bottom part of D
  add rax,rbx   //rax is the bottom part
  lea rbx,[r13+r9]   //now we have the middle slice in RBX
  // rdx = top
  // rbx = middle
  // rax = bottom

  //we need to split up the 4 columns in 2 pairs.
  mov r9,rsi
  mov r13,rsi      //Get masks
  mov rcx,rsi
  and r13,rax      //r13 = row 26-bottom
  and rcx,rbx      //rcx = row 26-middle
  and r9,rdx       //r9 - row 26-top
  //Extract 48
  and rax,rbp      //rax = row 48 - bottom
  and rbx,rbp      //rbx = row 48 - middle
  and rdx,rbp      //rdx - row 48 - top
  //Align 48 with 26
  shr rax,2        //align 48 with 26
  shr rbx,2        //align 48 with 26
  shr rdx,2        //align 48 with 26
  add rcx,r9       //middle 26 + top 26
  add rcx,r13      //top+middle+bottom 26
  add rax,rbx      //Add 48 rows (bottom+middle) together
  add rax,rdx      //Add top+middle+bottom 48 for 9 cell count

  //now we have the neighbor counts including the centre cells.
  //rax is NCount for columns 48
  //rcx is NCount for columns 26
  //Now we do the same for D1357 and B1357
  mov r9,r8       //Ncount for D1357
  mov rdx,r8      //Ncount for D1357
  mov rbx,r12     //NCount for B1357
  shl r8,8        //Middle of D
  shr r12,8       //middle of B

  shl rdx,16      //bottom part of D
  add rbx,rdx     //rbx = bottom
  add r8,r12      //r8=middle, r9 = top
  //Split up the 4 columns in 2 pairs
  mov r12,rsi     {for some reason mov x,mask+and x,value is faster than the other way round}
  mov r13,rsi     //gets masks
  mov rdx,rsi
  and r12,rbx     //bottom 15
  and r13,r8      //middle 15
  and rdx,r9      //top 15
  //fix up rbx,r8,r9
  and rbx,rbp     //bottom 37
  and r8,rbp      //middle 37
  and r9,rbp      //top 37
  //Add everything together
  shr rbx,2       //align 37 with 15
  shr r8,2        //align 37 with 15
  shr r9,2        //align 37 with 15
  add rdx,r13     //top+middle 15
  add rdx,r12     //top+middle+bottom 15
  add rbx,r8      //bottom+middle 37
  add rbx,r9      //top+middle+bottom 37


  //Register use is as follows:
  ///  rax:  NCount 48
  ///  rbx:  NCount 37
  ///  rcx:  NCount 26
  ///  rdx:  NCount 15

  //////////////////////////////////////////////////////
  ///  The following code is specific to a rule.
  ///  This code is tuned to 23/3
  ///  It needs to be patched for a different rule
  //////////////////////////////////////////////////////
  //Register use is as follows:
  ///  rax: NCount 48
  ///  rbx: NCount 37
  ///  rcx: NCount 26
  ///  rdx: NCount 15
  ///  r14: Live cells 15
  ///  r15: Live cells 26
  ///  r10: Live cells 37
  ///  r11: Live cells 48

/// Start with NCount 15
///  subtract the live cells from the cell count
  sub rdx,r14     //NCount 15 - Live 15
  sub rcx,r15     //NCount 26 - Live 26
  sub rbx,r10     //NCount 37 - Live 37
  sub rax,r11     //NCount 48 - Live 48
  xor rdx,rbp     //reg = not(NCount xor 3)
  xor rcx,rbp     //if dead+3 or Live+3 -> 0 xor FF = 1111, if 2 neighbors -> 1 xor FF = 1110
  xor rbx,rbp     //so it's a 1111 if new cell for sure, 1110 if maybe new cell
  xor rax,rbp     //and something else otherwise
  or rdx,r14      //if NCount=2 (1110) + life cell (0001) -> reg = 1111
  or rcx,r15      //otherwise reg has 0 somewhere.
  or rbx,r10
  or rax,r11
//////////////////////////////////////////////////////
///  End of the code that's specific to a rule.
/////////////////////////////////////////////////////

  //Register use is as follows:
  ///  rax: Dirty live counts 48 , nibble=0 if new live, other if dead
  ///  rbx: Dirty live counts 37
  ///  rcx: Dirty live counts 26
  ///  rdx: Dirty live counts 15
//Clean up the live counts
    //Truthtable for cell counts
//1.  Full cellcount                     0  	 1	  2    3	  4 	 5 	  6    7     8     9
//1.  Fill cellcount                  0000  0001 0010 0011 0100 0101 0110 0111  1000  1001
//2.  Full celcount xor 3	              11  	10	  1	   0	111  110 	101  100  1011  1010
//3.  Full Celcount and not life cell 0010  0010 0000	0000 0110 0110 0100 0100  1010  1010
//4.  Full celcount and not dead cell 0011  0010 0001	0000 0111 0110 0101 0100  1011  1010 //no-op from 2.
//3a  Not 3                           1101  1101 1111 1111 1001 1001 1011 1011  0101  0101
//4a  Not 4.                          1100  1101 1110 1111 1000 1001 1010 1011  0100  0101
//3b  bit 23 and bit 10 for 3a          01    01   11   11   00   00   10   10    01    01

  mov r8,rax      //Compress the 1111 into   0001
  mov r9,rbx      //and everything else into 0000
  mov r10,rcx
  //mov r11,rdx
  and rax,rsi     //First compress 4 bits into 2
  and rbx,rsi
  and rcx,rsi
  and rsi,rdx
  shl rax,2       //Shift rax up 3 places (2 now, 1 later)
  shl rbx,2       //shift rbx up 2 places
  shr r10,2       //shift rcx up 1 place (0 now, 1 later)
  shr rdx,2       //leave rsi
  and rax,r8      //combine with 23
  and rbx,r9
  and rcx,r10
  and rsi,rdx     //Now 1111 -> 0011  and all the rest into 10,01 or 00
  //combine the halfs
  or rax,rcx     //Combine the halfs
  or rbx,rsi
  mov r10,rax     //then compress 2 bits into 1
  //mov rdx,rbx
  and rax,rdi         //keep bit 0
  and rdi,rbx
  shl rax,1
  shr rbx,1
  and rax,r10    //and combine it with bit 1
  and rdi,rbx
  or rax,rdi    //add everything together

  pop rbp   //cleanup
  pop r13
  pop r12
  pop r15
  pop r14
  pop rbx
  pop rsi
  pop rdi
end;



//157 instructions for 64 cells = 2.5 instructions per cell.
//55 cycles = 2.85 instr per cycle

function Generate_PtoQ_x64_Test2(A,B,C,D: Int64):Int64;
asm
  push rdi   //rcx: A
  push rbx   //rdx: B
  push rsi   //r8:  C
  push r14   //r9:  D
  push r15   // Layout of a cell is
  push r12   //  A  B
  push r13   //  C  D
  push rbp

  mov rbp,$0303030303030303  //The rightmost 2 columns of A and C
  shl rdx,(64-16)            //Keep only the two lower rows of B
  and r8,rbp                 //Rightmost 2 columns of C
  mov rsi,$1111111111111111  //counting mask
  lea rdi,[rsi+rsi*4]        //$55555555  //fast counting mask
  shl r8,(8-2)               //shift out the 6 columns we don't need
  and ecx,ebp            //The rightmost 2 columns of A
  shl rcx,(64-16+(8-2))  //shift out the top 6 rows and the 6 columns we don't need
  not rbp                //mask for leftmost 6 columns of D/B to be combined with C/A

@CountNeighborsPerRow:      //count C+D, interleaved with count A+B
  ///////////Count the first 3 columns.
  ///
  //count 3 neighbors in 4 columns at once
  mov r10,r9  //Cache D'
  mov rax,rdi  //Cache D"
  and rax,r9   //r8= bits in column0246
  shr r10,1
  and r10,rdi  //r10= bits in column 1357
  mov r11,rdi
  and r9,rbp        //Mask off lower 2 columns
  shr r9,2
  lea rbx,[r8+r9]  //add colums 8+9
  and r11,rbx  //r11= bits in column 2468
  //Add all the bits together
  lea r9,[r11+r10] //add bits 1357+2468 together
  lea r8,[rax+r9]  //r9 = NCount 1357  (0246+1357+2468)
  //////////////Count the next 3 columns
  //Do the same for the neighborhood counts in column
  //r11 is live cells in 2468
  xor rbx,r11    //23456789 -/- 2468 = columns 3579
  shr rbx,1     //rbx=bitcount in column 3579
  add r9,rbx    //column 3579+2468+1357 = NCount for columns 2468
  ///  register use:
  ///  r8: NCount 1357
  ///  r9: NCount 2468
  ///  r10: live bits in 1357
  ///  r11: live bits in 2468


    ///////////Count the first 3 columns.
  mov r12,rdx   //break the dependency chain by caching B
  mov r13,rdx   //count 3 neighbors in 4 columns at once
  and r13,rdi   //r13= bits in column0246
  shr rdx,1
  and rdx,rdi  //r14= bits in column 1357
  mov r15,rdi
  and r12,rbp        //Mask off rightmost 2 columns
  shr r12,2
  add rcx,r12  //add colums 8+9
  and r15,rcx  //r15= bits in column 2468
  //Add all the bitcounts together
  lea rax,[r15+rdx] //1357+2468
  lea r12,[rax+r13] //r12 = NCount 1357  (0246+1357+2468)
  //////////////Count the next 3 columns
  //Do the same for the neighborhood counts in column
  //r15 is bitcount in 1357+2468
  //xor rcx,r15    //23456789 -/- 2468 = columns 3579
  shr rcx,1     //rcx=bitcount in column 3579
  and rcx,rdi
  lea r13,[rcx+rax]  //and add column 2468+1357 = NCount for columns 2468

  //Now we're all done with the neighborhood counts in ABC and D
  ///  Register use
  ///  r12: NCount 1357
  ///  r13: NCount 2468
  ///  rdx: live bits in 1357
  ///  r15: live bits in 2468
  lea rsi,[rsi+rsi*2] //Mask 333333333
  mov rbp,rsi
  not rbp             //Mask CCCCCCC

  @ExtractLiveCells:
  ///  We now need to extract just the middle part for the live cells
  shr r10,8     //middle part of D 1357
  shl rdx,8     //middle part of B 1357
  add r10,rdx   //Live cells 1357
  mov r14,r10

  shr r11,8     //middle part of D 2468
  shl r15,8     //middle part of B 2468
  add r11,r15   //Live cells 2468
  mov r15,r11

  and r10,rbp   //split up the live cells so they match with the counts.
  shr r10,2     //r10 = live cells 37
  and r14,rsi   //r8 = live cells 15

  and r11,rbp   //split up the live cells so they match with the counts.
  shr r11,2     //r11 = live cells 48
  and r15,rsi   //r9 = live cells 26

@AddRowsTogether:
  //input: r8:  Ncount for D1357  //rdx = copy
  //       r9:  Ncount for D2468
  //       r10: live cells for 1357
  //       r11: live cells for 2468
  //       r12: NCount for B1357  //rax = copy
  //       r13: NCount for B2468
  //       rdi: mask 11111111
  //Push the live cells

  //Now we collect the counts in a 3x3 grid.
  // Start at the bottom and work our way up.

  mov rdx, r9  //Neighborhood counts for D-2468
  mov rbx, r13  //Neighborhood counts for B-2468, just the 2 rows
  //Get 3 counts: bottom-rcx, middle-rdx, top-rbx
  //the bottom is already in rcx
  shr r9,8           //middle of D
  shl r13,8          //bottom row of B
  mov rax,rdx   //rax = D
  mov rcx,rsi
  shr rax,16    //top part of D
  add rax,rbx   //rax is the top part
  lea rbx,[r13+r9]   //now we have the middle row in RBX

  //we need to split up the 4 columns in 2 pairs.
  mov r13,rsi    //Get masks
  mov r9,rsi
  and r13,rax    //r11 = row 26-top
  and rcx,rbx    //rcx = row 26-middle
  and r9,rdx     //r9 - row 26-bottom
  //Extract 48
  and rax,rbp    //rax = row 48 - top
  and rbx,rbp    //rbx = row 48 - middle
  and rdx,rbp     //rdx - row 48 - bottom
  //Align 48 with 26
  //shr rax,2      //Line up 48 with 26
  //shr rbx,2      //No shortcuts or we'll get overflows and bitloss.
  shr rax,2
  shr rbx,2
  shr rdx,2
  add r9,rcx      //top 26 + middle 26
  lea rcx,[r9+r13] //top+middle+bottom 26
  add rax,rbx
  add rax,rdx    //Add top+middle+bottom for 9 cell count

  //now we have the neighbor counts including the centre cells.
  //rax is NCount for columns 48
  //rcx is NCount for columns 26
  //Now we do the same for D1357 and B1357
  mov r9,r8     //Ncount for D1357
  mov rdx,r8    //D
  mov rbx,r12    //NCount for B1357
  shr r8,8       //Middle of D
  shl r12,8      //middle of B

  shr rdx,16      //top part of D
  add rbx,rdx     //rbx = top
  add r8,r12      //r8=middle, r9 = bottom
  //Split up the 4 columns in 2 pairs
  mov r12,rsi     {for some reason mov x,mask+and x,value is faster than the other way round}
  mov r13,rsi
  mov rdx,rsi
  and r12,rbx     //top 15
  and r13,r8      //middle 15
  and rdx,r9      //bottom 15
  //fix up rbx,r8,r9
  and rbx,rbp     //top 37
  and r8,rbp      //middle 37
  and r9,rbp      //bottom 37
  //Add everything together
  shr rbx,2
  shr r8,2
  shr r9,2
  add rbx,r8      //top+middle 37
  add rbx,r9      //top+middle+bottom 37
  add rdx,r13     //top+middle 15
  add rdx,r12     //top+middle+bottom 15
  //Add everything together

  //Register use is as follows:
  ///  rax:  NCount 48
  ///  rbx:  NCount 37
  ///  rcx:  NCount 26
  ///  rdx:  NCount 15

  //////////////////////////////////////////////////////
  ///  The following code is specific to a rule.
  ///  This code is tuned to 23/3
  ///  It needs to be patched for a different rule
  //////////////////////////////////////////////////////
  //Register use is as follows:
  ///  rax: NCount 48
  ///  rbx: NCount 37
  ///  rdx: NCount 15
  ///  rcx:  NCount 26
  ///  r14: Live cells 15
  ///  r15: Live cells 26
  ///  r10: Live cells 37
  ///  r11: Live cells 48

/// Start with NCount 15
  sub rdx,r14     //subtract the live cells from the cell count
  sub rcx,r15
  sub rbx,r10
  sub rax,r11
  xor rdx,rbp    //reg = not(NCount xor 3)
  xor rcx,rbp    //if dead+3 or Live+3 -> 0 xor FF = 1111, if 2 neighbors -> 1 xor FF = 1110
  xor rbx,rbp    //so it's a 1111 if new cell for sure, 1110 if maybe new cell
  xor rax,rbp    //and something else otherwise
  or rdx,r14      //if NCount=2 (1110) + life cell (0001) -> reg = 1111
  or rcx,r15      //otherwise reg has 0 somewhere.
  or rbx,r10
  or rax,r11
//////////////////////////////////////////////////////
///  End of the code that's specific to a rule.
/////////////////////////////////////////////////////

  //Register use is as follows:
  ///  rax: Dirty live counts 48 , nibble=0 if new live, other if dead
  ///  rbx: Dirty live counts 37
  ///  rcx: Dirty live counts 26
  ///  rdx: Dirty live counts 15
//Clean up the live counts
    //Truthtable for cell counts
//1.  Full cellcount                     0  	 1	  2    3	  4 	 5 	  6    7     8     9
//1.  Fill cellcount                  0000  0001 0010 0011 0100 0101 0110 0111  1000  1001
//2.  Full celcount xor 3	              11  	10	  1	   0	111  110 	101  100  1011  1010
//3.  Full Celcount and not life cell 0010  0010 0000	0000 0110 0110 0100 0100  1010  1010
//4.  Full celcount and not dead cell 0011  0010 0001	0000 0111 0110 0101 0100  1011  1010 //no-op from 2.
//3a  Not 3                           1101  1101 1111 1111 1001 1001 1011 1011  0101  0101
//4a  Not 4.                          1100  1101 1110 1111 1000 1001 1010 1011  0100  0101
//3b  bit 23 and bit 10 for 3a          01    01   11   11   00   00   10   10    01    01

  mov r8,rax      //Compress the 1111 into   0001
  mov r9,rbx      //and everything else into 0000
  mov r10,rcx
  //mov r11,rdx
  and rax,rsi     //First compress 4 bits into 2
  and rbx,rsi
  and rcx,rsi
  and rsi,rdx
  shl rax,2       //Shift rax up 3 places (2 now, 1 later)
  shl rbx,2       //shift rbx up 2 places
  shr r10,2       //shift rcx up 1 place (0 now, 1 later)
  shr rdx,2       //leave rsi
  and rax,r8      //combine with 23
  and rbx,r9
  and rcx,r10
  and rsi,rdx     //Now 1111 -> 0011  and all the rest into 10,01 or 00
  //combine the halfs
  or rax,rcx     //Combine the halfs
  or rbx,rsi
  mov r10,rax     //then compress 2 bits into 1
  //mov rdx,rbx
  and rax,rdi         //keep bit 0
  and rdi,rbx
  shl rax,1
  shr rbx,1
  and rdi,rbx
  and rax,r10    //and combine it with bit 1
  or rax,rdi    //add everything together
  //cleanup
  pop rbp
  pop r13
  pop r12
  pop r15
  pop r14
  pop rsi
  pop rbx
  pop rdi
end;


//Layout of a cell
//Single cell: 8 wide x 8 bits high, fits into a single XMM register
//Layed out like so
//1:   FEDCBA98   line 7
//2:   76543210   line 6
// etc.
//The cells need to be shuffled around so that every byte in the
//register gets the neighboring cells in.
//To this end the bordering 3 cells need to be recruited as well
//They are staggered like so:
//
//    A A A A B B B B B B B B
//         +------------------
//    A A A|A B B B B B B B B
//         | +---------------
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
//        9 8 7 6 5 4 3 2 1 0

{$align 16}

type
  TInput = record
    main: array[0..7] of word;
    se: array[0..7] of word;
    s: array[0..7] of word;
    e: array[0..7] of word;

  end;


function Generate_QtoP_Full_AVX(main, S, E, SE: pointer): TUnitStatus;
asm
end;

function Generate_PtoQ_Full_AVX(main,N,W,NW: pointer): TUnitStatus;
const
  input: Tinput = (main:(0,0,0,8,8,8,0,0);
                   se:  (0,0,0,0,0,0,0,0);
                   s:   (0,0,0,0,0,0,0,0);
                   e:   (0,0,0,0,0,0,0,0));
asm
    // A cell looks like this:
    //   BCD   123                 //done: 035678
    //   AxA   405
    //   BCD   678
    // we're using half-adder logic to store the 1, 2 and 4's count in 3 bitplanes.
  movdqu xmm15,[main]
  movdqu xmm1,[N]
  movdqu xmm2,[W]
  movdqu xmm3,[NW]
db $C5,$E1,$73,$DB,$0C           // vpsrldq xmm3,xmm3,C                     //
db $C5,$C9,$73,$D9,$0E           // vpsrldq xmm6,xmm1,E                     //
db $C5,$F1,$73,$D9,$0C           // vpsrldq xmm1,xmm1,C                     //
db $C5,$E9,$71,$D2,$0E           // vpsrlw xmm2,xmm2,E                      //
db $C5,$E1,$71,$D3,$0E           // vpsrlw xmm3,xmm3,E                      //
db $C4,$C1,$51,$73,$FF,$04        // vpslldq xmm5,xmm15,4                    //
db $C4,$C1,$59,$73,$FF,$02        // vpslldq xmm4,xmm15,2                    //
db $C5,$71,$EF,$F5              // vpxor xmm14,xmm1,xmm5                   //
db $C5,$59,$EF,$EE              // vpxor xmm13,xmm4,xmm6                   //
db $C5,$F1,$71,$D2,$01           // vpsrlw xmm1,xmm2,1                      //
db $C4,$C1,$41,$71,$F7,$01        // vpsllw xmm7,xmm15,1                     //
db $C4,$C1,$39,$71,$F5,$01        // vpsllw xmm8,xmm13,1                     //
db $C4,$C1,$31,$71,$F6,$01        // vpsllw xmm9,xmm14,1                     //
db $C5,$41,$EF,$E1              // vpxor xmm12,xmm7,xmm1                   //
db $C5,$C1,$71,$F7,$01           // vpsllw xmm7,xmm7,1                      //
db $C5,$41,$EF,$DA              // vpxor xmm11,xmm7,xmm2                   //
db $C4,$C1,$29,$73,$FB,$02        // vpslldq xmm10,xmm11,2                   //
db $C5,$C1,$73,$DB,$02           // vpsrldq xmm7,xmm3,2                     //
db $C5,$C9,$71,$F6,$02           // vpsllw xmm6,xmm6,2                      //
db $C5,$29,$EF,$D7              // vpxor xmm10,xmm10,xmm7                  //
db $C5,$29,$EF,$D6              // vpxor xmm10,xmm10,xmm6                  //
db $C5,$F1,$73,$F9,$02           // vpslldq xmm1,xmm1,2                     //
db $C5,$C1,$71,$D7,$01           // vpsrlw xmm7,xmm7,1                      //
db $C5,$B9,$EF,$C1              // vpxor xmm0,xmm8,xmm1                    //
db $C5,$F9,$EF,$C7              // vpxor xmm0,xmm0,xmm7                    //
db $C5,$F1,$73,$FA,$04           // vpslldq xmm1,xmm2,4                     //
db $C4,$C1,$39,$71,$F1,$01        // vpsllw xmm8,xmm9,1                      //
db $C5,$39,$EF,$C1              // vpxor xmm8,xmm8,xmm1                    //
db $C5,$39,$EF,$C3              // vpxor xmm8,xmm8,xmm3                    //
db $C5,$D9,$71,$D1,$01           // vpsrlw xmm4,xmm1,1                      //
db $C5,$D1,$71,$D3,$01           // vpsrlw xmm5,xmm3,1                      //
db $C5,$D9,$EF,$CD              // vpxor xmm1,xmm4,xmm5                    //
db $C5,$31,$EF,$C9              // vpxor xmm9,xmm9,xmm1                    //
db $C4,$C1,$39,$EF,$C9           // vpxor xmm1,xmm8,xmm9                    //
db $C4,$C1,$39,$DB,$D1           // vpand xmm2,xmm8,xmm9                    //
db $C4,$C1,$29,$EF,$DB           // vpxor xmm3,xmm10,xmm11                  //
db $C4,$C1,$29,$DB,$E3           // vpand xmm4,xmm10,xmm11                  //
db $C4,$C1,$19,$EF,$ED           // vpxor xmm5,xmm12,xmm13                  //
db $C4,$C1,$19,$DB,$F5           // vpand xmm6,xmm12,xmm13                  //
db $C4,$C1,$09,$EF,$FF           // vpxor xmm7,xmm14,xmm15                  //
db $C4,$41,$09,$DB,$C7           // vpand xmm8,xmm14,xmm15                  //
db $C5,$71,$DB,$D3               // vpand xmm10,xmm1,xmm3                   //
db $C5,$F1,$EF,$CB               // vpxor xmm1,xmm1,xmm3                    //
db $C5,$51,$DB,$E7               // vpand xmm12,xmm5,xmm7                   //
db $C5,$D1,$EF,$EF               // vpxor xmm5,xmm5,xmm7                    //
db $C5,$71,$DB,$F5               // vpand xmm14,xmm1,xmm5                   //
db $C5,$F1,$EF,$CD               // vpxor xmm1,xmm1,xmm5                    //
db $C5,$E9,$DB,$DC               // vpand xmm3,xmm2,xmm4                    //
db $C5,$E9,$EF,$D4               // vpxor xmm2,xmm2,xmm4                    //
db $C4,$C1,$49,$DB,$E8           // vpand xmm5,xmm6,xmm8                    //
db $C4,$C1,$49,$EF,$F0           // vpxor xmm6,xmm6,xmm8                    //
db $C4,$C1,$29,$DB,$FC           // vpand xmm7,xmm10,xmm12                  //
db $C4,$41,$29,$EF,$C4           // vpxor xmm8,xmm10,xmm12                  //
db $C5,$69,$DB,$CE              // vpand xmm9,xmm2,xmm6                    //
db $C5,$E9,$EF,$E6              // vpxor xmm4,xmm2,xmm6                    //
db $C4,$41,$39,$DB,$DE           // vpand xmm11,xmm8,xmm14                  //
db $C4,$41,$39,$EF,$E6           // vpxor xmm12,xmm8,xmm14                  //
db $C5,$61,$EB,$FD              // vpor xmm15,xmm3,xmm5                    //
db $C4,$41,$41,$EB,$E9           // vpor xmm13,xmm7,xmm9                    //
db $C4,$41,$21,$EB,$F7           // vpor xmm14,xmm11,xmm15                  //
db $C5,$99,$EF,$D4              // vpxor xmm2,xmm12,xmm4                   //
db $C4,$C1,$09,$EB,$E5           // vpor xmm4,xmm14,xmm13                   //
db $C5,$F9,$DB,$C2              // vpand xmm0,xmm0,xmm2                    //
db $C5,$E9,$DB,$D9              // vpand xmm3,xmm2,xmm1                    //
db $C5,$F9,$EB,$C3              // vpor xmm0,xmm0,xmm3                     //
db $C5,$D9,$DF,$C0              // vpandn xmm0,xmm4,xmm0                   //
end;


function Generate_PtoQ_x64_AVX(A,B,C,D: Int64):Int64;
// A  RCX
// B  RDX
// C  R8
// D  R9
const
  input: Tinput = (main:(0,0,0,8,8,8,0,0);
                   se:  (0,0,0,0,0,0,0,0);
                   s:   (0,0,0,0,0,0,0,0);
                   e:   (0,0,0,0,0,0,0,0));
asm
  movdqu xmm0,[input.main]
  movdqu xmm1,[input.se]
  movdqu xmm2,[input.s]
  movdqu xmm3,[input.e]
//    ;extract 8 bitplanes with the neighborhood cells
  db $C5,$B9,$73,$F8,$02            // vpslldq xmm8,xmm0,2        //  C
  db $C5,$B1,$73,$D8,$02            // vpsrldq xmm9,xmm0,2        //  C'
  db $C5,$A9,$71,$F0,$01            // vpsllw xmm10,xmm0,1        //  A
  db $C5,$A1,$71,$D0,$01            // vpsrlw xmm11,xmm0,1        //  A'
  db $C4,$C1,$19,$71,$F0,$01        // vpsllw xmm12,xmm8,1        //  B
  db $C4,$C1,$11,$71,$F1,$01        // vpsllw xmm13,xmm9,1        //  B'
  db $C4,$C1,$09,$71,$D0,$01        // vpsrlw xmm14,xmm8,1        //  D
  db $C4,$C1,$01,$71,$D1,$01        // vpsrlw xmm15,xmm9,1        //  D'
 //    ;First get all the counts
  db $C4,$C1,$39,$EF,$C9            // vpxor xmm1,xmm8,xmm9       //  1's count of c
  db $C4,$C1,$39,$DB,$D1            // vpand xmm2,xmm8,xmm9       //  2's count of c
  db $C4,$C1,$29,$EF,$DB            // vpxor xmm3,xmm10,xmm11     //  1's count of a
  db $C4,$C1,$29,$DB,$E3            // vpand xmm4,xmm10,xmm11     //  2's count of a
  db $C4,$C1,$19,$EF,$ED            // vpxor xmm5,xmm12,xmm13     //  1's count of b
  db $C4,$C1,$19,$DB,$F5            // vpand xmm6,xmm12,xmm13     //  2's count of b
  db $C4,$C1,$09,$EF,$FF            // vpxor xmm7,xmm14,xmm15     //  1's count of d
  db $C4,$41,$09,$DB,$C7            // vpand xmm8,xmm14,xmm15     //  2's count of d
 //    ;Now add the 1's together
  db $C5,$71,$DB,$D3                // vpand xmm10,xmm1,xmm3      //  2's count of CA
  db $C5,$F1,$EF,$CB                // vpxor xmm1,xmm1,xmm3       //  combined ones of CA
  db $C5,$51,$DB,$E7                // vpand xmm12,xmm5,xmm7      //  2's count of BD
  db $C5,$D1,$EF,$EF                // vpxor xmm5,xmm5,xmm7       //  combined ones of BD
  db $C5,$71,$DB,$F5                // vpand xmm14,xmm1,xmm5      //  2's count of CABD
  db $C5,$F1,$EF,$CD                // vpxor xmm1,xmm1,xmm5       //  final count of the 1's
 //   ;now we need to add all the 2's together.
  db $C5,$E9,$DB,$DC                // vpand xmm3,xmm2,xmm4       //  4's count of ca
  db $C5,$E9,$EF,$D4                // vpxor xmm2,xmm2,xmm4       //  2's count of ca
  db $C4,$C1,$49,$DB,$E8            // vpand xmm5,xmm6,xmm8       //  4's count of bd
  db $C4,$C1,$49,$EF,$F0            // vpxor xmm6,xmm6,xmm8       //  2's count of bd
  db $C4,$C1,$29,$DB,$FC            // vpand xmm7,xmm10,xmm12     //  4's count of CABD
  db $C4,$41,$29,$EF,$C4            // vpxor xmm8,xmm10,xmm12     //  2's count of CABD
  db $C5,$69,$DB,$CE                // vpand xmm9,xmm2,xmm6       //  4's count of cabd
  db $C5,$E9,$EF,$E6                // vpxor xmm4,xmm2,xmm6       //  2's count of cabd
  db $C4,$41,$39,$DB,$DE            // vpand xmm11,xmm8,xmm14     //  4's count of CABD+abcd
  db $C4,$41,$39,$EF,$E6            // vpxor xmm12,xmm8,xmm14     //  2's count of CABD+abcd
 //   ;add all 4's
  db $C5,$61,$EB,$FD                // vpor xmm15,xmm3,xmm5       //
  db $C4,$41,$41,$EB,$E9            // vpor xmm13,xmm7,xmm9       //
  db $C4,$41,$21,$EB,$F7            // vpor xmm14,xmm11,xmm15     //
 //    ;add the 2's
  db $C5,$99,$EF,$D4                // vpxor xmm2,xmm12,xmm4      //
 //    ;final add
  db $C4,$C1,$09,$EB,$E5            // vpor xmm4,xmm14,xmm13      //
 //     ;now we have all the counts.
 //   ;register usage:
 //   ;xmm0 - original pattern
 //   ;xmm1 - count of the ones
 //   ;xmm2 - count of the twos
 //   ;xmm4 - count of the fours
  db $C5,$F9,$DB,$C2                // vpand xmm0,xmm0,xmm2       //  anything with a 2 stays the same
  db $C5,$E9,$DB,$D9                // vpand xmm3,xmm2,xmm1       //  anything with a 3 is alive
  db $C5,$F9,$EB,$C3                // vpor xmm0,xmm0,xmm3        //  add the alive cells
  db $C5,$D9,$DF,$C0                // vpandn xmm0,xmm4,xmm0      //  subtract the cells with 4 or more neighbors
end;


//Layout of a cell
//Single cell: 8 wide x 8 bits high, fits into a single XMM register
//Layed out like so
//1:   FEDCBA98   line 7
//2:   76543210   line 6
// etc.
//The cells need to be shuffled around so that every byte in the
//register gets the neighboring cells in.
//To this end the bordering 3 cells need to be recruited as well
//They are staggered like so:
//
//    A A A A B B B B B B B B
//         +------------------
//    A A A|A B B B B B B B B
//         | +---------------
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
function Generate_PtoQ_x64_Test(A,B,C,D: Int64):Int64;
const
  LastPass = 1;
asm
  push rbx
  push rdi
  push rsi
  push r12
  push r13
  push r14
  push r15
  push rbp
  mov rax,rcx
  mov rbx,rdx
  mov rcx,r8
  mov rdx,r9  //regs are also abcd.
  //get the masks for the regs.
  //A gets shifted
  mov r9d,        $0000FFFF  //The bottom 2 rows of    B
  mov r13,$0303030303030303  //The rightmost 2 rows of C
  //D gets shifted.
  //Apply the masks
  and rbx,r9  //apply the mask for B
  and rcx,r13 //and for C
  mov r15,$1111111111111111  //Get the counting mask
  //Apply the mask to D
  xor esi,esi //Mark this as the first pass
@countneighbors:      //count C+D
  ///////////Count the first 3 columns.
  //mov r8, rdx
  mov r9,r15   //Count the first 3 columns
  and r9,rdx   //r9= bitcount in column0+4
  mov r10,r15
  shr rdx,1
  and r10,rdx  //r10= bitcount in column 1+5
  mov r11,r15
  shr rdx,1
  and r11,rdx  //r11= bitcount in column 2+6
  //Add all the bitcounts together
  lea r8,[r9+r10]
  add r8,r11   //r9 = neighborhood counts for columns 1 + 5
  movq xmm0,r8  //Store the neighborhood counts
  movq xmm8,r10  //We need to store the live cell counts in column 1+5 for later use
  movq xmm9,r11  //same for column 2 + 6
  //////////////Count the next 3 columns
  //Do the same for the neighborhood counts in column 2 + 6
  sub r8,r9     //remove column 0+4 from the count
  mov r9,r15
  shr rdx,1
  and r9,rdx
  add r8,r9     //and add column 3+7 to the count

  movq xmm1,r8  //store the count
  movq xmm10,r9 //Store live cell count in column 3 + 7
  ///////////////Count the next 3 columns
  //And the same for column 3 + 7
  sub r8,r10    //remove column 1+5 from the count

  mov rdi,r15   //Now we need to shift in the cells from the left.
  shr rdx,1     //rdx 4th shift.
  mov r14,($F0F0F0F0F0F0F0F0 shr 4)
  and rdx,r14
  shl rcx,4     //C is already masked
  add rdx,rcx   //Include C in the new D
  and rdi,rdx
  add r8,rdi
  movq xmm2, r8 //Store the count
  movq xmm11,rdi //and store the live cell count for columns 4 + 8
  ////////////Count the final columns
  sub r8,r11    //remove column 2 + 6 from the count
  //Get columns 5 and 9 into the count
  shr rdx,1     //get columns 5 and 9 into the count
  and rdx,r15
  add r8,rdx
  movq xmm3,r8   //All done, save the result
  //Now we're all done with the neighborhood counts in C and D
  //We just need to do A and B

  or esi,esi   //Goto 2nd pass or to done.
  jnz @donecounting

  movdqa xmm4,xmm0 //Let's swap things around and repeat
  movdqa xmm5,xmm1
  movdqa xmm6,xmm2
  movdqa xmm7,xmm3
  movdqa xmm12,xmm8
  movdqa xmm13,xmm9
  movdqa xmm14,xmm10
  movdqa xmm15,xmm11
  mov rcx,rax
  mov rdx,rbx
  not esi             //Mark this as the second (and last pass).
  jmp @countneighbors
@donecounting:

  //xchg rax,rcx   //put A,B,C,D back the way they were.
  //We still need them to subtract the live cells themselves.
  //Right now the neighborhoodcounts include the live cells.
  //xchg rbx,rdx
  //The following regs contain the following data at this point
  //xmm0..xmm3   - neighborhood counts from A+B
  //xmm4..xmm7   - neighborhood counts from C+D
  //xmm8..xmm11  - live cells from A + B
  //xmm12..xmm15 - live cells from C + D
  //rax..rdx     - the original data from A,B,C,D //we don't need this at this point
  {TODO -oJB -cclean up : simplify loop}
  ///put all the stuff saved in Xmmregs in normal regs and run the first pass.
  ///  after the pass see which pass we are in, set up the regs accordingly
  ///  and run the pass again
  ///  until done
  ///  rax,rbx,rcx and rdx are available
  xor esi,esi  //Start at the end and work our way forwards for 4*2 columns
  movq rax,xmm11 //rax =  live cells in Bx
  movq rdx,xmm15 //rdx = live cells in Dx
  movq r9,xmm3   //r9 = neighborhood counts from Bx
  movq r8,xmm7   //r8 = neighborhood counts from Dx
  xor rdi,rdi  //rdi will contain the new block of cells
  mov r13,$3333333333333333   //mask for testing liveness
  {in use: R15, R13 (masks)
   rax,rbx,rdx,r8,r9    data of columns X
   rcx,r11,r12,r14,rbp  data of columns Y
   rsi,rdi              counter and result
   r10, rsp -> temp data
   free: rcx,r11,r12,r14,rbp}
 //process 2 sets of columns in parallel
  movq rcx,xmm10    //live cells in By
  movq r11,xmm14    //live Cells in Dy
  movq r12,xmm2     //neighborhood counts in By
  movq r14,xmm6     //neighborhood counts in Dy
  movq xmm15,rsp  //Free up rsp as a general register
@CalculateNeighborhoodCounts:
  //We get the real neighborhood counts by adding 3 rows together and
  //subtracting the centre cell.
  //Lets get the counts for column 1+5 (2+6, 3+7, 4+8)
  mov rbx,r9      //rbx = neighborhood counts from Bx
  mov rbp,r12     //rbp = neighborhood counts from By
  shl rdi,2       //Make space for the next 2 sets of columns
  mov r10,r8      //neighborhood counts in Dx
  mov rsp,r14     //neighborhood counts in Dy
  shr r10,8       //remove the bottom row of D
  shr rsp,8
  shl r9,(64-8)   //move the bottom row in Bx to the top
  shl r12,(64-8)  //move the bottom row in By to the top
  add r9,r10      //combine the two neighborhood counts
  add r12,rsp     //combine the two neighborhood counts
  //We now have the middle slice of counts in R9/R12
  shr r10,8       //remove another bottom row of Dx
  shr rsp,8       //remove another bottom row of Dy
  shl rbx,(64-16)  //move the bottom 2 rows of Bx to the top
  shl rbp,(64-16)  //move the bottom 2 rows of By to the top
  add r10,rbx   //and the top slice is in R10
  add rsp,rbp   //and the top slice is in R10
  add r8,r10     //Add everything together in Dx
  add r14,rsp    //Add everything together in Dy
  add r8,r9     //Now we have the true cell counts in Dx
  add r14,r12   //Now we have the true cell counts in Dy
  //We just need to subtract the centre cell.
  //The centre cells are (well.. ) exactly in the centre, so we need to shift out the
  //bottom row from D and shift in the top row from B

  ////The register usage is now as follows:
  ///  R8 <- counts of Dx
  ///  R14 <- counts of Dy
  ///  rax = live cells in Bx
  ///  rdx = live cells in Dx
  ///  rcx = live cells in By
  ///  r11 = live Cells in Dy
  ///  R15,R13 = masks

  //Let a new block be born
  ///////////////////////////////////////
  ///  Code to generate the new cell.
  ///////////////////////////////////////

  shr rdx,8      //remove the bottom row from Dx
  shr r11,8      //remove the bottom row from Dy
  shl rax,(64-8) //move bottom row in Bx to the top
  shl rcx,(64-8) //move bottom row in By to the top
  or rdx,rax     //combine the two.
  or r11,rcx     //combine the two.
  //////////////////////////////////////////////////////
  ///  The following code is specific to a rule.
  ///  This code is tuned to 23/3
  ///  It needs to be patched for a different rule
  //////////////////////////////////////////////////////
  //// The register usage is now as follows:
  ///  R8 <- counts of Dx
  ///  R14 <- counts of Dy
  ///  rdx = live cells
  ///  r11 = live Cells
  ///  R15,R13 = masks
  //Compare the counts (including live cells) with 3.
@ProcessAColumn:
  //Truthtable for cell counts
//1.  Full cellcount                   0	 1	 2	3	  4	  5	  6	  7	   8     9
//2.  Full celcount xor 3	            11	10	 1	0	111	110	101	100	1011	1010
//3.  Full Celcount and not life cell	10	10	 0	0	110	110	100	100	1010	1010
//4.  Full celcount and not dead cell 11	10	 1	0	111	110	101	100	1011	1010 //no-op from 2.
//Note the exact match with 23/3

  mov rax,r8  //counts in Dx
  mov rbx,r14 //counts in Dy
  sub rax,rdx //remove the live cells from Dx counts
  sub rbx,r11 //remove the live cells from Dy counts
  xor rax,r13 //test for live on 23/3
  xor rbx,r13 //test for liveness
  not rax     //reverse the pattern, 2=1110, 3=1111, everything else have 0's in the 'wrong' places.
  not rbx
  or  rax,rdx //fill in the zero for 2 iff cell is live.
  or  rbx,r11 //add live cell
  //All cells that are live will have all 1's in their nibble, all dead cells will have a zero somewhere
  //Compress the live/dead data into a single bit
  mov rdx,rax   //All 1's is alive and a 0 somewhere is dead.
  mov r11,rbx
  shr rdx,2
  shr r11,2
  and rdx,r13
  and r11,r13
  and rax,rdx   //compress the 4 bits into 2
  and rbx,r11
  mov rdx,rax
  mov r11,rbx
  shr rdx,1
  shr r11,1
  and rdx,r15   //compress the 2 bits into 1
  and r11,r15
  and rax,rdx
  and rbx,r11
  lea rcx,[rax*2+rbx]
  //shl rax,1
  //Now a single bit will be set if the new cell is live and nothing will be set if the cell is dead.
  //or rdi,rax //dump our data in the destination
  //or rdi,rbx
  or rdi,rcx
  or esi,esi
  jnz @Done
  ///Set up the registers for the 2nd to 4th pass.
  //Test to see where we are and what data we need to start with
  //esi = 0, all done, see above
  //esi = 3 -> second pass: process columns 2+6
  //esi = 2 -> third pass: columns 3+7
  //esi = 1 -> last pass: columns 4+8
////////////////////////////////////////////////////
/// End of rule specific code
////////////////////////////////////////////////////

  movq rax,xmm9 //rax =  live cells in Bx
  movq rdx,xmm13 //rdx = live cells in Dx
  movq r9,xmm1   //r9 = neighborhood counts from Bx
  movq r8,xmm5   //r8 = neighborhood counts from Dx
  {in use: R15, R13 (masks)
   rax,rbx,rdx,r8,r9    data of columns X
   rcx,r11,r12,r14,rbp  data of columns Y
   rsi,rdi              counter and result
   r10, rsp -> temp data
   free: rcx,r11,r12,r14,rbp}
 //process 2 sets of columns in parallel
  movq rcx,xmm8    //live cells in By
  movq r11,xmm12    //live Cells in Dy
  movq r12,xmm0     //neighborhood counts in By
  movq r14,xmm4     //neighborhood counts in Dy
  not esi
  jmp @CalculateNeighborhoodCounts
@Done:
  movq rsp,xmm15  //Restore esp
  //Now we have a full cell in rdi.
  //Let's put it in RAX and exit the function
  mov rax,rdi
  //cleanup
  pop rbp
  pop r15
  pop r14
  pop r13
  pop r12
  pop rsi
  pop rdi
  pop rbx
  ret
end;


//Layout of a cell
//Single cell: 8 wide x 8 bits high, fits into a single XMM register
//Layed out like so
//1:   FEDCBA98   line 7
//2:   76543210   line 6
// etc.
//The cells need to be shuffled around so that every byte in the
//register gets the neighboring cells in.
//To this end the bordering 3 cells need to be recruited as well
//They are staggered like so:
//
//    A A A A B B B B B B B B
//         +------------------
//    A A A|A B B B B B B B B
//         | +---------------
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
function Generate_PtoQ_x64(A,B,C,D: Int64):Int64;
const
  LastPass = 1;
asm
  xchg rcx,r8
  xchg rdx,r9
  push rbx
  push rdi
  push rsi
  push r12
  push r13
  push r14
  push r15
  mov rax,r8
  mov rbx,r9 //regs are also abcd.
  //get the masks for the regs.
  //A gets shifted
  mov r9d,        $0000FFFF  //The bottom 2 rows of    B
  mov r13,$0303030303030303  //The rightmost 2 rows of C
  //D gets shifted.
  //Apply the masks
  and rbx,r9  //apply the mask for B
  and rcx,r13 //and for C
  mov r15,$1111111111111111  //Get the counting mask
  //Apply the mask to D
  xor esi,esi //Mark this as the first pass
@countneighbors:      //count C+D
  ///////////Count the first 3 columns.
  //mov r8, rdx
  mov r9,rdx  //Count the first 3 columns
  and r9,r15   //r9= bitcount in column0+4
  mov r10,rdx
  shr r10,1
  and r10,r15  //r10= bitcount in column 1+5
  mov r11,rdx
  shr r11,2
  and r11,r15  //r11= bitcount in column 2+6
  //Add all the bitcounts together
  lea r8,[r9+r10]
  add r8,r11   //r9 = neighborhood counts for columns 1 + 5
  movq xmm0,r8  //Store the neighborhood counts
  movq xmm8,r10  //We need to store the live cell counts in column 1+5 for later use
  movq xmm9,r11  //same for column 2 + 6
  //////////////Count the next 3 columns
  //Do the same for the neighborhood counts in column 2 + 6
  sub r8,r9     //remove column 0+4 from the count
  mov r9,rdx
  shr r9,3
  and r9,r15
  add r8,r9     //and add column 3+7 to the count

  movq xmm1,r8  //store the count
  movq xmm10,r9 //Store live cell count in column 3 + 7
  ///////////////Count the next 3 columns
  //And the same for column 3 + 7
  sub r8,r10    //remove column 1+5 from the count

  mov rdi,rcx   //Now we need to shift in the cells from the left.
  shl rdi,7     //leave only row 0
  mov r9,rdx
  mov r14,not($0101010101010101) //clear row0 of D
  and r9,r14
  shr r9,1      //and shift it out of the way
  or rdi,r9     //add row0 of C to row 7 of D
  shr rdi,3
  and rdi,r15    //Count Rows 4+8
  add r8,rdi
  movq xmm2, r8 //Store the count
  movq xmm11,rdi //and store the live cell count for columns 4 + 8
  ////////////Count the final columns
  sub r8,r11    //remove column 2 + 6 from the count
  //Get columns 5 and 9 into the count
  mov rdi,rcx   //get columns 5 and 9 into the count
  shl rdi,6     //Keep rightmost bit of C
  and r9,r14    //clear row 0 in D
  shr r9,1      //and move it out of the way.
  or rdi,r9
  shr rdi,3     //align with the mask
  and rdi,r15
  add r8,rdi
  movq xmm3,r8   //All done, save the result
  //Now we're all done with the neighborhood counts in C and D
  //We just need to do A and B
  //Are we al done?
  or esi,esi   //how many more passes?
  jnz @donecounting

  movdqa xmm4,xmm0 //Let's swap things around and repeat
  movdqa xmm5,xmm1
  movdqa xmm6,xmm2
  movdqa xmm7,xmm3
  movdqa xmm12,xmm8
  movdqa xmm13,xmm9
  movdqa xmm14,xmm10
  movdqa xmm15,xmm11
  xchg rax,rcx
  xchg rdx,rbx
  inc esi           //Mark this as the second (and last pass).
  jmp @countneighbors
@donecounting:

  //xchg rax,rcx   //put A,B,C,D back the way they were.
  //We still need them to subtract the live cells themselves.
  //Right now the neighborhoodcounts include the live cells.
  //xchg rbx,rdx
  //The following regs contain the following data at this point
  //xmm0..xmm3   - neighborhood counts from A+B
  //xmm4..xmm7   - neighborhood counts from C+D
  //xmm8..xmm11  - live cells from A + B
  //xmm12..xmm15 - live cells from C + D
  //rax..rdx     - the original data from A,B,C,D //we don't need this at this point
  {TODO -oJB -cclean up : simplify loop}
  ///put all the stuff saved in Xmmregs in normal regs and run the first pass.
  ///  after the pass see which pass we are in, set up the regs accordingly
  ///  and run the pass again
  ///  until done
  ///  rax,rbx,rcx and rdx are available
  mov esi,2  //Start at the end and work our way forwards for 4*2 columns
  movq rax,xmm11 //rax =  live cells in Bx
  movq rdx,xmm15 //rdx = live cells in Dx
  movq r9,xmm3   //r9 = neighborhood counts from Bx
  movq r8,xmm7   //r8 = neighborhood counts from Dx
  xor rdi,rdi  //rdi will contain the new block of cells
  mov r13,$3333333333333333   //mask for testing liveness
  {in use: R15, R13 (masks)
   rax,rbx,rdx,r8,r9    data of columns X
   rcx,r11,r12,r14,rbp  data of columns Y
   rsi,rdi              counter and result
   r10, rsp -> temp data
   free: rcx,r11,r12,r14,rbp}
 //process 2 sets of columns in parallel
  movq rcx,xmm10    //live cells in By
  movq r11,xmm14    //live Cells in Dy
  movq r12,xmm2     //neighborhood counts in By
  movq r14,xmm6     //neighborhood counts in Dy
  movq xmm15,rsp  //Free up rsp as a general register
  movq xmm14,rbp  //free up rbp as a general register
@CalculateNeighborhoodCounts:
  //We get the real neighborhood counts by adding 3 rows together and
  //subtracting the centre cell.
  //Lets get the counts for column 1+5 (2+6, 3+7, 4+8)
  mov rbx,r9      //rbx = neighborhood counts from Bx
  mov rbp,r12     //rbp = neighborhood counts from By
  shl rdi,2       //Make space for the next 2 sets of columns
  mov r10,r8      //neighborhood counts in Dx
  mov rsp,r14     //neighborhood counts in Dy
  shr r10,8       //remove the bottom row of D
  shr rsp,8
  shl r9,(64-8)   //move the bottom row in Bx to the top
  shl r12,(64-8)  //move the bottom row in By to the top
  add r9,r10      //combine the two neighborhood counts
  add r12,rsp     //combine the two neighborhood counts
  //We now have the middle slice of counts in R9/R12
  shr r10,8       //remove another bottom row of Dx
  shr rsp,8       //remove another bottom row of Dy
  shl rbx,(64-16)  //move the bottom 2 rows of Bx to the top
  shl rbp,(64-16)  //move the bottom 2 rows of By to the top
  add r10,rbx   //and the top slice is in R10
  add rsp,rbp   //and the top slice is in R10
  add r8,r10     //Add everything together in Dx
  add r14,rsp    //Add everything together in Dy
  add r8,r9     //Now we have the true cell counts in Dx
  add r14,r12   //Now we have the true cell counts in Dy
  //We just need to subtract the centre cell.
  //The centre cells are (well.. ) exactly in the centre, so we need to shift out the
  //bottom row from D and shift in the top row from B

  ////The register usage is now as follows:
  ///  R8 <- counts of Dx
  ///  R14 <- counts of Dy
  ///  rax = live cells in Bx
  ///  rdx = live cells in Dx
  ///  rcx = live cells in By
  ///  r11 = live Cells in Dy
  ///  R15,R13 = masks

  //Let a new block be born
  ///////////////////////////////////////
  ///  Code to generate the new cell.
  ///////////////////////////////////////

  shr rdx,8      //remove the bottom row from Dx
  shr r11,8      //remove the bottom row from Dy
  shl rax,(64-8) //move bottom row in Bx to the top
  shl rcx,(64-8) //move bottom row in By to the top
  or rdx,rax     //combine the two.
  or r11,rcx     //combine the two.
  //////////////////////////////////////////////////////
  ///  The following code is specific to a rule.
  ///  This code is tuned to 23/3
  ///  It needs to be patched for a different rule
  //////////////////////////////////////////////////////
  //// The register usage is now as follows:
  ///  R8 <- counts of Dx
  ///  R14 <- counts of Dy
  ///  rdx = live cells
  ///  r11 = live Cells
  ///  R15,R13 = masks
  //Compare the counts (including live cells) with 3.
@ProcessAColumn:
  //Truthtable for cell counts//1.  Full cellcount                   0	 1	 2	3	  4	  5	  6	  7	   8     9//2.  Full celcount xor 3	            11	10	 1	0	111	110	101	100	1011	1010//3.  Full Celcount and not life cell	10	10	 0	0	110	110	100	100	1010	1010//4.  Full celcount and not dead cell 11	10	 1	0	111	110	101	100	1011	1010 //no-op from 2//Note the exact match with 23/3
  mov rax,r8  //counts in Dx
  mov rbx,r14 //counts in Dy
  sub rax,rdx //remove the live cells from Dx counts
  sub rbx,r11 //remove the live cells from Dy counts
  xor rax,r13 //test for live on 23/3
  xor rbx,r13 //test for liveness
  not rax     //reverse the pattern, 2=1110, 3=1111, everything else have 0's in the 'wrong' places.
  not rbx
  or  rax,rdx //fill in the zero for 2 iff cell is live.
  or  rbx,r11 //add live cell
  //All cells that are live will have all 1's in their nibble, all dead cells will have a zero somewhere
  //Compress the live/dead data into a single bit
  mov rdx,rax   //All 1's is alive and a 0 somewhere is dead.
  mov r11,rbx
  shr rdx,2
  shr r11,2
  and rdx,r13
  and r11,r13
  and rax,rdx   //compress the 4 bits into 2
  and rbx,r11
  mov rdx,rax
  mov r11,rbx
  shr rdx,1
  shr r11,1
  and rdx,r15   //compress the 2 bits into 1
  and r11,r15
  and rax,rdx
  and rbx,r11
  lea rcx,[rax*2+rbx]
  //shl rax,1
  //Now a single bit will be set if the new cell is live and nothing will be set if the cell is dead.
  //or rdi,rax //dump our data in the destination
  //or rdi,rbx
  or rdi,rcx
  sub esi,1
  jz @Done
  ///Set up the registers for the 2nd to 4th pass.
  //Test to see where we are and what data we need to start with
  //esi = 0, all done, see above
  //esi = 3 -> second pass: process columns 2+6
  //esi = 2 -> third pass: columns 3+7
  //esi = 1 -> last pass: columns 4+8
////////////////////////////////////////////////////
/// End of rule specific code
////////////////////////////////////////////////////

  movq rax,xmm9 //rax =  live cells in Bx
  movq rdx,xmm13 //rdx = live cells in Dx
  movq r9,xmm1   //r9 = neighborhood counts from Bx
  movq r8,xmm5   //r8 = neighborhood counts from Dx
  {in use: R15, R13 (masks)
   rax,rbx,rdx,r8,r9    data of columns X
   rcx,r11,r12,r14,rbp  data of columns Y
   rsi,rdi              counter and result
   r10, rsp -> temp data
   free: rcx,r11,r12,r14,rbp}
 //process 2 sets of columns in parallel
  movq rcx,xmm8    //live cells in By
  movq r11,xmm12    //live Cells in Dy
  movq r12,xmm0     //neighborhood counts in By
  movq r14,xmm4     //neighborhood counts in Dy
  jmp @CalculateNeighborhoodCounts
@Done:
  movq rsp,xmm15  //Restore esp
  movq rbp,xmm14  //restore rbp
  //Now we have a full cell in rdi.
  //Let's put it in RAX and exit the function
  mov rax,rdi
  //cleanup
  pop r15
  pop r14
  pop r13
  pop r12
  pop rsi
  pop rdi
  pop rbx
  ret
end;


//Layout of a cell
//Single cell: 8 wide x 8 bits high, fits into a single XMM register
//Layed out like so
//1:   FEDCBA9876543210   line 7
//2:   FEDCBA9876543210   line 6
// etc.
//The cells need to be shuffled around so that every byte in the
//register gets the neighboring cells in.
//To this end the bordering 3 cells need to be recruited as well
//They are staggered like so:
//
//    A A A A B B B B B B B B
//         +------------------
//    A A A|A B B B B B B B B
//         | +---------------
//    C C C|C|D D D D D D D D
//         | |
//    C C C|C|D D D D D D D D
//         | |
// Counting works as follows:
// A mask 00010001 is applied to every (row).
// This gives use the 2 bitcounts for that row
// Then we shift the bitfield, apply the mask
// and add the two results together.
// One more shift, mask and add and we have all the neighbors
// in the that we're interested in.

type
  PInt128 = ^Int128;
  Int128 = record
    a: Int64;
    b: Int64;
  end;

procedure TForm34.ShowCell(Canvas: TCanvas; Data: int64);
type
  TBlock = array[0..7] of byte;
var
  bmpinfo: PBitmapInfo;
  color: PRGBQUAD;
  i: Integer;
  y,h: integer;
  data_buffer: array[0..7] of integer;
  ScanlineWidth: integer;
begin
  GetMem(bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD)*2);
  //GetMem(data_buffer, SizeOf(Int64));
  color:= @bmpinfo^.bmiColors[0];
  color^.rgbRed:= 255;
  color^.rgbBlue:= 255;
  color^.rgbGreen:= 255;
  color^.rgbReserved:= 0;
  Inc(color);
  color^.rgbRed:= 0;
  color^.rgbBlue:= 0;
  color^.rgbGreen:= 0;
  color^.rgbReserved:= 0;

  with bmpinfo.bmiHeader do begin
    biSize:= SizeOf(bmpinfo.bmiHeader);
    biWidth:= 8;
    biHeight:= 8;
    biPlanes:= 1;
    biBitCount:= 1;
    biCompression:= BI_RGB;
    biSizeImage:= 0;
    biXPelsPerMeter:= 0;
    biYPelsPerMeter:= 0;
    biClrUsed:= 0;
    biClrImportant:= 0;
  end;


  //GetMem(data_buffer, ScanlineWidth * height);
//  function SetDIBitsToDevice(DC: HDC; DestX, DestY: Integer; Width, Height: DWORD;
//  SrcX, SrcY: Integer; nStartScan, NumScans: UINT; Bits: Pointer;
//  var BitsInfo: TBitmapInfo; Usage: UINT): Integer; stdcall;

  ScanlineWidth:= 8 div 8;
  if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);

  for i := 0 to 7 do begin
    data_buffer[i]:= TBlock(Data)[i];
  end;
  //data_buffer^:= 0;
  //SetDIBitstoDevice(Canvas.Handle, 0, 0, 8, 8, 0, 0, 0, 8, data_buffer, bmpinfo^, DIB_RGB_COLORS);
  //Banding
  h:= height div 4;
  y:= 0;


  StretchDIBits(Canvas.Handle, 0,0,80,80,0,0,8,8,@data_buffer, bmpinfo^, DIB_RGB_COLORS, SRCCOPY);

//  SetDIBitstoDevice(Canvas.Handle, 0, y, width, h,
//                    0, y, y, h, data_buffer,
//                    bmpinfo^, DIB_RGB_COLORS);
//  y:= y + h;
//  SetDIBitstoDevice(Canvas.Handle, 0, y, width, h,
//                    0, y, y, h, data_buffer,
//                    bmpinfo^, DIB_RGB_COLORS);
//  y:= y + h;
//  SetDIBitstoDevice(Canvas.Handle, 0, y, width, h,
//                    0, y, y, h, data_buffer,
//                    bmpinfo^, DIB_RGB_COLORS);
//  y:= y + h;
//  SetDIBitstoDevice(Canvas.Handle, 0, y, width, h,
//                    0, y, y, h, data_buffer,
//                    bmpinfo^, DIB_RGB_COLORS);
  //FreeMem(data_buffer);
  FreeMem(bmpinfo);
end;


function GetMask(x, y: integer): Int64;
begin
  Result:= 1;
  Result:= Result shl x;
  Result:= Result shl (y * 8);
end;

function GetBasePtoQ(A,B,C,D: int64; var x, y: integer): Int64;
begin
  case x of
    0..7: begin
      if y in [0..7] then begin
        Result:= D;
      end else begin
        Result:= B;
        Dec(y,8);
      end;
    end;
    8,9: begin
      if y in [0..7] then begin
        Result:= C;
        Dec(x,8);
      end else begin
        Result:= A;
        Dec(y,8); Dec(x,8);
      end;
    end;
  end;
end;

function GetBaseQtoP(A,B,C,D: int64; var x, y: integer): Int64;
begin
  case x of
    2..9: begin
      if y in [2..9] then begin
        Result:= D;
        Dec(x,2); Dec(y,2);
      end else begin
        Result:= B;
        Dec(x,2);
      end;
    end;
    0..1: begin
      if y in [0..1] then begin
        Result:= C;
      end else begin
        Result:= A;
        Dec(y,2);
      end;
    end;
  end;
end;

///  Cells are stacked like so:
///  DDDDDDDDDCC
///  DDDDDDDDDCC
///  DDDDDDDDDCC
///  BBBBBBBBBAA
///  BBBBBBBBBAA


function ShiftPtoQ(a,b,c,d: int64): int64;
///  A = RCX
///  B = RDX
///  C = R8
///  D = R9
asm
  mov r10,$7F7F7F7F7F7F7F7F     //strip off the high order bits
  and r9,r10                    //off D
  and rdx,r10                   //and off B
  shl r9,1+8                    //make space to add C to D
  shr rdx,7*8-1                 //move top top row to the bottom
  not r10
  and r8,r10                    //keep only the left most row of C
  and rcx,r10
  shl r8,8-1                    //Keep the leftmost column of C
  shr rcx,7*8+7                 //keep only the msb of A
  or rdx,r9                     //combine all four
  or rcx,r8
  lea rax,[rcx+rdx]
end;

///  Cells are stacked like so:
///    A A A A B B B B B B B B
///         +------------------
///    A A A|A B B B B B B B B
///         | +---------------
///    C C C|C|D D D D D D D D
///         | |
///    C C C|C|D D D D D D D D


function ShiftQtoP(a,b,c,d: int64): int64;
///  A = RCX
///  B = RDX
///  C = R8
///  D = R9
asm
  mov r10,$FEFEFEFEFEFEFEFE     //strip off the low order bits
  and r9,r10                    //off D
  and rdx,r10                   //and off B
  shr r9,1+8                    //make space to add C to D
  shl rdx,7*8-1                 //move top top row to the bottom
  not r10
  and r8,r10                    //keep only the left most row of C
  and rcx,r10
  shr r8,8-1                    //Keep the leftmost column of C
  shl rcx,7*8+7                 //keep only the msb of A
  or rdx,r9                     //combine all four
  or rcx,r9
  lea rax,[rcx+rdx]
end;

function Difference(a,b: int64): int64;
asm
  xor rcx,rdx
  mov rax,rcx
end;

function CellCount(a: int64): integer;
asm
  popcnt rax,rcx
end;

type
  TQuadrant = (NW {0}, NE {1}, SE {2}, SW{3});

function HeavyQuadrant(a: int64): TQuadrant;
asm
  mov rdx,rcx
  mov r8,rcx
  mov r9,rcx
  mov rax,$F0F0F0F000000000;    //test NW
  and rcx,rax
  shr rax,4                     //test NE
  and rdx,rax
  shr rax, 32-4                 //test SW
  and r8,rax
  shr rax,4                     // test SE
  and r9,rax
  popcnt rcx,rcx                //count NW
  popcnt rdx,rdx                //count NE
  popcnt r8,r8                  //count SW
  popcnt r9,r9                  //count SE
  xor rax,rax                   //assume NW is heaviest
  cmp rcx,rdx                   //CF =1 if NE is heavier
  adc rax,0
  cmovc rcx,rdx                 //make rcx the heaviest of the two
  cmp r8,r9
  cmovc r8,r9                   //r8 is the bigger of the two
  sbb r9,r9                     //r9 =0 if r8>r9; r9=-1 if r9>r8
  lea r9,[r9+3]                 //r9 =3 if r8>r9; r9= 2 if r9>r8
  cmp rcx,r8
  cmovc rax,r9
end;


function TestGeneratePtoQ(A,B,C,D: int64): int64;

  function GetCell(x,y: integer): boolean;
  var
    Mask: Int64;
    Basis: Int64;
  begin
    Basis:= GetBasePtoQ(A,B,C,D,x,y);
    Mask:= GetMask(x,y);
    Result:= (Basis and Mask) <> 0;
  end;

  procedure SetCell(x,y: integer; Cell: boolean);
  var
    Mask: Int64;
  begin
    Mask:= GetMask(x,y);
    if Cell then begin
      Result:= Result or Mask;
    end else begin
      Result:= result and not Mask;
    end;
  end;
var
  x: Integer;
  y: Integer;
  x1,y1: integer;
  Count: integer;
  Cell: boolean;
begin
  Result:= 0;
  for x := 1 to 8 do begin
    for y := 1 to 8 do begin
      Count:= 0;
      Cell:= GetCell(x,y);  //The cell itself
      for x1:= -1 to 1 do begin
        for y1:= -1 to 1 do begin
          //Neighborhood count incl the cell
          if GetCell(x+x1,y+y1) then Inc(Count);
        end; {for y1}
      end; {for x1}
      case Count of
        3: SetCell(x-1,y-1,true);
        4: if Cell then SetCell(x-1,y-1,true);
      end; {case}
    end; {for y}
  end; {for x}
end;

//    D D D D D D D D D D|C|C
//                       | |
//    D D D D D D D D D D|C|C
//    -------------------+ |
//    B B B B B B B B B B A|A
//    ---------------------+
//    B B B B B B B B B B A A

function TestGenerateQtoP(A,B,C,D: int64): int64;

  function GetCell(x,y: integer): boolean;
  var
    Mask: Int64;
    Basis: Int64;
  begin
    Basis:= GetBaseQtoP(A,B,C,D,x,y);
    Mask:= GetMask(x,y);
    Result:= (Basis and Mask) <> 0;
  end;

  procedure SetCell(x,y: integer; Cell: boolean);
  var
    Mask: Int64;
  begin
    Mask:= GetMask(x,y);
    if Cell then begin
      Result:= Result or Mask;
    end else begin
      Result:= result and not Mask;
    end;
  end;
var
  x: Integer;
  y: Integer;
  x1,y1: integer;
  Count: integer;
  Cell: boolean;
begin
  Result:= 0;
  for x := 1 to 8 do begin
    for y := 1 to 8 do begin
      Count:= 0;
      Cell:= GetCell(x,y);  //The cell itself
      for x1:= -1 to 1 do begin
        for y1:= -1 to 1 do begin
          //Neighborhood count incl the cell
          if GetCell(x+x1,y+y1) then begin
            Inc(Count);
          end;
        end; {for y1}
      end; {for x1}
      case Count of
        3: SetCell(x-1,y-1,true);
        4: if Cell then SetCell(x-1,y-1,true);
      end; {case}
    end; {for y}
  end; {for x}
end;


procedure TForm34.Button3Click(Sender: TObject);
var
  a,b,c,d: int64;
  d1,d2: int64;
  i: integer;
  SleepTime: integer;
begin
  SleepTime:= 10;
  a:= 0; b:= 0; c:= 0;
  b:=  $0000000;
  //d:= $00E00;
  d:= $E00000000000;
  ShowCell(Form34.Canvas, d);
  for i := 0 to 7 do begin
    //d1:= Unit34.Generate_PtoQ_x64_Test(a,b,c,d);
    d1:= TestGeneratePtoQ(a,b,c,d);
    d2:= Generate_PtoQ_x64_Test2(a,b,c,d);
    Assert(d1 = d2,'d1 = '+IntToHex(d1,16)+#10#13+'d2 = '+IntToHex(d2,16));
    d:= d2;
    sleep(SleepTime);
    ShowCell(Form34.Canvas, d);
  end;
  a:= 0; b:=  0; c:= 0;
  d:= $0020107;
  ShowCell(Form34.Canvas, d);
  for i := 0 to 7 do begin
    //d1:= Unit34.Generate_PtoQ_x64_Test(a,b,c,d);
    d1:= TestGeneratePtoQ(a,b,c,d);
    ShowCell(Form34.Canvas, d1);
    sleep(SleepTime);
    d2:= Generate_PtoQ_x64_Test2(a,b,c,d);
    ShowCell(Form34.Canvas, d2);
    Assert(d1 = d2,'d1 = '+IntToHex(d1,16)+#10#13+'d2 = '+IntToHex(d2,16));
    d:= d2;


  end;

  c:= $0000010101010000;
  d:= $0000000000000000;
  ShowCell(Form34.Canvas, d);
  for i := 0 to 7 do begin
    //d1:= Unit34.Generate_PtoQ_x64_Test(a,b,c,d);
    d1:= TestGeneratePtoQ(a,b,c,d);
    d2:= Generate_PtoQ_x64_Test2(a,b,c,d);
    Assert(d1 = d2,'d1 = '+IntToHex(d1,16)+#10#13+'d2 = '+IntToHex(d2,16));
    d:= d2;
    sleep(SleepTime);
    ShowCell(Form34.Canvas, d);
  end;
  b:=  $00; c:= 0;
  d:= $00000000FF000000;
  ShowCell(Form34.Canvas, d);
  for i := 0 to 7 do begin
    //d1:= Unit34.Generate_PtoQ_x64_Test(a,b,c,d);
    d1:= TestGeneratePtoQ(a,b,c,d);
    d2:= Generate_PtoQ_x64_Test2(a,b,c,d);
    Assert(d1 = d2,'d1 = '+IntToHex(d1,16)+#10#13+'d2 = '+IntToHex(d2,16));
    d:= d2;
    sleep(200);
    ShowCell(Form34.Canvas, d);
  end;
  a:=  -1; b:=-1; c:= -1;
  d:= $0000000000000000;
  ShowCell(Form34.Canvas, d);
  Sleep(SleepTime);
  for i := 0 to 7 do begin
    //d1:= Unit34.Generate_PtoQ_x64_Test(a,b,c,d);
    d1:= TestGeneratePtoQ(a,b,c,d);
    ShowCell(Form34.Canvas, d1);
    sleep(SleepTime);
    d2:= Generate_PtoQ_x64_Test2(a,b,c,d);
    ShowCell(Form34.Canvas, d2);
    Assert(d1 = d2,'d1 = '+IntToHex(d1,16)+#10#13+'d2 = '+IntToHex(d2,16));
    d:= d2;
    ShowCell(Form34.Canvas, d);
  end;
end;

procedure TForm34.Test;
begin
  Generate_PtoQ_x64_Test(0, 0, 0, 100);
end;




procedure TForm34.Button4Click(Sender: TObject);
const
  Repeats = 1000000;
var
  Timer: THiResStopWatch;
  i,a: integer;
  d: int64;
  ms, ticks: integer;
  ticksOld: integer;
  f: string;
begin
  Memo1.Lines.Clear;
  ms:= MaxInt;
  Ticks:= MaxInt;
  //d:= $0070102000000000;
    d:= $FFFFFFFFFFFFFFFF;
  for a:= 1 to 50 do begin
    Timer:= THiresStopwatch.StartNew;
    for i:= 1 to (repeats) do begin
      d:= not(Generate_PtoQ_x64_AVX(0, 0, 0, d));
      //d:= not(Generate_PtoQ_x64_AVX(0,0,0,d));
      //d:= not(TestGenerate(0,0,0,d));  500x faster
    end;
    Timer.Stop;
    ms:= Min(ms, Timer.ElapsedMilliseconds);
    Ticks:= Min(Ticks, (Timer.GetElapsedTicks div (Repeats)));
  end;
  Memo1.Lines.add(Format('xmm code: %d ms, %d ticks ', [ms, ticks]));
  TicksOld:= Ticks;
  ms:= MaxInt;
  Ticks:= MaxInt;
  //d:= $0070102000000000;
  d:= $FFFFFFFFFFFFFFFF;
  for a:= 1 to 50 do begin
    Timer:= THiResStopwatch.StartNew;
    for i:= 1 to repeats do begin
      //d:= not(Generate_QtoP_x64_Test2(0, 0, 0, d));
      //d:= not(Generate_PtoQ_Full_AVX(0,0,0,d));
    end;
    Timer.Stop;
    ms:= Min(ms, Timer.ElapsedMilliseconds);
    Ticks:= Min(Ticks, Timer.GetElapsedTicks div Repeats);
  end;
  Memo1.Lines.add(Format('x64 code: %d ms, %d ticks ', [ms, Ticks]));
  if TicksOld < Ticks then f:= 'slower' else f:= 'faster';

  Memo1.Lines.add(Format('x64 is: %d%% %s', [((TicksOld - Ticks) * 100) div (Ticks),f]));
end;




{ TUnit }

procedure TUnit.Display(const BitmapStart: pointer; const LineOffset: integer);
asm
  ///  rcx = self
  ///  rdx = bitmapstart
  ///  r8  = LineOffset
  mov rax,[rcx]    //get the first 4 lines.
  mov r9,[rcx+64]
  mov [rdx],ax     //store the first line
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  lea rdx,[rdx+r8*2]
  mov [rdx],r9w
  shr r9,16
  mov [rdx+r8],r9w
  shr r9,16
  lea rdx,[rdx+r8*2]
  mov [rdx],r9w
  shr r9,16
  mov [rdx+r8],r9w
end;

{ TBitmap }

function TBitmap.Next(index: integer): integer;
asm
  ///  rcx = self
  ///  rdx = index
  ///  A bitmap is 128 bits.
  cmp edx,63              //are we in the first or second qword?
  cmovl rax,[rcx]         //first qword
  cmovge rax,[rcx+8]      //second qword
  lea rcx,[rdx+1]         //shift 1 if index = 0, no shift if index = -1 or 63
  shr rax,cl              //shift bits out if count <> 0 or 64
  rep bsf rax,rax         //tzcnt: Count the number of bits to be skipped.
  add eax,ecx             //Return the old index (ecx)+1+zero bit count
end;

procedure TBitmap.TurnOff(index: integer);
asm
  //rcx = self
  //rdx = index
  cmp edx,63
  lea rax,[rcx+8]
  cmovl rax,rcx
  mov ecx,edx
  mov rdx,-2
  rol rdx,cl
  and [rax],rdx
end;

procedure TBitmap.TurnOn(index: integer);
asm
  //rcx = self
  //rdx = index
  cmp edx,63
  lea rax,[rcx+8]
  cmovl rax,rcx
  mov ecx,edx
  mov edx,1
  rol rdx,cl
  or [rax],rdx
end;


procedure TBitmap.ProcessNW(status: PBorderStatus);
asm
  //rcx = self
  //rdx = PStatus
  //  ;bit    7----6----5----4----3----2----1----0
  //  ;       +    active      |  alive          +
  //  ;       NW   W    N   all   NW   W    N   all
  mov r8,[rdx]                //r8 = StatusN
  mov rax,r8                  //rax = statusNW
  shl r8,2                    //move N status bits to the MSB
  mov r11,$8080808080808080   //Mask out the active N bits
  shl rax,8                   //eliminate the East-most NW byte
  mov al,[rdx+24]             //Get NW status
  mov r10b,al                 //save NW status for later
  and r8,r11                  //mask off the N status
  and rax,r11                 //mask off the NW status
  or rax,r8                   //combine the two
  movq xmm1,rax               //put into an xmm register for movmsk
  pmovmskb eax,xmm1           //compress the 8 bits into a single byte
  or [rcx],al                 //add the status bits to the bitmap for the north row.
  ror r11,1                   //mask the W bits
  //now process W and NW
  mov rax,[rdx+8]             //rax = statusW1
  mov rdx,[rdx+16]            //rdx = statusW2
  mov r8,rax                  //r8 = status NW1
  mov r9,rdx                  //r9 = status NW2
  rol r8,8                    //eliminate the Southern most NW bit
  rol r9,8                    //this is now located in the (soon to be replaced) LSB
  mov r9b,r8b                 //move the overflow from W1 into W2
  mov r8b,r10b                //replace the W western most part with the NW byte we saved earlier
  shr r8,1                    //align r10 with the W-mask
  shr r9,1                    //align r11 with the W-mask
  and rax,r11                 //mask off W1
  and rdx,r11                 //mask off W2
  and r8,r11                  //mask off NW1
  and r9,r11                  //mask off NW2
  or rax,r8                   //combine W1 and NW1
  or rdx,r9                   //combine W2 and NW2
  shr rax,6                   //shift to LSB, because that's where the bitmap needs it.
  shr rdx,6                   //ditto for W2
  or [rcx],rax                //write W1 to the bitmap
  or [rcx+8],rdx              //ditto for W2
  ret
end;

procedure TBitmap.ProcessSE(status: PBorderStatus);
asm
  //rcx = self
  //rdx = PStatus
  //  ;bit    7----6----5----4----3----2----1----0
  //  ;       +    active      |  alive          +
  //  ;       SE   E    S   all   SE   E    S   all
  mov r8,[rdx]                //r8 = StatusS
  mov rax,r8                  //rax = statusSE
  shl r8,2                    //move S status bits to the MSB
  mov r11,$8080808080808080   //Mask out the active S bits
  shr rax,8                   //eliminate the West-most SE byte
  mov al,[rdx+24]             //Get SE status
  movzx r10,al                //save SE status for later
  and r8,r11                  //mask off the S status
  and rax,r11                 //mask off the SE status
  shr r11,1                   //mask for the E bits
  or rax,r8                   //combine the two
  movq xmm1,rax               //put into an xmm register for movmsk
  pmovmskb eax,xmm1           //compress the 8 bits into a single byte
  or [rcx+15],al              //add the status bits to the bitmap for the south row.
  //now process W and NW
  mov rax,[rdx+8]             //rax = statusE1
  mov rdx,[rdx+16]            //rdx = statusE2
  mov r8,rax                  //r8 = status SE1
  mov r9,rdx                  //r9 = status SE2
  mov r9b,r8b                 //move the overflow from W1 into W2
  mov r8b,r10b                //replace the W western most part with the NW byte we saved earlier
  ror r8,8                    //eliminate the northern most SE bit
  ror r9,8                    //this is now located in the (soon to be replaced) MSB
  shr r8,1                    //align r10 with the E-mask
  shr r9,1                    //align r11 with the E-mask
  and rax,r11                 //mask off E1
  and rdx,r11                 //mask off E2
  and r8,r11                  //mask off SE1
  and r9,r11                  //mask off SE2
  or rax,r8                   //combine E1 and SE1
  or rdx,r9                   //combine E2 and SE2
  shl rax,2                   //shift to MSB, because that's where the bitmap needs it.
  shl rdx,2                   //ditto for E2
  or [rcx],rax                //write E1 to the bitmap
  or [rcx+8],rdx              //ditto for E2
  ret
end;






{ CellBlockHelper }

procedure CellBlockHelper.Free;
begin
  FreeMem(Self);
end;

class function CellBlockHelper.New: PCellBlock;
begin
  GetMem(Result, SizeOf(TCellBlock));
end;

{ TLifeHeader }
function TLifeHeader.North(index: integer): PUnit;
begin
  index:= index - XCount;
  if index < 0 then Result:= @N.CellBlock.p[index+UnitsPerBlock]
  else Result:= @CellBlock.p[index];
end;

function TLifeHeader.West(index: integer): PUnit;
begin
  if (index and MaxX) = 0 then Result:= @W.CellBlock.p[index+MaxX]
  else Result:= @CellBlock.p[index-1];
end;

function TLifeHeader.NorthWest(index: integer): PUnit;
begin
  //Test the NW corner.
  if index = 0 then Result:= @NW.CellBlock.p[127]
  //it's not the extreme corner, so if it's W, it must be one of the lower rows.
  else if (index and 7) = 0 then Result:= @W.CellBlock.p[index + 7 - XCount]//W
  //if it's  not north, then it must be the default case.
  else if (index > XCount) then Result:= @CellBlock.p[index - 1 - XCount]
  //else it must be north
  else begin
    index:= index - 1 - XCount + UnitsPerBlock;
    Result:= @N.CellBlock.p[index];
  end;
end;

function TLifeHeader.South(index: integer): PUnit;
begin
  index:= index + XCount;
  if index >= UnitsPerBlock then Result:= @S.CellBlock.q[index-UnitsPerBlock]
  else Result:= @CellBlock.q[index];
end;

procedure TLifeHeader.DisplayP(const Bitmap: TDIBits; const BoundingRect: TRect);
var
  index: integer;
  BitmapPointer: pbyte;
  SelfRect: TRect;
  StartOffset, Offset: integer;
begin
  SelfRect:= Rect(Coordinate.x, Coordinate.y, Coordinate.x+128, Coordinate.y+128);
  if not IntersectRect(SelfRect, BoundingRect) then exit;
  BitmapPointer:= Bitmap.data;
  StartOffset:= (Coordinate.x - BoundingRect.Left) shr 3;
  StartOffset:= StartOffset + (Coordinate.y - BoundingRect.Top) * (Bitmap.width shr 3);
  index:= -1;
  while index <= UnitsPerBlock do begin
    index:= self.DisplayBitmap.Next(index);
    Offset:= StartOffset + ((index and 7) * 2) + ((index and not 7) shr 3) * (Bitmap.width shr 3);
    BitmapPointer:= @Bitmap.data[Offset];
    CellBlock.p[index].Display(BitmapPointer, Bitmap.width div 8);
  end;
end;

procedure TLifeHeader.DisplayQ(const Bitmap: TDIBits; const BoundingRect: TRect);
var
  index: integer;
  BitmapPointer: pbyte;
  SelfRect: TRect;
  StartOffset, Offset: integer;
begin
  SelfRect:= Rect(Coordinate.x, Coordinate.y, Coordinate.x+128, Coordinate.y+128);
  if not IntersectRect(SelfRect, BoundingRect) then exit;
  BitmapPointer:= Bitmap.data;
  StartOffset:= (Coordinate.x - BoundingRect.Left) shr 3;
  StartOffset:= StartOffset + (Coordinate.y - BoundingRect.Top) * (Bitmap.width shr 3);
  index:= -1;
  while index <= UnitsPerBlock do begin
    index:= self.DisplayBitmap.Next(index);
    Offset:= StartOffset + ((index and 7) * 2) + ((index and not 7) shr 3) * (Bitmap.width shr 3);
    BitmapPointer:= @Bitmap.data[Offset];
    CellBlock.q[index].Display(BitmapPointer, Bitmap.width div 8);
  end;
end;

function TLifeHeader.East(index: integer): PUnit;
begin
  if (index and MaxX) = MaxX then Result:= @E.CellBlock.q[index-MaxX]
  else Result:= @CellBlock.q[index+1];
end;


function TLifeHeader.SouthEast(index: integer): PUnit;
begin
  if index = (UnitsPerBlock-1) then Result:= @SE.CellBlock.q[0]
  else if (index and MaxX) = MaxX then Result:= @E.CellBlock.q[index - MaxX + XCount]
  else if (index < (UnitsPerBlock - XCount)) then Result:= @CellBlock.q[index + 1 + XCount]
  else begin
    index:= index + 1 + XCount - UnitsPerBlock;
    Result:= @S.CellBlock.q[index];
  end;
end;


function TLifeHeader.GeneratePtoQ(const CellUnit: PUnit; index: Integer): TUnitStatus;
begin
  Result:= Generate_PtoQ_Full_AVX(CellUnit, North(index), West(index), NorthWest(index));
end;

function TLifeHeader.GenerateQtoP(const CellUnit: PUnit; index: Integer): TUnitStatus;
begin
  Result:= Generate_QtoP_Full_AVX(CellUnit, South(index), East(index), SouthEast(index));
end;

procedure TLifeHeader.ProcessPtoQ;
var
  index: integer;
  CellUnit: PUnit;
  UnitState: TUnitStatus;
begin
  //run through the active cells in P and process them
  //store the result in Q.
  index:= -1;
  while index < UnitsPerBlock do begin
    index:= p.ActiveBitmap.Next(index);
    CellUnit:= @CellBlock.p[index];
    UnitState:= Self.GeneratePtoQ(CellUnit, index);
    q.BorderStatus[index]:= UnitState;
  end;
end;

procedure TLifeHeader.ProcessQtoP;
var
  index: integer;
  CellUnit: PUnit;
  UnitState: TUnitStatus;
begin
  //run through the active cells in Q and process them
  //store the result in P.
  index:= -1;
  while index < UnitsPerBlock do begin
    index:= q.ActiveBitmap.Next(index);
    CellUnit:= @CellBlock.q[index];
    UnitState:= Self.GenerateQtoP(CellUnit, index);
    p.BorderStatus[index]:= UnitState;
  end;
end;


end.
