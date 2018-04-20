unit LifeList;

interface

uses
  System.Types,
  System.Classes,
  system.generics.collections,
  LifeTypes,
  SparseArray,
  LifeCell,
  OtlParallel,
  VCL.Graphics
  ;

{$pointermath on}
{$T+}

type
  TListSortCompare = function (const Item1, Item2: TCoordinate): Integer;
  PCoordinate = ^TCoordinate;

  /// <summary>
  ///  The list contains an list of coordinates to be processed
  /// </summary>
  TLifeList = class(TObject)
  private type
    T = TCoordinate;
    PT = ^T;
    TData = PT;
  private const
    MaxCapacity = 1024*1024*SizeOf(T) * 8; //32 megs, 8 million cells = 800 MB of (active !) cells
    InitialSize = 1024*1024 * SizeOf(T); //4 MB
  private
    FData: TData;
    FCapacity: cardinal;
    FCount: cardinal;
    function GetData(index: integer): TCoordinate; inline;

    /// <summary>
    ///  Double the size of the storage.
    /// </summary>
    procedure Grow;
    class function BinarySearch(Start, Finish: PT; Item: T): PT; static;
    function IndexOf(Ptr: PT): cardinal; overload; inline;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Add an item to the end of the list. Grow the list as needed.
    /// </summary>
    function Add(const Item: TCoordinate): integer; overload;

    /// <summary>
    ///   Add an item to the list only if it falls within the bounding box.
    /// </summary>
    /// <returns>
    ///  -1 if the item falls outside the bounding box, the insertion point otherwise.
    /// </returns>
    function Add(const BoundingBox: TRect; const Item: TCoordinate): integer; overload;

    /// <summary>
    ///  Add a list of item to the collection.
    /// </summary>
    /// <param name="List">
    ///  The Items to add.
    /// </param>
    /// <remarks>
    ///  Note that the donating List will have its items removed.
    ///  Duplicates are allowed, but will not be inserted.
    /// </remarks>
    procedure Add(List: TLifeList); overload;

    procedure Add(const BoundingBox: TRect; const List: TLifeList); overload;

    /// <summary>
    ///  Delete all items from the list.
    /// </summary>
    /// <remarks>
    ///  This actually just sets the count to zero.
    /// </remarks>
    procedure Clear;

    /// <summary>
    ///  Delete the item.
    ///  Fill up the hole by moving the last item into the gap.
    /// </summary>
    /// <remarks>
    ///   Note that the list does not shrink; if we grew to that size once
    ///  we'll likely need that space again.
    /// </remarks>
    procedure Delete(index: integer); overload;


    /// <summary>
    ///  Remove a list of items.
    ///  The list will be sorted afterwards.
    /// </summary>
    procedure Delete(List: TLifeList); overload;

    /// <summary>
    ///  Find an item in the list.
    /// </summary>
    /// <param name="Item">
    ///  The item to be located.
    /// </param>
    /// <returns>
    ///  The index if found, -1 if not found.
    /// </returns>
    function IndexOf(Item: TCoordinate): integer; overload;

    /// <summary>
    ///  Find an item in the list starting at the specified index inclusively.
    /// </summary>
    /// <param name="Start">
    ///  The start index (inclusively where to start the search
    /// </param>
    /// <param name="Item">
    ///  The Item to search
    /// </param>
    /// <returns>
    ///  The index if found, -1 if not found.
    /// </returns>
    /// <remarks>
    ///  This function uses a binary search
    /// </remarks>
    function IndexOf(Start: cardinal; Item: TCoordinate): integer; overload;


    /// <summary>
    ///   Sort the list based on the coordinate.
    /// </summary>
    procedure Sort;

    /// <summary>
    ///  Remove all items that do not touch the bounding box
    ///  move those items to the wastebin.
    /// </summary>
    procedure Clip(const BoundingBox: TRect; WasteBin: TLifeList); overload;

    /// <summary>
    ///   Reclaim any memory that we might have over-allocated.
    /// </summary>
    /// <remarks>
    ///   Only run this code if the data has been reset.
    /// </remarks>
    procedure Shrink; overload;

    property Item[index: integer]: TCoordinate read GetData; default;
    property Count: cardinal read FCount;
  end;


  TFixup = record
  private
    FWakeUp: TLifeList;
    FPutToSleep: TLifeList;
    FBorn: TLifeList;
    FKill: TLifeList;
  public
    class function Create: TFixup; static;
    property WakeUp: TLifeList read FWakeUp;
    property PutToSleep: TLifeList read FPutToSleep;
    property Born: TLifeList read FBorn;
    property Kill: TLifeList read FKill;
  end;


  PUniverse = ^TUniverse;
  TUniverse = record
  private type
    TStorage = TSparseArray<TLifeCell>;
  private
    FLiveCells: TLifeList;
    FSleepingCells: TLifeList;
    FDisplayCells: TLifeList;
    FFixup: array of TFixup;
    FStorage: TStorage;
    FCycle: TCycle;
    FGeneration: Int64;
    FId: integer;
  property Cycle: TCycle read FCycle;
  public type
    PT = TStorage.PT;
  private
    function GetFixup(ThreadId: integer): TFixup; inline;
    function GetItem(const Coordinate: TCoordinate): PT; overload; inline; //Fixup when running multiple threads
    //function GetItem(const x,y: integer): PT; overload; inline;
  private
    procedure DoCell(value: integer);
    function GetThreadCount: integer;
  private //LifeCell methods
    procedure GeneratePtoQ(var c: TLifeCell);
    procedure GenerateQtoP(var c: TLifeCell);
    procedure UpdateQState(var c: TLifeCell; NewQState: TQLifeState); //only used inside GeneratePtoQ;
    procedure UpdatePState(var c: TLifeCell; NewPState: TPLifeState); //only used inside GenerateQtoP;
    function N(const c: TLifeCell): PT; inline;
    function W(const c: TLifeCell): PT; inline;
    function NW(const c: TLifeCell): PT; inline;
    function S(const c: TLifeCell): PT; inline;
    function E(const c: TLifeCell): PT; inline;
    function SE(const c: TLifeCell): PT; inline;
  private
    property LiveCells: TLifeList read FLiveCells;
    property SleepingCells: TLifeList read FSleepingCells;
    property DisplayCells: TLifeList read FDisplayCells;
    property Storage: TStorage read FStorage;
    property ThreadCount: integer read GetThreadCount;
    property Item[const Coordinate: TCoordinate]: PT read GetItem; default;
    //property Item[const x,y: integer]: PT read GetItem; default;
  public
    class function Create: TUniverse; static;
    procedure Add(const Cell: TLifeCell);
    procedure Display(Canvas: TCanvas);
    procedure DoGeneration;
    procedure SimpleGeneration(const DisplayBox: TRect);
    property Fixup[ThreadId: integer]: TFixup read GetFixup;
  end;


var
  Universes: TList<TUniverse>;

implementation

uses
  System.Math,
  OtlCommon,
  Winapi.Windows;

procedure QuickSort(SortList: TLifeList; L, R: integer; SCompare: TListSortCompare);
var
  I, J: integer;
  P, T: TCoordinate;
begin
  if SortList.Count = 0 then exit;
  repeat
    I:= L;
    J:= R;
    P:= SortList.FData[(L + R) shr 1];
    repeat
      while SCompare(SortList.FData[I], P) < 0 do Inc(I);
      while SCompare(SortList.FData[J], P) > 0 do Dec(J);
      if I <= J then begin
        if I <> J then begin
          T:= SortList.FData[I];
          SortList.FData[I]:= SortList.FData[J];
          SortList.FData[J]:= T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SCompare);
    L:= I;
  until I >= R;
end;

function Generate_QtoP(SE,SW,NE,NW_: Int64):Int64;
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
  // Layout of a Cell is
  //  DDD  C
  //  BBB  A

  mov rbp,$C0C0C0C0C0C0C0C0  //The leftmost 2 columns of A and C
  shr rdx,(64-16)            //Keep only the two top rows of B (and put them at the bottom)
  mov rsi,$1111111111111111  //counting mask
  and r8,rbp                 //leftmost 2 columns of C
  shr r8,(8-2)               //shift out the 6 columns we don't need
  lea rdi,[rsi+rsi*4]        //$55555555  //fast counting mask
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
  mov rbx,r9  //Cache D'
  mov r11,rdi  //Cache D"
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
  //rax is live Cells in 2468
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
  mov rbp,rsi
  not rbp             //Mask CCCCCCC

  @ExtractLiveCells:
  ///  We now need to extract just the middle part for the live Cells
  shl r10,8     //middle part of D 1357
  shr r14,8     //middle part of B 1357
  add r10,r14   //Live Cells 1357
  mov r14,r10   //copy

  shl r11,8     //middle part of D 2468
  shr r15,8     //middle part of B 2468
  add r11,r15   //Live Cells 2468
  mov r15,r11   //copy

  and r10,rbp   //r10,r14 = Live Cells 1357
  shr r10,2     //r10 = live Cells 37
  and r14,rsi   //r8 = live Cells 15


  and r11,rbp   //r11,r15 = Live Cells 2468
  shr r11,2     //r11 = live Cells 48
  and r15,rsi   //r9 = live Cells

@AddCountsTogether:
  //input: r8:  Ncount for D1357
  //       r9:  Ncount for D2468    //rdx = copy
  //       r10: live Cells for 1357 //r14 = copy
  //       r11: live Cells for 2468 //r15 = copy
  //       r12: NCount for B1357
  //       r13: NCount for B2468    //rbx = copy
  //       rdi: mask 55555555
  //Push the live Cells

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
  add rax,rdx      //Add top+middle+bottom 48 for 9 Cell count

  //now we have the neighbor counts including the centre Cells.
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
  ///  r14: Live Cells 15
  ///  r15: Live Cells 26
  ///  r10: Live Cells 37
  ///  r11: Live Cells 48

/// Start with NCount 15
///  subtract the live Cells from the Cell count
  sub rdx,r14     //NCount 15 - Live 15
  sub rcx,r15     //NCount 26 - Live 26
  sub rbx,r10     //NCount 37 - Live 37
  sub rax,r11     //NCount 48 - Live 48
  xor rdx,rbp     //reg = not(NCount xor 3)
  xor rcx,rbp     //if dead+3 or Live+3 -> 0 xor FF = 1111, if 2 neighbors -> 1 xor FF = 1110
  xor rbx,rbp     //so it's a 1111 if new Cell for sure, 1110 if maybe new Cell
  xor rax,rbp     //and something else otherwise
  or rdx,r14      //if NCount=2 (1110) + life Cell (0001) -> reg = 1111
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
    //Truthtable for Cell counts
//1.  Full Cellcount                     0  	 1	  2    3	  4 	 5 	  6    7     8     9
//1.  Fill Cellcount                  0000  0001 0010 0011 0100 0101 0110 0111  1000  1001
//2.  Full Cellcount xor 3	              11  	10	  1	   0	111  110 	101  100  1011  1010
//3.  Full Cellcount and not life Cell 0010  0010 0000	0000 0110 0110 0100 0100  1010  1010
//4.  Full Cellcount and not dead Cell 0011  0010 0001	0000 0111 0110 0101 0100  1011  1010 //no-op from 2.
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
  //16*16 = 256 nops.
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
end;


procedure GenerationTail;
//Paste this code when you're patching the life rule.

asm
//////////////////////////////////////////////////////
///  End of the code that's specific to a rule.
/////////////////////////////////////////////////////

  //Register use is as follows:
  ///  rax: Dirty live counts 48 , nibble=0 if new live, other if dead
  ///  rbx: Dirty live counts 37
  ///  rcx: Dirty live counts 26
  ///  rdx: Dirty live counts 15
//Clean up the live counts
    //Truthtable for Cell counts
//1.  Full Cellcount                     0  	 1	  2    3	  4 	 5 	  6    7     8     9
//1.  Fill Cellcount                  0000  0001 0010 0011 0100 0101 0110 0111  1000  1001
//2.  Full Cellcount xor 3	              11  	10	  1	   0	111  110 	101  100  1011  1010
//3.  Full Cellcount and not life Cell 0010  0010 0000	0000 0110 0110 0100 0100  1010  1010
//4.  Full Cellcount and not dead Cell 0011  0010 0001	0000 0111 0110 0101 0100  1011  1010 //no-op from 2.
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



//157 instructions for 64 Cells = 2.5 instructions per Cell.
//55 cycles = 2.85 instr per cycle

function Generate_PtoQ(NW,NE,SW,SE_: Int64):Int64;
asm
  push rdi   //rcx: A
  push rbx   //rdx: B
  push rsi   //r8:  C
  push r14   //r9:  D
  push r15   // Layout of a Cell is
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
  //r11 is live Cells in 2468
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
  ///  We now need to extract just the middle part for the live Cells
  shr r10,8     //middle part of D 1357
  shl rdx,8     //middle part of B 1357
  add r10,rdx   //Live Cells 1357
  mov r14,r10

  shr r11,8     //middle part of D 2468
  shl r15,8     //middle part of B 2468
  add r11,r15   //Live Cells 2468
  mov r15,r11

  and r10,rbp   //split up the live Cells so they match with the counts.
  shr r10,2     //r10 = live Cells 37
  and r14,rsi   //r8 = live Cells 15

  and r11,rbp   //split up the live Cells so they match with the counts.
  shr r11,2     //r11 = live Cells 48
  and r15,rsi   //r9 = live Cells 26

@AddRowsTogether:
  //input: r8:  Ncount for D1357  //rdx = copy
  //       r9:  Ncount for D2468
  //       r10: live Cells for 1357
  //       r11: live Cells for 2468
  //       r12: NCount for B1357  //rax = copy
  //       r13: NCount for B2468
  //       rdi: mask 11111111
  //Push the live Cells

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
  add rax,rdx    //Add top+middle+bottom for 9 Cell count

  //now we have the neighbor counts including the centre Cells.
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
  ///  r14: Live Cells 15
  ///  r15: Live Cells 26
  ///  r10: Live Cells 37
  ///  r11: Live Cells 48

/// Start with NCount 15
  sub rdx,r14     //subtract the live Cells from the Cell count
  sub rcx,r15
  sub rbx,r10
  sub rax,r11
  xor rdx,rbp    //reg = not(NCount xor 3)
  xor rcx,rbp    //if dead+3 or Live+3 -> 0 xor FF = 1111, if 2 neighbors -> 1 xor FF = 1110
  xor rbx,rbp    //so it's a 1111 if new Cell for sure, 1110 if maybe new Cell
  xor rax,rbp    //and something else otherwise
  or rdx,r14      //if NCount=2 (1110) + life Cell (0001) -> reg = 1111
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
    //Truthtable for Cell counts
//1.  Full Cellcount                     0  	 1	  2    3	  4 	 5 	  6    7     8     9
//1.  Fill Cellcount                   0000  0001 0010  0011 0100 0101 0110 0111  1000  1001
//2.  Full Cellcount xor 3	             11  	10	   1	   0	111  110 	101  100  1011  1010
//3.  Full Cellcount and not life Cell 0010  0010 0000	0000 0110 0110 0100 0100  1010  1010
//4.  Full Cellcount and not dead Cell 0011  0010 0001	0000 0111 0110 0101 0100  1011  1010 //no-op from 2.
//3a  Not 3                            1101  1101 1111  1111 1001 1001 1011 1011  0101  0101
//4a  Not 4.                           1100  1101 1110  1111 1000 1001 1010 1011  0100  0101
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
  and rax,rdi     //keep bit 0
  and rdi,rbx
  shl rax,1
  shr rbx,1
  and rdi,rbx
  and rax,r10     //and combine it with bit 1
  or rax,rdi      //add everything together
  //cleanup
  pop rbp
  pop r13
  pop r12
  pop r15
  pop r14
  pop rsi
  pop rbx
  pop rdi
    //16*16 = 256 nops.
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
//  ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret ret
end;



{ TLifeList }

function TLifeList.IndexOf(Ptr: PT): cardinal;
begin
  Result:= (NativeInt(Ptr) - NativeInt(FData)) shr (SizeOf(T) div 2);
end;

function TLifeList.Add(const Item: TCoordinate): integer;
begin
  FData[FCount]:= Item;
  Result:= FCount;
  Inc(FCount);
  if FCount = FCapacity then Grow;
end;

function TLifeList.Add(const BoundingBox: TRect; const Item: TCoordinate): integer;
begin
  if Item in BoundingBox then Result:= Add(Item)
  else Result:= -1;
end;


procedure TLifeList.Add(List: TLifeList);
begin
  //Test to see if we have enough free space in the list.
  if (Count + List.Count) > FCapacity then Grow;
  Move(List.FData[0], FData[Count], List.Count * SizeOf(T));
  FCount:= FCount + List.Count;
  List.Clear;
  Sort;
end;

procedure TLifeList.Add(const BoundingBox: TRect; const List: TLifeList);
var
  Item: TCoordinate;
  i: integer;
begin
  for i:= 0 to List.Count - 1 do begin
    Item:= List[i];
    if Item in BoundingBox then Add(Item)
  end;
end;

class function TLifeList.BinarySearch(Start, Finish: PT; Item: T): PT;
var
  L, H, Middle: PT;
  cmp: Integer;
begin
  Result:= nil;
  if Start < Finish then Exit;

  L:= Start;
  H:= Finish;
  while L <= H do begin
    Middle:= L + (H - L) shr 1;
    cmp:= T.Compare(Middle^,Item);
    if cmp < 0 then L:= @Middle[1]
    else begin
      if cmp = 0 then Exit(Middle);
      H:= @Middle[-1];
    end;
  end; {while}
end;

constructor TLifeList.Create;
begin
  inherited;
  GetMem(FData,InitialSize);
  FCapacity:= InitialSize;
end;

procedure TLifeList.Delete(index: integer);
var
  Temp: TCoordinate;
begin
  Dec(FCount);
  //if (index <> FCount) then begin //99% of the time the test is true, eliminate to avoid misprediction.
  //swap gap and last element.
  //if gap=last element, then this is a no-op.
  FData[index]:= FData[FCount];
end;

procedure TLifeList.Delete(List: TLifeList);
var
  i,GapSize, MoveSize: Integer;
  Item: PT;
  Last: PT;
begin
  //Sort the list.
  List.Sort;
  Self.Sort;
  //Make a list of all the items that need to be removed.
  Item:= FData;
  Last:= @FData[FCount-1];
  for i := 0 to List.Count - 1 do begin
    Item:= BinarySearch(Item, Last, List[i]);
    Assert(Item <> nil,'TLifeList.Delete: Cannot find the item');
    //Store the deleted item in the DeleteList
    List.FData[i]:= TCoordinate(IndexOf(Item));
  end;
  List.FData[List.Count].Data:= Cardinal(-1);  //put sentinel

  //Now we have a list of indexes to remove, process these to close the gaps.

  {TODO -oJB -cOptimization : ??Rewrite this move section in assembly??}
  i:= 0;
  while i < List.Count do begin
    GapSize:= 1;
    while (List[i] - List[i+1]) = -1 do Inc(GapSize);
    Last:= @FData[Integer(List[i+GapSize+1])];
    Dec(Last);
    //Do not double move data.
    if Last < FData then Last:= @FData[FCount-1];
    //Only move the items between two gaps.
    MoveSize:= NativeInt(Last) - NativeInt(@FData[Integer(List[i])]);
    Move(FData[List[i].Data+GapSize], FData[Integer(List[i])], MoveSize);
    Inc(i, GapSize);
  end;
  FCount:= FCount - List.Count;
  List.Clear;
end;

destructor TLifeList.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

function TLifeList.GetData(index: integer): TCoordinate;
begin
  Result:= FData[index];
end;

procedure TLifeList.Grow;
begin
  FCapacity:= FCapacity * 2;
  Assert(FCapacity <= MaxCapacity);
  ReAllocMem(FData, FCapacity * SizeOf(T));
end;

function TLifeList.IndexOf(Item: TCoordinate): integer;
begin
  Result:= IndexOf(BinarySearch(@FData[0], @Fdata[FCount-1], Item));
end;

function TLifeList.IndexOf(Start: cardinal; Item: TCoordinate): integer;
begin
  Result:= IndexOf(BinarySearch(@FData[Start], @Fdata[FCount-1], Item));
end;

procedure TLifeList.Clear;
begin
  FCount:= 0;
end;

procedure TLifeList.Clip(const BoundingBox: TRect; WasteBin: TLifeList);
var
  i: Integer;
begin
  i:= 0;
  while i < FCount do begin
    if not(FData[i] in BoundingBox) then begin
      WasteBin.Add(FData[i]);
      Delete(i);
    end else Inc(i);
  end; {while}
end;

procedure TLifeList.Shrink;
var
  NewCapacity: cardinal;
begin
  NewCapacity:= FCapacity;
  while NewCapacity > FCount do begin
    NewCapacity:= NewCapacity shr 1;
  end; {while}
  NewCapacity:= NewCapacity shl 1;
  if NewCapacity <> FCapacity then begin
    FCapacity:= NewCapacity;
    ReAllocMem(FData, NewCapacity);
  end;
end;

procedure TLifeList.Sort;
begin
  QuickSort(Self, 0, FCount-1, TCoordinate.Compare);
end;

{ TFixup }

class function TFixup.Create: TFixup;
begin
  Result.FWakeUp:= TLifeList.Create;
  Result.FPutToSleep:= TLifeList.Create;
  Result.FBorn:= TLifeList.Create;
  Result.FKill:= TLifeList.Create;
end;

{ TUniverse }

procedure TUniverse.UpdatePState(var c: TLifeCell; NewPState: TPLifeState);
var
  BorderChange: TLifeState;
  StillPart: TStillPartP;
  Quadrant: TQuadrant;
  Offset: TPoint;
begin
  //Check to see if the PState has changed and take action based on that.
  //1. Check to see if old PState has dead borders in it.
  if c.PState.GetHibernatingBorders <> 0 then begin

  end;


  if c.PState.GetDeadBorders <> 0 then begin
    //Yes: has anything changed in these dead borders?
    BorderChange:= (c.PState.GetDeadBorders xor NewPState.GetDeadBorders);
    repeat
      StillPart:= BorderChange.GetStillPart;
      BorderChange:= BorderChange - StillPart;

      //This Quadrant has just become live. Create the new neighborcell for this quadrant.
      Offset:= StillPart.GetDeadOffset;
      if not(Offset.IsZero) then begin
        //Add(LifeCell^, Offset);
      end else begin
        Offset:= StillPart.GetHiberOffset;
        if not(Offset.IsZero) then begin
          Fixup[0].WakeUp.Add(c.Coordinate.Offset(Offset));
        end;
      end;
    until (StillPart = 0);
  end else begin
    c.PState:= NewPState;
    //Have we just died?
    if (c.PState or c.QState).IsDead then Fixup[0].Kill.Add(c.Coordinate)
    else if (c.PState or c.QState).IsHibernating then Fixup[0].PutToSleep.Add(c.Coordinate);
  end;
end;


procedure TUniverse.UpdateQState(var c: TLifeCell; NewQState: TQLifeState);
begin

end;



procedure TUniverse.GeneratePtoQ(var c: TLifeCell);
var
  NewSE, NewSW, NewNE, NewNW: Int64;
  DiffSE, DiffSW, DiffNE, DiffNW: TLifeState;
begin
  {TODO 1 -oJB -cOptimization : Only generate when there is a need.}
  //NW,NE,SW,SE_ (in that order)
  NewSE:= Generate_PtoQ(    c.p[cNW],  c.p[cNE],   c.p[cSW],c.p[cSE]);
  NewSW:= Generate_PtoQ( W(C).p[cNE],  c.p[cNW],W(C).p[cSE],c.p[cSW]);
  NewNE:= Generate_PtoQ( N(C).p[cSW],N(C).p[cSE],  c.p[cNW],c.p[cNE]);
  NewNW:= Generate_PtoQ(NW(C).p[cSE],N(C).p[cSW],W(C).p[cNE],c.p[cNW]);
  DiffSE:= NewSE.Difference(c.q[cSE], cSE);
  DiffSW:= NewSW.Difference(c.q[cSW], cSW);
  DiffNE:= NewNE.Difference(c.q[cNE], cNE);
  DiffNW:= NewNW.Difference(c.q[cNW], cNW);
  c.qState:= (DiffSE or DiffSW or DiffNE or DiffNW);
  c.q[cSE]:= NewSE;
  c.q[cSW]:= NewSW;
  c.q[cNE]:= NewNE;
  c.q[cNW]:= NewNW;
end;

procedure TUniverse.GenerateQtoP(var c: TLifeCell);
var
  NewSE, NewSW, NewNE, NewNW: Int64;
  DiffSE, DiffSW, DiffNE, DiffNW: TLifeState;
  NewPState: TLifeState;
  StateDiff: TLifeState;
  BorderChange: TLifeState;

begin
  //SE,SW,NE,NW_ in that order
  NewNW:= Generate_QtoP(   c.q[cSE],  c.q[cSW],  c.q[cNE],c.q[cNW]);
  NewNE:= Generate_QtoP( E(c).q[cSW],  c.q[cSE],E(c).q[cNW],c.q[cNE]);
  NewSW:= Generate_QtoP( S(c).q[cNE],S(c).q[cNW],  c.q[cSE],c.q[cSW]);
  NewSE:= Generate_QtoP(SE(c).q[cNW],S(c).q[cNE],E(c).q[cSW],c.q[cSE]);
  DiffSE:= NewSE.Difference(c.p[cSE], cSE);
  DiffSW:= NewSW.Difference(c.p[cSW], cSW);
  DiffNE:= NewNE.Difference(c.p[cNE], cNE);
  DiffNW:= NewNW.Difference(c.p[cNW], cNW);
  NewPState:= (DiffSE or DiffSW or DiffNE or DiffNW);
  c.p[cSE]:= NewSE;
  c.p[cSW]:= NewSW;
  c.p[cNE]:= NewNE;
  c.p[cNW]:= NewNW;
  UpdatePState(c, NewPState);
end;


procedure TUniverse.Add(const Cell: TLifeCell);
var
  NewCell: PT;
begin
  NewCell:= Storage.Add(Cell.x, Cell.y);
  Move(Cell, NewCell^, SizeOf(Cell));
  LiveCells.Add(NewCell.Coordinate);
  DisplayCells.Add(NewCell.Coordinate);
end;

class function TUniverse.Create: TUniverse;
var
  i: integer;
begin
  FillChar(Result, SizeOf(Result), #0);
  Result.FStorage:= TStorage.Create;
  Result.FLiveCells:= TLifeList.Create;
  Result.FSleepingCells:= TLifeList.Create;
  Result.FDisplayCells:= TLifeList.Create;
  SetLength(Result.FFixup, Result.ThreadCount);
  for i:= 0 to Result.ThreadCount - 1 do begin
    Result.FFixup[i]:= TFixup.Create;
  end;
  Result.FId:= Universes.Add(Result);
end;

function TUniverse.N(const c: TLifeCell): PT;
begin
  Result:= Item[c.Coordinate.N];
end;

function TUniverse.W(const c: TLifeCell): PT;
begin
  Result:= Item[c.Coordinate.W];
end;

function TUniverse.NW(const c: TLifeCell): PT;
begin
  Result:= Item[c.Coordinate.NW];
end;

function TUniverse.E(const c: TLifeCell): PT;
begin
  Result:= Item[c.Coordinate.E];
end;

function TUniverse.S(const c: TLifeCell): PT;
begin
  Result:= Item[c.Coordinate.S];
end;

function TUniverse.SE(const c: TLifeCell): PT;
begin
  Result:= Item[c.Coordinate.SE];
end;

procedure ShowCell(Universe: TUniverse; List: TLifeList; Canvas: TCanvas; Cycle: TCycle);
const
  cHeight = 100;
  cWidth = 40;
var
  bmpinfo: PBitmapInfo;
  BMPInfo1: array[1..SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD)*2] of byte;
  color: PRGBQUAD;
  i: Integer;
  x,y,h: integer;
  DataBuffer: array[0..cHeight-1,0..cWidth-1] of byte;
  ScanLineWidth: integer;
  Cell: TLifeCell;
  Coordinate: TCoordinate;
begin
  FillChar(DataBuffer, SizeOf(DataBuffer), #0);
  //GetMem(bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD)*2);
  bmpInfo:= PBitmapInfo(@BMPInfo1);
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
    biWidth:= cWidth*8;
    biHeight:= -cHeight;
    biPlanes:= 1;
    biBitCount:= 1;
    biCompression:= BI_RGB;
    biSizeImage:= 0;
    biXPelsPerMeter:= 0;
    biYPelsPerMeter:= 0;
    biClrUsed:= 0;
    biClrImportant:= 0;
  end;

  ScanlineWidth:= cWidth div 8;
  if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);

//  for x:= 0 to cHeight-1 do begin
//    for y:= 0 to cWidth-1 do begin
//      DataBuffer[x][y]:= Random(255);
//    end;
//  end;
//
//  StretchDIBits(Canvas.Handle, 0 , 0, cHeight*2, cWidth*2, 0, 0, cHeight, cWidth,
//    @DataBuffer, bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
//  for x:= 0 to cwidth-1 do begin
//    for y:= 0 to cheight-1 do begin
//      DataBuffer[y][x]:= Random(255);
//    end;
//  end;
  for i := 0 to List.Count - 1 do begin
    Coordinate:= List[i];
    Universe[Coordinate].Display(@DataBuffer[0][0], 0,0,cWidth,Cycle);
  end;


  StretchDIBits(Canvas.Handle, Cycle.x*2,Cycle.y*2,cWidth*2*8,cHeight*2,0,0,cwidth*8,cHeight,@DataBuffer,
    bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
  //FreeMem(bmpinfo);
end;


procedure TUniverse.Display(Canvas: TCanvas);
begin
  ShowCell(Self, DisplayCells, Canvas, Cycle);
end;


procedure TUniverse.DoCell(value: integer);
begin

end;

procedure TUniverse.DoGeneration;
var
  Universe: PUniverse;
begin
  Universe:= @Self;
  Parallel.&For(0, LiveCells.Count-1)
          .Execute(Universe.DoCell);
  Inc(FGeneration);
  //FCycle.Flip;
end;



procedure TUniverse.SimpleGeneration(const DisplayBox: TRect);
var
  i: integer;
begin
  case Cycle of
    Pcycle: begin
      for i:= 0 to LiveCells.Count - 1 do begin
        GeneratePtoQ(Item[LiveCells[i]]^);
      end;
    end;
    QCycle: begin
      for i:= 0 to LiveCells.Count - 1 do begin
        GenerateQtoP(Item[LiveCells[i]]^);
      end;
    end;
  end;
  Inc(FGeneration);
  Boolean(FCycle):= not(Boolean(FCycle));
  LiveCells.Delete(Fixup[0].Kill);
  LiveCells.Delete(Fixup[0].PutToSleep);
  LiveCells.Add(Fixup[0].WakeUp);
  LiveCells.Add(Fixup[0].Born);
  SleepingCells.Delete(Fixup[0].WakeUp);
  SleepingCells.Add(Fixup[0].PutToSleep);
  DisplayCells.Delete(Fixup[0].Kill);
  DisplayCells.Add(DisplayBox, Fixup[0].Born);
end;



function TUniverse.GetFixup(ThreadId: integer): TFixup;
begin
  Result:= FFixup[ThreadId];
end;

//function TUniverse.GetItem(const x, y: integer): PT;
//begin
//  Result:= FStorage[x,y];
//end;

function TUniverse.GetItem(const Coordinate: TCoordinate): PT;
begin
  Result:= FStorage[Coordinate];
end;




function TUniverse.GetThreadCount: integer;
begin
  Result:= Environment.Process.Affinity.Count;
end;


initialization
  Universes:= TList<TUniverse>.Create;
finalization
  Universes.Free;
end.
