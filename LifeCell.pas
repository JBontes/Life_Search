unit LifeCell;

interface

uses
  System.Types,
  SparseArray,
  LifeTypes;


{$TypedAddress On}

type
  TLifeFlag = (lfValid,  //Must always be 1.
               lfDisplay, lfMorgue, lfHibernation, lfIncineratable,
               lfPinMorgue, lfQinMorgue, lfPHibernation, lfQHibernation,
               lfRattling, lfCellIsDeleted);

  TLifeFlags = set of TLifeFlag;

  TFlagHelper = record helper for TLifeFlags
  end;

const
  cNW=0; cNE=1; cSW=2; cSE=3;

type
  TQuadrant = cNW..cSE;

  TQuadrantHelper = record helper for TQuadrant
    function x: integer; inline;
    function y: integer; inline;
    function Point: TPoint; inline;
  end;

  TCycle = (Pcycle, QCycle);

  TCycleHelper = record helper for TCycle
    function Flip: TCycle;
    function X: integer; inline;
    function Y: integer; inline;
  end;


  TForbidden = record    //Never use TStillPart directly, always use the specialized type.
  private type
    TStillPart = type cardinal;
  end;

  TStillPartHelper = record helper for TForbidden.TStillPart
    function GetQuadrant: TQuadrant;
  end;

  TStillPartP = type TForbidden.TStillPart;
  TStillPartQ = type TForbidden.TStillPart;

  TStillPartPHelper = record helper for TStillPartP
    function GetHiberOffset: TPoint;
    function GetDeadOffset: TPoint;
  end;

  TStillPartQHelper = record helper for TStillPartQ
    function GetHiberOffset: TPoint;
    function GetDeadOffset: TPoint;
  end;


  TPointHelper = record helper for TPoint
    function IsZero : Boolean; inline;
  end;


  TLifeState = cardinal;
    // each group of 4 bits:
    // |Whole 8x8|Vertical 2x8|Horizontal 8x2|Corner 2x2|
    //  bitmap:
    // |31 30 29 28.27 26 25 24|23 22 21 20.19 18 17 16|15 14 13 12.11 10  9  8| 7  6  5  4. 3  2  1  0|
    // | Northwest quadrant    | Northeast quadrant    | SouthWest quadrant    | Southeast quadrant    |
    // | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     |
    //

  TLifeStateHelper = record helper for TLifeState
  private
  const   //Masks , combine masks to do tests
    MaskCornerHiber =  $01010101;
    MaskHorzHiber =    $02020202;
    MaskVertHiber =    $04040404;
    MaskFullHiber =    $08080808;
    MaskHiber =        $0F0F0F0F;
    MaskBordersHiber = $07070707;
    MaskCornerDead =   $10101010;
    MaskHorzDead =     $20202020;
    MaskVertDead =     $40404040;
    MaskFullDead =     $80808080;
    MaskDead =         $F0F0F0F0;
    MaskBordersDead =  $70707070;
    Dead =             $FFFFFFFF;
    Hibernating =      $0F0F0F0F;
    MaskE = $00FF00FF;
    MaskW = not(MaskE);
    MaskN = $FFFF0000;
    MaskS = not(MaskN);
    MaskNW = MaskN and MaskW;
    MaskSW = MaskS and MaskW;
    MaskNE = MaskN and MaskE;
    MaskSE = MaskS and MaskE;
    MaskP  = $FFAACC00;    //Only look at N, W
    MaskQ  = not(MaskP);   //Only look at N, W
  public

    /// <summary>
    ///  Get the highest bit that is non-active.
    /// </summary>
    function GetStillPart: TForbidden.TStillPart;
    function IsDead: boolean; inline;
    function IsHibernating: boolean; inline;
    function IsSDead: boolean; inline;
    function IsEDead: boolean; inline;
    function IsSEDead: boolean; inline;
    function IsNDead: boolean; inline;
    function IsWDead: boolean; inline;
    function IsNWDead: boolean; inline;
    function IsSEHibernating: boolean; inline;
    function IsSHibernating: boolean; inline;
    function IsEHibernating: boolean; inline;
    function IsNHibernating: boolean; inline;
    function IsNWHibernating: boolean; inline;
    function IsWHibernating: boolean; inline;
    procedure TranquilizeAll; inline;
    procedure KillAll; inline;
  end;

  TPLifeState = type TLifeState;
  TQLifeState = type TLifeState;

  TPLifeStateHelper = record helper for TPLifeState
    function GetDeadBorders: TLifeState; inline;
    function GetHibernatingBorders: TLifeState; inline;
  end;

  TQLifeStateHelper = record helper for TQLifeState
    function GetDeadBorders: TLifeState; inline;
    function GetHibernatingBorders: TLifeState; inline;
  end;

  TLine = array[0..7] of byte;
  TLifePartHelper = record helper for Int64
  private
    function GetLine: TLine; inline;
    procedure SetLine(const Value: TLine); inline;
  public
    function Difference(NewPart: int64; Quadrant: TQuadrant): TLifeState;
    property Line: TLine read GetLine write SetLine;
  end;

  TLifeBlock = array[0..3] of Int64;


  TLifeCell = record
  private type
    TStorage = TSparseArray<TLifeCell>;
  public type
    PLifeCell = TSparseArray<TLifeCell>.PT;
  public
    procedure FillBlack(Cycle: TCycle);
    procedure Invert(Cycle: TCycle);
    procedure Clear(Cycle: TCycle);
    function IsNilCell: boolean; inline;

    /// <summary>
    ///  Apply a mask to the Cell
    /// </summary>
    /// <param name="Mask">
    ///  A lifeblock with the mask. Only the 'on' Cells will be taken into account.
    /// </param>
    /// <param name="Add">
    ///  true to add (or) the mask, false to subtract (nand) the mask.
    /// </param>
    /// <param name="Cycle">
    ///  apply the mask to the PCycle or QCycle.
    /// </param>
    procedure SetMask(const Mask: TLifeBlock; Add: boolean; Cycle: TCycle);

    /// <summary>
    ///  Extract a number of Cells using a mask.
    /// </summary>
    /// <param name="Mask">
    ///  The Mask to extract from the Cell
    /// </param>
    /// <param name="Cycle">
    ///  Extract the mask from the PCycle or QCycle
    /// </param>
    function GetMask(const Mask: TLifeBlock; Cycle: TCycle): TLifeBlock;

    /// <summary>
    ///  Sets a single pixel in a Cell.
    ///  Note that x=0,y=0 is the lower left corner.
    /// </summary>
    /// <param name="x,y">
    ///  x/y-coordinates only the lower 4 bits are taken into account
    /// </param>
    /// <param name="Enable">
    ///  Set the Cell on (true) or off (false), defaults to true
    /// </param>
    /// <param name="Cycle">
    ///  Affect the data for the PCycle or the QCycle, defaults to PCycle
    /// </param>
    procedure SetCell(x,y: integer; Enable: boolean = true; Cycle: TCycle = PCycle);
    function GetCell(x,y: integer; Cycle: TCycle = PCycle): boolean;

    /// <summary>
    ///  Displays the Cell data in the buffer.
    /// </summary>
    /// <param name="DisplayBuffer">
    ///  The displaybuffer; the buffer is always aligned to exact Cell boundaries.
    /// </param>
    /// <param name="OffsetXInBytes">
    ///  The number of Bytes to skip horizontally.
    /// </param>
    /// <param name="OffsetYinLines">
    ///  The number of Lines to skip vertically
    /// </param>
    /// <param name="WidthInBytes">
    ///  The Width of the buffer scanlines in Bytes.
    /// </param>
    /// <param name="Cycle">
    ///  PCycle or QCycle
    /// </param>
    procedure Display(DisplayBuffer: PByte; OffsetXInBytes, OffsetYinLines, WidthInBytes: integer; Cycle: TCycle); inline;

    class procedure Init(Cell: PLifeCell; x,y: integer); static;
  private
    function ConvertPtoQ: TLifeBlock;
    function ConvertQtoP: TLifeBlock;
    procedure DisplayP(DisplayBuffer: PByte; Offset: TPoint; WidthInBytes: integer);
    procedure DisplayQ(DisplayBuffer: PByte; Offset: TPoint; WidthInBytes: integer);
  public
    Flags: TLifeFlags;  //Must be the first field!
    Coordinate: TCoordinate;
    function x: integer; inline;
    function y: integer; inline;
  public

  case boolean of
    true: (
      pState: TPLifeState;
      qState: TQLifeState;
      p,q: TLifeBlock;
    );
    false: (
      State: array [TCycle] of TLifeState;
      Data:  array [TCycle] of TLifeBlock;
    );
  end; //TotalSize: 4*4+4*8*2 = 80 bytes.

  PLifeCell = TLifeCell.PLifeCell;



implementation

uses
  LifeList;


{ TPointHelper }

function TPointHelper.IsZero: Boolean;
begin
  Result:= UInt64(self) = 0;
end;


{ TQuadrantHelper }

//cNW=0; cNE=1; cSW=2; cSE=3;

function TQuadrantHelper.x: integer;
begin
  //E: 1 or 3   -> -1
  //W: 0 or 2   -> +1
  Result:= 1 - ((self and 1) * 2);
end;

function TQuadrantHelper.y: integer;
begin
  //N: 3 or 2
  //S: 0 or 1
  Result:= 1 - (self and 2);
end;

function TQuadrantHelper.Point: TPoint;
begin
  Result.x:= x;
  Result.y:= y;
end;

//    D D D D D D D D D D|C|C
//                       | |
//    D D D D D D D D D D|C|C
//    -------------------+ |
//    B B B B B B B B B B A|A
//    ---------------------+
//    B B B B B B B B B B A A


{ TLifePartHelper }


{TODO -oJB -cCorrect : Change code so it aligns with new definition of TLifeState}
function TLifePartHelper.Difference(NewPart: int64; Quadrant: TQuadrant): TLifeState;
const
  NS = $2;
  EW = $1;
var
  Diff: Int64;
  Mask: Int64;
  i: integer;
begin
    // each group of 4 bits:
    // |Whole 8x8|Vertical 2x8|Horizontal 8x2|Corner 2x2|
    //  bitmap:
    // |31 30 29 28.27 26 25 24|23 22 21 20.19 18 17 16|15 14 13 12.11 10  9  8| 7  6  5  4. 3  2  1  0|
    // | Northwest quadrant    | Northeast quadrant    | SouthWest quadrant    | Southeast quadrant    |
    // | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     |
    //
  Diff:= NewPart;
  Result:= 0;
  for i:= 0 to 1 do begin
    //All
    if Diff = 0 then Result:= Result or $0F
      //Just the corner
    else begin
      Mask:= ($0000000000000033) shl ((56 * (Quadrant and NS)) + (3 * (Quadrant and EW))); //SE
      if (Diff and Mask) = 0 then begin
        Result:= Result or $01;
        //If the corner is stable the other parts might also be stable.
        //Horizontal 2x8
        Mask:= ($00000000000000FF) shl (56 * (Quadrant and NS));
        if Diff and Mask = 0 then Result:= Result or $2;

        //Vertical 8x2
        Mask:= ($0303030303030303) shl (3 * (Quadrant and EW));
        if Diff and Mask = 0 then Result:= Result or $4;
      end;
    end;
    Result:= Result shl 4;
    Diff:= Self xor NewPart;
  end; {for i}
  Result:= Result shl (8 * Quadrant);
end;

function TLifePartHelper.GetLine: TLine;
begin
  Result:= TLine(Self);
end;

procedure TLifePartHelper.SetLine(const Value: TLine);
begin
  Self:= Int64(Value);
end;







function TLifeCell.x: integer;
begin
  Result:= Coordinate.x;
end;

function TLifeCell.y: integer;
begin
  Result:= Coordinate.y;
end;

procedure TLifeCell.Clear(Cycle: TCycle);
begin
  Data[cycle][0]:= 0;
  Data[cycle][1]:= 0;
  Data[cycle][2]:= 0;
  Data[cycle][3]:= 0;
end;

procedure TLifeCell.FillBlack(Cycle: TCycle);
begin
  Data[cycle][0]:= -1;
  Data[cycle][1]:= -1;
  Data[cycle][2]:= -1;
  Data[cycle][3]:= -1;
end;



function TLifeCell.GetCell(x, y: integer; Cycle: TCycle): boolean;
var
  Mask: Int64;
  Block: integer;
begin
  if Cycle = QCycle then begin
    Dec(x);
    Dec(y);
  end;
  Mask:= 1;
  Mask:= (Mask shl (x and $7)) shl ((y and $7)*8);
  Block:= ((x and 8) div 8) + ((y and 8) div 4);
  Result:= (Data[Cycle][Block] and Mask) <> 0;
end;

function TLifeCell.GetMask(const Mask: TLifeBlock; Cycle: TCycle): TLifeBlock;
begin
  Result[cNW]:= Data[Cycle][cNW] and Mask[cNW];
  Result[cNE]:= Data[Cycle][cNE] and Mask[cNE];
  Result[cSW]:= Data[Cycle][cSW] and Mask[cSW];
  Result[cSE]:= Data[Cycle][cSE] and Mask[cSE];
end;

class procedure TLifeCell.Init(Cell: PLifeCell; x, y: integer);
begin
  Cell.Flags:= [lfValid];
  Cell.Coordinate:= point(x,y);
end;

procedure TLifeCell.Invert(Cycle: TCycle);
begin
  Data[cycle][0]:= not(Data[cycle][0]);
  Data[cycle][1]:= not(Data[cycle][1]);
  Data[cycle][2]:= not(Data[cycle][2]);
  Data[cycle][3]:= not(Data[cycle][3]);
end;

function TLifeCell.IsNilCell: boolean;
begin
  Result:= @self = TStorage.EmptyCell;
end;

procedure TLifeCell.SetCell(x, y: integer; Enable: boolean; Cycle: TCycle);
var
  Mask: Int64;
  Block: integer;
begin
  if Cycle = QCycle then begin
    Dec(x);
    Dec(y);
  end;
  Mask:= 1;
  Mask:= (Mask shl (x and $7)) shl ((y and $7)*8);
  Block:= ((x and 8) div 8) + ((y and 8) div 4);
  if Enable then Data[Cycle][Block]:= Data[Cycle][Block] or Mask
  else Data[Cycle][Block]:= Data[Cycle][Block] and not(Mask);
end;

procedure TLifeCell.SetMask(const Mask: TLifeBlock; Add: boolean; Cycle: TCycle);
const
  Addition = true;
  Subtraction = false;
begin
  case Add of
    Addition: begin
      Data[Cycle][cNW]:= Data[Cycle][cNW] or Mask[cNW];
      Data[Cycle][cNE]:= Data[Cycle][cNE] or Mask[cNE];
      Data[Cycle][cSW]:= Data[Cycle][cSW] or Mask[cSW];
      Data[Cycle][cSE]:= Data[Cycle][cSE] or Mask[cSE];
    end;
    Subtraction: begin
      Data[Cycle][cNW]:= Data[Cycle][cNW] and not Mask[cNW];
      Data[Cycle][cNE]:= Data[Cycle][cNE] and not Mask[cNE];
      Data[Cycle][cSW]:= Data[Cycle][cSW] and not Mask[cSW];
      Data[Cycle][cSE]:= Data[Cycle][cSE] and not Mask[cSE];
    end;
  end;
end;




function TLifeCell.ConvertQtoP: TLifeBlock;
begin

end;


procedure TLifeCell.Display(DisplayBuffer: PByte; OffsetXInBytes, OffsetYinLines, WidthInBytes: integer; Cycle: TCycle);
begin
  if Cycle = Pcycle then DisplayP(DisplayBuffer, Point(OffsetXInBytes, OffsetYInLines), WidthInBytes)
  else DisplayQ(DisplayBuffer, Point(OffsetXInBytes, OffsetYInLines), WidthInBytes);
end;



procedure TLifeCell.DisplayP(DisplayBuffer: PByte; Offset: TPoint; WidthInBytes: integer);
var
  i: integer;
  block: Integer;
  Start: integer;
begin
  ///  asm
  ///  See: DisplayQ

  Start:= (WidthInBytes * Offset.Y) + Offset.X;
  for block:= cNW to cSE do begin
    case Block of
      cNW: {do nothing};
      cNE, cSE: Inc(Start, 1);
      cSW: Inc(Start, WidthInBytes);
    end; {case}
    for i:= 0 to 7 do begin
      DisplayBuffer[Start]:= p[Block].Line[i];
      Inc(Start, WidthInBytes);
    end;
    Dec(Start,WidthInBytes*8); //back to the start
  end;
end;

{TODO -oJB -coptimization : This would be a little faster if I have 4 bytes on a horizontal line}
procedure TLifeCell.DisplayQ(DisplayBuffer: PByte; Offset: TPoint; WidthInBytes: integer);
var
  i: integer;
  block: Integer;
  Start: integer;
begin
  ///  asm
  ///  push r12   //overflow
  ///  //Regs: rcx: self
  ///  //      rdx: DisplayBuffer
  ///  //      r8: Offset (TPoint)
  ///  //      r9: WidthInBytes
  ///  //Get the starting pos
  ///  mov r12d,r8  //Get x coordinate
  ///  shr r8,32  //Get y coordinate
  ///  imul r8,r9
  ///  add r8,r12 //Offset position
  ///  add rdx,r8 //Start point
  ///
  ///  //Combine NW + NE
  ///  movdqa xmm0,[self.q]      //NWNE
  ///  movhlps xmm1,xmm0
  ///  movdqa xmm2,[self.q+16]   //SW
  ///  movhlps xmm3,xmm2
  ///  PUNPCKLBW xmm0, xmm1
  ///  PUNPCKLBW xmm2, xmm3
  ///  movq rax,xmm0
  ///  MOVHLPS xmm0, xmm0
  ///  movq rcx,xmm0
  ///  mov  [rdx],ax  //NWNE[0]
  ///  shr rax,16
  ///  mov [rdx+r9],ax  //NWNE[1]
  ///  add rdx,WidthInBytes
  ///  shr rax,16
  ///  mov [rdx+r9],ax  //NWNE[2]
  ///  add rdx,WidthInBytes
  ///  shr rax,16
  ///  mov [rdx+r9],ax  //NWNE[3]
  ///  add rdx,WidthInBytes
  ///  mov [rdx+r9],cx  //NWNE[4]
  ///  shr rcx,16
  ///  mov [rdx+r9],cx  //NWNE[5]
  ///  add rdx,WidthInBytes
  ///  shr rcx,16
  ///  mov [rdx+r9],cx  //NWNE[6]
  ///  add rdx,WidthInBytes
  ///  shr rcx,16
  ///  mov [rdx+r9],cx  //NWNE[7]
  ///  add rdx,WidthInBytes
  ///
  ///  Repeat for SW+SE   (xmm2)

  Start:= (WidthInBytes * Offset.Y) + Offset.X;
  for block:= cNW to cSE do begin
    case Block of
      cNW: {do nothing};
      cNE, cSE: Inc(Start, 1);
      cSW: Inc(Start, WidthInBytes);
    end; {case}
    for i:= 0 to 7 do begin
      DisplayBuffer[Start]:= q[Block].Line[i];
      Inc(Start, WidthInBytes);
    end;
    Dec(Start, WidthInBytes*8);
  end;
end;

function TLifeCell.ConvertPtoQ: TLifeBlock;
begin

end;


{ TLifeStateHelper }
    // each group of 4 bits:
    // |Whole 8x8|Vertical 2x8|Horizontal 8x2|Corner 2x2|
    //  bitmap:
    // |31 30 29 28.27 26 25 24|23 22 21 20.19 18 17 16|15 14 13 12.11 10  9  8| 7  6  5  4. 3  2  1  0|
    // | Northwest quadrant    | Northeast quadrant    | SouthWest quadrant    | Southeast quadrant    |
    // | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     |
    //

function TPLifeStateHelper.GetDeadBorders: TLifeState;
begin
  Result:= (self and (TLifeState.MaskBordersDead and TLifeState.MaskP));
end;

function TPLifeStateHelper.GetHibernatingBorders: TLifeState;
begin
  Result:= (self and (TLifeState.MaskBordersHiber and TLifeState.MaskP));
end;

function TQLifeStateHelper.GetDeadBorders: TLifeState;
begin
  Result:= (self and (TLifeState.MaskBordersDead and TLifeState.MaskQ));
end;

function TQLifeStateHelper.GetHibernatingBorders: TLifeState;
begin
  Result:= (self and (TLifeState.MaskBordersHiber and TLifeState.MaskQ));
end;

function TLifeStateHelper.GetStillPart: TForbidden.TStillPart;
asm
  xor eax,eax
  test ecx,ecx      //if ecx is zero, then return zero
  jz @done
  inc eax
  //will execute as bsr on pre-SSE4(A) cpus and thus be undefined on 0.
  //However in CPU's that support it lzcnt is much faster than bsr.
  lzcnt ecx,ecx     //Which bit is set?
  shl eax,cl        //Reconstruct that bit.
  @done:
end;

function TLifeStateHelper.IsDead: boolean;
begin
  Result:= (self = Dead);
end;

function TLifeStateHelper.IsHibernating: boolean;
begin
  Result:= (self and Hibernating) = Hibernating;
end;

function TLifeStateHelper.IsEDead: boolean;
const
  Mask = MaskE and MaskFullDead;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsEHibernating: boolean;
const
  Mask = MaskE and MaskFullHiber;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsNDead: boolean;
const
  Mask = MaskN and MaskFullDead;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsNHibernating: boolean;
const
  Mask = MaskN and MaskFullHiber;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsNWDead: boolean;
const
  Mask = MaskNW and MaskFullDead;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsNWHibernating: boolean;
const
  Mask = MaskNW and MaskFullHiber;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsSDead: boolean;
const
  Mask = MaskS and MaskFullDead;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsSHibernating: boolean;
const
  Mask = MaskN and MaskFullHiber;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsSEDead: boolean;
const
  Mask = MaskSE and MaskFullDead;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsSEHibernating: boolean;
const
  Mask = MaskSE and MaskFullHiber;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsWDead: boolean;
const
  Mask = MaskW and MaskFullDead;
begin
  Result:= (self and Mask) = Mask;
end;

function TLifeStateHelper.IsWHibernating: boolean;
const
  Mask = MaskW and MaskFullHiber;
begin
  Result:= (self and Mask) = Mask;
end;

procedure TLifeStateHelper.KillAll;
begin
  self:= $FFFFFFFF;
end;

procedure TLifeStateHelper.TranquilizeAll;
begin
  self:= $0f0f0f0f;
end;


{ TStillPartHelper }

      // each group of 4 bits:
      // |Whole 8x8|Vertical 2x8|Horizontal 8x2|Corner 2x2|
      //  bitmap:
      // | Northwest quadrant    | Northeast quadrant    | SouthWest quadrant    | Southeast quadrant    |
      // | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     | morgue    | hiber     |
      // |31 30 29 28.27 26 25 24|23 22 21 20.19 18 17 16|15 14 13 12.11 10  9  8| 7  6  5  4. 3  2  1  0|
//OffsetX|0   0 -1 -1| 0  0 -1 -1|0   0  1  1|0   0  1  1|0   0 -1 -1|0   0 -1 -1|0   0  1  1|0   0  1  1|
//OffsetY|0   1  0  1| 0  1  0  1|0   1  0  1|0   1  0  1|0  -1  0 -1|0  -1  0 -1|0  -1  0 -1|0  -1  0 -1|
//Quadrant 0000000000000000000000|11111111111111111111111|22222222222222222222222|33333333333333333333333|
const
  rex64bit = $48;

//Only take N, W and NW into account
function TStillPartPHelper.GetDeadOffset: TPoint;
asm
 test ecx,ecx        //Allow uop folding with jmp
 jz @ZeroResult      //On cpu's that do not support tzcnt bsf reg,0 is undefined.
 mov edx,ecx         //Save original
 db $F3
 bsf ecx,ecx         //Which bit is set?   (0..31)
 //tzcnt ecx,ecx     //Which bit is set?   (0..31)
 //Lets put some lookup tables in registers
 mov r8d, $30003000  //W: lookup for Negative xOffset
 //mov r9d, $00000000  //E: Positive xOffset
 //mov r10d,$00000000  //S: Negative yOffset
 mov r11d,$50500000  //N: Positive yOffset
 and r8,rdx          //Extract all the offsets
 //and r9,rdx
 //and r10,rdx
 and r11,rdx
 shr r8,cl           //reduce the offset to 0 or 1
 //shr r9,cl
 //shr r10,cl        //Move the yOffset into the high dword of the result.
 shr r11,cl
 neg r8              //XOffset = 1 or -1  (or 0)
 //sub r11,r10       //YOffset = 1 or -1  (or 0)
 shl r11,32          //Result.y:= YOffset
 lea rax,[r8+r11]    //Put TPoint record together.
 ret
@ZeroResult:
 xor eax,eax
end;


//Only take N, W and NW into account
function TStillPartPHelper.GetHiberOffset: TPoint;
asm
 test ecx,ecx        //Allow uop folding with jmp
 jz @ZeroResult      //On cpu's that do not support tzcnt bsf reg,0 is undefined.
 mov edx,ecx         //Save original
 db $F3
 bsf ecx,ecx         //Which bit is set?   (0..31)
 //Lets put some lookup tables in registers
 mov r8d, $03000300  //W: lookup for Negative xOffset
 //mov r9d, $00030003  //E: Positive xOffset
 //mov r10d,$00000505  //S: Negative yOffset
 mov r11d,$05050000  //N: Positive yOffset
 and r8,rdx          //Extract all the offsets
 //and r9,rdx
 //and r10,rdx
 and r11,rdx
 shr r8,cl           //reduce the offset to 0 or 1
 //shr r9,cl
 //shr r10,cl          //Move the yOffset into the high dword of the result.
 shr r11,cl
 //sub r9,r8           //XOffset = 1 or -1  (or 0)
 neg r8
 //sub r11,r10         //YOffset = 1 or -1  (or 0)
 shl r11,32          //Result.y:= YOffset
 lea rax,[r8+r11]    //Put TPoint record together.
 ret
@ZeroResult:
 xor eax,eax
end;

//Only take S, E and SE into account
function TStillPartQHelper.GetDeadOffset: TPoint;
asm
 test ecx,ecx        //Allow uop folding with jmp
 jz @ZeroResult      //On cpu's that do not support tzcnt bsf reg,0 is undefined.
 mov edx,ecx         //Save original
 db $F3
 bsf ecx,ecx         //Which bit is set?   (0..31)
 //tzcnt ecx,ecx     //Which bit is set?   (0..31)
 //Lets put some lookup tables in registers
 //mov r8d, $30003000  //W: lookup for Negative xOffset
 mov r9d, $00300030  //E: Positive xOffset
 mov r10d,$00005050  //S: Negative yOffset
 //mov r11d,$50500000  //N: Positive yOffset
 //and r8,rdx          //Extract all the offsets
 and r9,rdx
 and r10,rdx
 //and r11,rdx
 //shr r8,cl           //reduce the offset to 0 or 1
 shr r9,cl
 shr r10,cl          //Move the yOffset into the high dword of the result.
 //shr r11,cl
 //sub r9,r8           //XOffset = 1 or -1  (or 0)
 //sub r11,r10         //YOffset = 1 or -1  (or 0)
 neg r10
 shl r10,32          //Result.y:= YOffset
 lea rax,[r9+r10]    //Put TPoint record together.
 ret
@ZeroResult:
 xor eax,eax
end;

function TStillPartQHelper.GetHiberOffset: TPoint;
asm
 test ecx,ecx        //Allow uop folding with jmp
 jz @ZeroResult      //On cpu's that do not support tzcnt bsf reg,0 is undefined.
 mov edx,ecx         //Save original
 db $F3
 bsf ecx,ecx         //Which bit is set?   (0..31)
 //tzcnt ecx,ecx     //Which bit is set?   (0..31)
 //Lets put some lookup tables in registers
 //mov r8d, $30000300  //W: lookup for Negative xOffset
 mov r9d, $00030003  //E: Positive xOffset
 mov r10d,$00000505  //S: Negative yOffset
 //mov r11d,$05050000  //N: Positive yOffset
 //and r8,rdx          //Extract all the offsets
 and r9,rdx
 and r10,rdx
 //and r11,rdx
 //shr r8,cl           //reduce the offset to 0 or 1
 shr r9,cl
 shr r10,cl          //Move the yOffset into the high dword of the result.
 //shr r11,cl
 //sub r9,r8           //XOffset = 1 or -1  (or 0)
 //sub r11,r10         //YOffset = 1 or -1  (or 0)
 neg r10
 shl r10,32          //Result.y:= YOffset
 lea rax,[r9+r10]    //Put TPoint record together.
 ret
@ZeroResult:
 xor eax,eax
end;



function TStillPartHelper.GetQuadrant: TQuadrant;
asm
  //self is in ecx
  lzcnt eax,ecx
  shr eax,3      //divide by 8
end;







{ TCycleHelper }

function TCycleHelper.Flip: TCycle;
begin
  Result:= TCycle(Integer(Self) xor 1);
end;

function TCycleHelper.X: integer;
begin
  Result:= - integer(self);
end;

function TCycleHelper.Y: integer;
begin
  Result:= integer(self);
end;

end.
