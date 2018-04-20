unit Unit64;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls,
  {$ifdef Win64}
  Windows
  {$endif};

{$OPTIMIZATION FORCENOSTACKFRAME}   //does not work :-(
{$optimization forcenostackframe}

type

  { TDiBits }

  TDiBits = record
  public
    function Stride: integer; inline;
    class function Create: TDIBits; static;
  public
    bmpinfo: PBitmapInfo;
    data: PByte;
    width, height: cardinal;
  end;

  TDirection = (dN,dW,dNW,dS,dE,dSE);

  { TUnit }

const
  BytesPerUnit = 16;
  MaxBytesPerUnit = BytesPerUnit -1;
  North = -8;
  South = -North;
  West = -1;
  East = -West;
  NorthWest = -9;
  SouthEast = -NorthWest;

type

  { TDoMap }

  { TIndexHelper }

  TIndexHelper = record helper for integer
    function N: integer; inline;
    function S: integer; inline;
    function E: integer; inline;
    function W: integer; inline;
    function NW: integer; inline;
    function SE: integer; inline;
  end;

  { TDoMap2 }

  { TBorderMap }

  TBorderMap = record
  private
    Horz, Vert, Corn: integer;
  public
    procedure Activate(index, offset: integer);
    procedure ClearAll;
    property Horizontal: integer read Horz;
    property Vertical: integer read Vert;
    property Corner: integer read Corn;
  end;

  TDoMap2 = record
  private
    a: Int64;
  public
    function Next(previous: integer): integer;
    procedure Clear;
    procedure ActivateAll;
    procedure Activate(index: integer); overload;
    function Activate(index, Offset: integer): boolean; overload;
    procedure Invert(index: integer);
    procedure SubtractAdd(var Subtract, Add: TDoMap2);
  end;

  TDoMap = record
  {strict} private
    a: array[0..MaxBytesPerUnit] of byte;
    function NextTry2(previous: integer): integer;
  public
    function Next(previous: integer): integer;
    function NextSSE(Previous: integer): integer;
    function TestNext(previous: integer): integer;
    procedure ActivateAll;
    procedure Clear;
    procedure Activate(index: integer); overload;
    function Activate(index, Offset: integer): boolean; overload;
    procedure Deactivate(index: integer);
    procedure Invert(index: integer);
    procedure SubtractAdd(var Subtract, Add: TDoMap);
    function IsActive(direction: TDirection): boolean;
  end;

  PUnit = ^TUnit;
  TUnit = record
    procedure Display(const BitmapStart: pointer; const LineOffset: integer);
    procedure SetPixel(x,y: integer; value: boolean = true);
    case integer of
      1: (b: array[0..MaxBytesPerUnit] of byte);
      2: (w: array[0..(BytesPerUnit div 2)-1] of word);
      3: (i: array[0..(BytesPerUnit div 4)-1] of integer);
      4: (q: array[0..(BytesPerUnit div 8)-1] of int64);
  end;

const
  UnitsPerBlock = 16*8;


/// Blocks are organized like so:
/// TUnit is the lowest level, rows of 16 bits are stored like so:
/// 0123456789ABCDEF     row  0  LSByte    LSB = Left, MSB = Right
/// 0123456789ABCDEF     row  1
/// 0123456789ABCDEF     row  2
/// 0123456789ABCDEF     row  3
/// 0123456789ABCDEF     row  4
/// 0123456789ABCDEF     row  5
/// 0123456789ABCDEF     row  6
/// 0123456789ABCDEF     row  7  MSByte
/// Thus 16 bytes are stored in a TUnit.
/// A TUnit fits in a XMM register and is always processed as a whole.
///
/// 16 * 8  = 128 Units make up a block like so:
/// 00 01 02 03 04 05 06 07     row 0
/// 08 09 0A 0B 0C 0D 0E 0F     row 1
/// 10 11 12 13 14 15 16 17     row 2
/// 18 19 1A 1B 1C 1D 1E 1F     row 3
/// 20 21 22 23 24 25 26 27     row 4
/// 28 29 2A 2B 2C 2D 2E 2F     row 5
/// 30 31 32 33 34 35 36 37     row 6
/// 38 39 3A 3B 3C 3D 3E 3F     row 7
/// 40 41 42 43 44 45 46 47     row 8
/// 48 49 4A 4B 4C 4D 4E 4F     row 9
/// 50 51 52 53 54 55 56 57     row A
/// 58 59 5A 5B 5C 5D 5E 5F     row B
/// 60 61 62 63 64 65 66 67     row C
/// 68 69 6A 6B 6C 6D 6E 6F     row D
/// 70 71 72 73 74 75 76 77     row E
/// 78 79 7A 7B 7C 7D 7E 7F     row F
/// Every unit is a 16W x 8H bitmap





type

    //bit    7----6----5----4----3----2----1----0
    //      P2+  P2+  P2+ alive P2+ alive  P1+ alive
    //      corn all   EW   EW   NS  NS   all  all

  //         b0,       b1,        b2,       .....
  TStatus = (AllAlive, AllActive, NS_Alive, NS_P2Plus, EW_Alive, EW_P2Plus, AllP2Plus, Corner_P2Plus);
  TGenerateStatus = set of TStatus;


  { TCellBlock }
  TPlaceHolder = record
  end;

  TUnitStorage = array[0..UnitsPerBlock-1] of TUnit;


  PCellBlock = ^TCellBlock;
  TCellBlock = record
  private
    class var FEmpty: TUnit;
    class var pstate: boolean;
    class constructor Init;
  public
    procedure DisplayQ(const DrawSurface: TDIBits);
    procedure DisplayP(const DrawSurface: TDIBits);
    function GeneratePtoQ(bN,bW,bNW: PCellBlock): TGenerateStatus;
    function GenerateQtoP(bS,bE,bSE: PCellBlock): TGenerateStatus;
    procedure SetPixel(x,y: integer; state: boolean = true);
    constructor Create(Activate: boolean);
    function CellCount(Generation: integer): integer;
    //procedure Difference(Generation: integer; var Diff: TUnitStorage);
    //function DifferenceCount(Generation: integer): integer;
  public
    p: array[0..UnitsPerBlock-1] of TUnit;
    q: array[0..UnitsPerBlock-1] of TUnit;   //must be continuous
    AddSubtract: TPlaceHolder;
    Subtractions: TDoMap;                    //Subtractions are all the units that should die
    Additions: TDoMap;                       //Additions are all the units that should come alive
    Border: TBorderMap;
    ToDoList: TDoMap;                        //All the blocks that are to be processed.
    //First we do the subtractions and then we do the additions, to make sure that in case of conflict the
    //additions win out.

  end;

  { CellBlockHelper }

  CellBlockHelper = record helper for PCellBlock
    class function New: PCellBlock; static;
    procedure Free;
  end;

  //TNode = class;
  //TLeaf = class
  //public
  //  procedure GeneratePtoQ; virtual;
  //  procedure GenerateQtoP; virtual;
  //  procedure DisplayQ(const DrawSurface: TDIBits; const Bounds: TRect); virtual;
  //  procedure DisplayP(const DrawSurface: TDIBits; const Bounds: TRect); virtual;
  //  constructor Create(x,y: integer; Parent: TNode);
  //  procedure Translate(var x,y: integer); inline;
  //  procedure SetPixel(x,y: integer; state: boolean = true); virtual;
  //  function GetPixel(x,y: integer): boolean; virtual;
  //private
  //  function goNorth(index: integer): PCellBlock;
  //  function goWest(index: integer): PCellBlock;
  //  function goNorthWest(index: integer): PCellBlock;
  //  function goSouth(index: integer): PCellBlock;
  //  function goEast(index: integer): PCellBlock;
  //  function goSouthEast(index: integer): PCellBlock;
  //end;

  //TNode is a node of a 64-ary tree.

  //TNode = class
  //public
  //  procedure GeneratePtoQ;
  //  procedure GenerateQtoP;
  //  procedure DisplayQ(const DrawSurface: TDIBits);
  //  procedure DisplayP(const DrawSurface: TDIBits);
  //  constructor Create(X,Y: integer; Parent: TNode); overload;
  //  procedure SetPixel(x,y: integer; state: boolean = true);
  //  function GetPixel(x,y: integer): boolean;
  //private
  //  Parent: TNode;
  //  ToDoList: TDoMap2;
  //  AddSubtract: TPlaceHolder;
  //  Subtractions: TDoMap2;
  //  Additions: TDoMap2;
  //  Border: TDoMap2;
  //  fN,fW,fNW,fS,fE,fSE: TNode;            //Neighboring nodes, can be nil.
  //  Level: integer; {level 0 = lowest sublevel, where FSubnodes holds PCellBlock}
  //  X,Y: integer;
  //  FSubNodes: array[0..63] of pointer;    //can be either PCellBlock or TNodes.
  //  function GetBlock(index: integer): PCellBlock;
  //  function GetIsLeaf: boolean; inline;
  //  function GetIsRoot: boolean; inline;
  //  function GetSubNode(index: integer): TNode; inline; overload;
  //public
  //  function GetSubNode(x,y: integer): TNode; overload;
  //  function Block(index: integer): PCellBlock; overload;
  //  function Block(x,y: integer): PCellBlock; overload;
  //
  //  property IsRoot: boolean read GetIsRoot;
  //  property IsLeaf: boolean read GetIsLeaf;
  //  property Pixel[x,y: integer]: boolean read GetPixel write SetPixel;
  //public
  //  FStaticCount: int64;
  //  FOscillatingCount: int64;
  //  FChoaticCount: int64;
  //  FDyingcount: int64;
  //end;


  //TUniverse = class(TNode)
  //private
  //  FGeneration: Int64;
  //public
  //  property GenCount: int64 read FGeneration;
  //end;



  { TForm64 }

  TForm64 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    Image1: TImage;
    Timer1: TTimer;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure PaintBox1Click(Sender: TObject);
  private
    FCellNode: TCellBlock;
    FDrawSurfaceP: TDIBits;
    FDrawSurfaceQ: TDIBits;
    FEmpty: TCellBlock;
    Generation: integer;
    procedure Draw;
    procedure Generate;
  public
    { public declarations }
  end;



var
  Form64: TForm64;

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}
uses
  HiResStopwatch in 'c:\borland\johan\HiResStopWatch\HiResStopWatch\HiResStopWatch.pas'
{$ifdef FPC}

  ,AVXGenerate;

{ TBorderMap }

procedure TBorderMap.Activate(index, offset: integer);
begin
  //The offset is either positive (when south facing) or negative (when north facing).
  //The offset is never zero.
  case offset of
    North: if index in [0..7] then Horz:= Horz or (1 shl index);
    West: if ((index and 7) = 0) then Vert:= Vert or (1 shl (index shr 3));
    South: if index in [120..127] then Horz:= Horz or (1 shl (index - 120));
    East: if ((index and 7) = 7) then Vert:= Vert or (1 shl ((index -7) shr 3));
    NorthWest: if index = 0 then Corn:= 1
      else if (index in [1..7]) then Horz:= Horz or (1 shl (index -1))
      else if ((index and 7) = 0) then Vert:= Vert or (1 shl ((index shr 3)-1));
    SouthEast: if index = 127 then Corn:= 1
      else if (index in [120..126]) then Horz:= Horz or (1 shl (index - 119))
      else if ((index and 7) = 7) then Vert:= Vert or (1 shl ((index shr 3)+1));
  end;
end;

procedure TBorderMap.ClearAll;
begin
  Horz:= 0;
  Vert:= 0;
  Corn:= 0;
end;


{ TNode }

//procedure TNode.GeneratePtoQ;
//var
//  i: integer;
//  P,N,W,NW: PCellBlock;
//  NIndex, WIndex, NWIndex: integer;
//  status: TGenerateStatus;
//begin
//  FillChar(AddSubtract, SizeOf(TDoMap2)*2, 0);
//  i:= ToDoList.Next(-1);
//  while (i < 64) do begin
//
//    N:= goNorth(i);
//    W:= goWest(i);
//    NW:= goNorthWest(i);
//    P:= Block(i);
//    status:= P.GeneratePtoQ(N,W,NW);
//    //All of the cell.
//    //if (AllActive in status) then begin
//    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
//      Additions.Activate(i);
//      if (NS_P2Plus in status) then begin
//        Additions.Activate(i,North);
//        Border.Activate(i, North);
//      end;
//      if (EW_P2Plus in status) then begin
//        Additions.Activate(i,West);
//        Border.Activate(i,West);
//        if (Corner_P2Plus in status) then begin
//          Additions.Activate(i,NorthWest);
//          Border.Activate(i,NorthWest);
//        end;
//      end;
//    end else Subtractions.Activate(i);
//    i:= ToDoList.Next(i);
//  end; {while/for}
//  ToDoList.SubtractAdd(Subtractions, Additions);
//end;
//
//procedure TNode.GenerateQtoP;
//var
//  i: integer;
//  Q,S,E,SE: PCellBlock;
//  Sindex, EIndex, SEIndex: integer;
//  status: TGenerateStatus;
//begin
//  FillChar(AddSubtract, SizeOf(TDoMap2)*2, 0);
//  //for i:= 127 downto 0 do begin
//  i:= ToDoList.Next(-1);
//  while (i < 64) do begin
//    Q:= Block(i);
//    S:= goSouth(i);
//    E:= goEast(i);
//    SE:= goSouthEast(i);
//    status:= TGenerateStatus(Q.GenerateQtoP(S,E,SE));
//    //All of the cell.
//    //if (AllActive in status) then begin
//    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
//      Additions.Activate(i);
//      if (NS_P2Plus in status) then Additions.Activate(i,South);
//      if (EW_P2Plus in status) then begin
//        Additions.Activate(i,East);
//        if (Corner_P2Plus in status) then Additions.Activate(i,SouthEast);
//      end;
//    end else Subtractions.Activate(i);
//    i:= ToDoList.Next(i);
//  end; {while/for}
//  ToDoList.SubtractAdd(Subtractions, Additions);
//end;
//
//procedure TNode.DisplayQ(const DrawSurface: TDIBits);
//begin
//  //
//end;
//
//procedure TNode.DisplayP(const DrawSurface: TDIBits);
//begin
//  //
//end;
//
//constructor TNode.Create(X,Y: integer; Parent: TNode);
//begin
//  inherited Create;
//  //Parent: TNode;
//  //ToDoList: TDoMap2;
//  //AddSubtract: TPlaceHolder;
//  //Subtractions: TDoMap2;
//  //Additions: TDoMap2;
//  //Border: TDoMap2;
//  //fN,fW,fNW,fS,fE,fSE: TNode;            //Neighboring nodes, can be nil.
//  //X,Y: integer;
//  //FSubNodes: array[0..63] of pointer;    //can be either PCellBlock or TNodes.
//  Self.X:= X;
//  Self.Y:= Y;
//  if Assigned(Parent) then begin
//    Self.Parent:= Parent;
//    //Check the neighbors and link them up.
//    fN:= Parent.GetSubNode(X, Y-1);
//    fW:= Parent.GetSubNode(X-1,Y);
//    fNW:= Parent.GetSubNode(X-1,Y-1);
//    fS:= Parent.GetSubNode(X,Y+1);
//    fE:= Parent.GetSubNode(X+1,Y);
//    fSE:= Parent.GetSubNode(X+1,Y+1);
//    if Assigned(fN) then fN.fS:= self;
//    if Assigned(fW) then fW.fE:= self;
//    if Assigned(fNW) then fNW.fSE:= self;
//    if Assigned(fS) then fS.fN:= self;
//    if Assigned(fE) then fE.fW:= self;
//    if Assigned(fSE) then fSE.fNW:= self;
//  end;
//end;
//
//procedure TNode.Translate(var x, y: integer);
//begin
//  x:= x - Self.x;
//  y:= y - Self.y;
//end;
//
//function TNode.GetPixel(x, y: integer): boolean;
//begin
//
//end;
//
//{$ifndef TNodeIsClass}
//procedure TNode.Init;
//begin
//  FillChar(Self, SizeOf(Self), #0);
//  ToDoList.ActivateAll;
//  self.IsLeaf:= true;
//end;
//{$endif}
//
//function TNode.goNorth(index: integer): PCellBlock;
//begin
//  if index in [0..7] then begin
//    Result:= fN.Block(index + North + 64);
//  end else Result:= Block(index + North);
//end;
//
//function TNode.goWest(index: integer): PCellBlock;
//begin
//  if (index and 7) = 0 then begin
//    Result:= fW.Block(index + West + 8)
//  end else Result:= Block(index + West);
//end;
//
//function TNode.goNorthWest(index: integer): PCellBlock;
//begin
//  if index = 0 then Result:= fNW.Block(63)
//  else if (index in [1..7]) then Result:= fN.Block(index + NorthWest + 64)
//  else if ((index and 7)=0) then Result:= fW.Block(index + NorthWest + 8)
//  else Result:= Block(index + NorthWest);
//end;
//
//function TNode.goSouth(index: integer): PCellBlock;
//begin
//  if index in [56..63] then begin
//    Result:= fS.Block(index + South - 64);
//  end else Result:= Block(index + South);
//end;
//
//function TNode.goEast(index: integer): PCellBlock;
//begin
//  if (index and 7) = 7 then begin
//    Result:= fE.Block(index + East - 8)
//  end else Result:= Block(index + East)
//end;
//
//function TNode.goSouthEast(index: integer): PCellBlock;
//begin
//  if index = 63 then Result:= fSE.Block(0)
//  else if (index in [56..63]) then Result:= fS.Block(index + SouthEast - 64)
//  else if ((index and 7)=7) then Result:= fE.Block(index + SouthEast - 8)
//  else Result:= Block(index + SouthEast);
//end;
//
//function TNode.GetSubNode(index: integer): TNode;
//begin
//  Result:= TNode(FSubNodes[index]);
//end;
//
//function TNode.GetSubNode(x, y: integer): TNode;
//begin
//  Result:= TNode(FSubNodes[(x and 7)*(y and 7)]);
//end;
//
//function TNode.GetBlock(index: integer): PCellBlock;
//begin
//  Result:= PCellBlock(FSubNodes[index]);
//end;
//
//function TNode.GetIsLeaf: boolean;
//begin
//  Result:= level = 0;
//end;
//
//function TNode.GetIsRoot: boolean;
//begin
//  Result:= (Parent = nil);
//end;
//
//procedure TNode.SetPixel(x, y: integer; state: boolean = true);
//var
//  NodeIndex: array of TPoint;
//  i: integer;
//  Node: TNode;
//  Block: PCellBlock;
//begin
//  SetLength(NodeIndex, Level);
//  NodeIndex[0].x:= x mod 128;
//  NodeIndex[0].y:= y mod 128;
//  x:= x div 128;
//  y:= y div 128;
//  for i:= 1 to Level do begin
//    NodeIndex[i].x:= x mod 8;
//    NodeIndex[i].y:= y mod 8;
//    x:= x div 8;
//    y:= y div 8;
//  end;
//  Node:= Self;
//  for i:= level downto 1 do begin
//    Node:= Node.GetSubNode(NodeIndex[i].x,NodeIndex[i].y);
//  end;
//  Block:= Node.Block(NodeIndex[0].x, NodeIndex[0].y);
//  Block.SetPixel(x,y, state);
//end;
//
//
//function TNode.Block(index: integer): PCellBlock;
//begin
//
//end;
//
//function TNode.Block(x, y: integer): PCellBlock;
//begin
//
//end;

{ TDoMap2 }

function TDoMap2.Next(previous: integer): integer; assembler; nostackframe;
asm
  //register usage
  //RCX: self
  //RDX: previous
  mov eax,64            //assume failure
  cmp edx,63            //are we all done?
  jae @done             //yes, done
  mov rax,[rcx]         //get the map
  lea ecx,[edx+1]       //put the bits it shift out in cl
  shr rax,cl            //shift out the bits we've already looked at
  tzcnt rax,rax         //get the next set bit
  add eax,ecx           //add the previous
  @done:
end;

procedure TDoMap2.Clear;
begin
  a:= 0;
end;

procedure TDoMap2.ActivateAll;
begin
  a:= -1;
end;

procedure TDoMap2.Activate(index: integer); assembler; nostackframe;
asm
  bts [rcx],rdx
end;

function TDoMap2.Activate(index, Offset: integer): boolean; assembler; nostackframe;
asm
//register usage:
//RCX : self
//EDX: index
//r8d: offset
  test r8d,r8d          //test if r8 is positive or negative
  lea r10d,[edx+r8d]    //store the desired result if <0 or >127 then problem.
  mov eax,0             //assume failure
  jns @positive
@negative:
  and edx,7             //if zero then potential problem
  shr r8d,1             //is offset odd?
  sbb edx,-1            //if ZF=1 then we have a problem
  bt r10d,31            //if CF=1 then we have a problem, is r10d negative?
  jbe @done
  bts [rcx],r10
  inc eax               //signal success
  ret
@positive:
  or edx,not(7)         //-1 if eax has 7 at the end.
  shr r8d,1             //is offset odd?
  adc edx,0             //ZF=1 if odd offset and index has a 7
  bt r10d,6             //CF=1 if overflow, is r10d > 64?
  jbe @done
  bts [rcx],r10
  inc eax               //signal success
@done:
  rep ret
end;

procedure TDoMap2.Invert(index: integer); assembler; nostackframe;
asm
  btc [rcx],rdx
end;

procedure TDoMap2.SubtractAdd(var Subtract, Add: TDoMap2); assembler; nostackframe;
asm
  mov r10,[rdx]     //get subtract
  mov rax,[rcx]     //get self
  xor r11,r11       //r11 = 0
  not r10           //not subtract
  and rax,r10       //self:= self and not subtract
  or rax,[r8]       //self:= self or add
  mov [rdx],r11     //clear subtract
  mov [r8],r11      //clear add
  mov [rcx],rax     //save self
end;

{$else}
  ;
 {$L 'C:\Users\Johan\Documents\Embarcadero\Studio\Projects\Life64\Lazarus\lib\x86_64-win64\AVXGenerate.o'}

function GenerateQtoP_AVX(main, N,W,NW: pointer): byte;
  external name 'AVXGENERATE_$$_GENERATEQTOP_AVX$POINTER$POINTER$POINTER$POINTER$$BYTE';
function GeneratePtoQ_AVX(main, S,E,SE: pointer): byte;
  external name 'AVXGENERATE_$$_GENERATEPTOQ_AVX$POINTER$POINTER$POINTER$POINTER$$BYTE';
{$endif}


{ TForm64 }


{ TIndexHelper }

function TIndexHelper.N: integer;
begin
  Result:= self + North;
end;

function TIndexHelper.S: integer;
begin
  Result:= self + South;
end;

function TIndexHelper.E: integer;
begin
  Result:= self + East;
end;

function TIndexHelper.W: integer;
begin
  Result:= self + West;
end;

function TIndexHelper.NW: integer;
begin
  Result:= self + NorthWest;
end;

function TIndexHelper.SE: integer;
begin
  Result:= self + SouthEast;
end;

function RDTSC: int64;
{$IFDEF CPUX64}
asm
  {$IFDEF AllowOutOfOrder}
  rdtsc
  {$ELSE}
    {$IFDEF FPC}
    db $0F,$01,$F9           //rdtscp
    {$ELSE}
    rdtscp       //rdstcp is read serialized, it will not execute too early.
    {$ENDIF}
    //also ensure it does not execute too late
    mov r8,rdx   //rdtscp changes rdx and rax, force dependency chain on rdx
    xor r8,rbx   //push rbx, do not allow push rbx to execute OoO
    xor rbx,rdx  //rbx=r8
    xor rbx,r8   //rbx = 0
    push rdx
    push rax
    mov rax,rbx  //rax = 0, but in a way that excludes OoO execution.
    cpuid
    pop rax
    pop rdx
    mov rbx,r8
    xor rbx,rdx  //restore rbx
  {$ENDIF AllowOutOfOrder}
  shl rdx,32
  or rax,rdx
  {$else}
  {$ENDIF}
{$IFDEF CPUX86}
asm
  {$IFNDEF AllowOutOfOrder}
    {$IFDEF ForceRDTSCP}
      db $0F, $01, $F9   //rdtscp
      //rdtscp       //rdstcp is read serialized, it will not execute too early.
      //also ensure it does not execute too late
      mov ecx,edx   //rdtscp changes rdx and rax, force dependency chain on rdx
      xor ecx,ebx   //push ebx, do not allow push ebx to execute OoO
      xor ebx,edx   //ebx=ecx
      xor ebx,ecx   //ebx = 0
      push edx
      push ecx
      push eax
      mov eax,ebx  //rax = 0, but in a way that excludes OoO execution.
      cpuid
      pop eax
      pop ecx
      pop edx
      mov ebx,ecx
      xor ebx,edx  //restore rbx
      {$else}
      xor eax,eax
      push ebx
      cpuid         // On x86 we can't assume the existance of RDTSP
      rdtsc
      pop ebx       // so use CPUID to serialize
      {$ENDIF}
      {$ELSE}
      rdtsc
  //error !
{$ENDIF}
{$endif}
end;


//todo: extend for the full unit.
function ReverseBitsInAllBytes_Reference(input: int64):int64;
const
  ReverseLowNibbles: array [0..15] of byte = ($00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0);
  ReverseHighNibbles:array [0..15] of byte = ($00,$08,$04,$0C,$02,$0A,$06,$0E,$01,$09,$05,$0D,$03,$0B,$07,$0F);
  NibbleMask:        array [0..15] of byte = ($0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F);
asm
  //register use:
  //RCX = input
  //RAX = output
  vmovdqa xmm3,[NibbleMask+rip]
  vmovdqa xmm1,[ReverseLowNibbles+rip]
  vmovdqa xmm2,[ReverseHighNibbles+rip]
  movq xmm0,rcx                     //storage for low nibbles
  movq xmm7,rcx                     //storage for high nibbles
  vpand xmm0,xmm0,xmm3              //keep the low nibbles
  vpandn xmm7,xmm3,xmm7             //keep the high nibbles
  vpsrlw xmm7,xmm7,4                //move the high nibbles in range for the mask
  vpshufb xmm0,xmm1,xmm0            //lookup the low nibbles
  vpshufb xmm7,xmm2,xmm7            //lookup the high nibbles
  vpor xmm0,xmm0,xmm7               //combine the two
  movq rax,xmm0                     //return the result.
end;

const
  ReverseLowNibbles: array [0..15] of byte = ($00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0);
  ReverseHighNibbles:array [0..15] of byte = ($00,$08,$04,$0C,$02,$0A,$06,$0E,$01,$09,$05,$0D,$03,$0B,$07,$0F);
  NibbleMask:        array [0..15] of byte = ($0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F);

procedure ReverseBitsInAllBytesSSE(ReverseMe: pointer);
asm
  //register use:
  //RCX = input
  //RAX = output
  vmovdqu xmm4,[NibbleMask+rip]
  //vmovdqu xmm0,[ReverseMe]
  vmovdqu xmm1,[ReverseMe]                     //low nibbles
  vmovdqu xmm2,[ReverseLowNibbles+rip]
  vmovdqu xmm3,[ReverseHighNibbles+rip]

  vpand xmm0,xmm4,xmm1              //keep the low nibbles
  vpandn xmm5,xmm4,xmm1             //keep the high nibbles
  vpsrlw xmm5,xmm5,4                //move the high nibbles in range for the mask
  vpshufb xmm0,xmm2,xmm0            //lookup the low nibbles
  vpshufb xmm5,xmm3,xmm5            //lookup the high nibbles
  //psllw xmm2,4                    //shift the high nibbles up
  vpor xmm0,xmm0,xmm5               //combine the two
  //movdqu [ReverseMe],xmm0
end;

procedure ReverseRowsIn8x8Grid(ReverseMe: pointer);
const
  ReverseRows: array [0..15] of byte = ($07,$06,$05,$04,$03,$02,$01,$00,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F);
asm
  //RCX = reverseme.
  mov rax,[rcx]
  movq xmm0,rax
  vpshufb xmm0,xmm0,[ReverseRows+rip]
  movq rax,xmm0
  mov [rcx],rax
end;

procedure ReverseRows(ReverseMe: pointer);
const
  ReverseRows: array [0..15] of byte = ($0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01,$00);
asm
  movdqu xmm0,[rcx]
  vpshufb xmm0,xmm0,[ReverseRows+rip]
  movdqu [rcx],xmm0
end;






procedure DisplayBitmap(const Canvas: TCanvas; const Rect: TRect; const Bitmap: TDIBits);
var
  //color: PRGBQUAD;
  setter: PByte;
  ScanlineWidth: Integer;
  i: Integer;
  //y,h: integer;
begin
  ScanlineWidth:= Bitmap.width div 8;
  if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);

  //GetMem(data_buffer, ScanlineWidth * height);

  { Set checkerboard pattern }
  setter:= Bitmap.data;
  for i:= 1 to ((ScanlineWidth * Bitmap.height) div 4) do begin
    if odd((i*4) div ScanLineWidth)  then PCardinal(setter)^:= $55555555
    else PCardinal(setter)^:= $AAAAAAAA;

    Inc(setter, SizeOf(Cardinal));
  end;
//  function SetDIBitsToDevice(DC: HDC; DestX, DestY: Integer; Width, Height: DWORD;
//  SrcX, SrcY: Integer; nStartScan, NumScans: UINT; Bits: Pointer;
//  var BitsInfo: TBitmapInfo; Usage: UINT): Integer; stdcall;

  //SetDIBitstoDevice(Canvas.Handle, 0, 0, width, height, 0, 0, 0, height, data_buffer, bmpinfo^, DIB_RGB_COLORS);
  //Banding
  //h:= Bitmap.height div 4;
  //y:= 0;

  StretchDIBits(Form64.PaintBox1.Canvas.Handle, 0,0,128*4-1,128*4-1,0,0,127,127, Bitmap.data, Bitmap.bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
end;



{ TDoMap }

function TDoMap.Next(previous: integer): integer;
asm
  //register use
  //RCX: self
  //RDX: previous
  //return value in RAX
  lea eax,[edx+1]   //increase the counter
  xor edx,edx       //prepare the bit mask
  cmp eax,128       //are we at 128?
  jae @done
  mov r8d,64
  mov r10,[rcx+8]   //speculatively load part 2, we may need it if part 1 is zero.
  //tzcnt r11,r10   //count the bits in part 2 in case part 1 is zero.
  rep bsf r11,r10
  lea r11,[r11+r8]  //add 64 because this *is* part 2
  bts rdx,rax       //Set the selected bit
  neg rdx           //Mask out all bits lower than the selected bit
  cmp eax,r8d       //are we in part 1 or 2?
  cmovl r10,[rcx]   //in part 1, load part 1
  mov ecx,0         //in part 1: start counting from 0
  cmovge ecx,r8d    //in part 2: stat counting from 64
  and r10,rdx       //mask off the bits we don't need
  //tzcnt rax,r10   //count the bits in r9
  rep bsf rdx,r10
  cmovc rdx,r11     //if the input is zero, use the count from part 2 instead.
  lea rax,[rdx+rcx] //add 64 if we were in part 2.
@done:
  //There are 4 options.
  //1: in part 1 and remaining bits not zero: mask and count p1 and return count + 0
  //2: in part 2 and remaining bits not zero: mask and count p2 and return count + 64
  //3: in part 1 and remaining bits zero: return count2 + 64 + 0
  //4: in part 2 and remaining bits zero: return count2 + 64 + 64 = overflow.
end;

function TDoMap.NextTry2(previous: integer): integer;
asm
  //register use:
  //RCX : self
  //edx : previous
  xor r8,r8         //Prepare the mask (Part 1/3)
  lea eax,[edx+1]
  lea edx,[edx+1]   //Increase previous
  bts r8,rax        //Mask out the bits we've covered already (part 2/3)
  neg r8            //The neg makes all those bits reset and all higher bits set. (3/3)
  shl dl,1          //CF on 128 or higher, SF on 64 or higher.
  jc @Done
  js @InPart2
@InPart1:
  tzcnt r9,[rcx+8]  //Count part 2 (in case part 1 is all zero's)
  and r8,[rcx]      //Mask off the part we've covered already
  lea r9,[r9+64]    //Add 64 because r9 is part 2.
  tzcnt rax,r8      //get the next bit.
  cmovc rax,r9      //if we had all zero's, then use the count in part 2 instead.
  {$ifndef nostackframe_works}
  lea    rsp,[rsp+8]
  {$endif}
  ret
@InPart2:
  and r8,[rcx+8]
  tzcnt rax,r8
  add eax,64
@Done:
  {$ifndef nostackframe_works}
  lea    rsp,[rsp+8]
  {$endif}
  rep ret
end;




function TDoMap.NextSSE(Previous: integer): integer;
asm
  inc dl
  js @Done
  mov r8,[rcx]
  mov r9,[rcx+8]
  mov cl,dl
  mov r11,-1            //assume the mask for part 2 is all ones
  xor r10,r10           //assume the mask for part 1 is zero
  mov rax,r11           //another all ones mask
  shl r11,cl            //shift mask for part 2
  cmp dl,64             //are we in part 1 or part 2
  cmovb r10,rax         //if part 1 is active, make that mask not zero.
  cmovb r11,rax         //if part 1 is active, undo the shift for part 2
  shl r10,cl            //shift mask for part 1
  and r8,r10            //compute the active bitmap for part 1
  cmovnz r9,rax         //if part 1 is non zero, we don't need to count part 2, make the value all ones
  and r9,r11            //compute part 2
  tzcnt r8,r8           //count part 1
  tzcnt r9,r9           //count part 1
  lea rax,[r8+r9]     //add the two counts
  jmp @DoubleDone
@Done:
  movzx eax,dl
@Doubledone:
end;

function TDoMap.TestNext(previous: integer): integer;
asm
  //register use:
  //rcx : PDoMap
  //rdx : index
  mov eax,edx
@loop:
  inc eax
  cmp eax,128
  jae @done
  bt [rcx],eax
  jnc @loop
@done:
end;

procedure TDoMap.ActivateAll;
asm
  mov qword ptr [rcx],-1
  mov qword ptr [rcx+8],-1
end;



procedure TDoMap.Clear;
asm
  mov qword ptr [rcx],0
  mov qword ptr [rcx+8],0
end;

procedure TDoMap.SubtractAdd(var Subtract, Add: TDoMap);
asm
  //movdqu xmm0,[Result]
  vmovdqu xmm1,[Subtract]
  //movdqu xmm2,[Add]
  vpandn xmm0,xmm1,[rcx]        //a:= not Subtractions and result
  vpor xmm0,xmm0,[Add]          //a:= a or additions
  vpxor xmm1,xmm1,xmm1          //xmm1 = 0
  vmovdqu [rcx],xmm0             //save the result
  vmovdqu [Subtract],xmm1        //Clear the subtractions
  vmovdqu [Add],xmm1             //Clear the additions
end;

function TDoMap.IsActive(direction: TDirection): boolean;
var
  Active: byte;
begin
  case Direction of
    dN: Result:= a[0] <> 0;
    dW: begin
      Active:= a[0] or a[1] or a[2] or a[3] or a[4] or a[5] or a[6] or a[7] or
               a[8] or a[9] or a[10] or a[11] or a[12] or a[13] or a[14] or a[15];
      Result:= ((Active and $1) <> 0);
    end;
    dNW:Result:= ((a[0] and $01) <> 0);
    dS: Result:= a[15] <> 0;
    dE: begin
      Active:= a[0] or a[1] or a[2] or a[3] or a[4] or a[5] or a[6] or a[7] or
               a[8] or a[9] or a[10] or a[11] or a[12] or a[13] or a[14] or a[15];
      Result:= ((Active and $80) <> 0);
    end;
    dSE: Result:= ((a[15] and $80) <> 0);
  end;
end;

procedure TDoMap.Activate(index: integer); assembler; nostackframe;
asm
  test index,index
  js @Done
  bts [rcx],rdx
  @Done:
end;

function TDoMap.Activate(index, Offset: integer): boolean; assembler; nostackframe;
asm
  //mov eax,edx
  test r8d,r8d
  lea r10d,[edx+r8d]    //store the desired result if <0 or >127 then problem.
  mov eax,0             //assume failure
  jns @positive
@negative:
  and edx,7             //if zero then potential problem
  shr r8d,1             //is offset odd?
  sbb edx,-1            //if ZF=1 then we have a problem
  bt r10d,31            //if CF=1 then we have a problem
  jbe @done
  bts [rcx],r10
  inc eax               //signal success
  ret
@positive:
  or edx,not(7)         //-1 if eax has 7 at the end.
  shr r8d,1             //is offset odd?
  adc edx,0             //ZF=1 if odd offset and index has a 7
  bt r10d,7             //CF=1 if overflow
  jbe @done
  bts [rcx],r10
  inc eax               //signal success
@done:
  rep ret
end;

procedure TDoMap.Deactivate(index: integer);
asm
  btr [rcx],rdx
end;

procedure TDoMap.Invert(index: integer);
asm
  btc [rcx],rdx
end;



{ TCellBlock }

{$pointermath on}

class constructor TCellBlock.Init;
begin
  FillChar(FEmpty, SizeOf(FEmpty), #0);
  PState:= true; //start with pstate.
end;

procedure TCellBlock.DisplayQ(const DrawSurface: TDIBits);
var
  i: integer;
  x,y: integer;
  Offset: integer;
begin
  for i:= 0 to 127 do begin
    x:= (i and 7) * 2;
    y:= (i shr 3) * 128;
    Offset:= x+y;
    Self.q[i].Display(pointer(NativeInt(DrawSurface.data)+Offset), 128 div 8);
  end;
end;

procedure TCellBlock.DisplayP(const DrawSurface: TDIBits);
var
  i: integer;
  x,y,offset: integer;
begin
  for i:= 0 to 127 do begin
    x:= (i and 7) * 2;
    y:= (i shr 3) * 128;
    offset:= x+y;
    Self.p[i].Display(pointer(NativeInt(DrawSurface.data)+Offset), 128 div 8);
  end;
end;


procedure TForm64.Generate;
begin
  if Odd(Generation) then FCellNode.GenerateQtoP(@FEmpty, @FEmpty, @FEmpty)
  else FCellNode.GeneratePtoQ(@FEmpty,@FEmpty,@FEmpty);
  Inc(Generation);
end;

procedure TForm64.Draw;
var
  x,y: integer;
begin
  case Odd(Generation) of
    false: begin
      FCellNode.DisplayP(FDrawSurfaceP);
      //function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
      //SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
      //Usage: UINT; Rop: DWORD): Integer; stdcall;
      StretchDIBits(Form64.PaintBox1.Canvas.Handle, 4,4,128*4,128*4,0,0,128,128, FDrawSurfaceP.data, FDrawSurfaceP.bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
      //BitBlt();


      //for x:= 0 to 7 do begin
      //  PaintBox1.Canvas.Line(x*16*4+4,4,x*16*4+4,128*4+4);
      //end;
      //for y:= 0 to 15 do begin
      //  PaintBox1.Canvas.Line(4,y*8*4+4,128*4+4,y*8*4+4);
      //end;
    end;
    true: begin
      FCellNode.DisplayQ(FDrawSurfaceQ);
      StretchDIBits(Form64.PaintBox1.Canvas.Handle, 0,0,128*4,128*4,0,0,128,128, FDrawSurfaceQ.data, FDrawSurfaceQ.bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
      //for x:= 0 to 7 do begin
      //  PaintBox1.Canvas.Line(x*16*4,0,x*16*4,128*4);
      //end;
      //for y:= 0 to 15 do begin
      //  PaintBox1.Canvas.Line(0,y*8*4,128*4,y*8*4);
      //end;
    end;

  end;
end;

function TCellBlock.GeneratePtoQ(bN, bW, bNW: PCellBlock): TGenerateStatus;
var
  i: integer;
  N,W,NW: PUnit;
  NIndex, WIndex, NWIndex: integer;
  status: TGenerateStatus;
begin
  FillChar(AddSubtract, SizeOf(TDoMap)*2, 0);
  i:= ToDoList.Next(-1);
  while (i < 128) do begin
    NIndex:= i - 8;
    WIndex:= i - 1;
    NWIndex:= i - 9;
    N:= @p[NIndex];
    W:= @p[WIndex];
    NW:= @p[NWIndex];
    if i < 8 then begin
      N:= @bN.p[Nindex + 128];
      NW:= @bNW.p[NWIndex + 128];
    end;
    if not(boolean(i and 7)) then begin
      W:= @bW.p[WIndex + 8];
      NW:= @bNW.p[NWIndex + 8];
    end;
    status:= TGenerateStatus(GeneratePtoQ_AVX(@p[i],N,W,NW));
    //All of the cell.
    //if (AllActive in status) then begin
    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
      Additions.Activate(i);
      if (NS_P2Plus in status) then begin
        Additions.Activate(i,North);
        Border.Activate(i, North);
      end;
      if (EW_P2Plus in status) then begin
        Additions.Activate(i,West);
        Border.Activate(i,West);
        if (Corner_P2Plus in status) then begin
          Additions.Activate(i,NorthWest);
          Border.Activate(i,NorthWest);
        end;
      end;
    end else Subtractions.Activate(i);
    i:= ToDoList.Next(i);
  end; {while/for}
  ToDoList.SubtractAdd(Subtractions, Additions);
end;

function TCellBlock.GenerateQtoP(bS, bE, bSE: PCellBlock): TGenerateStatus;
var
  i: integer;
  S,E,SE: PUnit;
  Sindex, EIndex, SEIndex: integer;
  status: TGenerateStatus;
begin
  FillChar(AddSubtract, SizeOf(TDoMap)*2, 0);
  //for i:= 127 downto 0 do begin
  i:= ToDoList.Next(-1);
  while (i < 128) do begin
    SIndex:= i + 8;
    EIndex:= i + 1;
    SEIndex:= i + 9;
    S:= @q[SIndex];
    E:= @q[EIndex];
    SE:= @q[SEIndex];
    if i >= 120 then begin
      S:= @FEmpty;
      SE:= @FEmpty;
    end;
    if (i and 7) = 7 then begin
      E:= @FEmpty;
      SE:= @FEmpty;
    end;
    status:= TGenerateStatus(GenerateQtoP_AVX(@q[i],S,E,SE));
    //All of the cell.
    //if (AllActive in status) then begin
    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
      Additions.Activate(i);
      if (NS_P2Plus in status) then Additions.Activate(i,South);
      if (EW_P2Plus in status) then begin
        Additions.Activate(i,East);
        if (Corner_P2Plus in status) then Additions.Activate(i,SouthEast);
      end;
    end else Subtractions.Activate(i);
    i:= ToDoList.Next(i);
  end; {while/for}
  ToDoList.SubtractAdd(Subtractions, Additions);
end;

procedure TCellBlock.SetPixel(x, y: integer; state: boolean = true);
var
  ux,uy: integer;
begin
  if not(Self.pstate) then begin
    Dec(x); Dec(y);
  end;
  ux:= x and 15;
  uy:= y and 7;
  x:= x shr 4;     //div 16
  y:= y shr 3;     //div 8
  if (pstate) then begin
    p[x+y*8].SetPixel(ux,uy, state);
  end else begin
    q[x+y*8].SetPixel(ux,uy,state);
  end;
end;

constructor TCellBlock.Create(Activate: boolean);
begin
  if Activate then begin
    FillChar(Self, SizeOf(Self)-SizeOf(ToDoList),#0);
    FillChar(ToDoList, SizeOf(ToDoList), $FF);
  end else begin
    FillChar(Self, SizeOf(Self),#0);
  end;
end;

function TCellBlock.CellCount(Generation: integer): integer;
asm
  //rcx = self
  //rdx = generation
  and edx,1
  shl edx,11
  lea rcx,[rcx+rdx+2048]
  mov rdx,-2048
  xor eax,eax
  @loop:
    popcnt r8,qword ptr [rcx+rdx]
    add rax,r8
    add rdx,8
    jnz @loop
end;



////Difference count compares against the previous generation
//function TCellBlock.DifferenceCount: integer;
//asm
//  //rcx = self
//  //rdx = generation
//  lea rax,[rcx+2048]     //end of p
//  lea rcx,[rcx+4096]     //end of q
//  mov rdx,-2048
//  xor r9,r9
//  @loop:
//    mov r8,[rcx+rdx]
//    xor r8,[rax+rdx]
//    popcount r8,r8
//    add r9,r8
//    add rdx,8
//    jnz @loop
//  mov eax,r9d
//end;

//function TCellBlock.BoundingRect(Generation: integer): TRect;
//begin
//  if OddGeneration then begin
//    //scan though the units. Remember the units that are non-zero.
//  end else begin
//
//  end;
//end;


{ CellBlockHelper }

class function CellBlockHelper.New: PCellBlock;
begin
  GetMem(Result, SizeOf(TCellBlock));
end;

procedure CellBlockHelper.Free;
begin
  FreeMem(@Self);
end;

{ TDiBits }

function TDiBits.Stride: integer;
begin
  Result:= 128 div 8;
end;

class function TDiBits.Create: TDIBits;
var
  ScanLineWidth: integer;
  color: PRGBQuad;
begin
  with Result do begin
    ScanlineWidth:= 128 div 8;
    if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);
    GetMem(bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD));
    GetMem(data, ScanlineWidth * 128);
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
      biWidth:= 128;
      biHeight:= -128;
      biPlanes:= 1;
      biBitCount:= 1;
      biCompression:= BI_RGB;
      biSizeImage:= 0;
      biXPelsPerMeter:= 0;
      biYPelsPerMeter:= 0;
      biClrUsed:= 0;
      biClrImportant:= 0;
    end;
  end;
end;

{ TUnit }

procedure TUnit.Display(const BitmapStart: pointer; const LineOffset: integer);
asm
  ///  rcx = self
  ///  rdx = bitmapstart
  ///  r8  = LineOffset
  //mov r10,[rcx+8]
  //mov rcx,[rcx]
  //push rdx
  call ReverseBitsInAllBytes    //returns reversed bits in xmm0.
  movq rax,xmm0
  vpsrldq xmm0,xmm0,8
  //pop rdx
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr eax,16
  mov [rdx+r8],ax
  lea rdx,[rdx+r8*2]
  movq rax,xmm0
  //push rdx
  //mov rcx,r10
  //call ReverseBitsInAllBytes
  //pop rdx
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr eax,16
  mov [rdx+r8],ax
end;

/// TUnit is the lowest level, rows of 16 bits are stored like so:
/// 0123456789ABCDEF     row  0  LSByte    LSB = Left, MSB = Right
/// 0123456789ABCDEF     row  1
/// 0123456789ABCDEF     row  2
/// 0123456789ABCDEF     row  3
/// 0123456789ABCDEF     row  4
/// 0123456789ABCDEF     row  5
/// 0123456789ABCDEF     row  6
/// 0123456789ABCDEF     row  7  MSByte
/// Thus 16 bytes are stored in a TUnit.
/// A TUnit fits in a XMM register and is always processed as a whole.

procedure TUnit.SetPixel(x, y: integer; value: boolean = true);
var
  Mask: word;
begin
  Mask:= 1 shl x;
  if value then w[y]:= w[y] or Mask
  else w[y]:= w[y] and not mask;
end;


procedure TForm64.Button1Click(Sender: TObject);
var
  i,j: integer;
begin
  //Timer1.Enabled:= not Timer1.Enabled;
  //FCellBlock.Init;
  Draw;
  for i:= 0 to 1000*1000 do begin
    {if (i and $7) = 0 then}
    //for j:= 0 to 90 do
    //Draw;
    //Sleep(100);
    //Application.ProcessMessages;
    Generate;
  end;
  Draw;
end;

procedure TForm64.Button2Click(Sender: TObject);
var
  i: int64;
  a: array[0..2] of int64;

begin
  for i:= 0 to $FFFF do begin
    a[0]:= i;
    a[1]:= ReverseBitsInAllBytes_Reference(i);
    ReverseBitsInAllBytesSSE(@a[1]);
    if a[0] <> a[1] then begin
      ReverseBitsInAllBytesSSE(@a[1]);
      Assert(a[0] = a[1]);
    end;
  end;
end;

procedure TForm64.Button3Click(Sender: TObject);
var
  a: TDoMap;
  i,r: integer;
  x,y: integer;
begin
  a.Clear;
  for i:= 0 to 100000 do begin
    r:= random(128);
    //r:= 127;
    a.Invert(r);
    //a.Activate(r);
    x:= -1;
    y:= -1;
    repeat
      x:= a.Next(x);
      y:= a.NextTry2(y);
      if (x <> y) and ((x < 128) or (y < 128)) then begin
        Assert(x=y);
      end;
    until (x > 127) and (y > 127);
  end;
end;

procedure TForm64.Button4Click(Sender: TObject);
var
  TimingTest2, TimingTest1: integer;
  i: integer;
  a: TDoMap;
  T2,T1: double;

  procedure Test1;
  var
    i:integer;
    a:TDoMap;
  begin
    for i := 0 to 127 do begin
      //i:= Random(128);
      a.Next(a.a[i and 127]);
      a.Next(128);
      //ReverseBitsInAllBytesSSE(@a);
    end;
  end;

  procedure Test2;
  var
    i: integer;
    a: TDoMap;
  begin
    for i := 0 to 127 do begin
      //a:= ReverseBitsInAllBytes_SSE_2Lookups(i);
      //ReverseBitsInAllBytes(@a);
      //i:= Random(128);
      a.NextTry2(a.a[i and 127]);
      a.Next(128);
    end;
  end;

begin
  for i:= 0 to 15 do begin
    a.a[i]:= random(255);
  end;
  TimingTest1:= THiResStopWatch.Sample(@Test1,10000);
  TimingTest2:= THiResStopWatch.Sample(@Test2,10000);
  T2:= TimingTest2;
  T1:= TimingTest1;
  Edit1.Text:= 'Test1: '+Format('%.0M, %.0M',[T1, (T1 / 128)])+
               ', Test2: '+Format('%.0M, %.0M',[T2, (T2 / 128)])+
               ',diff: '+IntToStr((TimingTest1-(TimingTest2 div 1))*100 div (TimingTest2 div 1));

end;

procedure TForm64.Button5Click(Sender: TObject);
var
  i,x,y: integer;
  GuineePig: TDoMap;
  Soll, Ist, Ist2: boolean;
begin
  for i:= 0 to 127 do begin
    for x:= -1 to 1 do begin
      for y:= -1 to 1 do begin
        if (x * y) < 0 then continue;
        Soll:= (((i and 7) = 0) and (x < 0)) or (((i and 7) = 7) and (x > 0))
            or ((i < 8) and (y < 0)) or ((i>= 120) and (y > 0));
        Ist:= GuineePig.Activate(i,x+y*8);
        if (Ist) then Ist2:= true else Ist2:= false;
        if Ist2 xor Soll then begin
          Ist:= GuineePig.Activate(i,x+y*8);
          Assert(not(Soll xor Ist2));
        end;
      end;
    end;
  end;
end;

procedure TForm64.Button6Click(Sender: TObject);
var
  All: int64;
  NW, NE, SE, SW: int64;
  x,y: integer;
  Bit: Int64;
function GetBit(x,y: integer): int64;
asm
  push rcx
  push rdx
  mov rcx,rdx
  mov eax,1
  shl rax,cl
  lea rcx,[r8*8]
  shl rax,cl
  pop rdx
  pop rcx
  //Result:= (1 shl x) shl (y * 8);
end;

begin
  All:= 0;
  for x:= 1 to 6 do begin
    for y:= 1 to 6 do begin
      bit:= GetBit(x,y);
      All:= All or bit;
    end;
  end;
  NW:= All;
  NE:= All;
  SE:= All;
  SW:= All;
  for x:= 1 to 3 do for y:= 1 to 3 do begin
    bit:= GetBit(x,y);
    SE:= SE and not bit;
  end;
  for x:= 1 to 3 do for y:= 4 to 6 do begin
    bit:= GetBit(x,y);
    NE:= NE and not bit;
  end;
  for x:= 4 to 6 do for y:= 1 to 3 do begin
    bit:= GetBit(x,y);
    SE:= SE and not bit;
  end;
  for x:= 4 to 6 do for y:= 4 to 6 do begin
    bit:= GetBit(x,y);
    NW:= NW and not bit;
  end;
end;

procedure TForm64.Button7Click(Sender: TObject);
var
  i: int64;
  a: int64;
  count: int64;
begin
  Count:= 0;
  i:= 1;
  repeat
    a:= NextCanocialNumber(i);
    Inc(count);
  until a = 0;
  ShowMessage(Format('count = %x, %d',[count,count]));
end;

procedure TForm64.Button8Click(Sender: TObject);
var
  i,a: int64;
  StartingBlock: TCellBlock;
begin
  FillChar(StartingBlock, SizeOf(StartingBlock), #0);
  i:= 1;
  while i > 0 do begin
    a:= NextCanocialNumber(i);
    //a:= $005E7E7E6E7E7A00;
    DivideStartingBlock(StartingBlock.p[59], StartingBlock.p[60], StartingBlock.p[67], StartingBlock.p[68], a);
    //DivideStartingBlock(StartingBlock.q[59], StartingBlock.q[60], StartingBlock.q[67], StartingBlock.q[68], a);
    FCellNode:= StartingBlock;
    Draw;
  end;
end;

procedure TForm64.Button9Click(Sender: TObject);
var
  a,i: int64;
begin
  i:= 1;
  while i > 0 do begin
    a:= NextCanocialNumber(i);
    //FCellNode.Init(true);
    DivideStartingBlock(FCellNode.p[59], FCellNode.p[60], FCellNode.p[67], FCellNode.p[68], a);
    //DivideStartingBlock(StartingBlock.q[59], StartingBlock.q[60], StartingBlock.q[67], StartingBlock.q[68], a);

  end;
end;


procedure TForm64.FormCreate(Sender: TObject);
var
  i,j: integer;
begin
  FDrawSurfaceP:= TDIBits.Create;
  FDrawSurfaceQ:= TDIBits.Create;
  FCellNode:= TCellBlock.Create(true);
  FEmpty:= TCellBlock.Create(false);
  //if SizeOf(TCellBlock) <> 1024*4 then ShowMessage('oops');
  //*********************************** Random data
  for i:= 0 to 127 do begin
    for j:= 0 to 15 do begin
      FCellNode.p[i].b[j]:= Random(255);
    end;
  end;
  //************************************
  //FCellBlock.p[0].b[0]:= $80;
  //FCellBlock.p[0].b[2]:= $40;
  //FCellBlock.p[0].b[4]:= $20;
  //FCellBlock.p[0].b[6]:= $10;
  //FCellBlock.p[0].b[8]:= $08;
  //FCellBlock.p[0].b[10]:= $04;
  //FCellBlock.p[0].b[12]:= $02;
  //FCellBlock.p[0].b[14]:= $01;
  //
  //FCellBlock.p[1].b[0]:= $80;
  //FCellBlock.p[1].b[2]:= $40;
  //FCellBlock.p[1].b[4]:= $20;
  //FCellBlock.p[1].b[6]:= $10;
  //FCellBlock.p[1].b[8]:= $08;
  //FCellBlock.p[1].b[10]:= $04;
  //FCellBlock.p[1].b[12]:= $02;
  //FCellBlock.p[1].b[14]:= $01;
  //************************************  Glider
  //FCellBlock.p[11].b[3]:= 2;
  //FCellBlock.p[11].b[5]:= 1;
  //FCellBlock.p[11].b[7]:= 7;
  ///***********************************

  //FCellBlock.p[4].w[1]:= $7000;
  //FCellBlock.p[3].w[2]:= $0003;
  //FillChar(FCellBlock.p, SizeOf(TCellBlock), $00);
  //FCellBlock.p[36].b[3]:=  $FF;
  //FCellBlock.p[36].b[5]:=  $FF;
  //FCellBlock.p[36].b[7]:=  $FF;
  //FCellBlock.p[36].b[9]:=  $FF;
  //FCellBlock.p[36].b[8]:=  $40;
  //FCellBlock.p[36].b[10]:= $20;
end;

procedure TForm64.Image1Click(Sender: TObject);
begin

end;

procedure TForm64.PaintBox1Click(Sender: TObject);
begin

end;

end.


end.

