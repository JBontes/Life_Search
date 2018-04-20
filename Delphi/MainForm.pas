unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Menus,

  Universe, Vcl.ExtDlgs, Vcl.Grids, Vcl.ComCtrls,
  Slices;

const
  WM_MyProgress = WM_User+1;
  WM_ThreadDone = WM_User+2;
  WM_CountThreadDone = WM_User+3;
  cNumberOfCores = 8;

type
  TTestSlice = array[0..63] of byte;
  TAncestorSlice = array[0..15] of TTestSlice;
  TAncestorData = array[0..$FFFF] of TAncestorSlice;

  TMyThread = class(TThread)
  private
    FID: integer;
  public
    constructor Create(ID: integer);

    procedure Execute; override;
  end;

  TAncestorCountPerBitThread = class(TThread)
  private
    FID: integer;
    FStart, FEnd: integer;
  public
    constructor Create(ID, Start, Eind: integer);
    procedure Execute; override;
  end;

  TListAncestorThread = class(TThread)
  private
    FDescendent: cardinal;
    FId: cardinal;
   public
     constructor Create(ID, Descendent: cardinal);
     procedure Execute; override;
  end;

  TForm75 = class(TForm)
    TopPanel: TPanel;
    ToolPanel: TPanel;
    StatusPanel: TPanel;
    MainPanel: TPanel;
    ScrollBar2: TScrollBar;
    Splitter1: TSplitter;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    ScrollBar1: TScrollBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Animation1: TMenuItem;
    Screen1: TMenuItem;
    Options1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    TestMap: TButton;
    Label1: TLabel;
    FileOpenDialog1: TFileOpenDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BtnCombineFiles: TButton;
    BtnMakeCountTable: TButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    StringGrid1: TStringGrid;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Button13: TButton;
    Button14: TButton;
    procedure TestMapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure BtnCombineFilesClick(Sender: TObject);
    procedure BtnMakeCountTableClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure StringGrid3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
  private
    LifeUniverse: TNode;
    CellBlock: array [0..3] of TCellBlock;
    Threads: array[0..7] of TThread;
    function CountsToStringList: TStringList;
    procedure CalculateAncestorbitsPerPixel(Segment: integer);
    procedure AncestorThreadsAllDone;
    procedure CalculateAncestorCountsPerPixel(ID, Start, Eind: integer);
    procedure CountThreadDone;
    procedure AncestorCountsThreadsAllDone;
  public
    Lookuptable: array[0..$FFFF] of byte;
    CountTable: array[0..15] of integer;
    CountTable4x4: array[0..$FFFF] of int64;
    AncestorTable4x4: array[0..3] of TAncestorData;
    AncestorTableBig: array[0..($10000*16*64*cNumberOfCores)-1] of byte;
    AncestorCountTable4x4: array {[0..256*256*512*16-1]} of cardinal;
    ThreadsDone: integer;
    CountThreadsDone: integer;
    NumberOfCores: integer;
    procedure WMMyProgress(var Msg: TMessage); message WM_MyProgress;
    procedure ThreadDone;
  private
    CurrentGrid: TGrid;
    procedure Display(const SG: TStringGrid);
    procedure ProcessEntailments;
    procedure ValidateGrid;
  end;

  function PopCount(input: int64): integer;



var
  Form75: TForm75;

implementation

{$R *.dfm}

uses
  System.Diagnostics, StrUtils, Math, pngimage, System.IOUtils, SparseSet, System.Types;

{$pointermath on}

function TForm75.CountsToStringList: TStringList;
var
  i: integer;
begin
  Result:= TStringList.Create;
  Result.Capacity:= 256*256;
  for i:= 0 to $FFFF do begin
    if (CountTable4x4[i] <> 0) then begin
      Result.Add(i.ToString + ','+CountTable4x4[i].ToString + ','+PopCount(i).ToString);
    end;
  end;
end;



procedure TForm75.Button10Click(Sender: TObject);
begin
  //Test transformation of core slices
  TSlice.InitTransformationMatrix;
end;

procedure TForm75.Button13Click(Sender: TObject);
begin
  CurrentGrid:= TGrid.Create(10,10,1);
  CurrentGrid.Past:= TGrid.Create(10,10,0);
  CurrentGrid.Future:= TGrid.Create(10,10,2);
  CurrentGrid.Pixel[4,4]:= psOn;
  CurrentGrid.Pixel[5,4]:= psOn;
  CurrentGrid.Pixel[6,4]:= psOn;
  CurrentGrid.Pixel[6,5]:= PsOn;
  CurrentGrid.Pixel[5,6]:= PsOn;
  CurrentGrid.Display(StringGrid2);
  CurrentGrid.Past.Display(StringGrid1);
  CurrentGrid.Future.Display(StringGrid3);
end;

procedure TForm75.Button14Click(Sender: TObject);
begin
  Display(StringGrid1);
  Display(StringGrid2);
  Display(StringGrid3);
  ValidateGrid;
  ProcessEntailments;
  ValidateGrid;
  Display(StringGrid1);
  Display(StringGrid2);
  Display(StringGrid3);
end;

procedure TForm75.Button1Click(Sender: TObject);
var
  i,a,x: int64;
  Timer: TStopWatch;
  status: TGenerateStatus;
  Empty: TCellBlock;
  c, S,E,SE: pbyte;
  Inner4x4: int32;
  List: TStringList;
begin
  Timer:= TStopWatch.StartNew;
  CellBlock[0]:= TCellBlock.Create(0,0,0,nil);
  for x:= $0 to $FF do begin
    for a:= $000000 to $FFFFFFF do begin
      i:= (a shl 8) + x;

      CellBlock[0].SetCore(i);
      //CellBlock.GeneratePtoQ;
      c:= @CellBlock[0].p[17];
      S:= @c[16*4];
      E:= c +  (16*1);
      SE:= c + (16*5);
      status:= TGenerateStatus(GeneratePtoQ_AVX_32(c,S,E,SE));
      //inner 4x4 result goes to Q[17].
      Inner4x4:= CellBlock[0].Get4x4(@CellBlock[0].q[17],6);
      inc(CountTable4x4[Inner4x4]);
    end;
    Label1.Caption:= 'Progress: '+x.ToString+' of 256';
    Application.ProcessMessages;
  end;
  Label1.Caption:= 'Progress: writing results to disk';
  Application.ProcessMessages;
  List:= CountsToStringList;
  List.SaveToFile('C:\Users\Johan\Documents\UCT\ResearchStats\4x4_ancestor_count.txt');
  Label1.Caption:= 'Progress: Done';
  ShowMessage('Done: time taken = '+Timer.ElapsedMilliseconds.ToString+' ms');
end;

procedure TForm75.Button2Click(Sender: TObject);
var
  x1,y1,i,x,y: Integer;
  mask: word;
  Count: integer;
  Previous: integer;
  New: integer;
begin
  for i:= $8880-$8880 to $FFFF do begin
    New:= 0;
    for x1 := 1 to 2 do for y1 := 1 to 2 do begin
      Count:= 0;
      Previous:= 0;
      for x := -1 to 1 do for y := -1 to 1 do begin
        Mask:= 1 shl ((x+x1)+((y+y1)*4));
        if (x or y) = 0 then begin
          Previous:= i and Mask;
        end else if ((i and Mask) <> 0) then Inc(Count);
      end;
      if ((Previous = 0) and (Count = 3))
      or ((Previous <> 0) and (Count in [2,3])) then New:= New or (1 shl ((x1-1)+((y1-1)*2)));
    end;
    LookupTable[i]:= New;
  end;
  for i := 0 to $FFFF do begin
    Inc(CountTable[Lookuptable[i]]);
  end;
end;



const
  Pal16: array[0..18] of TColor = ($000000 ,$000033 ,$000066 ,$000088 ,$0000AA ,$0000CC ,$0000FF ,$3232FF
                                  ,$7676FF ,$BBBBFF ,$FFFFFF ,$FFBBBB ,$FF7676 ,$FF3200 ,$FF0000 ,$DD0000,$AA0000,$00FF00,$00FF00);

  SegmentWidths: array[0..16] of integer = (1,1,1,4,10,21,28,40,45,40,28,21,10,4,1,1,1);
  SegmentWidths2: array[0..16] of integer = (1,1,1,2,5,10,14,20,22,20,14,10,5,2,1,1,1);
  SegmentHeights: array[0..16] of integer = (1,17,120,140,182,208,286,286,286,286,286,208,182,140,120,17,1);

function TotalWidth(input: array of integer): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= low(input) to high(input) do begin
    Result:= Result + input[i];
  end;
end;

function Log2(input: int64): integer;
asm
  rep bsr rcx,rcx
  mov rax,64
  sub rax,rcx
end;

function PopCount(input: int64): integer;
asm
  popcnt rax,rcx
end;

function GetABit(input, bit: integer): integer;
asm
  //ecx: input
  //edx: bit
  xor eax,eax
  bt ecx,edx
  adc eax,0
end;



procedure SetABit(var data: integer; bit: integer; value: integer);
asm
  //data: RCX
  //bit: edx
  //value: r8d
  xchg rcx,rdx
  shl r8d,cl
  or [rdx],r8d
end;

type
  TBit = record
  private Fdata: integer;
    function GetBit(index: integer): integer;
    procedure SetBit(index: integer; const Value: integer);
  public
    property bit[index: integer]: integer read GetBit write SetBit; default;
    class operator Implicit(const A: integer): TBit;
    class operator Implicit(const B: TBit): integer;
    class operator GreaterThanOrEqual(const A: TBit; const B: integer): boolean;
  end;

function MirrorHorz16(input: TBit): TBit;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to 3 do begin
    Result[3+i*4]:= input[0+i*4];
    Result[2+i*4]:= input[1+i*4];
    Result[1+i*4]:= input[2+i*4];
    Result[0+i*4]:= input[3+i*4];
  end;
end;

function MirrorVert16(input: TBit): TBit;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to 3 do begin
    Result[i+0]:= input[i+12];
    Result[i+4]:= input[i+8];
    Result[i+8]:= input[i+4];
    Result[i+12]:= input[i+0];
  end;
end;

function MirrorDiagA16(input: TBit): TBit;
var
  i: integer;
begin
  Result:= 0;
  Result[0]:= Input[15];
  Result[1]:= Input[11];
  Result[2]:= Input[7];
  Result[3]:= Input[3];
  Result[4]:= Input[14];
  Result[5]:= Input[10];
  Result[6]:= Input[6];
  Result[7]:= Input[2];
  Result[8]:= Input[13];
  Result[9]:= Input[9];
  Result[10]:= Input[5];
  Result[11]:= Input[1];
  Result[12]:= Input[12];
  Result[13]:= Input[8];
  Result[14]:= Input[4];
  Result[15]:= Input[0];
end;

function MirrorDiagB16(input: TBit): TBit;
var
  i: integer;
begin
  Result:= 0;
  Result[0]:= Input[0];
  Result[1]:= Input[4];
  Result[2]:= Input[8];
  Result[3]:= Input[12];
  Result[4]:= Input[1];
  Result[5]:= Input[5];
  Result[6]:= Input[9];
  Result[7]:= Input[13];
  Result[8]:= Input[2];
  Result[9]:= Input[6];
  Result[10]:= Input[10];
  Result[11]:= Input[14];
  Result[12]:= Input[3];
  Result[13]:= Input[7];
  Result[14]:= Input[11];
  Result[15]:= Input[15];
end;

function Rotate180_16(input: TBit): TBit;
begin
  Result:= MirrorVert16(MirrorHorz16(input));
end;

function Rotate90_16(input: TBit): TBit;
begin
  Result:= MirrorHorz16(MirrorDiagA16(input));
end;

function Rotate270_16(input: TBit): TBit;
begin
  Result:= MirrorHorz16(MirrorDiagB16(input));
end;


{$pointermath on}

procedure TForm75.Button3Click(Sender: TObject);
type
  TRGBTripleArray = array [word] of TColor;
  pRGBTripleArray = ^TRGBTripleArray; // Use a PByteArray for pf8bit color.
  TCountarray = array[0..16] of integer;
var
  S: TStringList;
  i, c,x, y, x1, x2: integer;
  items: TArray<string>;
  output: TStringList;
  Bitmap: TBitmap;
  Bitmap2: TBitmap;
  Colors: array [0 .. 15] of TColor;
  ScanLine: pRGBTripleArray;
  png: TPNGObject;
  HistoCount: TCountArray;
  SegmentStart: TCountArray;
  Segment: integer;
  Filename: string;
const
  ScaleFactor = 8;
begin
  FillChar(HistoCount, sizeof(HistoCount),#0);
  S:= TStringList.Create;
  png:= TPNGObject.Create;
  Filename:= 'C:\Users\Johan\Documents\UCT\ResearchStats\4x4_ancestor_count.txt';
  try
    if (FileOpenDialog1.Execute) then begin
      S.LoadFromFile(FileOpenDialog1.FileName);
    end
    else exit;
    //S.LoadFromFile(Filename);
    for i:= 0 to S.Count -1 do begin
      items:= S[i].Split([',']);
      CountTable4x4[items[0].ToInteger]:= items[1].ToInt64;
      S[i]:= s[i]+','+PopCount(items[0].ToInteger).ToString;
    end;
    for i:= 0 to 15 do begin
      Colors[i]:= Pal16[i];
    end;
    Bitmap:= TBitmap.Create;
    Bitmap.Height:= 286;
    Bitmap.Width:= 130;
    Bitmap.PixelFormat:= pf32bit;
    Bitmap.Canvas.Brush.Color:= RGB($DD,$DD,$DD);
    Bitmap.Canvas.FillRect(Rect(0,0,Bitmap.Width, Bitmap.Height));
    x:= 0;
    for i:= 0 to 16 do begin
      SegmentStart[i]:= x;
      x:= x + SegmentWidths2[i];
    end;
    for i:= 0 to $FFFF do begin
      if (CountTable4x4[i] = 0) then continue;
      Segment:= PopCount(i);
      x:= SegmentStart[Segment] + (HistoCount[Segment] mod SegmentWidths2[Segment]);
      y:= HistoCount[Segment] div SegmentWidths2[Segment];
      y:= 285-y;
      c:= log2(CountTable4x4[i])-15;
      Bitmap.Canvas.Pixels[x,y]:= Pal16[c];
      if (c < 0) or (c > 17) then ShowMessage(c.ToString);
      Inc(HistoCount[Segment]);
    end;
    Bitmap2:= TBitmap.Create;
    Bitmap2.Height:= Bitmap.Height * ScaleFactor;
    Bitmap2.Width:= Bitmap.Width * ScaleFactor+16;   //for the separating grid lines
    Bitmap2.PixelFormat:= pf32bit;
    for i:= 0 to 16 do begin
      x1:= SegmentStart[i];
      x2:= SegmentStart[i]+SegmentWidths2[i];
      Bitmap2.Canvas.CopyRect(Rect(x1*ScaleFactor+i,0,x2*ScaleFactor+i,286*ScaleFactor),Bitmap.Canvas,Rect(x1,0,x2,285+1));
    end;
    Image1.Picture.Assign(Bitmap2);
    png.Assign(Bitmap2);
    png.SaveToFile(FileName + '.png');
    //S.SaveToFile(Filename + 'Bitcount.txt');
  finally
    png.Free;
    Bitmap.Free;
    Bitmap2.Free;
    S.Free;
  end;
end;

procedure TForm75.Button4Click(Sender: TObject);
var
  Filename: string;
  S: TStringList;
  i: integer;
  items: TArray<string>;
begin
  Filename:= 'C:\Users\Johan\Documents\UCT\ResearchStats\4x4_ancestor_count.txt';
  S:= TStringList.Create;
  try
    // if (FileOpenDialog1.Execute) then begin
    // S.LoadFromFile(FileOpenDialog1.FileName);
    // end
    // else exit;
    FillChar(CountTable4x4,SizeOf(CountTable4x4), #0);
    S.LoadFromFile(Filename);
    for i:= 0 to $FFFF do begin
      if (MirrorHorz16(i) >= i) and (MirrorVert16(i) >= i) and (MirrorDiagA16(i) >= i) and (MirrorDiagB16(i) >= i) and
        (Rotate180_16(i) >= i) and (Rotate90_16(i) >= i) and (Rotate270_16(i) >= i) then begin
        items:= S[i].Split([',']);
        CountTable4x4[i]:= items[1].ToInt64;
        S[i]:= S[i] + ',' + PopCount(items[0].ToInteger).ToString;
      end;
    end;
    S.Free;
    S:= CountsToStringList;
    S.SaveToFile(Filename+'dedup.txt');
  finally
    S.Free;
  end;
end;

procedure TForm75.Button5Click(Sender: TObject);
var
  S: TStringList;
  Filename: string;
  i: integer;
  item: TArray<string>;

begin
  Filename:= 'C:\Users\Johan\Documents\UCT\ResearchStats\4x4_ancestor_count.txt';
  S:= TStringList.Create;
  try
    S.LoadFromFile(Filename);
    for i:= 0 to $FFFF do begin
      Item:= S[i].Split([',']);
      S[i]:= S[i] + ','+ PopCount(Item[0].ToInteger).ToString;
    end;
    S.SaveToFile(Filename);
  finally
    S.Free;
  end;
end;

procedure BTS(P: pointer; bitindex: integer);
asm
  //RCX = pointer
  //EDX = offset
  bts [rcx],rdx
end;

const
  Bitmask: array [0..15] of int64 = ($71C7,       $71C7 shl 1, $71C7 shl 2, $71C7 shl 3,
                                     $71C7 shl 6, $71C7 shl 7, $71C7 shl 8, $71C7 shl 9,
                                     $71C7 shl 12,$71C7 shl 13,$71C7 shl 14,$71C7 shl 15,
                                     $71C7 shl 18,$71C7 shl 19,$71C7 shl 20,$71C7 shl 21);

function ShrinkInputToMask(input, mask: int64): integer;
asm
  ///  registers
  ///  RCX = input
  ///  RDX = mask
  ///  eax = output
  rep bsf rax,rdx      //al = count the number of trailing zeros
  and rdx,rcx          //rdx = the 9 relevant bits in the input
  mov cl,al            //cl = shift count
  shr rdx,cl           //shift input to canocial position
  mov ecx,edx          //ecx = middle row
  mov eax,edx          //eax = bottom row
  and edx,7            //edx = top row (LSB bits)
  shr ecx,3            //shift middle row into place, LSB's will fall off
  shr eax,6            //shift top row into place.
  and eax,not(7)       //mask off LSB from top row
  and ecx,$3F           //mask off MSB from middle row
  or eax,edx           //combine all rows
  or eax,ecx           //...
end;





procedure TForm75.BtnCombineFilesClick(Sender: TObject);
var
  Stream: TFileStream;
  Filename: string;
begin
  Label1.Caption:= 'Progress: read data';
  Application.ProcessMessages;
  Filename:= TPath.GetDocumentsPath;
  FileName:= Filename + '\UCT\ResearchStats';
  ForceDirectories(Filename);
  Filename:= Filename + '\4x4_ancestor_slices_small.bin';
  Stream:= TFileStream.Create(Filename, fmOpenRead);
  Stream.ReadBuffer(AncestorTableBig, SizeOf(AncestorTableBig));
  Stream.Free;
  AncestorThreadsAllDone;
end;


function PopCount64Bytes(var Data; index: integer): integer;
asm
  //register usage
  //RCX = data
  //edx = index
  shl edx,6    //edx=edx*64
  lea rcx,[rcx+rdx]      //move the pointer in place (every element = 64 bytes)
  popcnt rax,[rcx]       //count bits in place
  popcnt rdx,[rcx+8]
  popcnt r8,[rcx+16]
  popcnt r9,[rcx+24]
  popcnt r10,[rcx+32]
  popcnt r11,[rcx+40]
  add eax,edx
  add r8,r9
  add r10,r11
  popcnt rdx,[rcx+48]
  popcnt r9,[rcx+56]
  add eax,r8d
  add r10,rdx
  add eax,r9d
  add eax,r10d
end;



function Popcount64BytesOn(var Data; index: integer): integer;
asm
  //register usage
  //RCX = data
  //edx = index
  //Count only bits that have the relevant center bit to be off.
  //There are 9 bits and we need to exclude
  shl edx,6    //edx=edx*64
  mov eax,8    //process 8 qwords
  lea rcx,[rcx+rdx]
  mov r8,$0000FFFF0000FFFF   //mask 16 off bits, 16 on bits
  xor r10,r10
  @loop:
    mov rdx,[rcx]      //get the data
    lea rcx,[rcx+8]
    and rdx,r8         //Mask out all the bits that have the center bit unset.
    popcnt rdx,rdx     //get the popcount
    add r10,rdx
    sub eax,1
  jnz @loop
  mov eax,r10d
end;

procedure TForm75.BtnMakeCountTableClick(Sender: TObject);
var
  SL: TStringList;
  i: integer;
  Stream: TFileStream;
  Filename: string;
  Constellation,bitnr,onoff,ncount,ncount_off,ncount_on: integer;
const
  c=',';
begin
  SL:= TStringList.Create;
  Label1.Caption:= 'Progress: read data';
  Application.ProcessMessages;
  Filename:= TPath.GetDocumentsPath;
  FileName:= Filename + '\UCT\ResearchStats';
  ForceDirectories(Filename);
  Filename:= Filename + '\4x4_ancestor_slices_small.bin';
  Stream:= TFileStream.Create(Filename, fmOpenRead);
  Stream.ReadBuffer(AncestorTableBig, SizeOf(AncestorTableBig) div 4);
  Stream.Free;
  Label1.Caption:= 'Progress: writing CSV';
  Application.ProcessMessages;
  try
    SL.Capacity:= 1024 * 1024+1;
    SL.Add('Constellation,bitnr,onoff,ncount,ncount_off,ncount_on');
    for i:= 0 to 1024*1024 do begin
      Constellation:= i div 16;
      Bitnr:= i and 15;
      OnOff:= Ord(Odd(Constellation shr Bitnr));
      ncount:= PopCount64Bytes(AncestorTableBig, i);
      ncount_on:= Popcount64BytesOn(AncestorTableBig, i);
      ncount_off:= ncount - ncount_on;
      SL.Add(Constellation.ToString+c+
             bitnr.ToString+c+
             onoff.ToString+c+
             ncount.ToString+c+
             ncount_off.ToString+c+
             ncount_on.ToString);
    end; {for i}
    Label1.Caption:= 'Progress: saving CSV file';
    Application.ProcessMessages;
    Filename:= TPath.GetDocumentsPath;
    FileName:= Filename + '\UCT\ResearchStats';
    Filename:= Filename + '\rbitstats.csv';
    SL.SaveToFile(Filename);
    Label1.Caption:= 'Progress: Done';
    Application.ProcessMessages;
  finally
    SL.Free;
  end;
end;

//
//  TTestSlice = array[0..63] of byte;
//  TAncestorSlice = array[0..15] of TTestSlice;
//  TAncestorData = array[0..$FFFF] of TAncestorSlice;
procedure TForm75.Button6Click(Sender: TObject);
var
  T: array [0..7] of TThread;
  i: integer;
begin
  for i:= 0 to NumberOfCores-1 do begin
    T[i]:= TMyThread.Create(i);
    SetThreadIdealProcessor(T[i].Handle, i);
    T[i].Start;
  end;
end;



procedure TForm75.Button7Click(Sender: TObject);
var
  i: integer;
begin
  for i in [32,33,34,35] do begin
    Threads[i-32]:= TListAncestorThread.Create(i-32, i);
    Threads[i-32].Start;
  end;
end;

procedure TForm75.Button8Click(Sender: TObject);
const
  Ends: array[0..8] of integer = (-1,71,1291,4304,10773,19777,32895,43685,65536);
var
  Threads: array[0..7] of TThread;
  i: integer;
begin
  SetLength(AncestorCountTable4x4, 256*256*16*512);
  for i:= 0 to 0 do begin
    Threads[i]:= TAncestorCountPerBitThread.Create(I, 0,256*256);
    //SetThreadIdealProcessor(Threads[i].Handle, i);
    Threads[i].Start;
  end;
end;

procedure TForm75.Button9Click(Sender: TObject);
begin
  SetLength(AncestorCountTable4x4, 256*256*256*(128 div SizeOf(Cardinal)));
  AncestorCountsThreadsAllDone;
end;

procedure TForm75.AncestorThreadsAllDone;
var
  Stream: TFileStream;
  i,x,y,z: integer;
  FileName: string;
begin
    //Wait for threads to finish
  Label1.Caption:= 'Progress: combining results';
  Application.ProcessMessages;
  Filename:= TPath.GetDocumentsPath;
  FileName:= Filename + '\UCT\ResearchStats';
  ForceDirectories(Filename);
  Filename:= Filename + '\4x4_ancestor_slices_4_parts.bin';
  Stream:= TFileStream.Create(Filename, fmCreate);
  Stream.WriteBuffer(AncestorTableBig, SizeOf(AncestorTableBig));
  Stream.Free;
  for i:= 1 to NumberOfCores-1 do begin
    for x:= 0 to $03FFFFFF do begin
      AncestorTableBig[x]:= AncestorTableBig[x] or AncestorTableBig[(i * $4000000) + x];
    end;
  end;
  Label1.Caption:= 'Progress: writing results to disk';
  Application.ProcessMessages;
  Filename:= TPath.GetDocumentsPath;
  FileName:= Filename + '\UCT\ResearchStats';
  ForceDirectories(Filename);
  Filename:= Filename + '\4x4_ancestor_slices_small.bin';
  Stream:= TFileStream.Create(Filename, fmCreate);
  try
    Stream.WriteBuffer(AncestorTableBig, SizeOf(AncestorTableBig) div NumberOfCores);
  finally
    Stream.Free;
  end;
  Label1.Caption:= 'All done';
end;


procedure TForm75.AncestorCountsThreadsAllDone;
var
  Stream: TFileStream;
  i,x,y,z: integer;
  FileName: string;
  BytesWritten: cardinal;
  Size: cardinal;
begin
    //Wait for threads to finish
  Filename:= TPath.GetDocumentsPath;
  FileName:= Filename + '\UCT\ResearchStats';
  ForceDirectories(Filename);
  Label1.Caption:= 'Progress: writing results to disk';
  Application.ProcessMessages;
  Filename:= Filename + '\4x4_ancestor_counts_per_pixel.bin';
  Stream:= TFileStream.Create(Filename, fmCreate);
  Size:= Length(AncestorCountTable4x4)*SizeOf(Cardinal);
  WriteFile(Stream.Handle, AncestorCountTable4x4[0], Size, BytesWritten, nil);
  if (BytesWritten < Size) then begin
    Size:= Size - BytesWritten;
    WriteFile(Stream.Handle, PByte(@AncestorCountTable4x4[0])[BytesWritten], Size - BytesWritten, BytesWritten, nil);
  end;
  Stream.Free;
  Label1.Caption:= 'All done';
end;



procedure TForm75.CalculateAncestorBitsPerPixel(Segment: integer);
var
  i,a,x,y: int64;
  Pixel: integer;
  Timer: TStopWatch;
  status: TGenerateStatus;
  Empty: TCellBlock;
  c, S,E,SE: pbyte;
  Inner4x4: int32;
  List: TStringList;
  Stream: TFileStream;
  Alive: boolean;
begin
  FillChar(AncestorTable4x4, SizeOf(AncestorTable4x4), #0);
  //Timer:= TStopWatch.StartNew;
  CellBlock[Segment]:= TCellBlock.Create(0,0,0,nil);
  for x:= Segment * ($100 div NumberOfCores) to Segment * ($100 div NumberOfCores) + (({ $100} NumberOfCores div NumberOfCores)-1) do begin
    for a:= $000000 to $FFFFFFF do begin
      i:= (a shl 8) + x;
      CellBlock[Segment].SetCore(i);
      //CellBlock.GeneratePtoQ;
      c:= @CellBlock[Segment].p[17];
      S:= @c[16*4];
      E:= c +  (16*1);
      SE:= c + (16*5);
      status:= TGenerateStatus(GeneratePtoQ_AVX_32(c,S,E,SE));
      //inner 4x4 result goes to Q[17].
      Inner4x4:= CellBlock[Segment].Get4x4(@CellBlock[Segment].q[17],6);
      inc(CountTable4x4[Inner4x4]);
      //**********************************************************************
      ///  i = cell input
      ///  p[17] = translated input
      ///  q[17] = raw output
      ///  Inner4x4 = translated output
      ///  The inner 4x4 need 16 masks
      ///  We need to count bits for every mask
      ///
      //**********************************************************************
      for Pixel:= 0 to 15 do begin
        //BTS(@AncestorTable4x4[Segment][Inner4x4][Pixel][0],ShrinkInputToMask(i, Bitmask[pixel]));
        y:= ShrinkInputToMask(i, Bitmask[pixel]);
        //Alive:= (PopCount(y and $1EF) = 3) or (PopCount(y) = 3);
        //if ((Odd(Inner4x4 shr Pixel) <> Alive)) then begin
        //  Assert(Odd(Inner4x4 shr Pixel) = Alive);
        //end;
        //Assert(y <= 511);
        BTS(@AncestorTableBig[(Segment * $10000*16*64) + (Inner4x4 *16*64) + (Pixel*64)],y);
      end;
    end;
    PostMessage(Application.MainFormHandle, WM_MyProgress,Segment,x);
    //Label1.Caption:= 'Progress: '+x.ToString+' of 256';
    //Application.ProcessMessages;
  end;
  //Label1.Caption:= 'Progress: writing results to disk';
  //Application.ProcessMessages;
  //Stream:= TFileStream.Create('C:\Users\Johan\Documents\UCT\ResearchStats\4x4_ancestor_slices.bin', fmCreate);
  //try
  //  Stream.WriteBuffer(AncestorTable4x4, SizeOf(AncestorTable4x4));
  //finally
  //  Stream.Free;
  //end;
  //Label1.Caption:= 'Progress: Done';
  //ShowMessage('Done: time taken = '+Timer.ElapsedMilliseconds.ToString+' ms');
  PostMessage(Application.MainFormHandle, WM_ThreadDone,0,0);
end;

procedure TForm75.CalculateAncestorCountsPerPixel(ID, Start, Eind: integer);
var
  i,a,x,y: int64;
  Pixel: integer;
  status: TGenerateStatus;
  c, S,E,SE: pbyte;
  Inner4x4: int32;
  Alive: boolean;
begin
  FillChar(AncestorTable4x4, SizeOf(AncestorTable4x4), #0);
  //Timer:= TStopWatch.StartNew;
  CellBlock[ID]:= TCellBlock.Create(0,0,0,nil);
  for x:= 0 to $FF do begin
    for a:= $000000 to $FFFFFFF do begin
      i:= (a shl 8) + x;
      CellBlock[ID].SetCore(i);
      //CellBlock.GeneratePtoQ;
      c:= @CellBlock[ID].p[17];
      S:= @c[16*4];
      E:= c +  (16*1);
      SE:= c + (16*5);
      GeneratePtoQ_AVX_32(c,S,E,SE);
      //inner 4x4 result goes to Q[17].
      Inner4x4:= CellBlock[ID].Get4x4(@CellBlock[ID].q[17],6);
      //inc(CountTable4x4[Inner4x4]);
      //**********************************************************************
      ///  i = cell input
      ///  p[17] = translated input
      ///  q[17] = raw output
      ///  Inner4x4 = translated output
      ///  The inner 4x4 need 16 masks
      ///  We need to count bits for every mask
      ///
      //**********************************************************************
      if (Inner4x4 >= Start) and (Inner4x4 <= Eind) then for Pixel:= 0 to 15 do begin
        y:= ShrinkInputToMask(i, Bitmask[pixel]);
        Inc(AncestorCountTable4x4[Inner4x4 * 16 * 512 + Pixel * 512 + y]);
      end;
    end;
    PostMessage(Application.MainFormHandle, WM_MyProgress,ID,x);
  end;
end;


procedure TForm75.FormCreate(Sender: TObject);
begin
  LifeUniverse:= TNode.Create(0,0,0,nil);
  NumberOfCores:= cNumberOfCores;
end;

procedure TForm75.StringGrid3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MousePoint : TPoint;
  Col, Row : integer;
  OldValue, NewValue: Char;
  SG: TStringGrid;
begin
  SG:= Sender as TStringGrid;
  if (Button <> mbLeft) then exit;
  {get X, Y in screen coordinates}
  MousePoint := SG.ClientToScreen(Point(X, Y));
  {get the cell clicked in}
  SG.MouseToCell(X, Y, Col, Row);
  if (SG.Cells[Col,Row] = '') then OldValue:= '?'
  else OldValue:= SG.Cells[Col, Row][1];
  NewValue:= ' ';
  case OldValue of
    '?': NewValue:= 'X';
    'X': NewValue:= '.';
    '.': NewValue:= '?';
  end;
  case NewValue of
    'X': Currentgrid.Pixel[Col, Row]:= psOn;
    '.': CurrentGrid.Pixel[Col, Row]:= psOff;
    '?': Currentgrid.Pixel[Col, Row]:= psUnknown;
  end;
  Display(SG);
  ValidateGrid;
  ProcessEntailments;
  ValidateGrid;
  Display(SG);
end;





procedure TForm75.ProcessEntailments;
var
  a,b,c: integer;
begin
  repeat
    a:= CurrentGrid.ProcessEntailments;
    b:= CurrentGrid.Past.ProcessEntailments;
    c:= CurrentGrid.Future.ProcessEntailments;
  until (a+b+c) = 0;
end;

procedure TForm75.Display(const SG: TStringGrid);
begin
  CurrentGrid.Display(SG);
  if (SG = StringGrid1) then CurrentGrid.Future.Display(StringGrid2)
  else if (SG = StringGrid2) then begin
    CurrentGrid.Past.Display(StringGrid1);
    CurrentGrid.Future.Display(StringGrid3);
  end else if (SG = StringGrid3) then begin
    CurrentGrid.Past.Display(StringGrid2);
  end;
  Application.ProcessMessages;
end;

procedure TForm75.ValidateGrid;
begin
  CurrentGrid.Validate;
  CurrentGrid.Past.Validate;
  CurrentGrid.Future.Validate;
end;


procedure TForm75.TestMapClick(Sender: TObject);
var
  a: TDoMap;
  i,r: integer;
  x,y: integer;
  yold: integer;
begin
  a.Clear;
  for i:= 0 to 1000 *1000 * 10 do begin

    //r:= 127;

    //a.Activate(r);
    x:= -1;
    y:= -1;
    repeat
      yold:= y;
      x:= a.Next(x);
      y:= a.Next(y);
      if (x <> y) and ((x < 128) or (y < 128)) then begin
        y:= a.Next(yold);
        Assert(x=y);
      end;
    until (x > 127) and (y > 127);
    r:= random(128);
    a.Invert(r);
  end;
end;

procedure TForm75.WMMyProgress(var Msg: TMessage);
begin
  case Msg.WParam of
    0: Label1.Caption:= (Msg.LParam).ToString;
    1: Label2.Caption:= (Msg.LParam).ToString;
    2: Label3.Caption:= (Msg.LParam).ToString;
    3: Label4.Caption:= (Msg.LParam).ToString;
    4: Label5.Caption:= (Msg.LParam).ToString;
    5: Label6.Caption:= (Msg.LParam).ToString;
    6: Label7.Caption:= (Msg.LParam).ToString;
    7: Label8.Caption:= (Msg.LParam).ToString;
  end;
end;

procedure TForm75.ThreadDone;
begin
  Inc(ThreadsDone);
  if (ThreadsDone = NumberOfCores) then AncestorThreadsAllDone;
end;

procedure TForm75.CountThreadDone;
begin
  NumberOfCores:= 1;
  Inc(CountThreadsDone);
  if (CountThreadsDone = NumberOfCores) then begin
    AncestorCountsThreadsAllDone;
    CountThreadsDone:= 0;
  end;
end;

{ TBit }

function TBit.GetBit(index: integer): integer;
begin
  Result:= GetABit(FData, index);
end;

class operator TBit.Implicit(const A: integer): TBit;
begin
  Result.Fdata:= A;
end;

class operator TBit.Implicit(const B: TBit): integer;
begin
  Result:= B.Fdata;
end;

class operator TBit.GreaterThanOrEqual(const A: TBit; const B: integer): boolean;
begin
  Result:= A.Fdata >= B;
end;

procedure TBit.SetBit(index: integer; const Value: integer);
begin
  SetABit(FData, index, value);
end;

{ TMyThread }

constructor TMyThread.Create(ID: integer);
begin
  inherited Create(true);
  FID:= ID;
end;

procedure TMyThread.Execute;
begin
  Form75.CalculateAncestorbitsPerPixel(FID);
  Queue(procedure begin Form75.ThreadDone end);
end;

{ TListAncestorThread }

constructor TListAncestorThread.Create(ID, Descendent: cardinal);
begin
  inherited Create(true);
  SetThreadIdealProcessor(Self.Handle, ID*2);
  FID:= ID;
  FDescendent:= Descendent;
end;

{$pointermath on}

procedure TListAncestorThread.Execute;
var
  Ancestors: TSparseSet;
  CellBlock: TCellBlock;
  i,a,b: int64;
  c,SE,E,S: pbyte;
  Inner4x4: integer;
  Stream: TFileStream;
  Filename: string;

begin
  Ancestors.Init;

  //Timer:= TStopWatch.StartNew;
  CellBlock:= TCellBlock.Create(0,0,0,nil);
  for a:= 0 to $FF do begin
   for b:= 0 to $FFFFFFF do begin
    i:= a shl (7*4) + b;
      CellBlock.SetCore(i);
      //CellBlock.GeneratePtoQ;
      c:= @CellBlock.p[17];
      S:= c + (16*4);
      E:= c +  (16*1);
      SE:= c + (16*5);
      GeneratePtoQ_AVX_32(c,S,E,SE);
      //inner 4x4 result goes to Q[17].
      Inner4x4:= CellBlock.Get4x4(@CellBlock.q[17],6);
      if (Inner4x4 = Self.Fdescendent) then Ancestors.Add(i);
    end;
    Queue(procedure var Msg: TMessage; begin
      Msg.Msg:= WM_MyProgress;
      Msg.WParam:= FID;
      Msg.LParam:= a+1;
      Form75.WMMyProgress(Msg);
    end);
  end;
  Queue(procedure begin
    Form75.Label1.Caption:= 'Thread '+FId.ToString+' Done, writing data';
  end);
  Filename:= TPath.GetDocumentsPath;
  FileName:= Filename + '\UCT\ResearchStats';
  ForceDirectories(Filename);
  Filename:= Filename + '\4x4_AncestorList_'+FDescendent.ToString+'.bin';
  Stream:= TFileStream.Create(Filename, fmCreate);
  Stream.Write(Ancestors.FTop, SizeOf(Cardinal));
  Stream.WriteBuffer(Ancestors.FStorage[0], Ancestors.FTop+1 * SizeOf(cardinal));
  Stream.WriteBuffer(Ancestors.FOverflowIndexes[0], 16*SizeOf(cardinal));
  Synchronize(procedure begin
    Form75.Label1.Caption:= 'Thread '+FId.ToString+' All Done';
  end);
end;


constructor TAncestorCountPerBitThread.Create(ID, Start, Eind: integer);
begin
  inherited Create(true);
  FID:= ID;
  FStart:= Start;
  FEnd:= Eind;
end;

procedure TAncestorCountPerBitThread.Execute;
begin
  Form75.CalculateAncestorCountsPerPixel(FID, FStart, Fend);
  Queue(procedure begin
    Form75.CountThreadDone;
  end);
end;

end.
