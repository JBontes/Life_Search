unit Unit64;

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls,
  {$ifdef Win64}
  Windows
  {$endif};

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




  { TUnit }

const
  BytesPerUnit = 16;
  MaxBytesPerUnit = BytesPerUnit -1;


type

  { TDoMap }

  TDoMap = record
  strict private
    a: array[0..MaxBytesPerUnit] of byte;
  public
    function Next(previous: integer): integer;
    procedure Activate(index: integer);
    procedure Deactivate(index: integer);
  end;

  PUnit = ^TUnit;
  TUnit = record
    procedure Display(const BitmapStart: pointer; const LineOffset: integer);
    case integer of
      1: (b: array[0..MaxBytesPerUnit] of byte);
      2: (w: array[0..(BytesPerUnit div 2)-1] of word);
      3: (i: array[0..(BytesPerUnit div 4)-1] of integer);
      4: (q: array[0..(BytesPerUnit div 8)-1] of int64);
  end;

const
  UnitsPerBlock = 16*8;

type

  PCellBlock = ^TCellBlock;

  { TCellBlock }

  TCellBlock = record
  public
    procedure DisplayQ(const DrawSurface: TDIBits);
    procedure DisplayP(const DrawSurface: TDIBits);
  public
    p: array[0..UnitsPerBlock-1] of TUnit;
    q: array[0..UnitsPerBlock-1] of TUnit;
  end;

  { CellBlockHelper }

  CellBlockHelper = record helper for PCellBlock
    class function New: PCellBlock; static;
    procedure Free;
  end;

  { TForm64 }

  TForm64 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Timer1: TTimer;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCellBlock: TCellBlock;
    FDrawSurfaceP: TDIBits;
    FDrawSurfaceQ: TDIBits;
    FEmpty: TUnit;
    Generation: integer;
    procedure Generate;
    procedure Draw;
    procedure GeneratePtoQ;
    procedure GenerateQtoP;
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
{$ifdef FPC}

  AVXGenerate;
{$else}
  ;
 {$L 'C:\Users\Johan\Documents\Embarcadero\Studio\Projects\Life64\Lazarus\lib\x86_64-win64\AVXGenerate.o'}

function GenerateQtoP_AVX(main, N,W,NW: pointer): byte;
  external name 'AVXGENERATE_$$_GENERATEQTOP_AVX$POINTER$POINTER$POINTER$POINTER$$BYTE';
function GeneratePtoQ_AVX(main, S,E,SE: pointer): byte;
  external name 'AVXGENERATE_$$_GENERATEPTOQ_AVX$POINTER$POINTER$POINTER$POINTER$$BYTE';
{$endif}


{ TForm64 }

{$a16}

function RDTSC: int64;
{$IFDEF CPUX64}
asm
  {$IFDEF AllowOutOfOrder}
  rdtsc
  {$ELSE}
    rdtscp       //rdstcp is read serialized, it will not execute too early.
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
    {$ELSE}
  error !
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;


//todo: extend for the full unit.
function ReverseBitsInAllBytes(input: int64):int64;
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
  //mov rcx,rax                       ////test code: see if reversal works correct.
    //register usage
    //rcx: input64
    //output: rax same byte order, but all bits in every byte reversed.
    //push    rdi
    //push    rsi
    //mov     rax, rcx
    //movabs  rcx, 1229782938247303441
    //movabs  r8, 17590038562815
    //rol     rax, 31
    //mov     rdx, rax
    //shr     rdx, 20
    //xor     rdx, rax
    //and     rdx, r8
    //mov     r8, rdx
    //sal     r8, 20
    //or      r8, rdx
    //movabs  rdx, 69805860803577863
    //xor     r8, rax
    //mov     rax, r8
    //shr     rax, 8
    //xor     rax, r8
    //and     rdx, rax
    //mov     rax, rdx
    //sal     rax, 8
    //or      rax, rdx
    //movabs  rdx, 578836249331134472
    //xor     r8, rax
    //mov     rax, r8
    //shr     rax, 4
    //xor     rax, r8
    //and     rdx, rax
    //mov     rax, rdx
    //sal     rax, 4
    //or      rax, rdx
    //xor     rax, r8
    //mov     rdx, rax
    //shr     rdx, 2
    //xor     rdx, rax
    //and     rdx, rcx
    //lea     rcx, [rdx*4]
    //or      rdx, rcx
    //xor     rax, rdx
    //bswap   rax
    //pop     rsi
    //pop     rdi
//  xor eax,eax
//  mov edx,64
//@loop:
//  shr rcx,1
//  rcl rax,1
//  dec edx
//  jnz @loop
//  bswap rax
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
  mov r8d,64
  mov r10,[rcx+8]   //speculatively load part 2, we may need it if part 1 is zero.
  tzcnt r11,r10     //count the bits in part 2 in case part 1 is zero.
  lea r11,[r11+64]  //add 64 because this *is* part 2
  xor rax,rax       //zero rax
  inc edx           //increase the counter
  mov r11,rdx       //clone the counter
  bts rax,rdx       //Set the selected bit
  xor rdx,rdx
  sub rdx,rax       //Mask with all the bits >= bit number set and all lower bits reset.
  cmp r11d,r8d      //are we in part 1 or 2?
  cmovl r9,[rcx]    //in part 1, load part 1
  mov ecx,0         //start counting from 0
  cmovge r9,r10     //otherwise load part 2
  cmovge ecx,r8d    //stat counting from 64
  and r9,rdx        //mask off the bits we don't need
  tzcnt rax,r9      //count the bits in r9
  cmovc rax,r11     //if the input is zero, use the count from part 2 instead.
  add eax,ecx       //add 64 if we were in part 2.
  ret
  //There are 4 options.
  //1: in part 1 and remaining bits not zero: mask and count p1 and return count + 0
  //2: in part 2 and remaining bits not zero: mask and count p2 and return count + 64
  //3: in part 1 and remaining bits zero: return count2 + 64 + 0
  //4: in part 2 and remaining bits zero: return count2 + 64 + 64 = overflow.
end;

procedure TDoMap.Activate(index: integer);
begin

end;

procedure TDoMap.Deactivate(index: integer);
begin

end;



{ TCellBlock }

{$pointermath on}

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
  if Odd(Generation) then GenerateQtoP
  else GeneratePtoQ;
  Inc(Generation);
end;

procedure TForm64.Draw;
var
  x,y: integer;
begin
  case Odd(Generation) of
    false: begin
      FCellBlock.DisplayP(FDrawSurfaceP);
      //function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
      //SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
      //Usage: UINT; Rop: DWORD): Integer; stdcall;
      StretchDIBits(Form64.PaintBox1.Canvas.Handle, 4,4,128*4,128*4,0,0,128,128, FDrawSurfaceP.data, FDrawSurfaceP.bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
      //for x:= 0 to 7 do begin
      //  PaintBox1.Canvas.Line(x*16*4+4,4,x*16*4+4,128*4+4);
      //end;
      //for y:= 0 to 15 do begin
      //  PaintBox1.Canvas.Line(4,y*8*4+4,128*4+4,y*8*4+4);
      //end;
    end;
    true: begin
      FCellBlock.DisplayQ(FDrawSurfaceQ);
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

procedure TForm64.GeneratePtoQ;
var
  i: integer;
  N,W,NW: PUnit;
  NIndex, WIndex, NWIndex: integer;
  R,S,T,U,V: integer;
begin
  for i:= 127 downto 0 do begin
    NIndex:= i - 8;
    WIndex:= i - 1;
    NWIndex:= i - 9;
    N:= @FCellBlock.p[NIndex];
    W:= @FCellBlock.p[WIndex];
    NW:= @FCellBlock.p[NWIndex];
    if i < 8 then begin
      N:= @FEmpty;
      NW:= @FEmpty;
    end;
    if not(boolean(i and 7)) then begin
      W:= @FEmpty;
      NW:= @FEmpty;
    end;
    GeneratePtoQ_AVX(@FCellBlock.p[i],N,W,NW);
  end;
  //R:= 1 shl Random(8);
  //S:= Random(16);
  //T:= Random(128);
  //FCellBlock.q[T].b[S]:= FCellBlock.q[T].b[S] or R;
end;

procedure TForm64.GenerateQtoP;
var
  i: integer;
  S,E,SE: PUnit;
  Sindex, EIndex, SEIndex: integer;
begin
  for i:= 0 to 127 do begin
    SIndex:= i + 8;
    EIndex:= i + 1;
    SEIndex:= i + 9;
    S:= @FCellBlock.q[SIndex];
    E:= @FCellBlock.q[EIndex];
    SE:= @FCellBlock.q[SEIndex];
    if i > 120 then begin
      S:= @FEmpty;
      SE:= @FEmpty;
    end;
    if (i and 7) = 7 then begin
      E:= @FEmpty;
      SE:= @FEmpty;
    end;
    GenerateQtoP_AVX(@FCellBlock.q[i],S,E,SE);
  end;
end;



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
  mov r10,[rcx+8]
  mov rcx,[rcx]
  push rdx
  call ReverseBitsInAllBytes
  pop rdx
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr eax,16
  mov [rdx+r8],ax
  lea rdx,[rdx+r8*2]
  push rdx
  mov rcx,r10
  call ReverseBitsInAllBytes
  pop rdx
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr eax,16
  mov [rdx+r8],ax
end;


procedure TForm64.Button1Click(Sender: TObject);
var
  i: integer;
begin
  //Timer1.Enabled:= not Timer1.Enabled;
  Draw;
  for i:= 0 to 10000 do begin
    if (i and $7) = 0 then Draw;
    Generate;
  end;
  Draw;
end;

procedure TForm64.Button2Click(Sender: TObject);
var
  i: int64;
  a: int64;
begin
  for i:= 0 to $FF do begin
    a:= ReverseBitsInAllBytes(i);
    Assert(a = i);
  end;
end;


procedure TForm64.FormCreate(Sender: TObject);
var
  i,j: integer;
begin
  FDrawSurfaceP:= TDIBits.Create;
  FDrawSurfaceQ:= TDIBits.Create;
  if SizeOf(TCellBlock) <> 1024*4 then ShowMessage('oops');
  for i:= 0 to 127 do begin
    for j:= 0 to 15 do begin
      FCellBlock.p[i].b[j]:= Random(255);
    end;
  end;
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
  //FCellBlock.p[11].b[3]:= 2;
  //FCellBlock.p[11].b[5]:= 1;
  //FCellBlock.p[11].b[7]:= 7;

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

end.


end.

