unit Display;

interface

uses
  WinAPI.Windows,
  VCL.Graphics;

type
  ///  TDiBits holds the bitmap to be displayed on screen.
  ///  It is always a multiple of 128x128 bits.
  ///  As such it always holds a clean multiple of the CellBlocks.
  ///
  TDiBits = record
  public
    function Stride: integer; inline;
  public
    bmpinfo: PBitmapInfo;
    data: PByte;
    width, height: cardinal;
  end;

  TBitmaps = class(TObject)
  private
    FPBitmap: TDIBits;
    FQBitmap: TDIBits;
    FXoffset, FYOffset: integer;
  end;

function InitBitmap(Width, Height: integer): TDIBits;
procedure FreeBitmap(Bitmap: TDIBits);
procedure DisplayBitmap(const Canvas: TCanvas; const Rect: TRect; const Bitmap: TDIBits);

implementation

function InitBitmap(Width, Height: integer): TDIBits;
var
  ScanLineWidth: integer;
  color: PRGBQuad;
begin
  Result.width:= Width;
  Result.height:= Height;
  ScanlineWidth:= width div 8;
  if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);
  GetMem(Result.bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD));
  GetMem(Result.data, ScanlineWidth * height);
  color:= @Result.bmpinfo^.bmiColors[0];
  color^.rgbRed:= 255;
  color^.rgbBlue:= 255;
  color^.rgbGreen:= 255;
  color^.rgbReserved:= 0;
  Inc(color);
  color^.rgbRed:= 0;
  color^.rgbBlue:= 0;
  color^.rgbGreen:= 0;
  color^.rgbReserved:= 0;

  with Result.bmpinfo.bmiHeader do begin
    biSize:= SizeOf(Result.bmpinfo.bmiHeader);
    biWidth:= width;
    biHeight:= height;
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

procedure FreeBitmap(Bitmap: TDIBits);
begin
  FreeMem(Bitmap.bmpinfo);
  FreeMem(Bitmap.data);
end;

procedure DisplayBitmap(const Canvas: TCanvas; const Rect: TRect; const Bitmap: TDIBits);
var
  color: PRGBQUAD;
  setter: PByte;
  ScanlineWidth: Integer;
  i: Integer;
  y,h: integer;
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
  h:= Bitmap.height div 4;
  y:= 0;

  StretchDIBits(Canvas.Handle, 0,0,800,800,0,0,100,100, Bitmap.data, Bitmap.bmpinfo^, DIB_RGB_COLORS, SRCCOPY);
end;

{ TDiBits }



function TDiBits.Stride: integer;
begin
  Result:= width shr 3;
end;

end.
