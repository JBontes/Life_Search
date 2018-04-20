unit Drawing;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm35 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  const
    width = 1600;
    height = 900;
  private
    bmpinfo: PBitmapInfo;
    data_buffer: PCardinal;
    procedure TestSetMonochromeBitmap(Canvas: TCanvas);
    procedure FreeMyMem;
    procedure InitMem(Width, Height: integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form35: TForm35;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

procedure TForm35.InitMem(Width, Height: integer);
var
  ScanLineWidth: integer;
  color: PRGBQuad;
begin
  ScanlineWidth:= width div 8;
  if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);
  GetMem(bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD));
  GetMem(data_buffer, ScanlineWidth * height);
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

procedure TForm35.FreeMyMem;
begin
  FreeMem(data_buffer);
  FreeMem(bmpinfo);
end;

procedure TForm35.TestSetMonochromeBitmap(Canvas: TCanvas);
var
  color: PRGBQUAD;
  setter: PCardinal;
  ScanlineWidth: Integer;
  i: Integer;
  y,h: integer;
begin
  //GetMem(bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD));
//  color:= @bmpinfo^.bmiColors[0];
//  color^.rgbRed:= 255;
//  color^.rgbBlue:= 255;
//  color^.rgbGreen:= 255;
//  color^.rgbReserved:= 0;
//  Inc(color);
//  color^.rgbRed:= 0;
//  color^.rgbBlue:= 0;
//  color^.rgbGreen:= 0;
//  color^.rgbReserved:= 0;
//
//  with bmpinfo.bmiHeader do begin
//    biSize:= SizeOf(bmpinfo.bmiHeader);
//    biWidth:= width;
//    biHeight:= height;
//    biPlanes:= 1;
//    biBitCount:= 1;
//    biCompression:= BI_RGB;
//    biSizeImage:= 0;
//    biXPelsPerMeter:= 0;
//    biYPelsPerMeter:= 0;
//    biClrUsed:= 0;
//    biClrImportant:= 0;
//  end;

  ScanlineWidth:= width div 8;
  if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);

  //GetMem(data_buffer, ScanlineWidth * height);

  { Set checkerboard pattern }
  setter:= data_buffer;
  for i:= 1 to ((ScanlineWidth * height) div 4) do begin
    if odd((i*4) div ScanLineWidth)  then setter^:= $55555555
    else setter^:= $AAAAAAAA;

    Inc(setter);
  end;
//  function SetDIBitsToDevice(DC: HDC; DestX, DestY: Integer; Width, Height: DWORD;
//  SrcX, SrcY: Integer; nStartScan, NumScans: UINT; Bits: Pointer;
//  var BitsInfo: TBitmapInfo; Usage: UINT): Integer; stdcall;

  //SetDIBitstoDevice(Canvas.Handle, 0, 0, width, height, 0, 0, 0, height, data_buffer, bmpinfo^, DIB_RGB_COLORS);
  //Banding
  h:= height div 4;
  y:= 0;

  StretchDIBits(Canvas.Handle, 0,0,800,800,0,0,100,100,data_buffer, bmpinfo^, DIB_RGB_COLORS, SRCCOPY);

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
  //FreeMem(bmpinfo);
end;

procedure TForm35.Button1Click(Sender: TObject);
var
  I: Integer;
  watch: TStopWatch;
begin
  InitMem(width,height);
  Watch:= TStopwatch.StartNew;

  for I := 0 to 100 do
  TestSetMonochromeBitmap(Form35.Canvas);

  Watch.Stop;
  FreeMyMem;
  Button1.Caption:= IntToStr(watch.ElapsedMilliseconds)+' ms';
end;

end.
