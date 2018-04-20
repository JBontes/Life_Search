unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Timer1: TTimer;
    PaintBox1: TPaintBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  LifeCell, SparseArray, LifeList;

var
  Universe: TUniverse;

procedure TForm1.Button1Click(Sender: TObject);
var
  Cell: TLifeCell;
begin
  Universe:= TUniverse.Create;
  FillChar(Cell, SizeOf(Cell), #0);
  Cell.Clear(PCycle);
  //Cell.p[0]:= $000701020000;
  Cell.p[0]:= $0007000000;
  Cell.Coordinate:= Point(64,40);
  Universe.Add(Cell);
  while true do begin
    Universe.Display(PaintBox1.Canvas);
    Universe.SimpleGeneration(Rect(0,0,100,100));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Cell: TLifeCell;
begin
  Cell.Display(nil, 0,0,100, PCycle);
end;

end.
