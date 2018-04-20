program Life64;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LifeCell in 'LifeCell.pas',
  LifeList in 'LifeList.pas',
  SparseArray in 'SparseArray.pas',
  LifeTypes in 'LifeTypes.pas',
  Universe in 'Universe.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
