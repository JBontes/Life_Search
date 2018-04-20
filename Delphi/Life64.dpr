program Life64;

{$define GUI}

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form75},
  Universe in 'Universe.pas',
  SparseSet in 'SparseSet.pas',
  Slices in 'Slices.pas';

{$R *.res}



begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm75, Form75);
  Application.Run;
end.
