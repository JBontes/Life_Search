program SatProject;

uses
  Vcl.Forms,
  SATForm in 'SATForm.pas' {Form76},
  SatSolve in 'SatSolve.pas',
  System.Yield in 'System.Yield.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm76, Form76);
  Application.Run;
end.
