program Life64TestBed;

uses
  Vcl.Forms,
  TestBed in 'TestBed.pas' {Form34};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm34, Form34);
  Application.Run;
end.
