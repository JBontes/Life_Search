unit System.Yield;

interface

type
  TYield<T> = procedure (Value: T) of object;
  TYieldProc<T> = reference to procedure(Yield: TYield<T>);

  TYieldEnumerator<T> = class
  private
    fYieldProc: TYieldProc<T>;
    fEOF: Boolean;
    fValue: T;
    property YieldProc: TYieldProc<T> read fYieldProc;
  private
    ThreadFiber: Cardinal;
    CallerFiber: Pointer;
    CalleeFiber: Pointer;
    FiberException: Pointer;
    procedure Execute; stdcall;
    procedure Yield(aValue: T);
  public
    constructor Create(const aYieldProc: TYieldProc<T>);
    destructor Destroy; override;
  public // enumerator
    function MoveNext: Boolean;
    property Current: T read fValue;
  end;

  TYieldEnumerable<T> = record
  private
    fYieldProc: TYieldProc<T>;
    property YieldProc: TYieldProc<T> read fYieldProc;
  public
    constructor Create(const aYieldProc: TYieldProc<T>);
    function GetEnumerator: TYieldEnumerator<T>;
  end;

function GetCurrentFiber: Pointer;

implementation

uses
  Winapi.Windows;

function GetCurrentFiber: Pointer;
asm
{$IFDEF CPUX64}
  mov rax, gs:[abs $20]
{$ELSE}
  mov eax, fs:[$10]
{$ENDIF}
end;

{ TYieldEnumerator<T> }

constructor TYieldEnumerator<T>.Create(const aYieldProc: TYieldProc<T>);
var
  _Execute: procedure of object; stdcall;
  __Execute: TMethod absolute _Execute;
begin
  fYieldProc := aYieldProc;
  inherited Create;
  ThreadFiber := ConvertThreadToFiber(nil);
  CallerFiber := GetCurrentFiber;
  _Execute := Execute;
  CalleeFiber := CreateFiber(0, __Execute.Code, __Execute.Data);
end;

destructor TYieldEnumerator<T>.Destroy;
begin
  DeleteFiber(CalleeFiber);
  if ThreadFiber <> 0 then
  begin
    ConvertFiberToThread;
  end;
  inherited;
end;

procedure TYieldEnumerator<T>.Execute;
begin
  try
    YieldProc(Yield);
  except
    FiberException := AcquireExceptionObject;
  end;
  SwitchToFiber(CallerFiber);
end;

function TYieldEnumerator<T>.MoveNext: Boolean;
begin
  if fEOF then
  begin
    Exit(False);
  end;
  fEOF := True;
  SwitchToFiber(CalleeFiber);
  if FiberException <> nil then
  begin
    raise TObject(FiberException);
  end;
  Result := not fEOF;
end;

procedure TYieldEnumerator<T>.Yield(aValue: T);
begin
  fEOF := False;
  fValue := aValue;
  SwitchToFiber(CallerFiber);
end;

{ TYieldEnumerable<T> }

constructor TYieldEnumerable<T>.Create(const aYieldProc: TYieldProc<T>);
begin
  fYieldProc := aYieldProc;
end;

function TYieldEnumerable<T>.GetEnumerator: TYieldEnumerator<T>;
begin
  Result := TYieldEnumerator<T>.Create(YieldProc);
end;

end.
