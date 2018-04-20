unit UnitTests;

interface
uses
  DUnitX.TestFramework, Slices;

type

  [TestFixture]
  SliceTests = class(TObject)
  private
    FTSlice: TSlice;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure TestCountingAllowedStates;

    [Test]
    function Test4x4to2x2: boolean;
    [Test]
    procedure TestSliceGetNextOption;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation

uses
  VCL.Dialogs,
  Universe;

function SliceTests.Test4x4to2x2: boolean;
var
  i,a,x,y, xoff,yoff: integer;
  inputgrid: array[0..3,0..3] of integer;
  outputgrid: array[1..2,1..2] of integer;
  Soll, Ist: integer;
  TestSubject: TCellBlock;
begin
  TestSubject:= TCellBlock.Create(0,0,nil);
  for i:= 0 to $FFFF do begin
    Soll:= 0;
    FillChar(outputgrid, SizeOf(outputgrid), #0);
    a:= i;
    for y:= 0 to 3 do for x:= 0 to 3 do begin
      if (Odd(a)) then inputgrid[x,y]:= 1 else inputgrid[x,y]:= 0;
      a:= a shr 1;
    end;
    for y:= 1 to 2 do for x:= 1 to 2 do begin
      for yoff:= -1 to 1 do for xoff:= -1 to 1 do begin
        Inc(outputgrid[x,y], inputgrid[x+xoff,y+yoff]);
      end;
    end; {for yx}
    for y:= 1 to 2 do for x:= 1 to 2 do begin
      if (outputgrid[x,y] = 3) or ((outputgrid[x,y] = 4) and (inputgrid[x,y]=1)) then begin
        Soll:= Soll or (1 shl ((y-1)*2+(x-1)));
      end;
    end;
    TestSubject.SetMiniCore(i);
    TestSubject.GeneratePtoQ;
    Ist:= TestSubject.Get2x2(@TestSubject.q[17],6);
    if (Soll <> Ist) then begin
      Assert.IsTrue(Soll = Ist);
    end;
  end; {for i}
end;

procedure SliceTests.TestCountingAllowedStates;
var
  i: integer;
  S: TSlice;
  C1,C2: Slices.TBitCounts;
begin
  for i:= 0 to $FFFF do begin
    S:= TSlice.FillWithRandomData;
    C1:= S.GetBitCounts;
    C2:= S.TestGetBitCounts;
    if (C1 <> C2) then begin
      Assert.IsTrue(C1 = C2);
    end;
  end;
end;

procedure SliceTests.Setup;
begin
end;

procedure SliceTests.TearDown;
begin
end;

procedure SliceTests.TestSliceGetNextOption;
var
  A: TSlice;
  i: integer;
  x,y: integer;
begin
  for i:= 0 to $FFFFF do begin
    A:= TSlice.FillWithRandomData;
    x:= -1; y:= -1;
    repeat
      x:= A.GetNextOption(x);
      y:= A.TestGetNextOption(y);
      if (x <> y) then begin
        Assert.IsTrue((x=y) or ((x>255) and (y>255)));
      end;
    until x > 255;
  end;
end;

procedure SliceTests.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(SliceTests);
end.
