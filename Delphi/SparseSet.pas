unit SparseSet;

interface

type
  /// <summary>
  ///  A sparse set, elements can only be added in increasing order
  /// </summary>
  TSparseSet = record
  public
    FTop: cardinal;
    FMostRecentIndex: cardinal;
    FMostRecentElement: Int64;
    FOverflowIndexes: array[0..16] of cardinal;
    FStorage: array of cardinal;  //must always be the last element.
  public
    procedure Add(a: int64);
    class operator In(A: int64; var b: TSparseSet): boolean;
    procedure Init;
  end;



implementation

{ TSparseSet }

procedure TSparseSet.Add(a: int64);
var
  Previous: cardinal;
  Offset: cardinal;
  a_high: cardinal;
  MostRecentHigh: cardinal;
begin
  //Grow storage by 1.5 on overflow
  if FTop = length(FStorage) then SetLength(FStorage, FTop + (FTop shr 2));

  MostRecentHigh:= (FMostRecentElement shr 32);
  a_high:= (a shr 32);
  //Do we start a new series?
  if (a_high > MostRecentHigh) then begin
    FOverFlowIndexes[a_high]:= FTop;   //mark the start of the series
  end;
  FStorage[FTop]:= a;
  Inc(FTop);
end;

function Max(a,b: cardinal): cardinal;
asm
  cmp a,b
  mov eax,a
  cmovc eax,b
end;

function Min(a,b: cardinal): cardinal;
asm
  cmp a,b
  mov eax,a
  cmovnc eax,b
end;

class operator TSparseSet.In(A: int64; var b: TSparseSet): boolean;
var
  i: cardinal;
  a_low: cardinal;
  a_high: cardinal;
  StartPoint: cardinal;
  EndPoint: cardinal;
  Element: cardinal;
begin
  a_low:= a;
  a_high:= (a shr 32);
  StartPoint:= b.FOverflowIndexes[a_high];

  if (StartPoint = $FFFFFFFF) then exit(false);
  i:= Max(b.FMostRecentIndex, StartPoint);
  if (A < i) then i:= StartPoint
  else if (A = b.FMostRecentElement) then exit(true);
  EndPoint:= Min(b.FOverflowIndexes[a_high+1], b.FTop);
  while i < EndPoint do begin
    inc(i);
    if (a_low > b.FStorage[i]) then exit(false)
    else if (b.FStorage[i] = a_low) then begin
      b.FMostRecentIndex:= i;
      b.FMostRecentElement:= A;
      exit(true);
    end;
  end;
  Result:= false;
end;

procedure TSparseSet.Init;
begin
  FillChar(self, SizeOf(self) - SizeOf(pointer), #0);
  FillChar(self.FOverflowIndexes[1], SizeOf(cardinal)*16, $FF);
  SetLength(Self.FStorage, 6144);
end;

end.
