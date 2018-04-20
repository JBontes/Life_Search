unit Unit1;

interface

type
  /// <summary>
  ///  A sparse set, elements can only be added in increasing order
  /// </summary>
  TSparseSet = record
  private
    FIndex: cardinal;
    FMostRecentIndex: cardinal;
    FMostRecentElement: Int64;
    FOverflowIndexes: array[0..16] of cardinal;
    FStorage: array of cardinal;  //must always be the last element.
    class operator Add(a: cardinal; const b: TSparseSet): TSparseSet;
    class operator In(A: cardinal; const b: TSparseSet): boolean;
    class function Init: TSparseSet; static;
  end;



implementation

{ TSparseSet }

class operator TSparseSet.Add(a: int64; const b: TSparseSet): TSparseSet;
var
  Previous: cardinal;
  Offset: cardinal;
  a_high: cardinal;
  MostRecentHigh: cardinal;
begin
  MostRecentHigh:= (b.FMostRecentElement shr 32);
  a_high:= (a shr 32);
  if (a_high > MostRecentHigh) then begin
    FOverFlowIndexes[a_high]:= FIndex;
  end;
  FStorage[FIndex]:= a;
  Inc(FIndex);
end;

function Max(a,b: cardinal): cardinal;
asm
  cmp a,b
  mov rax,a
  cmovc rax,b
end;

function Min(a,b: cardinal): cardinal;
asm
  cmp a,b
  mov rax,a
  cmovnc rax,b
end;

class operator TSparseSet.In(A: int64; const b: TSparseSet): boolean;
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
  EndPoint:= Min(b.FOverflowIndexes[a_high+1], FTop);
  while i <= EndPoint do begin
    inc(i);
    if (a_low > b.FStorage[i]) then exit(false)
    else if (b.FStorage[i] = a_low) then begin
      b.FMostRecentElement:= Element;
      b.FMostRecentIndex:= i;
      exit(true);
    end;
  end;
  Result:= false;
end;

class function TSparseSet.Init: TSparseSet;
begin
  FillChar(Result, SizeOf(Result) - SizeOf(pointer), #0);
  FillChar(Result.FOverflowIndexes[1], SizeOf(cardinal)*16, $FF);
end;

end.
