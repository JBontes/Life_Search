unit SatSolve;

interface

uses System.Yield, System.Generics.Collections;

type

  ///  Some notes on encoding
  ///  Variables are encoded as number from 0 to n-1.
  ///  Literal v is encoded as 2*v and not(v) is encoded as 2*v+1.
  ///  Any v is thus even and any not(v) is thus odd.
  ///

  TLiteral = record
  strict private
    FData: integer;
  public
    class operator Implicit(a: integer): TLiteral; inline;
    class operator LogicalNot(a: TLiteral): TLiteral; inline;
    class operator Implicit(a: TLiteral): boolean; inline;
    class operator Implicit(a: TLiteral): integer; inline;

  end;

    TAssignment = TArray<integer>;

  TIntegerArray = TArray<integer>;
  TClauseArray = TArray<TIntegerArray>;
  TStringArray = TArray<string>;

  TInstance = record
  public
    Variables: array of integer;
  end;

  TWatchList = record
  private
    Variables: TArray<TArray<TIntegerArray>>;
    function GetItem(i, j, k: integer): integer;
    procedure SetItem(i, j, k: integer; const Value: integer);
    function GetWatchedVars(i, j: integer): TIntegerArray;
    procedure SetWatchedVars(i, j: integer; const Value: TIntegerArray);
  public
    procedure Add(WatchedVar: integer; const Clause: TClauseArray);
    property Items[i,j,k: integer]: integer read GetItem write SetItem; default;
    property WatchedVars[i,j: integer]: TIntegerArray read GetWatchedVars write SetWatchedVars; default;
  end;

  TSolver = class(TObject)
  private
    variables: TStringArray;
    variable_table: TDictionary<string, integer>;
    clauses: TClauseArray;
  private
    function ClauseToString(Clause: TIntegerArray): string;
    function LiteralToString(Literal: integer): string;
    function AssignmentToString(Assignment: TAssignment; Brief: boolean = false; const StartWith: string = ''): string;
    function SetupWatchlist: TWatchList;
  public
    procedure ParseAndAddClause(const Line: string);
    function Solve(Watchlist: TWatchList; assignment: TAssignment; StartFrom: integer; verbose: boolean): TList<TAssignment>;
    function Update_Watchlist(WatchList: TWatchList; false_literal: integer; Assignment: TAssignment; verbose: boolean): boolean;
    constructor Create;
    constructor CreateFromFile(const AFilename: string);
  end;

const
  None = 0;

type
  TArrayHelper = record helper for TStringArray
    function Append(const element: string): TStringArray;
  end;

  TIntegerArrayHelper = record helper for TIntegerArray
    function Append(const element: integer): TIntegerArray; overload;
    function Append(const elements: TIntegerArray): TIntegerArray; overload;
  end;

  TClauseArrayHelper = record helper for TClauseArray
    function Append(const element: TIntegerArray): TClauseArray;
  end;



//  TStringHelper = record helper for string
//    class function &&op_multiply(const a: string; b: cardinal): string; static;
//  end;


implementation

uses
  StrUtils, System.Classes, SysUtils;

{ TVar }

class operator TLiteral.Implicit(a: integer): TLiteral;
begin
  Result.FData:= a shl 2;
end;

class operator TLiteral.Implicit(a: TLiteral): boolean;
begin
  Result:= not(Odd(a.FData));
end;

class operator TLiteral.Implicit(a: TLiteral): integer;
begin
  Result:= a.FData shr 1;
end;

class operator TLiteral.LogicalNot(a: TLiteral): TLiteral;
begin
  Result.FData:= (a.FData xor 1);
end;



procedure TSolver.ParseAndAddClause(const Line: string);
var
  Clause: TIntegerArray;
  Literal: string;
  Str: string;
  Negated: integer;
  Encoded_literal: integer;
  Separator: TArray<char>;
  Variable: integer;
begin
  Separator:= [' '];
  for literal in Line.Split(Separator) do begin
    Negated:= integer(literal[1] = '~');
    str:= Literal.Substring(1+Negated);
    if (variable_table.TryGetValue(Str, Variable)) then
    if not(variable_table.ContainsKey(Str)) then begin
      Variable:= Length(Variables);
      variable_table.Add(Str, Variable);
      Variables.Append(Str);
    end;
    Encoded_Literal:= (Variable * 2) or Negated;
    Clause.Append(Encoded_Literal);
    //assume we never have duplicate variables in a clause
    Clauses.Append(Clause);
  end;
end;

constructor TSolver.Create;
begin
  inherited Create;
  Self.variable_table:= TDictionary<string, integer>.Create;
end;

constructor TSolver.CreateFromFile(const AFilename: string);
var
  Lines: TStringList;
  Line: string;
begin
  Create;
  Lines:= TStringList.Create;
  Lines.LoadFromFile(AFilename);
  for Line in Lines do begin
    if (Line.Trim <> '') and (Line[1] <> '#') then begin
      ParseAndAddClause(Line.Trim);
    end;
  end;
end;

function TSolver.LiteralToString(Literal: integer): string;
begin
  if (Odd(Literal)) then Result:= '~'+(Literal shr 1).ToString
  else Result:= Literal.ToString;
end;

function TSolver.ClauseToString(Clause: TIntegerArray): string;
var
  i: integer;
begin
  for i in Clause do begin
    Result:= Result + ' ' + LiteralToString(i);
  end;
end;

function Min(a,b: integer): integer; inline;
begin
  Result:= a xor ((a xor b) and -integer(a > b));
end;

function TSolver.AssignmentToString(Assignment: TAssignment; Brief: boolean = false; const StartWith: string = ''): string;
var
  Literals: string;
  i: integer;
begin
  for i:= 0 to Min(Length(Assignment), Length(variables)) do if (StartWith = '') or (pos(StartWith, Variables[i]) = 1) then begin
    if (Assignment[i] = 0) and not brief then Literals:= Literals + '~' + Variables[i]
    else if (Assignment[i] <> 0) then Literals:= Literals + Variables[i];
  end;
  Result:= ' '+Literals;
end;

function TSolver.SetupWatchlist: TWatchList;
var
  Clause: TIntegerArray;
begin
  SetLength(Result.Variables, Length(Self.variables) * 2);
  for clause in Clauses do begin
    Result.Add(Clause[0], Clause);
  end;
end;

function TSolver.Solve(Watchlist: TWatchList; assignment: TAssignment; StartFrom: integer; verbose: boolean): TList<TAssignment>;
var
  n: integer;
  state: array of byte;
  tried_something: boolean;
  a: integer;
begin
  n:= Length(Variables);
  SetLength(state, n);   //start with all zeros
  while true do begin
    if (StartFrom = n) then Result.Add(Assignment);
    Dec(StartFrom);
    continue;

    tried_something:= false;
    for a:= 0 to 1 do begin
      if ((state[StartFrom] shr 1) and 1) = 0 then begin
        if (Verbose) then ; //print something.
        tried_something:= true;
        state[StartFrom]:= state[StartFrom] or (1 shl a);
        assignment[StartFrom]:= a;
        if (not(update_watchlist(WatchList, (StartFrom shl 1) or a, assignment, verbose))) then Assignment[StartFrom]:= None
        else begin
          Inc(StartFrom);
          Break;
        end; {else}
      end; {if}
    end; {for}
    if (not(tried_something)) then begin
      if (StartFrom = 0) then exit
      else begin
        state[StartFrom]:= 0;
        assignment[StartFrom]:= None;
        Dec(StartFrom);
      end;
    end;
  end; {while}
end;



{ TWatchList }


function TSolver.Update_Watchlist(Watchlist: TWatchList; false_literal: integer; Assignment: TAssignment; verbose: boolean): boolean;
begin

end;



{ THelper }

function TIntegerArrayHelper.Append(const element: integer): TIntegerArray;
var
  L: NativeUInt;
begin
  L:= Length(Self);
  SetLength(Self, L+1);
  Self[L]:= element;
end;

{ TWatchList }

procedure TWatchList.Add(WatchedVar: integer; const Clause: TClauseArray);
var
  L: cardinal;
begin
  L:= Length(Self.Variables[WatchedVar]);
  SetLength(Self.Variables[WatchedVar], L+1);
  Self.Variables[WatchedVar][L]:= Clause;
end;

function TWatchList.GetItem(i, j, k: integer): integer;
begin

end;

function TWatchList.GetWatchedVars(i, j: integer): TIntegerArray;
begin

end;

procedure TWatchList.SetItem(i, j, k: integer; const Value: integer);
begin

end;

procedure TWatchList.SetWatchedVars(i, j: integer; const Value: TIntegerArray);
begin

end;

{ TClauseArrayHelper }

function TClauseArrayHelper.Append(const element: TIntegerArray): TClauseArray;
var
  L: NativeUInt;
begin
  L:= Length(Self);
  SetLength(Self, L+1);
  Self[L]:= element;
end;


{ TArrayHelper }

function TArrayHelper.Append(const element: string): TStringArray;
var
  L: NativeUInt;
begin
  L:= Length(Self);
  SetLength(Self, L+1);
  Self[L]:= element;
end;

function TIntegerArrayHelper.Append(const elements: TIntegerArray): TIntegerArray;
var
  L: NativeInt;
begin
  L:= Length(Self);
  SetLength(Self, L + Length(elements));
  Move(Elements[0], Self[L], Length(elements) * SizeOf(Integer));
end;

end.
