unit LifeTypes;

interface

uses
  System.Types;

type
  TCoordinate = record
    ///  The coordinate is set up as follows:
    ///  low 16 bits - x coordinate in the block.
    ///  high 16 bits - y coordinate in the block
    ///  Coordinates follow screen conventions: topleft (NW) = 0,0
    ///  South, East = +, North, West = -
  private const
    Mask = $FFFF;
    Shift = 16;
    BlockShift = Shift - 6;
    BlockMask = (63 shl 6);
  private
    Fdata: cardinal;
  public
    class operator Implicit(a: TCoordinate): TPoint; inline;
    class operator Implicit(a: TPoint): TCoordinate; inline;
    class operator In(a: TCoordinate; const b: TRect): boolean; inline;
    class operator Subtract(a,b: TCoordinate): integer; inline;
    class operator Add(a, b: TCoordinate): integer; inline;
    function NeighborIndex: integer; inline;
    function x: integer; inline;
    function y: integer; inline;
    function BlockIndex: integer; inline;
    function SetData(x,y: integer): integer; inline;
    procedure GetData(var x,y: integer); inline;
    function Offset(Offset: TPoint): TCoordinate; inline;
    function Max(a,b: TCoordinate): TCoordinate; inline;
    function N: TCoordinate; inline;
    function W: TCoordinate; inline;
    function NW: TCoordinate; inline;
    function S: TCoordinate; inline;
    function E: TCoordinate; inline;
    function SE: TCoordinate; inline;
    class function Compare(const Item1, Item2: TCoordinate): integer; static;
    property Data: cardinal read FData write FData;
  end;

implementation

{ TCoordinate }

function TCoordinate.BlockIndex: integer;
begin
  Result:= (FData and 63) + ((FData shr 10) and (63 shl 6));
end;

class function TCoordinate.Compare(const Item1, Item2: TCoordinate): integer;
begin
  Result:= (Item1.BlockIndex - Item2.BlockIndex) + ((Item1.NeighborIndex - Item2.NeighborIndex) * 2);
end;

procedure TCoordinate.GetData(var x, y: integer);
begin
  x:= (FData and $FFFF);
  y:= (FData shr 16);
end;

class operator TCoordinate.Implicit(a: TCoordinate): TPoint;
begin
  Result.x:= (a.FData and $FFFF);
  Result.y:= (a.FData shr 16);
end;

class operator TCoordinate.In(a: TCoordinate; const b: TRect): boolean;
begin
  Result:= b.Contains(a);
end;

function TCoordinate.Max(a, b: TCoordinate): TCoordinate;
begin
  if Compare(a,b) > 0 then Result:= a
  else Result:= b;
end;

//class operator TCoordinate.Implicit(a: integer): TCoordinate;
//begin
//  Result.FData:= a;
//end;
//
//class operator TCoordinate.Implicit(a: TCoordinate): integer;
//begin
//  Result:= a.Fdata;
//end;

    ///  Coordinates follow screen conventions: topleft (NW) = 0,0
    ///  South, East = +, North, West = -
function TCoordinate.N: TCoordinate;
begin
  Result.FData:= Self.FData - (1 shl 16);
end;

function TCoordinate.NW: TCoordinate;
begin
  Result.FData:= ((Fdata - 1) and $FFFF) + ((Fdata and $FFFF0000) - (1 shl 16));
end;

function TCoordinate.W: TCoordinate;
begin
  Result.FData:= ((Fdata - 1) and $FFFF) + (FData and $FFFF0000);
end;

function TCoordinate.S: TCoordinate;
begin
  Result.FData:= Fdata - (1 shl 16);
end;

function TCoordinate.SE: TCoordinate;
begin
  Result.FData:= ((Fdata + 1) and $FFFF) + ((Fdata and $FFFF0000) + (1 shl 16));
end;

function TCoordinate.E: TCoordinate;
begin
  Result.FData:= ((FData + 1) and $FFFF) + (Fdata and $FFFF0000);
end;

function TCoordinate.NeighborIndex: integer;
var
  x,y: integer;
  Edge: boolean;
begin
  //if a coordinate is in the middle of a block then it does not have a neighbor.
  x:= (FData and 63);
  y:= (FData shr 16) and 63;
  Result:= 0 - integer(x = 0) + integer(x = 63) - (integer(y = 0) shl 1) + (integer(y = 63) shl 1);
end;

function TCoordinate.SetData(x, y: integer): integer;
begin
  Result:= (x and $FFFF) or ((y and $FFFF) shl 16);
end;

class operator TCoordinate.Subtract(a, b: TCoordinate): integer;
begin
  Result:= Integer(a) - Integer(b);
end;

class operator TCoordinate.Add(a, b: TCoordinate): integer;
begin
  Result:= Integer(a) + Integer(b);
end;

function TCoordinate.x: integer;
begin
  Result:= (FData and $FFFF);
end;

function TCoordinate.y: integer;
begin
  Result:= FData shr 16;
end;

function TCoordinate.Offset(Offset: TPoint): TCoordinate;
begin
  Result.FData:= SetData(x+Offset.x,y+Offset.y);
end;


class operator TCoordinate.Implicit(a: TPoint): TCoordinate;
begin
  Result.Fdata:= Result.SetData(a.x, a.y);
end;


//class operator TCoordinate.Explicit(a: TCoordinate): Integer;
//begin
//  Result:= Integer(a);
//end;

end.
