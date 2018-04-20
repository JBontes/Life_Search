unit SparseArray;

interface

uses
  LifeTypes,
  System.Types;

{$T+}

const
  BlockSizeX = 64;
  BlockSizeY = BlockSizeX;
  ShiftX = 6; //(2^6 = 128)
  ShiftY = ShiftX;
  CellSize = 16;

  DictSizeX = (1024*1024 div BlockSizeX) div CellSize;
  DictSizeY = DictSizeX;

type


  TSparseArray<T> = class(TObject)
  public type
    PT = ^T;
  private
    class var EmptyBlock: T;  //initialized to zero.
    class var FEmptyCell: PT;
    class constructor Init;
  private type
    TCell = record
      Flags: integer;
      //other fields are not relevant.
    end;
    PBlock = ^TBlock;
    TBlock = array[0..BlockSizeX-1, 0..BlockSizeY-1] of T;   //64*64*80 = 320KB
    PSubArray = ^TSubArray;
    TSubArray = record
      Data: PBlock;
      Count: integer;
      {$ifdef 64bit}
      Filler: integer;
      {$endif}
    end;
    TDict = array [0..DictSizeX-1, 0..DictSizeY-1] of TSubArray;  //16MB
  private
    FDict: TDict;

  public
  protected
    function IsEmpty(const item: PT): boolean; inline;  //in a proper generic class this would be virtual.
    procedure ZeroCell(const Item: PT); inline;         //in a proper generic class this would be virtual.
    function GetItem(const Coordinate: TCoordinate): PT; overload; inline;
    function GetItem(x,y: integer): PT; overload;
  public
    /// <summary>
    ///  access the storage as a 2 dimensional array.
    ///  note that a write property does not make sense.
    ///  if the item does not exist it does not create it, but it returns an empty block instead.
    ///  this allows some speedups if you want to see if neighbors of a Cell are empty.
    /// </summary>
    /// <remarks>
    ///  The returned Block is readonly.
    /// </remarks>
    property Item[x,y: integer]: PT read GetItem; default;
    property Item[const Coordinate: TCoordinate]: PT read GetItem; default;

    /// <summary>
    ///  adds a new item at the specified location and returns a pointer
    ///  so you can fill in the data.
    ///  Note that the item is not initialized.
    /// </summary>
    function Add(x,y: integer): PT; overload;
    function Add(Coordinate: TCoordinate; Offset: TPoint): PT; overload;

    /// <summary>
    ///  Deletes the item at the specified location.
    /// </summary>
    procedure Delete(x, y: integer);

    constructor Create;
    destructor Destroy;
    class property EmptyCell: PT read FEmptyCell;
  end;
implementation

{ TSparseArray<T> }



function TSparseArray<T>.Add(x, y: integer): PT;
var
  NewBlock: PBlock;
  x1,y1: integer;
  SubArray: PSubArray;
begin
  //set the index of
  x1:= x shr ShiftX;
  y1:= y shr ShiftY;
  //See if the block exists
  SubArray:= @FDict[x1,y1];
  if Assigned(SubArray.Data) then begin
    Inc(SubArray.Count);
  end else begin
    Getmem(NewBlock, SizeOf(TBlock));
    SubArray.Data:= NewBlock;
    //SubArray.Count:= 1;  Count is already initialized to 1.
  end;
  x:= x and (BlockSizeX - 1);
  y:= y and (BlockSizeY - 1);
  Result:= @(SubArray.Data^[x,y]);
end;

function TSparseArray<T>.Add(Coordinate: TCoordinate; Offset: TPoint): PT;
var
  x,y: integer;
begin
  x:= (Coordinate.x + Offset.X);
  y:= (Coordinate.y + Offset.Y);
  Result:= Add(x,y);
end;

constructor TSparseArray<T>.Create;
var
  x,y: integer;
begin
  inherited;
  //Set all the counts of all the blocks to 1.
  //That way we don't have to update the count when we add a new block.
  for x := 0 to DictSizeX-1 do for y := 0 to DictSizeY-1 do begin
    FDict[x,y].Count:= 1;
  end;
end;

procedure TSparseArray<T>.Delete(x, y: integer);
var
  NewBlock: PBlock;
  x1,y1: integer;
  SubArray: PSubArray;
begin
  //set the index of
  x1:= x shr ShiftX;
  y1:= y shr ShiftY;
  //See if the block exists
  SubArray:= @FDict[x1,y1];
  if Assigned(SubArray.Data) then begin
    if SubArray.Count = 1 then begin
      FreeMem(SubArray.Data);
      SubArray.Data:= nil;
    end else begin
      Dec(SubArray.Count);
      x:= x and (BlockSizeX - 1);
      y:= y and (BlockSizeY - 1);
      ZeroCell(@(SubArray.Data^[x,y]));
    end;
  end else begin
    {do nothing}
  end;
end;


destructor TSparseArray<T>.Destroy;
var
  x,y: integer;
  Block: PBlock;
begin
  //Set all the counts of all the blocks to 1.
  //That way we don't have to update the count when we add a new block.
  for x := 0 to DictSizeX-1 do for y := 0 to DictSizeY-1 do begin
    Block:= FDict[x,y].Data;
    if Assigned(Block) then FreeMem(Block);
  end;
  inherited;
end;

function TSparseArray<T>.GetItem(x,y: integer): PT;
var
  BlockIndex: integer;
  x1,y1: integer;
  SubArray: TSubArray;
begin
  //set the index of
  x1:= x shr ShiftX;
  y1:= y shr ShiftY;
  //See if the block exists
  SubArray:= FDict[x1,y1];
  if Assigned(SubArray.Data) then begin
    x:= x and (BlockSizeX - 1);
    y:= y and (BlockSizeY - 1);
    Result:= @(SubArray.Data^[x,y]);
    if IsEmpty(Result) then Result:= @EmptyBlock;
  end else Result:= @EmptyBlock;
end;

function TSparseArray<T>.GetItem(const Coordinate: TCoordinate): PT;
begin
  Result:= GetItem(Coordinate.x, Coordinate.y);
end;



class constructor TSparseArray<T>.Init;
begin
  FEmptyCell:= @EmptyBlock;
end;

function TSparseArray<T>.IsEmpty(const item: PT): boolean;
begin
  Result:= (Item = @EmptyBlock);
end;

procedure TSparseArray<T>.ZeroCell(const Item: PT);
begin
  TCell(pointer(item)^).Flags:= 0;
end;



end.
