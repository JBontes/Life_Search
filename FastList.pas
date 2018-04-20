unit FastList;


interface

{$TYPEDADDRESS ON}


uses
  System.Generics.Collections;

type
  TMoveEvent = procedure(Sender: TObject; OldIndex, NewIndex: integer) of object;

  TFastList<T> = class(TObject)
  public type
    PT = ^T;
  private
    FItems: array of T;
    FCount: integer;
    FOnMove: TMoveEvent;
    procedure Grow;
    function GetItem(index: Integer): PT;
    procedure SetItem(index: Integer; const Value: PT);
  public
    constructor Create;
    function Add(const Value: T): Integer; inline;
    procedure Delete(Index: Integer); inline;
    property Count: integer read FCount;
    property Items[index: Integer]: PT read GetItem write SetItem;
    property OnMove: TMoveEvent read FOnMove write FOnMove;
  end;

implementation

{ TFastList<T> }

function TFastList<T>.Add(const Value: T): Integer;
begin

end;

constructor TFastList<T>.Create;
begin

end;

procedure TFastList<T>.Delete(Index: Integer);
begin
  //Take the last item and move it to the empty spot.

end;

function TFastList<T>.GetItem(index: Integer): PT;
begin
  Result:= @FItems[Index];
end;

procedure TFastList<T>.Grow;
begin

end;

function TFastList<T>.OnMove: TMoveEvent;
begin

end;

procedure TFastList<T>.SetItem(index: Integer; const Value: PT);
begin
  Move(Value^, FItems[index], SizeOf(T));
end;

end.
