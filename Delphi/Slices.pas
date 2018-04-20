unit Slices;

interface

uses
  Generics.Collections, System.Types, Universe, VCL.Graphics, VCL.Grids, System.SysUtils;

type
  TDropPart = (dpLower, dpUpper, dpOuter);
  TUpdate = (uAChanged, uBChanged);
  TUpdates = set of TUpdate;
  /// <summary>
  ///  Do we look at on or off pixels?
  /// </summary>
  TPixelState = (pcOff, pcOn);

  /// <summary>
  ///  A set that records the changes in a grid.
  ///  Contains an iterator that allows you to work through the set as fast as possible
  /// </summary>
  TChangedSet<K: record> = record
  private
    FData: K;
    class var Max: cardinal;
    class constructor Init;
  public type
    PK = ^K;
    TSetEnumerator = record
    private
      FAdjustedIndex: integer;
      FIndex: integer;
      FData: PK;
    public
      constructor Create(const Data: PK);
      function MoveNext: boolean;
      function GetCurrent: boolean;
      property Current: boolean read GetCurrent;
    end;
  private
    FEnumerator: TSetEnumerator;
  public
    class function Create(Max: cardinal; var Data: K): TChangedSet<K>; static;
    procedure Add(index: integer);
    procedure Clear;
  end;



  /// <summary>
  ///  The internal data format of a slice.
  ///  A slice is 256 bits, denoting all possible states of 8 pixels that the slice holds.
  /// </summary>
  TSliceData = array[0..31] of byte;
  /// <summary>
  ///  TByteSet is an alternative representation for TSliceData
  ///  in case we want to have a set representation (which is of course exactly what a slice is :-).
  /// </summary>
  TByteSet = set of byte;

//const
//  cBit0: TSliceData = ($AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,
//                       $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA);
//  cBit1: TSliceData = ($CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,
//                       $CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC);
//  cBit2: TSliceData = ($F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,
//                       $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0);
//  cBit3: TSliceData = ($00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,
//                       $00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF);
//  cBit4: TSliceData = ($00,$00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,$FF,$FF,
//                       $00,$00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,$FF,$FF);
//  cBit5: TSliceData = ($00,$00,$00,$00,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$FF,$FF,$FF,$FF,
//                       $00,$00,$00,$00,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$FF,$FF,$FF,$FF);
//  cBit6: TSliceData = ($00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
//                       $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
//  cBit7: TSliceData = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
//                       $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
//type


  /// <summary>
  ///  There are 8 bits in a slice, each of which can be forced on or off
  ///  TForcedBits lists all the bits that have been forced, because there are
  ///  zero options for that bit left.
  ///  A bit with zero options forces its opposite to be set.
  ///  All bits set of 1 have zero options. The bits go like [0..7,pcOff..pcOn]
  ///  Rightmost dimensions advances first (see TBitCounts).
  /// </summary>
  TForcedBit = 0..15;
  /// <summary>
  ///  There are 8 bits in a slice, each of which can be forced on or off
  ///  TForcedBits lists all the bits that have been forced, because there are
  ///  zero options for that bit left.
  ///  A bit with zero options forces its opposite to be set.
  ///  All bits set of 1 have zero options. The bits go like [0..7,pcOff..pcOn]
  ///  Rightmost dimensions advances first (see TBitCounts).
  /// </summary>
  TForcedBits = set of TForcedBit;

  TForcedBitsHelper = record helper for TForcedBits
    function GetForcedBitCount: integer;
    /// <summary>
    ///  Get the next forced bit.
    ///  The result is a integer between 0..15 (-1 if not found).
    ///  the lsb denoted the state (even = pcOff, Odd= pcOnn)
    ///  the upper 3 bits denote the bitindex.
    /// </summary>
    function GetNextForcedBit(previous: integer): integer;
  end;


  /// <summary>
  ///  Store the option counts for set(0) and unset(1) bits
  ///  for bit 0..7.
  ///  If a count reaches zero then the bit associated with that count becomes forced
  ///  set or unset.
  ///  If both the set and unset counts for a bit reach zero, then we have a conflict
  ///  and we need to backtrack.
  ///  We can store the counts in a byte, because the allowed range is from 0..128
  /// </summary>
  TBitCounts = record
  private
    /// <summary>
    ///
    /// </summary>
    FData: array[0..7,pcOff..pcOn] of byte;   //16 bytes
    /// <summary>
    ///  Get the number of allowed neighbor states given a pixel's
    ///  state (on/off) if there are zero allowed neighbor-states in e.g. ON
    ///  then we can force the pixel OFF and visa versa.
    /// </summary>
    function GetCount(Pixel: integer; Config: TPixelState): integer;
    /// <summary>
    ///  Get all bits that have zero options, i.e. force the opposite bit.
    /// </summary>
    function GetForcedBits: TForcedBits;
  public
    /// <summary>
    ///  Two TBitsCounts are identical
    /// </summary>
    class operator Equal(const A,B: TBitCounts): boolean;
    class operator NotEqual(const A,B: TBitCounts): boolean;
    property Count[Bit: integer; Config: TPixelState]: integer read GetCount;
    property ForcedBits: TForcedBits read GetForcedBits;
  end;

  TAllowedStates = record
    FData: array[0..15] of byte;
  end;

  const
  Horz = 0;
  Vert = 1;

type

  /// <summary>
  ///  A simplified core with no unknown and don't care states.
  ///  The bits are numbered like  <para/>
  ///  01                          <para/>
  ///  23                          <para/>
  ///  And align with bits         <para/>
  ///  12                          <para/>
  ///  56                          <para/>
  ///  In a flat slice.
  ///  If we want to integrate the simplecore into a standing slice we need to transpose
  ///  bits 1 and 2 (corresponding to 2 and 5 in the slice) in the core.
  ///  Note: only 4 bits are used, because this is the core
  /// </summary>
  TSimpleCore = record
  private
    FData: integer;
    /// <summary>
    ///  Transform the core of a flat slice to the core of a standing
    ///  slice and visa versa.
    /// </summary>
    function FlatToStanding: TSimpleCore; //for performance reasons
  public
    class operator Implicit(a: integer): TSimpleCore; inline;
    class operator Implicit(a: TSimpleCore): integer; inline;
  end;

  /// <summary>
  ///  The orientations a slice can take. Because this is also used for slice pairs
  ///  we have both oStanding and oFlat.
  /// </summary>
  Orientations = (oFlat, oStanding);

  /// <summary>
  ///  Is a slice flat (canocial) or standing?
  /// </summary>
  TPos = set of Orientations;

  OrientationHelper = record helper for TPos
  public
    function IsFlat: Boolean; inline;
    /// <summary>
    ///  Returns Horz(0) if flat, Vert(1) if standing.
    /// </summary>
    function Position: Integer; inline;
    class function Flat: TPos; static;
    class function Standing: TPos; static;
  end;

  /// <summary>
  ///  A sliver is half of a <see cref="TSlice"/>. When overlapping two slices we take half
  ///  of each slice and throw the other bits away.
  ///  Then we are left with 4 pixels, which need 2^4 = 16 bits to encode.
  /// </summary>
  TSliver = type Int16;

  /// <summary>
  ///  workaround for <see cref="TSlice"/> properties as var parameters.
  /// </summary>
  PSlice = ^TSlice;
  /// <summary>
  ///  A TSlice is a part of the grid ordered like:
  ///  0123
  ///  4567  //for a Flat slice and order 04,15,26,37 for a standing slice.
  ///  The middle part 12,56 is a core, the rest are outer pixels.
  ///  The lower pixels are 0123, the upper pixels are 4567.
  ///  When combining two neighboring slices we always drop the lower, upper, or outer
  ///  pixels first, then we perform a birwise AND on both slices and add the
  ///  dropped part back in.
  /// </summary>
  TSlice = record
  private class var
    /// <summary>
    ///  The Mask gives all instances where a given pixel is **unset**
    ///  If you want to get all instances of a set pixel you need to use the inverse of the mask.
    /// </summary>
    Mask: array[0..7] of TSliceData;
    class constructor Init;
  strict private
    /// <summary>
    ///  Swap 2 bits in the slice.
    ///  The code is used internally by the TransformFlatSliceToStandingSlice code.
    /// </summary>
    procedure BitSwap(a,b: integer);
  private
    FData: TSliceData;
    /// <summary>
    ///  Drop bits 0123 from the slice so that two slices with overlapping bits
    ///  4567 can be joined.
    /// </summary>
    function DropUpper: TSliver;
    /// <summary>
    ///  Drop bits 4567 from the slice so that two slices with overlapping bits
    ///  0123 can be joined.
    ///  For performance reasons we return a sliver, so we do not have to haul a full slice
    ///  to and from memory.
    /// </summary>
    function DropLower: TSliver;
    /// <summary>
    ///  Drop bits 0178 from the slice, leaving only the inner bits in the core
    ///  We still need a transposition or the standing to a flat slice before joining the slices.
    /// </summary>
    function DropOuter: TSliver;
    /// <summary>
    ///  Take a 4x4 past for a 2x2 core and take the middle standing slice from it.
    ///  Set the bit for that slice
    ///  Note: used to build a lookup table one bit at a time
    /// </summary>
    procedure AddStanding4x4Past(input: integer);
    /// <summary>
    ///  Take a 4x4 past for a 2x2 core and take the middle flat slice from it.
    ///  Set the bit for that slice
    ///  Note: used to build a lookup table one bit at a time
    /// </summary>
    procedure AddFlat4x4Past(input: integer);
    class operator BitwiseAnd(const A,B: TSlice): TSlice; overload;
    class operator BitwiseAnd(const A: TSlice; const B: TSliceData): TSlice; overload;
    class operator BitwiseOr(const A, B: TSlice): TSlice;
    class operator BitwiseXor(const A,B: TSlice): TSlice;
    class operator LogicalNot(const A: TSlice): TSlice;
    /// <summary>
    ///  Translate a (flat) core to an index into the slice.
    ///  We still need to generate all 16 outer  (0347) bits for the core.
    ///  Note: used to build a lookup table
    /// </summary>
    class function SimpleCoreToSliceIndex(SimpleCore: TSimpleCore): integer; static;
    //function TransformFlatCoreToStandingCoreLazy: TSlice;
  public
    procedure SetBorderPixel(index: integer; PixelConfig: TPixelState);

    /// <summary>
    ///  Extract the Part bits from the slice
    ///  Return a slice with those bits duplicated.
    ///  The slice-bits are ordered like so:
    ///  0: 0101010101010101010101010101010101010101010101010101010101010101
    ///  1: 0011001100110011001100110011001100110011001100110011001100110011
    ///  2: 0000111100001111000011110000111100001111000011110000111100001111
    ///  3: 0000000011111111000000001111111100000000111111110000000011111111
    ///  4: 0000000000000000111111111111111100000000000000001111111111111111
    ///  5: etc.
    ///  In short the period doubles as the number goes up.
    ///  If we want to remove a slice we need to take the 0-mask for that slice,
    ///  shift it and OR it with the 1-part for that slice.
    /// </summary>
    function Drop(Part: TDropPart): TSliver;

    /// <summary>
    ///  Combine ands two neighboring slices together.
    ///  DroppedA and -B are the slices with their non-overlapping pixels dropped.
    ///  OriginalB is the slice that is about to be updated.
    ///  The Self slice will also be updated.
    ///  If there is a change in either A or B then the result will signal this.
    /// </summary>
    function Combine(const DroppedA, DroppedB: TSliver; OriginalB: PSlice): TUpdates; overload;
    /// <summary>
    ///  *and* two slices together, return true if any changes were made to the destination.
    /// </summary>
    function Combine(const LookupValue: TSlice): boolean; overload;

    /// <summary>
    ///  Converts a flat core to a standing core an visa versa.
    ///  Because only 2 bits need to be swapped the transform happens to be symmetrical
    /// </summary>
    function TransformFlatCoreToStandingCore: TSlice;
    /// <summary>
    ///  Test function to see what the transformation looks like.
    ///  Used to investigate runs that occur as a consequence of the transformation
    ///  We can exploit the phenomenon of runs to built a faster algorithm.
    /// </summary>
    class procedure InitTransformationMatrix; static;
    /// <summary>
    ///  Look for the next set bit (or the first if previous =-1)
    ///  And return the next set bit.
    ///  The value returned is a byte with the set bits 01234567 in the slice
    /// </summary>
    function GetNextOption(previous: integer): integer;
    /// <summary>
    ///  Unit testing for GetNextOption
    /// </summary>
    function TestGetNextOption(previous: integer): integer;
    /// <summary>
    ///  Get the counts of bits set for each of 8 masks and their inverse
    ///  This gives us the number of allowed states for a given pixel in its on/off
    ///  configuration.
    ///  If a pixel in a given config has zero allowed states, that means that that
    ///  pixel becomes forced in the opposite state.
    ///  If a pixel has few allowed states in a config, it is worthwhile to explore
    ///  all possible configurations and see if we can exclude these to force the
    ///  pixel.
    ///  The function returns an array of counts. The maximum count for a pixelconfig
    ///  (either on or off) is 128 and min is 0.
    /// </summary>
    function GetAllowedBitCounts: TBitCounts;
    /// <summary>
    ///  Get the total number of allowed states in a slice.
    ///  Every allowed state is a configuration of pixels benoted by a set bit.
    /// </summary>
    function GetAllowedStateCounts: integer; overload;
    /// <summary>
    ///  Get the number of allowed states for the pixel with bitindex p.
    /// </summary>
    /// <returns>
    ///  0 for an invalid slice (one with zero options). <para/>
    ///  (-)2..128 if multiple states are allowed. The value is negative if fewer off states remain, positive if fewer on states remain. <para/>
    ///  1 if the pixel is forced to on. <para/>
    ///  -1 if the pixel is foced to off. <para/>
    /// </returns>
    function GetAllowedStateCounts(p: cardinal): integer; overload;
    /// <summary>
    ///  If a past grid as two neighboring slices with few allowed states, then we can calculate
    ///  all possible future cores in the current grid.
    ///  If the outcome is less than all possible 16 cores, then we forbid the ones that are not possible
    ///  using this function.
    ///  Note: the usually call TSlices.RemoveForbiddenCore, not the removal function for individual slices
    /// </summary>
    procedure RemoveForbiddenCore(ForbiddenCore: TSimpleCore; Orientation: TPos);
    /// <summary>
    ///  Get the first 16 allowed configurations.
    ///  Can be used to calculate future cores is the number of allowed states is small.
    ///  note: This function is only useful if there are 16 or fewer states allowed.
    /// </summary>
    procedure GetAllowedStates(out States: TAllowedStates);
    /// <summary>
    ///  For unit-testing GetBitCounts
    /// </summary>
    function TestGetBitCounts: TBitCounts;
    /// <summary>
    ///  For unit-testing
    /// </summary>
    class function FillWithRandomData: TSlice; static;
    /// <summary>
    ///  Return false if the slice contains zero, meaning that there are no valid states.
    /// </summary>
    function IsValid: boolean;
    /// <summary>
    ///  Drop bits 0123, returning a slice with only the rightmost bits expressed.
    ///  Note: alias of DropUpper
    /// </summary>
    property DropLeft: TSliver read DropUpper;
    /// <summary>
    ///  Dtop bits 4567, returning a slice with only the leftost bits expressed.
    ///  Note: alias of DropLower
    /// </summary>
    property DropRight: TSliver read DropLower;
    property Counts: TBitCounts read GetAllowedBitCounts;
  end;

  TSliverHelper = record helper for TSliver
  private
  class constructor Init;
    /// <summary>
    ///  Transpose the two bits A and B. <para>
    ///  The bit can have the following encodings:
    ///  <para>   0123456789ABCDEF </para>
    ///  <para> 0  0101010101010101 </para>
    ///  <para> 1  0011001100110011 </para>
    ///  <para> 2  0000111100001111 </para>
    ///  <para> 3  0000000011111111 </para>
    /// </summary>
    function TransposeBit12: TSliver;
    function ExpandToSlice: TSlice;
  end;

  TPixelState = (psOff, psOn, psUnknown, psDontCare);

const
  cTL = 0;
  cTR = 1;
  cBL = 2;
  cBR = 3;
  cCoreUnknown = $AA;
  cSliceUnknown = $FF;

type
  /// <summary>
  ///  Holds the future state for 4 pixels
  ///  Every pixel can be in 4 states, on, off, unknown, or don't care
  ///  Off=00, on=01, unknown = 10, don't care = 11.
  ///   The grid is like this: <para/>
  ///   <code>
  ///      TL|TR        <para/>
  ///     ---+---       <para/>
  ///      BL|BR        <para/>
  ///  </code>
  /// </summary>
  TCore = record
  private class var
    CombineLookupTable: array[0..256*256-1] of byte;
  private
    class constructor InitLookuptable;
    class function Idx(a,b: TCore): integer; static; inline;
  private
    FData: byte;
    function GetPixel(const Index: Integer): TPixelState;
    procedure SetPixel(const Index: Integer; const Value: TPixelState);
    function FlatToStanding: TCore;
  public
    procedure SetPixels(TL, TR, BL, BR: TPixelState);
    class operator Implicit(a: TCore): integer; inline;
    /// <summary>
    ///  Combine two cores.
    ///  The known bits of Core A take precedence over the known bits of core B.
    ///  Takes care to preserve psDontCare over psUnknown.
    /// </summary>
    class function Combine(const A,B: TCore): TCore; static;
    /// <summary>
    ///  Return true is there are no unknown or dont care states in the core.
    ///  This core can be reduced to a <see cref="TSimpleCore"/>
    /// </summary>
    function IsFixed: boolean; inline;

    property Pixel[const index: integer]:TPixelState read GetPixel write SetPixel;
    property TL: TPixelState index cTL read GetPixel write SetPixel;
    property TR: TPixelState index cTR read GetPixel write SetPixel;
    property BL: TPixelState index cBL read GetPixel write SetPixel;
    property BR: TPixelState index cBR read GetPixel write SetPixel;
  end;





  /// <summary>
  ///  A set of all possible future cores that are allowed.
  ///  A set bit indicates that the future slice allows this core, a unset bit
  ///  means that this core is forbidden.
  ///  Note that the core is a Flat core. If you want the standing core you'll have to translate.
  /// </summary>
  TFutureCoreEnum = 0..15;
  TFutureCoreSet = set of TFutureCoreEnum;
  TFutureCoreIndex = 0..(2 shl High(TFutureCoreEnum)) -1;


  /// <summary>
  ///  An array of 16 cores (each occupying 4 bits) that have been calculated
  ///  from 16 4x4 past grids.
  ///  Note that the FutureCores do not keep track of the number of
  ///  valid cores inside, you'll have to do that seperately.
  /// </summary>
  TFutureCores = record
  private
    FData: int64;
    /// <summary>
    ///  Extract one of the 16 possible cores.
    /// </summary>
    function GetCore(index: integer): TCore;
  public
    /// <summary>
    ///  Page through the cores returned and set a bit for each core found.
    /// </summary>
    function GetUniqueCores(CoreCount: integer): TFutureCoreSet;
    class operator Implicit(const input: Int64): TFutureCores; inline;
    class function Empty: TFutureCores; static;
    property Core[index: integer]: TCore read GetCore;
  end;

  TSlices = record
  private
    FData: array[Horz..Vert] of TSlice;
    /// <summary>
    ///  Use only for the lookup table
    /// </summary>
    class operator BitwiseOr(const A,B: TSlices): TSlices;
    class operator BitwiseAnd(const A,B: TSlices): TSlices;
    function GetCore: TCore;
  public
    /// <summary>
    ///  Return an empty TSlice pair, i.e. all zeros. This means
    ///  that every state is disallowed.
    /// </summary>
    class function Empty: TSlices; static;
    /// <summary>
    ///  Return a full TSlice pair, i.e. all ones.
    ///  This means that all states are allowed.
    /// </summary>
    class function Full: TSlices; static;
    /// <summary>
    ///  Walk through the set of allowed cores and remove all options that
    ///  are not listed as allowed.
    ///  Because a core is 4 bits and there are 8 bits in a slice this means
    ///  that a forbidden core will eliminate 16 states from a slice.
    ///  Note that the allowedCores stores the core in Flat config:
    ///  Bits 1256 are stored in 0123 numerical representation of the set position.
    /// </summary>
    function RemoveForbiddenCores(const AllowedCores: TFutureCoreSet): boolean;
    /// <summary>
    ///  Combine the slice pair with a slice pair from the lookup table.
    ///  Returns true if self has changed.
    /// </summary>
    function Combine(const LookupValue: TSlices): boolean;
    property Core: TCore read GetCore;
    property Flat: TSlice read FData[Horz] write FData[Horz];
    property Standing: TSlice read FData[Vert] write Fdata[Vert];
  end;

  /// <summary>
  ///  The Grid contains current Slices (all possible states of a pixel in gen x)
  ///  and current cores (the effective state of a pixel in generation x).
  ///  If a pixel in a slice becomes forced, the pixel in the corresponding
  ///  core is also fixed
  ///  Any changes in a core in a grid affects the slices the past grid
  ///  If all or most pixels in a core/slice become fixed, then we can try to ripple
  ///  those changes to the next generation.
  ///  To recap: A Slice links 1-to-1 with its current core
  ///  A core links to its past slice via the lookuptable
  ///  If there are few options left in a slice, then we can calculate the options
  ///  for a future core using plain old calculation of all options.
  /// </summary>
  TGrid = class(TObject)
  private class var
    /// <summary>
    ///  The size of the grid.
    ///  This is a class var, because we do not mix different grids in an instance.
    /// </summary>
    FSizeX, FSizeY: cardinal;
  private
    FCores: array of TCore;
    FSlices: array of TSlices;
    FPixelsChanged: TList<TPoint>;
    FFuture: TGrid;
    FPast: TGrid;
    FGeneration: integer;   //can never be negative
    /// <summary>
    ///  The workerbee calculates futures if we have few options in a slice.
    ///  We calculate 16 4x4 blocks in parallel and see how many 2x2 blocks come out.
    ///  If its less than 16 then we can forbid the options associated with the cores
    ///  not found.
    /// </summary>
    FWorkerBee: TCellBlock;
    /// <summary>
    ///  Dirty is true if the cores and slices are out of sync.
    /// </summary>
    FDirty: boolean;
    /// <summary>
    ///  For performance and convienence reasons the cores and Slices are stored in flat
    ///  Arrays. Use idx(x,y) to access them as 2D arrays.
    ///  This functions wraps around, e.g.: Idx(-1,-1) wraps to Idx(SizeX-1, SizeY-1)
    ///  However Idx(-100,-100) is out of scope, so make sure to only wrap by a little bit.
    ///  This works because the grid is surrounded by a border of fixed pixels.
    /// </summary>
    function Idx(x,y: integer): integer; {inline;}
    function GetSlice(x, y: integer; const Index: Integer): PSlice;
    procedure SetSlice(x, y: integer; const Index: integer; const Value: PSlice);
    function GetPixel(x, y: integer): TPixelState;
    procedure SetPixel(x, y: integer; const Value: TPixelState);
    /// <summary>
    ///  Combine a given standing slice with its two overlapping neighbors to the left and right.
    ///  Returns true if there are any changes to the middle slice.
    /// </summary>
    function CombineStanding(x, y: Integer): boolean;
    /// <summary>
    ///  Combine a given flatslice with its overlapping neighbors to the top and bottom.
    ///  Returns true if there are any changes to the middle slice.
    /// </summary>
    function CombineFlat(x, y: Integer) : boolean;
    /// <summary>
    ///  Combine two perpendicular slices with each other.
    ///  Returns true if there are any changes to the either slice.
    /// </summary>
    function CombineCross(x, y: Integer): boolean;
    /// <summary>
    ///  Take two neighboring (non-overlapping) slices (a 4x4 grid) and calculate all possible
    ///  future cores (2x2 pixels) that can occur.
    ///  This can be used to eliminate future states that cannot possible occur.
    ///  Only used if there are 16 or less possible states to calculate.
    ///  Works in O(1) time.
    /// </summary>
    function CalculateFutureSlices(const A,B: TSlice; orientation: TPos): TFutureCores;
    /// <summary>
    ///  Combine two bytes with either two 4x2 (Flat) or two 2x4 (standing) parts into a 4x4 block
    ///  that can be pasted into a CellBlock.
    ///  In order to to this the 4x4 block need to be put into bits 0123, 16171819, 32333435, 48495051
    ///  of the result int64.
    ///  This allows for easy pasting into the TCellBlock.
    /// </summary>
    class function CombineStates(const A,B: integer; orientation: TPos): int64; static;
    /// <summary>
    ///  If we have two neighboring slices with CountA * CountB <= 16 then
    ///  we can calculate all possible future 2x2 states in a single calculation.
    ///  We simply tile a 16x16 block with 4x4 tiles and calculate the result
    ///  Then we update the core bits of the two overlapping standing and flat slices
    ///  (In the future grid). With luck this should yield a reduced set of options
    ///  Do not update the cores, let the grid handle its own cores when it needs to.
    /// </summary>
    function CalculateFutureCoresAndSlices(x,y: integer{; var Slices: TSlices}): boolean;
    /// <summary>
    ///  For all pixels that have seen changes, perform
    ///  <see cref="CalculateFutureCoresAndSlices"/> on the slices that intersect that pixel.
    /// </summary>
    function ProcessSlicesToFutureCores: integer;
    /// <summary>
    ///  Perform one full walk of the grid
    ///  If there are any changes, see if this forces a pixel.
    ///  If there are any changes, or the grid is dirty then alter the core.
    ///  Return the number of changes
    ///  Store the coordinates of all pixels changed in <see cref="TGrid.FCoresChanged"/>
    /// </summary>
    function WalkTheGrid: integer;
    /// <summary>
    ///  Perform a full walk of the cores
    ///  Combine the slice data from the cores with what's already there.
    ///  Return the number of slices changed.
    /// </summary>
    function ProcessCoresToPastGrid: integer;
    /// <summary>
    ///  Lookup a slice in a lookup table given a full core.
    ///  Usually called at initialization time.
    /// </summary>
    procedure LookupSliceForCore(x,y: integer);
    procedure SetFuture(const Value: TGrid);
    procedure SetPast(const Value: TGrid);
    /// <summary>
    ///  Set a border of zeros/ones just outside the cores at the edge of the grid.
    ///  Because slices cover only the middle part of the core and not the full block
    ///  the 4 pixels at the extreme corners will not be covered.
    ///  If we allow the grid to wrap around this problem will be solved.
    ///  This can be done by adjusting the <see cref="idx"/> function.
    /// </summary>
    procedure SetBorderPixels(const Value: TPixelState);
  public
    constructor Create(SizeX: integer = 0; SizeY: integer = 0; Generation: integer = 0);
    destructor Destroy; override;
    /// <summary>
    ///  Go through all slices and make sure we do not have an invalid state in any of them.
    ///  Raise an assertion if that happens.
    /// </summary>
    procedure Validate;
    /// <summary>
    ///  WalkTheGrid and ProcessCoresToPastGrid until there are no more changes.
    ///  Return the number of loops it took for the grid to stabilize.
    /// </summary>
    function ProcessEntailments: integer;
    /// <summary>
    ///  Display to the given bitmap.
    /// </summary>
    procedure Display(var Bitmap: TBitmap); overload;
    /// <summary>
    ///  Display to the given stringgrid.
    /// </summary>
    procedure Display(const StringGrid: TStringGrid); overload;
    property HorzSlice[x,y: integer]: PSlice index 0 read GetSlice write SetSlice;
    property VertSlice[x,y: integer]: PSlice index 1 read GetSlice write SetSlice;
    property Pixel[x,y: integer]: TPixelState read GetPixel write SetPixel;
    /// <summary>
    ///  Dirty is true if the cores and slices are out of sync.
    /// </summary>
    property Dirty: boolean read FDirty;
    property Future: TGrid read FFuture write SetFuture;
    property Past: TGrid read FPast write SetPast;
    class property SizeX: cardinal read FSizeX;
    class property SizeY: cardinal read FSizeY;
  end;

  /// <summary>
  ///  A lookup table to translate a full slice (with unknown and don't care data)
  ///  to a full slice with all allowed options.
  /// </summary>
  TFullCoreToSliceLookuptable = record
  private class var
    FFlat: array[0..255] of TSlice;
    FStanding: array[0..255] of TSlice;
  private
    class constructor CreateLookupTable;
    class function GetSlices(const Core: TCore): TSlices; static;
  public
    /// <summary>
    ///  Returns the past slice pair for the current core given.
    /// </summary>
    //class function SlicesFromCore(const Core: TCore; Orientation: TPos): TSlices; static;
    class property Slices[const Core: TCore]: TSlices read GetSlices;
  end;

  TSimpleCoreToSliceLookupTable = record
  private class var
    FJustTheSlices: array[0..15] of TSlices;
    FJustTheCore: array[0..15] of TCore;
    FEveryThingButTheCore: array[0..15] of TSlices;
    FFutureSlices: array[TFutureCoreIndex] of TSlices; //4MB of lookup table
    FFutureCore: array[TFutureCoreIndex] of TCore; //64KB of lookup table
  private
    class constructor CreateLookupTables;
  public
    /// <summary>
    ///  Get a slice pair that allows only the given core and nothing else
    /// </summary>
    class function PositiveSlicesFromCore(const Core: TSimpleCore): TSlices; static; inline;
    /// <summary>
    ///  Get a slice pair that allows everything except the given core.
    /// </summary>
    class function NegativeSlicesFromCore(const Core: TSimpleCore): TSlices; static; inline;
  end;

  /// <summary>
  /// A set of all possible future cores that are allowed.
  /// A set bit indicates that the future slice allows this core, a unset bit
  /// means that this core is forbidden.
  /// Note that the core is a Flat core. If you want the standing core you'll have to translate.
  /// </summary>
  TFutureCoreSetHelper = record helper for TFutureCoreSet
    function Count: integer;
    /// <summary>
    ///  Get the slice pair for the given FutureCoreSet.
    ///  You'll need to *and* that slice-pair with the slice-pair in the grid
    ///  in order to eliminate states that are not allowed.
    ///  This function uses a lookup table to get the allowed slice.
    /// </summary>
    function GetAllowedSlices: TSlices;
    /// <summary>
    ///  Return the union of all allowed cores. If there is a pixel that is forced
    ///  than that pixel will be set correctly, otherwise psUnknown will be set.
    ///  Do not combine this with cores that have psDontCare set.
    /// </summary>
    function GetAllowedCore: TCore;
  end;

implementation

const
  NotFound = -1;

var SliverTransposeBit12Lookup: array[0..$FFFF] of Integer;

{ TSlice }

//Swap bits A and B in the input
procedure SwapBits(BitA, BitB: integer; var input: integer);
asm
  xor eax, eax
  xor r10d, r10d

  mov r9d, [r8]   // read the value
  btr r9d, edx    // read and clear the edx bit

  adc eax,eax     // convert cf to bit
  shl eax, cl     // shift bit to ecx position

  btr r9d, ecx    // read and clear the ecx bit

  mov ecx, edx    // need edx in ecx for shift
  adc r10d,r10d   // convert cf to bit
  shl r10d, cl    // shift bit to edx position

  or r9d, eax     // copy in old edx bit
  or r9d, r10d    // copy in old ecx bit

  mov [r8], r9d   // store result
end;

//function Max(A,B: integer): integer; overload;
//asm
//  //ecx=a
//  //edx=b
//  cmp   A, B
//  mov   eax, B
//  cmovnl eax, A
//end;


function Max(A,B: integer): integer; overload;
begin
  if (A > B) then Result:= A else Result:= B;
end;



//function Max(A,B: TPixelState): TPixelState; overload;
//asm
//  //ecx=a
//  //edx=b
//  cmp   A, B
//  movzx  eax, B
//  movzx  ecx, A
//  cmovnl eax, ecx
//end;


function Max(A,B: TPixelState): TPixelState; overload;
begin
  if integer(A) > integer(B) then Result:= A else Result:= B;
end;


function Min(A,B: integer): integer;
asm
  //ecx=a
  //edx=b
  cmp   A, B
  mov   eax, B
  cmovl eax, A
end;

function ABSMin(A, B: integer): integer;
begin
  if (ABS(a) > ABS(b)) then Result:= B else Result:= A;
end;

procedure TSlice.AddFlat4x4Past(input: integer);
asm
  and edx,$0FF0
  shr edx,4
  bts [rcx],rdx
end;

procedure TSlice.AddStanding4x4Past(input: integer);
asm
  //RCX = self
  //edx = input
  and edx,$6666      //mask off the non-relevant bits
  xor eax,eax
  shr edx,1          //remove bit 0
  mov al,dl
  shr edx,2
  or al,dl           //combine bits 01+23
  shr edx,2
  mov ah,dh
  shr edx,2
  or ah,dh           //combine bits 45+67
  bts [rcx],rax      //set the bit in the slice
end;

procedure TSlice.BitSwap(a, b: integer);
asm
  //RCX = self
  //edx = a
  //r8d = b
  xor eax,eax
  xor r9,r9
  bt [rcx],rdx      //test the second bit
  adc eax,0         //eax = bitA
  bt [rcx],r8
  adc r9,r9         //r9 = bitB
  xor rax,r9        //eax = 1 if different, 0 if equal
  je @done
  //The bits are different, invert them
  btc [rcx],rdx
  btc [rcx],r8
@done:
  rep ret
end;

class operator TSlice.BitwiseAnd(const A, B: TSlice): TSlice;
asm
  //RCX = @result
  //RDX = @A
  //R8 = @B
  movdqu xmm0,[rdx]
  movdqu xmm1,[rdx+16]
  movdqu xmm2,[r8]
  movdqu xmm3,[r8+16]
  pand xmm0,xmm2
  pand xmm1,xmm3
  movdqu [rcx],xmm0
  movdqu [rcx+16],xmm1
end;

class operator TSlice.BitwiseAnd(const A: TSlice; const B: TSliceData): TSlice;
asm
  //RCX = @Result
  //RDX = @A
  //R8 = @B
  movdqu xmm0,[rdx]
  movdqu xmm1,[rdx+16]
  movdqu xmm2,[r8]
  movdqu xmm3,[r8+16]
  pand xmm0,xmm2
  pand xmm1,xmm3
  movdqu [rcx],xmm0
  movdqu [rcx+16],xmm1
end;

class operator TSlice.BitwiseOr(const A, B: TSlice): TSlice;
asm
  //RCX = Result
  //RDX = @A
  //R8 = @B
  movdqu xmm0,[rdx]
  movdqu xmm1,[rdx+16]
  movdqu xmm2,[r8]
  movdqu xmm3,[r8+16]
  por xmm0,xmm2
  por xmm1,xmm3
  movdqu [rcx],xmm0
  movdqu [rcx+16],xmm1
end;

class operator TSlice.BitwiseXor(const A, B: TSlice): TSlice;
asm
  //RCX = Result
  //RDX = @A
  //R8 = @B
  movdqu xmm0,[rdx]
  movdqu xmm1,[rdx+16]
  movdqu xmm2,[r8]
  movdqu xmm3,[r8+16]
  pxor xmm0,xmm2
  pxor xmm1,xmm3
  movdqu [rcx],xmm0
  movdqu [rcx+16],xmm1
end;

function TSlice.Combine(const LookupValue: TSlice): boolean;
asm
  //rcx = self
  //rdx = lookupvalue
  movdqu xmm0,[rcx]       //get self
  movdqu xmm1,[rcx+16]
  movdqu xmm2,[rdx]       //get lookup data
  movdqu xmm3,[rdx+16]
  mov edx,1               //edx = true, changes are made
  pand xmm2,xmm0          //combine the two
  pand xmm3,xmm1
  pcmpeqq xmm0,xmm2       //compare to see if the two are equal
  pcmpeqq xmm1,xmm3
  pand xmm0,xmm1          //fold the two
  pshufd xmm0,xmm0,$cc    //mix the result xmm0_low: <- -1 if all equal, 0 if not all equal.
  movq rax,xmm0
  inc rax                 //force eax=0 if the same
  cmovnz eax,edx          //force eax=1 if difference.
end;

const
  SliverToSliceMask: array[0..7] of word = (1,2,4,8,16,32,64,128); //8 words, 16 bytes

function TSlice.Combine(const DroppedA, DroppedB: TSliver; OriginalB: PSlice): TUpdates;
asm
  //registers
  //RCX = self
  //EDX = DroppedA
  //R8d = DroppedB
  //R9 = @OriginalB
  //eax = Result: TUpdates
  //First AND the two Dropped slices together.
  movdqu xmm14,[rip+SliverToSliceMask]    //xmm14 = mask to isolate bits 0..7
  movdqa xmm15,xmm14                      //xmm15 = mask
  movdqu xmm2,[rcx]                       //Get originalA slice part 1
  movdqu xmm3,[rcx+16]                    //Get originalA slice part 2
  movdqu xmm4,[r9]                        //Get originalB slice part 1
  movdqu xmm5,[r9+16]                     //Get originalB slice part 2
  pslldq xmm15,1                          //convert mask from bit 0..7 to bit 8..15
  and edx,r8d                             //combine the two slivers
  //Now expand the sliver into a slice
  movd xmm0, edx              //xmm0 = sliver
  pshuflw xmm0,xmm0,$00       //Paste the sliver 4 times (expanding it to 8 bytes).
  pshufd xmm0, xmm0, $50     //Paste the sliver 2 more times (expanding it to 16 bytes)
  movdqa xmm1,xmm0           //make a copy,expanding it to 32 bytes (in 2 regs)
  //now xmm0 and xmm1 are filled with 16 copies of the input sliver, mask out the bit
  //that is intended per word
  pand xmm0,xmm14             //mask off the words for bit 0..7
  pand xmm1,xmm15             //mask off the words for bit 8..15
  pcmpeqw xmm0,xmm14          //set 1's to $FFFF and 0's to 0
  pcmpeqw xmm1,xmm15          //Thus expanding the sliver (1 bit per outer sliver) to a slice (16 bits per sliver)


  //Now AND the combined slice with each of the two other slices
  //First save self
  movdqa xmm2,xmm4         //xmm2,3 = original self
  movdqa xmm3,xmm5
  pand xmm4,xmm0           //xmm4,5 = updated self
  pand xmm5,xmm1
  pand xmm0,xmm6           //xmm0,1 = updated B
  pand xmm1,xmm7           //xmm6,7 = original B
  movdqu [rcx],xmm4        //store the updated self data
  movdqu [rcx+16],xmm5
  movdqu [r9],xmm0         //store the updated B
  movdqu [r9+16],xmm1
  //Now see if Self has changed
  pcmpeqq xmm2,xmm4        //either qword = 0 if not equal.
  pcmpeqq xmm3,xmm5        //either qword = 0 if not equal
  pand xmm2,xmm3           //fold the high and low part
  pshufd xmm2,xmm2,$CC     //The low qword = -1 if no changes, other if changes
  //See if original B has changed.
  pcmpeqq xmm0,xmm6        //either qword = 0 if not equal.
  pcmpeqq xmm1,xmm7        //either q word = 0 if not equal
  pand xmm0,xmm1           //fold the high and low part
  pshufd xmm0,xmm0,$CC     //The low qword = -1 if no changes, other if changes
  xor eax,eax              //assume no changes
  mov ecx,1                //1: A has changed
  mov edx,2                //2: B has changed
  movq r8,xmm2             //rcx = -1 if self has changed
  movq r9,xmm0             //rdx = -1 if B has changed
  cmp r8,-1
  cmove ecx,eax            //-1, mark no changes in self
  cmp r9,-1
  cmove edx,eax            //-1, mark no changes in B
  lea eax,[ecx+edx]        //Return the changes
end;

function TSlice.Drop(Part: TDropPart): TSliver;
begin
  case Part of
    dpLower: Result:= DropLower;
    dpUpper: Result:= DropUpper;
    dpOuter: Result:= DropOuter;
  end;
end;

const
  ShuffleMask0: array[0..15] of byte = ($00,$02,$04,$06,$08,$0A,$0C,$0E,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
  ShuffleMask1: array[0..15] of byte = ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$02,$04,$06,$08,$0A,$0C,$0E);

function TSlice.DropLower: TSliver;
//RCX = self
//RDX = @result
asm
  movdqu xmm1,[rcx]        //Get the slice data
  movdqu xmm2,[rcx+16]
  movdqu xmm3,[rip+ShuffleMask0]  //Shuffle mask to reduce 16 bits to 8 low ones
  movdqu xmm4,[rip+ShuffleMask1]  //Shuffle mask to reduce 16 bits to 8 high ones
  //we can do this because th pcmpeqq reduces the bitdepth from 32 to 16 bits.
  pxor xmm0,xmm0           //compare against zero
  pcmpeqq xmm15,xmm15      //xmm15 is all ones (to invert the result)
  pcmpeqw xmm1,xmm0        //if the bits 0123 are all zero's then 1, else 0
  pcmpeqw xmm2,xmm0        //This performs a logical not(or) of bits 0123
  pxor xmm1,xmm15          //invert the low result
  pxor xmm2,xmm15          //invert the high result
  pshufb xmm1,xmm3         //reduce 8 words into 8 low bytes
  pshufb xmm2,xmm4         //reduce 8 words into 8 high bytes
  por xmm1,xmm2            //combine the low and high bytes
  pmovmskb eax,xmm1        //return as 16 bits
end;

const
  ShuffleMask34: array [0..15] of byte = ($00,$04,$08,$0C,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);

var
  LookupFold16BitsTo8: array[0..$FFFF] of byte;

procedure InitLookupFold16BitsTo8;
type
  TByteSet = set of 0..7;
var
  i,j: integer;
  a: TByteSet;
begin
  for i:= 0 to $FFFF do begin
    a:= [];
    for j:= 0 to 7 do begin
      if (i and (1 shl (j*2)) <> 0) then Include(a,j);
    end;
    TByteSet(LookupFold16BitsTo8[i]):= a;
  end;
end;

function TSlice.DropOuter: TSliver;
//RCX = self
//Drop Pixels 0,3,4,7
asm
  movdqu xmm0,[rcx]
  movdqu xmm1,[rcx+16]
  movdqu xmm15,[rip+ShuffleMask34]
  por xmm0,xmm1       //fold bit 7, reduce from 256 to 128 bits
  movdqa xmm1,xmm0
  psrldq xmm0,2       //line up bit4-zeros with bit4 ones
  por xmm0,xmm1       //reduce bit 4, we now need to fold, wait until we're done with bit 3
  movdqa xmm1,xmm0
  psrldq xmm1,1       //line up bit3 zeros with bit3 ones
  por xmm0,xmm1       //reduce bit 3, we need to fold bytes 0123 -> 0, 4567-> 1, 89AB -> 2, CDEF-> 3
  pshufb xmm0,xmm15   //fold 128 bits into 32 bits.
  movq rax,xmm0       //eax = slice minus bits 7,4,3. We still need to remove bit 0
  lea rcx,[rax+rax]   //rcx = rax shl 1.
  or eax,ecx          //fold bit 0.
  //we now need to select every odd bit.
  //We use a lookup table for this
  lea rcx,[rip+LookupFold16BitsTo8]     //Get the lookup table
  mov edx,eax
  and eax,$FFFF                         //eax = lower 16 bytes
  shr edx,16                            //edx = higher 16 bytes
  mov al,[rcx+rax]                      //al = lower 16 bytes reduced
  mov ah,[rcx+rdx]                      //ah = higer 16 bytes reduced
end;


function TSlice.DropUpper: TSliver;
//RCX = self
//RAX = @result
asm
  movdqu xmm0,[rcx]
  movdqu xmm1,[rcx+16]
  //movdqu xmm14,[rip+ShuffleMask0]
  //movdqu xmm15,[rip+ShuffleMask1]
  por xmm0,xmm1      //combine bits 7 (reduce for 256 to 128 bits)
  movhlps xmm6,xmm0
  por xmm0,xmm6      //combine bits 6
  movq rax,xmm0      //load 64 bits into rax
  mov rcx,rax
  shr rcx,32
  or rax,rcx         //fold bit 5, reduce to 32 bit
  mov ecx,eax
  shr eax,16
  or eax,ecx         //fold bit 4, reduce to 16 bits
end;

class function TSlice.FillWithRandomData;
var
  i: integer;
begin
  for i:= 0 to 31 do begin
    Result.FData[i]:= Random(255);
  end;
end;

function TSlice.TestGetBitCounts: TBitCounts;
var
  MaskedA, MaskedB: TSlice;
  i: integer;
  a: integer;
begin
  FillChar(Result, SizeOf(Result),0);
  //Do counts for all 8 masks
  for i:= 0 to 7 do begin
    MaskedA:= Self and TSlice.Mask[i];       //Get the allowed unset pixel configs
    MaskedB:= Self xor MaskedA;              //Get the allowed set pixel configs
    for a:= 0 to 255 do begin                //count bit by bit
      if (a in TByteSet(MaskedA.FData)) then begin
        inc(Result.FData[i][pcOff]);
      end;
      if (a in TByteSet(MaskedB.FData)) then begin
        inc(Result.FData[i][pcOn]);
      end;
    end; {for a}
  end;
end;

function TSlice.GetAllowedStateCounts: integer;
asm
  //rcx = self
  popcnt rax,[rcx]
  popcnt rdx,[rcx+8]
  popcnt r8,[rcx+16]
  popcnt r9,[rcx+24]
  add eax,edx
  add r8,r9
  add eax,r8d
end;

procedure TSlice.GetAllowedStates(out States: TAllowedStates);
var
  NumberOfStates: integer;
  i: integer;
  Index: integer;
begin
  NumberOfStates:= min(High(FData)+1, GetAllowedStateCounts);
  Index:= -1;
  for i:= 0 to NumberOfStates-1 do begin
    Index:= GetNextOption(Index);
    States.FData[i]:= Index;
  end;
end;

function TSlice.GetAllowedBitCounts: TBitCounts;
asm
  //registers
  //RCX = self
  //RDX = @result
  lea rax,[TSlice.Mask+rip]//Get the (address of the) first mask
  movdqu xmm0,[rcx]        //Get the self data
  movdqu xmm1,[rcx+16]
  mov ecx,8                //repeat for all 8 bits
@loop:
  movdqu xmm2,[rax]        //Get mask0 for unset bits
  movdqu xmm3,[rax+16]
  movdqa xmm4,xmm2         //Inverse of the mask for set bits
  movdqa xmm5,xmm3
  pand xmm2,xmm0           //Get number of options for unset bits
  pand xmm3,xmm1
  pandn xmm4,xmm0          //Get number of options of set bits
  pandn xmm5,xmm1
  movq r8,xmm2             //Get the counts in gp-regs
  movq r9,xmm3
  movhlps xmm2,xmm2        //including the high parts
  movhlps xmm3,xmm3
  movq r10,xmm2
  movq r11,xmm3
  popcnt r8,r8             //count number of options for unset bits
  popcnt r9,r9
  popcnt r10,r10
  popcnt r11,r11
  add r8,r9
  add r10,r11
  add r8,r10               //The maximum count is 128, which fits into a byte
  mov [rdx],r8b            //store the count
  //****************************************************************************
  movq r8,xmm4             //Get the counts in gp-regs
  movq r9,xmm5
  movhlps xmm4,xmm4        //including the high parts
  movhlps xmm5,xmm5
  movq r10,xmm4
  movq r11,xmm5
  popcnt r8,r8             //count number of options for set bits
  popcnt r9,r9
  popcnt r10,r10
  popcnt r11,r11
  add r8,r9                //add all the counts
  add r10,r11
  add r8,r10               //The maximum count is 128, which fits into a byte
  mov [rdx+1],r8b          //store the count
  lea rdx,[rdx+2]          //next two bytes
  lea rax,[rax+32]         //process next qqword mask
  sub ecx,1                //repeat 8x
jnz @loop
  rep ret
end;

function TSlice.GetAllowedStateCounts(p: cardinal): integer;
asm
  //registers
  //RCX = self
  //edx = p
  //eax = result.
  lea rax,[TSlice.Mask+rip]//Get the (address of the) first mask
  movdqu xmm0,[rcx]        //Get the self data
  movdqu xmm1,[rcx+16]
  shl edx,5                //p:= p *32
  //Get the correct mask for the
  movdqu xmm2,[rax+rdx]        //Get mask for unset bits
  movdqu xmm3,[rax+rdx+16]
  movdqa xmm4,xmm2         //Inverse of the mask for set bits
  movdqa xmm5,xmm3
  pand xmm2,xmm0           //Get number of options for unset bits
  pand xmm3,xmm1
  pandn xmm4,xmm0          //Get number of options of set bits
  pandn xmm5,xmm1
@count_set_bits:
  movq r8,xmm2             //Get the counts in gp-regs
  movq r9,xmm3
  movhlps xmm2,xmm2        //including the high parts
  movhlps xmm3,xmm3
  movq r10,xmm2
  movq r11,xmm3
  popcnt rax,r8             //count number of options for unset bits
  popcnt r9,r9
  popcnt r10,r10
  popcnt r11,r11
  add rax,r9
  add r10,r11
  add rax,r10               //The maximum count is 128, which fits into a byte
  mov ecx,eax               //save for later use
  //****************************************************************************
@count_unset_bits:
  movq r8,xmm4             //Get the counts in gp-regs
  movq r9,xmm5
  movhlps xmm4,xmm4        //including the high parts
  movhlps xmm5,xmm5
  movq r10,xmm4
  movq r11,xmm5
  popcnt r8,r8             //count number of options for set bits
  popcnt r9,r9
  popcnt r10,r10
  popcnt r11,r11
  add r8,r9                //add all the counts
  add r10,r11
  add r8,r10               //The maximum count is 128, which fits into a byte
  mov rdx,r8               //save for later use
  //test to see if we have a zero, this forces the pixel.
  add eax,r8d              //if we have an invalid slice then return zero.
  jz @done
  cmp eax,edx              //return the bit with the lowest option count
  ja @fewer_unset_options
@fewer_set_options:
  inc eax                 //Result = set_count+1
  ret
@fewer_unset_options:
  not edx                 //result = -(unset_count+1)
  mov eax,edx
  ret
  @done:
  rep ret
end;


function TSlice.TestGetNextOption(previous: integer): integer;
var
  i: integer;
begin
  Result:= 256; //assume failure
  for i:= previous+1 to 255 do begin
    if (i in TByteSet(Self.FData)) then exit(i);
  end;
end;

function TSlice.GetNextOption(previous: integer): integer;
asm
  //rcx = self  (32 bytes of data = 4 qwords
  //edx = previous, a number between -1 and 255
  mov r11,rcx                //save self
  mov r10d,64                //A block is 64 bits - previous
  mov ecx,edx                //put previous in cl for shifting
  and ecx,63                 //max = 63
  inc ecx                    //ecx is the amount of bits to shift out.
  sub r10d,ecx               //r10d is the amount of bits left after shifting
  cmp edx,191                //if previous > 191
  jge @morethan192           //then only process the last block.
  cmp edx,127                //if previous > 127
  jge @morethan128           //then process blocks 3+4
  cmp edx,63                 //if previous > 63
  jge @morethan64            //then process block 2+3+4
@lessthan63:
  mov r8,[r11]               //get the data
  shr r8,cl                  //shift out -previous- number of bits
  bsf rax,r8                 //get the first set bit, rax = offset, ZF=1 if input is zero
  mov cl,0                   //reset the shift
  jnz @done                  //if we found something, we're done
  mov edx,63                 //force previous to 63 and move to the next block
@morethan64:
  mov r8,[r11+8]             //get the data for block 2
  shr r8,cl                  //shift out -previous- number of bits
  bsf rax,r8                 //get the first set bit, rax = offset, ZF=1 if input is zero
  mov cl,0                   //reset the shift
  jnz @done                  //if we found something, we're done
  mov edx,127                //force previous to 127 and move to the next block
@morethan128:
  mov r8,[r11+16]            //get the data for block 3
  shr r8,cl                  //shift out -previous- number of bits
  bsf rax,r8                 //get the first set bit, rax = offset, ZF=1 if input is zero
  mov cl,0                   //reset te shift
  jnz @done                  //if we found something, we're done
  mov edx,191                //force previous to 191 and move to the next block
@morethan192:
  mov r8,[r11+24]            //get the data
  shr r8,cl                  //shift out -previous- number of bits
  bsf rax,r8                 //get the first set bit, rax = offset, ZF=1 if input is zero
  jnz @done                  //if we found something, we're done
  mov edx,255                //force previous to 63 and move to the next block
@done:
  lea eax,[eax+edx+1]        //Result = previous + 1 + first bit found
end;

class constructor TSlice.Init;
var
  i: integer;
begin
  for i:= 0 to 31 do begin
    //The first state encoded (0) is all zero's, therefor an odd mask encodes all
    //unset pixel configs.
    Mask[0][i]:= $55;
    Mask[1][i]:= $33;
    Mask[2][i]:= $0F;
    if Odd(i) then Mask[3][i]:= $FF else Mask[3][i]:= $00;
    if Odd(i shr 1) then Mask[4][i]:= $FF else Mask[4][i]:= $00;
    if Odd(i shr 2) then Mask[5][i]:= $FF else Mask[5][i]:= $00;
    if Odd(i shr 3) then Mask[6][i]:= $FF else Mask[6][i]:= $00;
    if Odd(i shr 4) then Mask[7][i]:= $FF else Mask[7][i]:= $00;
  end;
end;

var
  TransformationMatrix: array[0..255] of byte;

//optimization for later
class procedure TSlice.InitTransformationMatrix;
var
  i: integer;
begin
    //i = the old location
  for i:= 0 to 255 do begin
    //calculate the new location.
    TransformationMatrix[i]:= (i and 3) + (i and 4) shl 3 + (i and (8+16)) + (i and 32) shr 3 + (i and (64+128));
  end;
end;




//function TSlice.IsValid: boolean;
//asm
//  movdqu xmm0,[rcx]
//  movdqu xmm1,[rcx+16]
//  por xmm0,xmm1            //add the two halves together
//  movhlps xmm1,xmm0        //fold the high and low parts
//  por xmm0,xmm1            //pack the bits in 8 bytes
//  mov ecx,1                //ecx = true
//  movq rax,xmm0            //mov to rax, so we can test the result.
//  test rax,rax             //is the result zero?
//  cmovnz eax,ecx           //if not result = valid (true), if zero, result = invalid (false)
//end;

function TSlice.IsValid: boolean;
var
  i: integer;
begin
  //A slice is 32 bytes
  for i:= 0 to 31 do begin

  end;
end;

class operator TSlice.LogicalNot(const A: TSlice): TSlice;
asm
  //rcx = result
  //rdx = A
  movdqu xmm0,[rdx]       //load A
  movdqu xmm1,[rdx+16]
  pcmpeqq xmm2,xmm2       //xmm2 =-1 as an inversion mask for xor
  pxor xmm0,xmm2
  pxor xmm1,xmm2
  movdqu [rcx],xmm0
  movdqu [rcx+16],xmm1
end;

procedure TSlice.RemoveForbiddenCore(ForbiddenCore: TSimpleCore; Orientation: TPos);
var
  SliceIndex: integer;
  Core: TSimpleCore;
begin
  if Orientation.IsFlat then Core:= ForbiddenCore
  else Core:= ForbiddenCore.FlatToStanding;
  Self:= Self and TSimpleCoreToSliceLookupTable.FEveryThingButTheCore[Core.FData].FData[Orientation.Position];
end;


procedure TSlice.SetBorderPixel(index: integer; PixelConfig: TPixelState);
begin
  case PixelConfig of
    pcOff: Self:= Self and not(TSlice(Mask[index]));
    pcOn: Self:= Self and Mask[index];
  end;
end;

class function TSlice.SimpleCoreToSliceIndex(SimpleCore: TSimpleCore): integer;
asm
  //ecx = simplecore
  //eax = output
  lea eax,[ecx+ecx]   //shift by 1
  and eax,6           //mask off bits 12
  shl ecx,3           //bits 23->56
  and ecx,32+64       //Mask off bits 56
  or eax,ecx          //combine bits 12 + 56
end;


function TSlice.TransformFlatCoreToStandingCore: TSlice;
var
  i, NewLocation: integer;
begin
  Result:= Self;
  //i = the old location
  for i:= 1 to 253 do begin //the first and last 2 elements are never swapped.
    //calculate the new location.
    NewLocation:= (i and 3) + (i and 4) shl 3 + (i and (8+16)) + (i and 32) shr 3 + (i and (64+128));
    if (NewLocation > i) then Result.BitSwap(i, NewLocation);
  end;
end;




{ TPixel }


class function TCore.Combine(const A, B: TCore): TCore;
begin
  Result.FData:= TCore.CombineLookupTable[Idx(A,B)];
end;

function TCore.FlatToStanding: TCore;
asm
  mov eax,ecx
  mov edx,ecx
  and eax,$30
  and edx,$c0
  shr edx,2
  shl eax,2
  or eax,edx
  and ecx,$0f
  or eax,ecx
end;

function TCore.GetPixel(const Index: Integer): TPixelState;
begin
  Result:= TPixelState((FData shr (Index * 2)) and 3);
end;

class function TCore.Idx(a, b: TCore): integer;
begin
  Result:= (A.FData * 256) + B.FData;
end;

class operator TCore.Implicit(a: TCore): integer;
begin
  Result:= a.FData;
end;

class constructor TCore.InitLookuptable;
var
  i,j,k: integer;
  A,B: TCore;
  Result: TCore;
begin
  for i:= 0 to 255 do begin
    for j:= 0 to 255 do begin
      A.FData:= i;
      B.FData:= j;
      for k:= 0 to 3 do begin
        //prioritize A
        if (A.Pixel[k] in [psOn, psOff]) then Result.Pixel[k]:= A.Pixel[k]
        //prioritize a known state over a don't care or unknown state
        else if (B.Pixel[k] in [psOn, psOff]) then Result.Pixel[k]:= B.Pixel[k]
        //prioritize psDontCare (3) over psUnknown (2);
        else Result.Pixel[k]:= Max(A.Pixel[k],B.Pixel[k]);
      end;
    end;
  end;
end;

function TCore.IsFixed: boolean;
begin
  Result:= (FData and $AA) = 0;
end;

procedure TCore.SetPixel(const Index: Integer; const Value: TPixelState);
var
  Mask: byte;
begin
  //2 set bits (out of 8){because 4 pixels} (for the 4 states) shifted by the index.
  Mask:= not(3 shl (Index * 2));
  //Reset the Pixel and set the new state
  FData:= (FData and Mask) or (Ord(Value) shl (Index * 2));
end;

procedure TCore.SetPixels(TL, TR, BL, BR: TPixelState);
begin
  Self.FData:= Ord(TL) or (Ord(TR) shl 2) or (Ord(BL) shl 4) or (Ord(BR) shl 6);
end;

{ TLookuptable }

class constructor TFullCoreToSliceLookuptable.CreateLookupTable;
var
  i,j,a,x: integer;
  CellBlock: TCellBlock;
  Conversion: array[0..3] of TSlices;
  Flavor: TPixelState;
begin
  CellBlock:= TCellBlock.Create(0,0,nil);
  for i:= 0 to $FFFF do begin
    CellBlock.SetMiniCore(i);
    CellBlock.GeneratePtoQ;
    a:= CellBlock.Get2x2(@CellBlock.q[17],6);
    a:= (a and 1) or ((a and 2) shl 2) or ((a and 4) shl 4) or ((a and 8) shl 6);
    TFullCoreToSliceLookuptable.FFlat[a].AddFlat4x4Past(i);
    TFullCoreToSliceLookuptable.FStanding[a].AddStanding4x4Past(i);
  end;
  //Now we have pasts for the 16 slices with all known futures.
  //Make the pasts for unknown futures.
  for i:= 0 to 255 do begin
    a:= (i and not $AA);
    if (i = a) then continue;
    for j:= 0 to 3 do begin
      Conversion[j]:= TSlices.Empty;
      Flavor:= TPixelState((i shr (j*2)) and 3);
      if (i and (2 shl (j * 2))) <> 0 then begin
        //We have an unknown or don't care pixel.
        //Combine the two planes accordingly.
        x:= a xor (1 shl (j * 2));
        case Flavor of
          psUnknown: begin
            Conversion[j].Flat:= TFullCoreToSliceLookuptable.FFlat[a] or TFullCoreToSliceLookuptable.FFlat[x];
            Conversion[j].Standing:= TFullCoreToSliceLookuptable.FStanding[a] or TFullCoreToSliceLookuptable.FStanding[x];
          end;
          psDontCare: begin
            Conversion[j].Flat:= TFullCoreToSliceLookuptable.FFlat[a] and TFullCoreToSliceLookuptable.FFlat[x];
            Conversion[j].Standing:= TFullCoreToSliceLookuptable.FStanding[a] and TFullCoreToSliceLookuptable.FStanding[x];
          end;
        end;
      end;
    end;
    TFullCoreToSliceLookuptable.FFlat[i]:= Conversion[0].Flat or Conversion[1].Flat or Conversion[2].Flat or Conversion[3].Flat;
    TFullCoreToSliceLookuptable.FStanding[i]:= Conversion[0].Standing or Conversion[1].Standing or Conversion[2].Standing or Conversion[3].Standing;
  end; {for i}
end;

class function TFullCoreToSliceLookuptable.GetSlices(const Core: TCore): TSlices;
begin
  Result.Flat:= FFlat[Core.FData];
  Result.Standing:= FStanding[Core.FlatToStanding.FData];
end;

{ TGrid }

function TGrid.Idx(x, y: integer): integer;
begin
  //we do not use mod, it is too slow.
  if (x < 0) then x:= x + FSizeX else if (x >= FSizeX) then x:= x - FSizeX;
  if (y < 0) then y:= y + FSizeY else if (y >= FSizeY) then y:= y - FSizeY;
  Result:= x + (y * FSizeX);
end;

function TGrid.GetPixel(x, y: integer): TPixelState;
begin
  Result:= FCores[Idx(x,y)].GetPixel(cTL);
end;

function TGrid.GetSlice(x, y: integer; const Index: Integer): PSlice;
begin
  Result:= @FSlices[Idx(x,y)].FData[Index];
end;



procedure TGrid.SetBorderPixels(const Value: TPixelState);
var
  x,y: integer;
begin
  //no need to optimize this routine, we just need to set some pixels.
  //at the start of the our running.
  for x:= 0 to FSizeX-1 do begin
    //Set the north border
    FSlices[Idx(x,0)].Standing.SetBorderPixel(0, Value);
    FSlices[Idx(x,0)].Standing.SetBorderPixel(1, Value);
    //Set the south border
    FSlices[Idx(x,FSizeY-1)].Standing.SetBorderPixel(6, Value);
    FSlices[Idx(x,FSizeY-1)].Standing.SetBorderPixel(7, Value);
  end;
  for y:= 0 to FSizeY-1 do begin
    //Set the east border
    FSlices[Idx(0,y)].Flat.SetBorderPixel(0, Value);
    FSlices[Idx(0,y)].Flat.SetBorderPixel(1, Value);
    //Set the west border
    FSlices[Idx(FSizeX-1,y)].Flat.SetBorderPixel(6, Value);
    FSlices[Idx(FSizeX-1,y)].Flat.SetBorderPixel(7, Value);
  end;
end;

procedure TGrid.SetFuture(const Value: TGrid);
begin
  if (FFuture <> Value) then begin
    FFuture := Value;
    if (FFuture <> nil) then Future.FPast:= self;
  end;
end;

procedure TGrid.SetPast(const Value: TGrid);
begin
  if (FPast <> Value) then begin
  FPast := Value;
  if (FPast <> nil) then Past.FFuture:= self;
  end;
end;

procedure TGrid.SetPixel(x, y: integer; const Value: TPixelState);
begin
  //Set the primary pixel
  FCores[Idx(x, y)].SetPixel(cTL, Value);
  // Also set the neighboring cores
  FCores[Idx(x - 1, y)].SetPixel(cTR, Value);
  FCores[Idx(x - 1, y - 1)].SetPixel(cBR, Value);
  FCores[Idx(x, y - 1)].SetPixel(cBL, Value);
end;

procedure TGrid.SetSlice(x, y: integer; const Index: integer; const Value: PSlice);
begin
  FSlices[Idx(x,y)].FData[Index]:= Value^;
end;


function TGrid.CombineStanding(x,y: Integer): boolean;
var
  UpdatesA, UpdatesB: TUpdates;
begin
  UpdatesA:= VertSlice[x,y].Combine(VertSlice[x,y].DropLeft, VertSlice[x-1,y].DropRight, VertSlice[x-1,y]);
  UpdatesB:= VertSlice[x,y].Combine(VertSlice[x,y].DropRight, VertSlice[x+1,y].DropLeft, VertSlice[x+1,y]);
  Result:= (UpdatesA + UpdatesB) <> [];
end;

var
  LookupVerticalToHorizontalA: array[0..255] of integer;
  LookupVerticalToHorizontalB: array[0..255] of integer;

procedure InitLookupVerticalToHorizontal;
var
  i: integer;
  output: integer;
begin
  for i:= 0 to 255 do begin
    output:= i;
    SwapBits(1,4,output);
    SwapBits(2,8,output);
    SwapBits(3,$c,output);
    SwapBits(7,$d,output);
    SwapBits(6,9,output);
    LookupVerticalToHorizontalA[i]:= Output;
  end;
  for i:= 0 to 255 do begin
    output:= i shl 8;
    SwapBits(2,8,output);
    SwapBits(3,$c,output);
    SwapBits(7,$d,output);
    SwapBits(6,9,output);
    SwapBits($B,$E,output);
    LookupVerticalToHorizontalB[i]:= Output;
  end;
end;


class function TGrid.CombineStates(const A, B: integer; orientation: TPos): int64;
asm
  //ecx = a
  //edx = b
  //r8d = orientation
  or r8,r8
  jnz @Standing
@Flat:
  mov eax,$0f    //get 4 bits
  mov r9,rax     //save the mask for later
  and eax,ecx    //get the first row of 4 bits into output bits 0123
  and ecx,$F0    //get the next row of 4 bits
  shl ecx,16-4   //put row 2 in place
  and r9,rdx     //get the 3rd row of  bits
  and edx,$F0    //Last row of 4 bits
  shl r9,32-0    //put row 3 in place
  shl rdx,48-4   //put row 4 in place
  or eax,ecx     //combine row 1+2
  or rdx,r9      //combine row 3+4
  or rax,rdx     //combine row 1234
  ret
@Standing:
  //Use lookup table to transform A and B from standing to the flat perspective,.
  lea r9,[rip+LookupVerticalToHorizontalA]
  mov ecx,[r9+rcx*4]    //ecx = LookupVerticalToHorizontalA[rcx]
  lea r9,[rip+LookupVerticalToHorizontalB]
  mov edx,[r9+rdx*4]    //edx = LookupVerticalToHorizontalB[rdx]
  shr edx,8     //put B back to its normal position, so we can reuse the code above
  jmp @Flat     //Now process the transformed standing->flat pixel-states using the flat transform code.
end;

constructor TGrid.Create(SizeX: integer = 0; SizeY: integer = 0; Generation: integer = 0);
var
  x,y: integer;
begin
  inherited Create;
  FPixelsChanged:= TList<TPoint>.Create;
  FWorkerBee:= TCellBlock.Create(0,0,0,nil);
  FGeneration:= Generation;  //Just for debugging purposes.
  if (SizeX <> 0) and (SizeY <> 0) then begin
    if (FSizeX <> 0) and (FSizeX <> SizeX) then raise Exception.Create('You can only set the size once');
    if (FSizeX <> 0) and (FSizeX <> SizeX) then raise Exception.Create('You can only set the size once');
    FSizeX:= SizeX;
    FSizeY:= SizeY;
  end;
  SetLength(FSlices,(FSizeX) * (FSizeY));
  SetLength(FCores, (FSizeX) * (FSizeY));
  FillChar(FSlices[0], SizeOf(TSlices) * (FSizeX) * (FSizeY), cSliceUnknown);
  FillChar(FCores[0],  SizeOf(TCore) *   (FSizeX) * (FSizeY), cCoreUnknown);
  //Put a border of zero's around the edge.
  SetBorderPixels(pcOff);
end;

destructor TGrid.Destroy;
begin
  FPixelsChanged.Free;
  FWorkerBee.Free;
  inherited;
end;

{$pointermath on}

procedure TGrid.Display(var Bitmap: TBitmap);
var
  x,y: integer;
  ScanLine: PColor;
  Color: TColor;
begin
  Bitmap.SetSize(FSizeX, FSizeY);
  Bitmap.PixelFormat:= pf32bit;
  for y:= 0 to FSizeY - 1 do begin
    ScanLine:= Bitmap.ScanLine[y];
    for x:= 0 to FSizeX - 1 do begin
      case Self.FCores[idx(x,y)].GetPixel(0) of
        psOff: Color:= clWhite;
        psOn: Color:= clBlack;
        psUnknown: Color:= clRed;
        psDontCare: Color:= clBlue;
      end;
      ScanLine[x]:= Color;
    end; {for x}
  end; {for y}
end;

procedure TGrid.Display(const StringGrid: TStringGrid);
var
  ScanLine: PColor;
  Color: TColor;
  x,y: integer;
  count: integer;
begin
  StringGrid.ColCount:= FSizeX;
  StringGrid.RowCount:= FSizeY;
  for y:= 0 to FSizeY - 1 do begin
    for x:= 0 to FSizeX - 1 do begin
      case Self.FCores[idx(x,y)].GetPixel(0) of
        psOff: StringGrid.Cells[x,y]:= '.';
        psOn: StringGrid.Cells[x,y]:= 'X';
        psUnknown: begin
          Count:= ABSMin(FSlices[Idx(x,y)].Flat.GetAllowedStateCounts(1),FSlices[Idx(x,y)].Standing.GetAllowedStateCounts(1));
          StringGrid.Cells[x,y]:= '?'+Count.ToString;
        end;
        psDontCare: begin
          Count:= ABSMin(FSlices[Idx(x,y)].Flat.GetAllowedStateCounts(1),FSlices[Idx(x,y)].Standing.GetAllowedStateCounts(1));
          StringGrid.Cells[x,y]:= '_'+Count.ToString;
        end;
      end;
    end; {for x}
  end; {for y}
end;

function TGrid.CombineFlat(x,y: Integer): boolean;
var
  UpdatesA, UpdatesB: TUpdates;
begin
  UpdatesA:= HorzSlice[x,y].Combine(HorzSlice[x,y].DropLower, HorzSlice[x,y-1].DropUpper, HorzSlice[x,y-1]);
  UpdatesB:= HorzSlice[x,y].Combine(HorzSlice[x,y].DropUpper, HorzSlice[x,y+1].DropLower, HorzSlice[x,y+1]);
  Result:= (UpdatesA + UpdatesB) <> [];
end;

function TGrid.CalculateFutureCoresAndSlices(x, y: integer{; var Slices: TSlices}): boolean;
var
  hc1,hc2, vc1,vc2: integer;
  FutureCores: TFutureCores;
  Change: boolean;
begin
  Change:= false;
  //Test to see if a slice has less than 16 options left.
  hc1:= HorzSlice[x,y].GetAllowedStateCounts;
  if (hc1 <= 16) then begin
    hc2:= HorzSlice[x,y-2].GetAllowedStateCounts;
    //Yes less than 16, test the slice to the on top
    if ((hc1 * hc2) <= 16) and ((hc1 * hc2 > 0)) then begin
      //We can now calculate all futures and see what comes out.
     FutureCores:= CalculateFutureSlices(HorzSlice[x,y-2]^, HorzSlice[x,y]^, TPos.Flat);
     Change:= FFuture.FSlices[Idx(x,y-1)].RemoveForbiddenCores(FutureCores.GetUniqueCores(hc1*hc2));
     //if (Change) then FFuture.FCores[Idx(x,y-1)]:= FFuture.FSlices[Idx(x,y-1)].Core;
    end;
    hc2:= HorzSlice[x,y+2].GetAllowedStateCounts;
    if ((hc1 * hc2) <= 16) and ((hc1 * hc2 > 0)) then begin
      //We can now calculate all futures and see what comes out.
      FutureCores:= CalculateFutureSlices(HorzSlice[x,y]^, HorzSlice[x,y+2]^, TPos.Flat);
      Change:= Change or FFuture.FSlices[Idx(x,y+1)].RemoveForbiddenCores(FutureCores.GetUniqueCores(hc1*hc2));
      //if (Change) then FFuture.FCores[Idx(x,y+1)]:= FFuture.FSlices[Idx(x,y+1)].Core;
    end;
  end;
  vc1:= VertSlice[x,y].GetAllowedStateCounts;

  if (vc1 <= 16) then begin
    //Yes less than 16, test the slice to the on top
    vc2:= VertSlice[x-2,y].GetAllowedStateCounts;
    if ((vc1 * vc2) <= 16) and ((vc1 * vc2 > 0)) then begin
      //We can now calculate all futures and see what comes out.
      FutureCores:= CalculateFutureSlices(VertSlice[x-2,y]^, VertSlice[x,y]^, TPos.Standing);
      Change:= Change or FFuture.FSlices[Idx(x-1,y)].RemoveForbiddenCores(FutureCores.GetUniqueCores(vc1*vc2));
      //if (Change) then FFuture.FCores[Idx(x-1,y)]:= FFuture.FSlices[Idx(x-1,y)].Core;
    end;
    vc2:= VertSlice[x+2,y].GetAllowedStateCounts;
    if ((vc1 * vc2) <= 16) and ((vc1 * vc2 > 0)) then begin
      //We can now calculate all futures and see what comes out.
      FutureCores:= CalculateFutureSlices(VertSlice[x,y]^, VertSlice[x+2,y]^, TPos.Standing);
      Change:= Change or FFuture.FSlices[Idx(x+1,y)].RemoveForbiddenCores(FutureCores.GetUniqueCores(vc1*vc2));
      //if (Change) then FFuture.FCores[Idx(x+1,y)]:= FFuture.FSlices[Idx(x+1,y)].Core;
    end;
  end;
  if (Change) then FFuture.FDirty:= true;
  Result:= Change;
end;


function TGrid.CalculateFutureSlices(const A,B: TSlice; orientation: TPos): TFutureCores;
var
  i,j: integer;
  indexA, indexB: integer;
  counter: integer;
  ca,cb: integer;
  CombinedStates: TArray<int64>;
  ResultCores: TFutureCores;
  FutureCoreSet: TFutureCoreSet;
begin
  //list all combinations of A and B states
  indexA:= -1;
  counter:= 0;
  ca:= A.GetAllowedStateCounts;
  cb:= B.GetAllowedStateCounts;
  if (ca * cb) = 0 then exit(TFutureCores.Empty);
  SetLength(CombinedStates, ca*cb);
  for i:= 0 to ca-1 do begin
    IndexA:= A.GetNextOption(indexA);
    indexB:= -1;
    for j:= 0 to cb-1 do begin
      indexB:= B.GetNextOption(indexB);
      //Extract stateA + B and paste it in two TCellBlocks.
      CombinedStates[counter]:= CombineStates(IndexA, IndexB, orientation);
      Inc(Counter);
    end;
    //Calculate 16 4x4 Blocks and store as 16 2x2 cores
    Result:= FWorkerBee.CalculatePtoQ_16_4x4_Blocks(CombinedStates);
  end;
end;

function TGrid.CombineCross(x,y: Integer): boolean;
var
  Updates: TUpdates;
  TransformedSlice: TSlice;
begin
  TransformedSlice:= VertSlice[x,y]^;
  //TransformedSlice.BitSwap(2,5);
  TransformedSlice:= TransformedSlice.TransformFlatCoreToStandingCore;

  Updates:= HorzSlice[x,y].Combine(HorzSlice[x,y].DropOuter, TransformedSlice.DropOuter, @TransformedSlice);
  if (uBChanged in Updates) then begin
    //TransformedSlice.BitSwap(2,5);
    //Transform the slice (including updates) back.
    TransformedSlice:= TransformedSlice.TransformFlatCoreToStandingCore;
    VertSlice[x,y]:= @TransformedSlice;
  end;
  Result:= Updates <> [];
end;


function TGrid.WalkTheGrid: integer;
var
  x,y,i,x1,y1: integer;
  Change, ChangeFlat, ChangeStanding, ChangeCross: boolean;
  BitCounts: TBitCounts;
  ForcedBits: TForcedBits;
  ForceOn: boolean;
  BitNr: integer;
begin
  Result:= 0;
  for y:= 0 to FSizeX-1 do begin
    for x:= 0 to FSizeY-1 do begin
      //take every standing slice and confront it with its left and right neighbor.
      //FSlices[Index(x,y)].Standing:= FSlices[Index(x-1,y)].Standing and FSlices[Index(x,y)].Standing and FSlices[Index(x+1,y)].Standing;
      //like so, but taking into account the nature of the overlap
      ChangeStanding:= CombineStanding(x,y);
      ChangeFlat:= CombineFlat(x,y);
      ChangeCross:= CombineCross(x,y);
      ChangeStanding:= ChangeStanding or ChangeCross;
      ChangeFlat:= ChangeFlat or ChangeCross;
      if (ChangeFlat or Dirty) then begin
        Inc(Result);
        //Test to see if we have any forced bits, if so force that change
        BitCounts:= HorzSlice[x,y].GetAllowedBitCounts;
        ForcedBits:= BitCounts.GetForcedBits;
        if (ForcedBits.GetForcedBitCount <> 0) then begin
          i:= -1;
          repeat
            i:= ForcedBits.GetNextForcedBit(i);
            ForceOn:= Odd(i);
            Bitnr:= i shr 1;
            x1:= x + (Bitnr and 3) -1;
            y1:= y + ((Bitnr and 4) shr 2);
            Self.Pixel[x1,y1]:= TPixelState(ForceOn);
            if not(FPixelsChanged.Contains(point(x1,y1))) then FPixelsChanged.Add(point(x1,y1));
          until (i = -1);
        end;
      end; {if Flat Changed}
      if (ChangeStanding or Dirty) then begin
        Inc(Result);
        // Test to see if we have any forced bits, if so force that change
        BitCounts:= VertSlice[x, y].GetAllowedBitCounts;
        ForcedBits:= BitCounts.GetForcedBits;
        if (ForcedBits.GetForcedBitCount <> 0) then begin
          i:= -1;
          repeat
            i:= ForcedBits.GetNextForcedBit(i);
            ForceOn:= Odd(i);
            BitNr:= i shr 1;
            x1:= x + ((BitNr and 4) shr 2);
            y1:= y + ((BitNr and 3) -1);
            Self.Pixel[x1,y1]:= TPixelState(ForceOn);
            if not(FPixelsChanged.Contains(point(x1,y1))) then FPixelsChanged.Add(point(x1,y1));
          until (i = -1);
        end;
      end; { if Flat Changed }
    end; {for x}
  end; {for y}
  FDirty:= false;
end;

procedure TGrid.Validate;
var
  x, y: integer;
begin
  for y:= 0 to FSizeX - 1 do begin
    for x:= 0 to FSizeY - 1 do begin
      Assert(Self.FSlices[Idx(x,y)].Standing.IsValid);
      Assert(Self.FSlices[Idx(x,y)].Flat.IsValid);
    end;
  end;
end;


procedure TGrid.LookupSliceForCore(x, y: integer);
var
  Index: integer;
begin
  Index:= Idx(x,y);
  FSlices[Index]:= TFullCoreToSliceLookuptable.Slices[FCores[Index]];
end;

function TGrid.ProcessCoresToPastGrid: integer;
var
  p: TPoint;
  Change: boolean;
begin
  Result:= 0;
  //Assume that all changes to a core have already been processed in the current slice.
  for p in FPixelsChanged do begin
    // Process the 4 cores that have had pixels changed.
    // Set the primary pixel
    if (Idx(p.x,p.y) >= Length(FSlices)) or (Idx(p.x,p.y) < 0) then begin
      Assert(false, 'Idx('+p.x.ToString+','+p.y.ToString+') = '+Idx(p.x,p.y).ToString);
    end;
    if (Idx(p.x-1,p.y) >= Length(FSlices)) or (Idx(p.x-1,p.y) < 0) then begin
      Assert(false, 'Idx('+(p.x-1).ToString+','+p.y.ToString+') = '+Idx(p.x-1,p.y).ToString);
    end;
    if (Idx(p.x-1,p.y-1) >= Length(FSlices)) or (Idx(p.x-1,p.y-1) < 0) then begin
      Assert(false, 'Idx('+(p.x-1).ToString+','+(p.y-1).ToString+') = '+Idx(p.x-1,p.y-1).ToString);
    end;
    if (Idx(p.x,p.y-1) >= Length(FSlices)) or (Idx(p.x,p.y-1) < 0) then begin
      Assert(false, 'Idx('+p.x.ToString+','+(p.y-1).ToString+') = '+Idx(p.x,p.y-1).ToString);
    end;
    Change:= FPast.FSlices[Idx(p.x,p.y)].Combine(TFullCoreToSliceLookuptable.Slices[FCores[Idx(p.X,p.Y)]]) or
      //And process the surrounding cores as well.
      FPast.FSlices[Idx(p.x-1,p.y)].Combine(TFullCoreToSliceLookuptable.Slices[FCores[Idx(p.X-1,p.Y)]]) or
      FPast.FSlices[Idx(p.x-1,p.y-1)].Combine(TFullCoreToSliceLookuptable.Slices[FCores[Idx(p.X-1,p.Y-1)]]) or
      FPast.FSlices[Idx(p.x,p.y-1)].Combine(TFullCoreToSliceLookuptable.Slices[FCores[Idx(p.X,p.Y-1)]]);
    //Note any changes.
    //They will have to be worked through in a later WalkTheGrid step.
    if (Change) then begin
      Inc(Result);
      FPast.FDirty:= true;
    end;
  end;
  //All cores are processed, clear the list.
end;

function TGrid.ProcessEntailments: integer;
var
  ChangesA, ChangesB, ChangesC: integer;
begin
  Result:= 0;
  repeat
    //First do a full walk of all pixels in the grid
    ChangesA:= WalkTheGrid;
    //If there are any changes then see if we can propagate the changes in te current cores to past slices
    ChangesB:= 0;
    ChangesC:= 0;
    if (FPixelsChanged.Count > 0) then begin
      if Assigned(Past) then ChangesB:= ProcessCoresToPastGrid;
      if Assigned(Future) then ChangesC:= ProcessSlicesToFutureCores;
      FPixelsChanged.Clear; //All changes in slices have been processed, clear the list.
    end;
    if (ChangesA > 0) then Inc(Result);
  until (ChangesA) = 0;
end;

function TGrid.ProcessSlicesToFutureCores: integer;
var
  p: TPoint;
  Change: boolean;
begin
  Result:= 0;
  //Assume that all changes to a core have already been processed in the current slice.
  for p in FPixelsChanged do begin
    CalculateFutureCoresAndSlices(p.X,p.Y);
    if (Change) then Inc(Result);
  end;
end;

{ TSlices }

class operator TSlices.BitwiseAnd(const A, B: TSlices): TSlices;
begin
  Result.FData[Vert]:= A.FData[Vert] and B.FData[Vert];
  Result.FData[Horz]:= A.FData[Horz] and B.FData[Horz];
end;

class operator TSlices.BitwiseOr(const A, B: TSlices): TSlices;
begin
  Result.FData[Vert]:= A.FData[Vert] or B.FData[Vert];
  Result.FData[Horz]:= A.FData[Horz] or B.FData[Horz];
end;

function TSlices.Combine(const LookupValue: TSlices): boolean;
begin
  Result:= Self.FData[Vert].Combine(LookupValue.Flat) or
    Self.FData[Vert].Combine(LookupValue.Standing);
end;

class function TSlices.Empty: TSlices;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TSlices.Full: TSlices;
begin
  FillChar(Result, SizeOf(Result), $FF);
end;


function TSlices.GetCore: TCore;
var
  AllowedBitsCounts: TBitCounts;
  ForcedBits: TForcedBits;
  NextForced: integer;
begin
  //Probe bits 1,2,5,6 of the flat slice to reconstruct the inner core
  AllowedBitsCounts:= Flat.GetAllowedBitCounts;
  //Get the forced bits
  ForcedBits:= AllowedBitsCounts.GetForcedBits;
  //Do we have any forced bits in the core?
  NextForced:= ForcedBits.GetNextForcedBit(1);
  Result.FData:= cCoreUnknown;
  if (NextForced = NotFound) then exit;  //if no forced bits, then we're done
  while NextForced <> -1 do begin
    //check to see if any bits are forced in the core.
    case NextForced of
      //Double check the direction of the bits, perhaps we need to reverse, a forced
      //bit forces its opposite.
      2: Result.TL:= psOn;
      3: Result.TL:= psOff;
      4: Result.TR:= psOn;
      5: Result.TR:= psOff;
     10: Result.BL:= psOn;
     11: Result.BL:= psOff;
     12: Result.BR:= psOn;
     13: Result.BR:= psOff;
    end; {case}
    NextForced:= ForcedBits.GetNextForcedBit(NextForced);
  end; {while}
end;

function TSlices.RemoveForbiddenCores(const AllowedCores: TFutureCoreSet): boolean;
var
  i,j: integer;
  count: integer;
begin
  //Track the number of allowed states before
  count:= FData[Horz].GetAllowedStateCounts + FData[Vert].GetAllowedStateCounts;
  Self:= Self and AllowedCores.GetAllowedSlices;

//  //Look at all positions of the set
//  for i:= 0 to 15 do begin
//    if (not(i in AllowedCores)) then begin
//      //Remove the 16 variants from the slice pair around the future core.
//      Self:= Self and TSimpleCoreToSliceLookupTable.FFutureCores[TFutureCoreIndex(i)];
//    end;
//  end;

  //if the number of states has reduced then we have a change.
  //Obviously it cannot increase.
  Result:= count <> (FData[Horz].GetAllowedStateCounts + FData[Vert].GetAllowedStateCounts);
end;

{ TBitCounts }

class operator TBitCounts.NotEqual(const A, B: TBitCounts): boolean;
begin
  Result:= not(A = B);
end;

class operator TBitCounts.Equal(const A, B: TBitCounts): boolean;
asm
  //registers
  //RCX = @A
  //RDX = @B
  xor eax,eax           //assume failure
  mov r9,[rcx]
  xor r9,[rdx]          //zero if equal, nz otherwise
  mov r10,[rcx+8]
  xor r10,[rdx+8]        //zero if equal, nz otherwise
  or r9,r10              //zero if both are equal
  setz al               //return true if both are equal
end;

function TBitCounts.GetCount(Pixel: integer; Config: TPixelState): integer;
begin
  Result:= FData[Pixel, Config];
end;

function TBitCounts.GetForcedBits: TForcedBits;
asm
  //RCX = self
  //RAX = result
  movdqu xmm1,[rcx]     //Get 16 bytes
  pxor xmm0,xmm0        //Compare against zero
  pcmpeqb xmm1,xmm0     //for each individual byte
                        //A byte = $FF if zero, $00 if not.
  pmovmskb eax,xmm1     //put the bytemask into eax, one bit per byte
                        //A bit is set if the byte was zero, unset if it was not.
end;


{ TForcedBitsHelper }

function TForcedBitsHelper.GetForcedBitCount: integer;
asm
  movzx eax,word ptr [rcx]
  popcnt eax,eax
end;

function TForcedBitsHelper.GetNextForcedBit(previous: integer): integer;
asm
  movzx eax, word ptr[rcx]      //the set is two bits in size
  lea ecx,[edx+1]               //ecx=previous+1
  shr eax,cl                    //mask off the bits already visited
  jz @NotFound                  //if zero remains, return -1
  rep bsf eax,eax               //skip to the next set bit
  add eax,ecx                   //add previous to the count
  ret
  @NotFound:
  mov eax,NotFound
  ret
end;

{ TFutureCores }

const
  MiniCoreToCoreLookup: array[0..15] of byte = ($00,$01,$04,$05,$10,$11,$14,$15,$40,$41,$44,$45,$50,$51,$54,$55);

function TFutureCores.GetUniqueCores(CoreCount: integer): TFutureCoreSet;
asm
  mov r8d,edx         //r8 = count
  mov rcx,[rcx]       //Get the data.
  xor eax,eax         //Result:= 0
@loop:
  mov edx,ecx
  and edx,$0F         //Get a single nibble
  bts rax,rdx         //Add that core to the set
  shr rcx,4           //process the next core
  sub r8d,1           //are we done?
jnz @loop
  rep ret
end;

class function TFutureCores.Empty: TFutureCores;
begin
  Result.FData:= 0;
end;

function TFutureCores.GetCore(index: integer): TCore;
asm
  //rcx = self
  //edx = index
  mov rax,[rcx]      //get the data
  mov ecx,edx        //cl = index
  lea rdx,[rip+MiniCoreToCoreLookup]   //Get the address of the lookup table
  //every core takes 4 bits
  shl ecx,2          //multiply by 4
  shr rax,cl
  and eax,$0F        //Mask off the upper cores
  //now we need to translate the core to a core format, including the "don't care" and "unknown" bits.
  movzx eax,byte ptr [rcx+rax]  //lookup the data
end;

class operator TFutureCores.Implicit(const input: Int64): TFutureCores;
begin
  Result.FData:= input;
end;

{ TFutureCoreSetHelper }

function TFutureCoreSetHelper.Count: integer;
asm
  movzx eax,word ptr [rcx]      //Get 16 bits
  popcnt eax,eax                //return the number of set bits.
end;

function TFutureCoreSetHelper.GetAllowedCore: TCore;
begin
  Result:= TSimpleCoreToSliceLookupTable.FFutureCore[TFutureCoreIndex(Self)];
end;

function TFutureCoreSetHelper.GetAllowedSlices: TSlices;
begin
  Result:= TSimpleCoreToSliceLookupTable.FFutureSlices[TFutureCoreIndex(Self)];
end;



{ TSimpleCore }

class operator TSimpleCore.Implicit(a: integer): TSimpleCore;
begin
  Result.FData:= a;
end;

class operator TSimpleCore.Implicit(a: TSimpleCore): integer;
begin
  Result:= a.FData;
end;

function TSimpleCore.FlatToStanding: TSimpleCore;
asm
  mov eax,ecx    //result = input
  mov edx,2+4    //Get the middle two bits
  and edx,ecx    //edx = the 2 middle bits of A
  xor ecx,6      //flip the middle bits (if different)
  popcnt edx,edx //popcnt = 1 if the bits are different
  sub edx,1      //are the bits different?
  cmovz eax,ecx  //yes, result = flipped middle bits
end;



{ TSimpleCoreToSliceLookupTable }

class constructor TSimpleCoreToSliceLookupTable.CreateLookupTables;
var
  i,j: integer;
  SliceIndexFlat: integer;
  SliceIndexStanding: integer;
  Count: integer;
begin
  FillChar(FJustTheSlices, SizeOf(FJustTheSlices), 0);
  for i:= 0 to 15 do begin
    //if (Orientation = cStanding) then SwapBits(2,5,ForBiddenCore);
  //the core occupies bits 1256
    SliceIndexFlat:= TSlice.SimpleCoreToSliceIndex(i);
    j:= i;
    SwapBits(2,5,j);
  //the core occupies bits 1256
    SliceIndexStanding:= TSlice.SimpleCoreToSliceIndex(j);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $01);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $08);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $09);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $10);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $11);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $18);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $19);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $80);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $81);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $88);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $89);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $90);
    Include(TByteSet(FJustTheSlices[i].Fdata[Horz]), SliceIndexFlat or $91);
    Include(TByteSet(FJustTheSlices[i].FData[Horz]), SliceIndexFlat or $98);
    Include(TByteSet(FJustTheSlices[i].FData[Horz]), SliceIndexFlat or $99);

    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $01);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $08);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $09);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $10);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $11);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $18);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $19);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $80);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $81);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $88);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $89);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $90);
    Include(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $91);
    Include(TByteSet(FJustTheSlices[i].FData[Vert]), SliceIndexStanding or $98);
    Include(TByteSet(FJustTheSlices[i].FData[Vert]), SliceIndexStanding or $99);
  end;

  FillChar(FEverythingButTheCore, SizeOf(FEverythingButTheCore), 0);
  for i:= 0 to 15 do begin
    //if (Orientation = cStanding) then SwapBits(2,5,ForBiddenCore);
  //the core occupies bits 1256
    SliceIndexFlat:= TSlice.SimpleCoreToSliceIndex(i);
    j:= i;
    SwapBits(2,5,j);
  //the core occupies bits 1256
    SliceIndexStanding:= TSlice.SimpleCoreToSliceIndex(j);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $01);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $08);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $09);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $10);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $11);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $18);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $19);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $80);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $81);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $88);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $89);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $90);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $91);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $98);
    Exclude(TByteSet(FEverythingButTheCore[i].Fdata[Horz]), SliceIndexFlat or $99);

    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $01);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $08);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $09);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $10);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $11);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $18);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $19);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $80);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $81);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $88);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $89);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $90);
    Exclude(TByteSet(FJustTheSlices[i].Fdata[Vert]), SliceIndexStanding or $91);
    Exclude(TByteSet(FJustTheSlices[i].FData[Vert]), SliceIndexStanding or $98);
    Exclude(TByteSet(FJustTheSlices[i].FData[Vert]), SliceIndexStanding or $99);
  end;

  FillChar(FFutureSlices, SizeOf(FFutureSlices), #0);
  for i:= 0 to High(TFutureCoreIndex) do begin
    for j:= 0 to 15 do begin
      if (j in TFutureCoreSet(TFutureCoreIndex(i))) then begin
        FFutureSlices[i]:= FFutureSlices[i] or FJustTheSlices[j];
      end;
    end;
  end;

  FillChar(FFutureCore, SizeOf(FFutureCore), #0);
  for i:= 0 to High(TFutureCoreIndex) do begin
    //Get TL: pixel

  end;
end;



class function TSimpleCoreToSliceLookupTable.PositiveSlicesFromCore(const Core: TSimpleCore): TSlices;
begin
  Result:= FJustTheSlices[Integer(Core)];
end;

class function TSimpleCoreToSliceLookupTable.NegativeSlicesFromCore(const Core: TSimpleCore): TSlices;
begin
  Result:= FEverythingButTheCore[Integer(Core)];
end;

{ OrientationHelper }

class function OrientationHelper.Flat: TPos;
begin
  Result:= [oFlat];
end;

function OrientationHelper.IsFlat: Boolean;
begin
  Result:= oFlat in Self;
end;

function OrientationHelper.Position: Integer;
begin
  if (oFlat in Self) then Result:= 0 else Result:= 1;
end;

class function OrientationHelper.Standing: TPos;
begin
  Result:= [oStanding];
end;

{ TSliverHelper }

function TSliverHelper.ExpandToSlice: TSlice;
begin

end;

class constructor TSliverHelper.Init;
type
  TWordSet = set of 0..31;   //use integers for performance reasons
var
  i,j,k: integer;
  Output: integer;
  bit: array[0..3] of integer;
begin
  for i:= 0 to $FFFF do begin
    Output:= i;
    //The first and last 2 elements are never swapped.
    for j:= 1 to 15-2 do begin
      k:= (j and 1) + ((j and 2) shl 1) + ((j and 4) shr 1) + (j and 8);
      if (k > j) then SwapBits(k,j,Output);
    end; {for j}
    SliverTransposeBit12Lookup[i]:= Output;
  end; {for i}
end;

function TSliverHelper.TransposeBit12: TSliver;
begin
  Result:= SliverTransposeBit12Lookup[Self];
end;

{ TChangedSet<K> }



{ TChangedSet<K> }

procedure TChangedSet<K>.Add(index: integer);
type
  TByteSet = set of byte;
  PByteSet = ^TByteSet;
begin
  Include(TByteSet(PByteSet(@FData)^), index);
end;

procedure TChangedSet<K>.Clear;
begin
  FillChar(FData, SizeOf(K), #0);
end;

class function TChangedSet<K>.Create(Max: cardinal; var Data: K): TChangedSet<K>;
begin
  TChangedSet<K>.Max:= Max;
  Result.FEnumerator:= TChangedSet<K>.TSetEnumerator.Create(@Data);
end;

class constructor TChangedSet<K>.Init;
begin
  Assert(GetTypeKind(K) = tkArray);
end;

{ TChangedSet<K>.TSetEnumerator }

constructor TChangedSet<K>.TSetEnumerator.Create(const Data: PK);
begin
  FData:= Data;
  FIndex:= -1;
  FAdjustedIndex:= -1;
end;

function TChangedSet<K>.TSetEnumerator.GetCurrent: boolean;
type
  TByteSet = set of byte;
  PByteSet = ^TByteSet;
begin
  Result:= FAdjustedIndex in TByteSet(PByteSet(self.FData)^);
end;

function TChangedSet<K>.TSetEnumerator.MoveNext: boolean;
begin
  Result:= FIndex < (TChangedSet<K>.Max -1);
  Inc(FIndex);
  Inc(FAdjustedIndex);
  if (FAdjustedIndex > 256) then begin
    Dec(FAdjustedIndex, 256);
    Inc(NativeUInt(FData),256 div 8);
  end;
end;

initialization
  InitLookupVerticalToHorizontal;
end.
