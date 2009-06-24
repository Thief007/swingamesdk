unit integerhash;

interface
uses hashtable, comparable, sysutils;

type
tIntHashIterator = class(tHashTableIterator)
public
   function getKey : integer; reintroduce;

   property key : integer read getKey;
//   property value : tObject read getValue;

//protected
//   constructor create(table : tHashTable);
end;


tIntegerHash = class(tObject)
private
    fHashTable : tHashTable;
protected
    procedure fSetValue(key : integer; value : tObject); virtual;

public
    function getIterator : tIntHashIterator; virtual;
    function containsKey(key : integer) : boolean; virtual;
    function containsValue(value : tObject) : boolean; virtual;
    function getValue(key : integer) : tObject; virtual;
    function setValue(key : integer; value : tObject) : boolean; virtual;
    function remove(key : integer) : tObject; virtual;

    function getCount : integer; virtual;

    property values[key : integer] : tObject read getValue write fsetValue;
    property count : integer read getCount;

    {$IFNDEF FPC}
    constructor create(initialcapacity : integer = 10);
    {$ELSE FPC}
    constructor create;
    constructor create(initialcapacity : integer);
    {$ENDIF FPC}

    destructor destroy; override;

    procedure clear; virtual;
    procedure deleteAll; virtual;
end;

implementation

(* tSHIterator - iterator for integer hash table *)
(* basically an adapter shell for tMapIterator *)

procedure throwTypeException(className : string);
begin
     raise exception.create('Wrong type. Expecting tInteger, got ' + className);
end;


function tIntHashIterator.getKey : integer;
var
   s : tObject;
begin
     s := inherited getKey;
     if not (s is tInteger) then
        throwTypeException(s.ClassName);
     result := tInteger(s).value;
end;


(*
constructor tIntHashIterator.create(table : tHashTable);
begin
     inherited create(table);
end;
*)

(* tIntegerHash *)
procedure tIntegerHash.fSetValue(key : integer; value : tObject);
begin
     setValue(key, value);
end;

function tIntegerHash.getIterator : tIntHashIterator;
begin
     result := tIntHashIterator.create(fHashTable);
end;

function tIntegerHash.containsKey(key : integer) : boolean;
var
   s : tInteger;
begin
   s := tInteger.create(key);
   try
   result := fHashTable.containsKey(s);
   finally
   s.free;
   end;
end;

function tIntegerHash.containsValue(value : tObject) : boolean;
begin
     result := fHashTable.containsValue(tComparable(value))
end;

function tIntegerHash.getValue(key : integer) : tObject;
var
   s : tInteger;
begin
  s := tInteger.create(key);
  try
    result := fHashTable.getValue(s);
  finally
    s.free;
  end;
end;

function tIntegerHash.setValue(key : integer; value : tObject) : boolean;
begin
    result := fHashTable.setValue(tInteger.create(key), value);
end;

function tIntegerHash.remove(key : integer) : tObject;
var
   s : tInteger;
begin
   s := tInteger.create(key);
   try
      result := fHashTable.remove(s);
   finally
      //s.free;
   end;
end;

function tIntegerHash.getCount : integer;
begin
     result := fHashTable.getCount;
end;


{$IFNDEF FPC}
constructor tIntegerHash.create(initialcapacity : integer = 10);
begin
    inherited create;
    fHashTable := tHashTable.create(initialcapacity);

end;

{$ELSE FPC}

constructor tIntegerHash.create;
begin
    inherited create;
    fHashTable := tHashTable.create;
end;

constructor tIntegerHash.create(initialcapacity : integer);
begin
    inherited create;
    fHashTable := tHashTable.create(initialcapacity, 0.75, nil, true);
end;
{$ENDIF FPC}

destructor tIntegerHash.destroy;
begin
     fHashTable.destroy;
     inherited destroy;
end;
procedure tIntegerHash.clear;
begin
     fHashTable.clear;
end;
procedure tIntegerHash.deleteAll;
begin
     fHashTable.deleteAll;
end;


end.
