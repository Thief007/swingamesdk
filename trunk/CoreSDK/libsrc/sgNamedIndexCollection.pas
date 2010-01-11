//=============================================================================
// sgNamedIndexCollection.pas
//=============================================================================
//
// This private SwinGame library unit is responsible for managing named
// collections of indexes.
//
// Change History:
//
// Version 3.0:
// - 2009-12-15: Andrew : Created
//=============================================================================

unit sgNamedIndexCollection;
interface
  uses sgTypes;
  
  /// Get the name of the value at index idx.
  ///
  function NameAt(const col: NamedIndexCollection; idx: Integer): String;
  
  /// Get the index of the specified name.
  ///
  function IndexOf(const col: NamedIndexCollection; name: String): Integer;
  
  /// The number of names in the collection
  ///
  function NameCount(const col: NamedIndexCollection): Integer;
  
  
  /// Add a new name to the index. Returns the index of the added element or
  /// -1 if the add fails.
  ///
  function AddName(var col: NamedIndexCollection; name: String): Integer;
  
  procedure InitNamedIndexCollection(var col: NamedIndexCollection; names: Array of String); overload;
  procedure InitNamedIndexCollection(var col: NamedIndexCollection); overload;
  
  procedure FreeNamedIndexCollection(var col: NamedIndexCollection);
  
implementation
uses sgShared, stringhash;

  function NameAt(const col: NamedIndexCollection;idx: Integer): String;
  begin
    if (idx >= Low(col.names)) and (idx <= High(col.names)) then
      result := col.names[idx]
    else
      result := '';
  end;

  function IndexOf(const col: NamedIndexCollection; name: String): Integer;
  var
    hash: TStringHash;
  begin
    hash := TStringHash(col.ids);
    if assigned(hash) then
      result := TIntegerContainer(hash.values[name]).Value
    else
      result := -1;
  end;

  function NameCount(const col: NamedIndexCollection): Integer;
  begin
    result := Length(col.names);
  end;

  function AddName(var col: NamedIndexCollection; name: String): Integer;
  var
    hash: TStringHash;
  begin
    hash := TStringHash(col.ids);
    
    if assigned(hash) then
    begin
      if hash.containsKey(name) then begin RaiseException('Error: Adding ' + name + ' to the name collection twice.'); exit; end;
      
      SetLength(col.names, Length(col.names) + 1);            // Add to the names array
      result := High(col.names);                              // Record the index of the added name
      hash.setValue(name, TIntegerContainer.Create(result));  // Store this idx in the hashtable
    end
    else
      result := -1;                                           // Failed to add return -1 idx
  end;
  
  procedure InitNamedIndexCollection(var col: NamedIndexCollection; names: Array of String); overload;
  var
    hash: TStringHash;
    i: Integer;
  begin
    hash := TStringHash.Create(False, 1024);   //Create the hash locally and store in col.ids
    col.ids := hash;
    
    SetLength(col.names, Length(names));
    for i := Low(names) to High(names) do
    begin
      col.names[i] := names[i];
      hash.setValue(names[i], TIntegerContainer.Create(i));
    end;
  end;
  
  procedure InitNamedIndexCollection(var col: NamedIndexCollection); overload;
  var
    names: Array of String;
  begin
    SetLength(names, 0);
    
    InitNamedIndexCollection(col, names);
  end;
  
  procedure FreeNamedIndexCollection(var col: NamedIndexCollection);
  var
    hash: TStringHash;
  begin
    hash := TStringHash(col.ids);
    if assigned(hash) then
    begin
      hash.DeleteAll();
      hash.Free();
    end;
    col.ids := nil;
  end;
end.