unit daObjectLib;

interface

{$M+}

uses
  Windows,
  daGlobals,
  Classes,
  daStreamLib;

type
  TcObject = class;

  TcBag = class(TObject)
 {******************************************************************************
  * Author: Regis Charlot
  *         Intelligent Medical Objects, Inc.
  *
  * Description: TcBag is the object-less collection TcObject class. This object
  *              overrides the TList Delphi object, providing basic pointer-list
  *              support for TcObject object lists. Freeing this object DOES NOT
  *              free object listed in the TcBag object.
  *
  * Inheritance: TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 01/11/99 Regis Created
  * 05/28/99 Regis Created from splitting TcCollection
  ******************************************************************************}
  private
    // Private declarations
    //
    m_lstObjects: TList;
    m_pParent: TcObject;

  private
    // private methods
    //
    function    Get(item: longint): TcObject;
    procedure   Put(item: longint; Value: TcObject);
    function    GetCount: longint;
    procedure   LocalClear;
    function    GetTopParent: TcObject;

  published
    // Published declarations
    //
    function    GetIsEmpty: boolean; virtual;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); virtual;                           // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; virtual;                                                 // Clear base method
    procedure   Copy(value: TcBag); overload; virtual;                          // Copy base method
    //   Compare
    function    Text: String; virtual;
    //   Load
    //   Save
    //
    // 2. Custom
    function    Add(parObject: TcObject): longint; overload; virtual;
    function    Add(parObject: TcBag): longint; overload; virtual;
    function    Find(sparName: String): TcObject; overload; virtual;
    function    Find(sparName: String; iparDepth: longint): TcObject; overload; virtual;
    function    Find(iparValue: longint; iparDepth: longint): TcObject; overload; virtual;
    function    FindValue(sparType, sparValue: String; iparDepth: longint): TcObject; virtual;
    function    FindAll(value: String): TcBag;
    function    Delete(item: longint): longint; overload; virtual;
    function    Delete(parObject: TcObject): longint; overload; virtual;
    function    Remove(parObject: TcObject): longint; overload; virtual;
    function    Remove(item: longint): longint; overload; virtual;
    function    IndexOf(parObject: TcObject): longint; virtual;
    function    Insert(Index: longint; parObject: TcObject): longint; virtual;
    procedure   Append(value: TcBag);  overload; virtual;
    procedure   Move(iparFrom, iparTo: integer);
    function    SendToObject(value: TObject): TObject; virtual;
    procedure   Exchange(Item1, Item2: longint);
    procedure   QuickSort(iLo, iHi: longint);

  public
    // Public declarations
    //
    property    Parent: TcObject                     read m_pParent       write m_pParent;
    property    TopParent: TcObject                  read GetTopParent;
    property    Objects[item: longint]: TcObject     read Get             write Put; default;
    property    Count: longint                       read GetCount;
    property    IsEmpty: boolean                     read GetIsEmpty;
  end;

  TcCollection = class(TcBag)
 {******************************************************************************
  * Author: Regis Charlot
  *         Intelligent Medical Objects, Inc.
  *
  * Description: TcCollection is the collection TcObject class
  *
  * Inheritance: TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 01/11/99 Regis Created
  * 05/28/99 Regis Shared with TcBag
  * 08/26/00 Regis Modified to be the base class
  ******************************************************************************}
  private
    // private methods
    //
    procedure   LocalClear;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear base method
    procedure   Copy(value: TcBag); overload; override;                         // Copy base method
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public declarations
    //
    function    Delete(item: longint): longint; override;
    function    Delete(parObject: TcObject): longint; override;
  end;

  TcObject = class(TcCollection)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcObject is the base and unique object for any database
  *              entity. This object is loaded from XML files.
  *
  * Inheritance:
  *   TcCollection
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 08/07/00 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_eType: TeType;
    m_sName: String;
    m_sValue: String;
    m_iTag: longint;

  published
    // Published declarations
    //
    function    GetXML: String; virtual;
    procedure   SetXML(value: String); virtual;
    procedure   SetValue(value: String); virtual;
    function    GetValue: String; virtual;
    function    GetDepth: longint;
    procedure   SetTag(value: longint); virtual;
    function    GetTag: longint; virtual;
    procedure   SetName(value: String); virtual;
    function    GetName: String; virtual;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); overload; override;                // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear the object
    procedure   Copy(value: TcObject); virtual;
    function    Compare(value: TcObject): boolean; virtual;
    //   Text
    //   Load
    //   Save
    function    LoadFromStream(parStream: TcTokenStream): boolean; virtual;     // Serialization (From)
    procedure   SaveToStream(parStream: TcTokenStream); virtual;                // Serialization (To)
    //
    // 2. Custom
    class function CreateSame(parParent: TcObject): TcObject;
    function    CountObjects(value: TeTypeSet): longint;
    function    Find(sparName: String): TcObject; overload; override;
    function    Find(sparName: String; iparDepth: longint): TcObject; overload; override;
    function    FindValue(sparType, sparValue: String; iparDepth: longint): TcObject; override;
    function    Find(iparValue: longint): TcObject; overload;
    function    Find(iparValue: longint; iparDepth: longint): TcObject; overload; override;
    function    Find(Value: TcObject): TcObject; overload; virtual;
    function    Find(parName, parValue: String; parDepth: longint): TcObject; overload;
    function    FindFirst(value: TeType): TcObject;
    function    FindFirstValue(value: TeType): String;
    function    Ancestor(parType: TeType): TcObject;
    function    Has(value: TcObject): boolean; overload; virtual;
    function    Insert(Index: longint; parObject: TcObject): longint; override;
    function    ParentIndex: longint;
    procedure   Exchange(parIndex1, parIndex2: longint);
    function    Child(value: String): TcObject; overload; virtual;
    function    Child(parObject: TcObject): TcObject; overload; virtual;
    function    StreamObject(value: TeObjectMarker): TcObject; virtual;
    procedure   ClearChildren; overload; virtual;
    function    XML_Filtered(parFilter: TcBag; parReplacements: TStringList): String; virtual;
    function    Remove: boolean; overload;
    procedure   SetError(value: String);

  public
    // Public Properties
    //
    property    eType: TeType                        read m_eType         write m_eType;
    property    sName: String                        read GetName         write SetName;
    property    sValue: String                       read GetValue        write SetValue;
    property    XML: String                          read GetXML          write SetXML;
    property    Tag: longint                         read GetTag          write SetTag;
    property    Depth: longint                       read GetDepth;
    property    iCode: longint                       read GetTag          write SetTag;
  end;

{$IFDEF SBS_DEBUG}
var
  gMemList: TcBag;
{$ENDIF}

implementation

uses
  daResourceStrings,
  sysUtils,
  comCtrls,
  stdCtrls;

var
  iObjectTag: longint;

//
// TcBag
//

// TcBag
//   Create
//
constructor TcBag.Create(parParent: TcObject);
begin
  inherited Create;
  m_pParent := parParent;
  m_lstObjects := TList.Create;
  LocalClear;
end;

// TcBag
//   Destroy
//
Destructor TcBag.Destroy;
begin
  m_lstObjects.free;
  inherited Destroy;
end;

// TcBag
//   Clear
//
procedure TcBag.Clear;
begin
  LocalClear;
end;

// TcBag
//   LocalClear
//
procedure TcBag.LocalClear;
begin
  m_lstObjects.Clear;
end;

// TcBag
//   Copy
//
procedure TcBag.Copy(value: TcBag);
begin
  LocalClear;
  Append(value);
end;

// TcBag
//   Text method
//
function TcBag.Text: String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to count - 1 do
    if Objects[i] <> nil then
      result := result + Objects[i].Text;
end;

// TcBag
//   Append (1)
//
procedure TcBag.Append(value: TcBag);
var
  i: longint;
begin
  for i := 0 to value.Count - 1 do
    Add(value[i]);
end;

// TcBag
//   Get
//
function TcBag.Get(item: longint): TcObject;
begin
  if item < GetCount then
    result := m_lstObjects[item]
  else
    raise Exception.Create(Format(krsINDEXOUTOFRANGE, [GetCount, item]));
end;

// TcBag
//   Put
//
procedure TcBag.Put(item: longint; Value: TcObject);
begin
  if (item >=0) and (item < GetCount) then
    m_lstObjects[item] := Value
  else
    raise exception.create(Format(krsINDEXOUTOFRANGE, [GetCount, item]));
end;

// TcBag
//   GetCount
//
function TcBag.GetCount: longint;
begin
  result := m_lstObjects.Count;
end;

// TcBag
//   Find (1)
//
function TcBag.Find(sparName: String): TcObject;
begin
  result := Find(sparName, kiLARGEINT);
end;

// TcBag
//   Find (2)
//
function TcBag.Find(sparName: String; iparDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  // Traverse children recursively
  if iparDepth >= 0 then
    for i := 0 to GetCount - 1 do
    begin
      result := Get(i).Find(sparName, iparDepth);
      if result <> nil then
        break;
    end;
end;

// TcBag
//   Find (3)
//
function TcBag.Find(iparValue: longint; iparDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  // Traverse children recursively
  if iparDepth >= 0 then
    for i := 0 to GetCount - 1 do
    begin
      result := Get(i).Find(iparValue);
      if result <> nil then
        break;
    end;
end;

// TcBag
//   FindValue
//
function TcBag.FindValue(sparType, sparValue: String; iparDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  // Traverse children recursively
  if iparDepth >= 0 then
    for i := 0 to GetCount - 1 do
    begin
      result := Get(i).FindValue(sparType, sparValue, iparDepth);
      if result <> nil then
        break;
    end;
end;

// TcBag
//   FindAll
//
function TcBag.FindAll(value: String): TcBag;
var
  i: longint;
begin
  result := TcBag.Create(nil);
  for i := 0 to GetCount - 1 do
    if AnsiCompareText(value, Objects[i].sName) = 0 then
      result.Add(Objects[i]);
end;

// TcBag
//   Add
//
function TcBag.Add(parObject: TcObject): longint;
begin
  result := kiUNDEFINED;
  if parObject <> nil then
    result := m_lstObjects.Add(parObject);
end;

// TcBag
//   Add
//
function TcBag.Add(parObject: TcBag): longint;
var
  i: longint;
begin
  for i := 0 to parObject.Count - 1 do
    Add(parObject[i]);
  result := Count - 1;
end;

// TcBag
//   IsEmpty
//
function TcBag.GetIsEmpty: boolean;
begin
  result := GetCount = 0;
end;

// TcBag
//   GetTopParent method
//
function TcBag.GetTopParent: TcObject;
begin
  if (m_pParent = self) or (m_pParent = nil) then
    result := self as TcObject // Woops! This may not be all that correct, but oh well..!
  else
    result := m_pParent.GetTopParent;
end;

// TcBag
//   Delete (1) method
//
function TcBag.Delete(item: longint): longint;
begin
  result := Remove(item);
end;

// TcBag
//   Delete (2) method
//
function TcBag.Delete(parObject: TcObject): longint;
begin
  result := Remove(parObject);
end;

// TcBag
//   Remove method
//
function TcBag.Remove(item: longint): longint;
begin
  result := item;
  if (item >= 0) and (item < GetCount) then
    m_lstObjects.delete(item)
  else
    raise exception.create(Format(krsINDEXOUTOFRANGE, [GetCount, item]));
end;

// TcBag
//   Remove method
//
function TcBag.Remove(parObject: TcObject): longint;
begin
  result := IndexOf(parObject);
  if result <> kiUNDEFINED then
    Remove(result)
  else
    raise exception.create(krsPOINTERNOTFOUND);
end;

// TcBag
//   IndexOf method
//
function TcBag.IndexOf(parObject: TcObject): longint;
begin
  result := m_lstObjects.IndexOf(parObject);
end;

// TcBag
//   Insert method
//
function TcBag.Insert(Index: longint; parObject: TcObject): longint;
begin
  m_lstObjects.Insert(Index, parObject);
  result := m_lstObjects.IndexOf(parObject);
end;

// TcBag
//   Move method
//
procedure TcBag.Move(iparFrom, iparTo: integer);
begin
  m_lstObjects.Move(iparFrom, iparTo);
end;

// TcBag
//   SendToObject method
//
function TcBag.SendToObject(value: TObject): TObject;
var
  i: longint;
begin
  result := nil;
  // ptrObject is a TreeView
  if (value <> nil) and (value is TTreeView) and (self is TcObject) then
    result := (value as TTreeView).Items.AddChildObject(nil, system.copy((self as TcObject).sName, 1, 128), self)
  // value is a TreeNode
  else if (value <> nil) and (value is TTreeNode) and (self is TcObject) then
  begin
    result := ((value as TTreeNode).TreeView as TTreeView).Items.AddChildObject(value as TTreeNode, system.copy((self as TcObject).sName, 1, 128), self);
    // Explore children
    if (result <> nil) and (result is TTreeNode) then
      for i := 0 to count - 1 do
        if Objects[i] <> nil then
          Objects[i].SendToObject(result);
  end
  // value is a ListBox
  else if (value <> nil) and (value is TCustomListBox) then
  begin
    (value as TCustomListBox).Items.BeginUpdate;
    (value as TCustomListBox).Items.Clear;
    for i := 0 to count - 1 do
      if Objects[i] <> nil then
        (value as TCustomListBox).Items.AddObject(Objects[i].sName, Objects[i]);
    (value as TCustomListBox).Items.EndUpdate;
  end
  // Explore children
  else if (value <> nil) and (value is TListView) then
  begin
    for i := 0 to count - 1 do
      if Objects[i] <> nil then
        Objects[i].SendToObject(value);
  end
  // value is a ListBox
  else
    for i := 0 to count - 1 do
      if Objects[i] <> nil then
        Objects[i].SendToObject(value);
end;

// TcBag
//   Exchange method
//
procedure TcBag.Exchange(Item1, Item2: longint);
begin
  m_lstObjects.Exchange(Item1, Item2);
end;

// TcBag
//   QuickSort
//
procedure TcBag.QuickSort(iLo, iHi: longint);
var
  Lo, Hi: longint;
  Pivot: TcObject;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := Objects[(Lo + Hi) div 2];
  repeat
    while AnsiCompareText(Objects[Lo].sName, Pivot.sName) < 0 do
      Inc(Lo) ;
    while AnsiCompareText(Objects[Hi].sName, Pivot.sName) > 0 do
      Dec(Hi) ;
    if Lo <= Hi then
    begin
      Exchange(Lo, Hi);
      Inc(Lo) ;
      Dec(Hi) ;
    end;
  until Lo > Hi;
  if Hi > iLo then
    QuickSort(iLo, Hi) ;
  if Lo < iHi then
    QuickSort(Lo, iHi) ;
end;

//
// TcCollection
//

// TcCollection
//   Create
//
constructor TcCollection.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  LocalClear;
end;

// TcCollection
//   Destroy
//
Destructor TcCollection.Destroy;
begin
  LocalClear;
  inherited Destroy;
end;

// TcCollection
//   Clear
//
procedure TcCollection.Clear;
begin
  LocalClear;
end;

// TcCollection
//   LocalClear
//
procedure TcCollection.LocalClear;
begin
  while count <> 0 do
    Delete(0);
  inherited Clear;
end;

// TcCollection
//   Copy
//
procedure TcCollection.Copy(value: TcBag);
var
  i: longint;
  p: TcObject;
begin
  Clear;
  for i := 0 to Value.Count - 1 do
  begin
    p := Value[i].CreateSame(nil);
    p.Copy(Value[i]);
    Add(p);
  end;
end;

// TcCollection
//   Delete (3) method
//
function TcCollection.Delete(parObject: TcObject): longint;
begin
  result := IndexOf(parObject);
  if result <> kiUNDEFINED then
    Delete(result)
  else
    raise exception.create(krsPOINTERNOTFOUND);
end;

// TcCollection
//   Delete (4) method
//
function TcCollection.Delete(item: longint): longint;
begin
  result := item;
  if (item >= 0) and (item < GetCount) then
  begin
    Objects[item].free;
    inherited Delete(item);
  end
  else
    raise exception.create(Format(krsINDEXOUTOFRANGE, [GetCount, item]));
end;

//
// TcObject
//

// TcObject
//   Create
//
constructor TcObject.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_iTag := iObjectTag;
  inc(iObjectTag);
{$IFDEF SBS_DEBUG}
  gMemList.Add(self);
{$ENDIF}
end;

// TcObject
//   Destroy method
//
destructor TcObject.Destroy;
begin
{$IFDEF SBS_DEBUG}
  gMemList.Remove(self);
{$ENDIF}
  inherited Destroy;
end;

// TcObject
//   Clear method
//
procedure TcObject.Clear;
begin
  inherited Clear;
  m_sName := ksEMPTY;
  m_sValue := ksEMPTY;
  m_eType := enUndefined;
end;

// TcObject
//   Copy
//
procedure TcObject.Copy(value: TcObject);
var
  i: longint;
  p: TcObject;
begin
  Clear;
  m_sName := value.m_sName;
  m_sValue := value.m_sValue;
  m_eType := value.m_eType;
  for i := 0 to value.count - 1 do
  begin
    p := value[i].CreateSame(self);
    Add(p);
    p.Copy(value[i]);
  end;
end;

// TcObject
//   Compare method
//
function TcObject.Compare(value: TcObject): boolean;
begin
  result := (m_eType = value.eType) and
            (m_sName = value.sName) and
            (m_sValue = value.sValue);
end;

// TcObject
//   LoadFromStream method
//
function TcObject.LoadFromStream(parStream: TcTokenStream): boolean;
var
  i, L: longint;
  p: TcObject;
begin
  with parStream do
  begin
    MatchMarker(emStart, eomObject);
    m_eType := TeType(AsInteger);
    m_sName := AsString;
    m_sValue := AsString;
    L := AsInteger;
    for i := 0 to L - 1 do
    begin
      p := StreamObject(NextObjectMarker);
      if p = nil then
        raise Exception.Create(Format(krsUNKNOWNOBJECT, [longint(NextObjectMarker)]));
      p.LoadFromStream(parStream);
      Add(p);
    end;
    MatchMarker(emEnd, eomObject);
  end;
  result := TRUE;
end;

// TcObject
//   SaveToStream method
//
procedure TcObject.SaveToStream(parStream: TcTokenStream);
var
  i: longint;
begin
  with parStream do
  begin
    Marker[emStart] := eomObject;
    AsInteger  := longint(m_eType);
    AsString   := m_sName;
    AsString   := m_sValue;
    AsInteger  := Count;
    for i := 0 to count - 1 do
      Get(i).SaveToStream(parStream);
    Marker[emEnd] := eomObject;
  end;
end;

// TcObject
//   CountObjects
//
function TcObject.CountObjects(value: TeTypeSet): longint;
var
  i: longint;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i].eType in value) then
      inc(result);
end;

// TcObject
//   Find (1)
//
function TcObject.Find(sparName: String): TcObject;
begin
  result := Find(sparName, kiLARGEINT);
end;

// TcObject
//   Find (2)
//
function TcObject.Find(sparName: String; iparDepth: longint): TcObject;
begin
  if CompareText(sparName, sName) = 0 then
    result := self
  else
    result := inherited Find(sparName, iparDepth - 1);
end;

// TcObject
//   Find (3)
//
function TcObject.Find(iparValue: longint): TcObject;
begin
  if iparValue = m_iTag then
    result := self
  else
    result := inherited Find(iparValue, kiLARGEINT);
end;

// TcObject
//   Find (4)
//
function TcObject.Find(iparValue: longint; iparDepth: longint): TcObject;
begin
  if iparValue = m_iTag then
    result := self
  else
    result := inherited Find(iparValue, iparDepth);
end;

// TcObject
//   FindValue
//
function TcObject.FindValue(sparType, sparValue: String; iparDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to count - 1 do
  begin
    result := objects[i].FindValue(sparType, sparValue, iparDepth);
    if result <> nil then
      break;
  end;
end;

// TcObject
//   Ancestor
//
function TcObject.Ancestor(parType: TeType): TcObject;
begin
  result := nil;
  if eType = parType then
    result := self
  else if (Parent <> nil) and (Parent <> self) then
    result := Parent.Ancestor(parType);
end;

// TcObject
//   GetXML
//
function TcObject.GetXML: String;
begin
  result := ksEMPTY;
end;

// TcObject
//   SetXML
//
procedure TcObject.SetXML(value: String);
begin
  //
end;

// TcObject
//   CreateSame
//
class function TcObject.CreateSame(parParent: TcObject): TcObject;
begin
  result := Create(parParent);
end;

// TcObject
//   Has method
//
function TcObject.Has(value: TcObject): boolean;
begin
  result := self = value;
  if (not result) and (value <> nil) and (value.parent <> nil) then
    result := Has(value.Parent);
end;

// TcObject
//   SetValue
//
procedure TcObject.SetValue(value: String);
begin
  m_sValue := Value;
end;

// TcObject
//   SetValue
//
function TcObject.GetValue: String;
begin
  result := m_sValue;
end;

// TcObject
//   GetDepth method
//
function TcObject.GetDepth: longint;
begin
  result := 0;
  if (parent <> nil) and (parent <> self) then
    result := parent.GetDepth + 1;
end;

// TcObject
//   Insert method
//
function TcObject.Insert(Index: longint; parObject: TcObject): longint;
begin
  result := inherited Insert(Index, parObject);
  parObject.Parent := Self;
end;

// TcObject
//   ParentIndex method
//
function TcObject.ParentIndex: longint;
begin
  result := kiUNDEFINED;
  if (parent <> nil) and (parent is TcObject) then
    result := Parent.IndexOf(self);
end;

// TcObject
//   Exchange Method
//
procedure TcObject.Exchange(parIndex1, parIndex2: longint);
begin
  m_lstObjects.Exchange(parIndex1, parIndex2);
end;

// TcObject
//   SetTag Method
//
procedure TcObject.SetTag(value: longint);
begin
  m_iTag := value;
end;

// TcObject
//   SetTag Method
//
function TcObject.GetTag: longint;
begin
  result := m_iTag;
end;

// TcObject
//   Child Method (1)
//
function TcObject.Child(value: String): TcObject;
begin
  result := Find(value);
end;

// TcObject
//   Child Method (2)
//
function TcObject.Child(parObject: TcObject): TcObject;
begin
  result := Find(parObject.sName);
end;

// TcObject
//   Find Method
//
function TcObject.Find(Value: TcObject): TcObject;
begin
  result := nil;
end;

// TcObject
//   SetName Method
//
procedure TcObject.SetName(value: String);
begin
  m_sName := value;
end;

// TcObject
//   GetName Method
//
function TcObject.GetName: String;
begin
  result := m_sName;
end;

// TcObject
//   StreamObject Method
//
function TcObject.StreamObject(value: TeObjectMarker): TcObject;
begin
  result := TcObject.Create(self);
end;

// TcObject
//   Find Method
//
function TcObject.Find(parName, parValue: String; parDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  if (AnsiCompareText(m_sName, trim(parName)) = 0) and (AnsiCompareText(m_sValue, trim(parValue)) = 0) then
    result := self
  else if parDepth > 0 then
    for i := 0 to count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcObject) then
      begin
        result := Objects[i].Find(parName, parValue, parDepth - 1);
        if result <> nil then
          break;
      end;
end;

// TcObject
//   ClearChildren Method
//
procedure TcObject.ClearChildren;
begin
  while count > 0 do
    delete(count - 1);
end;

// TcObject
//   XML_Filtered Method
//
function TcObject.XML_Filtered(parFilter: TcBag; parReplacements: TStringList): String;
begin
  result := GetXML;
end;

// TcObject
//   FindFirst Method
//
function TcObject.FindFirst(value: TeType): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i].eType = value) then
    begin
      result := Objects[i];
      break;
    end;
end;

// TcObject
//   FindFirstValue Method
//
function TcObject.FindFirstValue(value: TeType): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  p := FindFirst(value);
  if (p <> nil) then
    result := p.sValue;
end;

// TcObject
//   Remove Method
//
function TcObject.Remove: boolean;
begin
  result := ParentIndex <> kiUNDEFINED;
  if result then
    Parent.Remove(self);
end;

// TcObject
//   SetError Method
//
procedure TcObject.SetError(value: String);
begin
  if (Parent <> nil) and (Parent is TcObject) then
    Parent.SetError(value)
  else
    ; // Woops
end;

//
//   Initialization Section
//
initialization
  iObjectTag := 0;
{$IFDEF SBS_DEBUG}
  gMemList := TcBag.Create(nil);
{$ENDIF}
finalization
{$IFDEF SBS_DEBUG}
  gMemList.free;
{$ENDIF}
end.
