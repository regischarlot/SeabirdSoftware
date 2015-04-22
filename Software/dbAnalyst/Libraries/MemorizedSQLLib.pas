unit MemorizedSQLLib;

interface

uses
  daObjectLib,     // TcObject
  ActiveX,         // IStorage
  ComCtrls,        // TTreeNode
  Forms;           // TForm

type
  TeMemItemType = (emiUndefined, emiStorage, emiStream);
  TeCategory = (ecToday, ecYesterday, ecLast7Days, ecLast14Days, esLast60Days, esHistorical);
  TeMemorizeResult = (mrDisplayCombo, mrDisplayList, mrDisplayItem);
  TeMemorizeResultSet = set of TeMemorizeResult;

  //
  // TcMemorizedSQLItem
  //
  TcMemorizedSQLItem = class(TcObject)
  private
    // Private members
    //
    m_bIsLoaded: boolean;
    m_bIsModified: boolean;
    m_datCreated: TDateTime;
    m_emType: TeMemItemType;
    m_objDisplay: TObject;

  private
    // Private declarations
    //
    function    GetHeader: String; overload;
    function    GetHeader(eCategory: TeCategory): String; overload;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    function    Load(value: IStorage; parMaxDays: longint): boolean; virtual;
    function    Save: boolean; virtual;
    //
    // 2. Custom
    function    SendToObject(value: TObject): TObject; override;
    function    Memorize(sparSection, sparSQL: String): TeMemorizeResultSet;

  public
    // public properties
    //
    property    datCreated: TDateTime           read m_datCreated               write m_datCreated;
    property    IsLoaded: boolean               read m_bIsLoaded                write m_bIsLoaded;
    property    Display: TObject                read m_objDisplay               write m_objDisplay;
    property    Header: String                  read GetHeader;
  end;

  //
  // TcMemorizedSQL
  //
  TcMemorizedSQL = class(TcMemorizedSQLItem)
  private
    // Private members
    //
    m_objItem: TcObject;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    function    Load(value: IStorage; parMaxDays: longint): boolean; override;
    //   Save
    //
    // 2. Custom
    function    GetIndex(value: TcObject): longint;

  public
    // public properties
    //
    Property    Item: TcObject                  read m_objItem                  write m_objItem;
  end;

implementation

uses
  daStreamLib,       // TcDataStream
  daResourceStrings,
  daGlobals,
  sysUtils,
  strUtils,
  frmMemorizedSQL, // TTfrmMemorizedSQL
  stdCtrls,        // TComboBox
  FormLib;         // TcFormSet

const
  krsFILE_MEMORIZEDSTMT = 'MemorizeStatements.dat';
  kasCATEGORY: array[TeCategory] of string =
    ('Today', 'Yesterday', 'Last 7 Days', 'Last 14 Days', 'Last 60 Days', 'Historical');
  kaiDAYS: array[TeCategory] of integer =
    (1, 2, 7, 14, 60, 1098);

//
// TcMemorizedSQL
//

// TcMemorizedSQL
//   Create
//
constructor TcMemorizedSQL.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objItem := nil;
end;

// TcMemorizedSQL
//   Load
//
function TcMemorizedSQL.Load(value: IStorage; parMaxDays: longint): boolean;
var
  s: IStorage;
  f: String;
begin
  result := FALSE;
  s := nil;
  try
    f := GetFilePath(Application.ExeName) + krsFILE_MEMORIZEDSTMT;
    if not SUCCEEDED(StgOpenStorage(PWideChar(WideString(f)), nil, STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, nil, 0, s)) then
      StgCreateDocFile(PWideChar(WideString(f)), STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, 0, s);
    if s <> nil then
      result := inherited Load(s, kaiDAYS[high(TeCategory)]);
  finally
    s := nil;
  end;
end;

// TcMemorizedSQL
//   GetIndex
//
function TcMemorizedSQL.GetIndex(value: TcObject): longint;
var
  p: TcObject;
begin
  result := kiUNDEFINED;
  if value <> nil then
  begin
    p := Find(value.sName, 1);
    if (p <> nil) and (p is TcMemorizedSQLItem) then
      result := p.ParentIndex;
  end;
end;

//
// TcMemorizedSQLItem
//

// TcMemorizedSQLItem
//   Create
//
constructor TcMemorizedSQLItem.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_bIsLoaded := FALSE;
  m_bIsModified := FALSE;
  m_emType := emiUndefined;
end;

// TcMemorizedSQLItem
//   Load
//
function TcMemorizedSQLItem.Load(value: IStorage; parMaxDays: longint): boolean;

  function SubLoad(value: IStorage; parItem: TcMemorizedSQLItem): boolean;
  var
    Enum: IEnumSTATSTG;
    StatStg: TStatStg;
    NumFetched: integer;
    p: TcMemorizedSQLItem;
    s: IStorage;
    strm: IStream;
    data: TcDataStream;
  begin
    if SUCCEEDED(value.EnumElements(0, nil, 0, Enum)) then
      while Enum.Next(1, StatStg, @NumFetched) = S_OK do
        case StatStg.dwType of
          STGTY_STORAGE:
          begin
            p := TcMemorizedSQLItem.Create(parItem);
            parItem.Add(p);
            p.sName := string(StatStg.pwcsName);
            p.m_emType := emiStorage;
            // Loop
            s := nil;
            try
              if SUCCEEDED(value.OpenStorage(PWideChar(WideString(p.sName)), nil, STGM_READWRITE or STGM_SHARE_EXCLUSIVE, nil, 0, s)) then
                SubLoad(s, p);
            finally
              s := nil;
            end;
          end;
          STGTY_STREAM:
          begin
            p := TcMemorizedSQLItem.Create(parItem);
            p.sName := string(StatStg.pwcsName);
            p.m_emType := emiStream;
            if SUCCEEDED(value.OpenStream(PWideChar(StatStg.pwcsName), nil, STGM_READ or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, strm)) then
            try
              data := nil;
              try
                data := TcDataStream.Create(strm);
                p.sName := string(data.AsAnsiString);
                p.sValue := string(data.AsAnsiString);
                p.m_datCreated := data.AsDate;
              finally
                data.Free;
              end;
            finally
              strm := nil;
            end;
            // Must be within bound of last to see
            if trunc(now) - trunc(p.m_datCreated) < parMaxDays then
              parItem.Add(p)
            else
              p.Free;
          end;
        end;
    result := TRUE;
  end;

begin
  result := SubLoad(value, self);
end;

// TcMemorizedSQLItem
//   Save
//
function TcMemorizedSQLItem.Save: boolean;

  function GetParentByLevel(value: TcMemorizedSQLItem; level: longint): TcMemorizedSQLItem;
  begin
    result := nil;
    if level = 0 then
      result := value
    else if (value.parent <> nil) and (value.Parent is TcMemorizedSQLItem) then
      result := GetParentByLevel(value.Parent as TcMemorizedSQLItem, level - 1);
  end;

var
  s, s1: IStorage;
  strm: IStream;
  data: TcDataStream;
  p1: TcMemorizedSQLItem;
  f: String;
begin
  s := nil;
  try
    f := GetFilePath(Application.ExeName) + krsFILE_MEMORIZEDSTMT;
    if not SUCCEEDED(StgOpenStorage(PWideChar(WideString(f)), nil, STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, nil, 0, s)) then
      StgCreateDocFile(PWideChar(WideString(f)), STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, 0, s);
    if s <> nil then
    begin
      p1 := GetParentByLevel(self, 1);
      if not SUCCEEDED(s.OpenStorage(PWideChar(WideString(p1.sName)), nil, STGM_READWRITE or STGM_SHARE_EXCLUSIVE, nil, 0, s1)) then
        s.CreateStorage(PWideChar(WideString(p1.sName)), STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, 0, 0, s1);
      if s1 <> nil then
      begin
        if SUCCEEDED(s1.CreateStream(PWideChar(WideString(sName)), STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, strm)) then
        try
          data := nil;
          try
            data := TcDataStream.Create(strm);
            data.AsAnsiString := AnsiString(sName);
            data.AsAnsiString := AnsiString(svalue);
            data.AsDate := m_datCreated;
          finally
            data.Free;
          end;
        finally
          strm := nil;
        end;
      end;
      s1 := nil;
    end;
    result := TRUE;
  finally
    s := nil;
  end;
end;

// TcMemorizedSQLItem
//   SendToObject
//
function TcMemorizedSQLItem.SendToObject(value: TObject): TObject;

  function FindCategory(parItem: TcMemorizedSQLItem; var Value: TeCategory): boolean;
  var
    e: TeCategory;
  begin
    result := FALSE;
    for e := low(TeCategory) to high(TeCategory) do
      if trunc(now) - trunc(parItem.m_datCreated) + 1 <= kaiDAYS[e] then
      begin
        result := TRUE;
        value := e;
        break;
      end;
  end;

var
  tv: TTreeView;
  i: longint;
  e: TeCategory;
  p, q: TcMemorizedSQLItem;
  cb: TComboBox;
begin
  result := nil;
  //
  // ComboBox
  if (value <> nil) and (value is TComboBox) then
  begin
    cb := value as TComboBox;
    cb.Items.BeginUpdate;
    cb.Items.Clear;
    for i := 0 to count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcMemorizedSQLItem) then
        cb.Items.AddObject(Objects[i].sName, Objects[i]);
    cb.Items.EndUpdate;
    m_objDisplay := value;
    result := value;
  end
  //
  // TreeView
  else if (value <> nil) and (value is TTreeView) then
  begin
    tv := value as TTreeView;
    tv.Items.BeginUpdate;
    if (Depth = 2) and (TopParent is TcMemorizedSQL) and (TopParent is TcMemorizedSQL) and ((TopParent as TcMemorizedSQL).Item = parent) then // Single item
      try
        if FindCategory(self, e) then
          m_objDisplay := tv.Items.AddChildObjectFirst(tv.Items[longint(e)], Header, self);
      except
          //
      end
    else
    begin
      q := nil;
      if Depth = 2 then
        q := Parent as TcMemorizedSQLItem
      else if Depth = 1 then
        q := self;
      if q <> nil then
      try
        tv.Items.Clear;
        for e := low(TeCategory) to high(TeCategory) do
          tv.Items.AddChildObject(nil, kasCATEGORY[e], nil);
        for i := 0 to q.count - 1 do
          if (q[i] <> nil) and (q[i] is TcMemorizedSQLItem) then
          begin
            p := q[i] as TcMemorizedSQLItem;
            if FindCategory(p, e) then
              p.m_objDisplay := tv.Items.AddChildObjectFirst(tv.Items[longint(e)], p.GetHeader(e), p);
          end;
        m_objDisplay := value;
        if (q.parent <> nil) and (q.parent is TcMemorizedSQL) then
          (q.parent as TcMemorizedSQL).Item := self;
      except
        //
      end;
      // Make sure combo is set correctly
      if Depth = 2 then
      begin
        p := TopParent as TcMemorizedSQLItem;
        if (p <> nil) and (p is TcMemorizedSQL) then
        begin
          if ((p as TcMemorizedSQL).m_objDisplay <> nil) and ((p as TcMemorizedSQL).m_objDisplay is TComboBox) then
            ((p as TcMemorizedSQL).m_objDisplay as TComboBox).ItemIndex := Parent.ParentIndex;
        end;
      end;
    end;
    tv.Items.EndUpdate;
    result := value;
  end;
end;

// TcMemorizedSQLItem
//   Memorize method
//
function TcMemorizedSQLItem.Memorize(sparSection, sparSQL: String): TeMemorizeResultSet;
var
  p1, p2: TcMemorizedSQLItem;
  s: String;
begin
  result := [];
  // Level 0: Section
  p1 := Find(sparSection, 1) as TcMemorizedSQLItem;
  if p1 = nil then
  begin
    p1 := TcMemorizedSQLItem.Create(self);
    Add(p1);
    p1.sName := sparSection;
    p1.m_emType := emiStorage;
    result := result + [mrDisplayCombo, mrDisplayList];
  end;
  // Level 1: Entry
  s := FormatDateTime('yyyymmddhhnnsszzz', now);
  p2 := TcMemorizedSQLItem.Create(p1);
  p1.add(p2);
  p2.sName := s;
  p2.m_emType := emiStream;
  p2.sValue := sparSQL;
  p2.m_datCreated := now;
  // Save
  p2.save;
  // Display
  if [mrDisplayList] * result = [] then
    p2.SendToObject(p1.m_objDisplay);
end;

// TcMemorizedSQLItem
//   GetHeader method
//
function TcMemorizedSQLItem.GetHeader: String;
begin
  result := GetHeader(esLast60Days);
end;

// TcMemorizedSQLItem
//   GetHeader method
//
function TcMemorizedSQLItem.GetHeader(eCategory: TeCategory): String;
var
  t, d: String;
const
  kasDAYS: array[1 .. 7] of string =
    ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
begin
  t := AnsiReplaceText(lowercase(FormatDateTime('t', m_datCreated)), ' ', ksEMPTY);
  d := AnsiReplaceText(lowercase(FormatDateTime('ddddd', m_datCreated)), ' ', ksEMPTY);
  case eCategory of
    ecToday, ecYesterday:
      result := system.copy(Format('%s: %s', [t, sValue]), 1, 127);
    ecLast7Days, ecLast14Days, esLast60Days, esHistorical:
      result := system.copy(Format('%s, %s %s: %s', [kasDAYS[DayOfWeek(m_datCreated)], d, t, sValue]), 1, 127); 
  end;
end;

end.

