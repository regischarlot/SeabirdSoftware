unit FormLib;

interface

uses
  Forms,
  Types,
  Menus,
  daGlobals,
  ExtCtrls,
  Controls,
  ConnectionLib,   // TcConnection
  Classes,         // TComponent
  daObjectLib,     // TcObject
  DataLib,         // TcMetaData, TcData
  FavoriteLib,     // TcFavorite
  PreferenceLib,   // TcPreferenceList
  StatementLib,    // TcStatementList
  SynEdit,         // TSynEdit
  ComCtrls,
  MemorizedSQLLib, // TcMemorizedSQL
  CursorStackLib,
  SynHighlighterdbAnalystSQL;

type
  //
  // TcForm
  //
  TcFormSet = class;

  TcForm = class(TForm)
  private
    // Private members
    //
    m_FormSet: TcFormSet;
    m_eForm: TeForm;
    m_rectToolbar: TRect;
    m_eDisplayType: TeQueryType;
    m_frmOwner: TForm;
    m_eFormState: TeFormState;
    m_lstCursorStack: TcCursorStack;

  private
    // Private methods
    //
    function    GetMetaData: TcMetaData;
    function    GetHasFeature(index: TeForm): boolean;
    function    GetControlBar: TControlBar;
    function    GetIconImageList: TImageList;
    function    GetAlertImageList: TImageList;
    function    GetToolbarLoc(parToolbar: TToolbar): TRect;

  protected
    // protected methods
    //
    procedure   SetFormSet(value: TcFormSet); virtual;
    procedure   GetToolbarLocation(parToolbar: TToolbar; parWidth: longint);
    procedure   SetToolbarLocation(parControlBar: TControlBar; parToolbar: TToolbar);
    procedure   SetDisplayType(value: TeQueryType); virtual;
    function    CheckConnection(bparConnect: boolean): boolean; virtual;
    function    GetLocalToolbar: TToolbar; virtual; abstract;
    function    ReConnect: boolean; virtual; abstract;

  public
    // public methods
    //
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean; virtual;
    function    Finalize: boolean; virtual; abstract;
    function    ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean; overload; virtual; abstract;
    function    ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean; overload; virtual; abstract;
    function    ExecuteItem(value: string; var sError: string): boolean; overload; virtual;
    procedure   onPreferenceChange(Sender: TObject); virtual;
    function    Header: String;
    procedure   FormActivate(Sender: TObject); virtual;
    procedure   FormDeactivate(Sender: TObject); virtual;
    procedure   StatusText(eparPanelSet: TePanelSet; value: String; eIcon: TeStatusIcon);
    procedure   SetMainMenu(eMenu: TeMenu; bEnabled: boolean; onEvent: TNotifyEvent); overload; virtual;
    procedure   SetMainMenu(eMenu: TeMenu; bEnabled, bVisible: boolean; onEvent: TNotifyEvent); overload; virtual;
    procedure   SetProgressBar(bparVisible: boolean; iValue, iMax: longint);
    procedure   SetLRUMenu(value: TNotifyEvent);
    function    Navigate(value: string): boolean; virtual;

  public
    // public properties
    //
    property    MetaData: TcMetaData                    read GetMetaData;
    property    HasFeature[index: TeForm]: boolean      read GetHasFeature;
    property    FormSet: TcFormSet                      read m_FormSet          write SetFormSet;
    property    eForm: TeForm                           read m_eForm            write m_eForm;
    property    eDisplayType: TeQueryType               read m_eDisplayType     write SetDisplayType;
    property    frmOwner: TForm                         read m_frmOwner         write m_frmOwner;
    property    ControlBar: TControlBar                 read GetControlBar;
    property    imgIcons: TImageList                    read GetIconImageList;
    property    imgAlerts: TImageList                   read GetAlertImageList;
    property    eFormState: TeFormState                 read m_eFormState       write m_eFormState;
    property    LocalToolbar: TToolbar                  read GetLocalToolbar;
    property    CursorStack: TcCursorStack              read m_lstCursorStack;
  end;

  //
  // TcFormList
  //
  TcFormList = class(TObject)
  private
    // Private members
    //
    m_eForm: TeForm;
    m_lstForms: TList;

  private
    // Private declarations
    //
    function    Get(item: longint): TcForm;
    procedure   Put(item: longint; Value: TcForm);
    function    GetCount: longint;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); virtual;
    destructor  Destroy; override;
    procedure   Clear;
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Add(value: TcForm): longint;
    function    Delete(value: TcForm): boolean; overload;
    function    Delete(value: longint): boolean; overload;

  public
    // Public Properties
    //
    property    eForm: TeForm                           read m_eForm            write m_eForm;
    property    Forms[item: longint]: TcForm            read Get                write Put; default;
    property    Count: longint                          read GetCount;
  end;

  //
  // TcFormSet
  //
  TcFormSet = class(TcObject)
  private
    // Private members
    //
    m_lstForm: array[TeForm] of TcFormList;
    m_objConnection: TcConnection;
    m_objMetaData: TcMetaData;
    m_objSchema: TcSchema;
    m_sXMLFileName: String;
    m_objDataAncestor: TcObject;
    m_eForm: TeForm;
    m_lstLOG: TcStatementList;
    m_objDataFind: TcObject;
    m_frmLog: TForm;
    m_frmMemory: TForm;
    m_objFavorite: TcFavorite;
    m_sConnectionCaption: String;
    // Remembered from previous calls
    m_AOwner: TComponent;
    m_sParameter: String;
    m_objImageList: TImageList;
    m_objPreferences: TcPreferenceList;
    m_objHighlighter: TSyndbAnalystSQLSyn;
    m_objHeaderImageList: TImageList;
    m_objAlertImageList: TImageList;
    // Code Completion
    m_bCodeCompletion: boolean;
    m_semBusy: longint;

  private
    // Private declarations
    //
    function    GetFormList(index: TeForm): TcFormList;
    function    GetHasFeature(index: TeForm): boolean;
    procedure   onCreateForm(sender: TObject); overload; virtual;
    procedure   onCreateForm(value: TeForm); overload; virtual;
    function    GetForm(Index: TeForm): TcForm;
    function    CreateAMenu(parMenu: TMenuItem; eForm: TeForm; cShortcut: char; iImageIndex: longint; sCaption: String; Handler: TNotifyEvent; bHasChildren, bInsertFirst, bEnabled: boolean; Tag: longint): TMenuItem;
    procedure   CleanupMenus(parMenu: TMenuItem);
    procedure   onToolsClick(Sender: TObject);
    procedure   SetFileName(value: String);
    procedure   SetLogDisplay(value: longint);
    procedure   onFavoriteQuery(Sender: TObject);
    function    GetSchema: TcSchema;
    function    GetSupport(value: TeFeatureSupport): boolean;
    function    GetMetaData: TcMetaData;
    function    GetRDBMS: String;
    function    SetInFormCreation(value: boolean): longint;
    function    IsInFormCreation: boolean;

  published
    // Published declarations
    //
    function    GetIsEmpty: boolean; override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;
    destructor  Destroy; override;
    procedure   Clear; override;
    //   Copy
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom
    function    CreateForm(eparForm: TeForm; AOwner: TComponent; eparMode: TeFormInitialization; eparParameter: String): boolean;
    function    CloseForm: boolean;
    function    CheckConnection(bparConnect: boolean): boolean;
    procedure   FormClose(Sender: TObject; var Action: TCloseAction);
    function    ExecuteSQL(eparForm: TeForm; eparValue: String; eparDisplayType: TeQueryType; eparOptionSet: TeQueryOptions): TcForm;
    procedure   onQuery(sender: TObject);
    procedure   InitializeToolMenu(parMenu: TMenuItem);
    procedure   onSwitchForm(sender: TObject); overload; virtual;
    procedure   onSwitchForm(value: TeForm); overload; virtual;
    procedure   SetHighlighter(value: TSynEdit);
    procedure   onPreferenceChange(Sender: TObject);
    procedure   SetHilights;
    function    SetLog(sSQLName, value: String; duration: TDateTime; eFlags: TeLogFlagSet): longint;
    function    SetHeaderImage(parImage: TImage; parIndex: longint): boolean;
    function    ApplySessionParameters(value: TcConnection): boolean;
    function    OnDataDump(value: TcObject): boolean; overload;
    function    OnDataDump(value: String): boolean; overload;
    function    Memorize(sSQL: String): TeMemorizeResultSet;
    procedure   SetLRUMenu(parMenu: TMenuItem; parHandle: TNotifyEvent);
    function    AddLRU(parFileName: string): boolean;
    function    Allowed(value: TeForm): boolean;
    function    GetOnlineHelp(value: string): boolean;
    function    HasMasterOnlineHelp: boolean;
    function    ComputeCaption(value: TStringList): boolean;
    function    Reconnect: boolean;

  public
    // Public Properties
    //
    property    FormList[index: TeForm]: TcFormList       read GetFormList; default;
    property    eForm: TeForm                             read m_eForm                  write m_eForm;
    property    Connection: TcConnection                  read m_objConnection          write m_objConnection;
    property    IsEmpty: boolean                          read GetIsEmpty;
    property    MetaData: TcMetaData                      read GetMetaData;
    property    HasFeature[index: TeForm]: boolean        read GetHasFeature;
    property    FileName: String                          read m_sXMLFileName           write SetFileName;
    property    objDataAncestor: TcObject                 read m_objDataAncestor        write m_objDataAncestor;
    property    Preferences: TcPreferenceList             read m_objPreferences         write m_objPreferences;
    property    frmLog: TForm                             read m_frmLog                 write m_frmLog;
    property    frmMemory: TForm                          read m_frmMemory              write m_frmMemory;
    property    lstLOG: TcStatementList                   read m_lstLOG                 write m_lstLOG;
    property    objDataFind: TcObject                     read m_objDataFind            write m_objDataFind;
    property    Schema: TcSchema                          read GetSchema;
    property    Favorites: TcFavorite                     read m_objFavorite;
    property    Highlighter: TSyndbAnalystSQLSyn          read m_objHighlighter;
    property    Support[value: TeFeatureSupport]: boolean read GetSupport;
    property    ImageList: TImageList                     read m_objImageList           write m_objImageList;
    property    HeaderImageList: TImageList               read m_objHeaderImageList     write m_objHeaderImageList;
    property    AlertImageList: TImageList                read m_objAlertImageList      write m_objAlertImageList;
    property    bCodeCompletion: boolean                  read m_bCodeCompletion        write m_bCodeCompletion;
  end;

  //
  // TcFormSetList
  //
  TcFormSetList = class(TcObject)
  private
    // Private members
    //
    m_frmLog: TForm;
    m_frmMemory: TForm;
    m_frmFind: TForm;
    m_hdlFormFocus: TNotifyEvent;
    m_objMemorizedSQL: TcMemorizedSQL;
    m_lstLRU: TcObject;

  private
    // Private methods
    //
    function    LoadLRU: boolean;
    function    SaveLRU: boolean;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Add(parObject: TcObject): longint; override;
    function    Delete(parObject: TcObject): longint; override;
    function    Delete(item: longint): longint; override;
    procedure   SetFormFocus(Sender: TObject);
    procedure   onPreferenceChange(Sender: TObject);
    function    Memorize(sparSection, sparSQL: String): TeMemorizeResultSet;
    function    AddLRU(parRDBMS, parFileName: string): boolean;
    procedure   SetLRUMenu(parRDBMS: String; parMenu: TMenuItem; parHandle: TNotifyEvent);

  public
    // Public Properties
    //
    property    hdlFormFocus: TNotifyEvent              read m_hdlFormFocus     write m_hdlFormFocus;
    property    frmLog: TForm                           read m_frmLog           write m_frmLog;
    property    frmMemory: TForm                        read m_frmMemory        write m_frmMemory;
    property    frmFind: TForm                          read m_frmFind          write m_frmFind;
    property    objMemorizedSQL: TcMemorizedSQL         read m_objMemorizedSQL  write m_objMemorizedSQL;
  end;

  //
  // Tools
  //
  procedure SetFormType(obj: TComponent; objMetaData: TcMetaData; value: TeForm; Handler: TNotifyEvent);
  function  StringToSyntaxHiliteType(value: String): TeSyntaxHiliteType;

implementation

uses
  Windows,
  Math,
  ADODB_TLB,
  ComObj,
  Registry,
  Variants,
  ExecuteLib,        // TcExecute
  strUtils,
  Oracle,
  daResourceStrings,
  sysUtils,
  stdCtrls,
  frmData2,          // TfrmData2
  frmQuery,          // TfrmQuery
  frmGrid,           // TfrmGrid
  frmOnlineHelp,     // TfrmOnlineHelp
  frmDataDump,       // TTfrmDataDump
  frmSQLLog,         // TfrmSQLLog
  frmDataFind,       // TfrmFind
  frmSessionManager, // TfrmSessionManager
  frmMemorizedSQL,   // TfrmMemorizedSQL
  Graphics,          // TColor
  Main,              // TfrmMain
  frmLogParser;      // TfrmLogParser

//
// TcForm
//

// TcForm
//   Create
//
constructor TcForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  m_lstCursorStack := TcCursorStack.Create(nil);
  m_eFormState := efmCreate;
  m_FormSet := nil;
  m_eDisplayType := eqtText;
  m_frmOwner := AOwner as TForm;
end;

// TcForm
//   Destroy
//
destructor TcForm.Destroy;
begin
  FreeAndNil(m_lstCursorStack);
  inherited;
end;

// TcForm
//   Initialize
//
function TcForm.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;
begin
  m_eFormState := efmInitialize;
  StatusText([psPanel1], ksEMPTY, esiNone);
  result := TRUE;
end;

// TcForm
//   GetMetaData
//
function TcForm.GetMetaData: TcMetaData;
begin
  result := nil;
  if m_FormSet <> nil then
    result := m_FormSet.MetaData;
end;

// TcForm
//   Header method
//
function TcForm.Header: String;
begin
  result := ksEMPTY;
  if m_FormSet <> nil then
    result := m_FormSet.Text;
end;

// TcForm
//   GetHasFeature method
//
function TcForm.GetHasFeature(index: TeForm): boolean;
begin
  result := FALSE;
  if Formset <> nil then
    result := Formset.HasFeature[index];
end;

// TcForm
//   SetFormSet
//
procedure TcForm.SetFormSet(value: TcFormSet);
begin
  m_FormSet := value;
end;

// TcForm
//   GetToolbarLoc
//
function TcForm.GetToolbarLoc(parToolbar: TToolbar): TRect;
var
  p: TWinControl;
begin
  result.Left := 0;
  result.Right := 0;
  result.Top := 0;
  result.Bottom := 0;
  if (parToolbar.Parent <> nil) and (parToolbar.Parent is TControlBar) then
  begin
    p := parToolbar.Parent as TWinControl;
    if (p.ControlCount > 0) and (p.Controls[0] <> nil) then
    begin
      result.Left := (p.Controls[0] as TToolbar).Left;
      result.Right := result.Left + (p.Controls[0] as TToolbar).Width;
      result.Top := (p.Controls[0] as TToolbar).Top;
      result.Bottom := result.Top + (p.Controls[0] as TToolbar).Height;
    end;
  end;
end;

// TcForm
//   GetToolbarLocation
//
procedure TcForm.GetToolbarLocation(parToolbar: TToolbar; parWidth: longint);
var
  p: TWinControl;
  i: longint;
  r: TRect;
begin
  // Hide toolbars not related
  if (parToolbar.Parent <> nil) and (parToolbar.Parent is TControlBar) then
  begin
    p := parToolbar.Parent as TWinControl;
    for i := 0 to p.ControlCount - 1 do
      if (p.Controls[i] <> nil) and (p.Controls[i] is TToolbar) and (p.Controls[i].Tag = 0) and (p.Controls[i] <> parToolbar) then
        (p.Controls[i] as TToolbar).Visible := FALSE;
  end;
  r := GetToolbarLoc(parToolbar);
  // Snap current toolbar
  parToolbar.Left := r.Right; // m_rectToolbar.Left;
  parToolbar.Top := r.Top; // m_rectToolbar.Top;
  //parToolbar.Width := parWidth;
  parToolbar.Visible := TRUE;
  parToolbar.AutoSize := TRUE;
  parToolbar.Height := 25;
  parToolbar.ButtonHeight := 25;
  parToolbar.ButtonWidth := 24;
end;

// TcForm
//   SetToolbarLocation
//
procedure TcForm.SetToolbarLocation(parControlBar: TControlBar; parToolbar: TToolbar);
begin
  //m_rectToolbar := Rect(0, 0, 0, 0);
  if (parControlBar <> nil) then
  begin
    parToolbar.Parent := parControlBar;
    parToolbar.Top := 0;
    parToolbar.Left := frmMain.ToolBar1.Width + 1;
  end;
  //m_rectToolbar.left := parToolbar.Left;
  //m_rectToolbar.top :=  parToolbar.top;
  m_rectToolbar := GetToolbarLoc(parToolbar);
end;

// TcForm
//   onPreferenceChange
//
procedure TcForm.onPreferenceChange(Sender: TObject);
begin
  //
end;

// TcForm
//   SetDisplayType
//
procedure TcForm.SetDisplayType(value: TeQueryType);
begin
  m_eDisplayType := value;
end;

// TcForm
//   FormActivate
//
procedure TcForm.FormActivate(Sender: TObject);
var
  f: TForm;
begin
  if (m_FormSet <> nil) and (m_FormSet.Parent <> nil) and (m_FormSet.Parent is TcFormSetList) then
  begin
    // Find Form
    f := (m_FormSet.Parent as TcFormSetList).frmFind;
    if (f <> nil) and (f is TfrmDataFind) then
    begin
      if self is TfrmData2 then
        (f as TfrmDataFind).objDataFind := m_FormSet.objDataFind as TcDataFind
      else
        (f as TfrmDataFind).objDataFind := nil;
    end;
    // Set
    (m_FormSet.Parent as TcFormSetList).SetFormFocus(self);
  end;
  // Toolbar
  if GetLocalToolbar <> nil then
    GetToolbarLocation(GetLocalToolbar, GetLocalToolbar.Width);
end;

// TcForm
//   FormDeactivate
//
procedure TcForm.FormDeactivate(Sender: TObject);
begin
  // Toolbar
  if GetLocalToolbar <> nil then
    GetLocalToolbar.visible := FALSE;
end;

// TcForm
//   StatusText
//
procedure TcForm.StatusText(eparPanelSet: TePanelSet; value: String; eIcon: TeStatusIcon);
begin
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    (m_frmOwner as TfrmMain).StatusText(eparPanelSet, value, eIcon);
end;

// TcForm
//   SetMainMenu
//
procedure TcForm.SetMainMenu(eMenu: TeMenu; bEnabled: boolean; onEvent: TNotifyEvent);
begin
  SetMainMenu(eMenu, bEnabled, TRUE, onEvent);
end;

// TcForm
//   SetMainMenu
//
procedure TcForm.SetMainMenu(eMenu: TeMenu; bEnabled, bVisible: boolean; onEvent: TNotifyEvent);
begin
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    (m_frmOwner as TfrmMain).SetMainMenu(eMenu, bEnabled, bVisible, onEvent);
end;

// TcForm
//   SetLRUMenu
//
procedure TcForm.SetLRUMenu(value: TNotifyEvent);
begin
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    (m_frmOwner as TfrmMain).SetLRUMenu(m_FormSet, value);
end;

// TcForm
//   GetControlBar
//
function TcForm.GetControlBar: TControlBar;
begin
  result := nil;
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    result := (m_frmOwner as TfrmMain).ControlBar;
end;

// TcForm
//   SetProgressBar
//
procedure TcForm.SetProgressBar(bparVisible: boolean; iValue, iMax: longint);
begin
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    (m_frmOwner as TfrmMain).SetProgressBar(bparVisible, iValue, iMax);
end;

// TcForm
//   GetIconImageList
//
function TcForm.GetIconImageList: TImageList;
begin
  result := nil;
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    result := (m_frmOwner as TfrmMain).ImageList;
end;

// TcForm
//   GetAlertImageList
//
function TcForm.GetAlertImageList: TImageList;
begin
  result := nil;
  if (m_frmOwner <> nil) and (m_frmOwner is TfrmMain) then
    result := (m_frmOwner as TfrmMain).AlertImageList;
end;

// TcForm
//   ExecuteItem
//
function TcForm.ExecuteItem(value: string; var sError: string): boolean;
var
  cmd: _Command;
  d: TDateTime;
  v: OLEVariant;
  cmd2: TOracleScript;
begin
  cmd := nil;
  sError := ksEMPTY;
  result := FALSE;
  case m_FormSet.m_objConnection.eMode of
    ecmMDAC:
      try
        cmd := CreateComObject(CLASS_Command) as _Command;
        cmd.Set_ActiveConnection(_Connection(m_FormSet.m_objConnection.adoConnection));
        cmd.CommandType := adCmdText;
        d := Now;
        try
          cmd.Set_CommandText(value);
          v := value;
          cmd.Execute(v, EmptyParam, adCmdText);
          result := TRUE;
        except
          on E: Exception do
            sError := E.Message;
        end;
        FormSet.SetLog(ksEMPTY, value, now - d, [elfValid, elfIncrement]);
      finally
        cmd := nil;
      end;
    ecmOracle:
      begin
        cmd2 := nil;
        try
          cmd2 := TOracleScript.Create(nil);
          cmd2.Session := m_FormSet.m_objConnection.Session;
          cmd2.Lines.Text := value;
          cmd2.ExitOnError := FALSE;
          cmd2.OutputOptions := [ooError];
          d := Now;
          try
            result := cmd2.Execute;
            sError := cmd2.Output.Text;
            if sError = ksEMPTY then
              m_FormSet.Connection.CommitTrans;
          except
            on E: Exception do
              sError := E.Message;
          end;
          FormSet.SetLog(ksEMPTY, value, now - d, [elfValid, elfIncrement]);
        finally
          cmd2.Free;
        end;
      end;
  end;
end;

// TcForm
//   CheckConnection
//
function TcForm.CheckConnection(bparConnect: boolean): boolean;
begin
  result := FALSE;
  if m_FormSet <> nil then
    result := m_FormSet.Connection.CheckConnection(bparConnect);
end;

// TcForm
//   Navigate
//
function TcForm.Navigate(value: string): boolean;
begin
  result := FALSE;
end;

//
// TcFormList
//

// TcFormList
//   Create
//
constructor TcFormList.Create(parParent: TcObject);
begin
  inherited Create;
  m_lstForms := TList.Create;
end;

// TcFormList
//   Destroy
//
Destructor TcFormList.Destroy;
begin
  Clear;
  m_lstForms.free;
  inherited Destroy;
end;

// TcFormList
//   Clear
//
procedure TcFormList.Clear;
var
  i: longint;
begin
  for i := m_lstForms.Count - 1 downto 0 do
  begin
    TcForm(m_lstForms[i]).Close;
    Delete(i);
  end;
end;

// TcFormList
//   Add
//
function TcFormList.Add(value: TcForm): longint;
begin
  result := m_lstForms.Add(value);
end;

// TcFormList
//   Delete (1)
//
function TcFormList.Delete(value: TcForm): boolean;
var
  L: longint;
begin
  L := m_lstForms.IndexOf(value);
  result := (L <> kiUNDEFINED) and Delete(L);
end;

// TcFormList
//   Delete (2)
//
function TcFormList.Delete(value: longint): boolean;
begin
  result := (value >= 0) and (value < m_lstForms.Count);
  if result then
    m_lstForms.Delete(value);
end;

// TcFormList
//   Get
//
function TcFormList.Get(item: longint): TcForm;
begin
  if item < GetCount then
    result := m_lstForms[item]
  else
    raise Exception.Create(Format(krsINDEXOUTOFRANGE, [GetCount, item]));
end;

// TcFormList
//   Put
//
procedure TcFormList.Put(item: longint; Value: TcForm);
begin
  if (item >= 0) and (item < GetCount) then
    m_lstForms[item] := Value
  else
    raise exception.create(Format(krsINDEXOUTOFRANGE, [GetCount, item]));
end;

// TcFormList
//   GetCount
//
function TcFormList.GetCount: longint;
begin
  result := m_lstForms.Count;
end;

//
// TcFormSet
//

// TcFormSet
//   Create
//
constructor TcFormSet.Create(parParent: TcObject);
var
  e: TeForm;
begin
  inherited Create(parParent);
  m_semBusy := 0;
  m_objConnection := TcConnection.Create(self);
  m_objConnection.procLog := SetLog;
  m_objConnection.FormSet := self;
  m_objMetaData := TcMetaData.Create(nil);
  m_objMetaData.Connection := m_objConnection;
  m_objSchema := TcSchema.Create(nil);
  for e := low(TeForm) to high(TeForm) do
  begin
    m_lstForm[e] := TcFormList.Create(self);
    (m_lstForm[e] as TcFormList).eForm := e;
  end;
  m_AOwner := nil;
  m_sParameter := ksEMPTY;
  m_objDataAncestor := nil;
  m_eForm := efsObjects;
  m_objImageList := nil;
  m_objPreferences := nil;
  m_lstLOG := TcStatementList.Create(nil);
  m_objDataFind := TcDataFind.Create(self);
  (m_objDataFind as TcDataFind).FormSet := self;
  (m_objDataFind as TcDataFind).ParentForm := frmMain.FindForm as TfrmDataFind;
  m_frmLog := nil;
  m_frmMemory := nil;
  m_objFavorite := TcFavorite.Create(nil);
  m_objFavorite.Load;
  m_objFavorite.hdlQuery := onFavoriteQuery;
  m_objHighlighter := TSyndbAnalystSQLSyn.Create(Application);
  m_objHeaderImageList := nil;
  m_objAlertImageList := nil;
  m_bCodeCompletion := TRUE;
  m_sConnectionCaption := ksEMPTY;
end;

// TcFormSet
//   Destroy
//
Destructor TcFormSet.Destroy;
var
  e: TeForm;
begin
  m_objConnection.Free;
  m_objMetaData.free;
  m_objMetaData := nil;
  m_objSchema.free;
  m_lstLOG.Free;
  m_objDataFind.Free;
  m_objDataFind := nil;
  for e := low(TeForm) to high(TeForm) do
    m_lstForm[e].free;
  m_objFavorite.free;
  //m_objHighlighter.Free;
  //m_objNumberHighlighter.free;
  inherited Destroy;
end;

// TcFormSet
//   Clear
//
procedure TcFormSet.Clear;
begin
  inherited Clear;
  m_objMetaData.Clear;
end;

// TcFormSet
//   Text
//
function TcFormSet.Text: String;
begin
  result := m_sConnectionCaption;
end;

// TcFormSet
//   GetForm
//
function TcFormSet.GetFormList(index: TeForm): TcFormList;
begin
  result := m_lstForm[index];
end;

// TcFormSet
//   GetIsEmpty
//
function TcFormSet.GetIsEmpty: boolean;
var
  e: TeForm;
begin
  result := TRUE;
  for e := low(TeForm) to high(TeForm) do
    result := result and (m_lstForm[e].Count = 0);
end;

// TcFormSet
//   CreateForm
//
function TcFormSet.CreateForm(eparForm: TeForm; AOwner: TComponent; eparMode: TeFormInitialization; eparParameter: String): boolean;
var
  frm: TcForm;
  c: String;
  d: TDateTime;
begin
  result := FALSE;
  if not IsInFormCreation then
  try
    SetInFormCreation(TRUE);
    if (GetForm(eparForm) <> nil) and (eparForm <> efsQuery) then
    begin
      GetForm(eparForm).Show;
      result := TRUE;
    end
    else
    begin
      d := now;
      // Get Meta Data
      if m_objMetaData.IsEmpty then
        m_objMetaData.Load(m_objConnection.sXML);
      // Preferences
      if m_objMetaData.Preferences = nil then
      begin
        m_objMetaData.Preferences := TcMetaDataPreferences.Create(m_objMetaData);
        m_objMetaData.Preferences.Load;
      end;
      // Initialize Query, if needed
      if not m_objSchema.Initialized and not m_objMetaData.IsEmpty then
      begin
        m_objSchema.Connection := m_objConnection;
        m_objSchema.MetaData := m_objMetaData;
        // We do not need to initialize the Schema. It will be initialized on the first access
      end;
      // Reset Syntax Hilighter.
      SetHilights;
      // Create Form
      frm := nil;
      if not (eparForm in [efsObjects]) and not HasFeature[eparForm] then
        eparForm := efsObjects;
      case eparForm of
        efsObjects:
          frm := TfrmData2.Create(AOwner);
        efsQuery:
          frm := TfrmQuery.Create(AOwner);
        efsGrid:
          frm := TfrmGrid.Create(AOwner);
        efsSessionManager:
          frm := TfrmSessionManager.Create(AOwner);
        efsLogParser:
          frm := TfrmLogParser.Create(AOwner);
        efsOnlineHelp:
          frm := TfrmOnlineHelp.Create(AOwner);
      end;
      if frm <> nil then
      try
        frm.CursorStack.Push(crHourGlass);
        frm.StatusText([psPanel2],'Initializing Form..', esiNone);
        c := AnsiReplaceText(kasFORMSET[eparForm], '&', ksEMPTY);
        frm.Caption := c; // Temporarily assign form caption
        m_lstForm[eparForm].Add(frm);
        frm.FormSet := self;
        frm.onPreferenceChange(self);
        if (parent <> nil) and (Parent is TcFormSetList) then
          (parent as TcFormSetList).SetFormFocus(frm);
        frm.Caption := Format('''%s'' - %s', [frm.Header, c]);
        frm.OnClose := FormClose;
        frm.eForm := eparForm;
        result := frm.Initialize(eparMode, eparParameter);
        frm.eFormState := efmRunning;
        if Assigned(frm.OnActivate) then
          frm.OnActivate(frm);
        SetLog(ksEMPTY, AnsiReplaceText(kasFORMSET[eparForm], '&', ksEMPTY) + ' Loaded', now - d, []);
        frm.StatusText([psPanel2], ksEMPTY, esiNone);
      finally
        frm.CursorStack.Pop;
      end;
      m_AOwner := AOwner;
      m_sParameter := eparParameter;
    end;
  finally
    SetInFormCreation(FALSE);
  end;
end;

// TcFormSet
//   FormClose
//
procedure TcFormSet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Sender <> nil) and (Sender is TcForm) then
  begin
    (Sender as TcForm).eFormState := efmFinalize;
    if (Sender as TcForm).Finalize then
    begin
      with Sender as TcForm do
        FormSet[eForm].Delete(sender as TcForm);
      if IsEmpty and (Parent <> nil) then
      begin
        if (m_frmLog <> nil) and (m_frmLog is TfrmSQLLog) then
          (m_frmLog as TfrmSQLLog).OnClearLog;
        if (m_frmMemory <> nil) and (m_frmMemory is TfrmMemorizedSQL) then
          (m_frmMemory as TfrmMemorizedSQL).OnClearLog;
        if (m_objDataFind <> nil) and (m_objDataFind is TcDataFind) then
          (m_objDataFind as TcDataFind).Disconnect;
        if Parent.IndexOf(Self) <> kiUNDEFINED then
          Parent.Delete(Parent.IndexOf(Self));
      end;
      Action := caFree;
    end;
  end;
end;

// TcFormSet
//   CloseForm
//
function TcFormSet.CloseForm: boolean;
var
  e: TeForm;
begin
  result := TRUE;
  for e := low(TeForm) to high(TeForm) do
    m_lstForm[e].Clear;
end;

// TcFormSet
//   CheckConnection
//
function TcFormSet.CheckConnection(bparConnect: boolean): boolean;
begin
  result := m_objConnection <> nil;
  if result then
  begin
    result := m_objConnection.Connected;
    if not result and bparConnect then
      result := m_objConnection.Login(nil, TRUE);
  end;
end;

// TcFormSet
//   onCreateForm
//
procedure TcFormSet.onCreateForm(sender: TObject);
begin
  if (Sender <> nil) and (Sender is TComponent) and (((Sender as TComponent).Tag - 1) in [longint(low(TeForm)) .. longint(high(TeForm))]) then
    onCreateForm(TeForm((Sender as TComponent).Tag - 1));
end;

// TcFormSet
//   onCreateForm
//
procedure TcFormSet.onCreateForm(value: TeForm);
begin
  CreateForm(value, m_AOwner, efiFromDatabase, m_sParameter);
end;

// TcFormSet
//   onSwitchForm
//
procedure TcFormSet.onSwitchForm(sender: TObject);
var
  p: TComponent;
begin
  if (Sender is TComponent) and ((Sender as TComponent).Tag <> 0) then
  begin
    p := TComponent((Sender as TComponent).Tag);
    if (p <> nil) and (p is TForm) and Allowed((p as TcForm).eForm) then
      (p as TForm).Show;
  end;
end;

// TcFormSet
//   Allowed
//
function TcFormSet.Allowed(value: TeForm): boolean;
begin
  result := m_objPreferences.IsLicensed or
            (not m_objPreferences.IsLicensed and not (value in [efsLogParser]));
  if not result then
    Application.MessageBox(krsDENIEDFEATURE, krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
end;

// TcFormSet
//   onSwitchForm
//
procedure TcFormSet.onSwitchForm(value: TeForm);
var
  p: TcFormList;
begin
  if Allowed(value) and (m_lstForm[value] <> nil) then
  begin
    p := m_lstForm[value];
    if (p <> nil) and (p.Count > 0) then
      with p.Forms[0] do
      begin
        if WindowState = wsMinimized then
          WindowState := wsNormal;
        Show;
      end
    else
      onCreateForm(value);
  end;
end;

// TcFormSet
//   onQuery
//
procedure TcFormSet.onQuery(sender: TObject);
var
  p: TcObject;
  q: TcMetaData;
begin
  if (Sender <> nil) and (Sender is TComponent) and ((Sender as TComponent).Tag <> 0) then
  begin
    p := TcObject((Sender as TComponent).Tag);
    q := nil;
    if (p <> nil) and (p is TcMetaData) then
      q := (p as TcMetaData).Find(enSQL, ksEMPTY) as TcMetaData;
    if q <> nil then
      ExecuteSQL(efsQuery, trim(q.sValue), eqtUndefined, []);
  end;
end;

// TcFormSet
//   ExecuteSQL
//
function TcFormSet.ExecuteSQL(eparForm: TeForm; eparValue: String; eparDisplayType: TeQueryType; eparOptionSet: TeQueryOptions): TcForm;
begin
  if GetForm(eparForm) = nil then
    CreateForm(eparForm, m_AOwner, efiFromFavorite, m_sParameter);
  result := GetForm(eparForm);
  if result <> nil then
  begin
    result.Show;
    if eparDisplayType <> eqtUndefined then
      result.eDisplayType := eparDisplayType;
    result.ExecuteItem(eparValue, eparOptionSet);
    result.Activate;
  end;
end;

// TcFormSet
//   GetHasFeature method
//
function TcFormSet.GetHasFeature(index: TeForm): boolean;
begin
  result := m_objMetaData.HasFeature[index];
end;

// TcFormSet
//   GetForm method
//
function TcFormSet.GetForm(Index: TeForm): TcForm;
begin
  result := nil;
  if m_lstForm[Index].Count > 0 then
    result := TcForm(m_lstForm[Index][0]);
end;

// TcFormSet
//   CreateAMenu
//
function TcFormSet.CreateAMenu(parMenu: TMenuItem; eForm: TeForm; cShortcut: char; iImageIndex: longint; sCaption: String; Handler: TNotifyEvent; bHasChildren, bInsertFirst, bEnabled: boolean; Tag: longint): TMenuItem;
begin
  result := nil;
  if kabFORMVISIBLE[eForm] then
  begin
    result := TMenuItem.Create(parMenu);
    if bInsertFirst and (parMenu.Count > 0) then
      parMenu.Insert(0, result)
    else
      parMenu.Add(result);
    result.Name := Format('%s_%d', [parMenu.Name, parMenu.Count]);
    result.Caption := sCaption;
    result.ImageIndex := iImageIndex;
    // result.Checked := Assigned(Handler) and (eForm = value);
    result.Tag := Tag;
    result.Enabled := bEnabled and ((m_objMetaData = nil) or ((m_objMetaData <> nil) and m_objMetaData.HasFeature[eForm]));
    if not bHasChildren then
      result.onClick := Handler;
    if not bHasChildren and (cShortcut <> #0) then
      result.ShortCut := ShortCut(Word(cShortcut), [ssAlt]);
  end;
end;

// TcFormSet
//   CleanupMenus
//
procedure TcFormSet.CleanupMenus(parMenu: TMenuItem);
var
  i: longint;
  p: TMenuItem;
begin
  for i := parMenu.Count - 1 downto 0 do
    if (i > 0) and (parMenu[i].Tag <> 0) then
    begin
      p := parMenu[i];
      CleanupMenus(p);
      parMenu.delete(i);
      p.free;
    end;
end;

// TcFormSet
//   InitializeToolMenu
//
procedure TcFormSet.InitializeToolMenu(parMenu: TMenuItem);
var
  e: TeForm;
  p: TMenuItem;
  i: longint;
begin
  CleanupMenus(parMenu);
  for e := high(TeForm) downto low(TeForm) do
  begin
    p := CreateAMenu(parMenu, e, kacSHORTCUTS[e], kaiFORMIMAGEINDEX[e], kasFORMSET[e], onCreateForm, e in [efsQuery, efsStoredQuery], TRUE, TRUE, longint(e) + 1);
    case e of
      efsStoredQuery:
        for i := 0 to m_objMetaData.Count - 1 do
          if (m_objMetaData[i].eType = enFeature) and
             (uppercase((m_objMetaData[i] as TcMetaData).Attribute[krsTYPE]) = krsQUERY) then
            CreateAMenu(p, e, #0, kiUNDEFINED, m_objMetaData[i].sName, onQuery, FALSE, FALSE, TRUE, longint(m_objMetaData[i]));
      efsQuery:
        CreateAMenu(p, e, kacSHORTCUTS[e], kiUNDEFINED, '&New Console', onCreateForm, FALSE, FALSE, TRUE, longint(e) + 1);
    end;
  end;
  parMenu.OnClick := onToolsClick;
end;

// TcFormSet
//   onToolsClick
//
procedure TcFormSet.onToolsClick(Sender: TObject);
var
  m, p: TMenuItem;
  i: longint;
begin
  if Sender is TMenuItem then
  begin
    m := Sender as TMenuItem;
    p := nil;
    for i := 0 to m.Count - 1 do
      if (m[i] <> nil) and (m[i] is TMenuItem) and (m[i].Tag - 1 = longint(efsQuery)) then
      begin
        p := m[i] as TMenuItem;
        break;
      end;
    if p <> nil then
    begin
      CleanupMenus(p);
      for i := 0 to m_lstForm[efsQuery].m_lstForms.Count - 1 do
        CreateAMenu(p, efsQuery, #0, kiUNDEFINED, TcForm(m_lstForm[efsQuery].m_lstForms[i]).Caption, onSwitchForm, FALSE, FALSE, TRUE, longint(m_lstForm[efsQuery].m_lstForms[i]));
    end;
  end;
end;

// TcFormSet
//   SetHighlighter
//
procedure TcFormSet.SetHighlighter(value: TSynEdit);
begin
  value.Highlighter := m_objHighlighter;
  value.HideSelection := False;
  value.ScrollBars := ssBoth;
  //value.EndOfTextMark.Style := psClear;
  //value.LeftMargin := 2;
  //value.ScrollTime := 0;
  //value.Options := [pmoKeepColumnPos, pmoPutExtraSpaces, pmoInsertKeyActive, pmoWideOverwriteCaret, pmoAutoScrollBars, pmoAutoIndent, pmoBackIndent, pmoWindowsSelColors, pmoBlockSelection, pmoSmartTabs];
  //value.ShowEndParSelected := FALSE;
  //value.Separators := m_objHighlighter.Separators;
end;

// TcFormSet
//   SetHilights
//
procedure TcFormSet.SetHilights;
const
  kasPREFERENCES: array[TeSyntaxHiliteType] of String =
    (krsPREF_SYNTAXKEYWORD, krsPREF_SYNTAXDATATYPE, krsPREF_SYNTAXFUNCTION, krsPREF_SYNTAXMARK, krsPREF_SYNTAXCOMMENT, krsPREF_SYNTAXPACKAGE, krsPREF_SYNTAXEXCEPTION, krsPREF_SYNTAXNUMBER, krsPREF_SYNTAXSTRING);
  SQLDelimiters: TSysCharSet =
    ['"', ' ', '''', '(', ')', ',', '.', '/', ':', ';', '<', '>', '[', ']', '{', '}', #9, '=', '-', '@'];
var
  e: TeSyntaxHiliteType;
  i: longint;
  p: TcMetaData;
  col: TColor;
  b: boolean;
  k: TtkTokenKind;
begin
  b := Preferences.StringVal[krsPREF_ENABLESYNTAXHILITING] <> krsFALSE;
  // A. Find Meta Data
  p := nil;
  if m_objMetaData <> nil then
    for i := 0 to m_objMetaData.Count - 1 do
      if (m_objMetaData[i] <> nil) and (m_objMetaData[i] is TcMetaData) and (m_objMetaData[i].eType = enSyntaxHilighting) then
      begin
        p := m_objMetaData[i] as TcMetaData;
        break;
      end;
  //
  if (p <> nil) and b then
  begin
    for i := 0 to p.Count - 1 do
      if (p[i] <> nil) and (p[i] is TcMetaData) and (p[i].eType = enKeyword) then
      begin
        k := tkNull;
        e := StringToSyntaxHiliteType((p[i] as TcMetaData).Attribute[krsTYPE]);
        col := m_objPreferences.Color[kasPREFERENCES[e]];
        case e of
          eshtKeyword:
            k := tkKey;
          eshtDatatype:
            k := tkdatatype;
          eshtFunction:
            k := tkFunction;
          eshtComment:
            k := tkComment;
          eshtPackage:
            k := tkPLSQL;
          eshtException:
            k := tkException;
        end;
        if k <> tkNull then
        try
          m_objHighlighter.SetWordList(k, p[i].sValue);
          m_objHighlighter.SetHilightStyle(k, [], col);
        except
          on x: Exception do
            m_objMetaData.SetError(elMetaFile, Format('Syntax Highligthing Error for ''%s'' list: %s', [kasSYNTAXHILITETYPE[e], x.Message]), ksEMPTY);
        end;
      end;
    // Numbers
    m_objHighlighter.SetHilightStyle(tkNumber, [], m_objPreferences.Color[krsPREF_SYNTAXMARK]);
    m_objHighlighter.SetHilightStyle(tkString, [], m_objPreferences.Color[krsPREF_SYNTAXMARK]);
  end;
end;

// TcFormSet
//   SetFileName
//
procedure TcFormSet.SetFileName(value: String);
begin
  m_sXMLFileName := value;
  m_objConnection.sXML := value;
end;

// TcFormSet
//   SetLog
//
function TcFormSet.SetLog(sSQLName, value: String; duration: TDateTime; eFlags: TeLogFlagSet): longint;
var
  p: TcStatement;
begin
  // a. Create Log Entry
  p := TcStatement.Create(m_lstLOG);
  result := m_lstLOG.Add(p);
  p.Parse(estSemicolon, value);
  p.datDuration := duration;
  p.bIsValid := elfValid in eFlags;
  p.sName := sSQLName;
  if elfIncrement in eFlags then
    p.iSQLNumber := m_lstLOG.SQLCount;
  SetLogDisplay(result);
  // b. Memorize?
  if ([elfMemorize, elfValid] * eFlags = [elfMemorize, elfValid]) and (value <> ksEMPTY) then
    Memorize(value);
end;

// TcFormSet
//   SetLogDisplay
//
procedure TcFormSet.SetLogDisplay(value: longint);
begin
  if (m_frmLog <> nil) and (m_frmLog is TfrmSQLLog) then
    (m_frmLog as TfrmSQLLog).OnSetLog(m_lstLog, value);
end;

// TcFormSet
//   onFavoriteQuery
//
procedure TcFormSet.onFavoriteQuery(Sender: TObject);
var
  p: TcFavoriteItem;
  frm: TcForm;
begin
  if (Sender <> nil) and (Sender is TMenuItem) and ((Sender as TMenuItem).Tag >= 0) and ((Sender as TMenuItem).Tag < m_objFavorite.Count) then
  begin
    p := m_objFavorite[(Sender as TMenuItem).Tag] as TcFavoriteItem;
    if GetForm(p.eFormType) = nil then
      CreateForm(p.eFormType, m_AOwner, efiFromFavorite, m_sParameter);
    frm := GetForm(p.eFormType);
    if frm <> nil then
      frm.ExecuteItem(p, []);
  end;
end;

// TcFormSet
//   GetSchema
//
function TcFormSet.GetSchema: TcSchema;
begin
  result := m_objSchema;
  if not m_objSchema.Initialized then
    m_objSchema.Initialize;
end;

// TcFormSet
//   onPreferenceChange
//
procedure TcFormSet.onPreferenceChange(Sender: TObject);
var
  i: longint;
  e: TeForm;
begin
  // Update Hilites
  SetHilights;
  // Send Preference Change message to everybody
  for e := low(TeForm) to high(TeForm) do
    for i := 0 to m_lstForm[e].Count - 1 do
      if (m_lstForm[e][i] <> nil) and (m_lstForm[e][i] is TcForm) then
        (m_lstForm[e][i] as TcForm).onPreferenceChange(Sender);
end;

// TcFormSet
//   GetSupport
//
function TcFormSet.GetSupport(value: TeFeatureSupport): boolean;
var
  p: TcObject;
begin
  p := nil;
  if m_objMetaData <> nil then
    p := m_objMetaData.Find(enFEATURE, kasFEATURESUPPORT[value]);
  result := (p = nil) or ((p <> nil) and (AnsiCompareText(p.sValue, krsTRUE) = 0));
end;

// TcFormSet
//   GetSupport
//
function TcFormSet.GetMetaData: TcMetaData;
begin
  if m_objMetaData.IsEmpty and (m_objConnection <> nil) then
    m_objMetaData.Load(m_objConnection.sXML);
  result := m_objMetaData;
end;

// TcFormSet
//   SetHeaderImage
//
function TcFormSet.SetHeaderImage(parImage: TImage; parIndex: longint): boolean;
begin
  result := (m_objHeaderImageList <> nil) and (parIndex <= m_objHeaderImageList.Count - 1);
  if result then
    m_objHeaderImageList.GetBitmap(parIndex, parImage.Picture.Bitmap);
end;

// TcFormSet
//   ApplySessionParameters
//
function TcFormSet.ApplySessionParameters(value: TcConnection): boolean;
var
  p: TcMetaData;
  i: longint;
  e: TcExecute;
begin
  result := (value <> nil) and Value.Connected;
  if result then
  begin
    p := m_objMetaData.Find(enFEATURE, 'SESSION') as TcMetaData;
    if p <> nil then
    begin
      e := nil;
      for i := 0 to p.count - 1 do
        if (p[i] <> nil) and (p[i].eType = enSQL) then
        try
          e := TcExecute.Create(nil);
          e.Connection := Value;
          try
            e.Execute(p[i].sValue);
          except
            //
          end;
        finally
          e.Free;
        end
    end;
  end;
end;

// TcFormSet
//   OnDataDump (1)
//
function TcFormSet.OnDataDump(value: String): boolean;
var
  frm: TfrmDataDump;
begin
  result := FALSE;
  frm := nil;
  if value <> ksEMPTY then
  try
    frm := TfrmDataDump.Create(Application);
    frm.FormSet := self;
    frm.SQL := value;
    result := frm.ShowModal = mrOK;
  finally
    frm.Release;
  end;
end;

// TcFormSet
//   OnDataDump (1)
//
function TcFormSet.OnDataDump(value: TcObject): boolean;
var
  frm: TfrmDataDump;
begin
  result := FALSE;
  frm := nil;
  if value <> nil then
  try
    frm := TfrmDataDump.Create(Application);
    frm.FormSet := self;
    frm.Data := value;
    result := frm.ShowModal = mrOK;
  finally
    frm.Release;
  end;
end;

// TcFormSet
//   Memorize
//
function TcFormSet.Memorize(sSQL: String): TeMemorizeResultSet;
begin
  result := [];
  if (parent <> nil) and (parent is TcFormSetList) and (m_objConnection <> nil) then
    result := (parent as TcFormSetList).Memorize(m_objConnection.sName, sSQL);
end;

// TcFormSet
//   GetRDBMS
//
function TcFormSet.GetRDBMS: String;
begin
  result := ksEMPTY;
  if (m_objMetaData <> nil) then
    result := m_objMetaData.Attribute[krsRDBMS];
end;

// TcFormSet
//   SetLRUMenu method
//
procedure TcFormSet.SetLRUMenu(parMenu: TMenuItem; parHandle: TNotifyEvent);
begin
  if (Parent <> nil) and (Parent is TcFormSetList) then
    (Parent as TcFormSetList).SetLRUMenu(GetRDBMS, parMenu, parHandle);
end;

// TcFormSet
//   AddLRU
//
function TcFormSet.AddLRU(parFileName: string): boolean;
begin
  result := FALSE;
  if (Parent <> nil) and (Parent is TcFormSetList) then
    result := (Parent as TcFormSetList).AddLRU(GetRDBMS, parFileName);
end;

// TcFormSet
//   SetInFormCreation
//
function TcFormSet.SetInFormCreation(value: boolean): longint;
begin
  if value then
    inc(m_semBusy)
  else
    dec(m_semBusy);
  result := m_semBusy;
end;

// TcFormSet
//   IsInFormCreation
//
function TcFormSet.IsInFormCreation: boolean;
begin
  result := m_semBusy > 0;
end;

// TcFormSet
//   GetOnlineHelp
//
function TcFormSet.GetOnlineHelp(value: string): boolean;
var
  p: TcFormList;
  m: TcObject;
begin
  result := FALSE;
  onSwitchForm(efsOnlineHelp);
  p := m_lstForm[efsOnlineHelp];
  if (p <> nil) and (p.Count > 0) then
  begin
    // Master Help?
    if value = ksEMPTY then
    begin
      m := MetaData.Find(enHELP, ksEMPTY) as TcMetaData;
      if (m <> nil) and (m.sValue <> ksEMPTY) then
        value := trim(m.sValue);
    end;
    // Navigate
    result := (p.Forms[0]).Navigate(value);
  end;
end;

// TcFormSet
//   HasMasterOnlineHelp
//
function TcFormSet.HasMasterOnlineHelp: boolean;
var
  p: TcObject;
begin
  p := MetaData.Find(enHELP, ksEMPTY) as TcMetaData;
  result := (p <> nil) and (p.sValue <> ksEMPTY);
end;

// TcFormSet
//   ComputeCaption
//
function TcFormSet.ComputeCaption(value: TStringList): boolean;
var
  c, s, v: String;
  L1, L2: longint;
begin
  result := FALSE;
  m_sConnectionCaption := ksEMPTY;
  if value <> nil then
  begin
    // Get Caption String
    c := '[User ID]@[Data Source]';
    if (m_objMetaData <> nil) and m_objMetaData.HasAttribute[krsCAPTION] then
      c := m_objMetaData.Attribute[krsCAPTION];
    // Replace bracket-enclosed strings
    repeat
      L1 := pos('[', c);
      if L1 > 0 then
      begin
        L2 := pos(']', system.copy(c, L1, length(c)));
        if L2 > 0 then
        begin
          s := system.Copy(c, L1 + 1, L2 - 2);
          system.delete(c, L1, L2);
          v := value.Values[s]; // use first connection string
          system.Insert(v, c, L1);
        end;
      end;
    until L1 = 0;
    m_sConnectionCaption := c;
    result := c <> ksEMPTY;
  end;
  if (m_sConnectionCaption = ksEMPTY) and (m_sXMLFileName <> ksEMPTY) then
    m_sConnectionCaption := m_sXMLFileName;
end;

// TcFormSet
//   Reconnect
//
function TcFormSet.Reconnect: boolean;
var
  e: TeForm;
  fl: TcFormList;
  f: TcForm;
  i: longint;
begin
  result := m_objConnection.Reconnect;
  if result then
    for e := low(TeForm) to high(TeForm) do
    begin
      fl := m_lstForm[e];
      for i := 0 to fl.Count - 1 do
      begin
        f := fl.Forms[i];
        if f <> nil then
        try
          f.CursorStack.Push(crHourGlass);
          f.StatusText([psPanel2],'Re-Connecting..', esiNone);
          f.Reconnect;
          f.StatusText([psPanel2], ksEMPTY, esiNone);
        finally
          f.CursorStack.Pop;
        end;
      end;
    end;
end;

//
// TcFormSetList
//

// TcFormSetList
//   Create
//
constructor TcFormSetList.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_hdlFormFocus := nil;
  m_frmLog := nil;
  m_frmMemory := nil;
  m_frmFind := nil;
  m_objMemorizedSQL := TcMemorizedSQL.Create(nil);
  m_lstLRU := TcObject.Create(nil);
  LoadLRU;
end;

// TcFormSetList
//   Destroy
//
Destructor TcFormSetList.Destroy;
begin
  m_objMemorizedSQL.free;
  m_lstLRU.free;
  inherited Destroy;
end;

// TcFormSetList
//   Add
//
function TcFormSetList.Add(parObject: TcObject): longint;
begin
  result := inherited Add(parObject);
  if Assigned(m_hdlFormFocus) then
    m_hdlFormFocus(parObject);
end;

// TcFormSetList
//   Delete
//
function TcFormSetList.Delete(parObject: TcObject): longint;
begin
  result := inherited Delete(parObject);
  if Assigned(m_hdlFormFocus) then
    m_hdlFormFocus(nil);
end;

// TcFormSetList
//   Delete
//
function TcFormSetList.Delete(item: longint): longint;
begin
  result := inherited Delete(item);
  if Assigned(m_hdlFormFocus) then
    m_hdlFormFocus(nil);
end;

// TcFormSetList
//   SetFocus
//
procedure TcFormSetList.SetFormFocus(Sender: TObject);
begin
  if assigned(m_hdlFormFocus) then
    m_hdlFormFocus(Sender);
  if (Sender <> nil) and (Sender is TcForm) then
  begin
    if (m_frmLog <> nil) and (m_frmLog is TfrmSQLLog) then
      (m_frmLog as TfrmSQLLog).SetFormFocus((Sender as TcForm).FormSet);
    if (m_frmMemory <> nil) and (m_frmMemory is TfrmMemorizedSQL) then
      (m_frmMemory as TfrmMemorizedSQL).SetFormFocus((Sender as TcForm).FormSet);
    //if (m_frmFind <> nil) and (m_frmFind is TfrmDataFind) then
    //  (m_frmFind as TfrmDataFind).SetFormFocus((Sender as TcForm).FormSet);
  end;
end;

// TcFormSetList
//   onPreferenceChange
//
procedure TcFormSetList.onPreferenceChange(Sender: TObject);
var
  i: longint;
begin
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcFormSet) then
      (Objects[i] as TcFormSet).onPreferenceChange(Sender);
end;

// TcFormSetList
//   Memorize
//
function TcFormSetList.Memorize(sparSection, sparSQL: String): TeMemorizeResultSet;
begin
  result := m_objMemorizedSQL.Memorize(sparSection, sparSQL);
  if (m_frmMemory <> nil) and (m_frmMemory is TfrmMemorizedSQL) then
  begin
    if [mrDisplayCombo] * result <> [] then
      (m_frmMemory as TfrmMemorizedSQL).Display(mrDisplayCombo);
    if [mrDisplayList] * result <> [] then
      (m_frmMemory as TfrmMemorizedSQL).Display(mrDisplayList);
  end;
end;

// TcFormSetList
//   SetLRUMenu method
//
procedure TcFormSetList.SetLRUMenu(parRDBMS: String; parMenu: TMenuItem; parHandle: TNotifyEvent);
var
  i, L: longint;
  p: TMenuItem;
  q: TcObject;
begin
  // Remove existing menu items
  for i := parMenu.Count - 1 downto 0 do
    if parMenu[i].Tag <> 0 then
      parMenu.Remove(parMenu.Items[i]);
  // Only draw if Handle is defined.
  if Assigned(parHandle) then
  begin
    // Find LRU Branch
    q := m_lstLRU.Find(parRDBMS);
    // Add new sub menus
    L := 0;
    if q <> nil then    
      for i := 0 to q.Count - 1 do
        if trim(q[i].sName) <> ksEMPTY then
        begin
          inc(L);
          p := TMenuItem.Create(parMenu);
          if i < 9 then
            p.Caption := Format('&%d %s', [i + 1, GetFileName(q[i].sName)])
          else
            p.Caption := Format('%s', [GetFileName(q[i].sName)]);
          p.Hint := Format('Location: %s', [q[i].sName]);
          p.OnClick := parHandle;
          p.Tag := longint(q[i]);
          p.ImageIndex := 86;
          parMenu.Add(p);
          if L >= kriLRULISTSIZE then
            break;
        end;
  end;
end;

// TcFormSetList
//   LoadLRU
//
function TcFormSetList.LoadLRU: boolean;
var
  i, j: longint;
  p1, p2: TcObject;
  c1, c2: TRegistry;
  lst1, lst2: TStringList;
begin
  m_lstLRU.Clear;
  c1 := nil;
  try
    c1 := TRegistry.Create;
    c1.RootKey := HKEY_CURRENT_USER;
    lst1 := nil;
    if c1.OpenKey(Format('%s%s', [krsREGISTRYLOCATION, krsREGISTRYLRU]), TRUE) then
    try
      lst1 := TStringList.Create;
      c1.GetKeyNames(lst1);
      for i := 0 to lst1.count - 1 do
      begin
        c2 := nil;
        try
          c2 := TRegistry.Create;
          c2.RootKey := HKEY_CURRENT_USER;
          if c2.OpenKey(Format('%s%s\%s', [krsREGISTRYLOCATION, krsREGISTRYLRU, lst1[i]]), FALSE) then
          begin
            p1 := TcObject.Create(m_lstLRU);
            m_lstLRU.Add(p1);
            p1.sName := lst1[i];
            lst2 := nil;
            try
              lst2 := TStringList.Create;
              c2.GetValueNames(lst2);
              for j := 0 to lst2.Count - 1 do
              begin
                p2 := TcObject.Create(p1);
                p1.Add(p2);
                p2.sName := c2.ReadString(lst2[j]);
              end;
            finally
              lst2.free;
            end;
            c2.CloseKey;
          end;
        finally
          c2.free;
        end;
      end;
      c1.CloseKey;
    finally
      lst1.free;
    end;
  finally
    c1.free;
  end;
  result := m_lstLRU.Count > 0;
end;

// TcFormSetList
//   SaveLRU
//
function TcFormSetList.SaveLRU: boolean;
var
  i, j: longint;
  p1: TcObject;
  c1: TRegistry;
begin
  c1 := nil;
  try
    c1 := TRegistry.Create;
    c1.RootKey := HKEY_CURRENT_USER;
    for i := 0 to m_lstLRU.Count - 1 do
    begin
      p1 := m_lstLRU[i];
      if c1.OpenKey(Format('%s%s\%s', [krsREGISTRYLOCATION, krsREGISTRYLRU, p1.sName]), TRUE) then
      begin
        for j := 0 to p1.Count - 1 do
          c1.WriteString(inttostr(j), p1[j].sName);
        c1.CloseKey;
      end;
    end;
  finally
    c1.Free;
  end;
  result := TRUE;
end;

// TcFormSetList
//   AddLRU
//
function TcFormSetList.AddLRU(parRDBMS, parFileName: string): boolean;
var
  p, q: TcObject;
begin
  result := FALSE;
  if (parRDBMS <> ksEMPTY) and (parFileName <> ksEMPTY) then
  begin
    // Find if already part of the list
    p := m_lstLRU.Find(parRDBMS);
    if p = nil then
    begin
      p := TcObject.Create(m_lstLRU);
      m_lstLRU.Add(p);
      p.sName := parRDBMS;
    end;
    // Find if already part of the list
    q := p.Find(parFileName);
    // If not there, then create new node, else pick up found node
    if q = nil then
    begin
      q := TcObject.Create(p);
      p.Insert(0, q);
      q.sName := parFileName;
      while p.Count > kriLRULISTSIZE do
        with p do
          Delete(Count - 1);
    end
    else
      p.Move(p.IndexOf(q), 0);
  end;
  SaveLRU;
end;

//
// Tools
//

// Tools
//   SetFormType
//
procedure SetFormType(obj: TComponent; objMetaData: TcMetaData; value: TeForm; Handler: TNotifyEvent);
var
  e: TeForm;
begin
  if obj is TComboBoxEx then
  begin
    (obj as TComboBoxEx).ItemsEx.BeginUpdate;
    (obj as TComboBoxEx).ItemsEx.Clear;
    for e := low(TeForm) to high(TeForm) do
      if (e <> efsStoredQuery) and kabFORMVISIBLE[e] and ((objMetaData = nil) or ((objMetaData <> nil) and objMetaData.HasFeature[e])) then
      begin
        (obj as TComboBoxEx).ItemsEx.AddItem(AnsiReplaceText(kasFORMSET[e], '&', ksEMPTY), kaiFORMIMAGEINDEX[e], kaiFORMIMAGEINDEX[e], kaiFORMIMAGEINDEX[e], 0, pointer(e));
        if e = value then
          (obj as TComboBoxEx).ItemIndex := (obj as TComboBoxEx).ItemsEx.Count - 1;
      end;
    with (obj as TComboBoxEx) do
      if (ItemIndex = kiUNDEFINED) and (ItemsEx.Count > 0) then
        ItemIndex := 0;
    (obj as TComboBoxEx).ItemsEx.EndUpdate;
  end
end;

// Tools
//   StringToSyntaxHiliteType
//
function StringToSyntaxHiliteType(value: String): TeSyntaxHiliteType;
var
  e: TeSyntaxHiliteType;
begin
  result := eshtKeyword;
  for e := low(TeSyntaxHiliteType) to high(TeSyntaxHiliteType) do
    if AnsiCompareText(kasSYNTAXHILITETYPE[e], value) = 0 then
    begin
      result := e;
      break;
    end;
end;

end.



