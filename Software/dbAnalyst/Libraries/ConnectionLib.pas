unit ConnectionLib;

interface

uses
  daObjectLib,
{$IFNDEF FACTORY}
  PreferenceLib,
{$ENDIF}
  daGlobals,
  Classes,
  ADODB_TLB,
  Oracle;

type
  TeConnectionOption = (ecoRefreshCache, ecoRecomputeGraph, ecoMemorize);
  TeConnectionOptionSet = set of TeConnectionOption;

  TcConnection = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcConnection is the connection object.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 09/07/00 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    // Generic
    m_eMode: TeConnectionMode;
    m_bLoginPrompt: boolean;
    m_bInTransaction: boolean;
    m_sXML: String;
    m_eForm: TeForm;
    m_lstXML: TcCollection;
    m_procLog: TfctSetLog;
    m_eOptions: TeConnectionOptionSet;
    m_objFormSet: TcObject;
    m_bOneConnection: boolean;
    m_bAutoCommit: boolean;
    m_lstValuePairs: TStringList;
    //m_lstValuePairs
    // MDAC
    m_sADO: String;
    m_objConnection: TConnection;
    // Oracle
    m_sUserName: String;
    m_sPassword: String;
    m_sServer: String;
    m_objSession: TOracleSession;
    m_sConnectAs: String;

  published
    // Published declarations
    //
    function    GetConnected: boolean;
    procedure   SetXMLList;
    function    GetADOConnection: _Connection;

  private
    // private declarations
    //
    function    GetConnection: String;
    procedure   SetAutoCommit(value: boolean);
    function    GetInTransaction: boolean;
    function    GetAttribute(key: String): String;
    function    GetHasAttribute(key: String): boolean;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    //   Clear
    procedure   Copy(value: TcObject); override;
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom
{$IFNDEF FACTORY}
    function    Select: boolean; virtual;
    function    Login(parPreference: TcPreference; bparOpen: boolean): boolean; virtual;
    function    CrypticFileName: String;
{$ENDIF}
    function    Open: boolean; virtual;
    function    Close: boolean;
    procedure   BeginTrans;
    procedure   CommitTrans;
    procedure   RollbackTrans;
    function    CheckConnection(bparConnect: boolean): boolean;
    function    SetLog(value: String; duration: TDateTime): longint; overload;
    function    SetLog(sSQLName, value: String; duration: TDateTime): longint; overload;
    function    SetLog(sSQLName, value: String; duration: TDateTime; eFlags: TeLogFlagSet): longint; overload;
    function    Reconnect: boolean;

  public
    // Public Properties
    //
    // Generic
    property    eMode: TeConnectionMode                 read m_eMode            write m_eMode;
    property    ADOConnection: _Connection              read GetADOConnection;
    property    Connected: boolean                      read GetConnected;
    property    bLoginPrompt: boolean                   read m_bLoginPrompt     write m_bLoginPrompt;
    property    InTransaction: boolean                  read GetInTransaction;
    property    sXML: String                            read m_sXML             write m_sXML;
    property    eForm: TeForm                           read m_eForm            write m_eForm;
    property    lstXML: TcCollection                    read m_lstXML;
    property    procLog: TfctSetLog                     read m_procLog          write m_procLog;
    property    Options: TeConnectionOptionSet          read m_eOptions         write m_eOptions;
    property    FormSet: TcObject                       read m_objFormSet       write m_objFormSet;
    property    bOneConnection: boolean                 read m_bOneConnection   write m_bOneConnection;
    property    bAutoCommit: boolean                    read m_bAutoCommit      write SetAutoCommit;
    property    HasAttribute[value: String]: boolean    read GetHasAttribute;
    property    Attribute[value: String]: String        read GetAttribute;
    property    AttributeKeys: TStringList              read m_lstValuePairs;
    // MDAC
    property    Connection: TConnection                 read m_objConnection;
    property    ConnectionString: String                read GetConnection      write m_sADO;
    // Oracle
    property    sUserName: String                       read m_sUserName        write m_sUserName;
    property    sPassword: String                       read m_sPassword        write m_sPassword;
    property    sServer: String                         read m_sServer          write m_sServer;
    property    Session: TOracleSession                 read m_objSession       write m_objSession;
    property    sConnectAs: String                      read m_sConnectAs       write m_sConnectAs;
  end;

implementation

uses
  Windows,
  ActiveX,
{$IFNDEF FACTORY}
  Forms,
  FormLib,
  Controls,
  frmConnection,
  frmConnectionLogin,   // TfrmConnectionLogin
{$ENDIF}
  ComObj,
  sysUtils,
  strUtils,
  daResourceStrings,
  Variants;

const
  krsUSERID = 'User ID';
  krsPASSWORD = 'Password';

// TcConnection
//   Create
//
constructor TcConnection.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objConnection := TConnection.Create(nil);
  m_bLoginPrompt := TRUE;
  m_bInTransaction := FALSE;
  m_eForm := low(TeForm);
  m_lstXML := TcCollection.Create(self);
  m_procLog := nil;
  m_eOptions := [];
  m_objFormSet := nil;
  m_eMode := ecmMDAC;
  m_bOneConnection := FALSE;
  m_sUserName := ksEMPTY;
  m_sPassword := ksEMPTY;
  m_sServer := ksEMPTY;
  m_objSession := TOracleSession.Create(nil);
  m_bAutoCommit := TRUE;
  m_sConnectAs := ksEMPTY;
  m_lstValuePairs := TStringList.Create;
  SetXMLList;
  CoInitialize(nil);
end;

// TcConnection
//   Destroy
//
Destructor TcConnection.Destroy;
begin
  m_objConnection.free;
  m_lstXML.Free;
  m_objSession.free;
  m_lstValuePairs.free;
  CoUninitialize;
  inherited Destroy;
end;

// TcConnection
//   Copy
//
procedure TcConnection.Copy(value: TcObject);
begin
  inherited Copy(value);
  if value is TcConnection then
  begin
    m_sADO           := (value as TcConnection).m_sADO;
    // m_objConnection
    m_bLoginPrompt   := (value as TcConnection).m_bLoginPrompt;
    m_bInTransaction := (value as TcConnection).m_bInTransaction;
    m_sXML           := (value as TcConnection).m_sXML;
    m_eForm          := (value as TcConnection).m_eForm;
    m_objFormSet     := (value as TcConnection).m_objFormSet;
    m_eMode          := (value as TcConnection).m_eMode;
    // m_lstXML
    m_procLog        := (value as TcConnection).m_procLog;
    m_eOptions       := (value as TcConnection).m_eOptions;
    m_sUserName      := (value as TcConnection).m_sUserName;
    m_sPassword      := (value as TcConnection).m_sPassword;
    m_sServer        := (value as TcConnection).m_sServer;
    m_bOneConnection := (value as TcConnection).m_bOneConnection;
    m_bAutoCommit    := (value as TcConnection).m_bAutoCommit;
    m_sConnectAs     := (value as TcConnection).m_sConnectAs;
    // m_objSession
    m_lstValuePairs.Text := (value as TcConnection).m_lstValuePairs.Text;
  end
  else if value is TcPreference then
  begin
    eMode            := (value as TcPreference).eMode;
    m_sADO           := (value as TcPreference).sADO;
    m_sUserName      := (value as TcPreference).Attribute[krsXML_USERNAME];
    m_sPassword      := (value as TcPreference).Attribute[krsXML_PASSWORD];
    m_sServer        := (value as TcPreference).Attribute[krsXML_DATASOURCE];
    m_sConnectAs     := (value as TcPreference).Attribute[krsXML_CONNECTAS];
    m_eOptions       := [];
    m_bOneConnection := (value as TcPreference).bOneConnection;
    m_lstValuePairs.Text := (value as TcPreference).ValuePairs.Text;
  end;
end;

// TcConnection
//   Text
//
function TcConnection.Text: String;
begin
  result := sName;
end;

{$IFNDEF FACTORY}
// TcConnection
//   Select
//
function TcConnection.Select: boolean;
var
  frm: TfrmConnection;
begin
  frm := TfrmConnection.Create(nil);
  frm.Connection := self;
  if (Parent <> nil) and (Parent is TcFormSet) then
  begin
    frm.ImageList := (Parent as TcFormSet).ImageList;
    frm.PreferenceList := (Parent as TcFormSet).Preferences;
  end;
  result := frm.ShowModal = idOK;
  if result and (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) then
  begin
    (m_objFormSet as TcFormSet).ApplySessionParameters((m_objFormSet as TcFormSet).Connection);
    (m_objFormSet as TcFormSet).ComputeCaption(m_lstValuePairs);
  end;
end;
{$ENDIF}

// TcConnection
//   Open
//
function TcConnection.Open: boolean;

  function StringToConnectAsOption(value: String): TConnectAsOption;
  var
    e: TConnectAsOption;
  const
    kasCONNECTAS: array[TConnectAsOption] of String =
      ('normal', 'sysdba', 'sysoper');
  begin
    result := caNormal;
    for e := low(TConnectAsOption) to high(TConnectAsOption) do
      if AnsiCompareText(kasCONNECTAS[e], value) = 0 then
      begin
        result := e;
        break;
      end;
  end;

type
  TConnectAsOption = (caNormal, caSYSDBA, caSYSOPER);
{$IFNDEF FACTORY}
var
  d: TDateTime;
  s: String;
{$ENDIF}
begin
{$IFNDEF FACTORY}
  d := Now;
{$ENDIF}
  case m_eMode of
    ecmMDAC:
      m_objConnection.Open(m_sADO, ksEMPTY, ksEMPTY, 0);
    ecmOracle:
      begin
        m_objSession.LogonUsername := m_sUserName;
        m_objSession.LogonPassword := m_sPassword;
        m_objSession.LogonDatabase := m_sServer;
        m_objSession.ConnectAs := StringToConnectAsOption(m_sConnectAs);
        m_objSession.LogOn;
      end;
  end;
  result := GetConnected;
{$IFNDEF FACTORY}
  if result then
  begin
    s := 'Connected';
    if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) then
      s := Format('Connected to ''%s''', [(m_objFormSet as TcFormSet).Text]);
    SetLog('Open Connection', s, now - d, []);
  end;
{$ENDIF}
end;

// TcConnection
//   Reconnect
//
function TcConnection.Reconnect: boolean;
var
  d: TDateTime;
begin
  result := FALSE;
  d := Now;
  try
    if getConnected then
      Close;
  except
    //
  end;
  try
    result := Open;
  except
    on E: Exception do
      SetLog('Reconnect Connection', Format('Exception: %s', [E.Message]), Now - d, []);
  end;
end;

// TcConnection
//   Close
//
function TcConnection.Close: boolean;
{$IFNDEF FACTORY}
var
  d: TDateTime;
  s: String;
{$ENDIF}
begin
{$IFNDEF FACTORY}
  d := Now;
{$ENDIF}
  result := GetConnected;
{$IFNDEF FACTORY}
  if result then
  try
    case m_eMode of
      ecmMDAC:
        m_objConnection.Close;
      ecmOracle:
        m_objSession.LogOff;
    end;
    s := 'Disconnected';
    if (m_objFormSet <> nil) and (m_objFormSet is TcFormSet) then
      s := Format('Disconnected from ''%s''', [(m_objFormSet as TcFormSet).Text]);
    SetLog('Close Connection', s, Now - d, []);
  except
    on E: Exception do
      SetLog('Close Connection', Format('Exception: %s', [E.Message]), Now - d, []);
  end;
{$ENDIF}
end;

// TcConnection
//   GetConnected
//
function TcConnection.GetConnected: boolean;
begin
  case m_eMode of
    ecmMDAC:
      result := (m_objConnection <> nil) and (m_objConnection.State and adStateOpen <> 0);
    ecmOracle:
      result := (m_objSession <> nil) and m_objSession.Connected;
    else
      result := FALSE;
  end;
end;

// TcConnection
//   BeginTrans
//
procedure TcConnection.BeginTrans;
begin
  if not GetInTransaction then
  try
    case m_eMode of
      ecmMDAC:
        begin
          m_objConnection.BeginTrans;
          m_bInTransaction := TRUE;
        end;
      ecmOracle:
        m_bInTransaction := m_objSession.InTransaction;
    end;
  except
    //
  end;
end;

// TcConnection
//   CommitTrans
//
procedure TcConnection.CommitTrans;
begin
  if GetInTransaction then
  try
    case m_eMode of
      ecmMDAC:
        begin
          m_objConnection.CommitTrans;
          m_bInTransaction := FALSE;
        end;
      ecmOracle:
        begin
          m_objSession.Commit;
          m_bInTransaction := m_objSession.InTransaction;
        end;
    end;
  except
    //
  end;
end;

// TcConnection
//   RollbackTrans
//
procedure TcConnection.RollbackTrans;
begin
  if GetInTransaction then
  try
    case m_eMode of
      ecmMDAC:
        begin
          m_objConnection.RollbackTrans;
          m_bInTransaction := FALSE;
        end;
      ecmOracle:
        begin
          m_objSession.Rollback;
          m_bInTransaction := m_objSession.InTransaction;
        end;
    end;
  except
    //
  end;
end;

// TcConnection
//   CheckConnection
//
function TcConnection.CheckConnection(bparConnect: boolean): boolean;
begin
  result := Connected;
{$IFNDEF FACTORY}
  if not result and bparConnect then
    result := Login(nil, TRUE);
{$ENDIF}
end;

// TcConnection
//   SetXMLList
//
procedure TcConnection.SetXMLList;
begin
  GetXMLList(m_lstXML);
end;

// TcConnection
//   SetLog (1)
//
function TcConnection.SetLog(value: String; duration: TDateTime): longint;
begin
  result := SetLog(ksEMPTY, value, duration, [elfValid, elfIncrement]);
end;

// TcConnection
//   SetLog (2)
//
function TcConnection.SetLog(sSQLName, value: String; duration: TDateTime): longint;
begin
  result := SetLog(sSQLName, value, duration, [elfValid, elfIncrement]);
end;

// TcConnection
//   SetLog (3)
//
function TcConnection.SetLog(sSQLName, value: String; duration: TDateTime; eFlags: TeLogFlagSet): longint;
{$IFDEF SBS_DEBUG}
var
   s: String;
{$ENDIF}
begin
  result := kiUNDEFINED;
  try
    if [ecoMemorize] * m_eOptions <> [] then
      eFlags := eFlags + [elfMemorize];
    if assigned(m_procLog) then
      result := m_procLog(sSQLName, value, duration, eFlags);
  except
    on E: Exception do
      SetError(Format('%s%sSQL [%s]: %s',[E.Message, ksCR, sSQLName, value]));
  end;
{$IFDEF SBS_DEBUG}
  try
     s := ksEMPTY;
     if duration <> 0 then
       s := Format(' [%1.3fs]', [Duration * 86400]);
     LogToFile('SQL.LOG', '######################' + ksCR +
                          Format('# %s%s', [sSQLName, s]) + ksCR +
                          '######################' + ksCR +
                          trim(value) + ksCR + ksCR);
  except
    on E: Exception do
      SetError(E.Message);
  end;
{$ENDIF}
end;

// TcConnection
//   GetADOConnection
//
function TcConnection.GetADOConnection: _Connection;
begin
  result := m_objConnection.DefaultInterface as _Connection;
end;

{$IFNDEF FACTORY}
// TcConnection
//   CrypticFileName
//
function TcConnection.CrypticFileName: String;
var
  i, L: longint;
  s: String;
  lst: TStringList;
begin
  lst := nil;
  try
    lst := TStringList.Create;
    lst.QuoteChar := '"';
    lst.Delimiter := ';';
    lst.DelimitedText := lowercase(Format('name="%s";', [sName]) + m_sADO);
    // Build String
    s := ksEMPTY;
    for i := 0 to lst.Count - 1 do
      s := s + lst.Values[lst.Names[i]];
    // Clean Up String
    L := 1;
    while L <= length(s) do
    begin
      if not CharInSet(s[L], ['a' .. 'z', '0' .. '9']) then
        system.Delete(s, L, 1)
      else
        inc(L);
    end;
    //
  finally
    lst.free;
  end;
  // Done.
  result := GetFilePath(Application.ExeName) + system.copy(s, 1, 100) + '.' + krsCACHEFILEEXTENSION;
end;
{$ENDIF}

// TcConnection
//   Login
//
function TcConnection.Login(parPreference: TcPreference; bparOpen: boolean): boolean;
var
  frm: TfrmConnectionLogin;
  p, q: TcPreference;
begin
  q := nil;
  try
    p := nil;
    try
      p := parPreference;
      if p = nil then
        p := TcPreference.Create(nil);
      // Create a Copy of the pref object
      q := TcPreference.Create(nil);
      q.Copy(p);
      // Pop form up.
      frm := nil;
      try
        frm := TfrmConnectionLogin.Create(application);
        frm.Connection := self;
        frm.Preference := q;
        result := frm.ShowModal = mrOK;
        if result and bparOpen then
          Open;
        // Prefernces should be updated?
        if result and frm.btnUpdateParameters.Checked then
        begin
          p.Copy(q);
          if parPreference <> nil then
            parPreference.WriteToRegistry;
        end;
      finally
        frm.release;
      end;
    finally
      if parPreference = nil then
        p.Free;
    end;
  finally
    q.Free;
  end;
end;

// TcConnection
//   GetConnection
//
function TcConnection.GetConnection: String;
begin
  case m_eMode of
    ecmMDAC:
    begin
      result := m_objConnection.ConnectionString;
      if result = ksEMPTY then
        result := m_sADO;
    end;
    ecmOracle:
      result := Format('%s=%s;%s=%s;Data Source=%s;', [krsUSERID, m_sUserName, krsPASSWORD, m_sPassword, m_sServer]);
    else
      result := ksEMPTY;
  end;
end;

// TcConnection
//   SetAutoCommit
//
procedure TcConnection.SetAutoCommit(value: boolean);
begin
  case m_eMode of
    ecmMDAC:
      ;
    ecmOracle:
      m_objSession.AutoCommit := value;
  end;
  m_bAutoCommit := value;
end;

// TcConnection
//   GetInTransaction
//
function TcConnection.GetInTransaction: boolean;
begin
  try
    case m_eMode of
      ecmMDAC:
        result := m_bInTransaction;
      ecmOracle:
        result := m_objSession.InTransaction
      else
        result := FALSE;
    end;
  except
    result := FALSE;
  end;
end;

// TcConnection
//   GetAttribute
//
function TcConnection.GetAttribute(key: String): String;
begin
  if AnsiCompareText(key, krsXML_USERNAME) = 0 then
    result := m_sUserName
  else if AnsiCompareText(key, krsXML_PASSWORD) = 0 then
    result := m_sPassword
  else if AnsiCompareText(key, krsXML_DATASOURCE) = 0 then
    result := m_sServer
  else if AnsiCompareText(key, krsXML_CONNECTAS) = 0 then
    result := m_sConnectAs
  else
    result := m_lstValuePairs.Values[key];
end;

// TcConnection
//   GetHasAttribute
//
function TcConnection.GetHasAttribute(key: String): boolean;
begin
  result := GetAttribute(key) <> ksEMPTY;
end;

end.



