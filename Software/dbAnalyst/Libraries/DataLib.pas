unit DataLib;

interface

{$M+}

uses
  Graphics,
  Classes,
  daGlobals,
  daObjectLib,
  ComCtrls, // TreeNode, ListView
  ADODB_TLB,
  daStreamLib,
  ConnectionLib;

type
  TcData = class;
  TcMetaData_ExpressionQuery = function(value: String): String of object;
  TcRuleResult = class;
  TcMetaDataPreferences = class;
  TeIndexAction = (eiaAdd, eiaDelete);
  TcValuePair = record Name, Value: String; end;
  TcValuePairSet = array of TcValuePair;
  TcMetaData = class;
  TeLoadFlag = (elfRecursive, elfRefresh, elfAll, elfSkim);
  TeLoadFlagSet = set of TeLoadFlag;
  TeFullNameMode = (efmIdentifier, efmExtended);

  TcCustomData = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcMetaData is the base meta object for reverse engineering.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 08/19/04 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objDisplay: TObject;

  protected
    // protected declarations
    //
    function    GetDisplay: TObject; virtual;
    function    GetFullName(value: TeFullNameMode): String; virtual;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    //   Destroy; override;                                                     // Standard destructor
    //   Clear; override;                                                       // Clear the object
    //   Copy(value: TcObject); override;
    //   Compare
    //   Text
    function    Load(parMetaData: TcMetaData; parFlags: TeLoadFlagSet): boolean; overload; virtual;
    //   Save
    //
    // 2. Custom
    procedure   ClearDisplayRefefence;
    function    StreamObject(value: TeObjectMarker): TcObject; override;
    function    CountType(value: TeType): longint;
    function    ParentObject(eparSet: TeTypeSet): TcCustomData;

  public
    // Public Properties
    //
    property    objDisplay: TObject                       read GetDisplay       write m_objDisplay;
    property    FullName[value: TeFullNameMode]: String   read GetFullName;
  end;

  TcMetaData = class(TcCustomData)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcMetaData is the base meta object for reverse engineering.
  *
  * Inheritance:
  *   TcCustomData
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 08/20/00 Regis Created
  * 09/29/00 Regis Added Dependencies
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objConnection: TcConnection;
    m_lstXMLDependencies: TStringList; // Set from the XML & Object Computed from value when reading the XML
    m_lstErrors: TcCollection;
    m_objMTOK: TcMetaData;
    m_eFlags: TeMetaFlagSet;
    m_sItem: String;
    m_sAlias: String;
    m_iPosition: longint;
    m_hdlQuery: TcMetaData_ExpressionQuery;
    m_objRS: TcObject;
    m_sFilter: String;
    m_objPreferences: TcMetaDataPreferences;
    m_lstMetaDataIndex: TStringList;
    m_objSections: TcObject;
    m_sDisplayName: String;
    m_eIcon: TeIcon;
    m_lstIndex: TStringList;
    m_objOrigin: TcMetaData;

  private
    // Private declarations
    //
    function    GetError(Level: TeLevel): String;
    procedure   LocalClear;
    function    GetDescription: String;
    function    GetScriptTypes: TStringList;
    function    GetAttribute(key: String; Index: Integer): String;
    procedure   SetAttribute(key: String; Index: Integer; value: String);
    function    GetHasAttribute(key: String): boolean;
    procedure   MetaRelationships;
    function    GetConnection: TcConnection;
    procedure   SetConnection(const value: TcConnection);
    function    MTOK: TcMetaData;
    function    GetHasFeature(index: TeForm): boolean;
    function    GetIncludeSection(value: String): boolean;
    function    GetOption(Index: String): String;
    procedure   SetOption(Index: String; value: String);
    function    FieldValue(value: String): String;
    procedure   SetHdlQuery(value: TcMetaData_ExpressionQuery);
    function    GetHdlQuery: TcMetaData_ExpressionQuery;
    function    GetPreferences: TcMetaDataPreferences;
    procedure   SetPreferences(const value: TcMetaDataPreferences);
    procedure   SetFastIndex(value: TcObject; parAction: TeIndexAction);
    function    GetSections: TcObject;
    function    GetGraphObject: TcMetaData;
    function    GetDisplayName: String;
    function    GetCanHaveChildren: boolean;
    function    GetHasDisplayName: boolean;
    function    GetObjectID: String;
    function    GetSectionsAsText: String;
    procedure   SetSectionsAsText(value: String);
    function    GetEmptySectionsAsText: String;
    procedure   ClearScripts;
    procedure   CheckMetaErrors;

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;
    procedure   SetName(value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear the object
    procedure   Copy(value: TcObject); override;
    //   Compare
    //   Text
    function    Load(value: String): boolean; overload; virtual;
    //   Save
    function    LoadFromStream(parStream: TcTokenStream): boolean; override;     // Serialization (From)
    procedure   SaveToStream(parStream: TcTokenStream); override;                // Serialization (To)
    //
    // 2. Custom
    function    ReverseEngineer(parParent: TcData; parDepth: longint; parArray: array of TcValuePair): boolean;
    function    Expression(value: String; parData: TObject; parMetaData: TcMetaData; sparSubType: String; parArray: array of TcValuePair; parBooleanExpr: boolean = FALSE): Variant;
    function    Statement(parData: TObject; eparSet: TeTypeSet; sparSubType: String): String;
    function    Find(parType: TeTypeSet; value: String): TcObject; overload;
    function    Find(parType: TeTypeSet; value: String; parDepth: longint): TcObject; overload;
    function    Find(value: TeType; SubType: String): TcObject; overload;
    function    FindFast(value: String): TcObject; overload;
    function    Has(value: TeType; SubType: String): boolean; overload;
    function    Has(value: TeType; SubType: String; parDepth: longint): boolean; overload;
    function    Identifier(value: String): TcMetaData;
    function    SendToObject(ptrObject: TObject): TObject; override;
    function    ProcessSQL(sparType: String; parData: TcData): boolean;
    function    Refresh(parParent: TcData): boolean;
    function    Translate(value: String): String;
    function    ApplyTransformation(value: String): string;
    function    DisplayErrors(parListView: TListView): boolean;
    procedure   SetError(Level: TeLevel; value, sSQL: String);
    function    Add(parObject: TcObject): longint; override;
    function    Delete(item: longint): longint; override;
    function    Delete(parObject: TcObject): longint; override;
    function    IsChildOf(value: TeTypeSet): boolean;
    function    CountData(value: TcObject): longint;
    procedure   SortByDependency(var parList: TcBag);
    function    OnlineHelpStr: String;

  public
    // Public Properties
    //
    property    Connection: TcConnection                  read GetConnection    write SetConnection;
    property    Attribute[value: String]: String index 0  read GetAttribute     write SetAttribute;
    property    Properties[value: String]: String index 1 read GetAttribute     write SetAttribute;
    property    HasAttribute[value: String]: boolean      read GetHasAttribute;
    property    Error[Level: TeLevel]: String             read GetError;
    property    Description: String                       read GetDescription;
    property    ScriptTypes: TStringList                  read GetScriptTypes;
    property    HasFeature[index: TeForm]: boolean        read GetHasFeature;
    property    eFlags: TeMetaFlagSet                     read m_eFlags;
    property    sItem: String                             read m_sItem;
    property    sAlias: String                            read m_sAlias;
    property    Option[Index: String]: String             read GetOption        write SetOption;
    property    hdlQuery: TcMetaData_ExpressionQuery      read GetHdlQuery      write SetHdlQuery;
    property    iPosition: longint                        read m_iPosition;
    property    sFilter: String                           read m_sFilter        write m_sFilter;
    property    Preferences: TcMetaDataPreferences        read GetPreferences   write SetPreferences;
    property    Sections: TcObject                        read GetSections;
    property    GraphObject: TcMetaData                   read GetGraphObject;
    property    DisplayName: String                       read GetDisplayName   write m_sDisplayName;
    property    CanHaveChildren: boolean                  read GetCanHaveChildren;
    property    HasDisplayName: boolean                   read GetHasDisplayName;
    property    eIcon: TeIcon                             read m_eIcon;
    property    MetaDependencies: TStringList             read m_lstXMLDependencies;
    property    ObjectID: string                          read GetObjectID;
    property    SectionsAsText: String                    read GetSectionsAsText write SetSectionsAsText;
    property    EmptySectionsAsText: string               read GetEmptySectionsAsText;
    property    lstIndex: TStringList                     read m_lstIndex;
    property    Origin: TcMetaData                        read m_objOrigin      write m_objOrigin;
  end;

  TcDataDependency = class(TcObject)
  private
    // Private members
    //
    m_iDepTag: longint;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public Properties
    //
    property    iDepTag: longint                          read m_iDepTag        write m_iDepTag;
  end;

  TcData = class(TcCustomData)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcData is the base object for reverse engineering.
  *
  * Inheritance:
  *   TcCustomData
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
    m_objMetaData: TcMetaData;
    m_procDisplay: ThdlDataDisplay;
    m_depFrom: TcCollection; // Main dependency pointers
    m_depTo: TcCollection;   // Computed dependencies
    m_lstRuleProcessing: TcCollection;
    m_objAncestor: TcObject;
    m_lstScripts: TcCollection;
    // Graph-related members
    m_X, m_Y: longint;
    m_eState: TeState;
    m_sPreviousValue: String;
    m_iAlertLevel: longint;
    m_bIsLoaded: boolean;

  protected
    // protected declarations
    //
    function    GetDisplay: TObject; override;
    function    GetFullName(value: TeFullNameMode): String; override;

  private
    // Private declarations
    //
    function    GetDependencyText(value: TeDirection; parFormat: TeOutputType): String;
    function    GetDependencyHeader: String;
    function    GetRuleErrorLevel: TeRuleErrorLevel;
    function    GetHeader: String;
    function    GetIsHeader: boolean;
    procedure   SetMetaData(Value: TcMetaData);
    function    GetHeaderObject: TcData;
    procedure   SetState(value: TeState);
    function    GetState: TeState;
    function    IsChildOf(value: TcObject): boolean;
    function    GetObjectID: String;
    procedure   SetObjectID(value: String);
    function    AddDependency(parObject: TcObject; parDirection: TeDirection): longint; overload;
    function    AddDependency(sparType, sparObject: String; iparTag: longint; parDirection: TeDirection): longint; overload;
    function    GetArguments: TcValuePairSet;
    function    GetHint: String;
    function    GetIcon: longint;
    function    GetIsLoaded: boolean;
    function    GetUser: TcData;
    procedure   SetAlertLevel(value: longint);
    function    ReverseEngineer(parMetaData: TcMetaData; parArray: array of TcValuePair; parSet: TeTypeSet; parFlags: TeLoadFlagSet): boolean; overload;
    function    GetScript(value: String): String;
    procedure   ClearScripts;
    function    AddScript(sType, value: String): longint;
    function    GetScriptHeader: String;
    function    GetHTML: String;
    function    GetValueByMeta(value: TcMetaData): String;

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;
    procedure   SetDependencies;
    procedure   SetValue(value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    //   Clear
    procedure   Copy(value: TcObject); override;
    //   Compare
    //   Text
    function    Load(parMetaData: TcMetaData; parFlags: TeLoadFlagSet): boolean; override;
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    function    Statement(eparSet: TeTypeSet; sparType: String): String;
    function    Find(eType: TeType; TypeValue: String): TcObject; overload;
    function    Find(eType: TeType; TypeValue: String; parDepth: longint): TcObject; overload;
    procedure   Display;
    function    SetRuleResult(objRule: TcMetaData; Value: String): TcRuleResult;
    procedure   ClearRuleResult;
    procedure   Translate(value: TcData);
    function    ProcessSQL(eparSet: TeTypeSet; sparType: String): boolean;
    function    IdentifierValue(sIdentifier, sparSubType: String): String;
    function    Child(value: String): TcObject; overload; override;
    function    Child(parObject: TcObject): TcObject; overload; override;
    procedure   CascadeValue(strMetaName, strValue: String);
    procedure   Retrofit(parObject: TcData);
    procedure   DescendAncestorValues;
    procedure   RemoveDependency;
    function    GetGraphXML: String;
    function    Add(parObject: TcObject): longint; override;
    procedure   ClearChildren; override;
    procedure   ClearChildren(eparType: TeType; value: String); overload; virtual;
    function    FindValue(sparType, sparValue: String; iparDepth: longint): TcObject; override;
    function    CompareSchemas(value: TcData; parPlaceHolders: TStringList; parComparisonSet: TeDifferenceSet; parFilter: TcBag): TcObject;
    function    XML_Filtered(parFilter: TcBag; parReplacements: TStringList): String; override;
    procedure   UpdateIcon;
    function    FindInstance(parMeta: TcMetaData; parValue: String): TcData;
    function    HasAllLoaded(parMetaData: TcMetaData): boolean;

  public
    // Public Properties
    //
    property    MetaData: TcMetaData                      read m_objMetaData    write SetMetaData;
    property    procDisplay: ThdlDataDisplay                                    write m_procDisplay;
    property    FromDependency: TcCollection              read m_depFrom;
    property    ToDependency: TcCollection                read m_depTo;
    property    Header: String                            read GetHeader;
    property    IsHeader: boolean                         read GetIsHeader;
    property    HeaderObject: TcData                      read GetHeaderObject;
    property    ObjectID: string                          read GetObjectID      write SetObjectID;
    property    DependencyHeader: String                  read GetDependencyHeader;
    // Graph-related members
    property    X: longint                                read m_X              write m_X;
    property    Y: longint                                read m_Y              write m_Y;
    property    State: TeState                            read GetState         write SetState;
    property    sPreviousValue: String                    read m_sPreviousValue;
    property    ErrorLevel: TeRuleErrorLevel              read GetRuleErrorLevel;
    property    lstRuleProcessing: TcCollection           read m_lstRuleProcessing;
    property    GraphXML: String                          read GetGraphXML;
    property    iAlertLevel: longint                      read m_iAlertLevel    write SetAlertLevel;
    property    IsLoaded: boolean                         read GetIsLoaded      write m_bIsLoaded;
    property    Arguments: TcValuePairSet                 read GetArguments;
    property    Hint: String                              read GetHint;
    property    Icon: longint                             read GetIcon;
    property    eState: TeState                           read m_eState         write m_eState;
    property    Script[value: String]: String             read GetScript;
    property    ScriptHeader: String                      read GetScriptHeader;
    property    HTML: String                              read GetHTML;
  end;

  TeDataDifferencePart = (ediSQL, ediHeader, ediDetails, ediItemHeader, ediDescription);

  TcDataDifference = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcDataDifference describes data structure diffrences, group by Object.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/27/01 Regis Created
  *
  ******************************************************************************}
  private
    // Private Methods
    //
    function    GetDifference: TeDifference;
    function    GetPart(Index: TeDataDifferencePart): String;

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom
    function    SendToObject(value: TObject): TObject; override;

  public
    // Public Properties
    //
    property    eDifference: TeDifference                 read GetDifference;
    property    Part[value: TeDataDifferencePart]: String read GetPart;
  end;

  TcDataDifferenceItem = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcDataDifferenceItem describes data structure diffrences.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 06/20/06 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_eDifference: TeDifference;
    m_objReference: TcObject;
    m_lstTarget: TcBag;
    m_sSQL: String;

  private
    // Private Methods
    //
    function    GetPart(Index: TeDataDifferencePart): String;
    procedure   SetPart(Index: TeDataDifferencePart; value: String);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom

  public
    // Public Properties
    //
    property    eDifference: TeDifference                 read m_eDifference    write m_eDifference;
    property    Reference: TcObject                       read m_objReference   write m_objReference;
    property    Part[index: TeDataDifferencePart]: String read GetPart          write SetPart;
  end;

  TcRuleResult = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcRuleResult describe a rule processing output after reverse
  *              engineering.
  *
  * Inheritance:
  *   TObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/06/01 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_eLevel: TeRuleErrorLevel;
    m_objRule: TcMetaData;

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
    //   Load
    //   Save
    //
    // 2. Custom

  public
    // Public Properties
    //
    property    eLevel: TeRuleErrorLevel              read m_eLevel        write m_eLevel;
    property    Rule: TcMetaData                      read m_objRule       write m_objRule;
  end;

  TcMetaDataPreferences = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcMetaDataPreferences is the class for managing preferences for a script
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 04/24/04 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    function    GetAsBoolean(index: String): boolean;
    procedure   SetAsBoolean(index: String; value: boolean);

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    function    Load: boolean;
    function    Save: boolean;
    //
    // 2. Custom
    function    Has(value: String): boolean; overload; virtual;

  public
    // Public Properties
    //
    property    AsBoolean[index: string]: boolean     read GetAsBoolean  write SetAsBoolean;
  end;

implementation

uses
  Windows,
  Controls,
  Variants,
  Forms,
  strUtils,
  ActiveX,
  comObj,
  ExecuteLib,
  sysUtils,
  Registry,
  daResourceStrings,
  oleCtnrs,
  BlockCiphers,
  SourceListBox;

// Tool
//   GetArgumentValue
//
function GetArgumentValue(parArray: array of TcValuePair; value: String): String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := low(parArray) to high(parArray) do
    if AnsiCompareStr(parArray[i].name, value) = 0 then
    begin
      result := parArray[i].value;
      break;
    end;
end;

//
// TcCustomData
//

// TcCustomData
//   Constructor
//
constructor TcCustomData.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objDisplay := nil;
end;

// TcCustomData
//   GetDisplay
//
function TcCustomData.GetDisplay: TObject;
begin
  result := m_objDisplay;
end;

// TcCustomData
//   ClearDisplayRefefence method
//
procedure TcCustomData.ClearDisplayRefefence;
var
  i: longint;
begin
  m_objDisplay := nil;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcCustomData) then
      (Objects[i] as TcCustomData).ClearDisplayRefefence;
end;

// TcCustomData
//   StreamObject
//
function TcCustomData.StreamObject(value: TeObjectMarker): TcObject;
begin
  case value of
    eomMetaData:
      result := TcMetaData.Create(self);
    eomData:
      result := TcData.Create(self);
  else
    raise Exception.Create(Format(krsUNKNOWNOBJECT, [longint(value)]));
  end;
end;

// TcCustomData
//   CountType method
//
function TcCustomData.CountType(value: TeType): longint;
var
  i: longint;
begin
  result := 0;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcObject) and (Objects[i].eType = value) then
      inc(result);
end;

// TcCustomData
//   ParentObject
//
function TcCustomData.ParentObject(eparSet: TeTypeSet): TcCustomData;
begin
  result := self;
  while (result <> nil) and (result.Parent <> nil) and not (result.eType in eparSet) do
    result := result.Parent as TcCustomData;
end;

// TcCustomData
//   GetFullName method
//
function TcCustomData.GetFullName(value: TeFullNameMode): String;
begin
  result := ksEMPTY;
  if (parent <> nil) and (Parent is TcCustomData) then
    result := (Parent as TcCustomData).GetFullName(value);
  if (result <> ksEMPTY) and (sName <> ksEMPTY) then
    result := result + '.';
  result := result + sName;
end;

// TcCustomData
//   Load method
//
function TcCustomData.Load(parMetaData: TcMetaData; parFlags: TeLoadFlagSet): boolean;
begin
  result := FALSE;
end;

//
// TcMetaData
//

// TcMetaData
//   Constructor
//
constructor TcMetaData.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objConnection := nil;
  m_objPreferences := nil;
  m_lstXMLDependencies := TStringList.Create;
  m_lstErrors := nil;
  m_objMTOK := nil;
  m_lstMetaDataIndex := TStringList.Create;
  m_lstMetaDataIndex.CaseSensitive := FALSE;
  m_lstMetaDataIndex.Sorted := FALSE;
  m_objSections := nil;
  m_objDisplay := nil;
  m_lstIndex := TStringList.Create;
  m_lstIndex.Sorted := TRUE;
  m_lstIndex.CaseSensitive := TRUE;
  m_lstIndex.Duplicates := dupAccept;
  m_objOrigin := nil;
  LocalClear;
end;

// TcMetaData
//   Destructor
//
destructor TcMetaData.Destroy;
begin
  LocalClear;
  m_lstXMLDependencies.free;
  m_lstErrors.free;
  m_objPreferences := nil;
  m_lstMetaDataIndex.Free;
  m_lstMetaDataIndex := nil;
  m_objSections.free;
  m_objSections := nil;
  m_lstIndex.Free;
  inherited Destroy;
end;

// TcMetaData
//   Clear
//
procedure TcMetaData.Clear;
begin
  inherited Clear;
  LocalClear;
end;

// TcMetaData
//   LocalClear
//
procedure TcMetaData.LocalClear;
begin
  m_lstXMLDependencies.Clear;
  if m_lstErrors <> nil then
    m_lstErrors.Clear;
  m_eFlags := [];
  m_sItem := ksEMPTY;
  m_sAlias := ksEMPTY;
  m_hdlQuery := nil;
  m_objRS := nil;
  m_iPosition := kiUNDEFINED;
  m_sFilter := ksEMPTY;
  m_lstMetaDataIndex.Clear;
  m_objDisplay := nil;
  m_sDisplayName := ksEMPTY;
  m_eIcon := eiUndefined;
  m_lstIndex.Clear;
end;

// TcMetaData
//   Copy
//
procedure TcMetaData.Copy(value: TcObject);
begin
  inherited Copy(value);
  if value is TcMetaData then
  begin
    m_objConnection     := (value as TcMetaData).GetConnection;
    m_lstXMLDependencies.Assign((value as TcMetaData).m_lstXMLDependencies);
    m_eFlags            := (value as TcMetaData).m_eFlags;
    m_sItem             := (value as TcMetaData).m_sItem;
    m_sAlias            := (value as TcMetaData).m_sAlias;
    m_iPosition         := (value as TcMetaData).m_iPosition;
    m_sFilter           := (value as TcMetaData).m_sFilter;
    m_objDisplay        := (value as TcMetaData).m_objDisplay;
    m_sDisplayName      := (value as TcMetaData).m_sDisplayName;
    m_eIcon             := (value as TcMetaData).m_eIcon;
    m_objOrigin         := value as TcMetaData;
    if (value as TcMetaData).m_lstErrors <> nil then
    begin
      if m_lstErrors = nil then
        m_lstErrors := TcCollection.Create(self);
      m_lstErrors.Copy((value as TcMetaData).m_lstErrors);
    end;
    if self = TopParent then
      MetaRelationships;
    SetFastIndex(value, eiaAdd);
  end;
end;

// TcMetaData
//   Load method
//
function TcMetaData.Load(value: String): boolean;
var
  s: String;
  strm: TcTokenStream;
begin
  Clear;
  result := FALSE;
  // Should we load from binary?
  (*
  s := value;
  while (s <> ksEMPTY) and (s[length(s)] <> '.') do
    system.delete(s, length(s), 1);
  if FileExists(s + 'bin') and (FileAge(s + 'bin') > FileAge(value)) then
  begin
    strm := nil;
    try
      strm := TcTokenStream.Create;
      strm.LoadFromFile(s + 'bin');
      result := LoadFromStream(strm);
      if result then
        MetaRelationships;
    finally
      strm.Free;
    end;
  end;
  *)
  // Binary did not exists: Load from XML
  if not result and FileExists(value) then
  begin
    // Load from XML
    SetXML(FileToString(value, ecsAnsi));
    // Spool binary to file
    strm := nil;
    try
      strm := TcTokenStream.Create;
      SaveToStream(strm);
      strm.SaveToFile(s + 'bin');
    finally
      strm.Free;
    end;
    result := TRUE;
  end;
end;

// TcMetaData
//   LoadFromStream method
//
function TcMetaData.LoadFromStream(parStream: TcTokenStream): boolean;
var
  L, i: longint;
  Level: TeLevel;
  sError, sSQL: String;
begin
  with parStream do
  begin
    MatchMarker(emStart, eomMetaData);
    // Inherited Part
    inherited LoadFromStream(parStream);
    // Discrete members
    m_lstXMLDependencies.Text := AsString;
    m_eFlags            := StringToMetaFlags(AsString);
    m_sItem             := AsString;
    m_sAlias            := AsString;
    m_iPosition         := AsInteger;
    m_sFilter           := AsString;
    m_sDisplayName      := AsString;
    m_eIcon             := TeIcon(AsInteger);
    // Errors
    L := AsInteger;
    for i := 0 to L - 1 do
    begin
      Level := TeLevel(AsInteger);
      sError := AsString;
      sSQL := AsString;
      SetError(Level, sError, sSQL);
    end;

    // Huh?
    // m_objPreferences: TcMetaDataPreferences;
    // m_objSections: TcObject;
    // m_lstMetaDataIndex: TStringList;

    // End of loading TcMetaNode object
    MatchMarker(emEnd, eomMetaData);
  end;
  result := TRUE;
end;

// TcMetaData
//   SaveToStream method
//
procedure TcMetaData.SaveToStream(parStream: TcTokenStream);
var
  i: longint;
begin
  with parStream do
  begin
    Marker[emStart] := eomMetaData;
    // Inherited Part
    inherited SaveToStream(parStream);
    // Discrete members
    AsString  := m_lstXMLDependencies.Text;
    AsString  := MetaFlagsToString(m_eFlags);
    AsString  := m_sItem;
    AsString  := m_sAlias;
    AsInteger := m_iPosition;
    AsString  := m_sFilter;
    AsString  := m_sDisplayName;
    AsInteger := longint(m_eIcon);
    // Errors
    if m_lstErrors <> nil then
    begin
      AsInteger := m_lstErrors.Count;
      for i := 0 to m_lstErrors.Count - 1 do
      begin
        AsInteger := m_lstErrors[i].iCode; // Level
        AsString := m_lstErrors[i].sName; // Error
        AsString := m_lstErrors[i].sValue; // SQL
      end;
    end
    else
      AsInteger := 0;
    // End of loading vControl object
    Marker[emEnd] := eomMetaData;
  end;
end;

// TcMetaData
//   SetXML method
//
procedure TcMetaData.SetXML(value: String);

  //
  // Recursively, Load all meta data elements from the XML.
  procedure SubLoad(pObject: TcMetaData; pOLE: OLEVariant);
  var
    i, j: longint;
    p: TcMetaData;
    s: string;
  begin
    pObject.sName := ksEMPTY;
    pObject.eType := StringToType(pOLE.nodeName);
    //
    // Attributes..?
    if pOLE.nodeType = ntNODE_ELEMENT then // NODE_ELEMENT only
      for i := 0 to pOLE.attributes.length - 1 do
        if AnsiCompareText(pOLE.attributes.item[i].name, krsNAME) = 0 then
          pObject.sName := vartostr(pOLE.attributes.item[i].value)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsERROR) = 0 then
          pObject.SetError(elWarning, XMLToText(pOLE.attributes.item[i].value), ksEMPTY)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsFLAGS) = 0 then
          pObject.m_eFlags := StringToMetaFlags(pOLE.attributes.item[i].value)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsFILTER) = 0 then
          pObject.m_sFilter := vartostr(pOLE.attributes.item[i].value)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsITEM) = 0 then
          pObject.m_sItem := vartostr(pOLE.attributes.item[i].value)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsALIAS) = 0 then
          pObject.m_sAlias := vartostr(pOLE.attributes.item[i].value)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsPOS) = 0 then
          pObject.m_iPosition := StrtoIntdef(vartostr(pOLE.attributes.item[i].value), kiUNDEFINED)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsDISPLAYNAME) = 0 then
          pObject.DisplayName := vartostr(pOLE.attributes.item[i].value)
        else if AnsiCompareText(pOLE.attributes.item[i].name, krsDEPENDENCY) = 0 then
        begin
          s := vartostr(pOLE.attributes.item[i].value);
          if (s <> ksEMPTY) and (AnsiCompareText(system.copy(s, 1, length(krsSELECT)), krsSELECT) <> 0) then
            for j := 0 to ItemCount(s, ',') - 1 do
              pObject.m_lstXMLDependencies.Add(Item(s, ',', j))
          else if s <> ksEMPTY then
            pObject.m_lstXMLDependencies.Add(s);
        end
        else
          pObject.Attribute[pOLE.attributes.item[i].name] := vartostr(pOLE.attributes.item[i].value);
    //
    // Object Icon
    if (pObject.eType = enObject) and (pObject.sName <> ksEMPTY) then
      pObject.m_eIcon := StringToIcon(pObject.sName);
    //
    // Child nodes..?
    for i := 0 to pOLE.childNodes.length - 1 do
      if pOLE.childNodes.item[i].nodeType = ntNODE_TEXT then
        pObject.sValue := pObject.sValue + XMLToText(VarToStr(pOLE.childNodes.item[i].nodeValue))
      else
      begin
        p := TcMetaData.Create(pObject);
        pObject.Add(p);
        SubLoad(p, pOLE.childNodes.item[i]);
      end;
    //
    // Option?
    if pObject.eType = enOption then
      SetOption(pObject.sName, pObject.sValue);
  end;

var
  p, xPE: OLEVariant;
begin
  if value <> ksEMPTY then
  try
    CoInitialize(nil);
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) then
      begin
        if p.childNodes.Length > 0 then
          SubLoad(self, p.childNodes.item[0]);
        MetaRelationships;
        CheckMetaErrors;
      end
      else
      begin
        xPE := p.parseError;
        if xPE.errorCode <> 0 then
          raise Exception.Create(
            'XML Meta Document failed to load.' + ksCR +
            Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xPE.Line), VarToStr(xPE.linepos), VarToStr(xPE.filepos), VarToStr(xPE.errorCode), xPE.reason]) + ksCR +
            trim(xPE.srcText));
      end;
      p := unassigned;
    except
      on E: Exception do
      begin
        Clear;
        SetError(elFatal, E.Message, value);
  {$IFNDEF FACTORY}
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
  {$ENDIF}
        p := unassigned;
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

// TcMetaData
//   GetXML method
//
function TcMetaData.GetXML: String;

  function TextToText(value: String): string;
  begin
    // pre-sanitize
    result := AnsiReplaceStr(value, ksCR, '<br>');
    result := AnsiReplaceStr(result, '\n', '<br>');
    result := AnsiReplaceStr(result, '\t', '  ');
  end;

  function SubXML(Node: TcMetaData; Depth: longint): String;
  var
    s, t: string;
    i, j: longint;
  begin
    // 1. Get children XML
    s := ksEMPTY;
    for i := 0 to Node.count - 1 do
      if Node[i].eType <> enAttribute then
        s := s + SubXML(Node[i] as TcMetaData, Depth + 1);
    // 2.a Name & Dependency Attributes
    result := Format('%s&lt;%s', [RepeatStr(Depth, '  '), kasTYPE[Node.eType]]);
    if Node.sName <> ksEMPTY then
      result := result + Format(' %s="%s"', [krsNAME, Node.sName]);
    if Node.eFlags <> [] then
      result := result + Format(' %s="%s"', [krsFLAGS, MetaFlagsToString(Node.eFlags)]);
    if Node.sFilter <> ksEMPTY then
      result := result + Format(' %s="%s"', [krsFILTER, Node.sFilter]);
    if Node.sItem <> ksEMPTY then
      result := result + Format(' %s="%s"', [krsITEM, Node.sItem]);
    if Node.sAlias <> ksEMPTY then
      result := result + Format(' %s="%s"', [krsALIAS, Node.sAlias]);
    if Node.m_lstXMLDependencies.Count > 0 then
    begin
      t := ksEMPTY;
      for j := 0 to Node.m_lstXMLDependencies.Count - 1 do
      begin
        if t <> ksEMPTY then
          t := t + ',';
        t := t + Node.m_lstXMLDependencies[j];
      end;
      result := result + Format(' %s="%s"', [krsDEPENDENCY, t]);
    end;
    // 2.b Attributes
    for i := 0 to Node.Count - 1 do
      if (Node[i] <> nil) and (Node[i].eType = enAttribute) then
        result := result + Format(' %s="%s"', [Node[i].sName, Node[i].sValue]);
    // 3. Put everything together
    if s = ksEMPTY then
    begin
      if (Node.sValue = ksEMPTY) then
        result := result + '/&gt;<br>'
      else
        result := result + Format('&gt;%s&lt;/%s&gt;', [TextToText(Node.sValue), kasTYPE[Node.eType]]) + '<br>'
    end
    else
    begin
      if (Node.sValue = ksEMPTY) then
        result := result + Format('&gt;%s%s&lt;/%s&gt;', ['<br>' + s, RepeatStr(Depth, '  '), kasTYPE[Node.eType]]) + '<br>'
      else
        result := result + Format('&gt;%s%s%s&lt;/%s&gt;', [TextToText(Node.sValue), '<br>' + s, RepeatStr(Depth, '  '), kasTYPE[Node.eType]]) + '<br>'
    end;
  end;

begin
  result := SubXML(self, 0);
end;

// TcMetaData
//   Expression
//
//     expr         -> term { addop term } ['^' factor]
//     term         -> factor { mulop factor }
//     factor       -> number |
//                     string |
//                     '[' bexpr ['?' expr [':' expr] ] ']' |
//                     '(' expr { expr } ')' |
//                     '{' string {string} '}' |
//                     'upper' '(' expr ')' |
//                     'lower' '(' expr ')'
//                     'copy' '(' expr ',' expr ',' expr ')' |
//                     'has' '(' expr ',' expr ')' |
//                     'chr' '(' expr ')'
//     addop        -> '+' | '-'
//     mulop        -> '*' | '/'
//
//     bexpr        -> bterm { 'or' bterm }
//     bterm        -> bfactor { 'and' bfactor }
//     bfactor      -> '(' bexpr ')' |
//                     string { '.' string } |
//                     expr [ relop expr | 'in' '(' expr { ',' expr } ')'] |
//                     'not' expr
//     relop        -> '<' | '<=' | '<>' | '>' | '>=' | '='
//
function TcMetaData.Expression(value: String; parData: TObject; parMetaData: TcMetaData; sparSubType: String; parArray: array of TcValuePair; parBooleanExpr: boolean = FALSE): Variant;

  // ConvertAsDouble
  //   Make sure the variant input is a number. If not, make it 0.0 and also
  //   handle conversion exceptions
  function ConvertAsDouble(value: Variant): Variant;
  begin
    try
      if VarToStr(value) <> ksEMPTY then
        result := VarAsType(value, varDouble)
      else
        result := 0.0;
    except
      result := 0.0;
    end;
  end;

  function bexpr(parStream: TcTokenStream): Variant; forward;
  function expr(parStream: TcTokenStream): Variant; forward;

  // relop   -> '<' | '<=' | '<>' | '>' | '>=' | '=' | 'LIKE'
  function IsRelOp(parKeyword: TvKeyword): boolean;
  begin
    result := parKeyword in [_EQUAL, _LOWER, _LOWEREQUAL, _NOTEQUAL, _GREATER, _GREATEREQUAL, _LIKE, _IS];
  end;

  // BooleanCompare
  function BooleanCompare(varA, varB: Variant; parOperator: TvKeyword): boolean;

    function VariantToNumeric(value: Variant): real;
    var
      v: real;
      L: longint;
    begin
      result := 0.0;
      try
        val(VarToStr(value), v, L);
        result := v;
      except
        //
      end;
    end;

    function CheckIfNumber(value: Variant): boolean;
    begin
      result := IsNumber(VarToStr(value)) or IsReal(VarToStr(value));
    end;

  begin
    result := FALSE;
    try
      // Check compatibility of var types.
      if VarIsNull(varA) or VarIsEmpty(varA) then
        varA := ksEMPTY;
      if VarIsNull(varB) or VarIsEmpty(varB) then
        varB := ksEMPTY;
      if VarType(varA) <> varType(varB) then
      begin
        if CheckIfNumber(varA) and (VarToStr(varB) = ksEMPTY) then
          varB := 0.0
        else if CheckIfNumber(varB) and (VarToStr(varA) = ksEMPTY) then
          varA := 0.0;
      end
      // Check if variants are both numbers then Convert them to numerics.
      else if (VarType(varA) = varType(varB)) and CheckIfNumber(varA) and CheckIfNumber(varB) then
      begin
        varA := VariantToNumeric(varA);
        varB := VariantToNumeric(varB);
      end;
      // Proceed to comparison
      case parOperator of
        _EQUAL:
          result := varA = varB;
        _LOWER:
          result := varA < varB;
        _LOWEREQUAL:
          result := varA <= varB;
        _NOTEQUAL:
          result := varA <> varB;
        _GREATER:
          result := varA > varB;
        _GREATEREQUAL:
          result := varA >= varB;
      end;
    except
      raise;
    end;
  end;

  // Tool
  //   GetIdentifier
  //
  function GetIdentifier(parObject: TcMetaData; parStream: TcTokenStream): TcMetaData;
  begin
    result := nil;
    if parObject <> nil then
      with parStream do
      begin
        result := parObject.Find([enObject, enItem], Token.Value) as TcMetaData;
        Match(Token.ID);
        while (Token.ID = _PERIOD) and not EOS do // Is there more following..?
        begin
          Match(_PERIOD);
          if result <> nil then
            result := result.Find([enObject, enItem, enField], Token.Value) as TcMetaData;
          Match(_STRING);
        end;
      end;
  end;

  // bfactor -> '(' bexpr ')' |
  //            string { '.' string } |
  //            expr [ relop expr | 'in' '(' expr { ',' expr } ')'] |
  //            'not' expr
  function bfactor(parStream: TcTokenStream): Variant;
  var
    v: Variant;
    op: TvKeyword;
    hdl: TcMetaData_ExpressionQuery;
    s: String;
    sOperator: String;
    p: TcData;
    e: TeState;
  begin
    result := ksEMPTY;
    with parStream do
      case Token.ID of
        _NOT:
          begin
            Match(_NOT);
            result := not VariantToBool(bexpr(parStream));
          end;
        _LPAREN: // '(' bexpr ')'
          begin
            Match(_LPAREN);
            result := VariantToBool(bexpr(parStream));
            Match(_RPAREN);
          end;
        _STRING:
          begin
            sOperator := ksEMPTY;
            result := Token.Value;
            Match(Token.ID);
            while (Token.ID = _PERIOD) and not EOS do // Is there more following..?
            begin
              Match(_PERIOD);
              result := result + _TOKENS[_PERIOD] + uppercase(Token.Value);
              Match(_STRING);
            end;
            if Token.ID = _OPERATOR then
            begin
              Match(_OPERATOR);
              sOperator := Token.Value;
              Match(_WILDCARD);
            end;
            // a. Dependencies?
            if (result = krsMACRODEPENDENCIES) and (parData <> nil) then
            begin
              result := ksEMPTY;
              if (parData <> nil) and (parData is TcData) then
                result := ((parData as TcData)).GetDependencyHeader;
            end
            // b. Section?
            else if result = krsMACROSECTION then
              result := parMetaData.GetIncludeSection(sOperator)
            // c. String starting with a '$'
            else if (result <> ksEMPTY) and (system.copy(result, 1, 1) = '$') then
              result := GetArgumentValue(parArray, result)
            // d.Expression?
            else
            begin
              //
              // A. Operator applied to Identifier?
              if sOperator <> ksEMPTY then
              begin
                p := nil;
                if (parData <> nil) and (parData is TcData) then
                  p := (parData as TcData).Child(result) as TcData;
                e := StringToState(sOperator);
                if (e <> edsUndefined) and (p <> nil) and (p is TcData) then
                  result := p.State = e
                else if (AnsiCompareText(sOperator, krsPREVIOUS) = 0) and (p <> nil) and (p is TcData) then
                  result := p.sPreviousValue
                else if (p <> nil) and (p is TcData) and (parData <> nil) and (parData is TcData) then
                  result := (parData as TcData).IdentifierValue(result, sOperator);
              end
              else
              //
              // B. Identifier Value!
              try
                // Should we use a plugged query?
                hdl := HdlQuery;
                if Assigned(hdl) then
                  result := hdl(result)
                // or, Should we use regular Identifier processing?
                else if (parData <> nil) and (parData is TcData) then
                  result := (parData as TcData).IdentifierValue(result, sparSubType)
                // or, is it a string list?
                else if (parData <> nil) and (parData is TStringList) then
                  result := (parData as TStringList).Values[result];
              except
                on E: Exception do
                begin
                  SetError(elFatal, E.Message, result);
                  result := ksEMPTY;
                end;
              end;
            end;
          end;
        else
        begin
          result := expr(parStream);
          if IsRelOP(Token.ID) then // relop expr
          begin
            op := Token.ID;
            Match(Token.ID);
            v := expr(parStream);
            case op of
              _EQUAL, _LOWER, _LOWEREQUAL, _NOTEQUAL, _GREATER, _GREATEREQUAL:
                result := BooleanCompare(result, v, op);
              _LIKE:
              begin
                s := VarToStr(v);
                if length(s) > 0 then
                begin
                  if (s[1] = '%') then
                    result := system.copy(VarToStr(result), length(VarToStr(result)) - length(s) + 2, length(s) - 1) = system.copy(s, 2, length(s) - 1)
                  else if s[length(s)] = '%' then
                    result := system.copy(VarToStr(result), 1, length(s) - 1) = system.copy(s, 1, length(s) - 1)
                  else
                    result := system.copy(VarToStr(result), 1, length(s)) = s;
                end;
              end;
            end;
          end
          else if Token.ID = _IN then // 'in' '(' expr { ',' expr } ')'
          begin
            Match(_LPAREN);
            v := result = Expr(parStream);
            while (Token.ID = _COMMA) and not EOS do
            begin
              Match(_COMMA);
              v := v or (result = Expr(parStream));
            end;
            Match(_RPAREN);
            result := v;
          end;
        end;
      end;
  end;

  // bterm -> bfactor { 'and' bfactor }
  function bterm(parStream: TcTokenStream): Variant;
  begin
    with parStream do
    begin
      result := bFactor(parStream);
      while (Token.ID = _AND) and not EOS do
      begin
        Match(Token.ID);
        result := varianttobool(bfactor(parStream)) and varianttobool(result);
      end;
    end;
  end;

  // bexpr -> bterm { 'or' bterm }
  function bexpr(parStream: TcTokenStream): Variant;
  begin
    with parStream do
    begin
      result := bTerm(parStream);
      while (Token.ID = _OR) and not EOS do
      begin
        Match(Token.ID);
        result := varianttobool(bterm(parStream)) or varianttobool(result);
      end;
    end;
  end;

  // factor -> number |
  //           string |
  //           '[' bexpr ['?' expr [':' expr] ] ']' |
  //           '(' expr { expr } ')' |
  //           '{' string {string} '}' |
  //           'upper' '(' expr ')' |
  //           'lower' '(' expr ')'
  //           'copy' '(' expr ',' expr ',' expr ')' |
  //           'has' '(' expr ',' expr ')' |
  //           'pos' '(' expr ',' expr ')' |
  //           'chr' '(' expr ')'


  function factor(parStream: TcTokenStream): Variant;
  var
    a, b: variant;
    i, j: longint;
    s, t: String;
    p, q: TcObject;
  begin
    result := ksEMPTY;
    with parStream do
      case Token.ID of
        _NUMBER, _REAL, // number
        _STRING: // string, ID
          begin
            result := Token.Value;
            Match(Token.ID);
          end;
        _LPAREN: // '(' expr { expr } ')'
          begin
            Match(_LPAREN);
            while (Token.ID <> _RPAREN) and not EOS do
              result := result + VarToStr(expr(parStream));
            Match(_RPAREN);
          end;
        _LCURLY: // '{' string {string} '}'
          begin
            Match(_LCURLY);
            //
            // End Tag
            if Token.ID = _DIV then
            begin
              Match(_DIV);
              // A. ref
              if AnsiCompareText(Token.Value, 'help') = 0 then
                result := result + '</a>'
              // B. ref
              else if AnsiCompareText(Token.Value, 'comment') = 0 then
                result := result + '</span>';
              // C. ...
              Match(_STRING);
            end
            else
            //
            // Start Tag
            begin
              // A. ref
              if AnsiCompareText(Token.Value, 'help') = 0 then
              begin
                Match(_STRING);
                result := result + Format('<a href="%s">', [Token.Value]);
                Match(_STRING);
              end;
              // B. comment
              if AnsiCompareText(Token.Value, 'comment') = 0 then
              begin
                Match(_STRING);
                result := result + Format('<span class="comment">', [Token.Value]);
              end;
              // C. ...
            end;
            Match(_RCURLY);
          end;
        _LBRACKET: // '[' bexpr ['?' expr [':' expr] ] ']' [ ':' expr]
          begin
            Match(_LBRACKET);
            result := bexpr(parStream);
            if Token.ID = _QUESTIONMARK then // '?' expr [':' expr]
            begin
              Match(_QUESTIONMARK);
              // Get the TRUE term
              a := ksEMPTY;
              repeat
                a := a + VarToStr(expr(parStream));
              until (Token.ID in [_COLON, _RBRACKET]) or EOS;
              // Get the FALSE term
              b := ksEMPTY;
              if Token.ID = _COLON then
              begin
                Match(_COLON);
                repeat
                  b := b + VarToStr(expr(parStream));
                until (Token.ID = _RBRACKET) or EOS;
              end;
              if result then
                result := a
              else
                result := b;
            end;
            Match(_RBRACKET);
          end;
        __UPPER: // 'upper' '(' expr ')'
          begin
            Match(__UPPER);
            Match(_LPAREN);
            result := uppercase(vartostr(expr(parStream)));
            Match(_RPAREN);
          end;
        __LOWER: // 'lower' '(' expr ')'
          begin
            Match(__LOWER);
            Match(_LPAREN);
            result := lowercase(vartostr(expr(parStream)));
            Match(_RPAREN);
          end;
        __COPY: // 'copy' '(' expr ',' expr ',' expr ')'
          begin
            Match(__COPY);
            Match(_LPAREN);
            s := vartostr(expr(parStream));
            Match(_COMMA);
            i := strtointdef(vartostr(expr(parStream)), 0);
            Match(_COMMA);
            j := strtointdef(vartostr(expr(parStream)), 0);
            result := system.copy(s, i, j);
            Match(_RPAREN);
          end;
        __HAS: // 'has' '(' expr ',' expr ')'
          begin
            result := FALSE;
            Match(__HAS);
            Match(_LPAREN);
            Match(_LBRACKET);
            q := nil;
            p := nil;
            if (parData <> nil) and (parData is TcData) then
            begin
              p := (parData as TcData).GetUser;
              if (p <> nil) and (p is TcData) then
                q := GetIdentifier((p as TcData).MetaData, parStream);
            end;
            Match(_RBRACKET);
            Match(_COMMA);
            t := vartostr(expr(parStream));
            if (p <> nil) and (p is TcData) and (q <> nil) and (q is TcMetaData) then
              result := (p as TcData).FindInstance(q as TcMetaData, t) <> nil;
            Match(_RPAREN);
          end;
        __POS:  // 'pos' '(' expr ',' expr ')'
          begin
            Match(__POS);
            Match(_LPAREN);
            s := vartostr(expr(parStream));
            Match(_COMMA);
            t := vartostr(expr(parStream));
            result := pos(s, t);
            Match(_RPAREN);
          end;
        __CHR:  // 'chr' '(' expr ')'
          begin
            Match(__CHR);
            Match(_LPAREN);
            result := chr(strtointdef(vartostr(expr(parStream)), 32));
            Match(_RPAREN);
          end;
      else // Unexpected input!!
        raise Exception.Create(Error(Format(krsUNEXPECTEDTOKEN, [Token.Value])));
    end;
  end;

  // term -> factor { mulop factor }
  function term(parStream: TcTokenStream): variant;
  var
    k: TvKeyword;
    v: variant;
  begin
    with parStream do
    begin
      result := factor(parStream);
      while (Token.ID in [_MULT, _DIV]) and not EOS do
      begin
        // make sure first operant is number-like..
        result := ConvertAsDouble(result);
        // Process operator
        k := Token.ID;
        Match(Token.ID);
        case k of
          _MULT:
            result := result * ConvertAsDouble(factor(parStream));
          _DIV:
          begin
            v := ConvertAsDouble(factor(parStream));
            if v <> 0 then
              result := result / v
            else
              result := varNull;
          end;
        end;
      end;
    end;
  end;

  // expr -> term { addop term }
  function expr(parStream: TcTokenStream): variant;
  var
    k: TvKeyword;
    n: Variant;
  begin
    with parStream do
    begin
      result := term(parStream);
      while (Token.ID in [_ADD, _SUBSTRACT]) and not EOS do
      begin
        // make sure first operant is number-like..
        result := ConvertAsDouble(result);
        // Process operator
        k := Token.ID;
        Match(Token.ID);
        case k of
          _ADD:
            result := result + ConvertAsDouble(term(parStream));
          _SUBSTRACT:
            result := result - ConvertAsDouble(term(parStream));
        end;
      end;
      //
      // ['^' factor]
      if Token.ID = _CARET then
      begin
        Match(Token.ID);
        n := factor(parStream);
        if IsNumber(VarToStr(n)) then
        begin
          n := strtointdef(VarToStr(n), 0);
          if n <> 0 then
            result := pad(result, n);
        end;
      end;
    end;
  end;

var
  parStream: TcTokenStream;
begin
  result := ksEMPTY;
  //
  parStream := nil;
  try
    parStream := StringToStream(value);
    try
      while not parStream.EOS do
        case parBooleanExpr of
          FALSE:
            result := result + VarToStr(expr(parStream));
          TRUE:
            result := result + VarToStr(bexpr(parStream));
        end;
    except
      on E:Exception do
        SetError(elFatal, Format('''%s'', Expression failed: %s [%s]', [sName, E.Message, value]), ksEMPTY);
    end;
  finally
    parStream.free;
  end;
  // Is it a number
  if IsNumber(result) then
    result := strtointdef(vartostr(result), 0);
  //
  // Is it a rule result..?
  if (vartostr(result) <> ksEMPTY) and (parMetaData <> nil) and (parMetaData.eType = enRule) and (parData <> nil) and (parData is TcData) then
    (parData as TcData).SetRuleResult(parMetaData, vartostr(result));
end;

// TcMetaData
//   GetAttribute
//
function TcMetaData.GetAttribute(key: String; Index: Integer): String;
var
  i: longint;
const
  keaINDEX: array[0 .. 1] of TeType = (enAttribute, enProperty);
begin
  result := ksEMPTY;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and
       (Objects[i].eType = keaINDEX[Index]) and
       (AnsiCompareText(Objects[i].sName, key) = 0) then
    begin
      result := Objects[i].sValue;
      break;
    end;
end;

// TcMetaData
//   GetHasAttribute
//
function TcMetaData.GetHasAttribute(key: String): boolean;
var
  i: longint;
begin
  result := FALSE;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and
       (Objects[i].eType = enAttribute) then
      result := result or (AnsiCompareText(Objects[i].sName, key) = 0);
end;

// TcMetaData
//   SetAttribute
//
procedure TcMetaData.SetAttribute(key: String; Index: Integer; value: String);
var
  i: longint;
  p: TcObject;
const
  keaINDEX: array[0 .. 1] of TeType = (enAttribute, enProperty);
begin
  p := nil;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and
       (Objects[i].eType = keaINDEX[Index]) and
       (AnsiCompareText(Objects[i].sName, key) = 0) then
    begin
      p := Objects[i];
      break;
    end;
  if p = nil then
  begin
    p := TcMetaData.Create(self);
    Add(p);
    p.sName := uppercase(key);
    p.eType := keaINDEX[Index];
  end;
  p.sValue := value;
end;

// TcMetaData
//   ReverseEngineer
//
function TcMetaData.ReverseEngineer(parParent: TcData; parDepth: longint; parArray: array of TcValuePair): boolean;
begin
  result := FALSE;
end;

// TcMetaData
//   Refresh
//
function TcMetaData.Refresh(parParent: TcData): boolean;
begin
  parParent.ClearChildren;
  result := parParent.Load(self, [elfRefresh]);
end;

// TcMetaData
//   SetError
//
procedure TcMetaData.SetError(level: TeLevel; value, sSQL: String);
var
  p: TcObject;
  s: String;
begin
  if m_lstErrors = nil then
    m_lstErrors := TcCollection.Create(self);
  if m_lstErrors.Find(value) = nil then
  begin
    p := TcObject.Create(self);
    m_lstErrors.Add(p);
    p.iCode := longint(level);
    p.sName := value;
    s := sSQL;
    s := AnsiReplaceText(s, ksCR, '\n');
    s := AnsiReplaceText(s, kcLF, '\n');
    s := AnsiReplaceText(s, kcCR, '\r');
    s := AnsiReplaceText(s, kcTAB, '\t');
    p.sValue := s;
  end;
  if m_objOrigin <> nil then
    m_objOrigin.SetError(level, value, sSQL);
end;

// TcMetaData
//   GetError
//
function TcMetaData.GetError(level: TeLevel): String;
var
  i: longint;
begin
  result := ksEMPTY;
  // Get error result for this element.
  if m_lstErrors <> nil then
    for i := 0 to m_lstErrors.Count - 1 do
      if (level = elAll) or (TeLevel(m_lstErrors[i].iCode) = level) then
      begin
        result := result + '[' + kasERRORLEVEL[TeLevel(m_lstErrors[i].iCode)] + '] ' + m_lstErrors[i].sName + '\n';
        if m_lstErrors[i].sValue <> ksEMPTY then
          result := result + m_lstErrors[i].sValue + '\n';
      end;
  // Browse children
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcMetaData) then
    result := result + (Objects[i] as TcMetaData).GetError(level);
end;

// TcMetaData
//   DisplayErrors
//
function TcMetaData.DisplayErrors(parListView: TListView): boolean;

  procedure SubDisplay(parMetaData: TcMetaData);
  var
    i: longint;
    p: TListItem;
  const
    kaiLevelToRuleLevel: array[TeLevel] of longint =
      (kiUNDEFINED, 2, 1, 0, 3);
  begin
    // Get error result for this element.
    if parMetaData.m_lstErrors <> nil then
      for i := 0 to parMetaData.m_lstErrors.Count - 1 do
      begin
        p := parListView.Items.Add;
        p.ImageIndex := kaiLevelToRuleLevel[TeLevel(parMetaData.m_lstErrors[i].iCode)];
        p.Caption := ksEMPTY;
        p.SubItems.Add(parMetaData.sName);
        p.SubItems.Add(parMetaData.m_lstErrors[i].sName);
        p.SubItems.Add(parMetaData.m_lstErrors[i].sValue);
        p.Data := pointer(parMetaData.m_lstErrors[i]);
      end;
    // Browse children
    for i := 0 to parMetaData.count - 1 do
      if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetaData) then
        SubDisplay(parMetaData[i] as TcMetaData);
  end;

begin
  parListView.Items.BeginUpdate;
  parListView.Items.Clear;
  SubDisplay(self);
  parListView.Items.EndUpdate;
  result := parListView.Items.Count > 0;
end;

// TcMetaData
//   Statement
//
function TcMetaData.Statement(parData: TObject; eparSet: TeTypeSet; sparSubType: String): String;
var
  i: longint;
  s: String;
begin
  result := ksEMPTY;
  for i := 0 to count - 1 do
    if (Objects[i].eType in eparSet) and
       ((sparSubType = ksEMPTY) or (AnsiCompareText(sparSubType, (Objects[i] as TcMetaData).Attribute[krsTYPE]) = 0)) then
    begin
      s := Expression(Objects[i].sValue, parData, Objects[i] as TcMetaData, sparSubType, [], FALSE);
      if (s <> ksEMPTY) and (eparSet = [enRule]) then
        s := s + ksCR;
      result := result + s;
    end;
end;

// TcMetaData
//   Find (1)
//
function TcMetaData.Find(value: TeType; SubType: String): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Objects[i].eType = value) and
       ((AnsiCompareText(SubType, (Objects[i] as TcMetaData).Attribute[krsTYPE]) = 0) or (SubType = ksEMPTY)) then
    begin
      result := Objects[i];
      break;
    end;
end;

// TcMetaData
//   Find (2)
//
function TcMetaData.Find(parType: TeTypeSet; value: String): TcObject;
var
  i: longint;
begin
  result := nil;
  if ((parType = []) or (eType in parType)) and (AnsiCompareText(value, sName) = 0) then
    result := self
  else for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcMetaData) then
    begin
      result := (Objects[i] as TcMetaData).Find(parType, value);
      if result <> nil then
        break;
    end;
end;

// TcMetaData
//   Find (3)
//
function TcMetaData.Find(parType: TeTypeSet; value: String; parDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  if (eType in parType) and (AnsiCompareText(value, sName) = 0) then
    result := self
  else if parDepth > 0 then
    for i := 0 to Count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcMetaData) then
      begin
        result := (Objects[i] as TcMetaData).Find(parType, value, parDepth - 1);
        if result <> nil then
          break;
      end;
end;

// TcMetaData
//   Has (1)
//
function TcMetaData.Has(value: TeType; SubType: String): boolean;
begin
  result := Has(value, SubType, kiLARGEINT);
end;

// TcMetaData
//   Has (2)
//
function TcMetaData.Has(value: TeType; SubType: String; parDepth: longint): boolean;
var
  i: longint;
begin
  result := FALSE;
  for i := 0 to Count - 1 do
  begin
    result := (Objects[i].eType = value) and ((AnsiCompareText(SubType, (Objects[i] as TcMetaData).Attribute[krsTYPE]) = 0) or (SubType = ksEMPTY));
    if not result and (parDepth > 1) then
      result := (Objects[i] as TcMetaData).Has(value, SubType, parDepth - 1);
    if result then
      break;
  end;
end;

// TcMetaData
//   Identifier method
//
function TcMetaData.Identifier(value: String): TcMetaData;

  function FirstWord(value: String): String;
  var
    L: longint;
  begin
    L := pos(krsPERIOD, value);
    if L = 0 then
      result := uppercase(value)
    else
      result := uppercase(system.copy(value, 1, L - 1));
  end;

  function GetIdentifier(parObject: TcMetaData; value: String; iDepth: longint; parType: TeTypeSet): TcMetaData;
  begin
    result := nil;
    if value <> ksEMPTY then
    begin
      result := parObject.Find(parType, FirstWord(value), iDepth) as TcMetaData;
      if (result = nil) and (parObject <> parObject.TopParent) and (parObject.TopParent <> nil) and (parObject.TopParent is TcMetaData) then
        result := (parObject.TopParent as TcMetaData).Find(parType, FirstWord(value), iDepth) as TcMetaData;
    end;
    if (result <> nil) and (result is TcMetaData) and (uppercase(value) <> FirstWord(value)) then
      result := GetIdentifier(result, system.copy(value, length(FirstWord(value)) + 2, length(value)), 1, [enObject, enItem, enField]);
  end;

begin
  result := GetIdentifier(self, value, kiLARGEINT, [enObject, enItem]);
end;

// TcMetaData
//   SendToObject method
//
function TcMetaData.SendToObject(ptrObject: TObject): TObject;
var
  i: longint;
begin
  result := nil;
  if self = TopParent then // If this is the tree root then avoid displaying self.
    for i := 0 to count - 1 do
      Objects[i].SendToObject(ptrObject)
  else if eType in [enUndefined, enObject] then // Only display objects: no fields, etc...
    result := inherited SendToObject(ptrObject);
end;

// TcMetaData
//   GetDescription method
//
function TcMetaData.GetDescription: String;
begin
  if (eType <> enObject) and (Ancestor(enObject) <> nil) and (Ancestor(enObject) is TcMetaData) then
    result := (Ancestor(enObject) as TcMetaData).Description
  else
  begin
    result := Attribute[krsDESCRIPTION];
    if result = ksEMPTY then
    begin
      result := FullName[efmIdentifier];
      SetError(elWarning, Format('%s does not have a ''Description'' attribute.', [FullName[efmIdentifier]]), ksEMPTY);
    end;
  end;
end;

// TcMetaData
//   GetScriptTypes method
//
function TcMetaData.GetScriptTypes: TStringList;
var
  lst: TStringList;

  procedure TraverseTree(objMeta: TcMetaData);
  var
    i: longint;
  begin
    if (objMeta.eType = enScript) and
       (objMeta.Parent <> nil) and
       (objMeta.Parent.eType = enObject) and
       objMeta.HasAttribute[krsTYPE] and
       (objMeta.Attribute[krsTYPE] <> ksEMPTY) and
       (not objMeta.HasAttribute[krsDISPLAY] or (objMeta.HasAttribute[krsDISPLAY] and (AnsiCompareText(objMeta.Attribute[krsDISPLAY], krsTRUE) = 0))) and
       (lst.IndexOf(objMeta.Attribute[krsTYPE]) = kiUNDEFINED) then
      lst.Add(objMeta.Attribute[krsTYPE]);
    for i := 0 to objMeta.count - 1 do
      TraverseTree(objMeta[i] as TcMetaData);
  end;

begin
  lst := TStringList.Create;
  TraverseTree(self);
  if lst.Count = 0 then
    lst.Add(krsDEFAULTSCRIPT);
  lst.Sort;
  result := lst;
end;

// TcMetaData
//   MetaRelationships method
//   Recursively, establish all meta data dependencies.
//
procedure TcMetaData.MetaRelationships;
var
  i: longint;
  p: TcObject;
begin
  try
    //
    // Set the MTOK
    if HasAttribute[krsMTOK] then
    begin
      m_objMTOK := (TopParent as TcMetaData).Identifier(Attribute[krsMTOK]);
      if m_objMTOK = nil then
        SetError(elWarning, Format('Parent %s, Object %s, MTOK %s is invalid.', [Parent.sName, sName, Attribute[krsMTOK]]), ksEMPTY)
      else if (m_objMTOK is TcMetaData) then
        Attribute[krsMTOK] := (m_objMTOK as TcMetaData).FullName[efmIdentifier];
    end;
    //
    // Set the dependency
    for i := 0 to m_lstXMLDependencies.count - 1 do
    begin
      p := nil;
      if AnsiCompareText(system.copy(m_lstXMLDependencies[i], 1, length(krsSELECT)), krsSELECT) <> 0 then
      begin
        p := (TopParent as TcMetaData).Identifier(m_lstXMLDependencies[i]);
        if p = nil then
          SetError(elWarning, Format('Parent %s, Object %s, Dependency %s is invalid.', [Parent.sName, sName, m_lstXMLDependencies[i]]), ksEMPTY)
        else if p is TcMetaData then
          m_lstXMLDependencies[i] := (p as TcMetaData).FullName[efmIdentifier];
      end;
      m_lstXMLDependencies.Objects[i] := p;
    end;
    //
    // Loop through items
    for i := 0 to Count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcMetaData) then
        (Objects[i] as TcMetaData).MetaRelationships;
  except
    //
  end;
end;

// TcMetaData
//   ProcessSQL method
//
function TcMetaData.ProcessSQL(sparType: String; parData: TcData): boolean;
var
  e: TcExecute;
  i: longint;
  s: String;
begin
  result := FALSE;
  e := nil;
  if (eType = enSQL) and (Attribute[krsTYPE] = sparType) then
  try
    e := TcExecute.Create(nil);
    e.Connection := GetConnection;
    e.FetchType := StringToFetchType(Option[krsOPTION_FETCHTYPE]);
    try
      if not HasAttribute[krsMODE] or (HasAttribute[krsMODE] and (Attribute[krsMODE] <> krsUNDIRECTION)) then
      begin
        s := Expression(sValue, parData, self, ksEMPTY, [], FALSE);
        e.Execute(s, sName);
      end
      else if HasAttribute[krsMODE] and (Attribute[krsMODE] = krsUNDIRECTION) then
      begin
        s := Expression(sValue, parData, self, ksEMPTY, [], FALSE);
        e.ExecuteUndirect(s, sName);
      end;
      result := TRUE;
    except
      //
    end;
  finally
    e.free;
  end;
  for i := 0 to count - 1 do
    if (objects[i] <> nil) and (objects[i] is TcMetaData) then
      result := (objects[i] as TcMetaData).ProcessSQL(sparType, parData) and result;
end;

// TcMetaData
//   SetConnection method
//
procedure TcMetaData.SetConnection(const value: TcConnection);
begin
  m_objConnection := value;
  if (parent <> nil) and (parent is TcMetaData) then
    (parent as TcMetaData).SetConnection(value);
end;

// TcMetaData
//   GetConnection method
//
function TcMetaData.GetConnection: TcConnection;
begin
  if (parent <> nil) and (parent is TcMetaData) then
    result := (parent as TcMetaData).GetConnection
  else
    result := m_objConnection;
end;

// TcMetaData
//   MTOK
//
function TcMetaData.MTOK: TcMetaData;
var
  i: longint;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Objects[i].eType = enField) and (Objects[i] is TcMetaData) and ((Objects[i] as TcMetaData).m_objMTOK <> nil) then
    begin
      result := Objects[i] as TcMetaData;
      break;
  end;
end;

// TcMetaData
//   GetHasFeature method
//
function TcMetaData.GetHasFeature(index: TeForm): boolean;
begin
  case index of
    efsObjects, efsQuery, efsGrid, efsOnlineHelp:
      result := TRUE;
    efsStoredQuery:
      result := Find(enFEATURE, krsQUERY) <> nil;
    efsSessionManager:
      result := Find(enFEATURE, krsSESSIONMANAGER) <> nil;
    efsLogParser:
      result := Find(enFEATURE, krsLOGPARSER) <> nil;
    else
      result := FALSE;
  end;
end;

// TcMetaData
//   Translate method
//   Check Translation to see if the value should not be something else.
//
function TcMetaData.Translate(value: String): String;
var
  i, j: longint;
  p: TcMetaData;
  s: String;
begin
  result := value;
  // Verify value translations
  for i := 0 to Count - 1 do
  begin
    p := Objects[i] as TcMetaData;
    if p.eType = enValue then
    begin
      s := p.Attribute[krsTRANSLATION];
      for j := 0 to ItemCount(s, ',') - 1 do
        if AnsiCompareText(trim(Value), trim(Item(s, ',', j))) = 0 then
        begin
          result := p.sName;
          break;
        end;
    end;
  end;
  result := ApplyTransformation(result);
end;

// TcMetaData
//   GetOption method
//
function TcMetaData.GetOption(Index: String): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  if (Parent <> nil) and (Parent is TcMetaData) then
    result := (Parent as TcMetaData).GetOption(Index)
  else
  begin
    p := Find(enOPTION, Index);
    if (p <> nil) and (p is TcMetaData) then
      result := p.sValue;
  end;
end;

// TcMetaData
//   SetOption method
//
procedure TcMetaData.SetOption(Index: String; value: String);
var
  p: TcObject;
begin
  if (Parent <> nil) and (Parent is TcMetaData) then
    (Parent as TcMetaData).SetOption(Index, value)
  else
  begin
    p := Find(enOPTION, Index);
    if p = nil then
    begin
      p := TcMetaData.Create(self);
      p.eType := enOption;
      p.sName := Index;
      p.sValue := value;
      Add(p);
    end;
  end;
end;

// TcMetaData
//   FieldValue
//   Used primarily as a Callback method set as TcMetaData.hdlQuery
//
function TcMetaData.FieldValue(value: String): String;
var
  i: longint;
begin
  result := ksEMPTY;
  if (value <> ksEMPTY) and (m_objRS <> nil) then
    for i := 0 to (m_objRS as TcRecordSet).FieldCount - 1 do
      if AnsiCompareText((m_objRS as TcRecordSet).FieldName(i), value) = 0 then
      begin
        result := VarToStr((m_objRS as TcRecordSet).Fields(i));
        break;
      end;
end;

// TcMetaData
//   GetHdlQuery method
//
function TcMetaData.GetHdlQuery: TcMetaData_ExpressionQuery;
begin
  result := nil;
  if (Parent <> nil) and (Parent is TcMetaData) then
    result := (Parent as TcMetaData).HdlQuery
  else
    result := m_hdlQuery;
end;

// TcMetaData
//   SetHdlQuery method
//
procedure TcMetaData.SetHdlQuery(value: TcMetaData_ExpressionQuery);
begin
  if (Parent <> nil) and (Parent is TcMetaData) then
    (Parent as TcMetaData).SetHdlQuery(value)
  else
    m_hdlQuery := value;
end;

// TcMetaData
//   ApplyTransformation
//
function TcMetaData.ApplyTransformation(value: String): string;
var
  i, L: longint;
  s: String;
begin
  // Flag meta data attribute
  if m_eFlags <> [] then
  begin
    if emUpperCase in m_eFlags then
      value := uppercase(value);
    if emLowerCase in m_eFlags then
      value := lowercase(value);
  end;
  // Item meta data attribute
  if m_sItem <> ksEMPTY then
  begin
    L := strtointdef(Item(m_sItem, ',', 0), kiUNDEFINED);
    if L <> kiUNDEFINED then
    begin
      s := Item(m_sItem, ',', 1);
      if (s <> ksEMPTY) and (ItemCount(value, s) >= L) then
        value := Item(value, s, L);
    end;
  end;
  // Apply Filter
  if m_sFilter <> ksEMPTY then
    for i := length(value) downto 1 do
      if pos(value[i], m_sFilter + krsPLACEHOLDER_USER + krsPLACEHOLDER_CR + krsPLACEHOLDER_LF) = 0 then
        system.Delete(value, i, 1);
  // Done
  result := value;
end;

// TcMetaData
//   GetPreferences
//
function TcMetaData.GetPreferences: TcMetaDataPreferences;
begin
  if (parent <> nil) and (parent is TcMetaData) then
    result := (parent as TcMetaData).GetPreferences
  else
    result := m_objPreferences;
end;

// TcMetaData
//   SetPreferences method
//
procedure TcMetaData.SetPreferences(const value: TcMetaDataPreferences);
begin
  m_objPreferences := value;
  if (parent <> nil) and (parent is TcMetaData) then
    (parent as TcMetaData).SetPreferences(value);
end;

// TcMetaData
//   FindFast method
//
function TcMetaData.FindFast(value: String): TcObject;
var
  L: longint;
begin
  result := nil;
  if value <> ksEMPTY then
  begin
    L := m_lstMetaDataIndex.IndexOf(value);
    if L <> kiUNDEFINED then
      result := TcObject(m_lstMetaDataIndex.Objects[L]);
  end;
end;

// TcMetaData
//   SetFastIndex method
//
procedure TcMetaData.SetFastIndex(value: TcObject; parAction: TeIndexAction);
var
  L: longint;
begin
  if m_lstMetaDataIndex <> nil then
    // Process indexing action
    case parAction of
      // Add
      eiaAdd:
        begin
          L := m_lstMetaDataIndex.IndexOfObject(value);
          if L = kiUNDEFINED then
            m_lstMetaDataIndex.AddObject(value.sName, value)
          else
            m_lstMetaDataIndex[L] := value.sName;
        end;
      // Delete
      eiaDelete:
        begin
          L := m_lstMetaDataIndex.IndexOfObject(value);
          if L <> kiUNDEFINED then
            m_lstMetaDataIndex.Delete(L);
        end;
    end;
end;

// TcMetaData
//   Add method
//
function TcMetaData.Add(parObject: TcObject): longint;
begin
  result := inherited Add(parObject);
  SetFastIndex(parObject, eiaAdd);
end;

// TcMetaData
//   Delete method (3)
//
function TcMetaData.Delete(item: longint): longint;
begin
  SetFastIndex(Objects[item], eiaDelete);
  result := inherited Delete(item);
end;

// TcMetaData
//   Delete method (4)
//
function TcMetaData.Delete(parObject: TcObject): longint;
begin
  SetFastIndex(parObject, eiaDelete);
  result := inherited Delete(parObject);
end;

// TcMetaData
//   Delete method
//
procedure TcMetaData.SetName(value: String);
begin
  inherited SetName(value);
  SetFastIndex(self, eiaDelete);
  SetFastIndex(self, eiaAdd);
end;

// TcMetaData
//   GetIncludeSection method
//
function TcMetaData.GetIncludeSection(value: String): boolean;
var
  p: TcObject;
  i: longint;
begin
  result := TRUE;
  p := GetSections;
  if (p <> nil) and (p is TcObject) then
    for i := 0 to p.count - 1 do
      if (p[i] <> nil) and (p[i].eType = enField) and (AnsiCompareText(p[i].sName, value) = 0) then
      begin
        result := p[i].sValue <> krsFALSE;
        break;
      end;
end;

// TcMetaData
//   GetSectionsAsText method
//
function TcMetaData.GetSectionsAsText: String;
const
  kabINT: array[boolean] of longint = (0, 1);
var
  p: TcObject;
  i: longint;
begin
  result := ksEMPTY;
  p := GetSections;
  if (p <> nil) and (p is TcObject) then
    for i := 0 to p.count - 1 do
      if (p[i] <> nil) and (p[i].eType = enField) then
      begin
        if result <> ksEMPTY then
          result := result + #2;
        result := result + Format('%s%s%d', [p[i].sName, #1, kabINT[p[i].sValue <> krsFALSE]]);
      end;
end;

// TcMetaData
//   SetSectionsAsText method
//
procedure TcMetaData.SetSectionsAsText(value: String);
const
  kabINT: array[boolean] of longint = (0, 1);
var
  p: TcObject;
  i, j: longint;
  s: String;
begin
  p := GetSections;
  if (p <> nil) and (p is TcObject) then
    for j := 0 to ItemCount(value, #2) - 1 do
    begin
      s := Item(value, #2, j);
      for i := 0 to p.count - 1 do
        if (p[i] <> nil) and (p[i].eType = enField) and (AnsiCompareText(p[i].sName, Item(s, #1, 0)) = 0) then
        begin
          p[i].sValue := kasBOOL[Item(value, #1, 1) = '1'];
          break;
        end;
    end;
end;

// TcMetaData
//   GetEmptySectionsAsText method
//
function TcMetaData.GetEmptySectionsAsText: String;
const
  kabINT: array[boolean] of longint = (0, 1);
var
  p: TcObject;
  i: longint;
begin
  result := ksEMPTY;
  p := GetSections;
  if (p <> nil) and (p is TcObject) then
    for i := 0 to p.count - 1 do
      if (p[i] <> nil) and (p[i].eType = enField) then
      begin
        if result <> ksEMPTY then
          result := result + #2;
        result := result + Format('%s%s0', [p[i].sName, #1]);
      end;
end;

// TcMetaData
//   GetSections method
//
function TcMetaData.GetSections: TcObject;
var
  p, q: TcObject;
  i: longint;
begin
  if (self <> TopParent) and (TopParent is TcMetaData) then
    result := (TopParent as TcMetaData).GetSections
  else
  begin
    if m_objSections = nil then
    begin
      m_objSections := TcObject.Create(nil);
      p := (TopParent as TcMetaData).Find(enFeature, krsSECTION);
      if (p <> nil) and (p is TcObject) then
        for i := 0 to p.count - 1 do
          if (p[i] <> nil) and (p[i].eType = enField) then
          begin
            q := TcObject.Create(m_objSections);
            q.Copy(p[i]);
            q.eType := enField;
            m_objSections.Add(q);
          end;
    end;
    result := m_objSections;
  end;
end;

// TcMetaData
//   GetGraphObject method
//
function TcMetaData.GetGraphObject: TcMetaData;
begin
  result := nil;
  if eType <> enObject then
  begin
    if (parent <> nil) and (parent <> self) and (parent is TcMetaData) then
      result := (parent as TcMetaData).GetGraphObject;
  end
  else
  begin
    if Has(enDisplay, ksEMPTY, 1) then
      result := self
    else if (parent <> nil) and (parent <> self) and (parent is TcMetaData) then
      result := (parent as TcMetaData).GetGraphObject;
  end;
end;

// TcMetaData
//   GetHasDisplayName method
//
function TcMetaData.GetHasDisplayName: boolean;
begin
  result := m_sDisplayName <> ksEMPTY;
end;

// TcMetaData
//   GetDisplayName method
//
function TcMetaData.GetDisplayName: String;
begin
  result := sName;
  if m_sDisplayName <> ksEMPTY then
    result := m_sDisplayName;
end;


// TcMetaData
//   GetCanHaveChildren
//
function TcMetaData.GetCanHaveChildren: boolean;
var
  i: longint;
begin
  result := FALSE;
  for i := 0 to count - 1 do
    if (objects[i] <> nil) and (objects[i] is TcMetaData) then
    begin
      result := (objects[i].eType = enObject);
      if not result then
        result := (objects[i] as TcMetaData).GetCanHaveChildren
      else
        break;
    end;
end;

// TcMetaData
//   GetObjectID method
//
function TcMetaData.GetObjectID: String;
begin
  result := ksEMPTY;
  if eType in [enObject, enItem] then
  begin
    if Attribute[krsID] <> ksEMPTY then
      result := Attribute[krsID];
    end
  else if (Parent <> nil) and (parent is TcMetaData) then
    result := (parent as TcMetaData).GetObjectID;
end;

// TcMetaData
//   GetObjectID method
//
function TcMetaData.IsChildOf(value: TeTypeSet): boolean;
begin
  result := eType in value;
  if not result and (Parent <> nil) and (Parent is TcMetaData) then
    result := (parent as TcMetaData).IsChildOf(value)
end;

// TcMetaData
//   ClearScripts
//
procedure TcMetaData.ClearScripts;
var
  i: longint;
  p: TcObject;
begin
  for i := 0 to m_lstIndex.Count - 1 do
  begin
    p := TcData(m_lstIndex.Objects[i]);
    if (p <> nil) and (p is TcData) then
      (p as TcData).ClearScripts;
  end;
end;

// TcMetaData
//   CheckMetaErrors method
//
procedure TcMetaData.CheckMetaErrors;
var
  i: longint;
begin
  //
  // A.Check this node.
  //
  // a. Check ID attribute
  if (Parent <> nil) and (eType in [enObject, enItem]) and not IsChildOf([enFeature, enRule, enMenu]) and (Attribute[krsID] = ksEMPTY) then
    SetError(elMetaFile, 'Meta Data must define an ID attribute.', ksEMPTY)
  else if not (eType in [enObject, enItem]) and (Attribute[krsID] <> ksEMPTY) then
    SetError(elMetaFile, 'Meta Data does not need to define an ID attribute.', ksEMPTY);
  // a.2. Verify that ID is a child
  if (Attribute[krsID] <> ksEMPTY) and (Find([enField], Attribute[krsID], 1) = nil) then
    SetError(elMetaFile, Format('ID attribute ''%s'' is not a child field.', [Attribute[krsID]]), ksEMPTY);
  // a.2. Verify that DisplayName is a child
  if (Parent <> nil) and (eType in [enObject, enItem]) and not IsChildOf([enFeature, enRule, enMenu]) and (GetDisplayName = ksEMPTY) then
    SetError(elMetaFile, 'Meta Data must define a DisplayName attribute.', ksEMPTY);
  // b. SQL Naming
  if (Parent <> nil) and not IsChildOf([enFeature, enRule, enMenu]) and (eType = enSQL) and (Parent.eType in [enItem, enObject]) and (Attribute[krsTYPE] = ksEMPTY) then
  begin
    if (sName = ksEMPTY) then
      SetError(elMetaFile, 'SQL declaration should include a NAME attribute.', ksEMPTY);
    if (pos('$' + krsID, sValue) = 0) then
    SetError(elMetaFile, 'SQL declaration must include $ID conditional declaration.', ksEMPTY);
  end;
  // b. Reference Naming
  if (Parent <> nil) and (eType = enDependency) and (sName = ksEMPTY) then
    SetError(elMetaFile, 'Dependency SQL declaration should include a NAME attribute.', ksEMPTY);
  // c. Key Attribute
  if (Attribute[krsKEY] <> ksEMPTY) and (parent <> nil) and (parent is TcMetaData) and ((parent as TcMetaData).Find([enField], Attribute[krsKEY], 1) = nil) then
    SetError(elMetaFile, Format('''%s'' KEY attribute declaration is invalid.', [Attribute[krsKEY]]), ksEMPTY);
  // d. MTOK Declaration
  if (AnsiCompareText(sName, krsUSER) <> 0) and (eType in [enItem, enObject]) and (parent <> nil) and (parent.eType in [enItem, enObject]) and (MTOK = nil) then
    SetError(elMetaFile, Format('%s should include a field with a MTOK attribute.', [kasTYPEINITCAP[eType]]), ksEMPTY);
  //
  // B.Check this node. Verify Children
  //
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcMetaData) then
      (Objects[i] as TcMetaData).CheckMetaErrors;
end;

// TcMetaData
//   CountData
//
function TcMetaData.CountData(value: TcObject): longint;
var
  i: longint;
  p: TcObject;
begin
  result := 0;
  if value <> nil then
    for i := 0 to lstIndex.Count - 1 do
    begin
      p := TcObject(lstIndex.Objects[i]);
      if (p <> nil) and value.Has(p) then
        inc(result);
    end;
end;

// TcMetaData
//   SortByDependency
//   > Dependency List sorting
//
procedure TcMetaData.SortByDependency(var parList: TcBag);
  //
  // IsOrdered
  function IsOrdered(obj: TcData; var lst: TcBag): boolean;
  var
    i: longint;
    b: boolean;
    q: TcObject;
  begin
    result := TRUE;
    for i := 0 to obj.FromDependency.Count - 1 do
    begin
      q := obj.FromDependency[i].Ancestor(enObject);
      b := lst.IndexOf(q) <> kiUNDEFINED; // Has it been added already?
      if not b then
        b := parList.IndexOf(q) = kiUNDEFINED; // Is it actually part of the list?
      result := result and b;
      if not result then
        break;
    end;
  end;

var
  lst: TcBag;
  p: TcObject;
  i, L: longint;
begin
  lst := nil;
  try
    lst := TcBag.Create(nil);
    while TRUE do
    begin
      // Remember how many elements were there
      L := parList.Count;
      // Pass 1 - Add all possible elements
      for i := 0 to parList.Count - 1 do
      begin
        p := parList[i];
        if (p <> nil) and (p is TcData) and IsOrdered(p as TcData, lst) then
        begin
          lst.Add(p);
          parList[i] := nil;
        end;
      end;
      // Pass 2 - Get rid of all nil entries
      for i := parList.Count - 1 downto 0 do
        if parList[i] = nil then
          parList.Delete(i);
      // Compare progress
      if L = parList.Count then // i.e. None.
      begin
        for i := 0 to parList.Count - 1 do
          lst.Add(parList[i]);
        break;
      end;
    end;
    // Finally, transfer temporary list into result
    parList.Clear;
    for i := 0 to lst.Count - 1 do
      parList.Add(lst[i]);
  finally
    lst.free;
  end;
end;

// TcMetaData
//   OnlineHelpStr
//
function TcMetaData.OnlineHelpStr: String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i  := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcMetaData) and ((Objects[i] as TcMetaData).eType = enHelp) then
      result := result + Format('<table border="0" cellpadding="2" cellspacing="0">' +
                                '<tr>' +
                                '<td valign="middle"><a href="%s"><img src="%%ref_ApplicationPath%%Images\Help2.gif" border="0"></a></td>' +
                                '<td valign="middle"><a href="%s">%s</a></td>' +
                                '</tr>' +
                                '</table>', [Objects[i].sValue, Objects[i].sValue, Objects[i].sName]);
end;

//
// TcData
//

// TcData
//   Constructor
//
constructor TcData.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objMetaData := nil;
  m_objDisplay := nil;
  m_depFrom := TcCollection.Create(self);
  m_depTo := TcCollection.Create(self);
  m_procDisplay := nil;
  m_lstRuleProcessing := TcCollection.Create(self);
  m_objAncestor := nil;
  m_eState := edsDB;
  m_iAlertLevel := kiUNDEFINED;
  m_X := 0;
  m_Y := 0;
  m_bIsLoaded := FALSE;
  m_lstScripts := TcCollection.Create(nil);
end;

// TcData
//   Destructor
//
destructor TcData.Destroy;
begin
  SetMetaData(nil);
  m_depFrom.free;
  m_depTo.free;
  m_lstRuleProcessing.free;
  m_lstScripts.free;
  inherited Destroy;
end;

// TcData
//   Copy
//
procedure TcData.Copy(value: TcObject);
begin
  inherited Copy(value);
  if (value <> nil) and (value is TcData) then
  begin
    SetMetaData((value as TcData).m_objMetaData);
    m_objDisplay := (value as TcData).m_objDisplay;
    m_procDisplay := (value as TcData).m_procDisplay;
    // m_depFrom
    // m_depTo
    // m_lstRuleProcessing
    // m_objAncestor
    Tag := value.Tag;
    m_sPreviousValue := value.sValue;
    m_iAlertLevel := (value as TcData).iAlertLevel;
    m_X := (value as TcData).m_X;
    m_Y := (value as TcData).m_Y;
    m_bIsLoaded := (value as TcData).m_bIsLoaded;
    ClearScripts;
  end
  else if (value <> nil) and (value is TcMetaData) then
    SetMetaData(value as TcMetaData);
end;

// TcData
//   SetXML method
//
procedure TcData.SetXML(value: String);

  //
  // Recursively, Load all meta data elements from the XML.
  procedure SubLoad(parParent: TcData; parMeta: TcMetaData; pOLE: OLEVariant);
  var
    i, j: longint;
    p: TcData;
    q, m: TcObject;
    s, s2, t: String;
    pAtt: OLEVariant;
  begin
    q := parMeta.Find([StringToType(VarToStr(pOLE.nodeName))], GetXMLAttribute(pOLE, 'name'));
    if q <> parMeta then
    begin
      p := TcData.Create(parParent);
      parParent.Add(p);
    end
    else
      p := parParent;
    if (q <> nil) and (q is TcMetaData) then
    begin
      p.SetMetaData(q as TcMetaData);
      // Attributes...?
      //for i := 0 to pOLE.attributes.length - 1 do
      //  p.m_lstAttributes.Values[pOLE.attributes.item[i].name] := VarToStr(pOLE.attributes.item[i].value);
      // Child element nodes..?
      for i := 0 to pOLE.childNodes.length - 1 do
        //
        // XML Value ...?
        if (pOLE.childNodes.item[i].nodeType = ntNODE_ELEMENT) and (AnsiCompareText(pOLE.childNodes.item[i].nodeName, 'value') = 0) then
        begin
          p.sValue := XMLToText(GetXMLValue(pOLE.childNodes.item[i]));
          if (q <> nil) and (q is TcMetaData) then
            p.sValue := (q as TcMetaData).Translate(p.sValue);
              p.sValue := p.sValue + s;
          p.State := edsDB;
        end
        //
        // Alert...?
        else if (pOLE.childNodes.item[i].nodeType = ntNODE_ELEMENT) and (AnsiCompareText(pOLE.childNodes.item[i].nodeName, krsALERT) = 0) then
        begin
          s := ksEMPTY;
          t := ksEMPTY;
          pAtt := pOLE.childNodes.item[i];
          for j := 0 to pAtt.attributes.length - 1 do
            if pAtt.attributes.item[j].name = krsNAME then
              t := pAtt.attributes.item[j].value;
          for j := 0 to pAtt.childNodes.length - 1 do
            if pAtt.childNodes.item[j].nodeType = ntNODE_TEXT then
              s := s + pAtt.childNodes.item[j].nodeValue;
          m := q.Find(t);
          if (m <> nil) and (m is TcMetaData) then
            p.SetRuleResult(m as TcMetaData, s);
        end
        //
        // Dependencies...?
        else if (pOLE.childNodes.item[i].nodeType = ntNODE_ELEMENT) and (AnsiCompareText(pOLE.childNodes.item[i].nodeName, krsDEPENDENCY) = 0) then
        begin
          s := ksEMPTY;
          q := nil;
          pAtt := pOLE.childNodes.item[i];
          for j := 0 to pAtt.attributes.length - 1 do
            if pAtt.attributes.item[j].name = krsNAME then
              s := pAtt.attributes.item[j].value
            else if pAtt.attributes.item[j].name = krsOBJECT then
              s2 := pAtt.attributes.item[j].value;
          if (s <> ksEMPTY) and (s2 <> ksEMPTY) then
            p.AddDependency(s, s2, pAtt.Tag, eTo);
        end
        //
        // Element children...
        else
          SubLoad(p, p.m_objMetaData, pOLE.childNodes.item[i]);
    end;
  end;

var
  p, err: OLEVariant;
  obj: TcObject;
begin
  try
    CoInitialize(nil);
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) then
      begin
        if p.ChildNodes.length > 0 then
        begin
          obj := m_objMetaData.Find([StringToType(VarToStr(p.ChildNodes.item[0].nodeName))], GetXMLAttribute(p.ChildNodes.item[0], 'name'));
          if (obj <> nil) and (obj is TcMetaData) then
          begin
            MetaData := obj as TcMetaData;
            SubLoad(self, obj as TcMetaData, p.ChildNodes.item[0]);
          end;
          SetDependencies;
        end;
      end
      else
      begin
        err := p.parseError;
        if err.errorCode <> 0 then
          raise Exception.Create(err.reason);
      end;
      p := unassigned;
    except
      on E: Exception do
      begin
        Clear;
        MetaData.SetError(elFatal, E.Message, ksEMPTY);
  {$IFNDEF FACTORY}
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
  {$ENDIF}
        p := unassigned;
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

// TcData
//   GetXML method
//
function TcData.GetXML: String;
begin
  result := XML_Filtered(nil, nil);
end;

// TcData
//   XML_Filtered Method
//
function TcData.XML_Filtered(parFilter: TcBag; parReplacements: TStringList): String;

  // Tool
  //   Create Comment block
  //
  function GetComment(obj: TcData): String;
  begin
    result := ksEMPTY;
    if obj.State <> edsDB then
      result := Format(' STATE="%s"', [kasSTATE[obj.State]]);
  end;

  // Tool
  //   Filter value with StringList for replacements
  //
  function GetReplacement(value: String): String;
  var
    i: longint;
    s: string;
  begin
    result := value;
    if parReplacements <> nil then
      for i := 0 to parReplacements.count - 1 do
      begin
        s := parReplacements.Names[i];
        result := AnsiReplaceStr(result, s, parReplacements.Values[s]);
      end;
    result := TextToXML(result);
  end;

  // Tool
  //   SubXML block
  //
  function SubXML(obj: TcData; Depth: longint): String;
  var
    s, sv, st, sTag: string;
    i: longint;
    p: TcDataDependency;
    q: TcObject;
    fish: TBlowfishCipher;
  begin
    st := RepeatStr(Depth, kcTAB);
    sTag := lowercase(kasTYPE[obj.MetaData.eType]);
    //
    // 1. Proceed children first
    //
    s := ksEMPTY;
    //
    // 1.a. Get children XML
    for i := 0 to obj.count - 1 do
      if (obj[i] <> nil) and (obj[i] is TcData) then
      begin
        q := (obj[i] as TcData).MetaData;
        if (q <> nil) then
          q := q.Ancestor(enObject);
        if (AnsiCompareText(q.sName, krsUSER) = 0) or
           (((parFilter = nil) or ((parFilter <> nil) and (parFilter.IndexOf(q) <> kiUNDEFINED)))) then
        begin
          if s <> ksEMPTY then
            s := s + ksCR;
          s := s + SubXML(obj[i] as TcData, Depth + 1);
        end;
      end;
    //
    // 1.b. Get Alerts
    for i := 0 to obj.m_lstRuleProcessing.Count - 1 do
    begin
      if s <> ksEMPTY then
        s := s + ksCR;
      with (obj.m_lstRuleProcessing[i] as TcRuleResult) do
        s := s + st + Format('<alert name="%s" level="%d">%s</alert>', [Rule.sName, longint(eLevel), sValue]);
    end;
    //
    // 1.c. Get Dependencies
    if obj.m_depFrom.Count > 0 then
      for i := 0 to obj.m_depFrom.Count - 1 do
      begin
        p := obj.m_depFrom[i] as TcDataDependency;
        if s <> ksEMPTY then
          s := s + ksCR;
        // Filter value with StringList for replacements
        s := s + RepeatStr(Succ(Depth), kcTAB) + Format('<dependency object="%s" name="%s" />', [p.sName, GetReplacement(p.sValue)]);
      end;
    //
    // 2. Proceed to build this node
    sv := GetReplacement(obj.sValue);
    // Encrypt?
    fish := nil;
    if AnsiCompareText(obj.MetaData.Attribute[krsMODE], krsENCRYPT) = 0 then
    try
      fish := TBlowfishCipher.Create(GetKey, SizeOf(Int64));
      sv := DataToHex((fish as T64BitBlockCipher).EncryptedString(sv));
    finally
      fish.free;
    end;
    result := ksEMPTY;
    if obj.MetaData <> nil then
      result := Format('%s<%s name="%s"', [st, sTag, obj.MetaData.sName]);
    if obj.TopParent = obj then
      result := result + Format(' xml="%s"', [MetaData.Connection.sXML]) +
                         Format(' date="%s"', [FormatDateTime('mm/dd/yyyy hh:nn:ss', now)]) +
                         Format(' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://www.seabirdsoftware.com/dbAnalyst.xsd" %s="%s"', [krsENGINE, GetApplicationVersion]);
    if s = ksEMPTY then
    begin
      if sv = ksEMPTY then
        result := result + Format(' %s/>', [GetComment(obj)])
      else
        result := result + Format('%s><value>%s</value></%s>', [GetComment(obj), sv, sTag])
    end
    else
    begin
      if sv = ksEMPTY then
        result := result + Format('%s>%s%s%s%s</%s>', [GetComment(obj), ksCR, s, ksCR, st, sTag])
      else
        result := result + Format('%s><value>%s</value>%s%s%s%s</%s>', [GetComment(obj), sv, ksCR, s, ksCR, st, sTag])
    end;
  end;

begin
  result := SubXML(Self, 0);
end;

// TcData
//   GetValueByMeta Method
//
function TcData.GetValueByMeta(value: TcMetaData): String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to count - 1 do
    if (self[i] <> nil) and (self[i] is TcData) and ((self[i] as TcData).MetaData = value) then
    begin
      result := self[i].sValue;
      break;
    end;
end;

// TcData
//   GetHTML Method
//
function TcData.GetHTML: String;

  // Tool
  //   GetDataList
  //
  function GetDataList(value: TcData; parMetaData: TcMetaData): TcBag;

    procedure Sub(parList: TcBag; value: TcData);
    var
      i: longint;
    begin
      if value.MetaData = parMetaData then
        parList.Add(value);
      for i := 0 to value.count - 1 do
        if (Value[i] <> nil) and (value[i] is TcData) then
          Sub(parList, value[i] as TcData);
    end;

  begin
    result := TcBag.Create(nil);
    Sub(result, value);
  end;

  // Tool
  //   SubGetHTML block
  //
  function SubGetHTML(value: TcMetaData): String;
  var
    i, j, k: longint;
    lst: TcBag;
    h, r: String;
    p: TcData;
  begin
    result := ksEMPTY;
    if value.eType in [enObject, enItem] then
    begin
      lst := nil;
      try
        // Get a list of data elements for *this meta data
        lst := GetDataList(self, value);
        // Create Output
        if lst.Count > 0 then
        begin
          //
          // Child Objects
          result := result +
                    ksCR +
                    kcTAB + kcTAB + '<table cellspacing="0" border="1" cellpadding="2" style="margin: 12px 12px 12px 12px;">' + ksCR +
                    kcTAB + kcTAB + kcTAB + Format('<tr class="header_yellow"><td colspan="%d">%s</td></tr>', [value.count, value.DisplayName]) +
                    ksCR;
          h := ksEMPTY;
          // Header
          for j := 0 to value.Count - 1 do
            if (value[j] <> nil) and (value[j] is TcMetaData) and (value[j].eType in [enField]) then
              h := h + Format('<td>%s</td>', [(value[j] as TcMetaData).DisplayName]);
          result := result +
                    kcTAB + kcTAB + kcTAB + Format('<tr class="header_purple">%s</tr>', [h]) + ksCR;
          // Rows
          for k := 0 to lst.Count - 1 do
          begin
            r := ksEMPTY;
            p := lst[k] as TcData;
            for j := 0 to value.Count - 1 do
              if (value[j] <> nil) and (value[j] is TcMetaData) and (value[j].eType in [enField]) then
                r := r + Format('<td>%s&nbsp;</td>', [p.GetValueByMeta(value[j] as TcMetaData)]);
            r := kcTAB + kcTAB + kcTAB + Format('<tr>%s</tr>', [r]);
            result := result + r + ksCR;
          end;
          result := result + kcTAB + kcTAB + '</table>' + ksCR;
        end;
      finally
        lst.free;
      end;
    end;
    for i := 0 to value.Count - 1 do
      if (value[i] <> nil) and (value[i] is TcMetaData) then
        result := result + SubGetHTML(value[i] as TcMetaData);
  end;

  // Tool
  //   SubGetDependencies block
  //
  function SubGetDependencies: String;
  begin
    result := ksEMPTY;
    if GetDependencyHeader <> ksEMPTY then
    begin
      // From Dependency?
      if GetDependencyText(eFrom, eHTML) <> ksEMPTY then
        result := result + kcTAB + kcTAB + kcTAB + Format('<tr><td>Parent Dependencies</td><td>%s</td></tr>', [GetDependencyText(eFrom, eHTML)]) + ksCR;
      // To Dependency?
      if GetDependencyText(eTo, eHTML) <> ksEMPTY then
        result := result + kcTAB + kcTAB + kcTAB + Format('<tr><td>Child Dependencies</td><td>%s</td></tr>', [GetDependencyText(eTo, eHTML)]) + ksCR;
      // Create Result;
      if result <> ksEMPTY then
        result := kcTAB + kcTAB + '<table cellspacing="0" border="1" cellpadding="2" style="margin: 12px 12px 12px 12px;">' + ksCR +
                  kcTAB + kcTAB + kcTAB + '<tr class="header_red"><td colspan="2">Dependencies</td></tr>' + ksCR +
                  result +
                  kcTAB + kcTAB + '</table>' + ksCR;
    end;
  end;

begin
  result := // Depenendecies
            SubGetDependencies +
            // Objects + Items
            SubGetHTML(self.MetaData);
end;

// TcData
//   GetGraphXML method
//
function TcData.GetGraphXML: String;

  function IsGraphNode(Node: TcObject; parPreference: TcMetaDataPreferences): boolean;
  var
    m: TcObject;
  begin
    m := Node;
    if (m <> nil) and (m is TcData) then
      m := (Node as TcData).MetaData;
    result := (m <> nil) and (m is TcMetaData) and (m.eType = enObject) and (m as TcMetaData).HasAttribute[krsGRAPH] and parPreference.AsBoolean[m.sName];
  end;

  function IsdependentTo(lst: TcBag; parPreference: TcMetaDataPreferences): boolean;
  var
    i: longint;
    p: TcDataDependency;
  begin
    result := FALSE;
    for i := 0 to lst.Count - 1 do
      if (lst[i] <> nil) and (lst[i] is TcDataDependency) then
      begin
        p := lst[i] as TcDataDependency;
        result := (p <> nil) and IsGraphNode(m_objMetaData.Find([enObject, enItem], p.sName), parPreference);
        if result then
          break;
      end;
  end;

  function SubXML(obj: TcData; Depth: longint; parPreference: TcMetaDataPreferences): String;
  var
    s, sv, t: string;
    i: longint;
    p, q, m: TcObject;
    b: boolean;
  begin
    s := ksEMPTY;
    b := IsGraphNode(obj, parPreference) and (IsDependentTo(obj.m_depTo, parPreference) or IsDependentTo(obj.m_depFrom, parPreference));
    // Get children XML
    for i := 0 to obj.count - 1 do
      if (obj[i] <> nil) and (obj[i] is TcData) then
      begin
        t := SubXML(obj[i] as TcData, Depth + 1, parPreference);
        if t <> ksEMPTY then
        begin
          if s <> ksEMPTY then
            s := s + ksCR;
          s := s + t;
        end;
      end;
    // Get Dependencies
    if b and (obj.m_depFrom.Count > 0) then
      for i := 0 to obj.m_depFrom.Count - 1 do
      begin
        p := obj.m_depFrom[i];
        if p <> nil then
        begin
          q := nil;
          m := m_objMetaData.Find([enObject, enItem], p.sName);
          if (m <> nil) and (m is TcMetaData) then
            q := FindInstance(m as TcMetaData, p.sValue);
          if q <> nil then
          begin
            if s <> ksEMPTY then
              s := s + ksCR;
            s := s + RepeatStr(Succ(Depth), kcTAB) + Format('<link code="%d" meta="%s" name="%s" type="1" />', [q.Tag, q.sName, q.sValue]);
          end;
        end;
      end;
    //
    // Proceed to build this node
    sv := TextToXML(obj.sValue);
    result := ksEMPTY;
    if (s <> ksEMPTY) or (obj.m_depTo.Count > 0) then
    begin
      if b or (self = obj) then
      begin
        // Tag declaration
        result := Format('%s<node code="%d"', [RepeatStr(Depth, kcTAB), obj.Tag]);
        // meta information?
        if obj.MetaData <> nil then
          result := result + Format(' meta="%s"', [obj.MetaData.sName]);
        // name?
        if obj.Header <> ksEMPTY then
          result := result + Format(' name="%s"', [obj.Header]);
        // X/Y ?
        if (obj.X <> 0) or (obj.Y <> 0) then
          result := result + Format(' X="%d" Y="%d"', [obj.X, obj.Y]);
        // icon
        result := result + Format(' icon="%d"', [longint(obj.MetaData.eIcon)]);
        // Any Alert?
        if obj.iAlertLevel <> kiUNDEFINED then
          result := result + Format(' alert="%d"', [obj.iAlertLevel]);
        // XML Header
        if self = obj then
          result := result + Format(' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://www.seabirdsoftware.com/dbAnalyst.xsd" %s="%s"', [krsENGINE, GetApplicationVersion]);
        result := result + Format('>%s%s%s%s</node>', [ksCR, s, ksCR, RepeatStr(Depth, kcTAB)]);
      end
      else
        result := result + s;
    end
  end;

begin
  try
    result := SubXML(Self, 0, m_objMetaData.Preferences);
  except
    result := ksEMPTY;
  end;
end;

// TcData
//   IdentifierValue method
//
function TcData.IdentifierValue(sIdentifier, sparSubType: String): String;

  function FirstWord(value: String): String;
  var
    L: longint;
  begin
    L := pos(krsPERIOD, value);
    if L = 0 then
      result := uppercase(value)
    else
      result := uppercase(system.copy(value, 1, L - 1));
  end;

  function GetChildIdentifier(parNode: TcData; value: string): String;
  var
    i, L: longint;
    p: TcMetaData;
    s: String;
  begin
    result := ksEMPTY;
    if AnsiCompareText(value, parNode.sName) = 0 then
    begin
      if eType in [enObject, enItem] then
        result := Statement([enScript], sparSubType)
      else
        result := sValue;
    end
    else
    begin
      L := pos(krsPERIOD, value);
      value := uppercase(system.copy(value, L + 1, length(value)));
      for i := 0 to parNode.Count - 1 do
        if (parNode[i] <> nil) and
           (parNode[i] is TcData) and
           ((parNode[i] as TcData).m_objMetaData <> nil) and (AnsiCompareText((parNode[i] as TcData).m_objMetaData.sName, value) = 0) then
        begin
          // Add create string
          s := (parNode[i] as TcData).IdentifierValue(value, sparSubType);
          // Separator..?
          if (result <> ksEMPTY) and ((parNode[i] as TcData).m_objMetaData <> nil) and (s <> ksEMPTY) then
          begin
            p := (parNode[i] as TcData).m_objMetaData.Find(enScript, sparSubType) as TcMetaData;
            if p <> nil then
              result := result + p.Attribute[krsSEPARATOR];
          end;
          // Append
          result := result + s;
        end;
    end
  end;

  function SubIdentifierValue(objData: TcData; value: String): String;
  begin
    result := ksEMPTY;
    // If this a child meta node?
    if (objData <> nil) and (objData.m_objMetaData <> nil) and (objData.m_objMetaData.Find(FirstWord(value)) <> nil) then
      result := GetChildIdentifier(objData, value)
    // it could be a parent node, or child of a parent node
    else if objData <> nil then
    begin
      if (objData.parent = nil) or (objData.parent = objData) or (TopParent = objData) or
         ((objData.parent <> nil) and (objData.parent <> objData) and (objData.parent is TcData) and ((objData.parent as TcData).m_objMetaData = nil) ) then
      begin
        m_objMetaData.SetError(elFatal, Format('Identifier ''%s'' is invalid.', [value]), ksEMPTY);
        result := format('[%s]', [value]);
      end
      else
        result := SubIdentifierValue(objData.Parent as TcData, value);
    end;
  end;

begin
  result := SubIdentifierValue(self, sIdentifier);
end;

// TcData
//   Child method (1)
//
function TcData.Child(value: String): TcObject;
var
  p: TcObject;
begin
  result := nil;
  p := m_objMetaData.Find(value);
  if (p <> nil) and (p is TcMetaData) then
    result := Child(p as TcMetaData);
end;

// TcData
//   Child method (2)
//
function TcData.Child(parObject: TcObject): TcObject;
begin
  result := Find(parObject.sName);
  if (result = nil) and (parObject <> nil) and (parObject is TcMetaData) then
  begin
    result := TcData.Create(self);
    (result as TcData).SetMetaData(parObject as TcMetaData);
    (result as TcData).State := edsDB;
    Add(result);
  end;
end;

// TcData
//   GetDisplay method
//
function TcData.GetDisplay: TObject;
begin
  result := nil;
  if m_objMetaData <> nil then
  begin
    if (m_objMetaData.eType <> enObject) then
    begin
      if (parent <> nil) and (parent <> self) and (parent is TcData) then
        result := (parent as TcData).GetDisplay;
    end
    else
    begin
      result := m_objDisplay
    end;
  end;
end;

// TcData
//   Statement method
//
function TcData.Statement(eparSet: TeTypeSet; sparType: String): String;
begin
  result := GetScript(sparType);
  if result = ksEMPTY then
  try
    if m_objMetaData <> nil then
    begin
      result := m_objMetaData.Statement(self, eparSet, sparType);
      AddScript(sparType, result);
    end;
  except
    on E: Exception do
      m_objMetaData.SetError(elFatal, Format('''%s'': Statement processing failed: %s', [m_objMetaData.sName, E.Message]), m_objMetaData.XML);
  end;
end;

// TcData
//   GetScript method
//
function TcData.GetScript(value: String): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  p := m_lstScripts.Find(value);
  if p <> nil then
    result := p.sValue;
end;

// TcData
//   GetScriptHeader method
//
function TcData.GetScriptHeader: String;
var
  t, c: String;
  p: TcObject;
begin
  result := ksEMPTY;
  if eType = enObject then
  begin
    // Get Comment Marker
    c := '--';
    p := nil;
    if (m_objMetaData <> nil) and (m_objMetaData.TopParent <> nil) and  (m_objMetaData.TopParent is TcMetaData) then
      p := (m_objMetaData.TopParent as TcMetaData).Find(enOPTION, krsCOMMENT);
    if (p <> nil) and (p is TcMetaData) then
      c := trim(p.sValue);
    // Build Header
    result := Format('%s\n%s %s %s\n%s', [c, c, m_objMetaData.DisplayName, sValue, c]);
    t := GetDependencyHeader;
    if t <> ksEMPTY then
      result := result + Format('\n%s %s\n%s', [c, t, c]);
    result := result + '\n';
  end;
end;

// TcData
//   ClearScripts
//
procedure TcData.ClearScripts;
begin
  m_lstScripts.Clear;
end;

// TcData
//   AddScript method
//
function TcData.AddScript(sType, value: String): longint;
var
  p: TcObject;
begin
  p := m_lstScripts.Find(sType);
  if p = nil then
  begin
    p := TcObject.Create(nil);
    p.sName := sType;
    m_lstScripts.Add(p);
  end;
  p.sValue := value;
  result := m_lstScripts.IndexOf(p);
end;

// TcData
//   GetDependencyText method
//
function TcData.GetDependencyText(value: TeDirection; parFormat: TeOutputType): String;
const
  kasDIRECTION: array[TeDirection] of string =
    (krsMACRODEPENDENCIES_UPON, krsMACRODEPENDENCIES_TO);
var
  i, L: longint;
  p: TcObject;
  lst: TcBag;
begin
  if value = eFrom then
    lst := m_depFrom
  else
    lst := m_depTo;
  result := ksEMPTY;
  for i := 0 to lst.Count - 1 do
  begin
    p := lst[i];
    if (p <> nil) and (p.sValue <> ksEMPTY) then
    begin
      if result <> ksEMPTY then
      begin
        if i <> lst.Count - 1 then
          result := result + ', '
        else
          result := result + ' and ';
      end;
      if p is TcDataDependency then
        L := (p as TcDataDependency).iDepTag
      else
        L := p.Tag;
      case parFormat of
        eSQL:
          result := result + trim(Format('%s %s', [Initcap(p.sName), trim(p.sValue)]));
        eHTML:
          result := result + trim(Format('<a href="%s:%d">%s %s</a>', [krsSE_INSTANCE, L, Initcap(p.sName), trim(p.sValue)]));
      end;
    end;
  end;
  if result <> ksEMPTY then
    result := Format('%s %s. ', [kasDIRECTION[value], result]);
end;

// TcData
//   GetDependencyHeader method
//
function TcData.GetDependencyHeader: String;
begin
  result := GetDependencyText(eFrom, eHTML) + GetDependencyText(eTo, eHTML);
end;

// TcData
//   Find method (1)
//
function TcData.Find(eType: TeType; TypeValue: String): TcObject;
begin
  result := Find(eType, TypeValue, kiLARGEINT);
end;

// TcData
//   Find method (2)
//
function TcData.Find(eType: TeType; TypeValue: String; parDepth: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
    begin
      if (Objects[i].eType = eType) and (AnsiCompareText(Objects[i].sName, TypeValue) = 0) then
      begin
        result := Objects[i];
        break;
      end;
      if (result = nil) and (parDepth > 1) then
        result := (Objects[i] as TcData).Find(eType, TypeValue, parDepth - 1);
    end;
end;

// TcData
//   Display method
//
procedure TcData.Display;
var
  i: longint;
begin
  try
    if Assigned((TopParent as TcData).m_procDisplay) and (m_objMetaData <> nil) and IsHeader then
      (TopParent as TcData).m_procDisplay(self);
    for i := 0 to count - 1 do
      (Objects[i] as TcData).Display;
  except
    //
  end;
end;

// TcData
//   GetRuleErrorLevel method
//
function TcData.GetRuleErrorLevel: TeRuleErrorLevel;
var
  i: longint;
begin
  result := relNone;
  for i := 0 to m_lstRuleProcessing.Count - 1 do
    if (m_lstRuleProcessing[i] as TcRuleResult).eLevel > result then
      result := (m_lstRuleProcessing[i] as TcRuleResult).eLevel;
end;

// TcData
//   SetRuleResult method
//
function TcData.SetRuleResult(objRule: TcMetaData; Value: String): TcRuleResult;
var
  q: TcData;
begin
  result := nil;
  if objRule <> nil then
  begin
    result := TcRuleResult.Create(self);
    q := HeaderObject;
    if q <> nil then
      result.sName := q.Header;
    result.Rule := objRule;
    result.eLevel := StringToRuleErrorLevel(objRule.Attribute[krsTYPE]);
    result.sValue := Value;
    m_lstRuleProcessing.Add(result);
  end;
end;

// TcData
//   ClearRuleResult method
//
procedure TcData.ClearRuleResult;
var
  i: longint;
begin
  // 1. Delete Rules
  m_lstRuleProcessing.Clear;
  // 2. Process children
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
      (Objects[i] as TcData).ClearRuleResult;
end;

// TcData
//   GetHeader method
//
function TcData.GetHeader: String;
begin
  result := sValue;
end;

// TcData
//   GetIsHeader method
//
function TcData.GetIsHeader: boolean;
begin
  result := (m_objMetaData <> nil) and (eType = enObject);
end;

// TcData
//   SetMetaData
//
procedure TcData.SetMetaData(Value: TcMetaData);
var
  L: longint;
begin
  //if m_objMetaData <> Value then
  begin
    if m_objMetaData <> nil then
    begin
      L := m_objMetaData.m_lstIndex.IndexOfObject(self);
      if L <> kiUNDEFINED then
        m_objMetaData.m_lstIndex.Delete(L);
    end;
    if value <> nil then
    begin
      value.m_lstIndex.AddObject(sValue, self);
      sName := Value.sName;
      eType := Value.eType;
    end;
    m_objMetaData := Value;
  end;
end;

// TcData
//   SetDependencies method
//   >> Recursively, establish all data dependencies.
//
procedure TcData.SetDependencies;
var
  i, j: longint;
  p, q, r, t: TcObject;
begin
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
    begin
      //
      // A. Process dependencies for this node.
      if ((Objects[i] as TcData).MetaData <> nil) and ((Objects[i] as TcData).MetaData.MetaDependencies.Count > 0) then
      begin
        p := GetUser;
        if (p <> nil) and (p is TcData) and (p as TcData).IsLoaded then
          for j := 0 to (Objects[i] as TcData).MetaData.MetaDependencies.Count - 1 do
          begin
            q := TcObject((Objects[i] as TcData).MetaData.MetaDependencies.Objects[j]);
            if (q <> nil) and (q is TcMetaData) then
            begin
              r := (p as TcData).FindInstance(q as TcMetaData, Objects[i].sValue);
              if r <> nil then
              begin
                t := Ancestor(enObject);
                if (t <> nil) and (t is TcData) then
                  (t as TcData).AddDependency(r.Ancestor(enObject), eFrom);
              end
              else
                m_objMetaData.SetError(elFatal, Format('%s %s: From Dependency %s %s Not Found.', [sName, sValue, q.sName, Objects[i].sValue]), ksEMPTY);
            end;
          end;
      end;
      //
      // B. Process Recursively down.
      if Objects[i].eType in [enObject, enItem] then
        (Objects[i] as TcData).SetDependencies;
    end;
end;

// TcData
//   GetHeaderObject method
//
function TcData.GetHeaderObject: TcData;
begin
  result := nil;
  if IsHeader then
    result := self
  else if (Parent <> nil) and (Parent is TcData) and (self <> parent) then
    result := (Parent as TcData).GetHeaderObject;
end;

// TcData
//   Translate method
//
procedure TcData.Translate(value: TcData);

  //
  // Phase 1 - Create objects from what can be transfered
  procedure Phase1(parData: TcData; parMetaData: TcMetaData; parTranslated: TcData);
  var
    m: TcMetaData;
    i: longint;
    p: TcData;
  begin
    m := nil;
    if (parTranslated <> nil) and (parTranslated.MetaData <> nil) then
      m := parMetaData.Find([enObject, enField, enItem], parTranslated.MetaData.sName) as TcMetaData;
    if m <> nil then
    begin
      parData.MetaData := m;
      parData.sValue := parTranslated.sValue;
      parData.m_objAncestor := parTranslated;
      parData.IsLoaded := TRUE;
      for i := 0 to parTranslated.Count - 1 do
      begin
        p := TcData.Create(parData);
        parData.Add(p);
        Phase1(p, m, parTranslated[i] as TcData);
        if p.MetaData = nil then
          parData.Delete(p);
      end;
    end;
  end;

  //
  // Phase 2 - Re-establish relationships
  procedure Phase2(parData: TcData);
  var
    i: longint;
    p, q: TcData;
    m: TcMetaData;
  begin
    // Establish relationships
    if (parData.m_objAncestor <> nil) and (parData.m_objAncestor is TcData) then
    begin
      p := parData.m_objAncestor as TcData;
      for i := 0 to p.m_depFrom.Count - 1 do
        if (p.m_depFrom[i] <> nil) and (p.m_depFrom[i] is TcDataDependency) then
        begin
          m := m_objMetaData.Find(p.m_depFrom[i].sName) as TcMetaData;
          q := nil;
          if m <> nil then
            q := FindInstance(m, p.m_depFrom[i].sValue);
          if q <> nil then
            parData.AddDependency(q, eFrom);
        end;
    end;
    // Browse children
    for i := 0 to parData.Count - 1 do
      if (parData[i] <> nil) and (parData[i] is TcData) then
        Phase2(parData[i] as TcData);
  end;

  //
  // Phase 3 - Process value translations
  procedure Phase3(parData: TcData);
  var
    i: longint;
  begin
    // Verify value translations
    with parData do
      sValue := MetaData.Translate(sValue);
    // Browse children
    for i := 0 to parData.Count - 1 do
      if (parData[i] <> nil) and (parData[i] is TcData) then
        Phase3(parData[i] as TcData);
  end;

begin
  Phase1(self, m_objMetaData, value);   // Create objects
  Phase2(self);                         // Create dependency lists
  SetDependencies;
  Phase3(self);                         // Process value translations
end;

// TcData
//   SetValue
//
procedure TcData.SetValue(value: String);
begin
  if m_objMetaData <> nil then
    value := m_objMetaData.ApplyTransformation(value);
  // Modified Flag
  if (AnsiCompareText(value, inherited sValue) <> 0) and (edsModified > m_eState) then
    SetState(edsModified);
  // Inherited function
  inherited SetValue(value);
  // Make sure indxing is set
  SetMetaData(m_objMetaData);
end;

// TcData
//   ProcessSQL method
//
function TcData.ProcessSQL(eparSet: TeTypeSet; sparType: String): boolean;
var
  e: TcExecute;
  s: String;
begin
  e := nil;
  s := Statement(eparSet, sparType);
  result := s = ksEMPTY;
  if not result then
  try
    e := TcExecute.Create(nil);
    e.Connection := m_objMetaData.Connection;
    e.FetchType := StringToFetchType(m_objMetaData.Option[krsOPTION_FETCHTYPE]);
    try
      e.Execute(s, m_objMetaData.sName);
      result := TRUE;
    except
      on E:Exception do
        m_objMetaData.SetError(elSQL, E.Message, s);
    end;
  finally
    e.free;
  end;
end;

// TcData
//   SetState
//
procedure TcData.SetState(value: TeState);
var
  i: longint;
begin
  m_eState := value;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
      (Objects[i] as TcData).SetState(value);
end;

// TcData
//   GetState
//
function TcData.GetState: TeState;
begin
  result := m_eState;
end;

// TcData
//   CascadeValue
//
procedure TcData.CascadeValue(strMetaName, strValue: String);
var
  i: longint;
begin
  if AnsiCompareText(sName, strMetaName) = 0 then
    sValue := strValue;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
      (Objects[i] as TcData).CascadeValue(strMetaName, strValue);
end;

// TcData
//   Retrofit
//
procedure TcData.Retrofit(parObject: TcData);
var
  i: longint;
  p: TcObject;
begin
  sValue := parObject.sValue;
  // Copy or Delete
  for i := count - 1 downto 0 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
    begin
      p := parObject.Find(Objects[i].Tag, 1);
      if (p <> nil) and (p is TcData) and ((p as TcData).State <> edsDeleted) then
        (Objects[i] as TcData).Retrofit(p as TcData)
      else
        Delete(i);
    end;
  // Copy or Add
  for i := parObject.count - 1 downto 0 do
    if (parObject[i] <> nil) and (parObject[i] is TcData) then
    begin
      p := Find(parObject[i].Tag, 1);
      if (p = nil) and ((parObject[i] as TcData).State <> edsDeleted) then
      begin
        p := TcData.Create(self);
        Add(p);
        p.Copy(parObject[i]);
      end
      else if (p <> nil) and ((p as TcData).State = edsDeleted) then
        Delete(p.ParentIndex);
    end;
end;

// TcData
//   DescendAncestorValues
//
procedure TcData.DescendAncestorValues;
var
  a, p: TcData;
  m: TcObject;
  i: longint;
begin
  if (parent <> nil) and (parent is TcData) and (m_objMetaData <> nil) then
  begin
    a := parent as TcData;
    for i := 0 to a.count - 1 do
      if (a[i] <> nil) and (a[i] is TcData) and (a[i] <> self) then
      begin
        m := m_objMetaData.Find(a[i].sName, 1);
        if (m <> nil) and (m is TcMetaData) and (Find(a[i].sName, 1) = nil) then
        begin
          p := TcData.Create(self);
          Add(p);
          p.MetaData := m as TcMetaData;
          p.State := edsAdded;
          p.sValue := a[i].sValue;
        end;
      end;
  end;
end;

// TcData
//   RemoveDependency
//
procedure TcData.RemoveDependency;
begin
  //
end;

// TcData
//   IsChildOf method
//
function TcData.IsChildOf(value: TcObject): boolean;
begin
  result := self = value;
  if not result and (value <> nil) and (Parent <> nil) and (parent is TcData) then
    result := (parent as TcData).IsChildOf(value);
end;

// TcData
//   GetObjectID method
//
function TcData.GetObjectID: String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  if (eType = enObject) and (m_objMetaData <> nil) and (m_objMetaData.ObjectID <> ksEMPTY) then
  begin
    p := Find(m_objMetaData.ObjectID, 1);
    if p <> nil then
      result := p.sValue;
  end
  else if (Parent <> nil) and (Parent is TcData) then
    result := (Parent as TcData).GetObjectID;
end;

// TcData
//   SetObjectID method
//
procedure TcData.SetObjectID(value: String);
var
  p: TcObject;
begin
  if (m_objMetaData <> nil) and (m_objMetaData.ObjectID <> ksEMPTY) then
  begin
    p := Find(m_objMetaData.ObjectID, 1);
    if p <> nil then
      p.sValue := value;
  end;
  sValue := value;
end;

// TcData
//   Add method
//
function TcData.Add(parObject: TcObject): longint;
begin
  result := inherited Add(parObject);
  //SetIndex(self, eiaAdd);
end;

// TcData
//   FindInstance
//
function TcData.FindInstance(parMeta: TcMetaData; parValue: String): TcData;
var
  L: longint;
begin
  result := nil;
  L := parMeta.m_lstIndex.IndexOf(parValue);
  if L <> kiUNDEFINED then
    while (L <= parMeta.m_lstIndex.Count - 1) and (AnsiCompareStr(parMeta.m_lstIndex[L], parValue) = 0) do
    begin
      if Has(TcObject(parMeta.m_lstIndex.Objects[L])) then
      begin
        result := TcData(parMeta.m_lstIndex.Objects[L]);
        break;
      end
      else
        inc(L);
    end;
end;

// TcData
//   Load method
//
function TcData.Load(parMetaData: TcMetaData; parFlags: TeLoadFlagSet): boolean;
var
  v: TcValuePairSet;
  e: TeTypeSet;
begin
  result := HasAllLoaded(parMetadata) and not (elfRefresh in parFlags);
  if not result then
  begin
    if not (elfAll in parFlags) then
      v := Arguments;
    e := [enObject, enItem, enSQL, enOPENSCHEMA];
    if (elfSkim in parFlags) then
      e := [enObject, enSQL, enOPENSCHEMA];
    result := ReverseEngineer(parMetaData, v, e, parFlags);
    ClearScripts;
  end;
end;

// TcData
//   ReverseEngineer (3)
//
function TcData.ReverseEngineer(parMetaData: TcMetaData; parArray: array of TcValuePair; parSet: TeTypeSet; parFlags: TeLoadFlagSet): boolean;

  // Tools
  //   FindByMetaData
  //
  function FindByMetaData(parData: TcData; objMeta: TcMetaData; sFieldValue: String): TcData;
  var
    i: longint;
  begin
    result := nil;
    if (parData.MetaData = objMeta) and (AnsiCompareText(parData.sValue, sFieldValue) = 0) then
      result := parData
    else for i := 0 to parData.Count - 1 do
    begin
      result := FindByMetaData(parData[i] as TcData, objMeta, sFieldValue);
      if result <> nil then
        break;
    end;
  end;

  // Tools
  //   FindInstanceObject
  //
  function FindInstanceObject(parParent: TcData; objMeta: TcMetaData; sFieldName, sFieldValue: String): TcData;
  var
    p: TcObject;
    q: TcData;
    L: longint;
  begin
    // Remove multipart '.' identifier expression
    while pos('.', sFieldName) <> 0 do
      sFieldName := system.Copy(sFieldName, pos('.', sFieldName) + 1, length(sFieldName));
    //
    result := nil;
    p := objMeta.Find([enField], sFieldName, 1);
    if (p <> nil) and (p is TcMetaData) then
    begin
      L := (p as TcMetaData).m_lstIndex.IndexOf(sFieldValue);
      if L <> kiUNDEFINED then
        while (L <= (p as TcMetaData).m_lstIndex.Count - 1) and (AnsiCompareText((p as TcMetaData).m_lstIndex[L], sFieldValue) = 0) do
        begin
          q := TcData((p as TcMetaData).m_lstIndex.Objects[L]);
          if parParent.Has(q) then
          begin
            result := q;
            if (result.MetaData <> objMeta) and objMeta.Has(result.MetaData) then
              while (objMeta <> result.MetaData) and (result <> nil) and (result.parent is TcData) do
                result := result.parent as TcData;
            break;
          end
          else
            inc(L);
        end;
    end;
  end;


  // Tool
  //   FieldToPosition
  //   Returns the field position by Meta Name within a SQL Tag, given the <MAP> tags.
  //
  function FieldToPosition(objSQL: TcMetaData; value: String): longint;
  var
    i: longint;
  begin
    result := kiUNDEFINED;
    for i := 0 to objSQL.Count - 1 do
      if (objSQL[i] <> nil) and
         (objSQL[i] is TcMetaData) and
         (objSQL[i].eType = enMap) and
         (AnsiCompareText(objSQL[i].sValue, value) = 0) then
      begin
        result := (objSQL[i] as TcMetaData).iPosition;
        break;
      end;
  end;

  // Tool
  //   PositionToMeta
  //   Returns the meta name given a field position .
  //
  function PositionToMeta(objSQL: TcMetaData; value: longint): String;
  var
    i: longint;
  begin
    result := ksEMPTY;
    for i := 0 to objSQL.Count - 1 do
      if (objSQL[i] <> nil) and
         (objSQL[i] is TcMetaData) and
         (objSQL[i].eType = enMap) and
         ((objSQL[i] as TcMetaData).iPosition = value) then
      begin
        result := objSQL[i].sValue;
        break;
      end;
  end;

  // Tools
  //   FindByField
  //   Locate a meta data element given a name
  function FindByField(objMeta: TcMetaData; value: String): TcObject;
  var
    i: longint;
  begin
    result := nil;
    for i := 0 to objMeta.Count - 1 do
      if (objMeta[i] <> nil) and
         (objMeta[i] is TcMetaData) and
         ((objMeta[i] as TcMetaData).eType = enField) and
         (
           // Straight Name comparison
           (AnsiCompareText(objMeta[i].sName, value) = 0) or
           // Alias comparison
           (
             ((objMeta[i] as TcMetaData).sAlias <> ksEMPTY) and
             (AnsiCompareText((objMeta[i] as TcMetaData).sAlias, value) = 0)
           )
         ) then
      begin
        result := objMeta[i];
        break;
      end;
  end;

  // Tools
  //   FindField
  //   Locate a record column given a meta data element
  //
  function FindField(lst: TStringList; objMeta, objSQL: TcMetaData): longint;
  var
    i: longint;
  begin
    result := kiUNDEFINED;
    // Should we proceed by position, using <map> tags?
    if objSQL <> nil then
    begin
      i := FieldToPosition(objSQL, objMeta.sName);
      if i <> kiUNDEFINED then
        result := i;
    end;
    // Else, proceed by name
    if result = kiUNDEFINED then
      for i := 0 to lst.Count - 1 do
        if // Straight Name comparison
           (AnsiCompareText(lst[i], objMeta.sName) = 0) or
           // Alias comparison
           ( (objMeta.sAlias <> ksEMPTY) and (AnsiCompareText(lst[i], objMeta.sAlias) = 0) ) then
        begin
          result := i;
          break;
        end;
  end;

  // Tools
  //   CreateObject
  //
  function CreateObject(objMeta, objSQL: TcMetaData; objParent: TcObject; lstColumns: TStringList; Values: TcRecordSet; parValue: String = ksEMPTY): TcData;
  var
    L: longint;
    p: TcMetaData;
    d: TcData;
    s: String;
  begin
    result := nil;
    // If we have a instance data parent, then create a instance data child object.
    if (objParent <> nil) and (objParent is TcData) then
    begin
      d := nil;
      p := objMeta.MTOK;
      if p <> nil then
      begin
        L := lstColumns.IndexOf(p.sName);
        if L <> kiUNDEFINED then
        begin
          s := VarToStr(values.Fields(L));
          if s <> ksEMPTY then
            d := (objParent as TcData).FindInstance(p.m_objMTOK, s);
          if d <> nil then
            d := d.ParentObject([enObject, enItem]) as TcData;
        end;
      end;
      if d = nil then
        d := objParent as TcData;
      // Check for duplicate
      //if result = nil then
      begin
        result := TcData.Create(d);
        d.Add(result);
        result.sValue := _TextToXML(parValue);
        result.SetMetaData(objMeta);
      end;
    end;
  end;

  // Tools
  //   ProcessRecordSet
  //
  procedure ProcessRecordSet(parParent: TcData; objMeta, objSQL: TcMetaData; rs: TcRecordSet; sparStatement: String; parResult: TcBag);
  var
    vKey, vKey2, vKey3: OleVariant;
    i, j, L, F, k: longint;
    p, prn: TcData;
    c, m, q: TcObject;
    s, md: String;
    lstColumns: TStringList;
    ic: char;
  begin
    //
    // 1. Get Column Names
    lstColumns := nil;
    if (rs <> nil) and rs.IsOpen and not rs.EOF then
    try
      lstColumns := TStringList.Create;
      for i := 0 to rs.FieldCount - 1 do
        lstColumns.Add(rs.FieldName(i));
      //
      // 2. If there is a key to match data against, then check if key is legal
      vKey := ksEMPTY;
      if objSQL.HasAttribute[krsKEY] then
      begin
        vKey := objSQL.Attribute[krsKEY];
        if FieldToPosition(objSQL, VarToStr(vKey)) <> kiUNDEFINED then
          vKey := FieldToPosition(objSQL, VarToStr(vKey));
        if (VarToStr(vKey) = ksEMPTY) then
        begin
          objMeta.SetError(elWarning, Format('Object ''%s'': Key ''%s'' for %s does not match any of the query fields', [objMeta.sName, VarToStr(vKey), sparStatement]), objSQL.sValue);
          vKey := ksEMPTY;
        end;
      end;
      //
      // 3. Verify the query
      if AnsiCompareText(m_objMetaData.Option[krsOPTION_MATCHWARNING], krsFALSE) <> 0 then
        for j := 0 to lstColumns.Count - 1 do
          if (FieldToPosition(objSQL, lstColumns[j]) = kiUNDEFINED) and
             (FindByField(objMeta, lstColumns[j]) = nil) then
            objMeta.SetError(elWarning, Format('Object ''%s'': Query field ''%s'' for %s does not match to any object field', [objMeta.sName, lstColumns[j], sparStatement]), objSQL.sValue);
      //
      // 4. Is there a clause to the query?
      c := objSQL.Find(enClause, ksEMPTY);
      if c <> nil then
      begin
        objSQL.m_objRS := rs;
        objSQL.hdlQuery := objSQL.FieldValue;
      end;
      //
      // 5. Fetch query
      if (rs <> nil) and not rs.EOF then
        while not rs.EOF do
        begin
          Application.ProcessMessages;
          // 4.1. Is the result within a clause, if such clause exists?
          if (c = nil) or ((c <> nil) and (m_objMetaData.Expression(c.sValue, nil, objSQL, ksEMPTY, []) = TRUE)) then
          begin
            p := nil;
            prn := parParent;
            // 4.2. Find the right parent
            if (objMeta.Parent <> nil) and
               (objMeta.Parent is TcMetaData) and
               ((prn.Metadata <> objMeta) and (prn.Metadata <> objMeta.Parent)) and
               (objMeta.MTOK <> nil) then
            begin
              vKey2 := objMeta.MTOK.sName;
              vKey3 := vKey2;
              if objMeta.MTOK.m_objMTOK <> nil then
                vKey3 := objMeta.MTOK.m_objMTOK.sName;
              if not rs.HasField(vKey2) then
                vKey2 := FieldToPosition(objSQL, objMeta.MTOK.sName);
              q := nil;
              if VarToStr(vKey2) <> ksEMPTY then
                q := FindInstanceObject(prn, objMeta.Parent as TcMetaData, VarToStr(vKey3), VarToStr(rs.Fields(VarToStr(vKey2))));
              if (q <> nil) and (q is TcData) then
                prn := q as TcData;
              if q = nil then
                objMeta.SetError(elSQL, Format('''%s'' SQL Statement, MTOK ''%s'' reference instance data does not map to parent ''%s''', [objSQL.sName, vKey2, vKey3]), ksEMPTY);
            end;
            // 4.2. Has it been already created?
            if VarToStr(vKey) <> ksEMPTY then
            begin
              if VarIsOrdinal(vKey) then
                L := strtointdef(VarToStr(vKey), kiUNDEFINED)
              else
                L := lstColumns.IndexOf(vKey);
              if L <> kiUNDEFINED then
                p := FindInstanceObject(prn, objMeta, objSQL.Attribute[krsKEY], VarToStr(rs.Fields(L)));
              // if not found, would it be self?
              if (p = nil) and (prn <> nil) and (prn.MetaData = objMeta) and (AnsiCompareText(prn.sValue, VarToStr(rs.Fields(L))) = 0) then
                p := prn;
            end;
{$IFDEF SBS_DEBUG}
            // Check parentship
            if (prn <> nil) and (prn.MetaData <> objMeta.Parent) then
              LogToFile('Wrong Parentship.txt', Format('%s > %s %s is not %s', [objSQL.sName, prn.MetaData.sName, prn.sValue, objMeta.sName]) + ksCR);
{$ENDIF}
            // 4.3. If Not, is it already existing?
            if (p = nil) and rs.HasField(objMeta.ObjectID) then
            begin
              s := VarToStr(rs.Fields(objMeta.ObjectID));
              if (p = nil) and (prn.MetaData = objMeta) and (prn.sValue = s) then
                p := prn;
              if (p = nil) and (objMeta.ObjectID <> ksEMPTY) and rs.HasField(objMeta.ObjectID) then
                p := FindInstanceObject(prn, objMeta, objMeta.ObjectID, s);
            end;
            // 4.4. Not. Let's create it.
            if p = nil then
            begin
              p := CreateObject(objMeta, objSQL, prn, lstColumns, rs);
              // Let's fill the sValue using the NAME field, if existing.
              if (p <> nil) and (objMeta.ObjectID <> ksEMPTY) then
              begin
                if rs.HasField(objMeta.ObjectID) then
                  p.sValue := _TextToXML(VarToStr(rs.Fields(objMeta.ObjectID)))
                else
                begin
                  F := FieldToPosition(objSQL, objMeta.ObjectID);
                  if F <> kiUNDEFINED then
                    p.sValue := _TextToXML(VarToStr(rs.Fields(F)));
                end;
              end;
            end;
            // 4.5.
            for j := 0 to lstColumns.Count - 1 do
            begin
              // Get field name, either by position, or straight from column name
              md := PositionToMeta(objSQL, j);
              if md = ksEMPTY then
                md := lstColumns[j];
              //
              m := FindByField(objMeta, md);
              if (m <> nil) and (m is TcMetaData) then
              begin
                //
                // a. Get a value from the query
                s := VarToStr(rs.Fields(j));
                // Get an item if the meta data says so: for example, ITEM="4,~"
                if (m as TcMetaData).sItem <> ksEMPTY then
                begin
                  ic := Item((m as TcMetaData).sItem, ',', 1)[1];
                  k := strtointdef(Item((m as TcMetaData).sItem, ',', 0), kiUNDEFINED);
                  if (pos(ic, s) > 0) and (k <> kiUNDEFINED) then
                    s := Item(s, ic, k);
                end;
                // Proceed to value translation, if appropriate
                s := (m as TcMetaData).Translate(s);
                //
                // b. Find Instance data field with 'md' name
                q := p.Find(enField, m.sName);
                // If not found, create it.
                if (q = nil) and (s <> ksEMPTY) then
                  {q := }CreateObject(m as TcMetaData, objSQL, p, nil, rs, s)
                // Else, update the value if existing
                else if (q <> nil) and (q is TcData) then
                  (q as TcData).sValue := _TextToXML(s);
              end;
            end;
            if p <> nil then
              parResult.Add(p);
          end;
          //parParent.Display;
          rs.MoveNext;
        end;
    finally
      lstColumns.Free;
    end;
    rs.Close;
    // 6. Reset Query Identifier Handler.
    objSQL.hdlQuery := nil;
    if objSQL.m_objRS <> nil then
      objSQL.m_objRS.free;
    objSQL.m_objRS := nil;
  end;

  // Tools
  //   ProcessOpenSchema
  //
  procedure ProcessOpenSchema(parParent: TcData; objMeta, objSQL: TcMetaData; iparType: longint; parArray: array of TcValuePair; parResult: TcBag);
  var
    e: TcExecute;
    rs: TcRecordSet;
    s: String;
    v: OLEVariant;
  begin
    // A.1 Check that meta data can support operation
    if (length(parArray) > 0) and (objMeta.ObjectID = ksEMPTY) then
      objMeta.SetError(elFatal, krsMETADATAMISSINGID, Format('OpenSchema(%s)', [kasSCHEMAENUM[iparType]]))
    else
    begin
      // A.2. Execute the SQL statement.
      Application.Hint := Format('Processing OpenSchema for %s', [objMeta.Description]);
      Application.ProcessMessages;
      e := nil;
      try
        e := TcExecute.Create(nil);
        e.Connection := objMeta.GetConnection;
        e.FetchType := StringToFetchType(objMeta.Option[krsOPTION_FETCHTYPE]);
        try
          // Process query
          v := Unassigned;
          if GetArgumentValue(parArray, '$' + krsID) <> ksEMPTY then
            v := GetArgumentValue(parArray, '$' + krsID);
          rs := nil;
          try
            rs := e.OpenSchema(kiaSCHEMAENUM[iparType], v, objSQL.sName);
            // Check for errors
            s := e.Error;
            if s <> ksEMPTY then
              objMeta.SetError(elSQL, s, Format('OpenSchema(%s)', [kasSCHEMAENUM[iparType]]));
            // Process the query if successful
            if rs <> nil then
              ProcessRecordSet(parParent, objMeta, objSQL, rs, Format('OpenSchema(%s)', [kasSCHEMAENUM[iparType]]), parResult);
          finally
            rs.free;
          end;
        except
          on E: Exception do
            objMeta.SetError(elSQL, E.Message, Format('OpenSchema(%s)', [kasSCHEMAENUM[iparType]]));
        end;
      finally
        e.free;
      end;
    end;
  end;

  //
  // ArrayAsString
  //
  function ArrayAsString(parArray: array of TcValuePair): String;
  var
    i: longint;
  begin
    result := ksEMPTY;
    for i := low(parArray) to high(parArray) do
      result := result + Format(' [%s=''%s'']', [parArray[i].name, parArray[i].value]);
  end;

  //
  // Process Straight Query
  //
  procedure ProcessQuery(parParent: TcData; objMeta, objSQL: TcMetaData; sparSQL: String; parArray: array of TcValuePair; parResult: TcBag);
  var
    e: TcExecute;
    rs: TcRecordSet;
    s, sSQL, nf: String;
    i: longint;
  begin
    // A.1. Parse the SQL statement.
    sSQL := VarToStr(objMeta.Expression(sparSQL, parParent, objMeta.Parent as TcMetaData, ksEMPTY, parArray));
    // A.2 Check that meta data can support operation
    if (length(parArray) > 0) and (objMeta.ObjectID = ksEMPTY) and (pos('$' + krsID, sSQL) > 0) then
      objMeta.SetError(elFatal, krsMETADATAMISSINGID, sSQL)
    // A.3. Execute the SQL statement.
    else if sSQL <> ksEMPTY then
    begin
      e := nil;
      try
        e := TcExecute.Create(nil);
        e.Connection := objMeta.GetConnection;
        e.FetchType := StringToFetchType(objMeta.Option[krsOPTION_FETCHTYPE]);
        try
          // Get name field
          nf := krsNAME;
          if objSQL.DisplayName <> ksEMPTY then
            nf := objSQL.DisplayName;
          // Process query
          rs := nil;
          try
            rs := e.Execute(sSQL, objSQL.sName + ArrayAsString(parArray));
            // Check for errors
            s := e.Error;
            if s <> ksEMPTY then
            begin
              // Set Error
              if objSQL.Find(enError, ksEMPTY) = nil then
                objMeta.SetError(elSQL, s, sSQL)
              else
                for i := 0 to objSQL.Count - 1 do
                  if (objSQL[i] <> nil) and (objSQL[i] is TcMetaData) and (objSQL[i].eType = enError) then
                    objMeta.SetError(elSQL, objSQL.Expression(objSQL[i].sValue, parParent, objSQL[i] as TcMetaData, ksEMPTY, []), sSQL);
            end
            // Process the query if successful
            else if rs <> nil then
              ProcessRecordSet(parParent, objMeta, objSQL, rs, sSQL, parResult);
          finally
            rs.free;
          end;
        except
          on E: Exception do
            objMeta.SetError(elSQL, E.Message, sSQL);
        end;
      finally
        e.free;
      end;
    end;
  end;

  //
  // Process Field Functions
  //
  procedure ProcessFunctions(objMeta: TcMetaData);
  var
    i, j: longint;
    q, a: TcObject;
    m: TcMetadata;
  begin
    // Is there a function to process?
    if (objMeta.eType = enField) and (objMeta.Parent <> nil) and (objMeta.Parent is TcMetaData) and objMeta.Has(enScript, ksEMPTY) then
    begin
      m := objMeta.Ancestor(enObject) as TcMetaData;
      for i := 0 to m.m_lstIndex.Count - 1 do
      begin
        // Get Parent TcData
        q := TcData(m.m_lstIndex.Objects[i]);
        // Find Aggregate Data. If not, create it.
        a := q.Find(objMeta.sName, 1);
        if a = nil then
        begin
          a := TcData.Create(q);
          (a as TcData).SetMetaData(objMeta);
          q.Add(a);
        end;
        // Process script
        a.sValue := ksEMPTY;
        for j := 0 to objMeta.Count - 1 do
          if (objMeta[j] <> nil) and
             (objMeta[j].eType = enScript) and
             (objMeta[j] is TcMetaData) and
             ((objMeta[j] as TcMetaData).Attribute[krsTYPE] = ksEMPTY) and
             (objMeta[j].sValue <> ksEMPTY) then
            a.sValue := TextToText((objMeta.Parent as TcMetaData).Expression(objMeta[j].sValue, q as TcData, objMeta.Parent as TcMetaData, ksEMPTY, []));
      end;
    end;
    // Browse children
    for i := 0 to objMeta.Count - 1 do
      if (objMeta[i] <> nil) and (objMeta[i] is TcMetaData) then
        ProcessFunctions(objMeta[i] as TcMetaData);
  end;

  //
  // Process Field Functions
  //
  procedure RemoveTemporary(value: TcData);
  var
    i: longint;
  begin
    for i := value.Count - 1 downto 0 do
      if (value[i] <> nil) and (value[i] is TcData) then
      begin
        if (emTemporary in (value[i] as TcData).MetaData.eFlags) then
          value.Delete(i)
        else
          RemoveTemporary(value[i] as TcData);
      end;
  end;

  //
  // ProcessDependencies
  //
  function ProcessDependencies(pardata: TcData; parMetaData: TcMetaData): boolean;
  var
    i: longint;
    e: TcExecute;
    rs: TcRecordSet;
    sSQL: String;
    p, q: TcObject;
  begin
    result := FALSE;
    e := nil;
    //if parData.IsLoaded then
    try
      e := TcExecute.Create(nil);
      e.Connection := parMetaData.GetConnection;
      e.FetchType := StringToFetchType(parMetaData.Option[krsOPTION_FETCHTYPE]);
      for i := 0 to parMetaData.Count - 1 do
        if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetaData) and (parMetaData[i].eType = enDependency) then
        begin
          rs := nil;
          try
            // A.1. Parse the SQL statement.
            sSQL := VarToStr(parMetadata.Expression(parMetaData[i].sValue, parData, parMetaData.Parent as TcMetaData, ksEMPTY, []));
            // execute SQL
            if sSQL <> ksEMPTY then
            try
              rs := e.Execute(sSQL, parMetaData[i].sName);
              if (rs <> nil) and rs.IsOpen and not rs.EOF then
              begin
                while not rs.EOF do
                begin
                  p := parData.Find(VarToStr(rs.Fields(0)), VarToStr(rs.Fields(1)), 1);
                  if p = nil then
                    parData.MetaData.SetError(elFatal, Format('%s %s: Object Not Found.', [VarToStr(rs.Fields(0)), VarToStr(rs.Fields(1))]), sSQL)
                  else if (p <> nil) and (p is TcData) then
                  begin
                    q := parData.Find(VarToStr(rs.Fields(2)), VarToStr(rs.Fields(3)), 1);
                    if q = nil then
                      parData.MetaData.SetError(elFatal, Format('%s %s: Object Not Found.', [VarToStr(rs.Fields(2)), VarToStr(rs.Fields(3))]), sSQL)
                    else if (q <> nil) and (q is TcData) then
                      (p as TcData).AddDependency(q, eFrom);
                  end;
                  rs.MoveNext;
                end;
                rs.Close;
              end;
            except
              on E: Exception do
                parMetaData.SetError(elSQL, E.Message, sSQL);
            end;
          finally
            rs.free;
          end;
        end;
    finally
      e.free;
    end;
  end;

  //
  // Sub
  //
  function Sub(pardata: TcData; parMeta: TcMetaData; parDepth: longint; parSet: TeTypeSet; parArray: array of TcValuePair): boolean;
  var
    i: longint;
    p: TcData;
    lst: TcBag;
  begin
    result := not (elfRefresh in parFlags) and pardata.HasAllLoaded(parMeta);
    lst := nil;
    if not result then
    try
      lst := TcBag.Create(nil);
      result := TRUE;
      for i := 0 to parMeta.Count - 1 do
        if (parMeta[i] <> nil) and (parMeta[i] is TcMetaData) and (parMeta[i].eType in parset) then
          case parMeta[i].eType of
            //
            //
            enObject:
              if (parDepth > 0) then
              begin
                if (parMeta[i] as TcMetaData).CountObjects([enSQL, enOpenSchema]) = 0 then
                begin
                  p := CreateObject(parMeta[i] as TcMetaData, nil, parData, nil, nil);
                  p.Display;
                  result := Sub(p, parMeta[i] as TcMetaData, parDepth, parSet, []);
                end
                else
                begin
                  if not (elfRecursive in parFlags) then
                    result := Sub(pardata, parMeta[i] as TcMetaData, parDepth - 1, parSet - [enObject, enItem], [])
                  else
                    result := Sub(pardata, parMeta[i] as TcMetaData, parDepth, parSet, [])
                end;
              end;
            //
            //
            enItem:
              result := Sub(pardata, parMeta[i] as TcMetaData, 0, parSet - [enObject], parArray);
            //
            //
            enSQL:
              if not (parMeta[i] as TcMetaData).HasAttribute[krsTYPE] and
                 not (parMeta[i] as TcMetaData).HasAttribute[krsMODE] then
                ProcessQuery(parData, parMeta, parMeta[i] as TcMetaData, parMeta[i].sValue, parArray, lst);
            //
            //
            enOPENSCHEMA:
              if (parMeta[i] as TcMetaData).HasAttribute[krsTYPE] then
                ProcessOpenSchema(parData, parMeta, parMeta[i] as TcMetaData, StringToSchemaEnum(((parMeta[i] as TcMetaData).Attribute[krsTYPE])), parArray, lst);
          end;
      //
      // IsLoaded Flag
      if (parDepth = 1) or (AnsiCompareText(parMeta.sName, krsUSER) = 0) then
        for i := 0 to lst.Count - 1 do
          if (lst[i] <> nil) and (lst[i] is TcData) then
          begin
            p := lst[i] as TcData;
            p.IsLoaded := p.IsLoaded or
                          (
                            ([elfSkim] * parFlags = []) and
                            (
                              ((p.m_objMetaData.CountObjects([enObject]) = 0) and (p.CountObjects([enItem, enField]) > 0)) or
                              ((p.m_objMetaData.CountObjects([enObject]) > 0) and (p.CountObjects([enObject, enItem]) > 0))
                            )
                          );
            p.Display;
          end;
      //
      SetState(edsDB);
    finally
      lst.free;
    end;
  end;

begin
  if (objDisplay <> nil) and (objDisplay is TTreeNode) then
    ((objDisplay as TTreeNode).TreeView as TTreeView).Items.BeginUpdate;
  if elfAll in parFlags then
    parMetaData.ClearScripts;
  result := Sub(self, parMetaData, 1, parSet, parArray);
  ProcessDependencies(self, parMetaData);
  ProcessFunctions(parMetaData);
  RemoveTemporary(self);
  SetDependencies;
  if (objDisplay <> nil) and (objDisplay is TTreeNode) then
    ((objDisplay as TTreeNode).TreeView as TTreeView).Items.EndUpdate;
end;

// TcData
//   AddDependency (1)
//
function TcData.AddDependency(parObject: TcObject; parDirection: TeDirection): longint;
const
  kesINVERSE_DIRECTION: array[TeDirection] of TeDirection = (eTo, eFrom);
begin
  result := AddDependency(parObject.sName, parObject.sValue, parObject.Tag, parDirection);
  if (parObject is TcData) then
    (parObject as TcData).AddDependency(sName, sValue, Tag, kesINVERSE_DIRECTION[parDirection]);
end;

// TcData
//   AddDependency (2)
//
function TcData.AddDependency(sparType, sparObject: String; iparTag: longint; parDirection: TeDirection): longint;
var
  p: TcDataDependency;
  lst: TcCollection;
  i: longint;
begin
  result := kiUNDEFINED;
  // Get a target collection
  lst := nil;
  case parDirection of
    eFrom:
      lst := m_depFrom;
    eTo:
      lst := m_depTo;
  end;
  // Browse that collection to see if element was not already added
  if lst <> nil then
    for i := 0 to lst.count - 1 do
      if (lst[i].sName = sparType) and (lst[i].sValue = sparObject) then
      begin
        result := i;
        break;
      end;
  // If not already added, then add it.
  if result = kiUNDEFINED then
  begin
    p := TcDataDependency.Create(self);
    result := lst.Add(p);
    p.sName := sparType;
    p.sValue := sparObject;
    p.iDepTag := iparTag;
  end;
end;

// TcData
//   GetArguments
//
function TcData.GetArguments: TcValuePairSet;
begin
  SetLength(result, length(result) + 1);
  result[length(result) - 1].name := krsDOLLARNAME;
  result[length(result) - 1].value := Header;
end;

// TcData
//   ClearChildren (1)
//
procedure TcData.ClearChildren;
begin
  inherited ClearChildren;
  m_bIsLoaded := FALSE;
  ClearScripts;
end;

// TcData
//   ClearChildren (2)
//
procedure TcData.ClearChildren(eparType: TeType; value: String);
var
  i: longint;
begin
  m_bIsLoaded := FALSE;
  for i := count - 1 downto 0 do
    if (Objects[i].eType = eparType) and ((value = ksEMPTY) or (value = Objects[i].sName)) then
      Delete(i);
  ClearScripts;
end;

// TcData
//   GetHint
//
function TcData.GetHint: String;
begin
  result := Format('%s %s', [m_objMetaData.DisplayName, sValue]);
  if GetDependencyHeader <> ksEMPTY then
    result := result + ksCR + GetDependencyHeader;
end;

// TcData
//   FindValue
//
function TcData.FindValue(sparType, sparValue: String; iparDepth: longint): TcObject;
var
  m: TcObject;
begin
  result := nil;
  m := m_objMetaData.Find(sparType);
  if (m <> nil) and (m is TcMetaData) then
    result := FindInstance(m as TcMetaData, sparValue);
end;

// TcData
//   GetIcon
//
function TcData.GetIcon: longint;
begin
  result := kiUNDEFINED;
  if m_objMetaData <> nil then
  begin
    result := longint(m_objMetaData.eIcon);
    if m_objMetaData.Has(enIcon, ksEMPTY) then
      result := strtointdef(m_objMetaData.Statement(self, [enIcon], ksEMPTY), result);
  end;
  if (result <> kiUNDEFINED) and m_bIsLoaded then
    result := result + longint(High(TeIcon)) - longint(low(TeIcon)) + 1;
end;

// TcData
//   GetIcon
//
function TcData.GetIsLoaded: boolean;
begin
  case eType of
    enUndefined, enItem:
      result := m_bIsLoaded;
    enObject:
      begin
        result := m_bIsLoaded;
        if result and (m_objMetaData.CountObjects([enObject]) > 0) then
          result := HasAllLoaded(m_objMetaData);
      end;
    else
      result := TRUE;
  end;
end;

// TcData
//   GetUser
//
function TcData.GetUser: TcData;
begin
  result := nil;
  if AnsiCompareText(m_objMetaData.sName, krsUSER) = 0 then
    result := self
  else if (parent <> nil) and (parent is Tcdata) then
    result := (parent as TcData).GetUser;
end;

// TcData
//   SetAlertLevel
//
procedure TcData.SetAlertLevel(value: longint);
begin
  m_iAlertLevel := value;
end;

// TcData
//   CompareSchemas
//
function TcData.CompareSchemas(value: TcData; parPlaceHolders: TStringList; parComparisonSet: TeDifferenceSet; parFilter: TcBag): TcObject;

  // Tools
  //   PlaceHolder_Replacement
  //
  procedure PlaceHolder_Replacement(parReference: TcData);
  var
    i: longint;
    s: String;
  begin
    // String replacements
    for i := 0 to parPlaceHolders.Count - 1 do
    begin
      s := parPlaceHolders.Names[i];
      parReference.sValue := AnsiReplaceText(parReference.sValue, s, parPlaceHolders.Values[s]);
    end;
    // Children depth-second search
    for i := 0 to parReference.Count - 1 do
      if (parReference[i] <> nil) and (parReference[i] is TcData) then
        PlaceHolder_Replacement(parReference[i] as TcData);
  end;

  // Tools
  //   IsPatternMatch
  //
  function IsPatternMatch(sparValue, parIgnored: String): boolean;
  var
    s: String;
  begin
    result := FALSE;
    if (parIgnored <> ksEMPTY) and (parIgnored[1] = '%') then
    begin
      s := system.Copy(parIgnored, 2, length(parIgnored) - 1);
      result := system.Copy(sparValue, length(sparValue) - length(s), length(s)) = s;
    end
    else if (parIgnored <> ksEMPTY) and (parIgnored[length(parIgnored)] = '%') then
    begin
      s := system.Copy(parIgnored, 1, length(parIgnored) - 1);
      result := system.Copy(sparValue, 1, length(s)) = s;
    end;
  end;

  // Tools
  //   LocalFind (1)
  //
  function LocalFind(parObject: TcData; eparType: TeType; sparName, sparValue: String; parResult: TcBag; parIgnored: String): TeDifference; overload;
  var
    i: longint;
    s1, s2: String;
  begin
    result := ecMissing;
    s2 := AnsiReplaceStr(sparValue, krsPLACEHOLDER_USER, sValue);
    parResult.Clear;
    for i := 0 to parObject.Count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcData) and (parObject[i].eType = eparType) and (AnsiCompareStr(parObject[i].sName, sparName) = 0) then
      begin
        result := ecDifference;
        s1 := AnsiReplaceStr((parObject[i] as TcData).sValue, krsPLACEHOLDER_USER, sValue);
        if (AnsiCompareStr(s1, s2) = 0) then
        begin
          parResult.Clear;
          result := ecMatch;
        end;
        parResult.Add(parObject[i]);
        if result = ecMatch then
          break;
      end;
  end;

  // Tools
  //   LocalFind (2)
  //
  function LocalFind(parObject: TcData; eparType: TeType; sparName: String; parResult: TcBag): longint; overload;
  var
    i: longint;
  begin
    parResult.Clear;
    for i := 0 to parObject.Count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcData) and (parObject[i].eType = eparType) and (parObject[i].sName = sparName) then
        parResult.Add(parObject[i]);
    result := parResult.Count;
  end;

  // Tools
  //   SubDifferences
  //
  procedure SubDifferences(parReference, parTarget: TcData; parResultSet: TcObject; parMode: TeDifference; eSet: TeTypeSet; parIgnored: String);

    function FindResultSet(parResultSet: TcObject; parValue: TcObject): TcDataDifferenceItem;
    var
      i: longint;
      p: TcDataDifference;
      v: TcObject;
    begin
      result := nil;
      // Find a data Difference element
      v := parValue.Ancestor(enObject);
      if (v <> nil) and (v is TcData) then
      begin
        p := nil;
        for i := 0 to parResultSet.Count - 1 do
          if (parResultSet[i].eType = v.eType) and (parResultSet[i].sName = (v as TcData).MetaData.DisplayName) and (parResultSet[i].sValue = v.sValue) then
          begin
            p := parResultSet[i] as TcDataDifference;
            break;
          end;
        // Not Found?
        if p = nil then
        begin
          p := TcDataDifference.Create(parResultSet);
          parResultSet.Add(p);
          p.eType := v.eType;
          p.sName := (v as TcData).MetaData.DisplayName;
          p.sValue := v.sValue;
        end;
        // Create a data difference item
        result := TcDataDifferenceItem.Create(p);
        p.Add(result);
      end;
    end;

  var
    i: Integer;
    q: TcObject;
    di: TcDataDifferenceItem;
    lst: TcBag;
    e: TeDifference;
  const
    kasSCRIPT: array[TeDifference] of String =
      (krsDROP, krsCREATE, krsMODIFY, ksEMPTY, ksEMPTY);
    kesModeToState: array[TeDifference] of TeState =
      (edsDeleted, edsAdded, edsModified, edsUndefined, edsUndefined);
  begin
    for i := 0 to parReference.Count - 1 do
      if (parReference[i] <> nil) and
         (parReference[i].eType in eSet) and
         (parReference[i] is TcData) and
         (parReference[i].sValue <> ksEMPTY) and
         ((parReference[i] as TcData).MetaData.eFlags * [emLocal] = []) and // Skip local fields
         (parFilter.IndexOf((parReference[i] as TcData).MetaData.Ancestor(enObject)) <> kiUNDEFINED) and
         ((parIgnored = ksEMPTY) or ((parIgnored <> ksEMPTY) and not IsPatternMatch(parReference[i].sValue, parIgnored))) then
      begin
        lst := nil;
        try
          lst := TcBag.Create(nil);
          case LocalFind(parTarget, parReference[i].eType, parReference[i].sName, parReference[i].sValue, lst, parIgnored) of
            //
            // Perfect Match
            ecMatch:
              SubDifferences(parReference[i] as TcData, lst[0] as TcData, parResultSet, parMode, eSet, parIgnored);

            //
            // Meta data Match
            ecDifference:
              begin
                e := parMode;
                if (e in [ecMissing, ecExtra]) and (parReference[i].eType = enField) then
                  e := ecDifference;
                if e in parComparisonSet then
                begin
                  di := FindResultSet(parResultSet, parReference[i]);
                  di.eDifference := e;
                  di.Reference := parReference[i];
                  (parReference[i] as TcData).eState := kesModeToState[e];
                  di.m_lstTarget.Copy(lst);
                  case e of
                    // Extra Item
                    ecExtra:
                    begin
                      di.Part[ediSQL] := TextToText((parReference[i] as TcData).Statement([enScript], kasSCRIPT[e]));
                      if di.Part[ediSQL] = ksEMPTY then
                        di.Part[ediSQL] := TextToText((parReference[i] as TcData).Statement([enScript], krsMODIFY));
                    end;
                    // Missing Item
                    ecMissing:
                    begin
                      q := (parReference[i] as TcData).ParentObject([enObject, enItem]) as TcData;
                      if (q <> nil) and (q is TcData) then
                      begin
                       (q as TcData).eState := kesModeToState[e];
                        di.Part[ediSQL] := TextToText((q as TcData).Statement([enScript], kasSCRIPT[e]));
                        if di.Part[ediSQL] = ksEMPTY then
                          di.Part[ediSQL] := TextToText((q as TcData).Statement([enScript], krsMODIFY));
                      end;
                    end;
                    // Item with Difference
                    ecDifference:
                    begin
                      q := parReference[i];
                      if (q <> nil) and not (q.eType in [enItem, enObject]) then
                        q := (parReference[i] as TcData).ParentObject([enObject, enItem]) as TcData;
                      if (q <> nil) and (q is TcData) then
                      begin
                       (q as TcData).eState := kesModeToState[e];
                       di.Part[ediSQL] := TextToText((q as TcData).Statement([enScript], kasSCRIPT[e]));
                       // If Empty, try parent, if itself an item.
                       if (di.Part[ediSQL] = ksEMPTY) and (q.Parent <> nil) and (q.eType = enItem) and (q.Parent.eType in [enItem, enObject]) and (q.parent is TcData) then
                         di.Part[ediSQL] := TextToText((q.Parent as TcData).Statement([enScript], kasSCRIPT[e]));
                      end;
                    end;
                  end;
                end;
              end;

            //
            // Node not found
            ecMissing:
              begin
                if parMode in parComparisonSet then
                begin
                  di := FindResultSet(parResultSet, parReference[i]);
                  di.eDifference := parMode;
                  di.Reference := parReference[i];
                  (parReference[i] as TcData).eState := kesModeToState[parMode];
                  q := (parReference[i] as TcData).ParentObject([enObject, enItem]) as TcData;
                  if (q <> nil) and (q is TcData) then
                  begin
                    (q as TcData).eState := edsModified;
                    di.Part[ediSQL] := TextToText((q as TcData).Statement([enScript], kasSCRIPT[parMode]));
                    if di.Part[ediSQL] = ksEMPTY then
                      di.Part[ediSQL] := TextToText((q as TcData).Statement([enScript], krsMODIFY));
                  end;
                end;
              end;
          end;

        finally
          lst.free;
        end;
      end;
  end;

var
  s: String;
begin
  result := TcObject.Create(nil);
  // Find if there are patterns to ignore
  s := m_objMetaData.GetOption(krsCOMPARISONPATTERNIGNORED);
  // Process place holder replacements
  PlaceHolder_Replacement(self);
  // Missing and different objects
  SubDifferences(value, self, result, ecExtra, [enObject, enItem, enField], s);
  // Extra and different objects
  SubDifferences(self, value, result, ecMissing, [enObject, enItem, enField], s);
end;

// TcData
//   GetFullName
//
function TcData.GetFullName(value: TeFullNameMode): String;
var
  s: String;
begin
  result := ksEMPTY;
  case value of
    efmIdentifier:
    begin
      if (parent <> nil) and (Parent is TcCustomData) then
        result := (Parent as TcCustomData).GetFullName(value);
      if sValue <> ksEMPTY then
      begin
        s := m_objMetaData.Option[krsOPTION_COLUMNDELIMITER];
        if (result <> ksEMPTY) and (sName <> ksEMPTY) then
          result := result + '.';
        result := result + format('%s%s%s', [Item(s, ',', 0), sValue, Item(s, ',', 1)]);
      end;
    end;
    efmExtended:
    begin
      if eType = enField then
        result := sName
      else
        result := Format('%s [%s]', [sName, sValue]);
      if (parent <> nil) and (parent is TcCustomData) and (Parent.Parent <> nil) then
        result := (parent as TcCustomData).GetFullName(value) + ', ' + result;
    end;
  end;
end;

// TcData
//   UpdateIcon
//
procedure TcData.UpdateIcon;
var
  i, L: longint;
  p: TTreeNode;
begin
  // Check that the current object (value) has the right display icons, etc.
  // Check *this node.
  if (eType = enObject) and (objDisplay <> nil) and (objDisplay is TTreeNode) then
  begin
    p := objDisplay as TTreeNode;
    if p.HasChildren <> (p.Count > 0) then
      p.HasChildren := p.Count > 0;
    L := Icon - 1;
    if p.ImageIndex <> L then
    begin
      p.ImageIndex := L;
      p.SelectedIndex := p.ImageIndex;
    end;
  end;
  // loop through children
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcData) then
      (Objects[i] as TcData).UpdateIcon;
end;

// TcData
//   HasAllLoaded
//
function TcData.HasAllLoaded(parMetaData: TcMetaData): boolean;
var
  i, L: longint;
  p: TcData;
begin
  result := TRUE;
  if m_objMetaData = parMetaData then
  begin
    result := m_bIsLoaded;
    if result and (parMetaData.eType = enObject) then
      for i := 0 to parMetaData.Count - 1 do
        if (parMetaData[i] <> nil) and (parMetaData[i] is TcMetadata) and (parMetaData[i].eType = enObject) then
        begin
          result := HasAllLoaded(parMetaData[i] as TcMetaData);
          if not result then
            break;
        end;
  end
  else if m_objMetaData.Has(parMetaData) then
  begin
    result := parMetaData.m_lstIndex.Count > 0;
    if result then
    begin
      L := 0;
      for i := 0 to parMetaData.m_lstIndex.Count - 1 do
        if (parMetaData.m_lstIndex.Objects[i] <> nil) and
           (Has(TcData(parMetaData.m_lstIndex.Objects[i]))) then
        begin
          inc(L);
          p := Tcdata(parMetaData.m_lstIndex.Objects[i]);
          if p <> self then
            result := result and p.HasAllLoaded(parMetaData);
          if not result then
            break;
        end;
      result := result and (L > 0);
    end;
  end;
end;

//
// TcRuleResult
//

// TcRuleResult
//   Create
//
constructor TcRuleResult.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objRule := nil;
end;

//
// TcMetaDataPreferences
//

// TcMetaDataPreferences
//   Load
//
function TcMetaDataPreferences.Load: boolean;
var
  Reg: TRegistry;
  lst: TStringList;
  i: longint;
  p: TcObject;
begin
  result := FALSE;
  if (Parent <> nil) and (Parent is TcMetaData) and ((parent as TcMetaData).Attribute[krsRDBMS] <> ksEMPTY) then
  begin
    Reg := nil;
    lst := nil;
    try
      lst := TStringList.Create;
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey(krsREGISTRYLOCATION + 'Preferences\' + (parent as TcMetaData).Attribute[krsRDBMS] + '\', True) then
      begin
        Reg.GetValueNames(lst);
        for i := 0 to lst.Count - 1 do
        begin
          p := TcObject.Create(self);
          p.sName := lst[i];
          p.eType := enObject;
          p.sValue := Reg.ReadString(p.sName);
          Add(p);
        end;
      end;
      Reg.CloseKey;
      result := TRUE;
    finally
      reg.free;
      lst.free;
    end;
  end;
end;

// TcMetaDataPreferences
//   Save
//
function TcMetaDataPreferences.Save: boolean;
var
  Reg: TRegistry;
  i: longint;
begin
  result := FALSE;
  if (Parent <> nil) and (Parent is TcMetaData) and ((parent as TcMetaData).Attribute[krsRDBMS] <> ksEMPTY) then
  begin
    Reg := nil;
    try
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey(krsREGISTRYLOCATION + 'Preferences\' + (parent as TcMetaData).Attribute[krsRDBMS] + '\', True) then
        for i := 0 to Count - 1 do
          if Objects[i] <> nil then
            Reg.WriteString(Objects[i].sName, Objects[i].sValue);
      Reg.CloseKey;
      result := TRUE;
    finally
      reg.free;
    end;
  end;
end;

// TcMetaDataPreferences
//   GetAsBoolean
//
function TcMetaDataPreferences.GetAsBoolean(index: String): boolean;
var
  p: TcObject;
begin
  result := TRUE;
  p := Find(index);
  if p <> nil then
    result := AnsiCompareText(p.sValue, krsTRUE) = 0;
end;

// TcMetaDataPreferences
//   SetAsBoolean
//
procedure TcMetaDataPreferences.SetAsBoolean(index: String; value: boolean);
var
  p: TcObject;
begin
  p := Find(index);
  if p = nil then
  begin
    p := TcObject.Create(self);
    p.sName := index;
    p.eType := enObject;
    Add(p);
  end;
  p.sValue := kasBOOL[value]
end;

// TcMetaDataPreferences
//   Has
//
function TcMetaDataPreferences.Has(value: String): boolean;
begin
  result := Find(value) <> nil;
end;

//
// TcDataDifference
//

// TcDataDifference
//   Text method
//
function TcDataDifference.Text: String;
begin
  result := format('%s %s', [sName, sValue]);
end;

// TcDataDifference
//   SendToObject method
//
function TcDataDifference.SendToObject(value: TObject): TObject;
var
  L: longint;
begin
  result := nil;
  // ptrObject is a TreeView
  if (value <> nil) and (value is TSourceListBox) then
  begin
     L := (value as TSourceListBox).Items.AddObject(trim(trim(trim(GetPart(ediHeader)) + ksCR + GetPart(ediDescription)) + ksCR + GetPart(ediSQL)), self);
    (value as TSourceListBox).LineIcon[L, 0] := longint(GetDifference);
  end;
end;

// TcDataDifference
//   GetDifference method
//
function TcDataDifference.GetDifference: TeDifference;
var
  i: Integer;
begin
  result := ecUnknown;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcDataDifferenceItem) then
    begin
      if (result = ecUnknown) then
        result := (Objects[i] as TcDataDifferenceItem).eDifference
      else if (result <> ecUnknown) and ((Objects[i] as TcDataDifferenceItem).eDifference <> result) then
        result := ecDifference;
    end;
end;

// TcDataDifference
//   GetPart method
//
function TcDataDifference.GetPart(Index: TeDataDifferencePart): String;
var
  i: Integer;
  s: String;
begin
  result := ksEMPTY;
  case Index of
    ediHeader:
      result := Format('%s %s', [sName, sValue]);
    ediDetails, ediSQL, ediItemHeader, ediDescription:
    begin
      for i := 0 to Count - 1 do
        if (Objects[i] <> nil) and (Objects[i] is TcDataDifferenceItem) then
        begin
          s := (Objects[i] as TcDataDifferenceItem).GetPart(Index);
          if (s <> ksEMPTY) and (pos(s, result) = 0) then
          begin
            if (Index = ediDescription) and (result <> ksEMPTY) then
              result := result + ', '
            else if result <> ksEMPTY then
              result := result + ksCR;
            result := result + s;
          end;
        end;
      if (Index = ediDescription) and (result <> ksEMPTY) then
        result := result + '.'
    end;
  end;
end;

//
// TcDataDifferenceItem
//

// TcDataDifferenceItem
//   Create method
//
constructor TcDataDifferenceItem.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_lstTarget := TcBag.Create(nil);
  m_sSQL := ksEMPTY;
end;

// TcDataDifferenceItem
//   Destroy method
//
destructor TcDataDifferenceItem.Destroy;
begin
  m_lstTarget.Free;
  inherited Destroy;
end;

// TcDataDifference
//   GetPart
//
function TcDataDifferenceItem.GetPart(Index: TeDataDifferencePart): String;

  function GetLocalHeader(value: TcData): String;
  begin
    result := ksEMPTY;
    if value.eType in [enItem, enObject] then
      result := Format('%s %s', [value.MetaData.DisplayName, value.sValue])
    else if (value.parent <> nil) and (value.Parent is TcData) then
      result := GetLocalHeader(value.parent as TcData);
  end;

const
  kasMODE: array[TeDifference] of String =
    ('is extraneous', 'is missing', 'presents differences', 'is matched', ' is in Unknown State');
begin
  result := ksEMPTY;
  case Index of
    ediSQL:
      result := m_sSQL;
    ediItemHeader:
      if (m_objReference <> nil) and (m_objReference is TcData) then
        result := GetLocalHeader(m_objReference as TcData);
    ediDetails:
      result := trim(trim(GetPart(ediItemHeader)) + ksCR + GetPart(ediSQL));
    ediDescription:
      result := Format('%s %s', [GetPart(ediItemHeader), kasMODE[m_eDifference]]);
  end;
end;

// TcDataDifference
//   SetPart
//
procedure TcDataDifferenceItem.SetPart(Index: TeDataDifferencePart; value: String);
begin
  case Index of
    ediSQL:
      m_sSQL := value;
    ediItemHeader:
      sName := value;
  end;
end;

end.

