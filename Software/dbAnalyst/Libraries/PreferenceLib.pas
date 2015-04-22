unit PreferenceLib;

interface

uses
  Windows,
  daGlobals,
  daObjectLib,
  Graphics,
  CustomControls,
  Classes;

type
  TcPreference = class;

  TcPreferenceList = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/08/01 Regis Created
  *
  ******************************************************************************}
  private
    // Private Members
    //
    m_lstDelete: TStringList;

  private
    // Private declarations
    //
    procedure   SetStringVal(item, value: String);
    function    GetStringVal(item: String): String;
    procedure   SetIntegerVal(item: String; value: longint);
    function    GetIntegerVal(item: String): longint;
    procedure   SetColor(item: String; value: TColor);
    function    GetColor(item: String): TColor;
    function    GetRTFHeader: String;
    function    GetFontHeader(item: String): String;
    function    GetFont(item: String): TFont;
    procedure   SetDefault;
    function    CreateItem(sparName: String; eparType: TePreferenceType): TcPreference;
    function    GetLicenseStatus: String;
    function    GetLicenseLevel: TeLicenseLevel;
    function    GetUserID: longint;
    procedure   SetFont(item: String; value: TFont);
    function    GetIsLicensed: boolean;

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;

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
    function    Edit(value: TeForm): boolean;
    procedure   WriteToRegistry;
    procedure   ReadFromRegistry(value: TePreferenceTypeSet);
    function    Find(sparName: String; eparType: TePreferenceType): TcObject; overload; virtual;
    function    Delete(parObject: TcObject): longint; override;

  public
    // Public Properties
    //
    property    StringVal[item: String]: String      read GetStringVal          write SetStringVal;
    property    IntegerVal[item: String]: longint    read GetIntegerVal         write SetIntegerVal;
    property    Color[item: String]: TColor          read GetColor              write SetColor;
    property    RTFHeader: String                    read GetRTFHeader;
    property    FontHeader[item: String]: String     read GetFontHeader;
    property    Font[item: String]: TFont            read GetFont               write SetFont;
    property    LicenseStatus: String                read GetLicenseStatus;
    property    LicenseLevel: TeLicenseLevel         read GetLicenseLevel;
    property    IsLicensed: boolean                  read GetIsLicensed;
    property    UserID: longint                      read GetUserID;
  end;

  TcPreference = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description:
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 02/07/01 Regis Created
  *
  ******************************************************************************}
  private
    // Private Members
    //
    m_ePreferenceType: TePreferenceType;
    m_objFont: TFont;
    m_sMetaXML: String;
    m_bValue: boolean;
    m_eForm: TeForm;
    m_sPreviousName: String;
    m_sDescription: String;
    m_lstValuePairs: TStringList;
    m_sADO: String;
    m_sProvider: String;
    m_eMode: TeConnectionMode;
    m_bOneConnection: boolean;
    m_sComment: String;

  private
    // Private declarations
    //
    procedure   LocalClear;
    function    GetFontHeader: String;
    function    GetFontIndex: longint;
    procedure   SetColor(Value: TColor);
    function    GetColor: TColor;
    function    GetIsValid: boolean;
    function    GetError: String;
    function    GetLoginPrompt: boolean;
    procedure   SetLoginPrompt(value: boolean);
    function    GetAttribute(name: String): String;
    procedure   SetAttribute(name, value: String);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear base method
    procedure   Copy(value: TcObject); override;
    //   Compare
    function    Text: String; override;
    //   Load
    //   Save
    //
    // 2. Custom
    function    BeforeEdit: boolean;
    procedure   WriteToRegistry;

  public
    // Public Properties
    //
    property    PreferenceType: TePreferenceType     read m_ePreferenceType write m_ePreferenceType;
    property    Color: TColor                        read GetColor          write SetColor;
    property    Font: TFont                          read m_objFont         write m_objFont;
    property    FontHeader: String                   read GetFontHeader;
    property    sMetaXML: String                     read m_sMetaXML        write m_sMetaXML;
    property    bValue: boolean                      read m_bValue          write m_bValue;
    property    IsValid: boolean                     read GetIsValid;
    property    Error: String                        read GetError;
    property    eForm: TeForm                        read m_eForm           write m_eForm;
    property    sPreviousName: String                read m_sPreviousName   write m_sPreviousName;
    property    sDescription: String                 read m_sDescription    write m_sDescription;
    property    bLoginPrompt: boolean                read GetLoginPrompt    write SetLoginPrompt;
    property    sADO: String                         read m_sADO            write m_sADO;
    property    ValuePairs: TStringList              read m_lstValuePairs;
    property    sProvider: String                    read m_sProvider       write m_sProvider;
    property    eMode: TeConnectionMode              read m_eMode           write m_eMode;
    property    Attribute[value: String]: String     read GetAttribute      write SetAttribute;
    property    bOneConnection: boolean              read m_bOneConnection  write m_bOneConnection;
    property    sComment: String                     read m_sComment        write m_sComment;
    property    AttributeKeys: TStringList           read m_lstValuePairs;
  end;

function DecodeLicense(Value: String; var parUser, parLicenseNumber: longint; var parLicenseLevel: TeLicenseLevel; var parProduct: longint): boolean;
function EncodeLicense(parUser, parLicenseNumber: longint; parLicenseLevel: TeLicenseLevel; parProduct: longint): String;

implementation

uses
  Forms,
  Variants,
  frmPreferences,
  sysUtils,
  comObj,
  Registry,
  daResourceStrings;

const
  krsPREFERENCEFILENAME = 'Connection.xml';
  krsXML_PREFERENCELIST = 'connection-list';
  krsXML_CONNECTION     = 'connection';
  krsXML_NAME           = 'name';
  krsXML_METADATA       = 'metadata';
  krsXML_ADO            = 'ado';
  krsXML_PROVIDER       = 'provider';
  krsXML_DEFAULTFORM    = 'defaultform';
  krsXML_DESCRIPTION    = 'description';
  krsXML_PROPERTY       = 'property';
  krsXML_MODE           = 'mode';
  krsXML_ONECONNECTION  = 'one-connection';

//
// TcPreferenceList
//

// TcPreferenceList
//   Create
//
constructor TcPreferenceList.Create(parParent: TcObject);
begin
  // Call inherited creator
  inherited Create(parParent);
  m_lstDelete := TStringList.Create;
  ReadFromRegistry([]);
end;

// TcPreferenceList
//   Destroy
//
destructor TcPreferenceList.Destroy;
begin
  m_lstDelete.Free;
  inherited Destroy;
end;

// TcPreferenceList
//   SetStringVal
//
procedure TcPreferenceList.SetStringVal(item, value: String);
var
  p: TcObject;
begin
  p := Find(item, eptValue);
  if p = nil then
    p := CreateItem(item, eptValue);
  if (p <> nil) and (p is TcObject) and ((p as TcPreference).PreferenceType = eptValue) then
    p.sValue := value;
end;

// TcPreferenceList
//   GetStringVal
//
function TcPreferenceList.GetStringVal(item: String): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  p := Find(item, eptValue);
  if p = nil then
    p := CreateItem(item, eptValue);
  if (p <> nil) and (p is TcObject) and ((p as TcPreference).PreferenceType = eptValue) then
    result := p.sValue;
end;

// TcPreferenceList
//   SetIntegerVal
//
procedure TcPreferenceList.SetIntegerVal(item: String; value: longint);
begin
  SetStringVal(item, inttostr(value));
end;

// TcPreferenceList
//   GetIntegerVal
//
function TcPreferenceList.GetIntegerVal(item: String): longint;
begin
  result := strtointdef(GetStringVal(item), 0);
end;

// TcPreferenceList
//   SetColor
//
procedure TcPreferenceList.SetColor(item: String; value: TColor);
var
  p: TcObject;
begin
  p := Find(item, eptColor);
  if p = nil then
    p := CreateItem(item, eptColor);
  if (p <> nil) and (p is TcPreference) and ((p as TcPreference).PreferenceType = eptColor) then
    p.sValue := ColorToString(value);
end;

// TcPreferenceList
//   GetColor
//
function TcPreferenceList.GetColor(item: String): TColor;
var
  p: TcObject;
begin
  result := clDefault;
  p := Find(item, eptColor);
  if p = nil then
    p := CreateItem(item, eptColor);
  if (p <> nil) and (p is TcObject) and ((p as TcPreference).PreferenceType in [eptColor]) and (p.sValue <> ksEMPTY) then
    result := StringToColor(p.sValue);
end;

// TcPreferenceList
//   GetRTFHeader
//
function TcPreferenceList.GetRTFHeader: String;
var
  ft, fc: String;
  i, k: longint;
  f: TFont;
begin
  ft := ksEMPTY;
  fc := ksEMPTY;
  k := 0;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcPreference) and ((Objects[i] as TcPreference).PreferenceType = eptFont) then
    begin
      f := (Objects[i] as TcPreference).Font;
      ft := ft + Format('{\f%d\fcharset%d\fprq%d %s;}', [k, longint(f.Charset), longint(f.Pitch), f.Name]);
      fc := fc + Format('\red%d\green%d\blue%d;', [f.Color mod $100, (f.Color shr 8) mod $100, (f.Color  shr 16) mod $100]);
      inc(k);
    end;
  result := Format('{\rtf1\ansi\deff9{\fonttbl%s}{\colortbl{%s}}\pard\margr720\margl720\margt720\margb720 ', [ft, fc]);
end;

// TcPreferenceList
//   GetFontHeader
//
function TcPreferenceList.GetFontHeader(item: String): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  p := Find(item, eptFont);
  if p = nil then
    p := CreateItem(item, eptFont);
  if (p <> nil) and (p is TcPreference) then
    result := (p as TcPreference).FontHeader;
end;

// TcPreferenceList
//   GetFont
//
function TcPreferenceList.GetFont(item: String): TFont;
var
  p: TcObject;
begin
  result := nil;
  p := Find(item, eptFont);
  if p = nil then
    p := CreateItem(item, eptFont);
  if (p <> nil) and (p is TcPreference) then
    result := (p as TcPreference).Font;
end;

// TcPreferenceList
//   SetDefault
//
procedure TcPreferenceList.SetDefault;
const

  krsPREF_SESSIONBACKGROUNDCOLOR                = 'Session Background Color';
  krsPREF_SESSIONCURRENTLINEBACKROUND           = 'Session Current Line Background';


  // Colors
  constCOLORSNAME: array[0..37] of String =
    (krsPREF_COLORNODE, krsPREF_COLORFIXEDNODE, krsPREF_COLORSELECTEDNODE, krsPREF_COLORDEPENDENCYNODE, krsPREF_COLORSHADOWNODE,
     krsPREF_COLORBACKGROUNDCOLOR, krsPREF_COLORTODEPENDENCYNODE, krsPREF_COLORMETASTRIPE,
     krsPREF_SYNTAXKEYWORD, krsPREF_SYNTAXDATATYPE, krsPREF_SYNTAXFUNCTION, krsPREF_SYNTAXMARK, krsPREF_SYNTAXCOMMENT,
     krsPREF_CONSOLEBACKGROUNDCOLOR, krsPREF_CONSOLECURRENTLINEBACKROUND, krsPREF_CONSOLEEXCUTIONTAB, krsPREF_CONSOLESCRIPTTAB,
     krsPREF_CONSOLEGRIDBACKGROUNDCOLOR, krsPREF_CONSOLEGRIDSELECTEDCOLUMNCOLOR, krsPREF_CONSOLEGRIDSELECTIONCOLOR, krsPREF_CONSOLEGRIDSELECTEDLINECOLOR,
     krsPREF_QUERYGRIDBACKGROUNDCOLOR, krsPREF_QUERYGRIDSELECTEDCOLUMNCOLOR, krsPREF_QUERYGRIDSELECTIONCOLOR, krsPREF_QUERYGRIDSELECTEDLINECOLOR, krsPREF_SESSIONGRIDBACKGROUNDCOLOR,
     krsPREF_SESSIONGRIDSELECTEDCOLUMNCOLOR, krsPREF_SESSIONGRIDSELECTIONCOLOR, krsPREF_SESSIONGRIDSELECTEDLINECOLOR, krsPREF_SESSIONGRIDNULLCELLCOLOR,
     krsPREF_SESSIONBACKGROUNDCOLOR, krsPREF_SESSIONCURRENTLINEBACKROUND,
     krsPREF_COLOREDITORREQUIREDFIELD, krsPREF_CONSOLEINTELLISENSECOLOR, krsPREF_SYNTAXNUMBER,
     krsPREF_SYNTAXANCHOR, krsPREF_SYNTAXANCHORHOVER, krsPREF_CONSOLEGRIDNULLCELLCOLOR);
  constCOLORSVALUE: array[0 .. 37] of TColor =
    (16752029, 16711680, 12320767, 12910532, 16777215,
     16777215, 11454975, 8421631,
     8388863, 8388736, 32768, 4227327, 8421504,
     clWindow, $CCFFFF, $3CC7FF, $89FF15,
     clWindow, 12582847, 15323601, 16773360,
     clWindow, 12582847, 15323601, 16773360, clWindow,
     12582847, 15323601, 16773360, clWindow,
     clWindow, $CCFFFF,
     $c8fbfb, clWindow, 4227327,
     $baa28d, clYellow, clCream);
  // Boolean Values
  constBOOLEANNAME: array[0..4] of String =
    (krsPREF_WRAPTEXT,
     krsPREF_CONNECTIONFRMWIDTH, krsPREF_CONNECTIONFRMHEIGHT, krsPREF_CONNECTIONFRMDISPLAY, krsPREF_ENABLESYNTAXHILITING);
  // Longint Values
  constLONGINTNAME: array[0..1] of String =
    (krsPREF_FIELDWIDTH, krsPREF_CONSOLEINTELLISENSEDELAY);
  constLONGINTVALUE: array[0..1] of longint =
    (kriSQLMAXCOLUMNSIZE, kiINTELLLISENSEDELAY);
  // Font
  constFONTNAME: array[0..7] of string =
    (krsPREF_FONTERROR, krsPREF_FONTGRAPH, krsPREF_FONTOBJECTHEADER, krsPREF_FONTOBJECTTEXT, krsPREF_FONTCONSOLE, krsPREF_FONTCONSOLEGRID, krsPREF_FONTSHADOWGRAPH, krsPREF_SESSIONGRIDFONT);
  constFONTCHARSET: array[0..7] of longint =
    (1, 0, 0, 0, 1, 0, 0, 0);
  constFONTHEIGHT: array[0..7] of longint =
    (-11, -9, -11, -11, -11, -11, -9, -11);
  constFONTFONTNAME: array[0..7] of String =
    ('MS Sans Serif', 'Arial', 'Arial', 'Courier New', 'Courier New', 'Arial', 'Arial', 'Arial');
  // IntelliSense
  constSTRINGNAME: array[0..1] of string =
    (krsPREF_CONSOLEINTELLISENSEDISMISS, krsPREF_CONSOLEINTELLISENSEDISMISSVALUE);
  constSTRINGVALUE: array[0..1] of string =
    (krsTRUE, '5');
var
  i: longint;
  p: TcPreference;
begin
  Clear;
  // Colors
  for i := low(constCOLORSNAME) to high(constCOLORSNAME) do
    Color[constCOLORSNAME[i]] := StringToColor(inttostr(constCOLORSVALUE[i]));
  // Boolean Values
  for i := low(constBOOLEANNAME) to high(constBOOLEANNAME) do
    StringVal[constBOOLEANNAME[i]] := krsTRUE;
  // Longint Values
  for i := low(constLONGINTNAME) to high(constLONGINTNAME) do
    StringVal[constLONGINTNAME[i]] := inttostr(constLONGINTVALUE[i]);
  // Font
  for i := low(constFONTNAME) to high(constFONTNAME) do
  begin
    p := CreateItem(constFONTNAME[i], eptFont);
    p.m_objFont.Name := constFONTNAME[i];
    p.m_objFont.charset := constFONTCHARSET[i];
    p.m_objFont.height := constFONTHEIGHT[i];
    p.m_objFont.Name := constFONTFONTNAME[i];
  end;
  // Strings
  for i := low(constSTRINGNAME) to high(constSTRINGNAME) do
    StringVal[constSTRINGNAME[i]] := constSTRINGVALUE[i];
end;

const
  krsPREFCOLORKEY       = 'Colors';
  krsPREFVALUEKEY       = 'Values';
  krsPREFCONNECTIONKEY  = 'Connections';
  krsPREFFONTKEY        = 'Fonts';
  krsCOLDSTORAGE        = 'FileName';

// TcPreferenceList
//   ReadFromRegistry
//
procedure TcPreferenceList.ReadFromRegistry(value: TePreferenceTypeSet);

  procedure ReadValues(subKey: string; eType: TePreferenceType);
  var
    Reg: TRegistry;
    lst: TStringList;
    i: longint;
    p: TcPreference;
  begin
    Reg := nil;
    lst := nil;
    try
      lst := TStringList.Create;
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey(krsREGISTRYLOCATION + subKey + '\', True) then
      begin
        Reg.GetValueNames(lst);
        for i := 0 to lst.Count - 1 do
        begin
          p := Find(lst[i], eType) as TcPreference;
          if p = nil then
            p := CreateItem(lst[i], eType);
          p.sValue := Reg.ReadString(p.sName);
          p.bValue := p.sValue = krsTRUE;
        end;
      end;
      Reg.CloseKey;
    finally
      reg.free;
      lst.free;
    end;
  end;

var
  i, j: longint;
  p: TcPreference;
  Reg: TRegistry;
  lst: TStringList;
  lst2: TStringList;
  s: string;
begin
  SetDefault;
  Reg := nil;
  lst := nil;
  lst2 := nil;
  try
    lst := TStringList.Create;
    lst2 := TStringList.Create;
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    //
    // Fonts
    if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFFONTKEY + '\', True) then
    begin
      Reg.GetKeyNames(lst);
      Reg.CloseKey;
      for i := 0 to lst.Count - 1 do
      begin
        p := Find(lst[i], eptFont) as TcPreference;
        if p = nil then
          p := CreateItem(lst[i], eptFont);
        if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFFONTKEY + '\' + p.sName, True) then
        begin
          p.m_objFont.Color   := StringToColor(Reg.ReadString(krsCOLOR));
          p.m_objFont.Charset := Reg.ReadInteger(krsCHARSET);
          p.m_objFont.Height  := Reg.ReadInteger(krsHEIGHT);
          p.m_objFont.Name    := Reg.ReadString(krsNAME);
        end;
        Reg.CloseKey;
      end;
    end;
    //
    // Discrete Value
    ReadValues(krsPREFVALUEKEY, eptValue);
    ReadValues(krsPREFCOLORKEY, eptColor);
    //
    // Read Connections
    if FileExists(GetFilePath(Application.ExeName) + krsPREFERENCEFILENAME) then
      XML := FileToString(GetFilePath(Application.ExeName) + krsPREFERENCEFILENAME, ecsAnsi)
    else
    begin
      //
      // Legacy: Read Connections from Registry
      if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFCONNECTIONKEY + '\', True) then
      begin
        Reg.GetKeyNames(lst);
        Reg.CloseKey;
        for i := 0 to lst.Count - 1 do
        begin
          p := Find(lst[i], eptConnection) as TcPreference;
          if p = nil then
            p := CreateItem(lst[i], eptConnection);
          if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFCONNECTIONKEY + '\' + p.sName, True) then
          try
            Reg.GetValueNames(lst2);
            for j := 0 to lst2.Count - 1 do
            begin
              if AnsiCompareText(lst2[j], krsMETA) = 0 then
                p.sMetaXML := Reg.ReadString(krsMETA)
              else if AnsiCompareText(lst2[j], krsADO) = 0 then
                p.sADO := Reg.ReadString(krsADO)
              else if AnsiCompareText(lst2[j], krsDEFAULTFORM) = 0 then
                p.eForm := TeForm(Reg.ReadInteger(krsDEFAULTFORM))
              else if AnsiCompareText(lst2[j], krsDESCRIPTION) = 0 then
                p.sDescription := Reg.ReadString(krsDESCRIPTION)
              else if AnsiCompareText(lst2[j], krsPROVIDER) = 0 then
                p.sProvider := Reg.ReadString(krsPROVIDER)
              else
              begin
                s := system.Copy(lst2[j], 2, length(lst2[j]) - 2);
                p.Attribute[s] := Reg.ReadString(lst2[j]);
              end;
            end;
          except
            //
          end;
          Reg.CloseKey;
        end;
      end;
    end;
  finally
    Reg.Free;
    lst.free;
    lst2.free;
  end;
end;

// TcPreferenceList
//   WriteToRegistry
//
procedure TcPreferenceList.WriteToRegistry;
var
  Reg: TRegistry;
  i: longint;
  p: TcPreference;
begin
  Reg := nil;
  try
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    // Delete Entries
    for i := 0 to m_lstDelete.count - 1 do
      Reg.DeleteKey(krsREGISTRYLOCATION + krsPREFCONNECTIONKEY + '\' +  m_lstDelete[i] + '\');
    // Save Entries
    for i := 0 to count - 1 do
    begin
      p := objects[i] as TcPreference;
      case p.PreferenceType of
        eptValue:
          if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFVALUEKEY + '\', True) then
          begin
            // 1. Delete Previous Entry?
            if (p.sPreviousName <> ksEMPTY) and (p.sPreviousName <> p.sName) then
              Reg.DeleteValue(p.sPreviousName);
            // 2. Save Entry
            Reg.WriteString(p.sName, p.sValue);
          end;
        eptColor:
          if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFCOLORKEY + '\', True) then
          begin
            // 1. Delete Previous Entry?
            if (p.sPreviousName <> ksEMPTY) and (p.sPreviousName <> p.sName) then
              Reg.DeleteValue(p.sPreviousName);
            // 2. Save Entry
            Reg.WriteString(p.sName, p.sValue);
          end;
        eptFont:
          begin
            // 1. Delete Previous Entry?
            if (p.sPreviousName <> ksEMPTY) and (p.sPreviousName <> p.sName) then
              Reg.DeleteKey(krsREGISTRYLOCATION + krsPREFFONTKEY + '\' + p.sPreviousName + '\');
            // 2. Save Entry
            if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFFONTKEY + '\' + p.sName + '\', True) then
            begin
              Reg.WriteString(krsCOLOR, ColorToString(p.m_objFont.Color));
              Reg.WriteInteger(krsCHARSET, p.m_objFont.Charset);
              Reg.WriteInteger(krsHEIGHT, p.m_objFont.Height);
              Reg.WriteString(krsNAME, p.m_objFont.Name);
            end;
          end;
        (*
        eptConnection:
          begin
            // 1. Delete Previous Entry?
            if (p.sPreviousName <> ksEMPTY) and (p.sPreviousName <> p.sName) then
              Reg.DeleteKey(krsREGISTRYLOCATION + krsPREFCONNECTIONKEY + '\' + p.sPreviousName + '\');
            // 2. Save Entry
            if Reg.OpenKey(krsREGISTRYLOCATION + krsPREFCONNECTIONKEY + '\' + p.sName + '\', True) then
            begin
              Reg.WriteString(krsMETA, p.sMetaXML);
              Reg.WriteString(krsADO, p.sADO);
              Reg.WriteString(krsPROVIDER, p.sProvider);
              Reg.WriteInteger(krsDEFAULTFORM, longint(p.eForm));
              Reg.WriteString(krsDESCRIPTION, p.sDescription);
              for j := 0 to p.ValuePairs.Count - 1 do
                Reg.WriteString('[' + p.ValuePairs.Names[j] + ']', p.ValuePairs.Values[p.ValuePairs.Names[j]]);
            end;
          end;
        *)
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  //
  // Connections
  if XML <> ksEMPTY then
  begin
    FileSetReadOnly(GetFilePath(Application.ExeName) + krsPREFERENCEFILENAME, FALSE);
    StringToFile(GetFilePath(Application.ExeName) + krsPREFERENCEFILENAME, XML, ecsAnsi);
  end;
end;

// TcPreferenceList
//   SetXML
//
procedure TcPreferenceList.SetXML(value: String);

  procedure ReadXML(parXMLNode: OLEVariant);

    function GetNode(parXMLNode: OLEVariant; value: String): OLEVariant;
    var
      i: longint;
    begin
      result := Unassigned;
      for i := 0 to parXMLNode.childNodes.Length - 1 do
        if AnsiCompareText(parXMLNode.childNodes.Item[i].nodeName, value) = 0 then
        begin
          result := parXMLNode.childNodes.Item[i];
          break;
        end;
    end;

  var
    p: TcPreference;
    i: longint;
    x: OLEVariant;
  begin
    if AnsiCompareText(parXMLNode.nodeName, krsXML_CONNECTION) = 0 then
    begin
      p := CreateItem(ksEMPTY, eptConnection);
      for i := 0 to parXMLNode.childNodes.Length - 1 do
      begin
        x := parXMLNode.childNodes.Item[i];
        if AnsiCompareText(x.nodeName, krsXML_NAME) = 0 then
          p.sName := GetXMLValue(x)
        else if AnsiCompareText(x.nodeName, krsXML_METADATA) = 0 then
          p.sMetaXML := GetXMLValue(x)
        else if AnsiCompareText(x.nodeName, krsXML_ADO) = 0 then
          p.sADO := GetXMLValue(x)
        else if AnsiCompareText(x.nodeName, krsXML_PROVIDER) = 0 then
          p.sProvider := GetXMLValue(x)
        else if AnsiCompareText(x.nodeName, krsXML_DEFAULTFORM) = 0 then
          p.eForm := TeForm(strtointdef(GetXMLValue(x), 0))
        else if AnsiCompareText(x.nodeName, krsXML_MODE) = 0 then
          p.eMode := TeConnectionMode(strtointdef(GetXMLValue(x), 0))
        else if AnsiCompareText(x.nodeName, krsXML_ONECONNECTION) = 0 then
          p.bOneConnection := GetXMLValue(x) = krsTRUE
        else if AnsiCompareText(x.nodeName, krsXML_METADATA) = 0 then
          p.sMetaXML := GetXMLValue(x)
        else if AnsiCompareText(x.nodeName, krsXML_DESCRIPTION) = 0 then
          p.sDescription := GetXMLValue(x)
        else if AnsiCompareText(x.nodeName, krsXML_PROPERTY) = 0 then
          p.Attribute[GetXMLAttribute(x, krsXML_NAME)] := GetXMLValue(x);
      end;
    end
    else for i := 0 to parXMLNode.childNodes.Length - 1 do
      ReadXML(parXMLNode.childNodes.Item[i]);
  end;

var
  i: longint;
  XMLDoc: OLEVariant;
begin
  try
    XMLDoc := CreateOLEObject('Microsoft.XMLDOM');
    if not VarIsEmpty(XMLDoc) then
    begin
      XMLDoc.LoadXML(Value);
      for i := 0 to XMLDoc.childNodes.Length - 1 do
        ReadXML(XMLDoc.childNodes.Item[i]);
    end;
    XMLDoc := Unassigned;
  except
    XMLDoc := Unassigned;
    raise;
  end;
end;

// TcPreferenceList
//   GetXML
//
function TcPreferenceList.GetXML: String;
var
  i, j: longint;
  p: TcPreference;
begin
  result := ksEMPTY;
  for i := 0 to count - 1 do
  begin
    p := objects[i] as TcPreference;
    if p.PreferenceType = eptConnection then
    begin
      result := result + ksCR +
                Format('%s<%s>', [kcTAB, krsXML_CONNECTION]) + ksCR +
                Format('%s%s<%s>%s</%s>',             [kcTAB, kcTAB, krsXML_NAME,          p.sName,                   krsXML_NAME]) + ksCR +
                Format('%s%s<%s><![CDATA[%s]]></%s>', [kcTAB, kcTAB, krsXML_METADATA,      p.sMetaXML,                krsXML_METADATA]) + ksCR +
                Format('%s%s<%s>%d</%s>',             [kcTAB, kcTAB, krsXML_DEFAULTFORM,   longint(p.eForm),          krsXML_DEFAULTFORM]) + ksCR +
                Format('%s%s<%s>%d</%s>',             [kcTAB, kcTAB, krsXML_MODE,          longint(p.eMode),          krsXML_MODE]) + ksCR +
                Format('%s%s<%s>%s</%s>',             [kcTAB, kcTAB, krsXML_ONECONNECTION, kasBOOL[p.bOneConnection], krsXML_ONECONNECTION]) + ksCR;
      if p.sADO <> ksEMPTY then
        result := result + Format('%s%s<%s><![CDATA[%s]]></%s>', [kcTAB, kcTAB, krsXML_ADO,         p.sADO,           krsXML_ADO]) + ksCR;
      if p.sProvider <> ksEMPTY then
        result := result + Format('%s%s<%s><![CDATA[%s]]></%s>', [kcTAB, kcTAB, krsXML_PROVIDER,    p.sProvider,      krsXML_PROVIDER]) + ksCR;
      if p.sDescription <> ksEMPTY then
        result := result + Format('%s%s<%s><![CDATA[%s]]></%s>', [kcTAB, kcTAB, krsXML_DESCRIPTION, p.sDescription,   krsXML_DESCRIPTION]) + ksCR;
      // Custom Values
      for j := 0 to p.ValuePairs.Count - 1 do
        result := result + Format('%s%s<%s name="%s"><![CDATA[%s]]></%s>', [kcTAB, kcTAB, krsXML_PROPERTY, p.ValuePairs.Names[j], p.Attribute[p.ValuePairs.Names[j]], krsXML_PROPERTY]) + ksCR;
      // End of tag
      result := result + Format('%s</%s>', [kcTAB, krsXML_CONNECTION]) + ksCR;
    end;
  end;
  if result <> ksEMPTY then
    result := Format('<%s>%s%s%s</%s>', [krsXML_PREFERENCELIST, ksCR, result, ksCR, krsXML_PREFERENCELIST]);
end;

// TcPreferenceList
//   Edit
//
function TcPreferenceList.Edit(value: TeForm): boolean;
var
  frm: TfrmPreferences;
  p: TcPreferenceList;
begin
  p := nil;
  try
    // Make preference list copy
    p := TcPreferenceList.Create(nil);
    p.Copy(self);
    // Show form.
    frm := nil;
    try
      frm := TfrmPreferences.Create(nil);
      frm.PreferenceList := p;
      frm.SetPage(value);
      result := frm.ShowModal = idOK;
      if result then
      begin
        Copy(p);
        WriteToRegistry;
      end;
    finally
      frm.Release;
    end;
  finally
    p.free;
  end;
end;

// TcPreferenceList
//   Find
//
function TcPreferenceList.Find(sparName: String; eparType: TePreferenceType): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcPreference) and ((Objects[i] as TcPreference).PreferenceType = eparType) and (Objects[i].sName = sparName) then
    begin
      result := Objects[i];
      break;
    end;
end;

// TcPreferenceList
//   CreateItem
//
function TcPreferenceList.CreateItem(sparName: String; eparType: TePreferenceType): TcPreference;
begin
  result := TcPreference.Create(self);
  Add(result);
  result.PreferenceType := eparType;
  result.sName := sparName;
end;

// TcPreferenceList
//   GetLicenseStatus
//
function TcPreferenceList.GetLicenseStatus: String;
var
  iUser, iLicenseNumber, iProduct: longint;
  eLevel: TeLicenseLevel;
begin
  result := 'Software is not Licensed.';
  if DecodeLicense(StringVal[krsLICENSEKEY], iUser, iLicenseNumber, eLevel, iProduct) then
    result := kasLICENSELEVEL[eLevel];
end;

// TcPreferenceList
//   GetLicenseLevel
//
function TcPreferenceList.GetLicenseLevel: TeLicenseLevel;
var
  iUser, iLicenseNumber, iProduct: longint;
  eLevel: TeLicenseLevel;
begin
  result := ellNone;
  if DecodeLicense(StringVal[krsLICENSEKEY], iUser, iLicenseNumber, eLevel, iProduct) then
    result := eLevel;
end;

// TcPreferenceList
//   GetUserID
//
function TcPreferenceList.GetUserID: longint;
var
  iUser, iLicenseNumber, iProduct: longint;
  eLevel: TeLicenseLevel;
begin
  result := kiUNDEFINED;
  if DecodeLicense(StringVal[krsLICENSEKEY], iUser, iLicenseNumber, eLevel, iProduct) then
    result := iUser;
end;

// TcPreferenceList
//   Delete
//
function TcPreferenceList.Delete(parObject: TcObject): longint;
begin
  result := kiUNDEFINED;
  if (parObject <> nil) and (parObject is TcPreference) and ((parObject as TcPreference).PreferenceType = eptConnection) then
  begin
    m_lstDelete.Add(parObject.sName);
    result := inherited Delete(parObject);
  end;
end;

// TcPreferenceList
//   SetFont
//
procedure TcPreferenceList.SetFont(item: String; value: TFont);
var
  p: TcObject;
begin
  p := Find(item, eptFont);
  if p = nil then
    p := CreateItem(item, eptFont);
  if (p <> nil) and (p is TcPreference) and ((p as TcPreference).PreferenceType = eptFont) then
    (p as TcPreference).Font.Assign(value);
end;

// TcPreferenceList
//   GetIsLicensed
//
function TcPreferenceList.GetIsLicensed: boolean;
begin
  result := GetLicenseLevel >= ellBeta;
end;

//
// TcPreference
//

// TcPreference
//   Create
//
constructor TcPreference.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objFont := TFont.Create;
  m_lstValuePairs := TStringList.Create;
  m_lstValuePairs.CaseSensitive := FALSE;
  LocalClear;
end;

// TcPreference
//   Destroy
//
Destructor TcPreference.Destroy;
begin
  m_objFont.free;
  m_lstValuePairs.Free;
  inherited Destroy;
end;

// TcPreference
//   Clear
//
procedure TcPreference.Clear;
begin
  inherited Clear;
  LocalClear;
end;

// TcPreference
//   LocalClear
//
procedure TcPreference.LocalClear;
begin
  m_ePreferenceType := eptUndefined;
  m_eForm := efsObjects;
  m_sDescription := ksEMPTY;
  // m_objFont
  m_lstValuePairs.Clear;
  m_sADO := ksEMPTY;
  m_eMode := ecmMDAC;
  m_bOneConnection := FALSE;
  m_sComment := ksEMPTY;
end;

// TcPreference
//   Copy
//
procedure TcPreference.Copy(value: TcObject);
begin
  inherited Copy(value);
  if (value <> nil) and (value is TcPreference) then
  begin
    m_ePreferenceType    := (value as TcPreference).m_ePreferenceType;
    m_sMetaXML           := (value as TcPreference).m_sMetaXML;
    m_bValue             := (value as TcPreference).m_bValue;
    m_eForm              := (value as TcPreference).m_eForm;
    m_sDescription       := (value as TcPreference).m_sDescription;
    m_objFont.Assign((value as TcPreference).m_objFont);
    m_lstValuePairs.Text := (value as TcPreference).m_lstValuePairs.Text;
    m_sADO               := (value as TcPreference).m_sADO;
    m_sProvider          := (value as TcPreference).m_sProvider;
    m_eMode              := (value as TcPreference).m_eMode;
    m_bOneConnection     := (value as TcPreference).m_bOneConnection;
    m_sComment           := (value as TcPreference).m_sComment;
  end;
end;

// TcPreference
//   GetFontHeader
//
function TcPreference.Text: String;
begin
  result := ksEMPTY;
  case m_eMode of
    ecmMDAC:
      result := m_sADO;
    ecmOracle:
      result := Format('Oracle Call Interface: %s@%s', [Attribute[krsXML_USERNAME], Attribute[krsXML_DATASOURCE]]);
  end;
end;

// TcPreference
//   GetFontHeader
//
function TcPreference.GetFontHeader: String;
begin
  result := Format('{\f%d\cf%d\fs%d', [GetFontIndex, GetFontIndex, m_objFont.Size * 2]) + ' %s}';
end;

// TcPreference
//   GetFontIndex
//
function TcPreference.GetFontIndex: longint;
var
  i: longint;
begin
  result := kiUNDEFINED;
  if (m_ePreferenceType = eptFont) and (Parent <> nil) and (Parent is TcObject) then
    for i := 0 to Parent.Count - 1 do
    begin
      if (Parent[i] <> nil) and (Parent[i] is TcPreference) and ((Parent[i] as TcPreference).PreferenceType = eptFont) then
          inc(result);
      if Parent[i] = self then
        break;
    end;
end;

// TcPreference
//   SetColor
//
procedure TcPreference.SetColor(Value: TColor);
begin
  sValue := inttostr(ColorToRGB(Value));
end;

// TcPreference
//   GetColor
//
function TcPreference.GetColor: TColor;
begin
  result := StringToColor(sValue);
end;

// TcPreference
//   GetIsValid
//
function TcPreference.GetIsValid: boolean;
begin
  result := GetError = ksEMPTY;
end;

// TcPreference
//   GetError
//
function TcPreference.GetError: String;
begin
  result := ksEMPTY;
  case m_ePreferenceType of
    eptConnection:
      if sMetaXML = ksEMPTY then
        result := Format('Connection ''%s'': A Connection XML must be specified.', [sName])
      else if not FileExists(sMetaXML) then
        result := Format('Connection ''%s'': File ''%s'' does not exists. Please check your connection settings.', [sName, sMetaXML]);
  end;
end;

// TcPreference
//   BeforeEdit
//
function TcPreference.BeforeEdit: boolean;
begin
  m_sPreviousName := sName;
  result := TRUE;
end;

// TcPreference
//   GetLoginPrompt
//
function TcPreference.GetLoginPrompt: boolean;
begin
  result := Attribute[krsLOGINPROMPT] = krsTRUE;
end;

// TcPreference
//   SetLoginPrompt
//
procedure TcPreference.SetLoginPrompt(value: boolean);
begin
  Attribute[krsLOGINPROMPT] := kasBOOL[value];
end;

// TcPreference
//   GetAttribute
//
function TcPreference.GetAttribute(name: String): String;
begin
  result := m_lstValuePairs.Values[name];
end;

// TcPreference
//   GetAttribute
//
procedure TcPreference.SetAttribute(name, value: String);
begin
  m_lstValuePairs.Values[name] := value;
end;

// TcPreference
//   WriteToRegistry
//
procedure TcPreference.WriteToRegistry;
begin
  if (parent <> nil) and (parent is TcPreferenceList) then
    (parent as TcPreferenceList).WriteToRegistry;
end;

// Tool
//   EncodeLicense
//
function EncodeLicense(parUser, parLicenseNumber: longint; parLicenseLevel: TeLicenseLevel; parProduct: longint): String;

  function NumToCypher(codec: String; value: longint; length: longint): String;
  begin
    case length of
      4: result := codec[trunc((value mod kiA4)/kiA3) + 1] + codec[trunc((value mod kiA3)/kiA2) + 1] + codec[trunc((value mod kiA2)/kiA1) + 1] + codec[trunc((value mod kiA1)/kiA0) + 1];
      2: result := codec[trunc((value mod kiA2)/kiA1) + 1] + codec[trunc((value mod kiA1)/kiA0) + 1];
    end;
  end;

var
  L, i: longint;
begin
  parUser := parUser + kiUSERCODEOFFSET;
  result := NumToCypher(krsCYPHER1, longint(parLicenseLevel), 2) +
            NumToCypher(krsCYPHER2, parUser, 4) +
            NumToCypher(krsCYPHER3, parLicenseNumber, 4) +
            NumToCypher(krsCYPHER4, parProduct, 2);
  L := 0;
  for i := 1 to length(result) - 1 do
    L := L + (7 * ord(result[i]) * ord(result[i + 1]));
  L := L mod kiA2;
  result := NumToCypher(krsCYPHER3, L, 2) + result + krsCYPHER3[random(kiA) + 1]
end;

// Tool
//   DecodeLicense
//
function DecodeLicense(Value: String; var parUser, parLicenseNumber: longint; var parLicenseLevel: TeLicenseLevel; var parProduct: longint): boolean;

  function CypherToNum(codec: String; value: string): longint;
  begin
    result := 0;
    case length(value) of
      4: result := (pos(value[1], codec) - 1) * kiA3 + (pos(value[2], codec) - 1) * kiA2 + (pos(value[3], codec) - 1) * kiA1 + (pos(value[4], codec) - 1) * kiA0;
      2: result := (pos(value[1], codec) - 1) * kiA1 + (pos(value[2], codec) - 1) * kiA0;
    end;
  end;

var
  L, i: longint;
begin
  result := FALSE;
  if Value <> ksEMPTY then
  begin
    parUser := 0;
    parLicenseNumber := 0;
    parLicenseLevel := ellNone;
    parProduct := 0;
    if Value <> ksEMPTY then
    begin
      parLicenseLevel := TeLicenseLevel(CypherToNum(krsCYPHER1, copy(Value, 3, 2)));
      parUser := CypherToNum(krsCYPHER2, copy(Value, 5, 4));
      parLicenseNumber := CypherToNum(krsCYPHER3, copy(Value, 9, 4));
      parProduct := CypherToNum(krsCYPHER4, copy(Value, 13, 2));
      parUser := parUser - kiUSERCODEOFFSET;
      L := 0;
      for i := 3 to (3 + 12 - 1 - 1) do
        L := L + (7 * ord(Value[i]) * ord(Value[i + 1]));
      L := L mod kiA2;
      result := CypherToNum(krsCYPHER3, copy(Value, 1, 2)) = L;
    end;
  end;
end;

end.


