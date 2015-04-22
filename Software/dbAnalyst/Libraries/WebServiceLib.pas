unit WebServiceLib;

interface

uses
  daObjectLib,
  PreferenceLib,
  Classes;

type
  TcWebServiceCall = class(TcObject)
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
  * 06/28/08 Regis Created
  *
  ******************************************************************************}
  private
    // Private Members
    //
    m_sMessage: String;
    m_sError: String;
    m_objPreferences: TcPreferenceList;
    m_lstPairs: TStringList;

  private
    // Private Members
    //
    function    DoParseResult(value: String): boolean;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;                                                // Clear base method
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    OnForgotPassword(strEmailAddress: String): boolean;
    function    OnRegister(value: String): boolean;
    function    OnGetInfo: boolean;
    function    GetInfoTag(value: String): string;

  public
    // Public Properties
    //
    property    sMessage: String                     read m_sMessage            write m_sMessage;
    property    sError: string                       read m_sError              write m_sError;
    property    Preferences: TcPreferenceList        read m_objPreferences      write m_objPreferences;
    property    Pairs: TStringList                   read m_lstPairs;
  end;


implementation

uses
  Windows,
  daGlobals,
  Forms,
  Variants,
  sysUtils,
  comObj,
  SOAPHTTPClient,
  XSBuiltIns,
  daResourceStrings,
  dbAnalystWebService;

const
  krsSERVICE              = 'http://www.seabirdsoftware.com/dbAnalystWebService.php';
  krsWEBSERVICECALLFAILED = 'Exchange of information with remote server failed: %s';
  krsINFOFILE             = 'News.xml';
  kiCHECKINTERVAL         = 7;

// TcWebServiceCall
//   Create
//
constructor TcWebServiceCall.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objPreferences := nil;
  m_lstPairs := TStringList.Create;
end;

// TcWebServiceCall
//   Destroy
//
destructor TcWebServiceCall.Destroy;
begin
  m_lstPairs.free;
  inherited Destroy;
end;

// TcWebServiceCall
//   Clear
//
procedure TcWebServiceCall.Clear;
begin
  inherited Clear;
  m_sMessage := ksEMPTY;
  m_sError := ksEMPTY;
end;

// TcWebServiceCall
//   OnForgotPassword
//
function TcWebServiceCall.OnForgotPassword(strEmailAddress: String): boolean;
var
  Intrf: dbAnalystPortType;
  Dt: TXSDateTime;
begin
  Clear;
  result := FALSE;
  intrf := nil;
  dt := nil;
  try
    Dt := TXSDateTime.Create;
    dt.AsDateTime := now;
    try
      Intrf := GetdbAnalystPortType(True, Format('%s?wsdl', [krsSERVICE]));
      m_sMessage := String(intrf.ProcessForgottenEmail(strEmailAddress, dt));
      result := TRUE;
    except
      on E: Exception do
        m_sError := Format(krsWEBSERVICECALLFAILED, [e.Message]);
    end;
  finally
    dt.free;
    intrf := nil;
  end;
end;

// TcWebServiceCall
//   OnRegister
//
function TcWebServiceCall.OnRegister(value: String): boolean;
var
  Intrf: dbAnalystPortType;
  Dt: TXSDateTime;
begin
  Clear;
  result := FALSE;
  intrf := nil;
  dt := nil;
  try
    Dt := TXSDateTime.Create;
    dt.AsDateTime := now;
    // Message XML
    try
      Intrf := GetdbAnalystPortType(True, Format('%s?wsdl', [krsSERVICE]));
      m_sMessage := intrf.CheckUserRegistration(value, dt);
      if DoParseResult(m_sMessage) then
        m_sError := m_lstPairs.Values['ERROR'];
      result := m_sError = ksEMPTY;
    except
      on E: Exception do
        m_sError := Format(krsWEBSERVICECALLFAILED, [e.Message]);
    end;
  finally
    dt.free;
    intrf := nil;
  end;
end;

// TcWebServiceCall
//   DoParseResult
//
function TcWebServiceCall.DoParseResult(value: String): boolean;
var
  p, xPE, t: OLEVariant;
  i: longint;
begin
  result := FALSE;
  m_lstPairs.Clear;
  if trim(value) <> ksEMPTY then
  try
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.LoadXML(value) and (p.childNodes.Length = 1) then
      begin
        t := p.childNodes.Item[0];
        for i := 0 to t.childNodes.Length - 1 do
          m_lstPairs.Values[t.childNodes.item[i].nodeName] := GetXMLValue(t.childNodes.item[i]);
      end
      else
      begin
        xPE := p.parseError;
        if xPE.errorCode <> 0 then
          raise Exception.Create('''Statement'' XML document failed to load.' + ksCR + Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xPE.Line), VarToStr(xPE.linepos), VarToStr(xPE.filepos), VarToStr(xPE.errorCode), xPE.reason]) + ksCR + trim(xPE.srcText));
      end;
      result := TRUE;
    except
      on E: Exception do
        Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_ICONSTOP + MB_OK);
    end;
  finally
    p := unassigned;
  end;
end;

// TcWebServiceCall
//   OnGetInfo
//
function TcWebServiceCall.OnGetInfo: boolean;
var
  Intrf: dbAnalystPortType;
  Dt: TXSDateTime;
  s, sInfoFile: String;
begin
  Clear;
  result := FALSE;
  intrf := nil;
  dt := nil;
  sInfoFile := GetFilePath(Application.ExeName) + krsINFOFILE;
  if m_objPreferences <> nil then
  try
    Dt := TXSDateTime.Create;
    dt.AsDateTime := now;
    //
    if //TRUE or
       (m_objPreferences.IntegerVal[krsPREF_NEXTINFOCHECKDATE] = 0) or
       (m_objPreferences.IntegerVal[krsPREF_NEXTINFOCHECKDATE] <= trunc(now)) or
       not FileExists(sInfoFile) then
    begin
      try
        Intrf := GetdbAnalystPortType(FALSE, '', nil);
        s := format('<%s>' +
                      '<%s>%d</%s>' +
                      '<%s>%s</%s>' +
                      '<%s>%s</%s>' +
                      '<%s>%d</%s>' +
                      '<%s>%s</%s>' +
                    '</%s>', [krsXML_TRANSACTION,
                                krsXML_USERID, m_objPreferences.UserID, krsXML_USERID,
                                krsXML_CPU, TextToXML(GetCPUID), krsXML_CPU,
                                krsXML_MAC, TextToXML(GetMACID), krsXML_MAC,
                                krsXML_STARTUPCOUNT, m_objPreferences.IntegerVal[krsPREF_STARTUPCOUNT], krsXML_STARTUPCOUNT,
                                krsXML_VERSION, GetApplicationVersion, krsXML_VERSION,
                              krsXML_TRANSACTION]);
        m_sMessage := intrf.GetInfo(s, dt);
        StringToFile(sInfoFile, m_sMessage, ecsAnsi);
        m_objPreferences.IntegerVal[krsPREF_NEXTINFOCHECKDATE] := trunc(now) + kiCHECKINTERVAL;
        m_objPreferences.WriteToRegistry;
        result := TRUE;
      except
        on E: Exception do
          m_sError := Format(krsWEBSERVICECALLFAILED, [e.Message]);
      end;
    end
    // Otherwise, just read file.
    else if FileExists(sInfoFile) then
    begin
      m_sMessage := FileToString(sInfoFile, ecsAnsi);
      result := TRUE;
    end;
  finally
    dt.free;
    intrf := nil;
  end
  //
  // Otherwise, just read file.
  else if FileExists(sInfoFile) then
  begin
    m_sMessage := FileToString(sInfoFile, ecsAnsi);
    result := TRUE;
  end;
end;

// TcWebServiceCall
//   GetInfoTag
//
function TcWebServiceCall.GetInfoTag(value: String): string;

  function ReadXML(parXMLNode: OLEVariant): String;
  var
    i: longint;
  begin
    result := ksEMPTY;
    if AnsiCompareText(parXMLNode.nodeName, value) = 0 then
      result := result + GetXMLValue(parXMLNode)
    else for i := 0 to parXMLNode.childNodes.Length - 1 do
      result := result + ReadXML(parXMLNode.childNodes.Item[i]);
  end;

var
  i: longint;
  XMLDoc: OLEVariant;
begin
  result := ksEMPTY;
  if m_sMessage <> ksEMPTY then
  try
    XMLDoc := CreateOLEObject('Microsoft.XMLDOM');
    if not VarIsEmpty(XMLDoc) then
    begin
      XMLDoc.LoadXML(m_sMessage);
      for i := 0 to XMLDoc.childNodes.Length - 1 do
        result := result + ReadXML(XMLDoc.childNodes.Item[i]);
    end;
    XMLDoc := Unassigned;
  except
    XMLDoc := Unassigned;
    raise;
  end;
end;

end.
