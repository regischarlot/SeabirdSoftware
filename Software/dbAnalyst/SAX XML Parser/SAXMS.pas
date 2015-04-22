unit saxms;

{
  Simple API for XML 2.0 (SAX) Implementation.
  IXMLReader adapter for MSXML3 IVBSAXXMLReader.
  Based on standard at http://www.saxproject.org/.
  Requires MSXML v3 package from Microsoft.

  Copyright © Keith Wood (kbwood@compuserve.com)
  Written 11 June, 2000.
  Updated 4 July, 2003.
}

interface

{$I SAX.inc}

uses
  Classes, SysUtils, Windows,
  SAX, SAXHelpers, SAXExt,
  Variants,
  ComObj, ActiveX, MSXML3;

const

  SAX_VENDOR_MSXML = SAXString('MSXML');


type

  TSAX2MSDeclAdapter = class;
  TSAX2MSLexicalAdapter = class;

  IMSDeclHandlerProperty = interface(IInterfaceProperty)
    ['{8A319F85-34EB-4843-BD66-C107CA31B8BF}']
  end;

  IMSLexicalHandlerProperty = interface(IInterfaceProperty)
    ['{984C23FA-0885-4C1C-991F-75EE5980B2A9}']
  end;

  { Adapter for Microsoft's SAX 2 reader as found in MSXML3.dll.

    Since it uses a different set of SAX definitions (that provide
    the same functionality) we need to adapt these to fit with the
    Delphi version of SAX.

    Thus, this class acts as handlers for Microsoft's SAX2 reader,
    while acting as a Delphi SAX2 reader to other parties. Care must
    be taken to reference the correct version of each interface.
  }
  TSAXMSXMLReader = class(TXMLReaderImpl, IVBSAXContentHandler,
    IVBSAXDTDHandler, IVBSAXEntityResolver, IVBSAXErrorHandler,
    IMSDeclHandlerProperty, IMSLexicalHandlerProperty, ILocator)
  private
    FAttributes: SAXHelpers.TAttributesImpl;
    FIAttributes: IAttributes;
    FLocator: IVBSAXLocator;
    FXMLReader: IVBSAXXMLReader;
    FDeclAdapter : TSAX2MSDeclAdapter;
    FLexicalAdapter : TSAX2MSLexicalAdapter;
    { IDeclHandlerProperty }
    function QueryInterfaceDeclHandler(const IID: TGUID;
      out Obj): HResult; stdcall;
    function GetDeclHandlerPropertyName : SAXString;
    function GetDeclHandlerPropertyValue : IUnknown;
    procedure SetDeclHandlerPropertyValue(const value : IUnknown);
    { ILexicalHandlerProperty }
    function QueryInterfaceLexicalHandler(const IID: TGUID;
      out Obj): HResult; stdcall;
    function GetLexicalHandlerPropertyName : SAXString;
    function GetLexicalHandlerPropertyValue : IUnknown;
    procedure SetLexicalHandlerPropertyValue(const value : IUnknown);
  public
    constructor Create; reintroduce; overload;
    constructor Create(ContentHandler: IContentHandler;
      DTDHandler: IDTDHandler = nil; EntityResolver: IEntityResolver = nil;
      ErrorHandler: IErrorHandler = nil); reintroduce; overload;
    destructor Destroy; override;
    { ILocator }
    function getColumnNumber: Integer; override;
    function getFeature(const Name: SAXString): Boolean; override;
    function getLineNumber: Integer; override;
    function getProperty(const name : SAXString) : IProperty; override;
    function getPublicId(): PSAXChar; override;
    function getSystemId(): PSAXChar; override;
    procedure parseInput(const Input: IInputSource); override;
    procedure setFeature(const Name: SAXString; Value: Boolean); override;
    { IDeclHandlerProperty }
    function IMSDeclHandlerProperty.QueryInterface = QueryInterfaceDeclHandler;
    function IMSDeclHandlerProperty.getName = GetDeclHandlerPropertyName;
    function IMSDeclHandlerProperty.getValue = GetDeclHandlerPropertyValue;
    procedure IMSDeclHandlerProperty.setValue = SetDeclHandlerPropertyValue;
    { ILexicalHandlerProperty }
    function IMSLexicalHandlerProperty.QueryInterface = QueryInterfaceLexicalHandler;
    function IMSLexicalHandlerProperty.getName = GetLexicalHandlerPropertyName;
    function IMSLexicalHandlerProperty.getValue = GetLexicalHandlerPropertyValue;
    procedure IMSLexicalHandlerProperty.setValue = SetLexicalHandlerPropertyValue;
    { IVBSAXContentHandler }
    procedure Set_documentLocator(const Param1: IVBSAXLocator); safecall;
    procedure startDocument; safecall;
    procedure endDocument; safecall;
    procedure startPrefixMapping(var strPrefix: WideString;
      var strURI: WideString); safecall;
    procedure endPrefixMapping(var strPrefix: WideString); safecall;
    procedure startElement(var strNamespaceURI: WideString;
      var strLocalName: WideString; var strQName: WideString;
      const oAttributes: IVBSAXAttributes); safecall;
    procedure endElement(var strNamespaceURI: WideString;
      var strLocalName: WideString; var strQName: WideString); safecall;
    procedure characters(var strChars: WideString); safecall;
    procedure ignorableWhitespace(var strChars: WideString); safecall;
    procedure processingInstruction(var strTarget: WideString;
      var strData: WideString); safecall;
    procedure skippedEntity(var strName: WideString); safecall;
    property documentLocator: IVBSAXLocator write Set_documentLocator;
    { IVBSAXDTDHandler }
    procedure notationDecl(var strName: WideString; var strPublicId: WideString;
      var strSystemId: WideString); safecall;
    procedure unparsedEntityDecl(var strName: WideString;
      var strPublicId: WideString; var strSystemId: WideString;
      var strNotationName: WideString); safecall;
    { IVBSAXEntityResolver }
    function resolveEntity(var strPublicId: WideString;
      var strSystemId: WideString): OleVariant; safecall;
    { IVBSAXErrorHandler }
    procedure error(const oLocator: IVBSAXLocator; var strError: WideString;
      nErrorCode: Integer); safecall;
    procedure fatalError(const oLocator: IVBSAXLocator;
      var strError: WideString; nErrorCode: Integer); safecall;
    procedure ignorableWarning(const oLocator: IVBSAXLocator;
      var strError: WideString; nErrorCode: Integer); safecall;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
      stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      stdcall;
  end;

  { A base adapter for SAX2 extension interfaces }
  TSAX2MSExtensionAdapter = class(TInterfacedObject, IDispatch)
  public
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
      stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      stdcall;
  end;

  { An adapter for the ISAXLexicalHandler interface }
  TSAX2MSLexicalAdapter = class(TSAX2MSExtensionAdapter, IVBSAXLexicalHandler)
  private
    FLexicalHandler: ILexicalHandler;
  public
    constructor Create(LexicalHandler: ILexicalHandler);
    procedure startDTD(var strName: WideString; var strPublicId: WideString;
      var strSystemId: WideString); safecall;
    procedure endDTD; safecall;
    procedure startEntity(var strName: WideString); safecall;
    procedure endEntity(var strName: WideString); safecall;
    procedure startCDATA; safecall;
    procedure endCDATA; safecall;
    procedure comment(var strChars: WideString); safecall;

    property LexicalHandler : ILexicalHandler read FLexicalHandler
      write FLexicalHandler;
  end;

  { An adapter for the ISAXDeclHandler interface }
  TSAX2MSDeclAdapter = class(TSAX2MSExtensionAdapter, IVBSAXDeclHandler)
  private
    FDeclHandler: IDeclHandler;
  public
    constructor Create(DeclHandler: IDeclHandler);
    procedure elementDecl(var strName: WideString;
      var strModel: WideString); safecall;
    procedure attributeDecl(var strElementName: WideString;
      var strAttributeName: WideString; var strType: WideString;
      var strValueDefault: WideString; var strValue: WideString); safecall;
    procedure internalEntityDecl(var strName: WideString;
      var strValue: WideString); safecall;
    procedure externalEntityDecl(var strName: WideString;
      var strPublicId: WideString; var strSystemId: WideString); safecall;

    property DeclHandler : IDeclHandler read FDeclHandler write FDeclHandler;
  end;

  TSAXMSXMLVendor = class(TSAXVendor)
    function Description: string; override;
    function XMLReader: IXMLReader; override;
  end;

  TSAXMSXML = class(TComponent)
  private
    function GetVendor: string;
  published
    property Vendor: string read GetVendor;
  end;


implementation

resourcestring
  ErrorMsg = '(%d) %s';

{ TSAXMSXMLReader --------------------------------------------------------------}

{ Initialise }
constructor TSAXMSXMLReader.Create;
begin
  inherited Create;
  FAttributes     := SAXHelpers.TAttributesImpl.Create;
  FIAttributes    := FAttributes as IAttributes;  { AddRef }
  FLocator        := nil;
  FDeclAdapter    := TSAX2MSDeclAdapter.Create(nil);
  FLexicalAdapter := TSAX2MSLexicalAdapter.Create(nil);
  { Instantiate MS SAX reader and register self as handlers }
  FXMLReader      := CoSAXXMLReader.Create;
  FXMLReader.contentHandler := Self;
  _Release;
  FXMLReader.dtdHandler     := Self;
  _Release;
  FXMLReader.errorHandler   := Self;
  _Release;
  // Try to put the DeclHandler property
  try
    FXMLReader.putProperty(DeclHandlerProperty, IVBSAXDeclHandler(FDeclAdapter));
  except
    // Do nothing
  end;

  // Try to put the LexicalHandler property
  try
    FXMLReader.putProperty(LexicalHandlerProperty, IVBSAXLexicalHandler(FLexicalAdapter));
  except
    // Do nothing
  end;

end;

{ Initialise and use specified XML handlers }
constructor TSAXMSXMLReader.Create(ContentHandler: IContentHandler;
  DTDHandler: IDTDHandler = nil; EntityResolver: IEntityResolver = nil;
  ErrorHandler: IErrorHandler = nil);
begin
  Create;
  setContentHandler(ContentHandler);
  setDTDHandler(DTDHandler);
  setEntityResolver(EntityResolver);
  setErrorHandler(ErrorHandler);
end;

{ Release resources }
destructor TSAXMSXMLReader.Destroy;
begin
  FIAttributes := nil;  { Release }
  FLocator     := nil;
  FXMLReader   := nil;
  FDeclAdapter := nil;
  FLexicalAdapter:= nil;
  inherited Destroy;
end;

{ ISAXXMLReader ---------------------------------------------------------------}

{ Adapt Delphi SAX feature query to MS SAX feature query }
function TSAXMSXMLReader.getFeature(const Name: SAXString): Boolean;
begin
  try
    Result := FXMLReader.getFeature(Name);
  except on Error: Exception do
    raise ESAXNotRecognizedException.Create(Error.Message);
  end;
end;

{ Adapt Delphi SAX property query to MS SAX property query }
function TSAXMSXMLReader.GetProperty(const Name: SAXString): IProperty;
begin
  if Name = LexicalHandlerProperty then
    Result := IMSLexicalHandlerProperty(Self)
  else if Name = DeclHandlerProperty then
    Result := IMSDeclHandlerProperty(Self)
  else
    raise ESAXNotRecognizedException.Create(Name);
end;

{ Adapt Delphi SAX parse invocation to MS SAX parse invocation }
procedure TSAXMSXMLReader.ParseInput(const Input: IInputSource);
var
  Stream: IStream;
  Isis : IStreamInputSource;
begin
  if (Input.QueryInterface(IStreamInputSource, Isis) = 0) then
  begin
    Stream := TStreamAdapter.Create(Isis.ByteStream) as IStream;
    try
      FXMLReader.parse(Stream);
    finally
      Stream := nil;
    end;
  end else
    FXMLReader.parseURL(Input.SystemId);
end;

{ Adapt Delphi SAX feature setting to MS SAX feature setting }
procedure TSAXMSXMLReader.SetFeature(const Name: SAXString;
  Value: Boolean);
begin
  try
    FXMLReader.putFeature(Name, Value);
  except on Error: Exception do
    raise ESAXNotRecognizedException.Create(Error.Message);
  end;
end;

function TSAXMSXMLReader.QueryInterfaceDeclHandler(const IID: TGUID;
  out Obj): HResult; stdcall;
begin
  if IsEqualGUID(IID, IInterfaceProperty) then
  begin
    Result:= S_OK;
    IMSDeclHandlerProperty(Obj):= Self;
  end else
    Result:= inherited QueryInterface(IID, Obj);
end;

function TSAXMSXMLReader.GetDeclHandlerPropertyName : SAXString;
begin
  Result:= DeclHandlerProperty;
end;

function TSAXMSXMLReader.GetDeclHandlerPropertyValue : IUnknown;
begin
  Result:= FDeclAdapter.DeclHandler;
end;

procedure TSAXMSXMLReader.SetDeclHandlerPropertyValue(const value : IUnknown);
begin
  FDeclAdapter.DeclHandler:= IDeclHandler(value);
end;

function TSAXMSXMLReader.QueryInterfaceLexicalHandler(const IID: TGUID;
  out Obj): HResult; stdcall;
begin
  if IsEqualGUID(IID, IInterfaceProperty) then
  begin
    Result:= S_OK;
    IMSLexicalHandlerProperty(Obj):= Self;
  end else
    Result:= inherited QueryInterface(IID, Obj);
end;

function TSAXMSXMLReader.GetLexicalHandlerPropertyName : SAXString;
begin
  Result:= LexicalHandlerProperty;
end;

function TSAXMSXMLReader.GetLexicalHandlerPropertyValue : IUnknown;
begin
  Result:= FLexicalAdapter.LexicalHandler;
end;

procedure TSAXMSXMLReader.SetLexicalHandlerPropertyValue(const value : IUnknown);
begin
  FLexicalAdapter.LexicalHandler:= ILexicalHandler(value);
end;

{ ISAXLocator -----------------------------------------------------------------}

{ Adapt Delphi SAX column query to MS SAX column query }
function TSAXMSXMLReader.getColumnNumber: Integer;
begin
  if Assigned(FLocator) then
    Result := FLocator.columnNumber
  else
    Result := -1;
end;

{ Adapt Delphi SAX line query to MS SAX line query }
function TSAXMSXMLReader.getLineNumber: Integer;
begin
  if Assigned(FLocator) then
    Result := FLocator.lineNumber
  else
    Result := -1;
end;

{ Adapt Delphi SAX public id query to MS SAX public id query }
function TSAXMSXMLReader.getPublicId: PSAXChar;
begin
  if Assigned(FLocator) then
    Result := PSAXChar(SAXString(FLocator.publicId))
  else
    Result := '';
end;

{ Adapt Delphi SAX system id query to MS SAX system id query }
function TSAXMSXMLReader.getSystemId: PSAXChar;
begin
  if Assigned(FLocator) then
    Result := PSAXChar(SAXString(FLocator.systemId))
  else
    Result := '';
end;

{ IVBSAXContentHandler --------------------------------------------------------}

{ Adapt MS SAX characters event to Delphi SAX characters event }
procedure TSAXMSXMLReader.characters(var strChars: WideString);
begin
  getContentHandler.Characters(strChars);
end;

{ Adapt MS SAX end document event to Delphi SAX end document event }
procedure TSAXMSXMLReader.endDocument;
begin
  getContentHandler.EndDocument;
end;

{ Adapt MS SAX end element event to Delphi SAX end element event }
procedure TSAXMSXMLReader.endElement(var strNamespaceURI: WideString;
  var strLocalName: WideString; var strQName: WideString);
begin
  getContentHandler.EndElement(strNamespaceUri, strLocalName, strQName);
end;

{ Adapt MS SAX end prefix mapping event to Delphi SAX end prefix mapping event }
procedure TSAXMSXMLReader.endPrefixMapping(var strPrefix: WideString);
begin
  getContentHandler.EndPrefixMapping(strPrefix);
end;

{ Adapt MS SAX ignorable whitespace event to Delphi SAX ignorable whitespace event }
procedure TSAXMSXMLReader.ignorableWhitespace(var strChars: WideString);
begin
  getContentHandler.IgnorableWhitespace(strChars);
end;

{ Adapt MS SAX processing instruction event to Delphi SAX processing instruction event }
procedure TSAXMSXMLReader.processingInstruction(var strTarget: WideString;
  var strData: WideString);
begin
  getContentHandler.ProcessingInstruction(strTarget, strData);
end;

{ Save MS locator for later use }
procedure TSAXMSXMLReader.Set_documentLocator(const Param1: IVBSAXLocator);
begin
  FLocator := Param1;
  { Fire the setDocumentLocator Callback }
  getContentHandler.setDocumentLocator(Self);
end;

{ Adapt MS SAX skipped entity event to Delphi SAX skipped entity event }
procedure TSAXMSXMLReader.skippedEntity(var strName: WideString);
begin
  getContentHandler.SkippedEntity(strName);
end;

{ Adapt MS SAX start document event to Delphi SAX start document event }
procedure TSAXMSXMLReader.startDocument;
begin
  getContentHandler.StartDocument;
end;

{ Adapt MS SAX start element event to Delphi SAX start element event }
procedure TSAXMSXMLReader.startElement(var strNamespaceURI: WideString;
  var strLocalName: WideString; var strQName: WideString;
  const oAttributes: IVBSAXAttributes);
var
  Index: Integer;
begin
  { Transfer attributes to Delphi interface }
  FAttributes.Clear;
  for Index := 0 to oAttributes.Length - 1 do
  begin
    FAttributes.AddAttribute(oAttributes.getUri(Index),
      oAttributes.getLocalName(Index), oAttributes.getQName(Index),
      oAttributes.getType(Index), oAttributes.getValue(Index));
  end;
  getContentHandler.StartElement(
    strNamespaceUri, strLocalName, strQName, FAttributes);
end;

{ Adapt MS SAX start prefix mapping event to Delphi SAX start prefix mapping event }
procedure TSAXMSXMLReader.startPrefixMapping(var strPrefix: WideString;
  var strURI: WideString);
begin
  getContentHandler.StartPrefixMapping(strPrefix, strUri);
end;

{ IVBSAXDTDHandler ------------------------------------------------------------}

{ Adapt MS SAX notation declaration event to Delphi SAX notation declaration event }
procedure TSAXMSXMLReader.notationDecl(var strName: WideString;
  var strPublicId: WideString; var strSystemId: WideString);
begin
  getDTDHandler.NotationDecl(strName, strPublicId, strSystemId);
end;

{ Adapt MS SAX unparsed entity event to Delphi SAX unparsed entity event }
procedure TSAXMSXMLReader.unparsedEntityDecl(var strName: WideString;
  var strPublicId: WideString; var strSystemId: WideString;
  var strNotationName: WideString);
begin
  getDTDHandler.UnparsedEntityDecl(
    strName, strPublicId, strSystemId, strNotationName);
end;

{ IVBSAXEntityResolver --------------------------------------------------------}

{ Adapt MS SAX resolve entity event to Delphi SAX resolve entity event }
function TSAXMSXMLReader.resolveEntity(var strPublicId: WideString;
  var strSystemId: WideString): OleVariant;
var
  Input: IInputSource;
  Stream: TStringStream;
  Isis : IStreamInputSource;
begin
  Result := Null;
  Input  := getEntityResolver.ResolveEntity(strPublicId, strSystemId);
  if Assigned(Input) then
  begin
    if (Input.QueryInterface(IStreamInputSource, Isis) = 0) then
    begin
      { Convert TSAXInputStream into a string for return to MS XML }
      Stream := TStringStream.Create('');
      try
        Stream.CopyFrom(Isis.ByteStream, 0);
        Result := Stream.DataString;
      finally
        Stream.Free;
        Input:= nil;
      end;
    end;
  end;
end;

{ IVBSAXErrorHandler ----------------------------------------------------------}

{ Adapt MS SAX error event to Delphi SAX error event }
procedure TSAXMSXMLReader.error(const oLocator: IVBSAXLocator;
  var strError: WideString; nErrorCode: Integer);
begin
  getErrorHandler.Error(TSAXParseError.Create(
    PSAXChar(SAXString(Format(ErrorMsg, [nErrorCode, Trim(strError)]))),
    PSAXChar(SAXString(oLocator.publicId)),
    PSAXChar(SAXString(oLocator.systemId)),
    oLocator.lineNumber,
    oLocator.columnNumber
    ) as ISAXParseError);
end;

{ Adapt MS SAX fatal error event to Delphi SAX fatal error event }
procedure TSAXMSXMLReader.fatalError(const oLocator: IVBSAXLocator;
  var strError: WideString; nErrorCode: Integer);
begin
  getErrorHandler.FatalError(TSAXParseError.Create(
    PSAXChar(SAXString(Format(ErrorMsg, [nErrorCode, Trim(strError)]))),
    PSAXChar(SAXString(oLocator.publicId)),
    PSAXChar(SAXString(oLocator.systemId)),
    oLocator.lineNumber,
    oLocator.columnNumber
    ) as ISAXParseError);
end;

{ Adapt MS SAX warning event to Delphi SAX warning event }
procedure TSAXMSXMLReader.ignorableWarning(const oLocator: IVBSAXLocator;
  var strError: WideString; nErrorCode: Integer);
begin
  getErrorHandler.Warning(TSAXParseError.Create(
    PSAXChar(SAXString(Format(ErrorMsg, [nErrorCode, Trim(strError)]))),
    PSAXChar(SAXString(oLocator.publicId)),
    PSAXChar(SAXString(oLocator.systemId)),
    oLocator.lineNumber,
    oLocator.columnNumber) as ISAXParseError);
end;

{ IDispatch -------------------------------------------------------------------}

{ IDispatch - not implemented }
function TSAXMSXMLReader.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSAXMSXMLReader.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSAXMSXMLReader.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSAXMSXMLReader.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TSAX2MSExtensionAdapter -----------------------------------------------------}

{ IDispatch - not implemented }
function TSAX2MSExtensionAdapter.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSAX2MSExtensionAdapter.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSAX2MSExtensionAdapter.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSAX2MSExtensionAdapter.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TSAX2MSLexicalAdapter -------------------------------------------------------}

{ Initialisation - wrap supplied SAX2 interface }
constructor TSAX2MSLexicalAdapter.Create(LexicalHandler: ILexicalHandler);
begin
  inherited Create;
  FLexicalHandler := LexicalHandler;
end;

{ Pass comment calls straight through }
procedure TSAX2MSLexicalAdapter.comment(var strChars: WideString);
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.Comment(strChars);
end;

{ Pass end CDATA section calls straight through }
procedure TSAX2MSLexicalAdapter.endCDATA;
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.EndCDATA;
end;

{ Pass end DTD calls straight through }
procedure TSAX2MSLexicalAdapter.endDTD;
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.EndDTD;
end;

{ Pass end entity calls straight through }
procedure TSAX2MSLexicalAdapter.endEntity(var strName: WideString);
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.EndEntity(strName);
end;

{ Pass start CDATA section calls straight through }
procedure TSAX2MSLexicalAdapter.startCDATA;
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.StartCData;
end;

{ Pass start DTD calls straight through }
procedure TSAX2MSLexicalAdapter.startDTD(var strName: WideString;
  var strPublicId: WideString; var strSystemId: WideString);
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.StartDTD(strName, strPublicId, strSystemId);
end;

{ Pass start entity calls straight through }
procedure TSAX2MSLexicalAdapter.startEntity(var strName: WideString);
begin
  if (FLexicalHandler <> nil) then
    FLexicalHandler.StartEntity(strName);
end;

{ TSAX2MSDeclAdapter ----------------------------------------------------------}

{ Initialisation - wrap supplied SAX2 interface }
constructor TSAX2MSDeclAdapter.Create(DeclHandler: IDeclHandler);
begin
  inherited Create;
  FDeclHandler := DeclHandler;
end;

{ Pass attribute declaration calls straight through }
procedure TSAX2MSDeclAdapter.attributeDecl(var strElementName: WideString;
  var strAttributeName: WideString; var strType: WideString;
  var strValueDefault: WideString; var strValue: WideString);
begin
  if (FDeclHandler <> nil) then
    FDeclHandler.AttributeDecl(strElementName,
      strAttributeName, strType, strValueDefault, strValue);
end;

{ Pass element declaration calls straight through }
procedure TSAX2MSDeclAdapter.elementDecl(var strName: WideString;
  var strModel: WideString);
begin
  if (FDeclHandler <> nil) then
    FDeclHandler.ElementDecl(strName, strModel);
end;

{ Pass external entity declaration calls straight through }
procedure TSAX2MSDeclAdapter.externalEntityDecl(var strName: WideString;
  var strPublicId: WideString; var strSystemId: WideString);
begin
  if (FDeclHandler <> nil) then
    FDeclHandler.ExternalEntityDecl(strName, strPublicId, strSystemId);
end;

{ Pass internal entity declaration calls straight through }
procedure TSAX2MSDeclAdapter.internalEntityDecl(var strName: WideString;
  var strValue: WideString);
begin
  if (FDeclHandler <> nil) then
    FDeclHandler.InternalEntityDecl(strName, strValue);
end;


{ TSAXMSXMLVendor -------------------------------------------------------------}

function TSAXMSXMLVendor.Description: string;
begin
  Result := SAX_VENDOR_MSXML;
end;

function TSAXMSXMLVendor.XMLReader: IXMLReader;
begin
  Result := TSAXMSXMLReader.Create;
end;

var
  SAXVendor: TSAXVendor;

function TSAXMSXML.GetVendor: string;
begin
  Result := SAXVendor.Description;
end;

initialization
  SAXVendor        := TSAXMSXMLVendor.Create;
  DefaultSAXVendor := SAXVendor.Description;
  RegisterSAXVendor(SAXVendor);
finalization
  UnRegisterSAXVendor(SAXVendor);
end.

