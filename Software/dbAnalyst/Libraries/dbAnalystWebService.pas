// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://www.seabirdsoftware.com/dbAnalystWebService.php?wsdl
//  >Import : http://www.seabirdsoftware.com/dbAnalystWebService.php?wsdl>0
// Encoding : ISO-8859-1
// Version  : 1.0
// (12/5/2010 10:24:08 PM - - $Rev: 25127 $)
// ************************************************************************ //

unit dbAnalystWebService;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Embarcadero types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"[]
  // !:dateTime        - "http://www.w3.org/2001/XMLSchema"[]


  // ************************************************************************ //
  // Namespace : http://seabirdsoftware.com/
  // soapAction: http://www.seabirdsoftware.com/dbAnalystWebService.php/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : rpc
  // binding   : dbAnalystBinding
  // service   : dbAnalyst
  // port      : dbAnalystPort
  // URL       : http://www.seabirdsoftware.com/dbAnalystWebService.php
  // ************************************************************************ //
  dbAnalystPortType = interface(IInvokable)
  ['{8A5BEADD-4D96-3B51-8ACE-CE8DD56938A4}']
    function  CheckUserRegistration(const value: string; const timestamp: TXSDateTime): string; stdcall;
    function  ProcessForgottenEmail(const value: string; const timestamp: TXSDateTime): string; stdcall;
    function  GetInfo(const value: string; const timestamp: TXSDateTime): string; stdcall;
  end;

function GetdbAnalystPortType(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): dbAnalystPortType;


implementation
  uses SysUtils;

function GetdbAnalystPortType(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): dbAnalystPortType;
const
  defWSDL = 'http://www.seabirdsoftware.com/dbAnalystWebService.php?wsdl';
  defURL  = 'http://www.seabirdsoftware.com/dbAnalystWebService.php';
  defSvc  = 'dbAnalyst';
  defPrt  = 'dbAnalystPort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as dbAnalystPortType);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  InvRegistry.RegisterInterface(TypeInfo(dbAnalystPortType), 'http://seabirdsoftware.com/', 'ISO-8859-1');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(dbAnalystPortType), 'http://www.seabirdsoftware.com/dbAnalystWebService.php/%operationName%');

end.