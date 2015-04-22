unit OracleExtensions_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 11/18/2008 9:04:12 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\dev\seabird\dbAnalyst Extension\OracleExtensions.tlb (1)
// LIBID: {FBBEAB71-4DB9-42E2-8757-E68ADE2FDF88}
// LCID: 0
// Helpfile: 
// HelpString: OracleExtensions Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  OracleExtensionsMajorVersion = 1;
  OracleExtensionsMinorVersion = 0;

  LIBID_OracleExtensions: TGUID = '{FBBEAB71-4DB9-42E2-8757-E68ADE2FDF88}';

  IID_ISessionTrace: TGUID = '{E6A8EDE4-FC4D-4483-BC89-A0E8F4156FC2}';
  CLASS_SessionTrace: TGUID = '{DEF097FC-A71F-4C2B-86D3-CADA5D633F25}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISessionTrace = interface;
  ISessionTraceDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  SessionTrace = ISessionTrace;


// *********************************************************************//
// Interface: ISessionTrace
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E6A8EDE4-FC4D-4483-BC89-A0E8F4156FC2}
// *********************************************************************//
  ISessionTrace = interface(IDispatch)
    ['{E6A8EDE4-FC4D-4483-BC89-A0E8F4156FC2}']
    function Get_Login(const Parameters: WideString): WordBool; safecall;
    function Get_SessionList(const Parameters: WideString): WideString; safecall;
    function Get_SessionEventDetail(const Parameters: WideString): WideString; safecall;
    function Get_SessionTraceState(const Parameter: WideString): WideString; safecall;
    function Get_SessionEventList(const Parameters: WideString): WideString; safecall;
    function Get_DeleteSession(const Parameters: WideString): WideString; safecall;
    function Get_SessionAutoTrace(const Parameters: WideString): WideString; safecall;
    function Get_SchemaList(const Parameters: WideString): WideString; safecall;
    function Get_DeleteLogTrace(const Parameters: WideString): WordBool; safecall;
    function Get_IsSessionTraced(const Parameter: WideString): Integer; safecall;
    property Login[const Parameters: WideString]: WordBool read Get_Login;
    property SessionList[const Parameters: WideString]: WideString read Get_SessionList;
    property SessionEventDetail[const Parameters: WideString]: WideString read Get_SessionEventDetail;
    property SessionTraceState[const Parameter: WideString]: WideString read Get_SessionTraceState;
    property SessionEventList[const Parameters: WideString]: WideString read Get_SessionEventList;
    property DeleteSession[const Parameters: WideString]: WideString read Get_DeleteSession;
    property SessionAutoTrace[const Parameters: WideString]: WideString read Get_SessionAutoTrace;
    property SchemaList[const Parameters: WideString]: WideString read Get_SchemaList;
    property DeleteLogTrace[const Parameters: WideString]: WordBool read Get_DeleteLogTrace;
    property IsSessionTraced[const Parameter: WideString]: Integer read Get_IsSessionTraced;
  end;

// *********************************************************************//
// DispIntf:  ISessionTraceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E6A8EDE4-FC4D-4483-BC89-A0E8F4156FC2}
// *********************************************************************//
  ISessionTraceDisp = dispinterface
    ['{E6A8EDE4-FC4D-4483-BC89-A0E8F4156FC2}']
    property Login[const Parameters: WideString]: WordBool readonly dispid 201;
    property SessionList[const Parameters: WideString]: WideString readonly dispid 202;
    property SessionEventDetail[const Parameters: WideString]: WideString readonly dispid 203;
    property SessionTraceState[const Parameter: WideString]: WideString readonly dispid 204;
    property SessionEventList[const Parameters: WideString]: WideString readonly dispid 205;
    property DeleteSession[const Parameters: WideString]: WideString readonly dispid 206;
    property SessionAutoTrace[const Parameters: WideString]: WideString readonly dispid 207;
    property SchemaList[const Parameters: WideString]: WideString readonly dispid 208;
    property DeleteLogTrace[const Parameters: WideString]: WordBool readonly dispid 209;
    property IsSessionTraced[const Parameter: WideString]: Integer readonly dispid 210;
  end;

// *********************************************************************//
// The Class CoSessionTrace provides a Create and CreateRemote method to          
// create instances of the default interface ISessionTrace exposed by              
// the CoClass SessionTrace. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSessionTrace = class
    class function Create: ISessionTrace;
    class function CreateRemote(const MachineName: string): ISessionTrace;
  end;

implementation

uses ComObj;

class function CoSessionTrace.Create: ISessionTrace;
begin
  Result := CreateComObject(CLASS_SessionTrace) as ISessionTrace;
end;

class function CoSessionTrace.CreateRemote(const MachineName: string): ISessionTrace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SessionTrace) as ISessionTrace;
end;

end.
