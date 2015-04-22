unit CursorStackLib;

interface

uses
  daObjectLib,
  Classes,
  Controls;

type
  TcCursorStack = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *         Intelligent Medical Objects, Inc.
  *
  * Description: TcCursorStack is the base class for all Cursor state handling
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 08/04/03 Regis Created
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_lstQueue: TList;
    m_lstSemaphore: TStringList;
    m_lstComponents: TList;

  private
    // Private methods
    //
    function    GetCurrentCursor: TCursor;
    function    HasPrecedence(value: TCursor): boolean;
    function    GetIndex(value: TCursor): longint;
    function    GetIsBusy: boolean;
    procedure   SetSemaphore(value: String; bparIncrement: longint);
    function    GetSemaphore(value: String): longint;
    function    GetHasSemaphore: boolean;
    procedure   SetComponentCursor(value: TCursor);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(Parent: TcObject); override;                             // Standard Contructor
    destructor  Destroy; override;                                              // Standard Destructor
    procedure   Clear; override;                                                // Clear base method
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    function    Push(value: TCursor): longint;
    function    Pop: longint;
    procedure   RestoreCursor;
    procedure   RegisterComponent(sender: TObject);

  public
    // Public declarations
    //
    property    CurrentCursor: TCursor                  read GetCurrentCursor;
    property    IsBusy: boolean                         read GetIsBusy;
    property    Semaphore[value: String]: longint       read GetSemaphore       write SetSemaphore;
    property    HasSemaphore: boolean                   read GetHasSemaphore;
  end;

implementation

uses
  Forms,
  daGlobals,
  SHDocVw,
  SynEdit;

const
  kacCURSORORDER: array[0 .. 2] of TCursor = (crDefault, crAppStart, crHourGlass);

//
// TcCursorStack
//

// TcCursorStack
//   Constructor
//
constructor TcCursorStack.Create(Parent: TcObject);
begin
  inherited Create(Parent);
  m_lstQueue := TList.Create;
  m_lstSemaphore := TStringList.Create;
  m_lstComponents := TList.Create;
end;

// TcCursorStack
//   Destructor
//
destructor TcCursorStack.Destroy;
begin
  m_lstQueue.free;
  m_lstSemaphore.free;
  m_lstComponents.Free;
  inherited Destroy;
end;

// TcCursorStack
//   Clear
//
procedure TcCursorStack.Clear;
begin
  inherited Clear;
  m_lstQueue.Clear;
  m_lstSemaphore.Clear;
end;

// TcCursorStack
//   GetCurrentCursor
//
function TcCursorStack.GetCurrentCursor: TCursor;
begin
  result := crDefault;
  if m_lstQueue.Count > 0 then
    result := TCursor(m_lstQueue[m_lstQueue.Count - 1]);
end;

// TcCursorStack
//   HasPrecedence
//
function TcCursorStack.HasPrecedence(value: TCursor): boolean;
begin
  result := GetIndex(value) > GetIndex(GetCurrentCursor);
end;

// TcCursorStack
//   GetIndex
//
function TcCursorStack.GetIndex(value: TCursor): longint;
var
  i: longint;
begin
  result := high(kacCURSORORDER);
  for i := low(kacCURSORORDER) to high(kacCURSORORDER) do
    if kacCURSORORDER[i] = value then
    begin
      result := i;
      break;
    end;
end;

// TcCursorStack
//   Push
//
function TcCursorStack.Push(value: TCursor): longint;
begin
  if not HasPrecedence(value) then
    value := GetCurrentCursor;
  result := m_lstQueue.Add(pointer(value));
  screen.cursor := value;
  SetComponentCursor(value);
end;

// TcCursorStack
//   Pop
//
function TcCursorStack.Pop: longint;
begin
  if m_lstQueue.Count > 0 then
    m_lstQueue.Delete(m_lstQueue.Count - 1);
  RestoreCursor;
  result := m_lstQueue.Count - 1;
end;

// TcCursorStack
//   RestoreCursor
//
procedure TcCursorStack.RestoreCursor;
begin
  screen.cursor := GetCurrentCursor;
  SetComponentCursor(GetCurrentCursor);
end;

// TcCursorStack
//   GetIsBusy
//
function TcCursorStack.GetIsBusy: boolean;
begin
  result := m_lstQueue.Count <> 0;
end;

// TcCursorStack
//   GetHasSemaphore
//
function TcCursorStack.GetHasSemaphore: boolean;
var
  i: longint;
begin
  result := FALSE;
  for i := 0 to m_lstSemaphore.Count - 1 do
    result := result or (longint(m_lstSemaphore.Objects[i]) <> 0);
end;

// TcCursorStack
//   setSemaphore
//
procedure TcCursorStack.SetSemaphore(value: String; bparIncrement: longint);
var
  L: longint;
begin
  L := m_lstSemaphore.IndexOf(value);
  if L = kiUNDEFINED then
    L := m_lstSemaphore.Add(value);
  m_lstSemaphore.Objects[L] := pointer(longint(m_lstSemaphore.Objects[L]) + bparIncrement);
end;

// TcCursorStack
//   GetSemaphore
//
function TcCursorStack.GetSemaphore(value: String): longint;
var
  L: longint;
begin
  L := m_lstSemaphore.IndexOf(value);
  if L = kiUNDEFINED then
    L := m_lstSemaphore.Add(value);
  result := longint(m_lstSemaphore.Objects[L]);
end;

// TcCursorStack
//   RegisterComponent
//
procedure TcCursorStack.RegisterComponent(sender: TObject);
begin
  if m_lstComponents.IndexOf(Sender) = kiUNDEFINED then
    m_lstComponents.Add(Sender)
end;

// TcCursorStack
//   SetComponentCursor
//
procedure TcCursorStack.SetComponentCursor(value: TCursor);
var
  i: longint;
  p: TObject;
begin
  for i := 0 to m_lstComponents.Count - 1 do
  begin
    p := m_lstComponents[i];
    if (p <> nil) and (p is TWebBrowser) then
      (p as TWebBrowser).Cursor := value
    else if (p <> nil) and (p is TSynEdit) then
      (p as TSynEdit).Cursor := value;
  end;
end;

end.
