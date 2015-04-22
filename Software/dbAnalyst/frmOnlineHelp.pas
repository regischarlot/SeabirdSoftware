unit frmOnlineHelp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, StdCtrls,
  PreferenceLib, ToolWin, ImgList, AppEvnts,
  daObjectLib,
  FormLib,
  daGlobals,
  StdActns, ActnList, OleCtrls, SHDocVw,
  FavoriteLib,
  IEDocHostUIHandler;

type
  TfrmOnlineHelp = class(TcForm)
    DataMainMenu: TMainMenu;
    mnuTools: TMenuItem;
    mnuAction: TMenuItem;
    ActionList: TActionList;
    actEditRefresh: TAction;
    wbOnlineHelp: TWebBrowser;
    ToolBar: TToolBar;
    btnRefresh: TToolButton;
    btnMasterHelp: TToolButton;
    actHelpMaster: TAction;
    btnNext: TToolButton;
    btnPrevious: TToolButton;
    ToolButton4: TToolButton;
    actNavForward: TAction;
    actNavBackward: TAction;
    mnuForward: TMenuItem;
    mnuBack: TMenuItem;
    mnuRefresh: TMenuItem;

    procedure FormActivate(Sender: TObject); override;
    procedure FormDeactivate(Sender: TObject); override;
    procedure FormCreate(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function  Navigate(value: string): boolean; override;
    procedure OnHelpMaster(Sender: TObject);
    procedure OnRefresh(Sender: TObject);
    procedure OnGoForward(Sender: TObject);
    procedure OnGoBackward(Sender: TObject);
    procedure wbOnlineHelpCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
    procedure wbOnlineHelpNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);

  private
    // private methods
    //

  protected
    // Protected Declarations
    //
    function    GetLocalToolbar: TToolbar; override;
    function    ReConnect: boolean; override;

  private
    // private members
    //
    m_FDocHostUIHandler: TDocHostUIHandler;

  public
    // public methods
    //
    function    Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean; override;
    function    Finalize: boolean; override;
    function    ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean; override;
    function    ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean; override;
  end;

implementation

uses
  Math,
  strUtils,
  Variants,
  ClipBrd,
  ComObj,
  daResourceStrings,
  MSHTML,
  ActiveX,
  IEConst;

{$R *.DFM}

// TfrmOnlineHelp
//   FormCreate
//
procedure TfrmOnlineHelp.FormCreate(Sender: TObject);
begin
  CursorStack.RegisterComponent(wbOnlineHelp);
  m_FDocHostUIHandler := TDocHostUIHandler.Create;
  m_FDocHostUIHandler.WebBrowser := wbOnlineHelp;
end;

// TfrmOnlineHelp
//   FormDestroy
//
procedure TfrmOnlineHelp.FormDestroy(Sender: TObject);
begin
  m_FDocHostUIHandler.WebBrowser := nil;
  FreeAndNil(m_FDocHostUIHandler);
end;

// TfrmOnlineHelp
//   FormShow
//
procedure TfrmOnlineHelp.FormShow(Sender: TObject);
begin
  SetToolbarLocation(ControlBar, Toolbar);
end;

// TfrmOnlineHelp
//   FormActivate
//
procedure TfrmOnlineHelp.FormActivate(Sender: TObject);
begin
  inherited FormActivate(sender);
  ActionList.State := asNormal;
end;

// TfrmOnlineHelp
//   FormDeactivate
//
procedure TfrmOnlineHelp.FormDeactivate(Sender: TObject);
begin
  inherited FormDeactivate(nil);
  ActionList.State := asSuspended;
  StatusText([psPanel1], ksEMPTY, esiNone);
end;

// TfrmOnlineHelp
//   Initialize
//
function TfrmOnlineHelp.Initialize(eparMode: TeFormInitialization; eparParameter: String): boolean;
begin
  inherited Initialize(eparMode, eparParameter);
  result := FormSet.CheckConnection(TRUE);
  FormSet.InitializeToolMenu(mnuTools);
  // Root Online Help?
  actHelpMaster.Enabled := FormSet.HasMasterOnlineHelp;
  actNavBackward.Enabled := FALSE;
  actNavForward.Enabled := FALSE;
  SetState(nil);
end;

// TfrmOnlineHelp
//   Finalize
//
function TfrmOnlineHelp.Finalize: boolean;
begin
  result := not CursorStack.HasSemaphore;
end;

// TfrmOnlineHelp
//   SetState
//
procedure TfrmOnlineHelp.SetState(Sender: TObject);
begin
  //
end;

// TfrmOnlineHelp
//   GetLocalToolbar
//
function TfrmOnlineHelp.GetLocalToolbar: TToolbar;
begin
  result := Toolbar;
end;

// TfrmOnlineHelp
//   Navigate
//
function TfrmOnlineHelp.Navigate(value: string): boolean;
var
  f: Variant;
begin
  try
    CursorStack.Push(crHourGlass);
    f := navNoHistory;
    wbOnlineHelp.Navigate(WideString(value), f);
    while wbOnlineHelp.ReadyState <> READYSTATE_COMPLETE do
    begin
      Application.ProcessMessages;
      Sleep(0);
    end;
    SetState(nil);
    result := TRUE;
  finally
    CursorStack.Pop;
  end;
end;

// TfrmOnlineHelp
//   onMasterHelp
//
procedure TfrmOnlineHelp.OnHelpMaster(Sender: TObject);
begin
  try
    CursorStack.Push(crHourGlass);
    FormSet.GetOnlineHelp(ksEMPTY);
    SetState(Sender);
  finally
    CursorStack.Pop;
  end;
end;

// TfrmOnlineHelp
//   ExecuteItem (1)
//
function TfrmOnlineHelp.ExecuteItem(value: String; eparOptionSet: TeQueryOptions): boolean;
begin
  result := FALSE;
end;

// TfrmOnlineHelp
//   ExecuteItem (2)
//
function TfrmOnlineHelp.ExecuteItem(value: TcFavoriteItem; eparOptionSet: TeQueryOptions): boolean;
begin
  result := FALSE;
end;

// TfrmOnlineHelp
//   OnRefresh
//
procedure TfrmOnlineHelp.OnRefresh(Sender: TObject);
begin
  try
    wbOnlineHelp.Refresh;
    SetState(Sender);
  except
    //
  end;
end;

// TfrmOnlineHelp
//   OnGoForward
//
procedure TfrmOnlineHelp.OnGoForward(Sender: TObject);
begin
  try
    wbOnlineHelp.GoForward;
    SetState(Sender);
  except
    //
  end;
end;

// TfrmOnlineHelp
//   OnGoBackward
//
procedure TfrmOnlineHelp.OnGoBackward(Sender: TObject);
begin
  try
    wbOnlineHelp.GoBack;
    SetState(Sender);
  except
    //
  end;
end;

// TfrmOnlineHelp
//   wbOnlineHelpCommandStateChange
//
procedure TfrmOnlineHelp.wbOnlineHelpCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK:
      actNavBackward.Enabled := Enable;
    CSC_NAVIGATEFORWARD:
      actNavForward.Enabled := Enable;
  end;
end;

// TfrmOnlineHelp
//   wbOnlineHelpNavigateComplete2
//
procedure TfrmOnlineHelp.wbOnlineHelpNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
var
  hr: HResult;
  CustDoc: ICustomDoc;
begin
  if m_FDocHostUIHandler <> nil then
  begin
    hr := wbOnlineHelp.Document.QueryInterface(ICustomDoc, CustDoc);
    if hr = S_OK then
      CustDoc.SetUIHandler(m_FDocHostUIHandler);
  end;
end;

// TfrmOnlineHelp
//   ReConnect
//
function TfrmOnlineHelp.ReConnect: boolean;
begin
  result := TRUE;
end;

end.




