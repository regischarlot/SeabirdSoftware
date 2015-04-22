unit frmFavorite;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ConnectionLib,
  ComCtrls, ExtCtrls, PreferenceLib, ImgList, Menus, ActnList,
  FavoriteLib, Buttons;

type
  TfrmFavorite = class(TForm)
    popList: TPopupMenu;
    pnlRight: TPanel;
    btnClose: TButton;
    btnDelete: TButton;
    btnRename: TButton;
    popEdit: TMenuItem;
    popDelete: TMenuItem;
    btnHelp: TButton;
    lstFavorites: TListBox;
    btnMoveDown: TBitBtn;
    btnMoveUp: TBitBtn;
    popMoveUp: TMenuItem;
    popMoveDown: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SetState(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvFavoritesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);

  private
    m_objFavorite: TcFavorite;

  private
    procedure SetDisplay(Sender: TObject);

  public
    property Favorite: TcFavorite       read m_objFavorite                      write m_objFavorite;
  end;

implementation

uses
  Main,
  daGlobals,
  daObjectLib,
  DBLogDlg,
  ADODB_TLB,
  daResourceStrings;

{$R *.DFM}

// TfrmFavorite
//   FormCreate
//
procedure TfrmFavorite.FormCreate(Sender: TObject);
begin
  m_objFavorite := nil;
end;

// TfrmFavorite
//   FormClose
//
procedure TfrmFavorite.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

// TfrmFavorite
//   FormShow
//
procedure TfrmFavorite.FormShow(Sender: TObject);
begin
  SetDisplay(Sender);
  SetState(Sender);
end;

// TfrmFavorite
//   SetDisplay
//
procedure TfrmFavorite.SetDisplay(Sender: TObject);
begin
  m_objFavorite.SendToObject(lstFavorites);
  SetState(Sender);
end;

// TfrmFavorite
//   SetState
//
procedure TfrmFavorite.SetState(Sender: TObject);
var
  L: longint;
begin
  L := lstFavorites.ItemIndex;
  btnDelete.Enabled := L <> kiUNDEFINED;
  btnRename.Enabled := L <> kiUNDEFINED;
  btnMoveUp.Enabled := (L <> kiUNDEFINED) and (L > 0);
  btnMoveDown.Enabled := (L <> kiUNDEFINED) and (L < lstFavorites.Items.count - 1);
end;

// TfrmFavorite
//   lvConnectionChange
//
procedure TfrmFavorite.lvFavoritesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  SetState(Sender);
end;

// TfrmFavorite
//   btnRenameClick
//
procedure TfrmFavorite.btnRenameClick(Sender: TObject);
var
  s: String;
  p: TcObject;
begin
  if lstFavorites.ItemIndex <> kiUNDEFINED then
  begin
    with lstFavorites do
      p := TcObject(Items.Objects[ItemIndex]);
    if p <> nil then
    begin
      s := InputBox('Rename Favorite Query Title', 'Please specify a ''Favorite Query'' Title', p.sName);
      if s <> ksEMPTY then
      begin
        p.sName := s;
        m_objFavorite.Save;
        SetDisplay(Sender);
      end;
    end;
  end;
end;

// TfrmFavorite
//   btnDeleteClick
//
procedure TfrmFavorite.btnDeleteClick(Sender: TObject);
var
  p: TcObject;
begin
  if lstFavorites.ItemIndex <> kiUNDEFINED then
  begin
    with lstFavorites do
      p := TcObject(Items.Objects[ItemIndex]);
    if p <> nil then
    begin
      if Application.MessageBox(PChar(Format('Delete ''%s''?', [p.sName])), krsINFORMATION, MB_YESNO + MB_ICONEXCLAMATION) = mrYES then
      begin
        m_objFavorite.Delete(p);
        m_objFavorite.Save;
        SetDisplay(Sender);
      end;
    end;
  end;
end;

// TfrmFavorite
//   btnMoveDownClick
//
procedure TfrmFavorite.btnMoveDownClick(Sender: TObject);
var
  p: TcObject;
begin
  with lstFavorites do
    if (ItemIndex <> kiUNDEFINED) and (ItemIndex < Items.Count - 1) then
    begin
      with lstFavorites do
        p := TcObject(Items.Objects[ItemIndex]);
      if (p <> nil) and (p is TcFavoriteItem) then
      begin
        m_objFavorite.MoveDown(p as TcFavoriteItem);
        m_objFavorite.Save;
        SetDisplay(Sender);
      end;
    end;
end;

// TfrmFavorite
//   btnMoveUpClick
//
procedure TfrmFavorite.btnMoveUpClick(Sender: TObject);
var
  p: TcObject;
begin
  with lstFavorites do
    if (ItemIndex <> kiUNDEFINED) and (ItemIndex > 0) then
    begin
      with lstFavorites do
        p := TcObject(Items.Objects[ItemIndex]);
      if (p <> nil) and (p is TcFavoriteItem) then
      begin
        m_objFavorite.MoveUp(p as TcFavoriteItem);
        m_objFavorite.Save;
        SetDisplay(Sender);
      end;
    end;
end;

end.




