unit FavoriteLib;

interface

uses
  daObjectLib,
  daGlobals,
  Classes;

type
  TcFavoriteItem = class;

  TcFavorite = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcFavorite is the object layer for Favorite Query storage
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/17/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_hdlQuery: TNotifyEvent;

  published
    // Published declarations
    //
    function    GetXML: String; override;
    procedure   SetXML(value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    function    Load: boolean;
    function    Save: boolean;
    //
    // 2. Custom
    function    Add(sparTitle: String; value: String): TcFavoriteItem; overload; virtual;
    function    Add(sparTitle: String; value: TcObject): TcFavoriteItem; overload; virtual;
    function    SendToObject(ptrObject: TObject): TObject; override;
    function    Edit: boolean; virtual;
    function    MoveUp(value: TcFavoriteItem): boolean;
    function    MoveDown(value: TcFavoriteItem): boolean;

  public
    // Public Properties
    //
    property    hdlQuery: TNotifyEvent                    read m_hdlQuery       write m_hdlQuery;
  end;

  TcFavoriteItem = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcFavorite is the object layer for Favorite Query storage
  *
  * Inheritance:
  *   TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 10/17/02 Regis Created
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_eFormType: TeForm;
    m_objStatement: TcObject;
    m_iPanelHeight: longint;

  published
    // Published declarations
    //
    function    GetXML: String; override;
    function    GetSQL: String;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor
    destructor  Destroy; override;                                              // Standard Destructor
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
    property    eFormType: TeForm                         read m_eFormType      write m_eFormType;
    property    objStatement: TcObject                    read m_objStatement   write m_objStatement;
    property    SQL: String                               read GetSQL;
    property    iPanelHeight: longint                     read m_iPanelHeight   write m_iPanelHeight;
  end;

implementation

uses
  Windows,
  ComObj,
  Variants,
  Forms,
  ComCtrls,         // TListView
  StdCtrls,         // TListBox
  sysUtils,
  frmFavorite    ,  // TfrmFavorite
  daResourceStrings,
  StatementLib,     // TcStatement
  Menus;            // TMenuITem

//
// TcFavorite
//

// TcFavorite
//   Constructor
//
constructor TcFavorite.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_hdlQuery := nil;
end;

//
//   Load
//
function TcFavorite.Load: boolean;
begin
  SetXML(GetFilePath(Application.ExeName) + krsXML_QUERIESFILENAME);
  result := Count > 0;
end;

// TcFavorite
//   Save
//
function TcFavorite.Save: boolean;
begin
  result := FALSE;
  if GetXML <> ksEMPTY then
  begin
    FileSetReadOnly(GetFilePath(Application.ExeName) + krsXML_QUERIESFILENAME, FALSE);
    result := StringToFile(GetFilePath(Application.ExeName) + krsXML_QUERIESFILENAME, GetXML, ecsAnsi);
  end;
end;

// TcFavorite
//   GetXML
//
function TcFavorite.GetXML: String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to count - 1 do
    if Objects[i] <> nil then
      result := result + kcTAB + Objects[i].XML + ksCR;
  if result <> ksEMPTY then
    result := Format('<%s>', [krsXML_FAVORITELIST]) + ksCR + result + Format('</%s>', [krsXML_FAVORITELIST]);
end;

// TcFavorite
//   SetXML
//
procedure TcFavorite.SetXML(value: String);

  procedure SubLoad(parParent: TcObject; pOLE: OLEVariant);
  var
    i: longint;
    p: TcFavoriteItem;
  begin
    if (pOLE.nodeType = ntNODE_ELEMENT) and (AnsiCompareText(pOLE.nodeName, krsXML_FAVORITE) = 0) then // NODE_ELEMENT only
    begin
      p := TcFavoriteItem.Create(parParent);
      parParent.Add(p);
      //
      // Attributes
      for i := 0 to pOLE.attributes.length - 1 do
      begin
        // Name
        if AnsiCompareText(pOLE.attributes.item[i].name, krsNAME) = 0 then
          p.sName := pOLE.attributes.item[i].value;
        // FormType
        if AnsiCompareText(pOLE.attributes.item[i].name, krsTYPE) = 0 then
          p.eFormType := TeForm(strtointdef(pOLE.attributes.item[i].value, longint(efsQuery)));
        // Panel Height
        if AnsiCompareText(pOLE.attributes.item[i].name, krsXML_PANELHEIGHT) = 0 then
          p.iPanelHeight := strtointdef(pOLE.attributes.item[i].value, kiUNDEFINED);
      end;
      //
      // Child nodes..?
      for i := 0 to pOLE.childNodes.length - 1 do
        if pOLE.childNodes.item[i].nodeType = ntNODE_TEXT then
          p.sValue := p.sValue + XMLToText(VarToStr(pOLE.childNodes.item[i].nodeValue))
        else if pOLE.childNodes.item[i].nodeType = ntNODE_ELEMENT then
        begin
          if p.m_objStatement = nil then
            p.m_objStatement := TcStatement.Create(nil);
          p.m_objStatement.XML := pOLE.childNodes.item[i].XML;
        end;
    end
    else
      for i := 0 to pOLE.childNodes.length - 1 do
        SubLoad(parParent, pOLE.childNodes.item[i]);
  end;

var
  p: OLEVariant;
begin
  Clear;
  try
    p := CreateOLEObject('Microsoft.XMLDOM');
    if p.Load(value) then
    begin
      if p.childNodes.Length > 0 then
        SubLoad(self, p.childNodes.item[0]);
    end
  except
    //
  end;
  p := unassigned;
end;

// TcFavorite
//   Add (1)
//
function TcFavorite.Add(sparTitle: String; value: String): TcFavoriteItem;
begin
  result := TcFavoriteItem.Create(self);
  Add(result);
  result.sName := sparTitle;
  result.eFormType := efsQuery;
  result.sValue := trim(value);
end;

// TcFavorite
//   Add (2)
//
function TcFavorite.Add(sparTitle: String; value: TcObject): TcFavoriteItem;
begin
  result := TcFavoriteItem.Create(self);
  Add(result);
  result.sName := sparTitle;
  result.eFormType := efsGrid;
  result.m_objStatement := TcStatement.Create(nil);
  result.m_objStatement.Copy(value);
end;

// TcFavorite
//   SendToObject
//
function TcFavorite.SendToObject(ptrObject: TObject): TObject;
var
  i: longint;
  m, p: TMenuItem;
  li: TListItem;
  q: TObject;
begin
  result := nil;
  //
  // TMenuItem
  if (ptrObject <> nil) and (ptrObject is TMenuItem) then
  begin
    m := ptrObject as TMenuItem;
    // 1. Clean up
    for i := m.Count - 1 downto 0 do
      if m[i].GroupIndex = 1 then
        m.Delete(i);
    // 2. Add Items.
    for i := 0 to Count - 1 do
    begin
      p := TMenuItem.Create(m);
      m.Add(p);
      p.Caption := Objects[i].sName;
      p.GroupIndex := 1;
      p.ImageIndex := kaiFORMSMALLIMAGEINDEX[(Objects[i] as TcFavoriteItem).eFormType];
      if i < 10 then
        p.Caption := Format('&%d ', [i + 1]) + p.Caption;
      p.onClick := m_hdlQuery;
      p.Tag := i;
    end;
  end
  //
  // ListView
  else if (ptrObject <> nil) and (ptrObject is TListView) then
    with ptrObject as TListView do
    begin
      Items.BeginUpdate;
      Items.Clear;
      for i := 0 to Count - 1 do
        if (Objects[i] <> nil) and (Objects[i] is TcFavoriteItem) then
        begin
          li := Items.Add;
          li.Caption := Objects[i].sName;
          li.Data := Objects[i];
        end;
      Items.EndUpdate;
    end
  //
  // ListBox
  else if (ptrObject <> nil) and (ptrObject is TListBox) then
    with ptrObject as TListBox do
    begin
      q := nil;
      if ItemIndex <> kiUNDEFINED then
        q := Items.Objects[ItemIndex];
      Items.BeginUpdate;
      Items.Clear;
      for i := 0 to self.Count - 1 do
      begin
        Items.AddObject(self[i].sName, self[i]);
        if self[i] = q then
          ItemIndex := i;
      end;
      Items.EndUpdate;
    end;
end;

// TcFavorite
//   Edit
//
function TcFavorite.Edit: boolean;
var
  frm: TfrmFavorite;
begin
  frm := nil;
  try
    frm := TfrmFavorite.Create(nil);
    frm.Favorite := self;
    result := frm.ShowModal = idOK;
  finally
    frm.Release;
  end;
end;

// TcFavorite
//   MoveUp
//
function TcFavorite.MoveUp(value: TcFavoriteItem): boolean;
begin
  result := IndexOf(value) > 0;
  if result then
    Exchange(IndexOf(value), IndexOf(value) - 1);
end;

// TcFavorite
//   MoveUp
//
function TcFavorite.MoveDown(value: TcFavoriteItem): boolean;
begin
  result := IndexOf(value) < count - 1;
  if result then
    Exchange(IndexOf(value), IndexOf(value) + 1);
end;

//
// TcFavoriteItem
//

// TcFavoriteItem
//   Constructor
//
constructor TcFavoriteItem.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_eFormType := efsQuery;
  m_objStatement := nil;
  m_iPanelHeight := kiUNDEFINED;
end;

// TcFavoriteItem
//   Destroy
//
destructor TcFavoriteItem.Destroy;
begin
  if m_objStatement <> nil then
    m_objStatement.free;
  inherited Destroy;
end;

// TcFavoriteItem
//   GetXML
//
function TcFavoriteItem.GetXML: String;
begin
  result := Format('<%s %s="%s" %s="%d" %s="%d">', [krsXML_FAVORITE, krsNAME, sName, krsTYPE, longint(eFormType), krsXML_PANELHEIGHT, m_iPanelHeight]);
  case m_eFormType of
    efsQuery:
      result := result + TextToXML(sValue);
    efsGrid:
      if m_objStatement <> nil then
        result := result + m_objStatement.XML;
  end;
  result := result + Format('</%s>', [krsXML_FAVORITE]);
end;

// TcFavoriteItem
//   GetSQL
//
function TcFavoriteItem.GetSQL: String;
begin
  result := ksEMPTY;
  case m_eFormType of
    efsQuery:
      result := sValue;
    efsGrid:
      if (m_objStatement <> nil) and (m_objStatement is TcStatement) then
        result := (m_objStatement as TcStatement).SQL;
  end;
end;

end.


