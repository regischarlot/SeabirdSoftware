unit ConnectionBuilderLib;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows,
  Controls,
  Classes,
  DataLib,
  daObjectLib,
  PreferenceLib,
  ExtCtrls,
  StdCtrls,
  Buttons,
  Dialogs,
  Forms;

type
  TewoType = (ewoUndefined, ewoCaption, ewoDescription, ewoLabel, ewoEdit, ewoCombo, ewoCheckBox, ewoFile, ewoFolder, ewoMemo, ewoButton, ewoRadio, ewoPassword, ewoBevel, ewoPageControl, ewoTabSheet, ewoTest_Connection, ewoTest_FTP, ewoGet_ConnectionString);
  TeConnectionBuilderType = (ibtSetup, ibtLogon);

  TcConnectionBuilder = class(TcMetaData)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TcConnectionBuilder is the base meta object for reverse engineering.
  *
  * Inheritance:
  *   TcMetaData
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 07/24/00 Regis Created
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_objPreference: TcPreference;
    m_lstComponents: TList;
    m_objImageList: TImageList;
    m_OnFieldExit: TNotifyEvent;
    m_cstLABELLEFT: longint;
    m_cstLABELWIDTH: longint;
    m_cstEDITLEFT: longint;
    m_cstEDITWIDTH: longint;
    m_cstSPACING: longint;
    m_cstTOPSTART: longint;
    m_iTabOrder: longint;
    m_bUseWizard: boolean;
    m_cstPAGECONTROLBORDER: longint;
    m_frmCanvas: TForm;
    m_hdlTestFTP: TNotifyEvent;
    m_hdlTestConnection: TNotifyEvent;
    m_hdlGetConnectionString: TNotifyEvent;

  private
    // Private declarations
    //
    procedure   DoFieldExit(Sender: TObject);
    procedure   DoFieldEnter(Sender: TObject);
    procedure   SetRoot(parObject: TcMetaData);
    function    GetIsFull: boolean;
    function    SetComboValues(parCombo: TComboBox; parMeta: TcMetaData): boolean;
    procedure   SetEnabled;
    procedure   OnProvidersChange(Sender: TObject);
    function    TextExtent(value: String): TSize;

  published
    // published declarations
    //
    function    GetIsEmpty: boolean; override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;                                              // Standard destructor
    procedure   Clear; override;
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //
    // 2. Custom
    function    Build(eparType: TeConnectionBuilderType; parParent: TWinControl; parForm: TForm; parTopStart: longint): longint;
    function    SetObject(eparType: TewoType; sparCaption: String; parY, parHeight: longint; parParent: TWinControl; parObject: TObject; parMeta: TcMetaData; sparValue: String): longint;
    procedure   SetConstants(parLABELLEFT, parLABELWIDTH, parEDITLEFT, parEDITWIDTH, parSPACING, parTOPSTART, parPAGECONTROLBORDER: longint);
    procedure   SetProviderList(obj: TComponent; objMetaData: TcMetaData);

  public
    // Public properties
    //
    property    Preference: TcPreference             read m_objPreference       write m_objPreference;
    property    Root: TcMetaData                                                write SetRoot;
    property    IsFull: boolean                      read GetIsFull;
    property    OnFieldExit: TNotifyEvent            read m_OnFieldExit         write m_OnFieldExit;
    property    ImageList: TImageList                read m_objImageList        write m_objImageList;
    property    cstLABELLEFT: longint                read m_cstLABELLEFT        write m_cstLABELLEFT;
    property    cstLABELWIDTH: longint               read m_cstLABELWIDTH       write m_cstLABELWIDTH;
    property    cstEDITLEFT: longint                 read m_cstEDITLEFT         write m_cstEDITLEFT;
    property    cstEDITWIDTH: longint                read m_cstEDITWIDTH        write m_cstEDITWIDTH;
    property    cstSPACING: longint                  read m_cstSPACING          write m_cstSPACING;
    property    cstTOPSTART: longint                 read m_cstTOPSTART         write m_cstTOPSTART;
    property    UseWizard: boolean                   read m_bUseWizard          write m_bUseWizard;
    property    cstPAGECONTROLBORDER: longint        read m_cstPAGECONTROLBORDER write m_cstPAGECONTROLBORDER;
    property    frmCanvas: TForm                     read m_frmCanvas           write m_frmCanvas;
    property    hdlTestFTP: TNotifyEvent             read m_hdlTestFTP          write m_hdlTestFTP;
    property    hdlTestConnection: TNotifyEvent      read m_hdlTestConnection   write m_hdlTestConnection;
    property    hdlGetConnectionString: TNotifyEvent read m_hdlGetConnectionString write m_hdlGetConnectionString;
  end;

  TChooser = class(TCustomPanel)
 {******************************************************************************
  * Author: Regis Charlot
  *
  * Description: TChooser is the file or folder selection object.
  *
  * Inheritance:
  *   TCustomPanel
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 07/24/04 Regis Created
  *
  ******************************************************************************}
  private
    // Private Members
    //
    FEdit: TEdit;
    FButton: TBitBtn;
    FType: TewoType;
    FOpenDialog: TOpenDialog;
    FImageList: TImageList;

  private
    // Private declarations
    //
    procedure   SetText(value: String);
    function    GetText: String;
    procedure   OnClick(Sender: TObject);
    procedure   SetType(value: TewoType);
    procedure   DoChooserExit(Sender: TObject);

  public
    // Public Methods
    //
    constructor Create(AOwner: TComponent); override;

  public
    // Public Properties
    //
    property    SelType: TewoType                       read FType              write SetType;
    property    Text: String                            read GetText            write SetText;
    property    ImageList: TImageList                   read FImageList         write FImageList;
  end;

implementation

uses
  ActiveX,
  ComObj,
  sysUtils,
  FileCtrl,
  strUtils,
  daGlobals,
  daResourceStrings,
  Graphics,
  Variants,
  ComCtrls,
  ConnectionLib,
  ExecuteLib,
  ADODB_TLB,
  Math,
  frmConnectionWizard, // TfrmConnectionWizard
  frmConnectionLogin;  // TfrmConnectionLogin

// Tools
//   StringtoewoType
//
function StringtoewoType(value: String): TewoType;
const
  kasVALUES: array[TewoType] of string =
    (ksEMPTY, krsCAPTION, krsDESCRIPTION, krsLABEL, krsEDIT, krsCOMBOBOX, krsCHECKBOX, krsFILE, krsFOLDER, krsMEMO, krsBUTTON, krsRADIO, krsPASSWORD, krsBEVEL, krsPAGECONTROL, krsTABSHEET, krsTESTCONNECTION, krsTESTFTP, krsGETCONNECTIONSTRING);
var
  i: TewoType;
begin
  result := ewoUndefined;
  for i := succ(low(TewoType)) to high(TewoType) do
    if AnsiCompareText(kasVALUES[i], value) = 0 then
    begin
      result := i;
      break;
    end;
end;

//
// TChooser
//

// TChooser
//   Create
//
constructor TChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 144;
  Height := 21;
  FType := ewoFile;
  FImageList := nil;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  // Edit
  FEdit := TEdit.Create(self);
  FEdit.Parent := self;
  FEdit.Left := 0;
  FEdit.Top := 0;
  FEdit.Width := 121;
  FEdit.Height := 21;
  FEdit.Anchors := [akLeft, akTop, akRight, akBottom];
  FEdit.ReadOnly := FALSE;
  FEdit.OnExit := DoChooserExit;
  FEdit.TabStop := TRUE;
  // Button
  FButton := TBitBtn.Create(self);
  FButton.Parent := self;
  FButton.Left := 123;
  FButton.Top := 0;
  FButton.Width := 21;
  FButton.Height := 21;
  FButton.Anchors := [akTop, akRight, akBottom];
  FButton.OnClick := OnClick;
  FButton.OnExit := DoChooserExit;
  // OpenDialog
  FOpenDialog := TOpenDialog.Create(self);
  FOpenDialog.Filter := 'All Files (*.*)|*.*';
  FOpenDialog.Options := [ofPathMustExist, ofFileMustExist, ofEnableSizing];
  FOpenDialog.Title := 'Select a File';
end;

// TChooser
//   SetText
//
procedure TChooser.SetText(value: String);
begin
  FEdit.Text := value;
end;

// TChooser
//   GetText
//
function TChooser.GetText: String;
begin
  result := FEdit.Text;
end;

// TChooser
//   OnClick
//
procedure TChooser.OnClick(Sender: TObject);
var
  s: String;
begin
  case FType of
    ewoFile:
    begin
      FOpenDialog.FileName := Fedit.Text;
      if FOpenDialog.Execute then
      begin
        Fedit.Text := FOpenDialog.FileName;
        DoChooserExit(self);
      end;
    end;
    ewoFolder:
    begin
      s := Fedit.Text;
      if SelectDirectory('Select a Directory', ksEMPTY, s) then
      begin
        Fedit.Text := s;
        DoChooserExit(self);
      end;
    end;
  end;
end;

// TChooser
//   SetType
//
procedure TChooser.SetType(value: TewoType);
begin
  FType := value;
  if FImageList <> nil then
    case FType of
      ewoFile:
        FImageList.GetBitmap(0, FButton.Glyph);
      ewoFolder:
        FImageList.GetBitmap(1, FButton.Glyph);
    end;
end;

// TChooser
//   DoChooserExit
//
procedure TChooser.DoChooserExit(Sender: TObject);
begin
  if Assigned(OnExit) then
    OnExit(self);
end;

//
// TcConnectionBuilder
//

// TcConnectionBuilder
//   Create
//
constructor TcConnectionBuilder.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_objPreference := nil;
  m_lstComponents := TList.Create;
  m_objImageList := nil;
  m_OnFieldExit := nil;
  m_bUseWizard := TRUE;
  m_frmCanvas := nil;
  m_hdlTestFTP := nil;
  m_hdlTestConnection := nil;
  m_hdlGetConnectionString := nil;
end;

// TcConnectionBuilder
//   Destroy
//
destructor TcConnectionBuilder.Destroy;
begin
  m_lstComponents.Free;
  m_OnFieldExit := nil;
  inherited Destroy;
end;

// TcConnectionBuilder
//   Clear
//
procedure TcConnectionBuilder.Clear;
begin
  inherited Clear;
  // m_objPreference
  m_lstComponents.Clear;
  // m_objImageList
  // m_OnFieldExit
  // m_cstLABELLEFT
  // m_cstLABELWIDTH
  // m_cstEDITLEFT
  // m_cstEDITWIDTH
  // m_cstSPACING
  // m_cstTOPSTART
  // m_iTabOrder
  // m_bUseWizard
  // m_cstPAGECONTROLBORDER
  // m_frmCanvas
  // m_hdlTestFTP := nil;
  // m_hdlTestConnection := nil;
end;

// TcConnectionBuilder
//   SetRoot
//
procedure TcConnectionBuilder.SetRoot(parObject: TcMetaData);
begin
  Clear;
  if parObject <> nil then
    Copy(parObject);
end;

// TcConnectionBuilder
//   DoFieldExit
//
procedure TcConnectionBuilder.DoFieldExit(Sender: TObject);
var
  p, q: TcObject;
  i: longint;
begin
  if (m_objPreference <> nil) and (Sender <> nil) and (Sender is TComponent) and ((Sender as TComponent).Tag <> 0) then
  begin
    p := TcObject((Sender as TComponent).Tag);
    if p <> nil then
    begin
      if Sender is TEdit then
        m_objPreference.Attribute[p.sName] := (Sender as TEdit).Text
      else if Sender is TComboBox then
        m_objPreference.Attribute[p.sName] := (Sender as TComboBox).Text
      else if Sender is TChooser then
        m_objPreference.Attribute[p.sName] := (Sender as TChooser).Text
      else if Sender is TCheckBox then
        m_objPreference.Attribute[p.sName] := kasBOOL[(Sender as TCheckBox).Checked]
      else if (Sender is TRadioButton) and (p is TcMetaData) then
        m_objPreference.Attribute[p.sName] := (p as TcMetaData).Attribute[krsVALUE];
    end;
  end;
  // Connection String
  if (m_objPreference <> nil) and m_bUseWizard then
  begin
    m_objPreference.sADO := ksEMPTY;
    for i := 0 to Count - 1 do
      if (Objects[i] <> nil) and (Objects[i] is TcMetaData) and (Objects[i].eType = enScript) and ((m_objPreference.sProvider = ksEMPTY) or (AnsiCompareText(Objects[i].sName, m_objPreference.sProvider) = 0)) then
      begin
        // ADO String
        q := Objects[i].FindFirst(enValue);
        if q <> nil then
          m_objPreference.sADO := Expression(q.sValue, m_objPreference.ValuePairs, q as TcMetaData, ksEMPTY, []);
        // Comment
        m_objPreference.sComment := Objects[i].FindFirstValue(enComment);
        // Done
        break;
      end;
  end;
  // Field Exit Event
  if Assigned(m_OnFieldExit) then
    m_OnFieldExit(Sender);
  SetEnabled;
end;

// TcConnectionBuilder
//   DoFieldEnter
//
procedure TcConnectionBuilder.DoFieldEnter(Sender: TObject);
var
  p: TcObject;
begin
  if (m_objPreference <> nil) and (Sender <> nil) and (Sender is TComponent) and ((Sender as TComponent).Tag <> 0) then
  begin
    p := TcObject((Sender as TComponent).Tag);
    if (p <> nil) and (p is TcMetaData) and (Sender is TComboBox) then
      SetComboValues(sender as TComboBox, p as TcMetaData);
  end;
end;

// TcConnectionBuilder
//   SetComboValues
//
function TcConnectionBuilder.SetComboValues(parCombo: TComboBox; parMeta: TcMetaData): boolean;
var
  p: TcConnection;
  q: TcObject;
  e: TcExecute;
  rs: TcRecordSet;
  s, t: String;
  i: longint;
begin
  result := FALSE;
  try
    screen.cursor := crHourGlass;
    p := nil;
    //
    // A. Conmbo Values from SQL Statement
    q := parMeta.Find([enATTRIBUTE], krsSQL);
    if (q <> nil) and (m_objPreference <> nil) and (q.sValue <> ksEMPTY) then
    try
      p := TcConnection.Create(nil);
      p.ConnectionString := m_objPreference.sADO;
      try
        if p.Open then
        begin
          s := m_objPreference.Attribute[parMeta.sName];
          e := nil;
          parCombo.Items.BeginUpdate;
          parCombo.Items.Clear;
          try
            e := TcExecute.Create(nil);
            e.Connection := p;
            rs := nil;
            try
              try
                rs := e.Execute(q.sValue);
                while not rs.EOF do
                try
                  t := VarToStr(rs.Fields(0));
                  parCombo.Items.Add(t);
                  if AnsiCompareText(t, s) = 0 then
                    parCombo.ItemIndex := parCombo.Items.Count - 1;
                  rs.MoveNext;
                except
                  //
                end;
                rs.Close;
                result := TRUE;
              except
                //
              end;
            finally
              rs.free;
            end;
          finally
            e.Free;
          end;
          parCombo.Items.EndUpdate;
        end;
      except
        //
      end;
    finally
      p.free;
    end
    //
    // B. Conmbo Values from <VALUE> Tags
    else
    begin
      parCombo.Items.BeginUpdate;
      parCombo.Items.Clear;
      for i := 0 to parMeta.Count - 1 do
        if parMeta[i].eType = enValue then
          parCombo.Items.Add(parMeta[i].sName);
      parCombo.Items.EndUpdate;
    end;
  finally
    screen.cursor := crDefault;
  end;
end;

// TcConnectionBuilder
//   Build
//
function TcConnectionBuilder.Build(eparType: TeConnectionBuilderType; parParent: TWinControl; parForm: TForm; parTopStart: longint): longint;

  function recurseBuild(parParent: TWinControl; parObject: TcObject; parTopStart: longint): longint;
  var
    i: longint;
    p: TcMetaData;
    L: longint;
    t: TewoType;
  begin
    // Set row start
    result := parTopStart;
    if result = kiUNDEFINED then
      result := m_cstTOPSTART;
    //
    for i := 0 to parObject.Count - 1 do
      if (parObject[i] <> nil) and (parObject[i] is TcMetaData) then
      begin
        p := parObject[i] as TcMetaData;
        case p.eType of
          //
          // Field Object
          enField:
          begin
            t := StringtoewoType(p.Attribute[krsTYPE]);
            // Description Object.
            if p.Attribute[krsDESCRIPTION] <> ksEMPTY then
              result := SetObject(ewoDescription, p.Attribute[krsDESCRIPTION], result, 0, parParent, nil, p, ksEMPTY);
            // Create Entry Object
            L := result;
            case t of
              ewoEdit, ewoPassword,       // Edit Object.
              ewoCombo,                   // Combo Box Object.
              ewoFolder,                  // Folder Object.
              ewoFile,                    // File Object.
              ewoBevel:                   // Bevel
                result := SetObject(t, ksEMPTY, result, 0, parParent, nil, p, m_objPreference.Attribute[p.sName]);
              ewoCheckBox:                // Checkbox Object.
                result := SetObject(t, p.Attribute[krsTEXT], result, 0, parParent, nil, p, m_objPreference.Attribute[p.sName]);
              ewoRadio:                   // Radiobutton Object.
                result := SetObject(t, p.Attribute[krsCAPTION], result, 0, parParent, nil, p, m_objPreference.Attribute[p.sName]);
              ewoTest_Connection, ewoTest_FTP, ewoGet_ConnectionString:
                result := SetObject(t, p.Attribute[krsCAPTION], result, 0, parParent, nil, p, m_objPreference.Attribute[p.sName]);
            end;
            // Caption Object.
            if not (t in [ewoRadio, ewoTest_Connection, ewoTest_FTP, ewoGet_ConnectionString]) and (p.Attribute[krsCAPTION] <> ksEMPTY) then
              SetObject(ewoCaption, p.Attribute[krsCAPTION], L, result - L, parParent, nil, p, ksEMPTY);
            if not (t in [ewoRadio, ewoTest_Connection, ewoTest_FTP, ewoGet_ConnectionString]) then
              inc(result, m_cstSPACING)
            else
              inc(result, 4);
          end;
          //
          // Section Object
          enSection:
          begin
            case eparType of
              // For setup, create tabs.
              ibtSetup:
              begin
                if parParent is TPageControl then
                begin
                  SetObject(ewoTabSheet, p.Attribute[krsCAPTION], 0, 0, parParent, nil, p, ksEMPTY);
                  L := recurseBuild(p.objDisplay as TWinControl, p, 2);
                  with parParent as TPageControl do
                    Height := max(Height, L + 30);
                  result := parParent.Top + parParent.Height;
                end;
              end;
              // For logon, create bevels.
              ibtLogon:
              begin
                inc(result, kiSECTIONINTERVAL);
                if p.Attribute[krsCAPTION] <> ksEMPTY then
                  result := SetObject(ewoDescription, p.Attribute[krsCAPTION], result, 0, parParent, nil, p, ksEMPTY);
                result := SetObject(ewoBevel, ksEMPTY, result, 0, parParent, nil, p, ksEMPTY) + 2;
                result := recurseBuild(parParent, p, result);
              end;
            end;
          end;
        end;
      end;
    end;

var
  i, L: longint;
  p: TcObject;
const
  kasTYPE: array[TeConnectionBuilderType] of String =
    (krsSETUP, krsLOGON);
  kiHEIGHT = 290;
begin
  result := kiUNDEFINED;
  m_iTabOrder := 0;
  // First, Clean Panel
  for i := m_lstComponents.count - 1 downto 0 do
    TObject(m_lstComponents[i]).Free;
  m_lstComponents.Clear;
  // Create components
  p := Find(enOBJECT, kasTYPE[eparType]);
  if p <> nil then
  begin
    result := recurseBuild(parParent, p, parTopStart);
    if result > kiHEIGHT then
    begin
      L := parForm.Height;
      if parForm is TfrmConnectionWizard then
        L := (parForm as TfrmConnectionWizard).InitialHeight
      else if parForm is TfrmConnectionLogin then
        L := (parForm as TfrmConnectionLogin).InitialHeight;
      parForm.Height := L + (result - kiHEIGHT);
    end;
    SetEnabled;
  end;
end;

// TcConnectionBuilder
//   SetObject
//
function TcConnectionBuilder.SetObject(eparType: TewoType; sparCaption: String; parY, parHeight: longint; parParent: TWinControl; parObject: TObject; parMeta: TcMetaData; sparValue: String): longint;
var
  lbl: TLabel;
  edt: TEdit;
  cbo: TWinControl;
  H, W, L: longint;
  cb: TCheckBox;
  sel: TChooser;
  mmo: TMemo;
  btn: TButton;
  bbtn: TBitBtn;
  rdo: TRadioButton;
  bvl: TBevel;
  pc: TPageControl;
  ts: TTabSheet;
  r: double;
  pt: TSize;
begin
  result := parY;
  case eparType of
    ewoCaption, ewoDescription:
    begin
      lbl := parObject as TLabel;
      if lbl = nil then
      begin
        lbl := TLabel.Create(parParent);
        lbl.Parent := parParent;
        m_lstComponents.Add(lbl);
      end;
      if sparCaption <> ksEMPTY then
        lbl.Caption := sparCaption;
      if eparType = ewoDescription then
      begin
        H := lbl.Height;
        lbl.Left := m_cstLABELLEFT;
        W := m_cstEDITLEFT + m_cstEDITWIDTH - m_cstLABELLEFT;
        L := lbl.Width div W;
        if lbl.Width mod W <> 0 then
          inc(L);
        lbl.AutoSize := FALSE;
        lbl.WordWrap := TRUE;
        lbl.Height := H * L;
        lbl.Width := W;
        lbl.Top := result;
        lbl.Font.Color := clActiveCaption;
      end
      else if eparType = ewoCaption then
      begin
        lbl.AutoSize := FALSE;
        lbl.WordWrap := FALSE;
        lbl.Left := m_cstLABELLEFT;
        lbl.Width := m_cstLABELWIDTH;
        lbl.Alignment := taRightJustify;
        lbl.Top := result + (parHeight - lbl.Height) div 2;
        if parMeta <> nil then
          lbl.Tag := longint(pointer(parMeta));
      end;
      inc(result, lbl.Height + m_cstSPACING div 2);
      if parMeta <> nil then
        lbl.Tag := longint(pointer(parMeta));
    end;
    ewoEdit, ewoPassword:
    begin
      edt := parObject as TEdit;
      if edt = nil then
      begin
        edt := TEdit.Create(parParent);
        edt.Parent := parParent;
        m_lstComponents.Add(edt);
      end;
      edt.Top := result;
      edt.Left := m_cstEDITLEFT;
      edt.Width := m_cstEDITWIDTH;
      if parMeta <> nil then
        edt.Tag := longint(pointer(parMeta));
      if sparValue <> ksEMPTY then
        edt.Text := sparValue
      else if (parMeta <> nil) and (parMeta.Attribute[krsDEFAULT] <> ksEMPTY) then
        edt.Text := parMeta.Attribute[krsDEFAULT];
      if eparType = ewoPassword then
      begin
        edt.Font.Charset := DEFAULT_CHARSET;
        edt.Font.Color := clBlack;
        edt.Font.Height := -11;
        edt.Font.Name := 'Symbol';
        edt.Font.Style := [fsBold];
        edt.ParentFont := False;
        edt.PasswordChar := '·';
      end;
      edt.OnExit := DoFieldExit;
      edt.OnChange := DoFieldExit;
      inc(result, edt.Height);
      edt.TabOrder := m_iTabOrder;
      if (parMeta <> nil) and (pos('upper', parMeta.Attribute[krsOPTION]) > 0) then
        edt.Charcase := ecUppercase;
      inc(m_iTabOrder);
    end;
    ewoCombo:
    begin
      cbo := TWinControl(parObject);
      if parObject = nil then
      begin
        cbo := TComboBox.Create(parParent);
        cbo.Parent := parParent;
        m_lstComponents.Add(cbo);
      end;
      (cbo as TWinControl).Left := m_cstEDITLEFT;
      (cbo as TWinControl).Top := result;
      (cbo as TWinControl).Width := m_cstEDITWIDTH;
      if parMeta <> nil then
        (cbo as TWinControl).Tag := longint(pointer(parMeta));
      if cbo is TComboBox then
      begin
        (cbo as TComboBox).OnChange := DoFieldExit;
        (cbo as TComboBox).OnEnter := DoFieldEnter;
        if sparValue <> ksEMPTY then
          (cbo as TComboBox).Text := sparValue
        else if (parMeta <> nil) and (parMeta.Attribute[krsDEFAULT] <> ksEMPTY) then
          (cbo as TComboBox).Text := parMeta.Attribute[krsDEFAULT];
      end
      else if cbo is TComboBoxEx then
        (cbo as TComboBoxEx).OnChange := DoFieldExit;
      inc(result, (cbo as TWinControl).Height);
      cbo.TabOrder := m_iTabOrder;
      inc(m_iTabOrder);
    end;
    ewoCheckBox:
    begin
      cb := parObject as TCheckBox;
      if cb = nil then
      begin
        cb := TCheckBox.Create(parParent);
        cb.Parent := parParent;
        m_lstComponents.Add(cb);
      end;
      cb.Left := m_cstEDITLEFT;
      cb.Top := result;
      cb.Width := m_cstEDITWIDTH;
      if sparCaption <> ksEMPTY then
        cb.Caption := sparCaption;
      if parMeta <> nil then
        cb.Tag := longint(pointer(parMeta));
      if sparValue <> ksEMPTY then
        cb.Checked := sparValue = krsTRUE;
      cb.OnClick := DoFieldExit;
      cb.OnExit := DoFieldExit;
      inc(result, cb.Height);
      cb.TabOrder := m_iTabOrder;
      inc(m_iTabOrder);
    end;
    ewoFile, ewoFolder:
    begin
      sel := parObject as TChooser;
      if sel = nil then
      begin
        sel := TChooser.Create(parParent);
        sel.Parent := parParent;
        m_lstComponents.Add(sel);
      end;
      sel.Left := m_cstEDITLEFT;
      sel.Top := result;
      sel.Width := m_cstEDITWIDTH;
      sel.ImageList := m_objImageList;
      sel.SelType := eparType;
      if parMeta <> nil then
        sel.Tag := longint(pointer(parMeta));
      if sparValue <> ksEMPTY then
        sel.Text := sparValue
      else if (parMeta <> nil) and (parMeta.Attribute[krsDEFAULT] <> ksEMPTY) then
        sel.Text := parMeta.Attribute[krsDEFAULT];
      sel.OnExit := DoFieldExit;
      inc(result, sel.Height);
      sel.TabOrder := m_iTabOrder;
      inc(m_iTabOrder);
    end;
    ewoMemo:
    begin
      mmo := parObject as TMemo;
      if mmo = nil then
      begin
        mmo := TMemo.Create(parParent);
        mmo.Parent := parParent;
        m_lstComponents.Add(mmo);
      end;
      mmo.Top := result;
      mmo.Left := m_cstLABELLEFT;
      mmo.Width := m_cstEDITLEFT + m_cstEDITWIDTH - m_cstLABELLEFT;
      if parMeta <> nil then
        mmo.Tag := longint(pointer(parMeta));
      if sparValue <> ksEMPTY then
        mmo.Text := sparValue
      else if (parMeta <> nil) and (parMeta.Attribute[krsDEFAULT] <> ksEMPTY) then
        mmo.Text := parMeta.Attribute[krsDEFAULT];
      inc(result, mmo.Height);
      mmo.TabOrder := m_iTabOrder;
      inc(m_iTabOrder);
    end;
    ewoButton:
    begin
      btn := parObject as TButton;
      if btn = nil then
      begin
        btn := TButton.Create(parParent);
        btn.Parent := parParent;
        m_lstComponents.Add(btn);
      end;
      btn.Top := result;
      btn.Left := m_cstEDITLEFT;
      btn.Width := m_cstEDITWIDTH;
      if parMeta <> nil then
        btn.Tag := longint(pointer(parMeta));
      inc(result, btn.Height);
      btn.TabOrder := m_iTabOrder;
      inc(m_iTabOrder);
    end;
    ewoRadio:
    begin
      rdo := parObject as TRadioButton;
      if rdo = nil then
      begin
        rdo := TRadioButton.Create(parParent);
        rdo.Parent := parParent;
        m_lstComponents.Add(rdo);
      end;
      rdo.Top := result;
      rdo.Left := m_cstLABELLEFT;
      rdo.Width := parParent.Width - rdo.Left;
      rdo.Caption := sparCaption;
      rdo.Checked := AnsiCompareText(parMeta.Attribute[krsVALUE], m_objPreference.Attribute[parMeta.sName]) = 0;
      // Height?
      L := rdo.Height;
      rdo.WordWrap := TRUE;
      pt := TextExtent(rdo.Caption);
      r := pt.cx / (rdo.Width - 20);
      if r <> trunc(r) then
        r := r + 1;
      rdo.Height := trunc(r) * L;
      //
      rdo.OnClick := DoFieldExit;
      if parMeta <> nil then
        rdo.Tag := longint(pointer(parMeta));
      inc(result, rdo.Height);
      rdo.TabOrder := m_iTabOrder;
      inc(m_iTabOrder);
    end;
    ewoBevel:
    begin
      bvl := parObject as TBevel;
      if bvl = nil then
      begin
        bvl := TBevel.Create(parParent);
        bvl.Parent := parParent;
        m_lstComponents.Add(bvl);
      end;
      bvl.Top := result;
      bvl.Left := m_cstLABELLEFT;
      bvl.width := m_cstEDITLEFT + m_cstEDITWIDTH - m_cstLABELLEFT;
      bvl.Height := 2;
      inc(result, bvl.Height);
    end;
    ewoPageControl:
    begin
      pc := parObject as TPageControl;
      if pc = nil then
      begin
        pc := TPageControl.Create(parParent);
        pc.Parent := parParent;
        m_lstComponents.Add(pc);
      end;
      pc.Top := result;
      pc.Left := m_cstPAGECONTROLBORDER;
      pc.width := parParent.Width - 2 * m_cstPAGECONTROLBORDER;
      //pc.Height := 2;
      inc(result, pc.Height);
    end;
    ewoTabSheet:
    begin
      ts := parObject as TTabSheet;
      if ts = nil then
      begin
        ts := TTabSheet.Create(parParent);
        m_lstComponents.Add(ts);
        ts.PageControl := parParent as TPageControl;
        ts.Caption := sparCaption;
        m_lstComponents.Add(ts);
      end;
      parMeta.objDisplay := ts;
    end;
    ewoTest_Connection, ewoTest_FTP:
    begin
      bbtn := parObject as TBitBtn;
      if bbtn = nil then
      begin
        bbtn := TBitBtn.Create(parParent);
        bbtn.Parent := parParent;
        m_lstComponents.Add(bbtn);
      end;
      if sparCaption <> ksEMPTY then
        bbtn.Caption := sparCaption;
      bbtn.Top := result;
      pt := TextExtent(bbtn.Caption);
      bbtn.Width := max(pt.cx + 10, 30) + 25;
      bbtn.Left := m_cstEDITLEFT;
      bbtn.Height := pt.cy + 8;
      if m_objImageList <> nil then
      begin
        (bbtn as TBitBtn).Layout := blGlyphLeft;
        m_objImageList.GetBitmap(2, (bbtn as TBitBtn).Glyph);
        bbtn.Height := max(bbtn.Height, (bbtn as TBitBtn).Glyph.Height + 8);
      end;
      inc(result, bbtn.Height + m_cstSPACING div 2);
      if parMeta <> nil then
        bbtn.Tag := longint(pointer(parMeta));
      // onClick event
      if eparType = ewoTest_Connection then
        bbtn.OnClick := m_hdlTestConnection
      else if eparType = ewoTest_FTP then
        bbtn.OnClick := m_hdlTestFTP;
    end;
    ewoGet_ConnectionString:
    begin
      lbl := parObject as TLabel;
      if lbl = nil then
      begin
        lbl := TLabel.Create(parParent);
        lbl.Parent := parParent;
        m_lstComponents.Add(lbl);
      end;
      if sparCaption <> ksEMPTY then
        lbl.Caption := sparCaption;
      pt := TextExtent(lbl.Caption);
      lbl.AutoSize := FALSE;
      lbl.WordWrap := FALSE;
      lbl.Width := max(pt.cx + 10, 30) + 5;
      lbl.Left := m_cstEDITLEFT;
      lbl.Height := pt.cy + 2;
      lbl.Alignment := taLeftJustify;
      lbl.Top := result;
      lbl.Font.Color := clHighlight;
      lbl.Font.Style := [fsUnderline];
      inc(result, lbl.Height + m_cstSPACING div 2);
      if parMeta <> nil then
        lbl.Tag := longint(pointer(parMeta));
      lbl.OnClick := m_hdlGetConnectionString;
    end;
  end;
end;

// TcConnectionBuilder
//   SetConstants
//
procedure TcConnectionBuilder.SetConstants(parLABELLEFT, parLABELWIDTH, parEDITLEFT, parEDITWIDTH, parSPACING, parTOPSTART, parPAGECONTROLBORDER: longint);
begin
  m_cstLABELLEFT         := parLABELLEFT;
  m_cstLABELWIDTH        := parLABELWIDTH;
  m_cstEDITLEFT          := parEDITLEFT;
  m_cstEDITWIDTH         := parEDITWIDTH;
  m_cstSPACING           := parSPACING;
  m_cstTOPSTART          := parTOPSTART;
  m_cstPAGECONTROLBORDER := parPAGECONTROLBORDER;
end;

// TcConnectionBuilder
//   GetIsFull
//
function TcConnectionBuilder.GetIsFull: boolean;
var
  i: longint;
  p: TObject;
  m: TcObject;
begin
  result := TRUE;
  for i := 0 to m_lstComponents.count - 1 do
  begin
    p := TObject(m_lstComponents[i]);
    if (p <> nil) and (p is TControl) then
    begin
      m := TcObject((p as TControl).Tag);
      if (m <> nil) and (m is TcMetaData) and (AnsiCompareText((m as TcMetaData).Attribute[krsREQUIRED], krsTRUE) = 0) then
      begin
        if (p is TEdit) and (p as TEdit).Enabled then
          result := result and ((p as TEdit).Text <> ksEMPTY)
        else if (p is TComboBox) and (p as TComboBox).Enabled then
          result := result and ((p as TComboBox).Text <> ksEMPTY);
      end;
    end;
  end;
end;

// TcConnectionBuilder
//   SetEnabled
//
procedure TcConnectionBuilder.SetEnabled;
var
  i: longint;
  p: TObject;
  m: TcObject;
  s: String;
const
  kaCOL: array[boolean] of TColor =
    (clBtnFace, clWindow);
begin
  for i := 0 to m_lstComponents.count - 1 do
  begin
    p := TObject(m_lstComponents[i]);
    if (p <> nil) and (p is TControl) then
    begin
      m := TcObject((p as TControl).Tag);
      if (m <> nil) and (m is TcMetaData) then
      begin
        s := (m as TcMetaData).Attribute[krsENABLED];
        if s <> ksEMPTY then
        try
          (p as TControl).Enabled := VariantToBool((m as TcMetaData).Expression(s, m_objPreference.ValuePairs, m as TcMetaData, ksEMPTY, [], TRUE));
          if p is TEdit then
            (p as TEdit).Color := kaCOL[(p as TControl).Enabled];
        except
          //
        end;
      end;
    end;
  end;
end;

// TcConnectionBuilder
//   GetIsEmpty
//
function TcConnectionBuilder.GetIsEmpty: boolean;
begin
  result := m_lstComponents.Count = 0;
end;

// TcConnectionBuilder
//   SetProviderList
//
procedure TcConnectionBuilder.SetProviderList(obj: TComponent; objMetaData: TcMetaData);
var
  i: longint;
  s: String;
begin
  if (obj is TComboBox) and (objMetaData <> nil) and (m_objPreference <> nil) then
  begin
    (obj as TComboBox).Items.BeginUpdate;
    (obj as TComboBox).Items.Clear;
    for i := 0 to objMetaData.Count - 1 do
      if (objMetaData[i] <> nil) and (objMetaData[i] is TcMetaData) and (objMetaData[i].eType = enScript) then
      begin
        s := (objMetaData[i] as TcMetaData).Attribute[krsDISPLAY];
        if s = ksEMPTY then
          s := objMetaData[i].sName;
        (obj as TComboBox).Items.AddObject(AnsiReplaceText(s, '&', ksEMPTY), pointer(objMetaData[i]));
        if ((m_objPreference.sProvider = ksEMPTY) and (i = 0)) or (AnsiCompareTExt(objMetaData[i].sName, m_objPreference.sProvider) = 0) then
          (obj as TComboBox).ItemIndex := (obj as TComboBox).Items.Count - 1;
      end;
    with (obj as TComboBox) do
      if (ItemIndex = kiUNDEFINED) and (Items.Count > 0) then
        ItemIndex := 0;
    (obj as TComboBox).Items.EndUpdate;
    (obj as TComboBox).OnSelect := OnProvidersChange;
  end
end;

// TcConnectionBuilder
//   OnProvidersChange
//
procedure TcConnectionBuilder.OnProvidersChange(Sender: TObject);
var
  p: TcObject;
begin
  if (Sender <> nil) and (Sender is TComboBox) and ((Sender as TComboBox).ItemIndex <> kiUNDEFINED) and (m_objPreference <> nil) then
  begin
    with Sender as TComboBox do
      p := TcObject(Items.Objects[ItemIndex]);
    if p <> nil then
      m_objPreference.sProvider := p.sName;
    DoFieldExit(Sender);
  end;
end;

// TcConnectionBuilder
//   TextExtent
//
function TcConnectionBuilder.TextExtent(value: String): TSize;
begin
  result.cx := 0;
  result.cy := 0;
  if m_frmCanvas <> nil then
    result := m_frmCanvas.Canvas.TextExtent(value);
end;

end.



