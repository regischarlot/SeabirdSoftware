unit GraphPanel;

(******************************************************************************
 * Filename: GraphPanel.PAS
 *
 * Author: Regis Charlot
 *         (c) Copyrights 2001-2004 Seabird Software, LLC
 *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
 *
 * Created: April 20, 1998
 *
 * Description: daObjectLib is the base unit for all objects.
 *
 * Classes defined:
 *   TGraphPanel - Visual Component
 *   TcGraph - Base Data Layer for graph nodes
 *   TcNode - Base graph node
 *   TcEdge - Node to node relationship
 *
 * Public procedures / functions:
 *   None
 *
 * Database objects used:
 *   None
 *
 * Notes:
 *   None
 *
 * Revision History:
 * Date     Who   Reason
 * -------- ----- ------------------------------------------------------------
 * 12/17/00 Regis Created
 * 07/03/01 Regis Shared with IMO
 * 03/20/04 Regis Kickoff version 2
 * 03/28/04 Regis Completed version 2
 *
 ******************************************************************************)

interface

uses
  ExtCtrls,  // TCustomPanel
  Classes,
  Controls,
  Graphics,
  Menus,
  daObjectLib,
  Forms;

type
  TedAction = (edAdd, edDelete, edUpdate);
  TDifferenceCallBack = function (parParent, parObject: TcObject; edAction: TedAction): TcObject of object;
  TeColor = (ecNode, ecSelectedNode, ecFromNodeDependency, ecToNodeDependency, ecEdge, ecSelectedEdge, ecBackground);
  TSelectEvent = procedure(const sType, sName: string) of object;
  TTextSelectEvent = procedure(const value: string) of object;
  TdPoint = record X, Y: double; end;
  TOnChangeLocation = procedure(const ID: longint; Value: TdPoint) of object;

  TGraphPanel = class(TScrollBox)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TGraphPanel is the graphing component
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/17/00 Regis Created
  * 07/03/01 Regis Shared with IMO
  * 03/20/04 Regis Kickoff version 2
  * 03/28/04 Regis Completed version 2
  *
  ******************************************************************************}
  private
    // private methods
    //
    m_objGraph: TcObject;
    m_imgIcons: TImageList;
    m_imgAlerts: TImageList;
    m_OnChange: TNotifyEvent;
    m_OnSelect: TSelectEvent;
    m_OnTextSelect: TTextSelectEvent;
    m_popPanel: TPopupMenu;
    m_OnChangeLocation: TOnChangeLocation;

  private
    // Private declarations
    //
    function    GetXML: String;
    procedure   SetXML(Value: String);
    function    GetMultiSelect: boolean;
    procedure   SetMultiSelect(Value: boolean);
    procedure   SetImageIcons(value: TImageList);
    function    GetColors(Index: TeColor): TColor;
    procedure   SetColors(Index: TeColor; value: TColor);
    function    GetVertScroll: longint;
    function    GetHorizScroll: longint;
    function    GetIsEmpty: boolean;
    function    GetProperties(Tag: String; ID: longint): String;
    procedure   SetProperties(Tag: String; ID: longint; value: String);
    procedure   SetSelected(value: longint);
    function    GetSelected: longint;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // public properties
    //
    property    Colors[Index: TeColor]: TColor          read GetColors          write SetColors;
    property    IsEmpty: boolean                        read GetIsEmpty;
    property    Properties[Tag: String; ID: longint]: String read GetProperties write SetProperties;

  published
    // Published properties
    //
    property    XML: String                             read GetXML             write SetXML;
    property    MultiSelect: boolean                    read GetMultiSelect     write SetMultiSelect;
    property    ImageIcons: TImageList                  read m_imgIcons         write SetImageIcons;
    property    ImageAlerts: TImageList                 read m_imgAlerts        write m_imgAlerts;
    property    OnChange: TNotifyEvent                  read m_OnChange         write m_OnChange;
    property    OnSelect: TSelectEvent                  read m_OnSelect         write m_OnSelect;
    property    OnTextSelect: TTextSelectEvent          read m_OnTextSelect     write m_OnTextSelect;
    property    VertScroll: longint                     read GetVertScroll;
    property    HorizScroll: longint                    read GetHorizScroll;
    property    PopupMenuPanel: TPopupMenu              read m_popPanel         write m_popPanel;
    property    Selected: longint                       read GetSelected        write SetSelected;
    property    OnChangeLocation: TOnChangeLocation     read m_OnChangeLocation write m_OnChangeLocation;
    //
    // Inherited Properties
    //
    property    Align;
    property    Anchors;
    property    AutoSize;
    property    BevelInner;
    property    BevelOuter;
    property    BevelWidth;
    property    BiDiMode;
    property    BorderWidth;
    property    BorderStyle;
    property    Caption;
    property    Color;
    property    Constraints;
    property    Ctl3D;
    property    UseDockManager default TRUE;
    property    DockSite;
    property    DragCursor;
    property    DragKind;
    property    DragMode;
    property    Enabled;
    property    Font;
    property    ParentBiDiMode;
    property    ParentColor;
    property    ParentCtl3D;
    property    ParentFont;
    property    ParentShowHint;
    property    PopupMenu;
    property    ShowHint;
    property    TabOrder;
    property    TabStop;
    property    Visible;
    property    OnCanResize;
    property    OnClick;
    property    OnConstrainedResize;
    property    OnContextPopup;
    property    OnDockDrop;
    property    OnDockOver;
    property    OnDblClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDock;
    property    OnEndDrag;
    property    OnEnter;
    property    OnExit;
    property    OnGetSiteInfo;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnResize;
    property    OnStartDock;
    property    OnStartDrag;
    property    OnUnDock;
  end;

  TObjectPanel = class(TCustomPanel)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TObjectPanel describes the base Graph-panel panel object
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 03/20/01 Regis Created
  * 07/05/01 Regis Adapted for Intelligent Medical Objects, Inc.
  * 03/28/04 Regis Revised for Revision 2
  *
  ******************************************************************************}
  private
    // private declarations
    //
    m_iIcon: longint;
    m_iAlertLevel: longint;
    m_imgIcons: TImageList;
    m_imgAlerts: TImageList;
    m_sText: TCaption;
    m_sMetaName: String;
    m_sDataName: String;

  private  // private declarations
    //
    procedure   SetIcon(value: longint);
    function    GetIndex: longint;
    function    GetText: TCaption;
    procedure   SetText(const Value: TCaption);
    procedure   SetAlertLevel(value: longint);

  public
    // public declarations
    //
    constructor Create(AOwner: TComponent); override;
    procedure   Paint; override;

  public
    // public declarations
    //
    property    Text: TCaption read GetText write SetText;
    property    DragMode;
    property    Canvas;
    property    Color;
    property    Alignment;
    property    onClick;
    property    Font;
    property    Tag;
    property    Top;
    property    Left;
    property    width;
    property    height;
    property    DragCursor;
    property    onMouseDown;
    property    onMouseUp;
    property    OnMouseMove;
    property    DragKind;
    property    onDragOver;
    property    PopupMenu;
    property    BevelOuter;
    property    Icon: longint                           read m_iIcon            write SetIcon       default 0;
    property    AlertLevel: longint                     read m_iAlertLevel      write SetAlertLevel default -1;
    property    imgIcons: TImageList                    read m_imgIcons         write m_imgIcons    default nil;
    property    imgAlerts: TImageList                   read m_imgAlerts        write m_imgAlerts   default nil;
    property    Index: longint                          read GetIndex;
    property    sMetaName: String                       read m_sMetaName        write m_sMetaName;
    property    sDataName: String                       read m_sDataName        write m_sDataName;
  end;

procedure Register;

implementation

uses
  Windows,
  SysUtils,
  Math,
  daResourceStrings,
  daGlobals,
  ComObj,
  Variants,
  Types,
  ActiveX, // Succeeded(), CoInitialize()
  SAX,
  SAXComps,
  SAXMS,
  SAXExt,
  SAXHelpers,
  oleCtnrs;

type
  TaColors = array[TeColor] of TColor;

const
  kiTEXTSPACE           = 4;
  kfLARGEDOUBLE         = 1.7e308;
  kiUNDEFINED           = -1;
  kiWIDTH               = 1000;
  kiHEIGHT              = 1000;
  kiPANELWIDTH          = 64;
  kiPANELHEIGHT         = 20;
  ksTABOODISTANCE       = kiPANELWIDTH * 1.5;
  kacCOLORS: TaColors   = ({ecNode}               $00FFE6E6, // Light Slate Blue
                           {ecSelectedNode}       $00BBFFFF, // Light Yellow
                           {ecFromNodeDependency} $00AEC9FF, // Orange
                           {ecToNodeDependency}   $00C6FFC6, // Light Green
                           {ecEdge}               clLtGray,  // Light Gray
                           {ecSelectedEdge}       clBlack,   // Black
                           {ecBackground}         clCream);  // Cream
type
  TcNode = class;
  TcCustomNode = class;
  TeDistribution = (edRectangular, edTriangular, edCircular);

  //
  // TcGraph
  //
  TcGraph = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TcGraph is the data layer for the TGraphPanel graphing component
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/17/00 Regis Created
  * 07/03/01 Regis Shared with IMO
  * 03/20/04 Regis Kickoff version 2
  * 03/28/04 Regis Completed version 2
  *
  ******************************************************************************}
  private
    // Private members
    //
    m_lstNodes, m_lstEdges: TcBag; // Vector
    m_ParentControl: TGraphPanel;
    m_bMultiSelect: boolean;
    m_idragdropX: longint;
    m_idragdropY: longint;
    m_aColors: TaColors;
    m_iImageWidth: longint;
    m_objPanel: TObjectPanel;
    m_objNode: TcNode;
    m_bPanelMoved: boolean;
    m_iSelected: longint;

    m_objWinner: TcNode;
    m_lstQueue: TcBag; // Vector
    m_iLastX, m_iLastY: double;
    min_x, max_x, min_y, max_y: integer;
    max_epochs, epoch: longint;
    radius_constant_time, radius, min_radius: longint;
    adaption, initial_adaption, min_adaption: double;
    factor, cooling_factor: double;
    tabooDist: double;
    temp: double;
    bTaboo, bClipping, bRandom: boolean;
    jump_radius, initial_jump_radius: longint;
    m_eDistribution: TeDistribution;

  private
    // Private declarations
    //
    function    GetSize: TPoint;
    function    closestNode(x, y: double): TcNode;
    function    InsideDistribution(x, y: double): boolean;
    function    RandomVector: TdPoint;
    procedure   adjust();
    procedure   update_parameters;
    procedure   random_input;
    procedure   adjustNodes(n: TcNode);
    function    inside_triangle(x, y: double): boolean;
    function    inside_circle(x, y: double): boolean;
    procedure   tabooCheck;
    procedure   Resize;
    procedure   PaintEdge(Sender: TObject);
    function    OnDifferenceCallBack(parNode1, parNode2: TcObject; edAction: TedAction): TcObject;
    procedure   RefreshProperties(parObject: TcObject; bRepaint: boolean);
    function    GetMultiSelect: boolean;
    procedure   SetMultiSelect(Value: boolean);
    function    AddObject(parObject: TcObject): longint;
    procedure   DeleteObject(parObject: TcObject);
    procedure   RePaint(parObject: TcObject);
    function    GetColors(Index: TeColor): TColor;
    procedure   SetColors(Index: TeColor; value: TColor);
    procedure   ResetColors(Sender: TObject);
    procedure   ReColor(Sender: TObject);
    function    GetNodeByID(value: longint): TcObject;
    function    GetProperties(Tag: String; ID: longint): String;
    procedure   SetProperties(Tag: String; ID: longint; value: String);
    // Events
    procedure   OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure   OnChange(Sender: TObject);
    function    SelectPanel(Sender: TObject; bToggle, bInternal: boolean): boolean;
    procedure   SetSelected(value: longint);
    function    GetSelected: longint;
    procedure   SetLocation(Index: longint; Value: TdPoint);

  published
    // Published declarations
    //
    function    GetIsEmpty: boolean; override;
    procedure   SetXML(Value: String); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;
    destructor  Destroy; override;
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    procedure   Run;
    function    Initialize: boolean;
    procedure   Close;

  public
    // Public Properties
    //
    property    ParentControl: TGraphPanel              read m_ParentControl    write m_ParentControl;
    property    IsEmpty: boolean                        read GetIsEmpty;
    property    MultiSelect: boolean                    read GetMultiSelect     write SetMultiSelect;
    property    Colors[Index: TeColor]: TColor          read GetColors          write SetColors;
    property    ImageWidth: longint                     read m_iImageWidth      write m_iImageWidth;
    property    Properties[Tag: String; ID: longint]: String read GetProperties write SetProperties;
    property    PanelMoved: boolean                     read m_bPanelMoved      write m_bPanelMoved;
    property    Selected: longint                       read GetSelected        write SetSelected;
  end;

  TcCustomNode = class(TcObject)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TcCustomNode is the base class for TcNode and TcEdge
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/17/00 Regis Created
  * 07/03/01 Regis Shared with IMO
  * 03/20/04 Regis Kickoff version 2
  * 03/28/04 Regis Completed version 2
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_eColor: TeColor;

  published
    // Published declarations
    //
    procedure   SetColor(value: TeColor); virtual;
    procedure   LocalDelete(parObject: TcObject);

  public
    // Public declarations
    //
    // 1. Standard
    //   Create
    //   Destroy
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // public methods
    //
    function    Add(parObject: TcObject): longint; override;
    function    Delete(parObject: TcObject): longint; override;
    function    Delete(Index: longint): longint; override;
    function    ResolveDifferences(parObject: TcCustomNode; hdlDifferenceCallBack: TDifferenceCallBack): boolean;

  public
    // Public declarations
    //
    property    eColor: TeColor                         read m_eColor           write SetColor;
  end;

  TcNode = class(TcCustomNode)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TcNode is the Node support class
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/17/00 Regis Created
  * 07/03/01 Regis Shared with IMO
  * 03/20/04 Regis Kickoff version 2
  * 03/28/04 Regis Completed version 2
  *
  ******************************************************************************}
  private
    // private methods
    //
    m_iIndex: longint;
    m_dX, m_dY: double;
    old_x, old_y: double;
    m_bFixed: boolean;
    m_bVisited: boolean;
    m_iDistance: longint;
    m_objPanel: TObjectPanel;
    m_iIcon: longint;
    m_iAlertLevel: longint;

  private
    // Private declarations
    //
    function    GetPanel: TObjectPanel;
    procedure   SetPanel(value: TObjectPanel);
    function    GetCenterX: longint;
    function    GetCenterY: longint;
    function    GetHint: String;
    function    GetCaption: String;
    procedure   SetIcon(value: longint);
    function    GetPoint: TdPoint;
    procedure   SetPoint(value: TdPoint);
    procedure   SetAlertLevel(value: longint);

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); overload; override;                // Standard Constructor, virtual
    destructor  Destroy; override;
    //   Clear
    //   Copy
    function    Compare(parValue: TcObject): boolean; override;                 // Compare base bmethod
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    function    Find(Value: TcObject): TcObject; override;
    procedure   SetXML(Value: String); override;
    procedure   Update(dx, dy: double);
    function    HasEdge(parCode: longint): boolean;

  public
    // Public declarations
    //
    property    iIndex: longint                         read m_iIndex           write m_iIndex;
    property    X: double                               read m_dX               write m_dX;
    property    Y: double                               read m_dY               write m_dY;
    property    AsPoint: TdPoint                        read GetPoint           write SetPoint;
    property    CenterX: longint                        read GetCenterX;
    property    CenterY: longint                        read GetCenterY;
    property    Fixed: boolean                          read m_bFixed           write m_bFixed;
    property    Panel: TObjectPanel                     read GetPanel           write SetPanel;
    property    Icon: longint                           read m_iIcon            write SetIcon;
    property    AlertLevel: longint                     read m_iAlertLevel      write SetAlertLevel;
    property    Hint: String                            read GetHint;
    property    Caption: String                         read GetCaption;
  end;

  TcEdge = class(TcCustomNode)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TcEdge describes a node to node edge relationship.
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 12/17/00 Regis Created
  * 07/03/01 Regis Shared with IMO
  * 03/20/04 Regis Kickoff version 2
  * 03/28/04 Regis Completed version 2
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_iEdgeType: longint;
    m_objPaintBox: TPaintBox;
    m_eColorDrawn: TeColor;

  private
    // Private declarations
    //
    function    GetToNode: TcNode;

  published
    // Published declarations
    //
    procedure   SetColor(value: TeColor); override;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(parParent: TcObject); override;                          // Standard Constructor, virtual
    destructor  Destroy; override;
    //   Destroy
    //   Clear
    //   Copy
    function    Compare(parValue: TcObject): boolean; override;                 // Compare base bmethod
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom

  public
    // Public declarations
    //
    property    iEdgeType: longint                      read m_iEdgeType        write m_iEdgeType;
    property    ToNode: TcNode                          read GetToNode;
    property    PaintBox: TPaintBox                     read m_objPaintBox      write m_objPaintBox;
    property    eColor;
    property    eColorDrawn: TeColor                    read m_eColorDrawn      write m_eColorDrawn;
  end;

  TcNodeSaxContentHandler = class(TSAXContentHandler)
 {******************************************************************************
  * Author: Regis Charlot
  *         (c) Copyrights 2001-2004 Seabird Software, LLC
  *         REUSED WITH PERMISSION FOR INTELLIGENT MEDICAL OBJECTS, INC
  *
  * Description: TcEdge describes a node to node edge relationship.
  *
  * Inheritance: TcObject
  *
  * Revision History:
  * Date     Who   Reason
  * -------- ----- ------------------------------------------------------------
  * 03/20/04 Regis Created
  * 03/28/04 Regis Completed version 2
  *
  ******************************************************************************}
  private
    // Private declarations
    //
    m_lstPath: TcBag;
    m_bIsValue: boolean;

  public
    // Public declarations
    //
    // 1. Standard
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    //   Clear
    //   Copy
    //   Compare
    //   Text
    //   Load
    //   Save
    //   LoadFromStream
    //   SaveToStream
    //
    // 2. Custom
    procedure   StartElement(const NamespaceURI, LocalName, QName: SAXString; const Atts: IAttributes); override;
    procedure   Characters(const PCh: SAXString); override;
    procedure   EndElement(const NamespaceURI, LocalName, QName: SAXString); override;
    procedure   Push(parObject: TcObject);
    procedure   Pop;
    function    Top: TcNode;

  public
    // Public declarations
    //
    property    bIsValue: boolean                       read m_bIsValue         write m_bIsValue;
  end;

const
  krsXMLCODE    = 'code';
  krsXMLINDEX   = 'index';
  krsXMLNAME    = 'name';
  krsXMLMETA    = 'meta';
  krsXMLVALUE   = 'value';
  krsXMLNODE    = 'node';
  krsXMLLINK    = 'link';
  krsXMLTYPE    = 'type';
  krsXMLICON    = 'icon';
  krsXMLALERT   = 'alert';
  krsPANELMOVED = 'panelmoved';

//
// Helpers
//
function dPoint(X, Y: double): TdPoint;
begin
  result.X := X;
  result.Y := Y;
end;

// TGraphPanel
//   Register
//
procedure Register;
begin
  RegisterComponents('Seabird', [TGraphPanel]);
  RegisterComponents('Seabird', [TObjectPanel]);
end;  {Register}

//
// TGraphPanel
//

// TGraphPanel
//   Create
//
constructor TGraphPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := GetWinVersion >= wvWinXP;
  m_objGraph := TcGraph.Create(nil);
  (m_objGraph as TcGraph).ParentControl := self;
  // onDragOver := (m_objGraph as TcGraph).onDragOver;
  Color := clCream;
  m_imgIcons := nil;
  m_imgAlerts := nil;
  m_OnChange := nil;
  m_OnSelect := nil;
  m_OnTextSelect := nil;
  m_popPanel := nil;
  m_OnChangeLocation := nil;
end;

// TGraphPanel
//   Create
//
destructor TGraphPanel.Destroy;
begin
  m_objGraph.free;
  inherited Destroy;
end;

// TGraphPanel
//   SetXML
//
procedure TGraphPanel.SetXML(Value: String);
begin
  m_objGraph.XML := Value;
end;

// TGraphPanel
//   GetXML
//
function TGraphPanel.GetXML: String;
begin
  result := m_objGraph.XML;
end;

// TGraphPanel
//   GetXML
//
function TGraphPanel.GetMultiSelect: boolean;
begin
  result := (m_objGraph as TcGraph).MultiSelect;
end;

// TGraphPanel
//   SetMultiSelect
//
procedure TGraphPanel.SetMultiSelect(Value: boolean);
begin
  (m_objGraph as TcGraph).MultiSelect := Value;
end;

// TGraphPanel
//   SetImageIcons
//
procedure TGraphPanel.SetImageIcons(value: TImageList);
begin
  m_imgIcons := value;
  (m_objGraph as TcGraph).ImageWidth := value.width;
end;

// TGraphPanel
//   GetColors
//
function TGraphPanel.GetColors(Index: TeColor): TColor;
begin
  result := (m_objGraph as TcGraph).Colors[Index];
end;

// TGraphPanel
//   SetColors
//
procedure TGraphPanel.SetColors(Index: TeColor; value: TColor);
begin
  (m_objGraph as TcGraph).Colors[Index] := value;
end;

// TGraphPanel
//   SetSelected (2)
//
procedure TGraphPanel.SetSelected(value: longint);
var
  p: TcObject;
begin
  // Set Selection
  (m_objGraph as TcGraph).SetSelected(value);
  // Scroll panel, if necessary
  p := (m_objGraph as TcGraph).GetNodeByID(value);
  if (p <> nil) and (p is TcNode) and ((p as TcNode).m_objPanel <> nil) then
    ScrollInView((p as TcNode).m_objPanel);
end;

// TGraphPanel
//   GetSelected
//
function TGraphPanel.GetSelected: longint;
begin
  result := (m_objGraph as TcGraph).GetSelected;
end;

// TGraphPanel
//   GetVertScroll
//
function TGraphPanel.GetVertScroll: longint;
begin
  result := VertScrollBar.ScrollPos;
end;

// TGraphPanel
//   GetHorizScroll
//
function TGraphPanel.GetHorizScroll: longint;
begin
  result := HorzScrollBar.ScrollPos;
end;

// TGraphPanel
//   GetIsEmpty
//
function TGraphPanel.GetIsEmpty: boolean;
begin
  result := (m_objGraph as TcGraph).IsEmpty;
end;

// TGraphPanel
//   GetProperties
//
function TGraphPanel.GetProperties(Tag: String; ID: longint): String;
begin
  if AnsiCompareText(Tag, krsPANELMOVED) = 0 then
    result := kasBOOL[(m_objGraph as TcGraph).PanelMoved]
  else
    result := (m_objGraph as TcGraph).Properties[Tag, ID];
end;

// TGraphPanel
//   SetProperties
//
procedure TGraphPanel.SetProperties(Tag: String; ID: longint; value: String);
begin
  (m_objGraph as TcGraph).Properties[Tag, ID] := value;
end;

//
// TgGraph
//

// TcGraph
//   Create
//
constructor TcGraph.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_lstNodes := TcBag.Create(self);
  m_lstQueue := TcBag.Create(self);
  m_lstEdges := TcBag.Create(self);
  m_ParentControl := nil;
  m_objNode := TcNode.Create(self);
  m_bMultiSelect := FALSE;
  m_aColors := kacCOLORS;
  m_iImageWidth := 0;
  m_objPanel := nil;
  m_bPanelMoved := FALSE;
  min_x := kiUNDEFINED;
  max_x := kiUNDEFINED;
  min_y := kiUNDEFINED;
  max_y := kiUNDEFINED;
  m_iSelected := kiUNDEFINED;
end;

// TcGraph
//   Destroy
//
destructor TcGraph.Destroy;
begin
  m_lstNodes.free;
  m_lstQueue.free;
  m_lstEdges.free;
  inherited destroy;
end;

// TcGraph
//   SetXML
//
procedure TcGraph.SetXML(Value: String);

  procedure Completedges(parNode: TcNode);
  var
    i: longint;
    ne: TcEdge;
  begin
    for i := 0 to parNode.Count - 1 do
      if (parNode[i] <> nil) and (parNode[i] is TcNode) then
      begin
        if (parNode <> m_objNode) and not parNode.HasEdge(parNode[i].iCode) then
        begin
          ne := TcEdge.Create(parNode);
          ne.iCode := parNode[i].iCode;
          ne.iEdgeType := 1;
          parNode.Add(ne);
        end;
        Completedges(parNode[i] as TcNode);
      end;
  end;

var
  p: TcNode;
begin
  p := nil;
  ResetColors(nil);
  try
    p := TcNode.Create(nil);
    p.XML := Value;
    p.ResolveDifferences(m_objNode as TcNode, OnDifferenceCallBack);
  finally
    p.free;
  end;
  Completedges(m_objNode);
  Initialize;
  Run;
  ReColor(nil);
  m_iSelected := kiUNDEFINED;
end;

// TcGraph
//   OnDifferenceCallBack
//
function TcGraph.OnDifferenceCallBack(parNode1, parNode2: TcObject; edAction: TedAction): TcObject;
begin
  result := nil;
  if parNode2 <> nil then
    case edAction of
      //
      // Add
      //
      edAdd:
      begin
        if parNode2 is TcEdge then
        begin
          result := TcEdge.Create(parNode1);
          (result as TcEdge).iEdgeType := (parNode2 as TcEdge).iEdgeType;
        end
        else
        begin
          result := TcNode.Create(parNode1);
          (result as TcNode).iIndex := (parNode2 as TcNode).iIndex;
          (result as TcNode).Icon := (parNode2 as TcNode).Icon;
          (result as TcNode).AlertLevel := (parNode2 as TcNode).AlertLevel;
          (result as TcNode).X := (parNode2 as TcNode).X;
          (result as TcNode).Y := (parNode2 as TcNode).Y;
          (result as TcNode).Fixed := ((result as TcNode).X <> 0) or ((result as TcNode).Y <> 0);
        end;
        result.iCode := parNode2.iCode;
        result.sName := parNode2.sName;
        result.sValue := parNode2.sValue;
        parNode1.Add(result);
      end;
      //
      // Delete
      //
      edDelete:
        if parNode2.Parent <> nil then
          parNode2.Parent.Delete(parNode2);
      //
      // UPdate
      //
      edUpdate:
        if (parNode1 <> nil) and (parNode2 <> nil) then
        begin
          if parNode1 is TcEdge then
            (parNode1 as TcEdge).iEdgeType := (parNode2 as TcEdge).iEdgeType
          else
          begin
            (parNode1 as TcNode).iIndex := (parNode2 as TcNode).iIndex;
            (parNode1 as TcNode).Icon := (parNode2 as TcNode).Icon;
            (parNode1 as TcNode).AlertLevel := (parNode2 as TcNode).AlertLevel;
            (parNode1 as TcNode).X := (parNode2 as TcNode).X;
            (parNode1 as TcNode).Y := (parNode2 as TcNode).Y;
            (parNode1 as TcNode).Fixed := ((parNode1 as TcNode).X <> 0) or ((parNode1 as TcNode).Y <> 0);
          end;
          parNode1.iCode := parNode2.iCode;
          parNode1.sName := parNode2.sName;
          parNode1.sValue := parNode2.sValue;
          // Repaint!
          RefreshProperties(parNode1 as TcNode, TRUE);
        end;
    end;
end;

// TcGraph
//   Initialize
//
function TcGraph.Initialize: boolean;
var
  i: longint;
  p: TcNode;
begin
  m_eDistribution := edRectangular;     // Distribution 0
  max_epochs := 1000;                   // MaxEpochs 1000
  initial_adaption := 0.9;
  cooling_factor := 2;                  // CoolingFactor 2.0
  radius := 5;                          // MaxRadius 5
  min_radius := 1;                      // MinRadius 1
  radius_constant_time := 100;
  adaption := initial_adaption;         // MaxAdaption 0.9
  min_adaption := 0.0;                  // MinAdaption 0.0
  temp := 0.03;                         // < 1
  initial_jump_radius := 100;           // JumpRadius 100
  jump_radius := initial_jump_radius;   //
  tabooDist := ksTABOODISTANCE;         // TabooSize 30
  m_iLastX := 0;
  m_iLastY := 0;
  min_x := 10;
  min_y := 10;
  m_objWinner := nil;
  bRandom := FALSE;                     // Randomize FALSE
  bClipping := FALSE;                   // Clipping FALSE
  bTaboo := TRUE;                       // Taboo FALSE
  // Drawing Size
  max_x := GetSize.x - 20;
  max_y := GetSize.y - 20;
  // Initial location of nodes
  for i := 0 to m_lstNodes.count - 1 do
  begin
    p := m_lstNodes[i] as TcNode;
    if not p.Fixed then
    begin
      p.m_bVisited := FALSE;
      p.AsPoint := RandomVector;
    end;
  end;
  result := TRUE;
end;

// TcGraph
//   ClosestNode
//
function TcGraph.ClosestNode(x, y: double): TcNode;
var
  dx, dy, x0, y0, dist: double;
  d: double;
  i: longint;
begin
  d := kfLARGEDOUBLE;
  result := nil;
  for i := 0 to m_lstNodes.count - 1 do
  begin
    x0 := (m_lstNodes[i] as TcNode).x;
    y0 := (m_lstNodes[i] as TcNode).y;
    if InsideDistribution(x0, y0) then
    begin
      dx := x0 - x;
      dy := y0 - y;
      dist := sqrt(dx * dx + dy * dy);
      if d > dist then
      begin
        d := dist;
        result := m_lstNodes[i] as TcNode;
      end;
    end;
  end;
end;

// TcGraph
//   RandomVector
//
function TcGraph.RandomVector: TdPoint;
begin
  m_iLastX := min_x + (max_x - min_x) * random;
  m_iLastY := min_y + (max_y - min_y) * random;
  result.X := m_iLastX;
  result.Y := m_iLastY;
end;

// TcGraph
//   Run
//
procedure TcGraph.Run;
var
  n: TcNode;
  i: longint;
  b: boolean;
  p: TcNode;
begin
  b := FALSE;
  for i := 0 to m_lstNodes.count - 1 do
  begin
    p := m_lstNodes[i] as TcNode;
    b := b or not p.Fixed;
  end;
  epoch := 1;
  if b then
  begin
    while TRUE do
    begin
      if max_epochs > epoch then
      begin
        adjust;
        update_parameters;
      end
      else
        break;

      if (bRandom and (random < temp)) then
      begin
        n := m_lstNodes[trunc(random * (m_lstNodes.count - 1))] as TcNode;
        if not n.Fixed then
        begin
          n.x := n.x + jump_radius * random - jump_radius / 2;
          n.y := n.y + jump_radius * random - jump_radius / 2;
        end;
      end;
    end;
    resize;
  end;
  RePaint(nil);
  m_bPanelMoved := FALSE;
end;

// TcGraph
//   Resize
//
procedure TcGraph.Resize;
var
  minX, minY: double;
  i: longint;
  p: TcNode;
begin
  minX := kfLARGEDOUBLE;
  minY := kfLARGEDOUBLE;
  // Getting MINs
  for i := 0 to m_lstNodes.count - 1 do
  begin
    p := m_lstNodes[i] as TcNode;
    if minX > p.x then
      minX := p.x;
    if minY > p.y then
      minY := p.y;
  end;
  // Adjusting nodes
  for i := 0 to m_lstNodes.count - 1 do
  begin
    p := m_lstNodes[i] as TcNode;
    p.x := p.x - minX + (kiPANELHEIGHT div 2);
    p.y := p.y - minY + (kiPANELHEIGHT div 2);
  end;
end;

// TcGraph
//   update_parameters
//
procedure TcGraph.update_parameters;
begin
  epoch := epoch + 1;
  // linear cooling:
  //      factor=(1-(double)epoch/max_epochs);
  // negative exponential cooling:
  factor := exp( -1 * cooling_factor * epoch / max_epochs);
  adaption := max(min_adaption, factor * initial_adaption);
  jump_radius := round(factor * jump_radius);
  temp := factor * temp;

  // apparently it is not a good idea to update just a single node
  // in isolation (radius=0)
  if ((radius > min_radius) and (epoch mod radius_constant_time = 0)) then
    radius := radius - 1;
end;

// TcGraph
//   adjust
//
procedure TcGraph.adjust;
var
  i: longint;
begin
  random_input;
  m_objWinner := closestNode(m_iLastX, m_iLastY);
  for i := 0 to m_lstNodes.count - 1 do
  begin
    (m_lstNodes[i] as TcNode).m_bVisited := FALSE;
    (m_lstNodes[i] as TcNode).m_iDistance := 0;
  end;
  adjustNodes(m_objWinner);
end;

// TcGraph
//   inside_triangle
//
function TcGraph.inside_triangle(x, y: double): boolean;
var
  range: double;
begin
  range := (y - min_y) / (max_y - min_y) * (max_x - min_x) / 2;
  result := (x >= (min_x + (max_x - min_x) / 2 - range)) and (x <= (min_x + (max_x - min_x) / 2 + range));
end;

// TcGraph
//   inside_circle
//
function TcGraph.inside_circle(x, y: double): boolean;
var
  centerX, centerY, radius, dist: double;
begin
  centerX := min_x + (max_x - min_x) / 2;
  centerY := min_y + (max_y - min_y) / 2;
  radius := min(max_x - min_x, max_y - min_y) / 2;
  dist := sqrt(power((centerX - x), 2.0) + power((centerY - y), 2.0));
  result := dist <= radius;
end;

// TcGraph
//   random_input
//
procedure TcGraph.random_input;
begin
  // here is an inefficient way to ensure equal
  // distribution over selected area
  RandomVector;
  while not InsideDistribution(m_iLastX, m_iLastY) do
    RandomVector;
end;

// TcGraph
//   InsideDistribution
//
function TcGraph.InsideDistribution(x, y: double): boolean;
begin
  case m_eDistribution of
    edRectangular:
      result := (x >= min_x) and (x <= max_x) and (y >= min_y) and (y<=max_y);
    edTriangular:
      result := inside_triangle(x,y);
    edCircular:
      result := inside_circle(x,y);
    else
    begin
      m_iLastX := 0;
      m_iLastY := 0;
      result := TRUE;
    end;
  end;
end;

// TcGraph
//   adjustNodes
//
procedure TcGraph.adjustNodes(n: TcNode);
var
  p: TcNode;
  dx, dy, factor: double;
  child: TcNode;
  i: longint;
begin
  // we have to do a breadth first traversal here
  m_lstQueue.Clear;
  n.m_iDistance := 0;
  n.m_bVisited := TRUE;
  m_lstQueue.Add(n);
  while not m_lstQueue.IsEmpty do
  begin
    p := m_lstQueue[0] as TcNode;
    m_lstQueue.delete(0);
    dx := m_iLastX - p.x;
    dy := m_iLastY - p.y;
    factor := adaption / power(2.0, p.m_iDistance);
    if not p.Fixed then
      p.Update(factor * dx, factor * dy);
    if p.m_iDistance < radius then
      for i := 0 to p.count - 1 do
      begin
        child := nil;
        if (p[i] <> nil) and (p[i] is TcNode) then
          child := p[i] as TcNode
        else if (p[i] <> nil) and (p[i] is TcEdge) then
          child := (p[i] as TcEdge).ToNode;
        if (child <> nil) and not child.m_bVisited and (not bClipping or InsideDistribution(child.x, child.y)) then
        begin
          child.m_bVisited := TRUE;
          child.m_iDistance := p.m_iDistance + 1;
          m_lstQueue.Add(child);
        end;
      end;
  end;
  if bTaboo then
    tabooCheck;
end;

// TcGraph
//   tabooCheck
//
procedure TcGraph.tabooCheck;
var
  i, j: longint;
  checknode, otherNode: TcNode;
  dx, dy, dist, x_translat, y_translat, translation, clip_factor: double;
begin
  for i := 0 to m_lstNodes.count - 1 do
  begin
    checkNode := m_lstNodes[i] as TcNode;
    for j := i + 1 to m_lstNodes.count - 1 do
    begin
      otherNode := m_lstNodes[j] as TcNode;
      dx := checkNode.x - otherNode.x;
      dy := checkNode.y - otherNode.y;
      dist := sqrt(power(dx, 2.0) + power(dy, 2.0));
      if dist < tabooDist then
      begin
        if checkNode.m_bVisited then
        begin
          x_translat := checkNode.x - checkNode.old_x;
          y_translat := checkNode.y - checkNode.old_y;
          translation := sqrt(power(x_translat, 2.0) + power(y_translat, 2.0));
          clip_factor := 0;
          if translation <> 0 then
            clip_factor := 1 - (double(tabooDist) - dist) / translation;
          checkNode.x := checkNode.old_x + clip_factor * x_translat;
          checkNode.y := checkNode.old_y + clip_factor * y_translat;
        end
        else if otherNode.m_bVisited then
        begin
          x_translat := otherNode.x - otherNode.old_x;
          y_translat := otherNode.y - otherNode.old_y;
          translation := sqrt(power(x_translat, 2.0) + power(y_translat, 2.0));
          clip_factor := 0;
          if translation <> 0 then
            clip_factor := 1 - (double(tabooDist) - dist) / translation;
          otherNode.x := otherNode.old_x + clip_factor * x_translat;
          otherNode.y := otherNode.old_y + clip_factor * y_translat;
        end;
      end;
    end;
  end;
end;

// TcGraph
//   GetSize
//
function TcGraph.GetSize: TPoint;
var
  L: longint;
begin
  L := round(sqrt(sqr(max(kiPANELWIDTH, kiPANELHEIGHT)) * 4 * m_lstNodes.count));
  result := Point(L, L);
end;

// TcGraph
//   RePaint
//
procedure TcGraph.RePaint(parObject: TcObject);

  procedure RepaintObject(parObject: TcObject);
  var
    pNode: TcNode;
    pTo, pFrom: TcNode;
    pEdge: TcEdge;
    iscrTop, iscrLeft: longint;
  begin
    iscrTop := m_ParentControl.VertScroll;
    iscrLeft := m_ParentControl.HorizScroll;
    //
    // Node
    if (parObject <> nil) and (parObject is TcNode) then
    begin
      pNode := parObject as TcNode;
      with pNode do
        if Panel <> nil then
        begin
          Panel.SetBounds(round(x) - iscrLeft, round(y) - iscrTop, Panel.width, Panel.height);
          Panel.Visible := TRUE;
          Panel.Color := m_aColors[eColor];
          SetLocation(pNode.Tag, dPoint(X, Y));
        end;
    end
    //
    // Edge
    else if (parObject <> nil) and (parObject is TcEdge) then
    begin
      pEdge := parObject as TcEdge;
      pFrom := pEdge.Parent as TcNode;
      pTo := pEdge.ToNode;
      if (pFrom <> nil) and (pTo <> nil) then
      begin
        pEdge.PaintBox.left := min(pFrom.CenterX, pTo.CenterX) - iscrLeft;
        pEdge.PaintBox.top  := min(pFrom.CenterY, pTo.CenterY) - iscrTop;
        pEdge.PaintBox.width := abs(pFrom.CenterX - pTo.CenterX) + 1;
        pEdge.PaintBox.height := abs(pFrom.CenterY - pTo.CenterY) + 1;
        pEdge.PaintBox.Tag := longint(pointer(pEdge));
        pEdge.PaintBox.Visible := TRUE;
      end;
    end;
  end;

var
  i: longint;
begin
  // Node
  if parObject = nil then
  begin
    for i := 0 to m_lstNodes.count - 1 do
      RepaintObject(m_lstNodes[i]);
    for i := 0 to m_lstEdges.count - 1 do
      RepaintObject(m_lstEdges[i]);
  end
  else
    RepaintObject(parObject);
end;

// TcGraph
//   PaintEdge
//
procedure TcGraph.PaintEdge(Sender: TObject);
var
  e: TcEdge;
  x, y, w, h: double;
  p: TPaintBox;
  pTo, pFrom: TcNode;
begin
  try
    p := Sender as TPaintBox;
    e := TcEdge(pointer(p.Tag));
    w := p.Width - 1;
    h := p.Height - 1;
    pFrom := e.Parent as TcNode;
    pTo := nil;
    if e <> nil then
      pTo := e.ToNode;
    if (pFrom <> nil) and (pTo <> nil) then
    begin
      if pFrom.CenterX < pTo.CenterX then
        x := 0
      else
        x := w;
      if pFrom.CenterY < pTo.CenterY then
        y := 0
      else
        y := h;
      p.Canvas.Pen.Color := m_aColors[e.eColor];
      p.Canvas.Pen.Mode := pmCopy;
      p.Canvas.MoveTo(round(x), round(y));
      if x = 0 then
        x := w
      else
        x := 0;
      if y = 0 then
        y := h
      else
        y := 0;
      p.Canvas.LineTo(round(x), round(y));
      e.eColorDrawn := e.eColor;
    end;
  except
    //
  end;
end;

// TcGraph
//   GetIsEmpty
//
function TcGraph.GetIsEmpty: boolean;
begin
  result := m_objNode.IsEmpty;
end;

// TcGraph
//   Close
//
procedure TcGraph.Close;
begin
  m_objNode.Clear;
end;

// TcGraph
//   AddObject
//
function TcGraph.AddObject(parObject: TcObject): longint;
var
  pnl: TObjectPanel;
  pb: TPaintBox;
begin
  result := kiUNDEFINED;
  //
  // Node
  //
  if (parObject <> nil) and (parObject is TcNode) then
  begin
    if (m_lstNodes.IndexOf(parObject) = kiUNDEFINED) then
      m_lstNodes.Add(parObject);
    if (parObject as TcNode).Panel = nil then
    begin
      (parObject as TcNode).Panel := TObjectPanel.Create(m_ParentControl);
      (parObject as TcNode).Panel.Parent := m_ParentControl;
    end;
    pnl := (parObject as TcNode).Panel;
    pnl.sMetaName := (parObject as TcNode).sName;
    pnl.sDataName := (parObject as TcNode).sValue;
    pnl.DragMode := dmManual;
    pnl.Visible := FALSE;
    pnl.Parent := m_ParentControl;
    pnl.Tag := longint(parObject);
    pnl.onMouseDown := onMouseDown;
    pnl.onMouseUp := onMouseUp;
    pnl.onMouseMove := onMouseMove;
    pnl.ShowHint := TRUE;
    pnl.PopupMenu := m_ParentControl.PopupMenuPanel;
    result := m_lstNodes.IndexOf(parObject);
  end
  //
  // Edge
  //
  else if (parObject <> nil) and (parObject is TcEdge) then
  begin
    if m_lstEdges.IndexOf(parObject) = kiUNDEFINED then
      m_lstEdges.Add(parObject);
    if (parObject as TcEdge).PaintBox = nil then
    begin
      (parObject as TcEdge).PaintBox := TPaintBox.Create(m_ParentControl);
      (parObject as TcEdge).PaintBox.Parent := m_ParentControl;
    end;
    pb := (parObject as TcEdge).PaintBox;
    pb.Visible := FALSE;
    pb.Parent := m_ParentControl;
    pb.onPaint := PaintEdge;
    pb.DragMode := dmManual;
    pb.Tag := longint(pointer(parObject));
    result := m_lstEdges.IndexOf(parObject);
  end;
  RefreshProperties(parObject, FALSE);
end;

// TcGraph
//   RefreshProperties
//
procedure TcGraph.RefreshProperties(parObject: TcObject; bRepaint: boolean);
var
  pnl: TObjectPanel;
begin
  //
  // Node
  //
  if (parObject <> nil) and (parObject is TcNode) and ((parObject as TcNode).Panel <> nil) then
  begin
    pnl := (parObject as TcNode).Panel;
    pnl.Text := (parObject as TcNode).Caption;
    if m_ParentControl <> nil then
    begin
      pnl.Font.Assign(m_ParentControl.Font);
      pnl.imgIcons := m_ParentControl.ImageIcons;
      pnl.imgAlerts := m_ParentControl.ImageAlerts;
      pnl.PopupMenu := m_ParentControl.PopupMenuPanel;
    end;
    if (parObject as TcNode).Icon = kiUNDEFINED then
    begin
      pnl.width := kiTEXTSPACE + pnl.Canvas.TextWidth(pnl.Text) + kiTEXTSPACE;
      pnl.Alignment := taCenter;
    end
    else
    begin
      pnl.width := m_iImageWidth + kiTEXTSPACE + pnl.Canvas.TextWidth(pnl.Text) + kiTEXTSPACE;
      pnl.Alignment := taRightJustify;
    end;
    pnl.height := max(pnl.Canvas.TextHeight('W') + 3, kiPANELHEIGHT);
    pnl.Hint := (parObject as TcNode).Hint;
    pnl.Color := m_aColors[(parObject as TcNode).eColor];
    pnl.Icon := (parObject as TcNode).Icon;
    pnl.AlertLevel := (parObject as TcNode).AlertLevel;
  end;
  // Repaint
  if bRepaint then
    RePaint(parObject);
end;

// TcGraph
//   DeleteObject
//
procedure TcGraph.DeleteObject(parObject: TcObject);
var
  pnl: TObjectPanel;
  pb: TPaintBox;
begin
  //
  // Node
  if (parObject <> nil) and (parObject is TcNode) then
  begin
    pnl := (parObject as TcNode).Panel;
    if pnl <> nil then
    begin
      m_ParentControl.RemoveComponent(pnl);
      pnl.Free;
    end;
    (parObject as TcNode).Panel := nil;
  end
  //
  // Edge
  else if (parObject <> nil) and (parObject is TcEdge) then
  begin
    pb := (parObject as TcEdge).PaintBox;
    if pb <> nil then
    begin
      m_ParentControl.RemoveComponent(pb);
      pb.Free;
    end;
    (parObject as TcEdge).PaintBox := nil;
  end;
end;

// TcGraph
//   GetMultiSelect
//
function TcGraph.GetMultiSelect: boolean;
begin
  result := m_bMultiSelect;
end;

// TcGraph
//   SetMultiSelect
//
procedure TcGraph.SetMultiSelect(Value: boolean);
begin
  m_bMultiSelect := value;
end;

// TcGraph
//   onMouseDown
//
procedure TcGraph.onMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TObjectPanel) and (Button <> mbRight) then
  begin
    m_objPanel := Sender as TObjectPanel;
    m_idragdropX := X;
    m_idragdropY := Y;
    SelectPanel(Sender, TRUE, FALSE);
    OnChange(Sender);
  end;
end;

// TcGraph
//   onMouseUp
//
procedure TcGraph.onMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if m_objPanel <> nil then
    SetLocation(m_objPanel.Index, dPoint(m_objPanel.Left, m_objPanel.Top));
  m_objPanel := nil;
end;

// TcGraph
//   OnMouseMove
//
procedure TcGraph.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: TcNode;
  i: longint;
begin
  if (m_objPanel <> nil) and (Sender = m_objPanel) and (m_objPanel.Tag <> 0) then
  try
    // a. Repaint Node
    p := TcNode(m_objPanel.Tag);
    if (p <> nil) and (p is TcNode) and ((x - m_idragdropX <> 0) or (y - m_idragdropY <> 0)) then
    begin
      p.x := p.x + x - m_idragdropX;
      p.y := p.y + y - m_idragdropY;
      m_bPanelMoved := TRUE;
      Repaint(p);
      // b. Repaint *To Edges
      if (p <> nil) and (p is TcNode) then
        for i := 0 to p.count - 1 do
          if (p[i] <> nil) and (p[i] is TcEdge) then
            RefreshProperties(p[i], TRUE);
      // c. Repaint *From Edges
      for i := 0 to m_lstEdges.count - 1 do
        if (m_lstEdges[i] <> nil) and (m_lstEdges[i] is TcEdge) and ((m_lstEdges[i] as TcEdge).ToNode = p) and (m_lstEdges[i].Parent <> nil) and (m_lstEdges[i].Parent is TcNode) then
          RefreshProperties(m_lstEdges[i], TRUE);
    end;
  except
    //
  end;
end;

// TcGraph
//   OnChange
//
procedure TcGraph.OnChange(Sender: TObject);
var
  p: TcObject;
begin
  // OnChange event
  if (m_ParentControl <> nil) and Assigned(m_ParentControl.m_OnChange) then
    m_ParentControl.m_OnChange(sender);
  // OnSelect Event
  //   Similar to OnChange event, except that the parameter is a node number
  if (m_ParentControl <> nil) and Assigned(m_ParentControl.m_OnSelect) and (Sender <> nil) and (Sender is TObjectPanel) and ((Sender as TObjectPanel).Tag <> 0) then
  begin
    p := TcObject((Sender as TObjectPanel).Tag);
    m_ParentControl.m_OnSelect(p.sName, p.sValue);
  end;
  // OnTextSelect Event
  //   Similar to OnSelect event, except that the parameter is a string
  if (m_ParentControl <> nil) and Assigned(m_ParentControl.OnTextSelect) and (Sender <> nil) and (Sender is TObjectPanel) and ((Sender as TObjectPanel).Text <> ksEMPTY) then
    m_ParentControl.OnTextSelect((Sender as TObjectPanel).Text);
end;

// TcGraph
//   SelectPanel
//
function TcGraph.SelectPanel(Sender: TObject; bToggle, bInternal: boolean): boolean;
var
  i: longint;
  q: TcObject;
  pNode: TcNode;
  b: boolean;
begin
  result := FALSE;
  //
  // 0. Select & Deselect feature
  b := TRUE;
  pNode := nil;
  if (Sender <> nil) and (Sender is TObjectPanel) then
    pNode := pointer((Sender as TObjectPanel).Tag)
  else if (Sender <> nil) and (Sender is TcNode) then
    pNode := Sender as TcNode;
  if (pNode <> nil) and (pNode is TcNode) and bToggle then
    b := (pNode.eColor <> ecSelectedNode);
  //
  // 1. Reset Colors
  ResetColors(Sender);
  //
  // 2. Make Node Selection
  if (b or m_bMultiSelect) and (pNode <> nil) and (pNode is TcNode) then
  begin
    // 2.a. Set *this node
    if m_bMultiSelect then
    begin
      if b then
        (pNode as TcNode).eColor := ecSelectedNode
      else
        (pNode as TcNode).eColor := ecNode;
    end
    else
    begin
      (pNode as TcNode).eColor := ecSelectedNode;
      // Set all To-Dependency selections
      for i := 0 to pNode.count - 1 do
        if (pNode[i] <> nil) and (pNode[i] is TcEdge) then
        begin
          (pNode[i] as TcEdge).eColor := ecSelectedEdge;
          q := (pNode[i] as TcEdge).ToNode;
          if (q <> nil) and (q is TcNode) then
            (q as TcNode).eColor := ecToNodeDependency;
        end;
      // Set all From-Dependency selections
      for i := 0 to m_lstEdges.count - 1 do
        if (m_lstEdges[i] <> nil) and (m_lstEdges[i] is TcEdge) and ((m_lstEdges[i] as TcEdge).ToNode = pNode) and (m_lstEdges[i].Parent <> nil) and (m_lstEdges[i].Parent is TcNode) then
        begin
          (m_lstEdges[i].Parent as TcNode).eColor := ecFromNodeDependency;
          (m_lstEdges[i] as TcEdge).eColor := ecSelectedEdge;
        end;
    end;
    result := TRUE;
  end;
  //
  // 3. Refresh All Nodes
  ReColor(Sender);
  //
  // 4. Send an OnClick event to TGraphPanel.
  if bInternal and (m_ParentControl <> nil) and Assigned(m_ParentControl.OnClick) then
    m_ParentControl.OnClick(Sender);
end;

// TcGraph
//   ResetColors
//
procedure TcGraph.ResetColors(Sender: TObject);
var
  i: longint;
begin
  for i := 0 to m_lstNodes.count - 1 do
    if (m_lstNodes[i] <> nil) and (m_lstNodes[i] is TcNode) then
      (m_lstNodes[i] as TcNode).eColor := ecNode;
  for i := 0 to m_lstEdges.count - 1 do
    if (m_lstEdges[i] <> nil) and (m_lstEdges[i] is TcEdge) then
      (m_lstEdges[i] as TcEdge).eColor := ecEdge;
end;

// TcGraph
//   ReColor
//
procedure TcGraph.ReColor(Sender: TObject);
var
  i: longint;
begin
  for i := 0 to m_lstNodes.count - 1 do
    if (m_lstNodes[i] <> nil) and
       (m_lstNodes[i] is TcNode) and
       ((m_lstNodes[i] as TcNode).Panel <> nil) and
       (m_aColors[(m_lstNodes[i] as TcNode).eColor] <> (m_lstNodes[i] as TcNode).Panel.Color) then
      RefreshProperties(m_lstNodes[i] as TcNode, TRUE);
  for i := 0 to m_lstEdges.count - 1 do
    if (m_lstEdges[i] <> nil) and
       (m_lstEdges[i] is TcEdge) and
       ((m_lstEdges[i] as TcEdge).eColor <> (m_lstEdges[i] as TcEdge).eColorDrawn) then
      RefreshProperties(m_lstEdges[i] as TcEdge, TRUE);
end;

// TcGraph
//   GetColors
//
function TcGraph.GetColors(Index: TeColor): TColor;
begin
  result := m_aColors[Index];
end;

// TcGraph
//   SetColors
//
procedure TcGraph.SetColors(Index: TeColor; value: TColor);
begin
  m_aColors[Index] := value;
end;

// TcGraph
//   SetSelected (2)
//
procedure TcGraph.SetSelected(value: longint);
var
  p: TcObject;
begin
  if m_iSelected <> value then
  begin
    p := GetNodeByID(value);
    if (p <> nil) and (p.Tag = value) then
    begin
      m_iSelected := kiUNDEFINED;
      if (p <> nil) and SelectPanel(p, FALSE, TRUE) then
        m_iSelected := p.Tag;
    end
  end
end;

// TcGraph
//   GetSelected
//
function TcGraph.GetSelected: longint;
begin
  result := m_iSelected;
end;

// TcGraph
//   GetNodeByID
//
function TcGraph.GetNodeByID(value: longint): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to m_lstNodes.Count - 1 do
    if (m_lstNodes[i] <> nil) and (m_lstNodes[i].Tag = value) then
    begin
      result := m_lstNodes[i];
      break;
    end;
end;

// TcGraph
//   GetProperties
//
function TcGraph.GetProperties(Tag: String; ID: longint): String;
var
  p: TcObject;
begin
  result := ksEMPTY;
  p := GetNodeByID(ID);
  if (p <> nil) and (p is TcNode) then
  begin
    // X
    if AnsiCompareText(Tag, 'X') = 0 then
      result := inttostr(trunc((p as TcNode).X))
    // Y
    else if AnsiCompareText(Tag, 'Y') = 0 then
      result := inttostr(trunc((p as TcNode).Y));
  end;
end;

// TcGraph
//   SetProperties
//
procedure TcGraph.SetProperties(Tag: String; ID: longint; value: String);
var
  p: TcObject;
begin
  p := GetNodeByID(ID);
  if (p <> nil) and (p is TcNode) then
  begin
    // X
    if AnsiCompareText(Tag, 'X') = 0 then
      (p as TcNode).X := strtointdef(value, 0)
    // Y
    else if AnsiCompareText(Tag, 'Y') = 0 then
      (p as TcNode).Y := strtointdef(value, 0);
  end;
end;

// TcGraph
//   SetLocation
//
procedure TcGraph.SetLocation(Index: longint; Value: TdPoint);
begin
  if (m_ParentControl <> nil) and (m_ParentControl is TGraphPanel) and Assigned((m_ParentControl as TGraphPanel).m_OnChangeLocation) then
    (m_ParentControl as TGraphPanel).m_OnChangeLocation(Index, Value);
end;

//
// TcCustomNode
//

// TcCustomNode
//   Add
//
function TcCustomNode.Add(parObject: TcObject): longint;
begin
  result := inherited Add(parObject);
  if (TopParent <> nil) and (TopParent is TcGraph) then
    (TopParent as TcGraph).AddObject(parObject);
end;

// TcCustomNode
//   LocalDelete (1)
//
procedure TcCustomNode.LocalDelete(parObject: TcObject);
begin
  if (TopParent <> nil) and (TopParent is TcGraph) then
    with TopParent as TcGraph do
    begin
      if (parObject <> nil) and (parObject is TcNode) then
        m_lstNodes.Delete(parObject)
      else if (parObject <> nil) and (parObject is TcEdge) then
        m_lstEdges.Delete(parObject);
      DeleteObject(parObject);
    end;
end;

// TcCustomNode
//   Delete (1)
//
function TcCustomNode.Delete(parObject: TcObject): longint;
begin
  LocalDelete(parObject);
  result := inherited Delete(parObject);
end;

// TcCustomNode
//   Delete (2)
//
function TcCustomNode.Delete(Index: longint): longint;
begin
  LocalDelete(Objects[Index]);
  result := inherited Delete(Index);
end;

// TcCustomNode
//   ResolveDifferences
//
function TcCustomNode.ResolveDifferences(parObject: TcCustomNode; hdlDifferenceCallBack: TDifferenceCallBack): boolean;

  procedure ProcessAll(parParent, parNode: TcObject; eparAction: TedAction; parMode: boolean);
  var
    i: longint;
    p: TcObject;
  begin
    p := nil;
    if eparAction = edAdd then
      if ( (not parMode and (parNode is TcNode)) or (parMode and (parNode is TcEdge)) ) then
        p := hdlDifferenceCallBack(parParent, parNode, eparAction);
    for i := 0 to parNode.count - 1 do
      if (parNode[i] <> nil) and (parNode[i] is TcCustomNode) then
        ProcessAll(p, parNode[i] as TcCustomNode, eparAction, parMode);
    if eparAction = edDelete then
      if ( (not parMode and (parNode is TcNode)) or (parMode and (parNode is TcEdge)) ) then
        hdlDifferenceCallBack(nil, parNode, eparAction);
  end;

  procedure SubCall(parNode1, parNode2: TcCustomNode; parMode: boolean);
  var
    i: longint;
    p: TcObject;
  begin
    //
    // Process difference (update)
    if (parNode2 <> nil) and not parNode1.Compare(parNode2) then
    begin
      if ( (not parMode and (parNode1 is TcNode)) or (parMode and (parNode1 is TcEdge)) ) then
        hdlDifferenceCallBack(parNode2, parNode1, edUpdate);
    end
    else if (parNode2 = nil) then
      ProcessAll(parObject, parNode1, edAdd, parMode);
    //
    // Process Differences (Updates) and Deletions
    if parNode2 <> nil then
    begin
      //
      //
      for i := parNode1.count - 1 downto 0 do
        if (parNode1[i] <> nil) and (parNode1[i] is TcCustomNode) then
        begin
          p := parNode2.Find(parNode1[i]);
          if (p <> nil) and (p is TcCustomNode) then
            SubCall(parNode1[i] as TcCustomNode, p as TcCustomNode, parMode)
          else
            ProcessAll(parNode2, parNode1[i] as TcCustomNode, edAdd, parMode);
        end;
      //
      // Process Insertions
      for i := parNode2.count - 1 downto 0 do
        if (parNode2[i] <> nil) and (parNode2[i] is TcCustomNode) then
        begin
          p := parNode1.Find(parNode2[i]);
          if p = nil then
            ProcessAll(parNode2, parNode2[i] as TcCustomNode, edDelete, parMode);
        end;
    end;
  end;

begin
  if Assigned(hdlDifferenceCallBack) then
  begin
    // Process Nodes
    SubCall(self, parObject, FALSE);
    // Process Edges
    SubCall(self, parObject, TRUE);
  end;
  result := TRUE;
end;

// TcCustomNode
//   SetColor
//
procedure TcCustomNode.SetColor(value: TeColor);
begin
  m_eColor := value;
end;

//
// TcNode
//

// TcNode
//   Create
//
constructor TcNode.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_iIndex := kiUNDEFINED;
  m_dX := 0;
  m_dY := 0;
  old_x := 0;
  old_y := 0;
  m_bFixed := FALSE;
  m_bVisited := FALSE;
  m_iDistance := 0;
  eColor := ecNode;
  m_objPanel := nil;
  m_iIcon := kiUNDEFINED;
  m_iAlertLevel := kiUNDEFINED;
end;

// TcNode
//   Destroy
//
destructor TcNode.Destroy;
begin
  //
  inherited destroy;
end;

// TcNode
//   Compare
//
function TcNode.Compare(parValue: TcObject): boolean;
begin
  result := (AnsiCompareText(sName, parValue.sName) = 0) and  // Skip collection count difference
            (iCode = parValue.iCode);
  if parValue is TcNode then
    result := result and
              (sValue = (parValue as TcNode).sValue) and
              (m_iIndex = (parValue as TcNode).m_iIndex) and
              (m_iIcon  =  (parValue as TcNode).m_iIcon) and
              (m_iAlertLevel = (parValue as TcNode).m_iAlertLevel);
end;

// TcNode
//   SetXML method
//
procedure TcNode.SetXML(Value: String);
Var
  pReader: TSAXXMLReader;
  pContent: TcNodeSaxContentHandler;
Begin
  CoInitialize(nil);
  pReader := nil;
  if value <> ksEMPTY then
  try
    pReader := TSAXXMLReader.Create(nil);
    pContent := TcNodeSaxContentHandler.Create(pReader);
    pReader.ContentHandler := pContent;
    pReader.DeclHandler := TSAXDeclHandler.Create(pReader);
    pReader.DTDHandler := TSAXDTDHandler.Create(pReader);
    pReader.EntityResolver := TSAXEntityResolver.Create(pReader);
    pReader.ErrorHandler := TSAXErrorHandler.Create(pReader);
    pReader.LexicalHandler := TSAXLexicalHandler.Create(pReader);
    pReader.Vendor := TSAXMSXML.Create(pReader).Vendor;
    pContent.Push(self);
    try
      pReader.XML.Text := value;
      pReader.Parse;
    except
      Clear;
      Raise;
    end;
  finally
    pReader.Free;
  end
  else
    Clear;
  CoUninitialize;
End;

// TcNode
//   Find method
//
function TcNode.Find(Value: TcObject): TcObject;
var
  i: longint;
begin
  result := nil;
  for i := 0 to count - 1 do
    if ((value is TcNode) and (Objects[i] <> nil) and (Objects[i] is TcNode) and (Objects[i].sName = value.sName) and (Objects[i].sValue = value.sValue)) or
       ((value is TcEdge) and (Objects[i] <> nil) and (Objects[i] is TcEdge) and (Objects[i].sName = value.sName) and (Objects[i].sValue = value.sValue) and ((Objects[i] as TcEdge).iEdgeType = (value as TcEdge).iEdgeType)) then
    begin
      result := Objects[i];
      break;
    end;
end;

// TcNode
//   Update
//
procedure TcNode.Update(dx, dy: double);
begin
  old_x := x;
  old_y := y;
  x := x + dx;
  y := y + dy;
end;

// TcNode
//   GetCenterX
//
function TcNode.GetCenterX: longint;
begin
  result := round(m_dX);
  if m_objPanel <> nil then
    result := result + m_objPanel.Width div 2;
end;

// TcNode
//   GetCenterY
//
function TcNode.GetCenterY: longint;
begin
  result := round(m_dY);
  if m_objPanel <> nil then
    result := result + m_objPanel.Height div 2;
end;

// TcNode
//   GetPanel
//
function TcNode.GetPanel: TObjectPanel;
begin
  result := m_objPanel;
end;

// TcNode
//   SetPanel
//
procedure TcNode.SetPanel(value: TObjectPanel);
begin
  m_objPanel := value;
end;

// TcNode
//   HasEdge
//
function TcNode.HasEdge(parCode: longint): boolean;
var
  i: longint;
begin
  result := FALSE;
  for i := 0 to count - 1 do
    if (Objects[i] <> nil) and (Objects[i] is TcEdge) and ((Objects[i] as TcEdge).iCode = parCode) then
    begin
      result := TRUE;
      break;
     end;
end;

// TcNode
//   GetHint
//
function TcNode.GetHint: String;
begin
  result := InitCap(sName);
  if sValue <> ksEMPTY then
    result := result + Format(' %s', [sValue]);
end;

// TcNode
//   GetCaption
//
function TcNode.GetCaption: String;
begin
  result := sValue;
  if result = ksEMPTY then
    result := sName;
end;

// TcNode
//   SetIcon
//
procedure TcNode.SetIcon(value: longint);
begin
  m_iIcon := value;
  if m_objPanel <> nil then
    m_objPanel.Icon := value;
end;

// TcNode
//   SetAlert
//
procedure TcNode.SetAlertLevel(value: longint);
begin
  m_iAlertLevel := value;
  if m_objPanel <> nil then
    m_objPanel.AlertLevel := value;
end;

// TcNode
//   GetPoint
//
function TcNode.GetPoint: TdPoint;
begin
  result.X := m_dX;
  result.Y := m_dY;
end;

// TcNode
//   SetPoint
//
procedure TcNode.SetPoint(value: TdPoint);
begin
  m_dX := value.X;
  m_dY := Value.Y;
end;

//
// TcNodeSaxContentHandler
//

// TcNodeSaxContentHandler
//   Create method
//
constructor TcNodeSaxContentHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  m_lstPath := TcBag.Create(nil);
  m_bIsValue := FALSE;
end;

// TcNodeSaxContentHandler
//   Destroy method
//
destructor TcNodeSaxContentHandler.Destroy;
begin
  m_lstPath.free;
  inherited Destroy;
end;

// TcNodeSaxContentHandler
//   StartElement method
//
procedure TcNodeSaxContentHandler.StartElement(const NamespaceURI, LocalName, QName: SAXString; const Atts: IAttributes);
var
  FCode: longint;
  par: TcNode;
  p: TcObject;
  ne: TcEdge;
Begin
  Inherited StartElement(NamespaceURI, LocalName, QName, Atts);
  par := Top;
  if par <> nil then
  try
    m_bIsValue := FALSE;
    // Code
    FCode := Strtointdef(Atts.getValue(krsXMLCODE), kiUNDEFINED);
    //
    // "value" node
    if AnsiCompareText(LocalName, krsXMLVALUE) = 0 then
    begin
      m_bIsValue := TRUE;
      Push(par);
    end
    //
    // "node" node
    else if AnsiCompareText(LocalName, krsXMLNODE) = 0 then
    begin
      if ((par.Parent = nil) or ((par.Parent <> nil) and (par.Parent is TcGraph))) and (par.sName = ksEMPTY) then
        p := par
      else
      begin
        p := TcNode.Create(par);
        par.Add(p);
      end;
      p.iCode := FCode;
      p.sName := Atts.getValue(krsXMLMETA);
      p.sValue := Atts.getValue(krsXMLNAME);
      (p as TcNode).iIndex := Strtointdef(Atts.getValue(krsXMLINDEX), kiUNDEFINED);
      (p as TcNode).Icon := Strtointdef(Atts.getValue(krsXMLICON), kiUNDEFINED);
      (p as TcNode).AlertLevel := Strtointdef(Atts.getValue(krsXMLALERT), kiUNDEFINED);
      (p as TcNode).X := Strtointdef(Atts.getValue('X'), 0);
      (p as TcNode).Y := Strtointdef(Atts.getValue('Y'), 0);
      (p as TcNode).Fixed := ((p as TcNode).X <> 0) or ((p as TcNode).Y <> 0);
      Push(p);
    end
    //
    // "link" node
    else if AnsiCompareText(LocalName, krsXMLLINK) = 0 then
    begin
      ne := TcEdge.Create(par);
      ne.iCode := FCode;
      ne.sName := Atts.getValue(krsXMLMETA);
      ne.sValue := Atts.getValue(krsXMLNAME);
      ne.iEdgeType := Strtointdef(Atts.GetValue(krsXMLTYPE), kiUNDEFINED);
      par.Add(ne);
      Push(ne);
    end;

  except
    on E: Exception do
      raise;
  end;
End;

// TcNodeSaxContentHandler
//   Characters method
//
procedure TcNodeSaxContentHandler.Characters(const PCh: SAXString);
var
  p: TcObject;
Begin
  Inherited Characters(PCh);
  if m_bIsValue then
  begin
    p := Top;
    if (p <> nil) and (p is TcNode) and (trim(PCh) <> ksEMPTY) then
      (p as TcNode).sValue := (p as TcNode).sValue + Pch;
  end;
End;

// TcNodeSaxContentHandler
//   EndElement method
//
procedure TcNodeSaxContentHandler.EndElement(const NamespaceURI, LocalName, QName: SAXString);
Begin
  Inherited EndElement(NamespaceURI, LocalName, QName);
  Pop;
  m_bIsValue := FALSE;
End;

// TcNodeSaxContentHandler
//   Push method
//
procedure TcNodeSaxContentHandler.Push(parObject: TcObject);
begin
  m_lstPath.Add(parObject);
end;

// TcNodeSaxContentHandler
//   Pop method
//
procedure TcNodeSaxContentHandler.Pop;
begin
  if m_lstPath.Count > 0 then
    m_lstPath.Delete(m_lstPath.Count - 1);
end;

// TcNodeSaxContentHandler
//   Top method
//
function TcNodeSaxContentHandler.Top: TcNode;
begin
  result := nil;
  if not m_lstPath.IsEmpty then
    result := m_lstPath[m_lstPath.Count - 1] as TcNode;
end;

//
// TcEdge
//

// TcEdge
//   Create
//
constructor TcEdge.Create(parParent: TcObject);
begin
  inherited Create(parParent);
  m_iEdgeType := kiUNDEFINED;
  m_objPaintBox := nil;
  eColor := ecEdge;
  m_eColorDrawn := eColor;
end;

// TcEdge
//   Destroy
//
destructor TcEdge.Destroy;
begin
  if m_objPaintBox <> nil then
    m_objPaintBox.Free;
  m_objPaintBox := nil;
  inherited destroy;
end;

// TcEdge
//   Compare
//
function TcEdge.Compare(parValue: TcObject): boolean;
begin
  result := inherited Compare(parValue);
  if parValue is TcEdge then
    result := result and (m_iEdgeType = (parValue as TcEdge).m_iEdgeType);
end;

// TcEdge
//   SetColor
//
procedure TcEdge.SetColor(value: TeColor);
begin
  inherited SetColor(value);
  if (m_objPaintBox <> nil) and Assigned(m_objPaintBox.onPaint) then
    m_objPaintBox.onPaint(m_objPaintBox);
end;

// TcEdge
//   GetToNode
//
function TcEdge.GetToNode: TcNode;
var
  lst: TcBag;
  i: longint;
begin
  result := nil;
  if (TopParent <> nil) and (TopParent is TcGraph) then
  begin
    lst := (TopParent as TcGraph).m_lstNodes;
    for i := 0 to lst.Count - 1 do
      if (lst[i] <> nil) and (lst[i] is TcNode) and (lst[i].iCode = iCode) then
      begin
        result := lst[i] as TcNode;
        break;
      end;
  end;
end;

//
// TObjectPanel
//

// TObjectPanel
//   Create
//
constructor TObjectPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := GetWinVersion >= wvWinXP;
  m_imgIcons := nil;
  m_imgAlerts := nil;
  m_iAlertLevel := kiUNDEFINED;
  m_sMetaName := ksEMPTY;
  m_sDataName := ksEMPTY;
end;

// TObjectPanel
//   Paint
//
procedure TObjectPanel.Paint;
var
  d, X, Y: longint;
const
  kiANGLE = 5;
begin
  // a. Clear Background
  Canvas.Brush.Color := (Parent as TGraphPanel).Color;
  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.FillRect(rect(0, 0, Width, Height));
  // b. Repaint Background as a Label
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := clAppWorkSpace;
  Canvas.Polygon( [Point(kiANGLE div 2, 0),
                   Point(Width - 1, 0),
                   Point(Width - 1, Height - kiANGLE - 1),
                   Point(Width - kiANGLE - 1, Height - 1),
                   Point(0, Height - 1), Point(0, kiANGLE),
                   Point(0, kiANGLE div 2)]);
  // Canvas.PolyLine([Point(Width - 1, Height - kiANGLE + 1), Point(Width - kiANGLE, Height)]);
  // c. Draw Icon
  d := 0;
  if (m_iIcon <> kiUNDEFINED) and (m_imgIcons <> nil) then
  begin
    m_imgIcons.Draw(Canvas, 2, (Height - m_imgIcons.Height) div 2 - 1, m_iIcon - 1, TRUE);
    d := m_imgIcons.Width;
  end;
  // d. Draw Text
  X := d + kiTEXTSPACE;
  Y := (Height - Canvas.TextHeight(GetText)) div 2;
  Canvas.TextRect(Rect(X, Y, X + Canvas.TextWidth(GetText), Y + Canvas.TextHeight(GetText)), X, Y, GetText);
  // e. Draw Alert
  if (m_iAlertLevel <> kiUNDEFINED) and (m_imgAlerts <> nil) then
    m_imgAlerts.Draw(Canvas, width - m_imgAlerts.Height - 2, Height - m_imgAlerts.Height - 2, 0 (*m_iAlertLevel*), TRUE);
end;

// TObjectPanel
//   SetIcon
//
procedure TObjectPanel.SetIcon(value: longint);
begin
  if m_imgIcons <> nil then
  begin
    m_iIcon := value;
    Paint;
  end;
end;

// TObjectPanel
//   GetIndex
//
function TObjectPanel.GetIndex: longint;
var
  p: TcObject;
begin
  result := kiUNDEFINED;
  if Tag <> 0 then
  begin
    p := pointer(Tag);
    if (p <> nil) and (p is TcObject) then
      result := p.Tag;
  end;
end;

// TObjectPanel
//   SetText
//
procedure TObjectPanel.SetText(const Value: TCaption);
begin
  m_sText := value;
end;

// TObjectPanel
//   GetText
//
function TObjectPanel.GetText: TCaption;
begin
  result := m_sText;
end;

// TObjectPanel
//   SetAlertLevel
//
procedure TObjectPanel.SetAlertLevel(value: longint);
begin
  m_iAlertLevel := value;
  Invalidate;
end;

end.
















