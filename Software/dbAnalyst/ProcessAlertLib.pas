unit ProcessAlertLib;

interface

uses
  Classes,
  daObjectLib,
  DataLib;

type
  TProcessAlert = class(TThread)
  private
    m_objData: TcData;
    m_objDisplayAlerts: TThreadMethod;

  protected
    procedure Execute; override;

  public
    property Data: TcData                  read m_objData          write m_objData;
    property DisplayAlerts: TThreadMethod  read m_objDisplayAlerts write m_objDisplayAlerts;
  end;

implementation

{ Important: Methods and properties of objects in VCL or CLX can only be used
  in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TProcessAlert.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TProcessAlert }

uses
  daGlobals;

procedure TProcessAlert.Execute;

  procedure Traverse(parData: TcData);
  var
    i: longint;
  begin
    // Trigger rule processing
    parData.Statement([enRule], ksEMPTY);
    // Process child elements
    for i := 0 to parData.count - 1 do
      Traverse(parData[i] as TcData);
  end;

begin
  if m_objData <> nil then
  try
    if m_objData is TcData then
    begin
      (m_objData as TcData).ClearRuleResult;
      Traverse(m_objData as TcData);
    end;
    if Assigned(m_objDisplayAlerts) then
      Synchronize(m_objDisplayAlerts);
  except
    //
  end;
end;

end.
