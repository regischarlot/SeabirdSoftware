{$I DFS.INC}

unit DFSSplitterReg;

interface

procedure Register;

implementation

uses
  DFSSplitter, DFSAbout, Classes, DesignIntf, DesignEditors;

procedure Register;
begin
  RegisterComponents('DFS', [TdfsSplitter]);
  RegisterPropertyEditor(TypeInfo(string), TdfsSplitter, 'Version',
     TdfsVersionProperty);
end;


end.
