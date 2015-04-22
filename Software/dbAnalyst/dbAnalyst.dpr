program dbAnalyst;

uses
  Forms,
  GraphPanel in 'GraphPanel.pas',
  PreferenceLib in 'PreferenceLib.pas',
  frmPreferences in 'frmPreferences.pas' {frmPreferences},
  frmDataExport in 'frmDataExport.pas' {tfrmDataExport},
  frmDataImport in 'frmDataImport.pas' {frmDataImport},
  ProcessAlertLib in 'ProcessAlertLib.pas',
  frmDataImportEdit in 'frmDataImportEdit.pas' {frmDataImportEdit},
  frmSessionManager in 'frmSessionManager.pas' {frmSessionManager},
  PanelLib in 'PanelLib.pas',
  frmGrid in 'frmGrid.pas' {frmGrid},
  StatementLib in 'StatementLib.pas',
  frmDataTranslate in 'frmDataTranslate.pas' {frmDataTranslate},
  frmSQLLog in 'frmSQLLog.pas' {frmSQLLog},
  frmStatementBuild in 'frmStatementBuild.pas' {frmStatementBuild},
  frmTextEdit in 'frmTextEdit.pas' {frmStatementEdit},
  frmStatementBuildCondition in 'frmStatementBuildCondition.pas' {frmStatementBuildCondition},
  frmStatementBuildDistinctValues in 'frmStatementBuildDistinctValues.pas' {frmStatementBuildDistinctValues},
  frmFavorite in 'frmFavorite.pas' {frmFavorite},
  ImageListBoxLib in 'ImageListBoxLib.pas',
  PreferencePanel in 'PreferencePanel.pas',
  frmEditor in 'frmEditor.pas' {frmEditor},
  frmQuery in 'frmQuery.pas' {frmQuery},
  Grid in 'Grid.pas',
  frmDataPreferences in 'frmDataPreferences.pas' {frmDataPreference},
  frmDataFind in 'frmDataFind.pas' {frmDataFind},
  Main in 'Main.pas' {frmMain},
  frmData2 in 'frmData2.pas',
  frmDataSnapshot in 'frmDataSnapshot.pas',
  frmDataSnapshotDifference in 'frmDataSnapshotDifference.pas',
  SourceListBox in 'SourceListBox.pas',
  frmWebService in 'frmWebService.pas' {Form1},
  frmLogParser in 'frmLogParser.pas',
  SynHighlighterdbAnalystSQL in 'SynHighlighterdbAnalystSQL.pas',
  frmSynEditConfirmReplace in 'frmSynEditConfirmReplace.pas' {ConfirmReplaceDialog},
  frmSynEditSearchText in 'frmSynEditSearchText.pas' {TextSearchDialog},
  frmSynEditReplaceText in 'frmSynEditReplaceText.pas',
  SynEditSearchReplaceLib in 'SynEditSearchReplaceLib.pas',
  wsdl in 'wsdl.pas',
  ADODB_TLB in 'ADODB_TLB.pas',
  WebServiceLib in 'WebServiceLib.pas',
  FTPLib in 'FTPLib.pas',
  BlockCiphers in 'BlockCiphers.pas',
  ConnectionBuilderLib in 'ConnectionBuilderLib.pas',
  ConnectionLib in 'ConnectionLib.pas',
  CursorStackLib in 'CursorStackLib.pas',
  CustomControls in 'CustomControls.pas',
  daGlobals in 'daGlobals.pas',
  daObjectLib in 'daObjectLib.pas',
  daResourceStrings in 'daResourceStrings.pas',
  daStreamLib in 'daStreamLib.pas',
  DataLib in 'DataLib.pas',
  dbListView in 'dbListView.pas',
  dbListViewEdit in 'dbListViewEdit.pas' {frmdbListViewEdit},
  EdgeLib in 'EdgeLib.pas',
  ExecuteLib in 'ExecuteLib.pas',
  FavoriteLib in 'FavoriteLib.pas',
  FormLib in 'FormLib.pas',
  frmAbout in 'frmAbout.pas' {frmAbout},
  frmConnection in 'frmConnection.pas' {frmConnection},
  frmConnectionLogin in 'frmConnectionLogin.pas' {frmConnectionLogin},
  frmConnectionWizard in 'frmConnectionWizard.pas' {frmConnectionWizard},
  frmOnlineHelp in 'frmOnlineHelp.pas',
  frmHelp in 'frmHelp.pas' {Form2};

{$R *.res}
{$R WindowsXP.res}

{$DEFINE VER20}

begin
  Application.Initialize;
  Application.Title := 'Seabird Software''s dbAnalyst';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
