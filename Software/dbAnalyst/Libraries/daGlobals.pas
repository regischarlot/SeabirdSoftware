unit daGlobals;

interface

uses
  Windows,
  ADODB_TLB,
  Graphics,
  ComCtrls,
  StdCtrls,
  Classes,
  SHDocVw,
  Progress,
  daResourceStrings;

type
  TeType                        = (enUndefined, enObject, enDependency, enSQL, enOpenSchema, enDisplay, enField, enAttribute, enScript, enRule,
                                   enMenu, enItem, enProperty, enInfo, enFeature, enSyntaxHilighting, enKeyword, enOption,
                                   enQuery, enClause, enMap, enValue, enEditor, enGrid, enError, enComboBox, enCheckBox, enDefault, enIcon,
                                   enSection, enComment, enHelp);
  TeTypeSet                     = set of TeType;
  TeDatatype                    = (etUndefined, etNull, etAnsiString, etInteger, etDate, etBoolean, etFloat, etLongString, etBinary, etUnicodeString);
  TvKeyword                     = ( // Parser flow keywords
                                    _UNDEFINED, _WILDCARD, _STRING, _NUMBER, _REAL, _BOOLEAN, _SPACE, _EOS, _IDENTIFIER,
                                    // Single character delimitors
                                    _LBRACKET, _RBRACKET, _LPAREN, _RPAREN, _LCURLY, _RCURLY, _COMMA, _PERIOD,
                                    _COLON, _SEMICOLON, _QUOTE, _MULT, _DIV, _ADD, _SUBSTRACT, _QUESTIONMARK, _CARET,
                                    _LOWER, _LOWEREQUAL, _NOTEQUAL, _GREATER, _GREATEREQUAL, _EQUAL, _ASSIGN,
                                    _NOT, _IN, _AND, _OR, _LIKE, _IS, _B_OR, _B_AND, _PRAGMA, _OPERATOR, _PARAGRAPH, _CONCAT,
                                    __ISNOT, __NOTIN,
                                    // Administrative
                                    _ITEM, _TERM, _MACRO, _LITERAL_STRING, _LITERAL_NUMBER, _LITERAL_REAL,
                                    // Functions
                                    __UPPER, __LOWER, __COPY, __HAS, __POS, __CHR,
                                    // SQL Keywords
                                    __CASE, __WHEN, __THEN, __END, __ELSE, __MINUS, __UNION, __INTERSECT,
                                    __SELECT, __FROM, __ORDER, __BY, __GROUP, __HAVING, __WHERE, __DISTINCT,
                                    __NOTLIKE, __AS, __INSERT, __INTO, __VALUES, __UPDATE, __SET, __DELETE, __DESC, __DESCRIBE,
                                   __DROP, __TRUNCATE, __CROSS, _INNER, __LEFT, __NATURAL, __RIGHT, __JOIN, __OUTER,
                                   __ON, __ASC, __NULLS, __LAST);
  TvKeywordSet                  = set of TvKeyword;

  TeTokenType                   = (ettUndefined, ettSelect, ettField, ettFrom, ettSet, ettDataset, ettWhere,
                                   ettGroupBy, ettOrderBy, ettAs, ettOperation, ettFunction, ettCase,
                                   ettWhen, ettThen, ettUnion, ettIntersect, ettMinus, ettParenthese,
                                   ettProperty,
                                   ettColumn, ettString, ettNumber, ettInlineView, ettReference, ettReal, ettNull,
                                   ettInsert, ettValues, ettInto, ettUpdate, ettDelete, ettDescribe, ettDrop, ettTruncate, ettElse,
                                   ettJoin, ettOn, ettEnd, ettTable, ettBracket);
  TeTokenTypeSet                = set of TeTokenType;

  TeXMLFileType                 = (xftUndefined, xftMetaData, xftData);
  TeDisplayType                 = (edtUndefined, edtGraph, edtScript, edtAlerts, edtErrors);
  TeLevel                       = (elAll, elFatal, elWarning, elSQL, elMetaFile);
  TeRuleErrorLevel              = (relNone, relScript, relInformation, relWarning, relError);
  TePreferenceType              = (eptUndefined, eptValue, eptFont, eptColor, eptConnection);
  TePreferenceTypeSet           = set of TePreferenceType;
  TeMenu                        = (mmSave, mmClose, mmSaveAs, mmNew, mmOpen, mmReOpen);
  TeForm                        = (efsObjects, efsQuery, efsGrid, efsSessionManager, efsStoredQuery, efsLogParser, efsOnlineHelp);
  TeFormInitialization          = (efiFromDatabase, efiFromTranslation, efiFromFavorite);
  TeSyntaxHiliteType            = (eshtKeyword, eshtDatatype, eshtFunction, eshtMark, eshtComment, eshtPackage, eshtException, eshtNumber, eshtString);
  TeMetaFlag                    = (emUpperCase, emLowerCase, emLocal, emTemporary);
  TeMetaFlagSet                 = set of TeMetaFlag;
  TeQueryType                   = (eqtUndefined, eqtText, eqtGrid, eqtAnalysis);
  TecContentType                = (ectStatement, ectIndex, ectObjects);
  TecContentTypeSet             = set of TecContentType;
  TeQueryOption                 = (eqoPaste);
  TeQueryOptions                = set of TeQueryOption;
  TeLogFlag                     = (elfValid, elfIncrement, elfMemorize);
  TeLogFlagSet                  = set of TeLogFlag;
  TfctSetLog                    = function (sSQLName, value: String; duration: TDateTime; eFlags: TeLogFlagSet): longint of object;
  TePartType                    = (epUndefined, epColumn, epSchema, epSequence, epSnapshot, epTable, epView, epAlias, epSynonym, epPrefix, epWhere, epOrder, epIndex);
  TePartSide                    = (epsNone, epsRHS, epsLHS);
  TePartTypeSet                 = set of TePartType;
  TeBooleanOperator             = (eboAnd, eboOr);
  TeConditionOperator           = (ecoNone, ecoEqual, ecoNotEqual1, ecoNotEqual2, ecoGreater, ecoGreaterEqual, ecoNotGreater, ecoSmaller, ecoSmallerEqual,
                                   ecoNotSmaller, ecoLike, ecoNotLike, ecoIn, ecoNotIn, ecoIsNull, ecoIsNotNull);
  TeOperatorRHS                 = (eorNone, eorColumn, eorValue, eorBoth);
  TeItemOption                  = (eioDecsription, eioHTML);
  TeItemOptionSet               = set of TeItemOption;
  TDBListViewOption             = (edboReadOnly, edboFixCRLF, edboHex, edboWrap);
  TDBListViewOptions            = set of TDBListViewOption;

  TeDeliveryType                = (edtEMail, edtLog);
  TeDeliveryTypeSet             = set of TeDeliveryType;
  TeState                       = (edsUndefined, edsDB, edsModified, edsAdded, edsDeleted);
  TeFeatureSupport              = (efsTransaction, efsAsynchronous);
  ThdlDataDisplay               = TNotifyEvent;
  TePanel                       = (psPanel1, psPanel2);
  TePanelSet                    = set of TePanel;
  TeIcon                        = (eiUndefined, eiUser, eiTable, eiIndex, eiView, eiTrigger, eiProcedure, eiFunction, eiType,
                                   eiTablespace, eiQuota, eiSequence, eiXMLSchema, eiSynonym, eiDatabaseLink, eiPackage,
                                   eiGrant, eiCluster, eiLibrary, eiDirectory, eiContext, eiDimension, eiOutline,
                                   eiIndexType, eiOperator, eiRole, eiProfile, eiVariable, eiUser2, eiSession, eiRecycleBin,
                                   eiMaterializedView, eiRoutine);
  TeDirection                   = (eFrom, eTo);
  TeOutputType                  = (eSQL, eHTML);
  TeOutputTypeSet               = set of TeOutputType;
  TeDifference                  = (ecExtra, ecMissing, ecDifference, ecMatch, ecUnknown);
  TeDifferenceSet               = set of TeDifference;
  TfctProgress                  = procedure (parLineStart, parLineEnd: longint; parMode: TeProgressMode) of object;
  TProcError                    = procedure (value: String) of object;
  TeConnectionMode              = (ecmMDAC, ecmOracle);
  TeStatusIcon                  = (esiNone, esiTextTop, esiTextBottom, esiGridBottom, esiTextRight, esiTextLeft, esiGridRight);
  TeFormState                   = (efmCreate, efmInitialize, efmRunning, efmFinalize, efmDestroy);
  TeLogHostType                 = (elhtWindowsLocal, elhtWindowsRemote, elhtFTP);
  TeSessionEventType            = (esetUndefined, esetSession, esetFile);
  TWinVersion                   = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME, wvWin2000, wvWinXP, wvWinVista);
  TeFetchType                   = (eftGetRows, eftMoveNext);
  TeLicenseLevel                = (ellNone, ellFree, ellBeta, ellProfessional);
  TeCharSize                    = (ecsAnsi, ecsUnicode);

const
  kiUNDEFINED                   = -1;
  ksEMPTY                       = '';
  kcSPACE                       = ' ';
  ksCR                          = #$D + #$A;
  kcTAB                         = #$9;
  ksPAR                         = '\par ';
  ksTAB                         = '\tab ';
  kiDATATYPEOFFSET              = $00001000;

  ntNODE_ELEMENT                = 1;
  ntNODE_ATTRIBUTE              = 2;
  ntNODE_TEXT                   = 3;
  ntNODE_CDATA_SECTION          = 4;
  ntNODE_ENTITY_REFERENCE       = 5;
  ntNODE_ENTITY                 = 6;
  ntNODE_PROCESSING_INSTRUCTION = 7;
  ntNODE_COMMENT                = 8;
  ntNODE_DOCUMENT               = 9;
  ntNODE_DOCUMENT_TYPE          = 10;
  ntNODE_DOCUMENT_FRAGMENT      = 11;
  ntNODE_NOTATION               = 12;

  kcolSELECTEDEDGE              = clBlack;
  kcolEDGE                      = clLtGray;
  kiICONSTART                   = 0;

  kasTYPE: array[TeType] of String =
    (ksEMPTY, 'OBJECT', 'DEPENDENCY', 'SQL', 'OPENSCHEMA', 'DISPLAY', 'FIELD', 'ATTRIBUTE', 'SCRIPT', 'RULE',
     'MENU', 'ITEM', 'PROPERTY', 'INFO', 'FEATURE', 'SYNTAXHILIGHTING', 'KEYWORD', 'OPTION', 'QUERY',
     'CLAUSE', 'MAP', 'VALUE', 'EDITOR', 'GRID', 'ERROR', 'COMBOBOX', 'CHECKBOX', 'DEFAULT', 'ICON',
     'SECTION', 'COMMENT', 'HELP');

  kasTYPEINITCAP: array[TeType] of String =
    (ksEMPTY, 'Object', 'Dependency', 'SQL', 'OpenSchema', 'Display', 'Field', 'Attribute', 'Script', 'Rule',
     'Menu', 'Item', 'Property', 'Info', 'Feature', 'SyntaxHilighting', 'Keyword', 'Option', 'Query',
     'Clause', 'Map', 'Value', 'Editor', 'Grid', 'Error', 'ComboBox', 'Checkbox', 'Default', 'Icon',
     'Section', 'Comment', 'Help');

  _TOKENS: array[TvKeyword] of String =
    ( '?', '', 'string', 'number', 'real', 'boolean', 'space', 'EOS', 'identifier',
      // Single character delimitors
      '[', ']', '(', ')', '{', '}', ',', '.',
      ':', ';', '"', '*', '/', '+', '-', '?', '^',
      '<', '<=', '!=', '>', '>=', '=', ':=',
      'not', 'in', 'and', 'or', 'like', 'is', '|', '&', '#pragma', '::', kcPARAGRAPH, '||',
      'is not', 'not in',
      // Administrative
      '', '', '', '', '', '',
      // Functions
      'upper', 'lower', 'copy', 'has', 'pos', 'chr',
      // SQL Keywords
      'case', 'when', 'then', 'end', 'else', 'minus', 'union', 'intersect',
      'select', 'from', 'order', 'by', 'group', 'having', 'where', 'distinct',
      'not like', 'as', 'insert', 'into', 'values', 'update', 'set', 'delete', 'desc', 'describe',
      'drop', 'truncate', 'cross', 'inner', 'left', 'natural', 'right', 'join', 'outer',
      'on', 'asc', 'nulls', 'last'
    );

  kasTOKENTYPE: array[TeTokenType] of String =
    (ksEMPTY, 'select', 'field', 'from', 'set', 'dataset', 'where',
     'group by', 'order by', 'as', 'operation', 'function', 'case',
     'when', 'then', 'union', 'intersect', 'minus', 'paren',
     'property',
     'column', 'string', 'number', 'inline view', 'reference', 'real', 'null',
     'insert', 'values', 'into', 'update', 'delete', 'describe', 'drop', 'truncate', 'else',
     'join', 'on', 'end', 'table', 'bracket');

  kasERRORLEVEL: array[TeLevel] of string =
    (ksEMPTY, 'Fatal Error', 'Warning', 'SQL Error', 'Meta Definition');

  kasBOOL: array[boolean] of String =
    (krsFALSE, krsTRUE);

  kasRULEERRORLEVEL: array[TeRuleErrorLevel] of String =
    (ksEMPTY, krsSCRIPT, krsINFORMATION, krsWARNING, krsERROR);

  kasRULEERRORMINLEVEL: array[TeRuleErrorLevel] of String =
    (ksEMPTY, ksEMPTY, 'Informations, Warnings & Errors', 'Warnings & Errors', 'Errors Only');

  kasPREFERENCETYPE: array[TePreferenceType] of String =
   (ksEMPTY, krsVALUE, krsFONT, krsCOLOR, krsCONNECTION);

  kabFORMVISIBLE: array[TeForm] of boolean =
    (TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE);

  kaiFORMIMAGEINDEX: array[TeForm] of longint =
    (23, 22, 47, 59, 30, 80, 89);

  kaiFORMSMALLIMAGEINDEX: array[TeForm] of longint =
    (-1, 48, 49, -1, -1, -1, -1);

  kasSYNTAXHILITETYPE: array[TeSyntaxHiliteType] of String =
    (krsSH_KEYWORD, krsSH_DATATYPE, krsSH_FUNCTION, krsSH_MARK, krsSH_COMMENT, krsSH_PACKAGE, krsSH_EXCEPTION, krsSH_NUMBER, krsSH_STRING);

  kasMETAFLAG: array[TeMetaFlag] of String =
    ('UpperCase', 'LowerCase', 'Local', 'Temporary');

  kasPARTTYPE: array[TePartType] of String =
    (ksEMPTY, 'column', 'schema', 'sequence', 'snapshot', 'table', 'view', 'alias', 'synonym', 'prefix', 'where', 'order by', 'index');

  kesTABLESET: TePartTypeSet =
    [epTable, epSynonym, epView, epSnapShot];

  kesCOLUMNSET: TePartTypeSet =
    [epColumn];

  kasPARTSIDE: array[TePartSide] of String =
    (ksEMPTY, krsXML_RHS, krsXML_LHS);

  krsCYPHER1            = 'NTYZPJLEDWXUBSIQRCOAVFGHKM';
  krsCYPHER2            = 'SIOWXPZJLUBDREQTNCAKMYVFGH';
  krsCYPHER3            = 'IVFGHAPSNCREZJOWXLYKMQUBDT';
  krsCYPHER4            = 'QREZJVFHYKIGLUWXNCAPSODTBM';
  kiA                   = 26;
  kiA0                  = 1;
  kiA1                  = kiA0 * kiA;
  kiA2                  = kiA1 * kiA;
  kiA3                  = kiA2 * kiA;
  kiA4                  = kiA3 * kiA;
  kiUSERCODEOFFSET      = 10000;

  kasLICENSELEVEL: array[TeLicenseLevel] of string =
    (ksEMPTY, 'Trial License', 'Beta License', 'Professional License');

  kiaSCHEMAENUM: array[0..41] of SchemaEnum =
    (adSchemaProviderSpecific, adSchemaAsserts, adSchemaCatalogs, adSchemaCharacterSets, adSchemaCollations,
     adSchemaColumns, adSchemaCheckConstraints, adSchemaConstraintColumnUsage, adSchemaConstraintTableUsage,
     adSchemaKeyColumnUsage, adSchemaReferentialContraints, adSchemaReferentialConstraints, adSchemaTableConstraints,
     adSchemaColumnsDomainUsage, adSchemaIndexes, adSchemaColumnPrivileges, adSchemaTablePrivileges,
     adSchemaUsagePrivileges, adSchemaProcedures, adSchemaSchemata, adSchemaSQLLanguages, adSchemaStatistics,
     adSchemaTables, adSchemaTranslations, adSchemaProviderTypes, adSchemaViews, adSchemaViewColumnUsage,
     adSchemaViewTableUsage, adSchemaProcedureParameters, adSchemaForeignKeys, adSchemaPrimaryKeys,
     adSchemaProcedureColumns, adSchemaDBInfoKeywords, adSchemaDBInfoLiterals, adSchemaCubes, adSchemaDimensions,
     adSchemaHierarchies, adSchemaLevels, adSchemaMeasures, adSchemaProperties, adSchemaMembers, adSchemaTrustees);

  kasSCHEMAENUM: array[0..41] of String =
    ('adSchemaProviderSpecific',           // -1
     'adSchemaAsserts',                    // 00
     'adSchemaCatalogs',                   // 01
     'adSchemaCharacterSets',              // 02
     'adSchemaCollations',                 // 03
     'adSchemaColumns',                    // 04
     'adSchemaCheckConstraints',           // 05
     'adSchemaConstraintColumnUsage',      // 06
     'adSchemaConstraintTableUsage',       // 07
     'adSchemaKeyColumnUsage',             // 08
     'adSchemaReferentialContraints',      // 09
     'adSchemaReferentialConstraints',     // 10
     'adSchemaTableConstraints',           // 11
     'adSchemaColumnsDomainUsage',         // 12
     'adSchemaIndexes',                    // 13
     'adSchemaColumnPrivileges',           // 14
     'adSchemaTablePrivileges',            // 15
     'adSchemaUsagePrivileges',            // 16
     'adSchemaProcedures',                 // 17
     'adSchemaSchemata',                   // 18
     'adSchemaSQLLanguages',               // 19
     'adSchemaStatistics',                 // 20
     'adSchemaTables',                     // 21
     'adSchemaTranslations',               // 22
     'adSchemaProviderTypes',              // 23
     'adSchemaViews',                      // 24
     'adSchemaViewColumnUsage',            // 25
     'adSchemaViewTableUsage',             // 26
     'adSchemaProcedureParameters',        // 27
     'adSchemaForeignKeys',                // 28
     'adSchemaPrimaryKeys',                // 29
     'adSchemaProcedureColumns',           // 30
     'adSchemaDBInfoKeywords',             // 31
     'adSchemaDBInfoLiterals',             // 32
     'adSchemaCubes',                      // 33
     'adSchemaDimensions',                 // 34
     'adSchemaHierarchies',                // 35
     'adSchemaLevels',                     // 36
     'adSchemaMeasures',                   // 37
     'adSchemaProperties',                 // 38
     'adSchemaMembers',                    // 39
     'adSchemaTrustees');                  // 40

  kasCONDITIONOPERATORS: array[TeConditionOperator] of String =
    ('', '=', '<>', '!=', '>', '>=', '!>', '<', '<=',
     '!<', 'like', 'not like', 'in', 'not in', 'is null', 'is not null');
  kaeRHSREQUIREMENT: array[TeConditionOperator] of TeOperatorRHS =
    (eorNone, eorBoth, eorBoth, eorBoth, eorBoth, eorBoth, eorBoth, eorBoth, eorBoth,
     eorBoth, eorValue, eorValue, eorValue, eorValue, eorNone, eorNone);

  kasFORMSET: array[TeForm] of String =
    ('&Schema Explorer', 'S&QL Query Console', 'Query &Builder', 'S&ession Manager', 'S&tored Queries', '&Database Spy', 'Online Help');
  kacSHORTCUTS: array[TeForm] of char =
    ('1', '2', '3', '4', '5', '6', '7');
  kasSTATE: array[TeState] of string =
    (ksEMPTY, krsFROMDB, krsMODIFIED, krsADDED, krsDELETED);

  kasFEATURESUPPORT: array[TeFeatureSupport] of String =
    (krsTRANSACTION, krsASYNCHRONOUS);

  kasICON: array[TeIcon] of String =
    (ksEMPTY, 'USER', 'TABLE', 'INDEX', 'VIEW', 'TRIGGER', 'PROCEDURE', 'FUNCTION', 'TYPE',
     'TABLESPACE', 'QUOTA', 'SEQUENCE', 'XMLSCHEMA', 'SYNONYM', 'DATABASELINK', 'PACKAGE',
     'GRANT', 'CLUSTER', 'LIBRARY', 'DIRECTORY', 'CONTEXT', 'DIMENSION', 'OUTLINE',
     'INDEXTYPE', 'OPERATOR', 'ROLE', 'PROFILE', 'VARIABLE', 'USER', 'SESSION', 'RECYCLEBIN',
     'MATERIALIZED_VIEW', 'ROUTINE');

  kasFETCHTYPE: array[TeFetchType] of String =
    ('GetRows', 'MoveNext');

  function  StringToType(Value: String): TeType;
  function  IsDelimitor(cparChar: Char): boolean;
  function  IsNumber(sparValue: string): boolean;
  function  IsReal(Value: String): boolean;
  function  VariantToBool(value: variant): boolean;
  function  StreamToString(Value: TStream; eCharSize: TeCharSize): String;
  function  ItemCount(value, separator: string): longint;
  function  Item(value, separator: string; index: longint): string;
  procedure StringToRTF(value: string; parObject: TCustomMemo; bparWrap: boolean);
  function  XMLtoRTF(value: String): string;
  function  RepeatStr(Occurences: longint; Value: String): String;
  function  _TextToXML(value: String): string;
  function  TextToXML(value: String): string;
  function  XMLToText(value: String): string;
  function  XMLFileType(sFileName: String): TeXMLFileType;
  function  StringToRuleErrorLevel(Value: String): TeRuleErrorLevel;
  function  Pad(Value: String; ilength: longint): String;
  function  GetItem(source, value: String): String;
  function  NoLineReturn(Value: String): String;
  function  TextToText(value: String): string;
  function  ParagraphToText(value: String): string;
  function  GetApplicationVersion: String;
  function  TextToRTF(value: String): string;
  function  HasS(value: longint): String; overload;
  function  HasS(value: longint; parPlural: String): String; overload;
  function  ShellOpenFile(value: String): boolean;
  function  FileToString(value: String; eCharSize: TeCharSize): String;
  function  StringToFile(sFileName: String; value: String; eCharSize: TeCharSize): boolean;
  function  StringToMetaFlags(value: String): TeMetaFlagSet;
  function  MetaFlagsToString(value: TeMetaFlagSet): String;
  function  StringToSchemaEnum(value: String): SchemaEnum;
  function  VariantTypeToDatatype(value: TVarType): TeDatatype;
  function  VarDataSize(const Value: OleVariant): Integer;
  function  GetFilePath(value: String): String; overload;
  function  GetFilePath(value: String; bparDrive: boolean): String; overload;
  function  GetFileName(value: String): String;
  function  GetNoDrive(value: String): String;
  function  GetLine(value: String; LineNumber: longint): String;
  function  StringToPartType(value: String): TePartType;
  function  GetXMLAttribute(parNode: OLEVariant; Value: String): String;
  function  GetXMLValue(ParentNode: OLEVariant): String; overload;
  function  GetXMLValue(ParentNode: OLEVariant; value: String): String; overload;
  function  PromptDataSource(ParentHandle: THandle; InitialString: WideString): WideString;
  function  GetXMLList(lst: TObject): boolean;
  function  StringToItemOptions(Value: String): TeItemOptionSet;
  function  ItemOptionsToString(Value: TeItemOptionSet): String;
  function  DeliveryTypeSetToString(value: TeDeliveryTypeSet): String;
  function  StringToDeliveryTypeSet(value: String): TeDeliveryTypeSet;
  function  InitCap(Value: String): String;
  function  StringToState(value: string): TeState;
  function  DataToHex(const X: String): string;
  function  HexToData(const value: String): string;
  function  GetKey: Int64;
  procedure LogToFile(sFileName: String; sValue: String);
  function  StringToIcon(value: String): TeIcon;
  function  FixCRLF(value: String): String;
  function  LoadTextResource(value: String): String;
  function  nvl2(value: boolean; A, B: String): String;
  function  GetTempFile(Extension: string): string;
  function  WordByIndex(value: String; Index: longint; var NextChar: longint): String;
  function  StringToConnectionMode(value: String): TeConnectionMode;
  function  GetCPUID: String;
  function  GetMACID: string;
  procedure SetBrowserContent(webBrowser: TWebBrowser; value: string); overload;
  function  xmlPart(xml, value: String): String;
  function  GetWinVersion: TWinVersion;
  function  ReadRegistry(eparRootKey: HKEY; sparRegistryPath, sparSection, sparEntry, sparDefault: String): String;
  procedure WriteRegistry(eparRootKey: HKEY; sparRegistryPath, sparSection, sparEntry, sparValue: String);
  function  ReadMADCVersion: String;
  function  NullIf(value: boolean; A, B: String): String;
  function  TokenTypeSetToString(value: TeTokenTypeSet): String;
  function  StringToFetchType(value: String): TeFetchType;
  function  Unquote(value: String): String;
  function  VarToItems(const value: OLEVariant; ItemDelimiter: char): String;
  function  CompareVersion(value1, value2: String): longint;
  function  ColorToHex(const value: TColor): string;
  function  StripHTMLTags(value: String): String;

  function  ADOType2ShortType(value: DataTypeEnum; Precision, NumericScale: longint): TeDatatype;
  function  FieldToInteger(const objField: Field20): longint;
  function  FieldToString(const objField: Field20): String;
  function  FieldToDate(const objField: Field20): TDateTime;
  function  FieldToReal(const objField: Field20): double;
  function  FieldToBoolean(const objField: Field20): boolean;
  function  FieldToItems(const objField: Field20; ItemDelimiter: char): String;
  function  FieldIsNull(const objField: Field20): boolean;
  function  HasField(objRS: RecordSet; FieldName: String): boolean;

implementation

uses
  comObj,
  daObjectLib,
  NB30,
  Math,
  MSHTML,
  Menus,
  Forms,
  Variants,
  Registry,
  sysUtils,
  strUtils,
  ActiveX, // CLSCTX_INPROC_SERVER
  OLEDB, // IDataInitialize, ...etc
  ExtActns,
  daStreamLib,
  BlockCiphers,
  WinInet,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdFTP,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, idFTPCommon;

// Tools
//   StringToType
//
function StringToType(Value: String): TeType;
var
  i: TeType;
begin
  result := enUndefined;
  for i := low(TeType) to high(TeType) do
    if AnsiCompareText(kasTYPE[i], value) = 0 then
    begin
      result := i;
      break;
    end;
end;

// IsDelimitor
//   Return TRUE if character is a regular token delimitor.
function IsDelimitor(cparChar: Char): boolean;
begin
  result := pos(cparChar, ' (),.:;=[]{}=*/+-<>?^' + kcDBLQUOTE + kcPARAGRAPH) > 0
end;

// Tools
//   IsNumber
//   Return TRUE if String is a number
function IsNumber(sparValue: string): boolean;
var
  i: integer;
begin
  result := sparValue <> ksEMPTY;
  for i := 1 to length(sparValue) do
    result := result and ( CharInSet(sparValue[i], ['0' .. '9']) or
                           ( (i = 1) and (sparValue[i] = '-') and (length(sparValue) > 1) ) or
                           ( (i = 2) and (AnsiCompareText(sparValue[i], 'x') = 0) and (length(sparValue) > 2) ) 
                         );
end;

// Tools
//   IsReal
//   Return TRUE if String is a Real point variable
{$HINTS OFF}
function IsReal(Value: String): boolean;
var
  i: integer;
  d: double;
begin
  result := Value <> '.';
  if result then
    for i := 1 to length(Value) do
      result := result and CharInSet(Value[i], ['0' .. '9', '.']);
  if result then
  begin
    val(Value, d, i);
    result := i = 0;
  end;
end;
{$HINTS ON}

// Tool
//   VariantToBool
//
function VariantToBool(value: variant): boolean;
begin
  result := FALSE;
  case VarType(value) of
    varEmpty, varNull:
      result := FALSE;
    varSmallint, varInteger, varSingle, varDouble, varCurrency:
      result := value <> 0;
    varDate:
      result := longint(result) <> 0;
    varBoolean:
      result := value;
    varString:
      if (AnsiCompareText(VarToStr(value), krsTRUE) = 0) or (AnsiCompareText(VarToStr(value), krsFALSE) = 0) then
        result := AnsiCompareText(VarToStr(value), krsTRUE) = 0
      else if (AnsiCompareText(VarToStr(value), krsYES) = 0) or (AnsiCompareText(VarToStr(value), krsNO) = 0) then
        result := AnsiCompareText(VarToStr(value), krsYES) = 0
      else
        result := VarToStr(value) <> ksEMPTY;
  end;
end;

// Tools
//   StreamToString
//
function StreamToString(Value: TStream; eCharSize: TeCharSize): String;
var
  s: AnsiString;
begin
  value.Position := 0;
  case eCharSize of
    ecsAnsi:
    begin
      SetLength(s, value.Size);
      value.ReadBuffer(Pointer(s)^, value.Size);
      result := string(s);
    end;
    ecsUnicode:
    begin
      SetLength(result, value.Size);
      value.ReadBuffer(Pointer(result)^, value.Size);
    end;
  end;
end;

// Tool
//   ItemCount
//
function ItemCount(value, separator: string): longint;
var
  i: longint;
begin
  result := 0;
  if value <> ksEMPTY then
    result := result + 1;
  for i := 1 to length(value) do
    if value[i] = separator then
      result := result + 1;
end;

// Tool
//   Item
//
function Item(value, separator: string; index: longint): string;
var
  L, i: longint;
begin
  result := ksEMPTY;
  if value <> ksEMPTY then
  begin
    L := 1;
    if index > 0 then
      while (index > 0) and (L < length(value)) do
      begin
        if value[L] = separator then
          dec(index);
        inc(L);
      end;
    i := pos(separator, system.copy(value, L, length(value)));
    if i = 0 then
      i := length(value) + 1;
    result := system.copy(value, L, i - 1);
  end;
end;

// Tool
//   StringToRTF
//
procedure StringToRTF(value: string; parObject: TCustomMemo; bparWrap: boolean);
var
  strm: TMemoryStream;
begin
  try
    parObject.Lines.BeginUpdate;
    parObject.Lines.Clear;
    strm := nil;
    if value <> ksEMPTY then
    try
      strm := TMemoryStream.Create;
      strm.write(value[1], length(value));
      strm.Position := 0;
      try
        parObject.Lines.LoadFromStream(strm);
      except
        //
      end;
    finally
      strm.free;
    end;
    parObject.Lines.EndUpdate;
    if ((parObject is TRichEdit) and ((parObject as TRichEdit).WordWrap <> bparWrap)) then
    begin
      (parObject as TRichEdit).WordWrap := bparWrap;
      parObject.Refresh;
    end
    else if ((parObject is TMemo) and ((parObject as TMemo).WordWrap <> bparWrap)) then
    begin
      (parObject as TMemo).WordWrap := bparWrap;
      parObject.Refresh;
    end;
  except
    on E: Exception do
      Application.MessageBox(PChar(E.Message), krsEXCEPTION, MB_OK + MB_ICONEXCLAMATION);
  end;
end;

// Tool
//   TextToText
//
function TextToText(value: String): string;
begin
  // pre-sanitize
  result := AnsiReplaceStr(trim(value), ksCR, '\n');
  result := AnsiReplaceStr(result, #$A, ksCR);
  result := AnsiReplaceStr(result, '\n', ksCR);
  result := AnsiReplaceStr(result, '\t', '  ');
  result := AnsiReplaceStr(result, #$9, '  ');
  result := AnsiReplaceStr(result, '\p', ksCR + kcPARAGRAPH);
end;

// Tool
//   ParagraphToText
//
function ParagraphToText(value: String): string;
begin
  result := value; // AnsiReplaceStr(value, kcPARAGRAPH, ';');
end;

// Tool
//   XMLtoRTF
//   Transform a flat XML string into an indented RTF output....!
//   01-29-2001 / Regis
//
function XMLtoRTF(value: String): string;
var
  i, k, j, n: longint;
  s: String;
begin
  result := value;
  //
  // A. Create indentation
  i := 1;
  k := 0;
  while TRUE do
  begin
    n := pos('<', system.copy(result, i, length(result) - i + 1));
    // Are we done..?
    if n = 0 then
      break;
    //
    if (i + n - 2 > 0) and (i + n - 2 <= length(result)) and (result[i + n] = '\') and (result[i + n - 2] = '>') then
    begin
      dec(k);
      s := ksPAR + RepeatStr(k, '  ');
      insert(s, result, i + n - 1);
      i := i + length(s);
    end

    else if (i + n - 2 > 0) and (i + n - 2 <= length(result)) and (result[i + n - 2] = '>') then
    begin
      j := i + n - 2;
      while (j > 0) and (result[j] <> '\') and (result[j] <> '<') do
        dec(j);
      if result[j] = '<' then
        inc(k);
      s := ksPAR + RepeatStr(k, '  ');
      insert(s, result, i + n - 1);
      i := i + length(s);
    end;

    i := i + n;
  end;
end;

// Tools
//   RepeatStr
//
function RepeatStr(Occurences: longint; Value: String): String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to Occurences - 1 do
    result := result + Value;
end;

// Tools
//   _TextToXML
//
function _TextToXML(value: String): string;
begin
  result := value; // TextToText(value);
  result := AnsiReplaceStr(result, '&', '&amp;');
  result := AnsiReplaceStr(result, '<', '&lt;');
  result := AnsiReplaceStr(result, '>', '&gt;');
end;

// Tools
//   TextToXML
//
function TextToXML(value: String): string;
var
  i, L: longint;
begin
  result := _TextToXML(value);
  // Characters > 127
  result := AnsiReplaceStr(result, kcCR, krsPLACEHOLDER_CR);
  result := AnsiReplaceStr(result, kcLF, krsPLACEHOLDER_LF);
  i := 1;
  while (i <= length(result)) do
  begin
    L := ord(result[i]);
    if (L >= 128) or (L < 32) then
    begin
      system.Delete(result, i, 1);
      system.Insert(format('&#%d;', [L]), result, i);
    end;
    inc(i);
  end;
end;

// Tools
//   XMLToText
//
function XMLToText(value: String): string;
var
  L, n: longint;
  s: String;
begin
  result := value;
  result := AnsiReplaceStr(result, '&amp;', '&');
  result := AnsiReplaceStr(result, '&lt;',  '<');
  result := AnsiReplaceStr(result, '&gt;',  '>');
  // &# Characters
  repeat
    L := pos('&#', result);
    if L > 0 then
    begin
      n := L + 2;
      while (n <= length(result)) and (result[n] <> ';') do
        inc(n);
      s := system.copy(result, L + 2, n - (L + 2));
      system.Delete(result, L, n - L + 1);
      system.Insert(chr(strtointdef(s, 32)), result, L);
    end;
  until L = 0;
  // Characters > 127
  result := AnsiReplaceStr(result, krsPLACEHOLDER_CR, kcCR);
  result := AnsiReplaceStr(result, krsPLACEHOLDER_LF, kcLF);
end;

(*
// Tools
//   TextToHTML
//
function TextToHTML(value: String): string;
begin
  result := value;
  //result := AnsiReplaceStr(result, ksCR, '<br>');
  result := AnsiReplaceStr(result, '\n', ksCR);
  result := AnsiReplaceStr(result, '\p', kcPARAGRAPH + ksCR);
end;
*)

// Tools
//   XMLFileType
//
function XMLFileType(sFileName: String): TeXMLFileType;
var
  p, xP: OLEVariant;
begin
  result := xftUndefined;
  try
    CoInitialize(nil);
    try
      p := CreateOLEObject('Microsoft.XMLDOM');
      if p.Load(sFileName) then
      begin
        if (p.childNodes.length = 1) and (p.childNodes.item[0].nodeName = krsDATA) then
          result := xftData
        else
          result := xftMetaData
      end
      else
      begin
        xP := p.parseError;
        if xP.errorCode <> 0 then
          Application.MessageBox(
            PChar(sFileName + ksCR +
                  'XML Meta Document failed to load.' + ksCR +
                  Format('Line %s, Column %s (File Pos. %s), Error# %s: %s', [VarToStr(xP.Line), VarToStr(xP.linepos), VarToStr(xP.filepos), VarToStr(xP.errorCode), xP.reason]) + ksCR +
                  trim(xP.srcText)
                 ),
            'Abort',
            MB_OK + MB_ICONSTOP);
      end;
    finally
      p := unassigned;
    end;
  finally
    CoUninitialize;
  end;
end;

// Tools
//   StringToRuleErrorLevel
//
function StringToRuleErrorLevel(Value: String): TeRuleErrorLevel;
var
  i: TeRuleErrorLevel;
begin
  result := relNone;
  for i := low(TeRuleErrorLevel) to high(TeRuleErrorLevel) do
    if AnsiCompareText(kasRULEERRORLEVEL[i], value) = 0 then
    begin
      result := i;
      break;
    end;
end;

// Tools
//   Pad
//
function Pad(Value: String; ilength: longint): String;
begin
  result := system.copy(value, 1, iLength);
  while length(result) < iLength do
    result := result + ' ';
end;

// Tools
//   GetItem
//
function GetItem(source, value: String): String;
var
  i: longint;
  s: String;
begin
  result := ksEMPTY;
  for i := 0 to ItemCount(source, ';') - 1 do
  begin
    s := Item(source, ';', i);
    if AnsiCompareText(trim(Item(s, '=', 0)), trim(value)) = 0 then
    begin
      result := trim(Item(s, '=', 1));
      break;
    end;
  end;
end;

// Tools
//   NoLineReturn
//
function NoLineReturn(Value: String): String;
var
  i: longint;
begin
  result := Value;
  for i := 1 to length(result) do
    if ord(result[i]) < 32 then
      result[i] := ' ';
end;

// Tools
//   GetApplicationVersion
//
function GetApplicationVersion: String;
var
  p: PChar;
  v: pointer; // PVSFixedFileInfo;
  tv: TVSFixedFileInfo;
  L: UINT;
const
  kiBUFFERSIZE = 1024;
  ksVERSIONSUBBLOCK = '\\';
  ksAPPNAMESUBBLOCK = '\\StringFileInfo\\040904E4\\FileDescription';
begin
  result := ksEMPTY;
  p := nil;
  try
    p := strAlloc(kiBUFFERSIZE);
    if GetFileVersionInfo(PWideChar(Application.ExeName), 0,  kiBUFFERSIZE, p) then
    begin
      VerQueryValue(p, PWideChar(ksVERSIONSUBBLOCK), v, L);
      tv := PVSFixedFileInfo(v)^;
      result := Format('%d.%d.%d.%d', [(tv.dwFileVersionMS and $FFFF0000) shr 16, tv.dwFileVersionMS and $FFFF, (tv.dwFileVersionLS and $FFFF0000) shr 16, tv.dwFileVersionLS and $FFFF]);
    end;
  finally
    strDispose(p);
  end;
end;

// Tools
//   ADOType2ShortType
//
function ADOType2ShortType(value: DataTypeEnum; Precision, NumericScale: longint): TeDatatype;
begin
  result := etUndefined;
  case value of
    adEmpty, adError, adUserDefined, adVariant, adIDispatch, adIUnknown, adGUID,
    adChapter, adFileTime, adPropVariant, adArray:
      result := etUndefined;
    adTinyInt, adSmallInt, adInteger, adBigInt, adUnsignedTinyInt, adUnsignedSmallInt,
    adUnsignedInt, adUnsignedBigInt, adVarNumeric:
      result := etInteger;
    adSingle, adDouble, adCurrency:
      result := etFloat;
    adDecimal, adNumeric:
      if (Precision <= 10) and (NumericScale = 0) then
        result := etInteger
      else
        result := etFloat;
    adBoolean:
      result := etBoolean;
    adDate, adDBDate, adDBTime, adDBTimeStamp:
      result := etDate;
    adBSTR, adChar, adVarChar:
      result := etAnsiString;
    adLongVarChar, adWChar, adVarWChar, adLongVarWChar:
      result := etLongString;
    adBinary, adVarBinary, adLongVarBinary:
      result := etBinary;
  end;
end;

// Tools
//   VariantTypeToDatatype
//
function VariantTypeToDatatype(value: TVarType): TeDatatype;
begin
  result := etUndefined;
  case value of
    varEmpty, varNull, varAny, varDispatch, varError, varUnknown:
      result := etUndefined;
    varSmallint, varInteger, varShortInt, varWord, varLongWord, varInt64, varByte:
      result := etInteger;
    varSingle, varDouble, varCurrency:
      result := etFloat;
    varDate:
      result := etDate;
    varVariant, varOleStr, varStrArg, varString:
      result := etAnsiString;
    varBoolean:
      result := etBoolean;
  end;
end;

// Tools
//   FieldToInteger
//
function FieldToInteger(const objField: Field20): longint;
begin
  if VarIsNull(objField.Value) then
    result := 0
  else
    result := longint(objField.Value);
end;

// Tools
//   FieldToString
//
function FieldToString(const objField: Field20): String;
const
  kiCHUNKSIZE = 1000;
var
  s: String;
  L, i: longint;
begin
  result := ksEMPTY;
  if objField.Type_ = adVarBinary then
  begin
    for i := 0 to objField.ActualSize - 1 do
    begin
      L := objField.Value[i];
      if L >= 32 then
        result := result + chr(L)
      else
        result := result + '~';
    end;
  end
  else if (objField.Attributes and adFldLong <> 0) then
  try
    s := ksEMPTY;
    L := objField.ActualSize;
    if L > 0 then
    repeat
      s := VarToStr(objField.GetChunk(min(L, kiCHUNKSIZE)));
      result := result + s;
      L := L - min(L, kiCHUNKSIZE);
    until L <= 0;
  except
    //
  end
  else if not VarIsNull(objField.Value) then
    result := VarToStr(objField.Value);
end;

// Tools
//   FieldToItems
//
function FieldToItems(const objField: Field20; ItemDelimiter: char): String;
const
  kiCHUNKSIZE = 1000;
var
  s: String;
  L, j, k: longint;
begin
  result := ksEMPTY;
  if objField.Attributes and adFldLong <> 0 then
  try
    s := ksEMPTY;
    L := objField.ActualSize;
    if L > 0 then
    repeat
      s := VarToStr(objField.GetChunk(min(L, kiCHUNKSIZE)));
      result := result + s;
      L := L - min(L, kiCHUNKSIZE);
    until L <= 0;
  except
    //
  end
  else if not VarIsNull(objField.Value) then
  begin
    if objField.Type_ <> adVarBinary then
      result := VarToStr(objField.Value)
    else for j := VarArrayLowBound(objField.Value, 1) to VarArrayHighBound(objField.Value, 1) do
    begin
      k := StrtoIntDef(VarToStr(objField.Value[j]), 32); // Space if not recognized.
      if k = 0 then
        k := ord(ItemDelimiter);
      result := result + chr(k);
    end;
  end;
end;

// Tools
//   FieldToDate
//
function FieldToDate(const objField: Field20): TDateTime;
begin
  if VarIsNull(objField.Value) then
    result := 0.0
  else
    result := TDateTime(objField.Value);
end;

// Tools
//   FieldToReal
//
function FieldToReal(const objField: Field20): double;
begin
  if VarIsNull(objField.Value) then
    result := 0.0
  else
    result := double(objField.Value);
end;

// Tools
//   FieldToBoolean
//
function FieldToBoolean(const objField: Field20): boolean;
begin
  if VarIsNull(objField.Value) then
    result := FALSE
  else
    result := objField.Value;
end;

// Tools
//   FieldIsNull
//
function FieldIsNull(const objField: Field20): boolean;
begin
  result := VarIsNull(objField.Value);
end;

// Tool
//   TextToRTF
//
function TextToRTF(value: String): string;
begin
  // pre-sanitize
  result := AnsiReplaceStr(trim(value), '\n', '~');
  result := AnsiReplaceStr(result, '\p', '~');
  result := AnsiReplaceStr(result, ksCR, '~');
  result := AnsiReplaceStr(result, #$A, '~');
  result := AnsiReplaceStr(result, #$D, '~');
  result := AnsiReplaceStr(result, '~', '\par ');
  result := AnsiReplaceStr(result, '\t', '  ');
  result := AnsiReplaceStr(result, #$9, '  ');
end;

// Tool
//   HasS (1)
//
function HasS(value: longint; parPlural: String): String; overload;
begin
  result := ksEMPTY;
  if value > 1 then
    result := parPlural;
end;

// Tool
//   HasS (2)
//
function HasS(value: longint): String;
begin
  result := HasS(value, 's');
end;

// Tool
//   OpenFile
//
function ShellOpenFile(value: String): boolean;
var
  f: TCustomFileRun;
begin
  result := FALSE;
  f := nil;
  try
    f := TCustomFileRun.Create(nil);
    f.FileName := value;
    result := f.Execute;
  except
    f.free;
  end;
end;

// Tool
//   FileToString method
//
function FileToString(value: String; eCharSize: TeCharSize): String;
var
  strm: TMemoryStream;
begin
  result := ksEMPTY;
  strm := nil;
  if FileExists(value) then
  try
    strm := TMemoryStream.Create;
    strm.LoadFromFile(value);
    result := StreamToString(strm, eCharSize);
  finally
    strm.free;
  end;
end;

// Tool
//   StringToFile method
//
function StringToFile(sFileName: String; value: String; eCharSize: TeCharSize): boolean;
var
  strm: TMemoryStream;
  s: AnsiString;
begin
  result := FALSE;
  strm := nil;
  if (value <> ksEMPTY) and (sFileName <> ksEMPTY) then
  try
    strm := TMemoryStream.Create;
    try
      case eCharSize of
        ecsAnsi:
        begin
          s := AnsiString(value);
          strm.WriteBuffer(Pointer(s)^, Length(s));
          strm.Position := 0;
        end;
        ecsUnicode:
          strm.WriteBuffer(Pointer(value)^, Length(value));
      end;
      strm.SaveToFile(sFileName);
      result := TRUE;
    except
      //
    end;
  finally
    strm.free;
  end;
end;

// Tool
//   StringToMetaFlags method
//
function StringToMetaFlags(value: String): TeMetaFlagSet;
var
  i: longint;
  e: TeMetaFlag;
  s: String;
begin
  result := [];
  for i := 0 to ItemCount(value, ',') - 1 do
  begin
    s := trim(Item(value, ',', i));
    for e := low(TeMetaFlag) to high(TeMetaFlag) do
    if AnsiCompareText(s, kasMETAFLAG[e]) = 0 then
      result := result + [e];
  end;
end;

// Tool
//   MetaFlagsToString method
//
function MetaFlagsToString(value: TeMetaFlagSet): String;
var
  e: TeMetaFlag;
begin
  result := ksEMPTY;
  for e := low(TeMetaFlag) to high(TeMetaFlag) do
    if e in value then
    begin
      if result <> ksEMPTY then
        result := result + ', ';
      result := result + kasMETAFLAG[e];
    end;
end;

// Tool
//   MetaFlagsToString method
//
function  StringToSchemaEnum(value: String): SchemaEnum;
var
  i: longint;
begin
  result := 0;
  for i := low(kasSCHEMAENUM) to high(kasSCHEMAENUM) do
    if AnsiCompareText(kasSCHEMAENUM[i], value) = 0 then
    begin
      result := i;
      break;
    end;
end;

// Tools
//   VarDataSize
//
function VarDataSize(const Value: OleVariant): Integer;
begin
  if VarIsNull(Value) then
    Result := -1
  else if VarIsArray(Value) then
    Result := VarArrayHighBound(Value, 1) + 1
  else if TVarData(Value).VType = varOleStr then
    Result := Length(PWideString(@TVarData(Value).VOleStr)^)
  else
    Result := SizeOf(OleVariant);
end;

// Tools
//   GetFilePath (1)
//
function GetFilePath(value: String): String;
begin
  result := value;
  while (result <> ksEMPTY) and (result[length(result)] <> '\') do
    system.Delete(result, length(result), 1);
  if (result <> ksEMPTY) and (result[length(result)] <> '\') then
    result := result + '\';
end;

// Tools
//   GetFilePath (1)
//
function GetFilePath(value: String; bparDrive: boolean): String;
begin
  result := GetFilePath(value);
  if not bparDrive then
    result := GetNoDrive(result);
end;

// Tools
//   GetNoDrive
//
function GetNoDrive(value: String): String;
begin
  result := value;
  while (result <> ksEMPTY) and not CharInSet(result[1], ['\', '/']) do
    delete(result, 1, 1);
end;

// Tools
//   GetFileName
//
function GetFileName(value: String): String;
var
  L: longint;
begin
  result := value;
  repeat
    L := pos('\', result);
    if L <> 0 then
      delete(result, 1, L);
  until L = 0;
end;

// Tools
//   GetLine
//
function GetLine(value: String; LineNumber: longint): String;
var
  L, i, j: longint;
begin
  result := ksEMPTY;
  // Find Start
  L := 1;
  for i := 1 to length(Value) do
  begin
    if Value[i] = kcLF then
      inc(L);
    // Find End
    if (L = LineNumber) and (i < length(Value)) then
    begin
      for j := i + 1 to length(Value) do
        if (Value[j] = kcLF) or (value[j] = kcCR) or (j = length(value)) then
        begin
          result := system.copy(value, i, j - i + 1);
          break;
        end;
      break;
    end;
  end;
end;

// Tools
//   StringToPartType
//
function StringToPartType(value: String): TePartType;
var
  e: TePartType;
begin
  result := epUndefined;
  for e := low(kasPARTTYPE) to high(kasPARTTYPE) do
    if AnsiCompareText(kasPARTTYPE[e], value) = 0 then
    begin
      result := e;
      break;
    end;
end;

// Tool
//   GetXMLAttribute
//
function GetXMLAttribute(parNode: OLEVariant; Value: String): String;
var
  i: longint;
begin
  result := ksEMPTY;
  if (parNode.nodeType = ntNODE_ELEMENT) then
    for i := 0 to parNode.Attributes.length - 1 do
      if AnsiCompareText(parNode.Attributes.item[i].nodeName, Value) = 0 then
      begin
        result := parNode.Attributes.item[i].nodeValue;
        break;
      end
end;

// Tool
//   GetXMLValue (1)
//
function GetXMLValue(ParentNode: OLEVariant): String;
var
  i: longint;
begin
  result := ksEMPTY;
  if (ParentNode.nodeType = ntNODE_ELEMENT) then
    for i := 0 to ParentNode.childNodes.length - 1 do
      if (ParentNode.childNodes.item[i].nodeType = ntNODE_TEXT) or (ParentNode.childNodes.item[i].nodeType = ntNODE_CDATA_SECTION) then
        result := result + ParentNode.childNodes.item[i].nodeValue;
end;

// Tool
//   GetXMLValue (2)
//
function GetXMLValue(ParentNode: OLEVariant; value: String): String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 0 to ParentNode.childNodes.length - 1 do
    if AnsiCompareText(ParentNode.childNodes.item[i].nodeName, value) = 0 then
    begin
      result := GetXMLValue(ParentNode.childNodes.item[i]);
      break;
    end;
end;

// Tool
//   PromptDataSource
//
function PromptDataSource(ParentHandle: THandle; InitialString: WideString): WideString;
var
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: PWideChar;
begin
  Result := InitialString;
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  if InitialString <> '' then
    DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER, PWideChar(InitialString), IUnknown, DataSource);
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;
  if Succeeded(DBPrompt.PromptDataSource(nil, ParentHandle, DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown, DataSource)) then
  begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr);
    Result := InitStr;
  end;
end;

// Tool
//   GetXMLList
//
function GetXMLList(lst: TObject): boolean;
var
  sr: TSearchRec;
  p: TcObject;
  i: longint;
  f: TextFile;
  s, t: String;
  L: longint;
begin
  result := FALSE;
  if lst is TcCollection then
  begin
    (lst as TcCollection).Clear;
    SetCurrentDir(GetFilePath(Application.ExeName));
    if FindFirst('*.xml', faAnyFile, sr) = 0 then
    begin
      repeat
        try
          AssignFile(f, sr.Name);
          Reset(f);
          if not eof(f) then
          begin
            Readln(f, s);
            if s <> ksEMPTY then
            begin
              L := pos(krsRDBMS, s);
              if L > 0 then
              begin
                t := ksEMPTY;
                inc(L, length(krsRDBMS));
                while (L <= length(s)) and (s[L] <> '"') do
                  inc(L);
                inc(L);
                i := L;
                while (L <= length(s)) and (s[L] <> '"') do
                  inc(L);
                if (L <= length(s)) then
                  t := system.copy(s, i, L - i);
                if t <> ksEMPTY then
                begin
                  p := TcObject.Create(nil);
                  (lst as TcCollection).Add(p);
                  p.sName := GetFilePath(Application.ExeName) + sr.Name;
                  p.sValue := t;
                end;
              end;
            end;
          end;
        finally
          CloseFile(f);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    result := TRUE;
  end;
end;

// Tools
//   StringToItemOptions
//
function StringToItemOptions(Value: String): TeItemOptionSet;
var
  i: TeItemOption;
begin
  result := [];
  for i := low(TeItemOption) to high(TeItemOption) do
    if pos(Format('/%d/', [longint(i)]), value) <> 0 then
      result := result + [i];
end;

// Tools
//   ItemOptionsToString
//
function ItemOptionsToString(Value: TeItemOptionSet): String;
var
  i: TeItemOption;
begin
  result := ksEMPTY;
  for i := low(TeItemOption) to high(TeItemOption) do
    if i in value then
      result := result + Format('/%d', [longint(i)]);
  if result <> ksEMPTY then
    result := result + '/';
end;

// Tools
//   DeliveryTypeSetToString
//
function DeliveryTypeSetToString(value: TeDeliveryTypeSet): String;
var
  i: TeDeliveryType;
begin
  result := ksEMPTY;
  for i := low(TeDeliveryType) to high(TeDeliveryType) do
    if i in value then
      result := result + '/' + inttostr(longint(i));
  if result <> ksEMPTY then
    result := result + '/';
end;

// Tools
//   StringToDeliveryTypeSet
//
function StringToDeliveryTypeSet(value: String): TeDeliveryTypeSet;
var
  i: TeDeliveryType;
begin
  result := [];
  for i := low(TeDeliveryType) to high(TeDeliveryType) do
    if pos(Format('/%d/', [longint(i)]), value) <> 0 then
      result := result + [i];
end;

// Tools
//   InitCap
//
function InitCap(Value: String): String;
var
  i: longint;
begin
  result := ksEMPTY;
  for i := 1 to length(value) do
  begin
    if (i = 1) or
       (
         (i > 1) and
         (pos(result[i - 1], ' -_!@#$%^&*()+=":;<>,.?/~`') > 0)
       ) then
      result := result + uppercase(value[i])
    else
      result := result + lowercase(value[i]);
  end;
end;

// Tools
//   StringToState
//
function StringToState(value: string): TeState;
var
  i: TeState;
begin
  result := edsUndefined;
  for i := low(TeState) to high(TeState) do
    if AnsiCompareText(kasSTATE[i], value) = 0 then
    begin
      result := i;
      break;
    end;
end;

// Tools
//   DataToHex
//
function DataToHex(const X: String): string;
const
  DigitToHex: string = '0123456789abcdef';
var
  j: Integer;
begin
  result := '';
  for j := 1 to length(x) do
    result := result + DigitToHex[ord(x[j]) div 16 + 1] + DigitToHex[ord(x[j]) mod 16 + 1];
end;

// Tools
//   ColorToHex
//
function ColorToHex(const value: TColor): string;
const
  DigitToHex: string = '0123456789abcdef';
begin
  result := //DigitToHex[((value and $FF000000) shr 24) div 16 + 1] + DigitToHex[(((value and $FF000000) shr 24) shr 8) mod 16 + 1] +
            DigitToHex[(value and $FF) div 16 + 1] + DigitToHex[(value and $FF) mod 16 + 1] +
            DigitToHex[((value and $FF00) shr 8) div 16 + 1] + DigitToHex[((value and $FF00) shr 8) mod 16 + 1] +
            DigitToHex[((value and $FF0000) shr 16) div 16 + 1] + DigitToHex[((value and $FF0000) shr 16) mod 16 + 1];
end;

// Tools
//   HexToData
//
function HexToData(const value: String): string;
const
  DigitToHex: string = '0123456789abcdef';
var
  j, i: Integer;
begin
  J := length(value) div 2;
  SetLength(Result, j);
  for i := 1 to j do
    result[i] := chr( ((pos(value[i * 2 - 1], DigitToHex) - 1) * 16) + pos(value[i * 2], DigitToHex) - 1);
end;

// Tools
//   GetKey
//
function GetKey: Int64;
begin
  with TDoubleDWORD(result) do
  begin
    R := $01234567;
    L := $89abcdef;
  end;
end;

// Tools
//   LogToFile
//
procedure LogToFile(sFileName: String; sValue: String);
var
  strm: TFileStream;
begin
  strm := nil;
  try
    if sValue <> ksEMPTY then
    try
      if FileExists(sFileName) then
        strm := TFileStream.Create(sFileName, fmShareDenyNone or fmOpenReadWrite)
      else
        strm := TFileStream.Create(sFileName, fmCreate or fmShareDenyNone);
      strm.Position := strm.Size;
      strm.write(sValue[1], length(sValue));
    except
      on E:Exception do
        Application.MessageBox(PChar(Format('Spooling failed: %s', [E.MEssage])), krsINFORMATION, MB_OK + MB_ICONEXCLAMATION);
    end;
  finally
    strm.Free;
  end;
end;

// Tools
//   StringToIcon
//
function StringToIcon(value: String): TeIcon;
var
  e: TeIcon;
begin
  result := eiUndefined;
  for e := low(TeIcon) to high(TeIcon) do
    if AnsiCompareText(kasICON[e], value) = 0 then
    begin
      result := e;
      break;
    end;
end;

// Tools
//   HasField
//
function HasField(objRS: RecordSet; FieldName: String): boolean;
var
  i: longint;
begin
  result := FALSE;
  if (objRS <> nil) and (objRS.Fields.Count > 0) then
    for i := 0 to objRS.Fields.Count - 1 do
      if SameText(FieldName, objRS.Fields[i].Name) then
      begin
        result := TRUE;
        break;
      end;
end;

// Tools
//   FixCRLF
//
function FixCRLF(value: String): String;
begin
  result := value;
end;

// Tools
//   LoadTextResource
//
function LoadTextResource(value: String): String;

  function GetResourceAsPointer(ResName: pchar; ResType: pchar; out Size: longword): pointer;
  var
    InfoBlock: HRSRC;
    GlobalMemoryBlock: HGLOBAL;
  begin
    InfoBlock := FindResource(hInstance, resname, restype);
    if InfoBlock = 0 then
      raise Exception.Create(SysErrorMessage(GetLastError));
    size := SizeofResource(hInstance, InfoBlock);
    if size = 0 then
      raise Exception.Create(SysErrorMessage(GetLastError));
    GlobalMemoryBlock := LoadResource(hInstance, InfoBlock);
    if GlobalMemoryBlock = 0 then
      raise Exception.Create(SysErrorMessage(GetLastError));
    Result := LockResource(GlobalMemoryBlock);
    if Result = nil then
      raise Exception.Create(SysErrorMessage(GetLastError));
  end;

var
  p: PAnsiChar;
  L: Longword;
  s: AnsiString;
begin
  s := ksEMPTY;
  try
    p := GetResourceAsPointer(PChar(value), 'TEXT', L);
    SetString(s, p, L);
  except
    //
  end;
  result := String(s);
end;

// Tool
//   GetHTMLText
//
function nvl2(value: boolean; A, B: String): String;
begin
  if value then
    result := A
  else
    result := B;
end;

// Tool
//   GetLongPathName
//   Source: http://www.delphi-fundgrube.de/faq03.htm
//   Author: Peter Haas
//
function GetLongPathName(APath: String):String;
var
  i: Integer;
  h: THandle;
  Data: TWin32FindData;
  IsBackSlash: Boolean;
begin
  APath := ExpandFileName(APath);
  i := Pos('\', APath);
  Result := system.Copy(APath, 1, i);
  system.Delete(APath, 1, i);
  repeat
    i := Pos('\', APath);
    IsBackSlash := i > 0;
    if Not IsBackSlash then
      i := Length(APath) + 1;
    h := FindFirstFile(PChar(Result + Copy(APath, 1, i-1)), Data);
    if h <> INVALID_HANDLE_VALUE then begin
      try
        Result := Result+Data.cFileName;
        if IsBackSlash then
          Result:=Result + '\';
      finally
        Windows.FindClose(h);
      end;
    end
    else begin
      Result := Result + APath;
      Exit;
    end;
    Delete(APath, 1, i);
  until Length(APath) = 0;
end;

// Tool
//   GetTempFile
//
function GetTempFile(Extension: string): string;
var
  p: PChar;
begin
  p:= nil;
  try
    p := strAlloc(MAX_PATH + 1);
    GetTempPath(MAX_PATH, p);
    GetTempFileName(p, '~', 0, p);
    result := GetLongPathName(String(p));
  finally
    strDispose(p);
  end;
end;

// Tools
//   WordByIndex
//
function WordByIndex(value: String; Index: longint; var NextChar: longint): String;
var
  strm: TcTokenStream;
begin
  result := ksEMPTY;
  NextChar := 0;
  strm := nil;
  if value <> ksEMPTY then
  try
    strm := TcTokenStream.Create;
    strm.AsValue := value;
    while not strm.EOS do
    begin
      if Index = 1 then
      begin
        result := strm.Token.Value;
        strm.Match(_WILDCARD);
        NextChar := strm.iTokenStart;
        break;
      end;
      strm.Match(_WILDCARD);
      dec(Index);
    end;
  finally
    strm.Free;
  end;
end;

// Tools
//   StringToConnectionMode
//
function StringToConnectionMode(value: String): TeConnectionMode;
begin
  result := ecmMDAC;
  if (value = ksEMPTY) or (AnsiCompareText(value, 'MDAC') = 0) then
    result := ecmMDAC
  else if (AnsiCompareText(value, 'Oracle') = 0) then
    result := ecmOracle;
end;

// Tool
//   GetCPUID
//
function GetCPUID: String;
const
  ID_BIT   = $200000;               // EFLAGS ID bit
type
  TCPUID   = array[1..4] of Longint;
  TVendor  = array [0..11] of ansichar;

  function IsCPUID_Available: Boolean; register;
  asm
    PUSHFD             {direct access to flags no possible, only via stack}
    POP     EAX        {flags to EAX}
    MOV     EDX,EAX    {save current flags}
    XOR     EAX,ID_BIT {not ID bit}
    PUSH    EAX        {onto stack}
    POPFD              {from stack to flags, with not ID bit}
    PUSHFD             {back to stack}
    POP     EAX        {get back to EAX}
    XOR     EAX,EDX    {check if ID bit affected}
    JZ      @exit      {no, CPUID not availavle}
    MOV     AL,True    {Result=True}
    @exit:
  end;

  function GetCPUID: TCPUID; assembler; register;
  asm
    PUSH    EBX        {Save affected register}
    PUSH    EDI
    MOV     EDI,EAX    {@Resukt}
    MOV     EAX,1
    DW      $A20F      {CPUID Command}
    STOSD              {CPUID[1]}
    MOV     EAX,EBX
    STOSD              {CPUID[2]}
    MOV     EAX,ECX
    STOSD              {CPUID[3]}
    MOV     EAX,EDX
    STOSD              {CPUID[4]}
    POP     EDI        {Restore registers}
    POP     EBX
  end;

  function GetCPUVendor: TVendor; assembler; register;
  asm
    PUSH    EBX        {Save affected register}
    PUSH    EDI
    MOV     EDI,EAX    {@Result (TVendor)}
    MOV     EAX,0
    DW      $A20F      {CPUID Command}
    MOV     EAX,EBX
    XCHG    EBX,ECX    {save ECX result}
    MOV     ECX,4
  @1:
    STOSB
    SHR     EAX,8
    LOOP    @1
    MOV     EAX,EDX
    MOV     ECX,4
  @2:
    STOSB
    SHR     EAX,8
    LOOP    @2
    MOV     EAX,EBX
    MOV     ECX,4
  @3:
    STOSB
    SHR     EAX,8
    LOOP    @3
    POP     EDI        {Restore registers}
    POP     EBX
  end;

var
  CPUID: TCPUID;
  I: Integer;
begin
  result := ksEMPTY;
  for I := Low(CPUID) to High(CPUID) do
    CPUID[I] := -1;
  if IsCPUID_Available then
  begin
    CPUID := GetCPUID;
    result := Format('%s %s%s%s%s%d%d%d%d', [string(GetCPUVendor), IntToHex(CPUID[1], 8), IntToHex(CPUID[2], 8), IntToHex(CPUID[3], 8), IntToHex(CPUID[4], 8), CPUID[1] shr 12 and 3, CPUID[1] shr 8 and $f, CPUID[1] shr 4 and $f, CPUID[1] and $f]);
  end;
end;

// Tool
//   GetMACID
//
function CoCreateGuid(var guid: TGUID): HResult; stdcall; far external 'ole32.dll';

function GetMACID: string;
var
  g: TGUID;
  i: Byte;
begin
  Result := ksEMPTY;
  CoCreateGUID(g);
  for i := 2 to 7 do
  begin
    if i > 2 then
      Result := Result + '-';
    Result := Result + IntToHex(g.D4[i], 2)
  end;
end;

// Tool
//   SetBrowserContent Method (1)
//
procedure SetBrowserContent(webBrowser: TWebBrowser; value: string);
var
  sl: TStringList;
  ms: TMemoryStream;
begin
  try
    WebBrowser.Silent := TRUE;
    WebBrowser.Navigate(WideString('about:blank'));
    while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
      Application.ProcessMessages;
    if Assigned(WebBrowser.Document) then
    begin
      sl := TStringList.Create;
      try
        ms := TMemoryStream.Create;
        try
          sl.Text := value;
          sl.SaveToStream(ms) ;
          ms.Seek(0, 0) ;
          (WebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms)) ;
        finally
          ms.Free;
        end;
      finally
        sl.Free;
      end;
    end;
  finally
  end;
end;

// Tool
//   xmlPart method
//
function xmlPart(xml, value: String): String;
var
  p, q: OLEVariant;
  i: longint;
begin
  result := ksEMPTY;
  try
    p := CreateOLEObject('Microsoft.XMLDOM');
    if p.LoadXML(xml) and (p.ChildNodes.length > 0) then
    begin
      q := p.ChildNodes.item[0];
      for i := 0 to q.ChildNodes.length - 1 do
        if AnsiCompareText(q.ChildNodes.Item[i].nodeName, value) = 0 then
        begin
          result := GetXMLValue(q.ChildNodes.Item[i]);
          break;
        end;
    end;
    p := unassigned;
  except
    on E: Exception do
      p := unassigned;
  end;
end;

// Tools
//   GetWinVersion
//
function GetWinVersion: TWinVersion;
var
  osVerInfo: TOSVersionInfo;
  majorVersion, minorVersion: Integer;
begin
  Result := wvUnknown;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo) ;
  if GetVersionEx(osVerInfo) then
  begin
    minorVersion := osVerInfo.dwMinorVersion;
    majorVersion := osVerInfo.dwMajorVersion;
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
      begin
        if majorVersion <= 4 then
          Result := wvWinNT
        else if (majorVersion = 5) and (minorVersion = 0) then
          Result := wvWin2000
        else if (majorVersion = 5) and (minorVersion = 1) then
          Result := wvWinXP
        else if (majorVersion = 6) then
          Result := wvWinVista;
      end;
      VER_PLATFORM_WIN32_WINDOWS:
      begin
        if (majorVersion = 4) and (minorVersion = 0) then
          Result := wvWin95
        else if (majorVersion = 4) and (minorVersion = 10) then
        begin
          if osVerInfo.szCSDVersion[1] = 'A' then
            Result := wvWin98SE
          else
            Result := wvWin98;
        end
        else if (majorVersion = 4) and (minorVersion = 90) then
          Result := wvWinME
        else
          Result := wvUnknown;
      end;
    end;
  end;
end;

// Tool
//   ReadRegistry
//
function ReadRegistry(eparRootKey: HKEY; sparRegistryPath, sparSection, sparEntry, sparDefault: String): String;
var
  cRegistry: TRegistry;
begin
  cRegistry := nil;
  try
    cRegistry := TRegistry.Create;
    cRegistry.RootKey := eparRootKey;
    if cRegistry.OpenKey(sparRegistryPath + sparSection, FALSE) then
      if cRegistry.ValueExists(sparEntry) then
      begin
        result := cRegistry.ReadString(sparEntry);
        cRegistry.CloseKey;
      end
      else
        result := sparDefault
    else
      result := sparDefault;
  finally
    cRegistry.Free;
  end;
end;

// Tool
//   WriteRegistry
//
procedure WriteRegistry(eparRootKey: HKEY; sparRegistryPath, sparSection, sparEntry, sparValue: String);
var
  cRegistry: TRegistry;
begin
  cRegistry := nil;
  try
    cRegistry := TRegistry.Create;
    cRegistry.RootKey := eparRootKey;
    if cRegistry.OpenKey(sparRegistryPath + sparSection, TRUE) then
    begin
      cRegistry.WriteString(sparEntry, sparValue);
      cRegistry.CloseKey;
    end;
  finally
    cRegistry.Free;
  end;
end;

// Tool
//   ReadMADCVersion
//
function ReadMADCVersion: String;
var
  cRegistry: TRegistry;
begin
  result := ksEMPTY;
  cRegistry := nil;
  try
    cRegistry := TRegistry.Create;
    try
      cRegistry.RootKey := HKEY_LOCAL_MACHINE;
      if cRegistry.OpenKey('SOFTWARE\Microsoft\DataAccess', FALSE) then
        if cRegistry.ValueExists('FullInstallVer') then
        begin
          result := cRegistry.ReadString('FullInstallVer');
          cRegistry.CloseKey;
        end;
    except
      //
    end;
  finally
    cRegistry.Free;
  end;
end;

// Tool
//   NullIf
//
function NullIf(value: boolean; A, B: String): String;
begin
  if value then result := A else result := B;
end;

// Tool
//   TokenTypeSetToString
//
function TokenTypeSetToString(value: TeTokenTypeSet): String;
var
  e: TeTokenType;
begin
  result := ksEMPTY;
  for e := low(TeTokenType) to high(TeTokenType) do
    if e in value then
    begin
      if result <> ksEMPTY then
        result := result + ', ';
      result := result + kasTOKENTYPE[e];
    end;
end;

// Tool
//   StringToFetchType
//
function StringToFetchType(value: String): TeFetchType;
var
  i: TeFetchType;
begin
  result := low(TeFetchType);
  for i := low(TeFetchType) to high(TeFetchType) do
    if AnsiCompareText(kasFETCHTYPE[i], value) = 0 then
    begin
      result := i;
      break;
    end;
end;

// Tool
//   Unquote
//
function Unquote(value: String): String;
begin
  result := trim(value);
  if (result <> ksEMPTY) and (result[1] = kcDBLQUOTE) and (result[length(result)] = kcDBLQUOTE) then
    result := system.copy(result, 2, length(result) - 2);
end;

// Tools
//   VarToItems
//
function VarToItems(const value: OLEVariant; ItemDelimiter: char): String;
const
  kiCHUNKSIZE = 1000;
var
  j, k: longint;
begin
  result := ksEMPTY;
  if not VarIsNull(Value) then
  begin
    if not VarIsType(value, adVarBinary) then
      result := VarToStr(Value)
    else for j := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
    begin
      k := StrtoIntDef(VarToStr(Value[j]), 32); // Space if not recognized.
      if k = 0 then
        k := ord(ItemDelimiter);
      result := result + chr(k);
    end;
  end;
end;

// Tool
//   CompareVersion
//
function CompareVersion(value1, value2: String): longint;
begin
  result := AnsiCompareText(Item(value1, '.', 0), Item(value2, '.', 0));
  if result = 0 then
  begin
    result := AnsiCompareText(Item(value1, '.', 1), Item(value2, '.', 1));
    if result = 0 then
    begin
      result := AnsiCompareText(Item(value1, '.', 2), Item(value2, '.', 2));
      if result = 0 then
        result := AnsiCompareText(Item(value1, '.', 3), Item(value2, '.', 3));
    end;
  end;
end;

// Tool
//   StripHTMLTags
//
function StripHTMLTags(value: String): String;
var
  L, N: longint;
  s: String;
begin
  result := value;
  repeat
    L := pos('<a', lowercase(result));
    if L = 0 then
      L := pos('</a', lowercase(result));
    if L > 0 then
    begin
      s := system.copy(result, L, length(result));
      N := pos('>', s);
      if N = 0 then
        N := length(s);
      delete(result, L, N);
    end;
  until L = 0;
end;

end.
