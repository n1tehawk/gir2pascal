unit girpascalwritertypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, girNameSpaces, girObjects, girTokens, contnrs;

type
  TgirOption = (goWantTest, goLinkDynamic, goSeperateConsts, goClasses, goObjects, goIncludeDeprecated);
  TgirOptions = set of TgirOption;
  TgirWriteEvent = procedure (Sender: TObject; AUnitName: AnsiString; AStream: TStringStream) of object;

  TPDeclaration = class
    function AsString: String; virtual; abstract;
  end;

  { TPDeclarationWithLines }

  TPDeclarationWithLines = class(TPDeclaration)
    Lines: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    function AsString: String; override;
  end;

  { TPDeclarationType }

  TPDeclarationType = class(TPDeclarationWithLines)
    function AsString: String; override;
  end;

  { TPDeclarationConst }

  TPDeclarationConst = class(TPDeclarationWithLines)
    function AsString: String; override;
  end;

  { TPDeclarationVar }

  TPDeclarationVar = class(TPDeclarationWithLines)
    function AsString: String; override;
  end;

  { TPDeclarationFunctions }

  TPDeclarationFunctions = class(TPDeclarationWithLines)
  private
    FDynamicFunctions: Boolean;
  public
    constructor Create(ADynamicFunctions: Boolean);
    function AsString: String; override;
  end;

  { TPUses }

  TPUses = class(TPDeclaration)
    Units: TStringList;
    constructor Create;
    destructor Destroy; override;
    function AsString: String; override;
  end;



  { TPDeclarationList }

  TPDeclarationList = class(TList)
  private
    function GetDeclarations(AIndex: Integer): TPDeclaration;
  public
    function AsString: String;
    property Declarations[AIndex: Integer]: TPDeclaration read GetDeclarations;
  end;

  { TPUnitPart }

  TPUnitPart = class
    FOwner: TObject;
    constructor Create(AOwner: TObject); virtual;
    function AsString: String; virtual ; abstract;
  end;

  { TPCommonSections }

  TPCommonSections = class(TPUnitPart)
  private
    FDeclarations: TPDeclarationList;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    property Declarations: TPDeclarationList read FDeclarations;
  end;
  { TPCodeText }

  TPCodeText = class(TPDeclarationWithLines)
  private
    function GetContent: String;
    procedure SetContent(AValue: String);
  public
    property Content: String read GetContent write SetContent;
  end;


  { TPInterface }

  TPInterface = class(TPCommonSections)
  private
    FConstSection: TPDeclarationConst;
    FFunctionSection: TPDeclarationFunctions;
    FUsesSection: TPUses;
  public
    constructor Create(AOwner: TObject; AUses: TPUses; ADynamicFunctions: Boolean);
    destructor Destroy; override;
    function AsString: String; override;
    property UsesSection: TPUses read FUsesSection;
    property ConstSection: TPDeclarationConst read FConstSection;
    property FunctionSection: TPDeclarationFunctions read FFunctionSection;
  end;

  { TPImplementation }

  TPImplementation = class(TPCommonSections)
    function AsString: String; override;
  end;

  { TPInitialize }

  TPInitialize = class(TPCommonSections)
    function AsString: String; override;
  end;

  { TPFinialization }

  TPFinialization = class(TPCommonSections)
    function AsString: String; override;
  end;

  TPascalUnit = class;
  TPascalUnitType = (utSimple, utConsts, utTypes, utFunctions, utObjects, utClasses);

  TPascalUnitTypes = set of TPascalUnitType;
const
  PascalUnitTypeAll = [utSimple, utConsts, utTypes, utFunctions, utObjects, utClasses];
  PascalUnitTypeCommon = [utConsts, utTypes, utFunctions];
type
  { TPascalUnitGroup }

  TPascalUnitGroup = class
  private
    FSimpleUnit: Boolean;
    FOptions: TgirOptions;
    FNameSpace: TgirNamespace;
    FWriter: TObject;//girPascalWriter;
    FUnits: TFPList;
    //Units: array[TPascalUnitType] of TPascalUnit;
    function GetUnitForType(AType: TPascalUnitType): TPascalUnit;
  public
    constructor Create(AWriter: TObject{TgirPascalWriter}; ANameSpace: TgirNamespace; AOptions: TgirOptions);
    destructor Destroy; override;
    procedure GenerateUnits;
    property UnitForType[AType: TPascalUnitType]: TPascalUnit read GetUnitForType;
  end;


  { TPascalUnit }

  TPascalUnit = class
  private
    FDynamicLoadUnloadSection: TPCodeText;
    FDynamicEntryNames: TStringList;
    FGroup : TPascalUnitGroup;
    FOptions: TgirOptions;
    FFinalizeSection: TPFinialization;
    FImplementationSection: TPImplementation;
    FInitializeSection: TPInitialize;
    FInterfaceSection: TPInterface;
    FLibName: String;
    FUnitType: TPascalUnitTypes;
    FNameSpace: TgirNamespace;

    ProcessLevel: Integer; //used to know if to write forward definitions
    //FTestCFile: TStringStream;
    FTestPascalFile: TStringStream;
    FTestPascalBody: TStringList;
    function GetUnitFileName: String;
    function GetUnitName: String;
    function GetUnitPostfix: String;

    // functions to ensure the type is being written in the correct declaration
    function WantTypeSection: TPDeclarationType;
    function WantConstSection: TPDeclarationConst;
    function WantFunctionSection: TPDeclarationFunctions;
    // function WantVarSection: TPDeclarationVar;

    // to process main language types
    procedure HandleNativeType(AItem: TgirNativeTypeDef);
    procedure HandleAlias(AItem: TgirAlias);
    procedure HandleCallback(AItem: TgirCallback);
    procedure HandleEnum(AItem: TgirEnumeration; ADeclareType: Boolean = True);
    procedure HandleBitfield(AItem: TgirBitField);
    procedure HandleRecord(AItem: TgirRecord);
    procedure HandleOpaqueType(AItem: TgirFuzzyType);
    procedure HandleFunction(AItem: TgirFunction);
    procedure HandleObject(AItem: TgirObject; AObjectType: TGirToken);
    procedure HandleUnion(AItem: TgirUnion);

    procedure WriteForwardDefinition(AType: TGirBaseType);


    //functions to write reused parts of types
    procedure WriteWrapperForObject(ARoutineType, AObjectName, AObjectFunctionName: String; AParams:TgirParamList; AFunctionReturns: String; AFlatFunctionName: String; AWantSelf: Boolean);
    function WriteCallBack(AItem: TgirFunction; IsInObject: Boolean; AExistingUsedNames: TStringList = nil): String;
    procedure WriteFunctionTypeAndReturnType(AItem: TgirFunction; out AFunctionType, AFunctionReturnType: String);
    function WriteFunctionParams(AParams: TgirParamList; AArgs: PString = nil): String;
    function WriteFunction(AFunction: TgirFunction; AItem: TGirBaseType; AIsMethod: Boolean; AWantWrapperForObject: Boolean; AFunctionList: TStrings; AExistingUsedNames: TStringList = nil): String;
    function WriteParamAsString(AParam: TgirTypeParam; AIndex: Integer;  out ABitSizeSpecified: Boolean; AFirstParam: PString = nil; AExistingUsedNames: TStringList = nil): String;
    function WriteRecord(ARecord: TgirRecord; ABaseIndent: Integer = 0; AIsUnion: Boolean = False): String;
    function WriteUnion(AUnion: TgirUnion; ASkipRecordName: Boolean; ABaseIndent: Integer = 0): String;
    function ParenParams(const AParams: String; const AForceParens: Boolean = False): String;
    // methods for writing dynamic load code and libray names
    procedure WriteDynamicLoadUnloadProcs;
    function GetLibs: TStringList;


    // methods for dealing with type names
    function SanitizeName(AName: String; AExistingUsedNames: TStringList = nil): String;
    procedure WritePointerTypesForType(AItem: TGirBaseType; ATypeName: String; APointerLevel: Integer; ALines: TStrings);
    function TypeAsString(AType: TGirBaseType; APointerLevel: Integer; ACTypeAsBackup: String = ''): String;
    procedure AssembleUsedFieldNamesFromParent(const AParent: TgirClass; var AUsedNamesList: TStringList);

    procedure ResolveTypeTranslation(ABaseType: TGirBaseType);
    function MakePascalTypeFromCType(CName: String; PointerLevel: Integer = MaxInt; Trim_T_IfExists: Boolean =True): String;

    function EscapeSingleQuote(AString: String): String;

    procedure AddGLibSupportCode;

    procedure ProcessType(AType: TGirBaseType; AForceWrite: Boolean = False);
    procedure ResolveFuzzyTypes;
    procedure AddTestType(AGType: TgirGType);
  public
    constructor Create(AGroup: TPascalUnitGroup; ANameSpace: TgirNamespace; AOptions: TgirOptions; AUnitType: TPascalUnitTypes);
    destructor Destroy; override;
    procedure ProcessConsts(AList:TList); // of TgirBaseType descandants
    procedure ProcessTypes(AList:TFPHashObjectList); // of TgirBaseType descandants
    procedure ProcessFunctions(AList:TList);// of TgirFunction
    procedure GenerateUnit;
    function AsStream: TStringStream;
    procedure Finish;

    property InterfaceSection: TPInterface read FInterfaceSection;
    property ImplementationSection: TPImplementation read FImplementationSection;
    property DynamicLoadUnloadSection: TPCodeText read FDynamicLoadUnloadSection;
    property InitializeSection: TPInitialize read FInitializeSection;
    property FinalizeSection: TPFinialization read FFinalizeSection;
    property UnitTypes: TPascalUnitTypes read FUnitType;
    property UnitName: String read GetUnitName;
    property UnitFileName: String read GetUnitFileName; // does not include the extension!
    property LibName: String read FLibName write FLibName;
    property NameSpace: TgirNamespace read FNameSpace;
  end;

implementation
uses girpascalwriter, girCTypesMapping;

function IndentText(const AText: String; Spaces: Integer = 0; LineEndingCount: Integer = 1): String;
begin
  if AText = '' then
    Exit('');
  SetLength(Result, Spaces);
  FillChar(Result[1], Spaces, ' ');
  Result := Result+AText;
  if LineEndingCount > 0 then
     begin
       SetLength(Result, Length(Result)+Length(LineEnding)*LineEndingCount);
       FillChar(Result[Length(AText)+Spaces+1], LineEndingCount, LineEnding);
     end;
end;

function MakePointerTypesForType(const AName: String; PointerLevel: Integer): TStringList;
var
  //Chars: String;
  BaseName: String;
  i: Integer;
begin
  Result := TStringList.Create;
  if AName = '' then
    Exit;
  BaseName:=AName;
  // check if it's already prefixed
  if AName[1] = 'T' then
    BaseName:=Copy(AName,2, Length(AName));

  for i := 0 to PointerLevel-1 do
    begin
      BaseName := 'P'+BaseName;
      Result.Add(BaseName);
    end;
end;

function CalculateUnitName(ANameSpace: String; AVersion: String): String;
var
  Version: String;
begin
  if ANameSpace[Length(ANameSpace)] in ['0'..'9'] then
    ANameSpace := ANameSpace + '_';
  Version := StringReplace(AVersion,'.','_',[rfReplaceAll]);
  Version := StringReplace(Version,'_0','',[rfReplaceAll]);
  Result := ANameSpace+Version;
end;

{ TPascalUnitGroup }

function TPascalUnitGroup.GetUnitForType(AType: TPascalUnitType): TPascalUnit;
var
  PUnit: TPascalUnit;
begin
  Result := nil;
  for Pointer(PUnit) in FUnits do
    if AType in PUnit.UnitTypes then
      Exit(PUnit);
end;

constructor TPascalUnitGroup.Create(AWriter:TObject{TgirPascalWriter}; ANameSpace: TgirNamespace;
  AOptions: TgirOptions);
begin
  FWriter := AWriter;
  FNameSpace := ANameSpace;
  FOptions := AOptions;
  FUnits := TFPList.Create;
  FSimpleUnit := ([goSeperateConsts, goClasses, goObjects] * AOptions ) = [];

  if FSimpleUnit then
  begin
    FUnits.Add(TPascalUnit.Create(Self, FNameSpace, FOptions, PascalUnitTypeAll));
    //Units[utSimple] := TPascalUnit.Create(Self, FNameSpace, FOptions, [utSimple])

  end
  else
  begin
    //Units[utConsts] := TPascalUnit.Create(Self, FNameSpace, FOptions, [utConsts]);
    //Units[utTypes] := TPascalUnit.Create(Self, FNameSpace, FOptions, [utTypes]);
    //Units[utFunctions] := TPascalUnit.Create(Self, FNameSpace, FOptions, [utFunctions]);
    FUnits.Add(TPascalUnit.Create(Self, FNameSpace, FOptions, PascalUnitTypeCommon));
    if goClasses in FOptions then
      FUnits.Add(TPascalUnit.Create(Self, FNameSpace, FOptions, [utClasses]))
    else
      FUnits.Add(TPascalUnit.Create(Self, FNameSpace, FOptions, [utObjects]))
  end;

end;

destructor TPascalUnitGroup.Destroy;
var
  PascalUnit: TPascalUnit;
begin
  for Pointer(PascalUnit) in FUnits do
    if Assigned(PascalUnit) then
      PascalUnit.Free;
  FUnits.Free;

  inherited Destroy;
end;

procedure TPascalUnitGroup.GenerateUnits;
var
  PUnit: TPascalUnit;
begin
  for Pointer(PUnit) in FUnits do
    if Assigned(PUnit) then
       PUnit.GenerateUnit;
  UnitForType[utConsts].ProcessConsts(FNameSpace.Constants);
  UnitForType[utTypes].ProcessTypes(FNameSpace.Types);
  UnitForType[utFunctions].ProcessFunctions(FNameSpace.Functions);
  for Pointer(PUnit) in FUnits do
    if Assigned(PUnit) then
    begin
      PUnit.Finish;
      TgirPascalWriter(FWriter).OnUnitWriteEvent(TgirPascalWriter(FWriter), PUnit.UnitFileName+TgirPascalWriter(FWriter).DefaultUnitExtension, PUnit.AsStream);
      TgirPascalWriter(FWriter).Units.Add(PUnit);
      if (goWantTest in FOptions) then
      begin
        TgirPascalWriter(FWriter).OnUnitWriteEvent(TgirPascalWriter(FWriter), PUnit.UnitFileName+'_test'+TgirPascalWriter(FWriter).DefaultUnitExtension, PUnit.FTestPascalFile);
      end;
    end;
end;

constructor TPDeclarationFunctions.Create(ADynamicFunctions: Boolean);
begin
  inherited Create;
  FDynamicFunctions:=ADynamicFunctions;
  Lines.Duplicates:=dupIgnore;
  Lines.Sorted:=True;
end;

function TPDeclarationFunctions.AsString: String;
begin
  if FDynamicFunctions then
    Result := 'var'+ LineEnding+inherited AsString
  else
    Result:= inherited AsString;
end;

{ TPDeclarationVar }

function TPDeclarationVar.AsString: String;
begin
  Result:= IndentText('var') + Lines.Text;
end;

{ TPDeclarationWithLines }

constructor TPDeclarationWithLines.Create;
begin
  Lines := TStringList.Create;
end;

destructor TPDeclarationWithLines.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

function TPDeclarationWithLines.AsString: String;
begin
  Result:=Lines.Text;
end;

function TPCodeText.GetContent: String;
begin
  Result := Lines.Text;
end;

procedure TPCodeText.SetContent(AValue: String);
begin
  Lines.Text:=AValue;
end;

{ TPDeclarationType }

function TPDeclarationType.AsString: String;
begin
  Result:= IndentText('type') + Lines.Text;
end;

{ TPDeclarationConst }

function TPDeclarationConst.AsString: String;
var
  FirstConst: String;
begin
  if Lines.Count < 1 then
    Exit('');
  if (Lines.count > 1) and (Lines[1] = 'type') then
    FirstConst := ''
  else
    FirstConst:=IndentText('const');

  Result:= FirstConst + Lines.Text;
end;

{ TPUses }

constructor TPUses.Create;
begin
  Units := TStringList.Create;
  Units.StrictDelimiter:=True;
  Units.Delimiter:=',';
end;

destructor TPUses.Destroy;
begin
  Units.Free;
  inherited Destroy;
end;

function TPUses.AsString: String;
begin
  Result := '';

  if Units.Count>0 then
    Result := IndentText('uses') + IndentText(Units.DelimitedText+';', 2)+LineEnding;
end;

{ TPFinialization }

function TPFinialization.AsString: String;
begin
  Result := 'finalization'+LineEnding+FDeclarations.AsString;
end;

{ TPInitialize }

function TPInitialize.AsString: String;
begin
  Result := 'initialization'+LineEnding+FDeclarations.AsString;
end;

function TPImplementation.AsString: String;
begin
  Result := IndentText('implementation')+FDeclarations.AsString;
end;

{ TPCommonSections }

constructor TPCommonSections.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FDeclarations := TPDeclarationList.Create;
end;

destructor TPCommonSections.Destroy;
begin
  FDeclarations.Free;
  inherited Destroy;
end;

constructor TPInterface.Create(AOwner: TObject; AUses: TPUses; ADynamicFunctions: Boolean);
begin
  inherited Create(AOwner);
  FUsesSection := AUses;
  FUsesSection.Units.Add('CTypes');
  FConstSection := TPDeclarationConst.Create;
  FFunctionSection := TPDeclarationFunctions.Create(ADynamicFunctions);
end;

destructor TPInterface.Destroy;
begin
  FConstSection.Free;
  FFunctionSection.Free;
  FUsesSection.Free;
  inherited Destroy;

end;

function TPInterface.AsString: String;
begin
  Result := IndentText('interface')+
      FUsesSection.AsString+
      FConstSection.AsString+
      FDeclarations.AsString+
      FFunctionSection.AsString;
end;

{ TPUnitPart }

constructor TPUnitPart.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

{ TPascalUnit }

function TPascalUnit.GetUnitName: String;
begin
  Result := CalculateUnitName(FNameSpace.NameSpace, FNameSpace.Version);
end;

function TPascalUnit.GetUnitFileName: String;
begin
  Result := UnitName+GetUnitPostfix;
end;

function TPascalUnit.GetUnitPostfix: String;
begin
  Result := '';
  if FUnitType = PascalUnitTypeAll then
    exit;

  if PascalUnitTypeCommon = FUnitType then
    Result := 'Common'
  else if utClasses in FUnitType then
    Result := 'Classes'
  else if utObjects in FUnitType then
    Result := 'Objects'
  else
    Result := 'UnknownPostfixName';


  {case FUnitType of
    utConsts:    Result := 'Consts';
    utTypes:     Result := 'Types';
    utFunctions: Result := 'Functions';
    utClasses:   Result := 'Classes';
    utObjects:   Result := 'Objects';
  end;}
end;

function TPascalUnit.MakePascalTypeFromCType(CName: String; PointerLevel: Integer = MaxInt; Trim_T_IfExists: Boolean =True): String;
var
  C: Integer = 0;
  i: Integer = 0;
  Prefix: String;
begin
  Result := '';

  repeat
    i := Pos('*', CName);
    if i > 0 then
      begin
        Inc(C);
        Delete(CName, i,1);
      end;
  until i = 0;

  if Trim_T_IfExists and (Length(CName) > 0) and (CName[1] = 'T') then
    Delete(CName,1,1);

  case PointerLevel of
    MaxInt:; // C remains the same
   -1: ;
    0: C := 0;
  else
    C := PointerLevel;
  end;

  if C = -1 then
    Prefix := ''
  else if C = 0 then
    Prefix := 'T'
  else
    begin
      SetLength(Prefix, C);
      FillChar(Prefix[1], C, 'P');
    end;
  Result := Trim(Prefix+Trim(CName));
end;

function TPascalUnit.EscapeSingleQuote(AString: String): String;
var
  i: Integer;
begin
  Result := AString;
  for i := Length(Result) downto 1 do
    if Result[i] = '''' then
      Insert('''', Result, i);
end;

procedure TPascalUnit.AddGLibSupportCode;
var
  TypeSect: TPDeclarationType;
  i: Integer;
begin
  //if not (FUnitType in [utSimple,utTypes]) then
  //  Exit;
  if not ((FUnitType = PascalUnitTypeAll) or (utTypes in FUnitType )) then
    Exit;
  TypeSect := WantTypeSection;
  for i := 1 to 31 do
  begin
    if i in [8,16,32] then
       continue;
    TypeSect.Lines.Add(Format('  guint%d = 0..(1 shl %d-1);',[i,i]));
  end;
end;


procedure TPascalUnit.ProcessType(AType: TGirBaseType; AForceWrite: Boolean = False);
begin
  if (AType = nil)  then
    Exit;

  if (AType.ObjectType = otFuzzyType) and (TgirFuzzyType(AType).ResolvedType <> nil) then
  begin
    TgirFuzzyType(AType).ResolvedType.ImpliedPointerLevel := AType.ImpliedPointerLevel;
    AType := TgirFuzzyType(AType).ResolvedType;
  end;

  if (AType.Owner <> FNameSpace) then
    Exit; // it's written in another Namespace

  if (AType.CType = '') then //(AType.Name = '') then
  begin
    WriteLn('WARNING: Type.Ctype undefined! : ', Atype.Name);
    //Halt;

  end;
  if ProcessLevel > 0 then
  begin
    WriteForwardDefinition(AType);
    if AType.InheritsFrom(TgirCallback) or AType.InheritsFrom(TgirBitField) then
      AForceWrite:=True;
    if not AForceWrite then
      Exit;
  end;
  if (AType.Writing = msWritten) or ((AType.Writing = msWriting) {and not AForceWrite}) then
  begin
    //WriteLn('Already Written Type Used: ', AType.TranslatedName);
    Exit;
  end;

  //if AForceWrite then
  //  WriteLn('ForceWriting: ', AType.CType);

  Inc(ProcessLevel);
  AType.Writing := msWriting;

  case AType.ObjectType of
    otAlias:         HandleAlias(TgirAlias(AType));
    otCallback:      HandleCallback(TgirCallback(AType));
    otEnumeration:   HandleEnum(TgirEnumeration(AType));
    otBitfield:      HandleBitfield(TgirBitField(AType));
    otRecord:        HandleRecord(TgirRecord(AType));
    otFunction:      HandleFunction(TgirFunction(AType));
    otGType:         HandleObject(TgirGType(AType), gtGType);
    otObject:        HandleObject(TgirObject(AType), gtObject);
    otClass:         HandleObject(TgirObject(AType), gtClass);
    otClassStruct:   HandleObject(TgirObject(AType), gtClassStruct);
    otNativeType:    HandleNativeType(TgirNativeTypeDef(AType));   // not called but the items are added to the list... where are they?
    otInterface:     HandleObject(TgirInterface(AType), gtInterface);
    otUnion:         HandleUnion(TgirUnion(AType));
    otFuzzyType:
      begin
        if TgirFuzzyType(AType).ResolvedType = nil then
          HandleOpaqueType(TgirFuzzyType(AType))
        else
        begin
          Dec(ProcessLevel); // it should be level 0
          ProcessType(TgirFuzzyType(AType).ResolvedType);
          Inc(ProcessLevel);
        end;
      end;
  else
    //WantTypeSection.Lines.Add(IndentText(AType.ClassName + ' ' +AType.Name + ' ' + AType.CType ,2));
    WriteLn('Unknown Type: ', AType.ClassName);
    Halt;
  end; // case
  if (AType.InheritsFrom(TgirGType)) then // and (TgirGType(AType).HasFields) then
    AddTestType((TgirGType(AType)));//, AType.TranslatedName, AType.CType, TgirGType(AType).GetTypeFunction);

  AType.Writing:=msWritten;
  Dec(ProcessLevel);
end;

procedure TPascalUnit.ResolveFuzzyTypes;
var
  BaseType: TGirBaseType;
  FuzzyType : TgirFuzzyType absolute BaseType;
  i: Integer;
  CTypesType: String;
begin
  // here we wil try to find unresolved types that have compatible types in pascal.
  // for instance xlib uses guint but does not depend on glib where that is defined, we will try to replace those with cuint from ctypes
  for i := 0 to NameSpace.Types.Count-1 do
    begin
      BaseType := TGirBaseType(NameSpace.Types.Items[i]);
      if BaseType.InheritsFrom(TgirFuzzyType) and (FuzzyType.ResolvedType = nil) then
      begin
        CTypesType := LookupGTypeToCType(FuzzyType.CType);
        if CTypesType <> '' then
        begin
          FuzzyType.TranslatedName:= CTypesType;
          FuzzyType.Writing := msWritten;
        end;
      end;
    end;
end;

procedure TPascalUnit.AddTestType(AGType: TgirGType);
const
  PTest = 'procedure Test_%s;'                                                         +LineEnding+
          'var'                                                                        +LineEnding+
          '  PSize: Integer;'                                                          +LineEnding+
          '  CSize: Integer;'                                                          +LineEnding+
          '  CClassSize: Integer;'                                                     +LineEnding+
          'begin'                                                                      +LineEnding+
          '  PSize := SizeOf(%s);'                                                     +LineEnding+
          '  CSize := GTypeSize(%s, CClassSize);'                                      +LineEnding+
          '  if CSize = PSize then'                                                    +LineEnding+
          '    WriteLn(''%s Matches C Size: '',CSize)'                                 +LineEnding+
          '  else'                                                                     +LineEnding+
          '    WriteLn(''%s size ('',PSize,'') does NOT match %s size ('',CSize,'')''); ' +LineEnding+
          '%send;'                                                                       +LineEnding;
  PTest2 ='  PSize := SizeOf(%s);'                                                +LineEnding+
          '  if CClassSize = PSize then'                                               +LineEnding+
          '    WriteLn(''%s Matches C Size: '',CSize)'                            +LineEnding+
          '  else'                                                                     +LineEnding+
          '    WriteLn(''%s size ('',PSize,'') does NOT match %s size ('',CSize,'')'');' +LineEnding;

var
  PT: String;
  PT2: String = '';
  Cls: TgirClass absolute AGType;
begin
  if not (goWantTest in FOptions) then
    Exit;
  if (AGType.CType = '') then //or (ACName[1] = '_') then // we skip private types
    Exit;
  ResolveTypeTranslation(AGType);

  if AGType.GetTypeFunction = '' then exit;

  if AGType.InheritsFrom(TgirClass) and (Cls.ClassStruct <> nil) then
  begin
    ResolveTypeTranslation(Cls.ClassStruct);
    PT2 := Format(PTest2, [cls.ClassStruct.TranslatedName, cls.ClassStruct.TranslatedName, cls.ClassStruct.TranslatedName, cls.ClassStruct.CType] );
  end;
  PT := Format(PTest, [AGType.CType, AGType.TranslatedName, AGType.GetTypeFunction, AGType.TranslatedName, AGType.TranslatedName, AGType.CType, PT2]);

  FTestPascalFile.WriteString(PT); // pascal testproc
  FTestPascalBody.Add(Format('Test_%s;',[AGType.CType])); //call pascal testproc
end;

function TPascalUnit.WantTypeSection: TPDeclarationType;
begin
  if (InterfaceSection.Declarations.Count = 0)
  or (InterfaceSection.Declarations.Declarations[InterfaceSection.Declarations.Count-1].ClassType <> TPDeclarationType.ClassType)
  then
    begin
      Result := TPDeclarationType.Create;
      InterfaceSection.Declarations.Add(Result);
    end
  else
    Result := TPDeclarationType(InterfaceSection.Declarations.Declarations[InterfaceSection.Declarations.Count-1]);
end;

function TPascalUnit.WantConstSection: TPDeclarationConst;
begin
  Result := InterfaceSection.ConstSection;
end;

function TPascalUnit.WantFunctionSection: TPDeclarationFunctions;
begin
  Result := InterfaceSection.FunctionSection;
end;

procedure TPascalUnit.WritePointerTypesForType(AItem: TGirBaseType; ATypeName: String; APointerLevel: Integer; ALines: TStrings);
var
  PTypes: TStrings;
  i: Integer;
begin
  if AItem.ForwardDefinitionWritten then
    WriteLn('Warning: Forwards definitions already written for : ', Aitem.TranslatedName);
  AItem.ForwardDefinitionWritten := True;
  PTypes := MakePointerTypesForType(ATypeName, APointerLevel);
  PTypes.Insert(0, ATypeName);
  for i := PTypes.Count-1 downto 1 do
    ALines.Add(IndentText(PTypes[i]+ ' = ^'+PTypes[i-1]+';',2,0));
  PTypes.Free;
end;

procedure TPascalUnit.HandleNativeType(AItem: TgirNativeTypeDef);
var
  TypeSect: TPDeclarationType;
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then begin
    ProperUnit.HandleNativeType(AItem);
    Exit;
  end;
  if (AItem.PascalName = AItem.CType) and (AItem.Name <> 'file') then
      Exit; // is a native pascal type plus a = a doesn't fly with the compiler


  if AItem.CType <> 'file' then
    AItem.CType:=SanitizeName(AItem.CType);

  TypeSect := WantTypeSection;
  AItem.TranslatedName:=AItem.CType;
  //WritePointerTypesForType(Aitem, AItem.CType, AItem.ImpliedPointerLevel, TypeSect.Lines);
  if AItem.Name <> 'file' then
    TypeSect.Lines.Add(IndentText(SanitizeName(AItem.CType)+ ' = '+ AItem.PascalName+';', 2,0));
end;

procedure TPascalUnit.HandleAlias(AItem: TgirAlias);
var
  ResolvedForName: String;
  CType: TGirBaseType = nil;
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then begin
    ProperUnit.HandleAlias(AItem);
    Exit;
  end;
  ResolveTypeTranslation(AItem);
  ResolveTypeTranslation(AItem.ForType);

  // some aliases are just for the parser to connect a name to an alias
  if AItem.CType = '' then
    Exit;
  ResolvedForName := aItem.ForType.TranslatedName;
  if ResolvedForName = '' then
    begin
      {
      //CType := NameSpace.LookupTypeByName('', AItem.ForType.CType);
      if CType <> nil then
         ResolvedForName := CType.TranslatedName;

      if ResolvedForName <> '' then
        aItem.ForType.TranslatedName := ResolvedForName
      else}
        ResolvedForName := AItem.ForType.CType;
    end;

  WriteForwardDefinition(AItem);

  AItem.TranslatedName:=MakePascalTypeFromCType(AItem.CType);

  if AItem.Writing < msWritten then
    WantTypeSection.Lines.Add(IndentText(Aitem.TranslatedName+' = '+ ResolvedForName+';' ,2,0));
end;

procedure TPascalUnit.HandleCallback(AItem: TgirCallback);
var
  TypeSect: TPDeclarationType;
  CB: String;
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then begin
    ProperUnit.HandleCallback(AItem);
    Exit;
  end;
  TypeSect := WantTypeSection;

  CB := WriteCallBack(AItem, False);

  if AItem.Writing < msWritten then
    TypeSect.Lines.Add(IndentText(CB,2,0))
end;

procedure TPascalUnit.HandleEnum(AItem: TgirEnumeration; ADeclareType: Boolean = True);
var
  ConstSection: TPDeclarationConst;
  Entry: String;
  i: Integer;
  CName: String;
  TypeName: String;
  ProperUnit: TPascalUnit;

begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then begin
    ProperUnit.HandleEnum(AItem, ADeclareType);
    Exit;
  end;
  ResolveTypeTranslation(AItem);

  ConstSection := WantConstSection;
  ConstSection.Lines.Add('');
                //ATK_HYPERLINK_IS_INLINE_
  if ADeclareType then
  begin
    // forces forward declarations to be written
    ProcessType(AItem);

    TypeName := ': '+AItem.TranslatedName;

    // yes we cheat a little here using the const section to write type info
    ConstSection.Lines.Add('type');
    ConstSection.Lines.Add(IndentText(AItem.TranslatedName+' = Integer;', 2,0));
    ConstSection.Lines.Add('const');
  end
  else
    TypeName:='';
  ConstSection.Lines.Add(IndentText('{ '+ AItem.CType + ' }',2,0));

  for i := 0 to AItem.Members.Count-1 do
    begin
      CName := AItem.Members.Member[i]^.CIdentifier;
      if CName = 'ATK_HYPERLINK_IS_INLINE' then
        CName :='ATK_HYPERLINK_IS_INLINE_';
      Entry := CName + TypeName+ ' = ' + AItem.Members.Member[i]^.Value+';';
      ConstSection.Lines.Add(IndentText(Entry,2,0));
    end;
  AItem.Writing:=msWritten;
end;

procedure TPascalUnit.HandleBitfield(AItem: TgirBitField);
{
const
  TemplateLongWord =
     '%s = packed object(TBitObject32)'+LineEnding+
     '%s'+LineEnding+
     'end';
var
  Intf: TPDeclarationType;
  CodeText: TPCodeText;
  Code: TStringList;
  PName: String;
  Entry: String;
  i: Integer;
  VarType: String;
 }
begin
  HandleEnum(AItem, True);
(*
  Intf := WantTypeSection;
  CodeText := TPCodeText.Create;
  ImplementationSection.Declarations.Add(CodeText);
  Code := TStringList.Create;

  PName:=MakePascalTypeFromCType(AItem.CType);

  {case AItem.Bits of
     //1..8:   VarType:='Byte';
     //9..16:  VarType:='Word';
     //0:;
     //17..32: VarType:='LongWord';
     //33..64: VarType:='QWord';
  else
    WriteLn('Bitfield <> 16bits');
    Halt;
  end;
  }
  HandleEnum(AItem, False);

  VarType:='DWord';

  Intf.Lines.Add(IndentText(PName+ ' = packed object(TBitObject32)',2,0));
  Intf.Lines.Add(IndentText('public',2,0));
  for i := 0 to AItem.Members.Count-1 do
    begin
       Entry := 'property '+ SanitizeName(AItem.Members.Member[i]^.Name) +': '+VarType+' index '+AItem.Members.Member[i]^.Value+' read GetBit write SetBit;';
       Intf.Lines.Add(IndentText(Entry, 4,0));
    end;
  Intf.Lines.Add(IndentText('end;',2,0));
  Intf.Lines.Add('');

  CodeText.Content:=Code.Text;
  Code.Free;
 *)
end;

procedure TPascalUnit.HandleRecord(AItem: TgirRecord);
var
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then begin
    ProperUnit.HandleRecord(AItem);
    Exit;
  end;
  ResolveTypeTranslation(AItem);
  AItem.ImpliedPointerLevel:=1; // will not be decreased only will grow

  WriteForwardDefinition(AItem);

  WantTypeSection.Lines.Add(WriteRecord(AItem));

end;

procedure TPascalUnit.HandleOpaqueType(AItem: TgirFuzzyType);
var
  TypeSect: TPDeclarationType;
  Plain: String;
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then begin
    ProperUnit.HandleOpaqueType(AItem);
    Exit;
  end;
  if AItem.CType = '' then
    Exit;
  TypeSect := WantTypeSection;
  Plain := StringReplace(AItem.CType, '*', '', [rfReplaceAll]);
  AItem.TranslatedName:=MakePascalTypeFromCType(Plain, 0);

  TypeSect.Lines.Add('');
  TypeSect.Lines.Add('  { '+ AItem.CType+' }');
  TypeSect.Lines.Add(IndentText(AItem.TranslatedName +' = record',2,0));
  TypeSect.Lines.Add(IndentText('{ opaque type }',4,0));
  TypeSect.Lines.Add(IndentText('Unknown: Pointer;',4,0)); // to prevent crashes of the compiler
  TypeSect.Lines.Add(IndentText('end;',2,1));
  WriteLn('Wrote Opaque Type Name = ', AItem.Name,' CType = ', AItem.CType);

end;

function HasPackedBitfield(var PackedBits: TStringList): Boolean;
begin
  HasPackedBitfield := PackedBits <> nil;
end;

procedure PackedBitsAddEntry (var PackedBits: TStringList; AItem: TGirBaseType; var APackedBitsFieldCount: Integer; AEntry: String; AOriginalDeclList: TStrings); // creates a new type to hold the packed bits
const
  BitType = '  %sBitfield%d = bitpacked record';
var
  BitEntry: String;
begin
  if PackedBits = nil then
  begin
    PackedBits := TStringList.Create;
    PackedBits.Add(Format(BitType,[AItem.TranslatedName, APackedBitsFieldCount]));
    BitEntry := Format('    Bitfield%d : %sBitfield%d; { auto generated type }', [APackedBitsFieldCount, AItem.TranslatedName, APackedBitsFieldCount]);
    AOriginalDeclList.Add(BitEntry);
    Inc(APackedBitsFieldCount);
  end;
  // now packed bits is assigned
  PackedBits.Add(Format('    %s;', [AEntry]));
end;

function EndPackedBits(var PackedBits: TStringList): String;
begin
  if PackedBits = nil then
    Exit;
  PackedBits.Add('  end;');
  Result := PackedBits. Text;
  FreeAndNil(PackedBits);
end;




procedure TPascalUnit.HandleFunction(AItem: TgirFunction);
var
  RoutineType: String;
  Returns: String;
  Params: String;
  FuncSect: TPDeclarationFunctions;
  Postfix: String;
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utFunctions);
  if ProperUnit <> Self then
  begin
    ProperUnit.HandleFunction(AItem);
    Exit;
  end;
  WriteFunctionTypeAndReturnType(AItem, RoutineType, Returns);
  Params := WriteFunctionParams(AItem.Params);
  Postfix := ' external;';// '+UnitName+'_library;';
  FuncSect := WantFunctionSection;
  if not (goLinkDynamic in FOptions) then
    FuncSect.Lines.Add(RoutineType +' '+ AItem.CIdentifier+ParenParams(Params)+Returns+Postfix)
  else
  begin
    FuncSect.Lines.Add(AItem.CIdentifier +': '+RoutineType +ParenParams(Params)+Returns);
    FDynamicEntryNames.Add(AItem.CIdentifier);
  end;
end;

function TPascalUnit.WriteFunction(AFunction: TgirFunction; AItem: TGirBaseType; AIsMethod: Boolean; AWantWrapperForObject: Boolean; AFunctionList: TStrings; AExistingUsedNames: TStringList = nil): String;
var
  Prefix: String = '';
  RoutineType: String;
  Returns: String;
  Params: String;
  Postfix: String;
  Entry: String;
  InLineS: String = '';
  Deprecated: String = '';
  ProperUnit: TPascalUnit;
  OptionsIndicateWrapperMethod: Boolean;
begin
  Result := '';
  OptionsIndicateWrapperMethod:= FUnitType = PascalUnitTypeAll;
  // we skip deprecated functions
  if AFunction.Deprecated and not (goIncludeDeprecated in FOptions) then //and (CompareStr(AFunction.DeprecatedVersion, NameSpace.Version) >=  0) then
    Exit;

  // some abstract functions that are to be implemented by a module and shouldn't be declared. There is no indicator in the gir file that this is so :(
  if (AFunction.CIdentifier = 'g_io_module_query')
  or (AFunction.CIdentifier = 'g_io_module_load')
  or (AFunction.CIdentifier = 'g_io_module_unload')
  then
    Exit; // they are functions to be implemented by a runtime loadable module, they are not actually functions in glib/gmodule/gio

  if AWantWrapperForObject then
    InLineS:=' inline;';

  if AFunction.Deprecated then Deprecated :=' deprecated ''Since ' + NameSpace.NameSpace + ' ' + AFunction.DeprecatedVersion+' '+StringReplace(AFunction.DeprecatedMsg,'''','`', [rfReplaceAll])+''';';
  // this fills in the values for procedure/function and the return type
  WriteFunctionTypeAndReturnType(AFunction, RoutineType, Returns);

  // check if it is a constructor
  if AFunction.InheritsFrom(TgirConstructor) then
    Returns := ': '+MakePascalTypeFromCType(AItem.TranslatedName ,1)+'; cdecl;';

  Params := WriteFunctionParams(AFunction.Params);
  if Pos('array of const', Params) + Pos('va_list', Params) > 0 then
    Prefix:='//';
  if not (goLinkDynamic in FOptions) then
    Postfix := ' external;'+ Deprecated// '+UnitName+'_library;';
  else
    PostFix := ''+Deprecated;

  // first wrapper proc
  Entry := Prefix + RoutineType +' '+ SanitizeName(AFunction.Name, AExistingUsedNames)+ParenParams(Params)+Returns+InLineS;

  // no need to pass self that will not be used
  if (not AIsMethod) and AWantWrapperForObject then
    Entry := Entry + ' static;';

  // This is the line that will be used by in the TObject declaration. <----
  // result will be written in the object declaration.
  if OptionsIndicateWrapperMethod then
    Result := Entry + Deprecated
  else
    Result := '';

  // now make sure the flat proc has all the params it needs
  if AIsMethod then
  begin
    // methods do not include the first param for it's type so we have to add it
    if Params <> '' then
      Params := SanitizeName('A'+AItem.Name) +': '+TypeAsString(AItem, 1)+'; ' + Params
    else
      Params := SanitizeName('A'+AItem.Name) +': '+TypeAsString(AItem, 1);
  end;

  ProperUnit := FGroup.GetUnitForType(utFunctions);

  // write the flat c function that will be linked
  if not (goLinkDynamic in FOptions) then
  begin
    // this is the flat c procedure that a wrapper would call
    Entry := RoutineType +' '+ AFunction.CIdentifier+ParenParams(Params)+Returns;
  end
  else // Link Dynamic
  begin
    Entry := AFunction.CIdentifier+': '+RoutineType+ParenParams(Params)+Returns;
    ProperUnit.FDynamicEntryNames.Add(AFunction.CIdentifier);
  end;

  // ignores duplicates
  AFunctionList.Add(Entry+Postfix);

  //RoutineType, AObjectName, AObjectFunctionName, AParams, AFunctionReturns, AFlatFunctionName, AWantSelf
  // writes the implementation of what we declared in the object
  if AWantWrapperForObject and  (Prefix = '') and OptionsIndicateWrapperMethod then
     WriteWrapperForObject(RoutineType, AItem.TranslatedName, ProperUnit.SanitizeName(AFunction.Name), AFunction.Params, Returns, AFunction.CIdentifier, AIsMethod);
end;

procedure TPascalUnit.HandleObject(AItem: TgirObject; AObjectType: TGirToken);
var
  TypeDecl: TStringList;
  i: Integer;
  UnitFuncs,
  TypeFuncs: TStrings; // functions and procedures of an object
  ParentType: String ='';
  UsedNames: TStringList;
  WrittenFields: Integer;
  PackedBitsFieldCount: Integer = 0;
  PackedBits: TStringList = nil;

  function GetTypeForProperty(AProperty: TgirProperty; out SetFound: Boolean): String;
  var
    i,j: Integer;
    FoundPos: Integer;
    LookingForGet,
    LookingForSet: String;
    Line: String;
    GetFound: Boolean;
  begin
    GetFound := False;
    SetFound := False;
    Result := 'UNABLE_TO_FIND_TYPE_FOR_PROPERTY';
    LookingForGet:=SanitizeName('get_'+AProperty.Name);
    LookingForSet:=SanitizeName('set_'+AProperty.Name);
    for i := TypeFuncs.Count-1 downto 0 do
    begin
      Line := TypeFuncs.Strings[i];

      if not GetFound then
      begin
        FoundPos:= Pos(LookingForGet+':', Line);
        //if FoundPos = 0 then
        //  FoundPos:=Pos(LookingForGet+'(', Line); // we do not yet support properties with parameters :(
      end;
      if (FoundPos > 0) and not GetFound then
      begin
        GetFound := True;
        for j := Length(Line) downto 1 do
          if Line[j] = ':' then
          begin
            Line := Copy(Line, j+1, Length(Line));
            break;
          end;
        FoundPos:=Pos(';', Line);
        Result := Trim(Copy(Line, 1,FoundPos-1));
        break;
      end
    end;

    for i := TypeFuncs.Count-1 downto 0 do
    begin
      Line := TypeFuncs.Strings[i];

      SetFound := Pos(LookingForSet+':', Line) > 0;
      SetFound := SetFound or (Pos(LookingForSet+'(', Line) > 0);

      // the first argument must match the property type! (result is the return type)
      //if SetFound and (Pos(Result+')', Line) = 0) then
      //  writeln('Eliminated ', Line, ' for missing: ', Result);
      SetFound := SetFound and (Pos(Result+')', Line) > 0);

      // pascal properties cannot use functions for the set 'procedure'
      SetFound := SetFound and (Pos('procedure ', Line) > 0) and (Pos('property '+AProperty.Name, Line) = 0);

      if SetFound then
        Exit;
    end;


  end;
  function WriteMethodProperty(AProperty: TgirProperty; AType: String; SetFound: Boolean): String;
  const
    Prop = '%sproperty %s: %s %s %s;';
  var
    ReadFunc,
    WriteProc: String;
    Comment: String='';
    OptionsIndicateWrapperMethod: Boolean;
  begin
    OptionsIndicateWrapperMethod:=FUnitType = PascalUnitTypeAll;
    if not OptionsIndicateWrapperMethod then
      Exit('');
    ReadFunc:= 'read '+SanitizeName('get_'+ AProperty.Name);
    if AProperty.Writable then
    begin
      if SetFound then
        WriteProc := 'write '+ SanitizeName('set_'+AProperty.Name)
      else
        WriteProc := ' { property is writeable but setter not declared } ';
    end;
    if AType = 'UNABLE_TO_FIND_TYPE_FOR_PROPERTY' then
      Comment := '//';

    Result := Format(Prop, [Comment, SanitizeName(AProperty.Name, UsedNames), AType, ReadFunc, WriteProc  ]);
  end;

  function AddField(AParam: TgirTypeParam): Boolean; // returns True if a bitsized param was used or false if it wasn't.
  var
    Param: String;
    ParamIsBitSized: Boolean;
  begin
    ResolveTypeTranslation(AParam.VarType);
    AddField := False;

    // this is for object inheritance. a struct conatins the parent as the first field so we must remove it since our object inherits it already
    Inc(WrittenFields);
    if (WrittenFields = 1) and (AObjectType = gtClass) and (TgirClass(AItem).ParentClass <> nil) then
    begin
      Exit;
    end;

    Param := WriteParamAsString(AParam,i, ParamIsBitSized, nil, UsedNames);

    if ParamIsBitSized then
      PackedBitsAddEntry(PackedBits, AItem, PackedBitsFieldCount, Param, TypeDecl)
    else
      TypeDecl.Add(IndentText(Param+';',4,0));
    AddField := ParamIsBitSized;
  end;

  procedure AddLinesIfSet(AList: TStrings; const TextIn: String);
  begin
    if TextIn <> '' then
      AList.Add(TextIn);
  end;

  procedure HandleFieldType(Field: TGirBaseType; AFirstPass: Boolean; out AddedBitSizedType: Boolean);
  var
    SetFound: Boolean;
    PropType: String;
  begin
    AddedBitSizedType:=False;
    // FIRST PASS
    if AFirstPass then
    begin

      case Field.ObjectType of
        otVirtualMethod: ; // ignore. may be usefull if we wrap this in pascal classes instead of objects. Is already written in the class struct
        otCallback,
        otArray,
        otTypeParam,
        otUnion: Exit; // these will be done on the second pass. this is to avoid duplicate names if they are the same as some function or property. giving the function priority of the original name


        otGlibSignal : if AObjectType = gtInterface then // signals are external to the object and not 'part' of them
                         TypeDecl.Add(IndentText(WriteCallBack(TgirCallback(Field),True, UsedNames),4,0));

        //WriteFunction(AFunction, AItem, AIsMethod, AWantWrapperForObject, AFunctionList): String;
        otFunction     : AddLinesIfSet(TypeFuncs, IndentText(WriteFunction(TgirFunction(Field), AItem, False, True, UnitFuncs, UsedNames),4,0));
        otMethod       : AddLinesIfSet(TypeFuncs, IndentText(WriteFunction(TgirFunction(Field), AItem, True, True, UnitFuncs, UsedNames),4,0));
        otConstructor  : AddLinesIfSet(TypeFuncs, IndentText(WriteFunction(TgirConstructor(Field), AItem, False, True, UnitFuncs, UsedNames),4,0));
        otProperty     :
                        begin
                          PropType := GetTypeForProperty(TgirProperty(Field), SetFound);
                          AddLinesIfSet(TypeFuncs, IndentText(WriteMethodProperty(TgirProperty(Field), PropType, SetFound),4,0));
                        end;

      else // case <
        WriteLn('Unknown Field Type : ', Field.ClassName);
        Halt;
      end;
    end;

    // SECOND PASS
    if not AFirstPass then
    begin
      case Field.ObjectType of
       otArray,
       otTypeParam: AddedBitSizedType := AddField(TgirTypeParam(Field));
       otCallback : TypeDecl.Add(IndentText(WriteCallBack(TgirCallback(Field),True, UsedNames),4,0));
       otUnion    :
            begin
              // we have to create a union outside the object and include it as a field
              Field.CType := AItem.CType+'_union_'+Field.Name;
              ResolveTypeTranslation(Field);
              HandleUnion(TgirUnion(Field));
              TypeDecl.Add(IndentText(SanitizeName(Field.Name, UsedNames)+': '+ Field.TranslatedName+'; //union extracted from object and named '''+Field.TranslatedName+'''',4,0));
            end
       end;
    end;

  end;
  function GetParentType(AClass: TgirClass): String;
  begin
    Result := '';
    AssembleUsedFieldNamesFromParent(AClass.ParentClass, UsedNames);
    if AClass.ParentClass = nil then
      Exit;
    if AClass.ParentClass.Writing < msWritten then
      ProcessType(AClass.ParentClass, True); // this type must be first

    Result := AClass.ParentClass.TranslatedName;
    if Result = '' then
    begin
      WriteLn('Class has parent but name is empty! : ', AClass.CType);
      WriteLn('Parent Name = ', AClass.ParentClass.Name);
      WriteLn('Parent CType = ', AClass.ParentClass.CType);
      WriteLn('Parent Translated Name = ', AClass.ParentClass.TranslatedName);
      Halt
    end;
  end;
  procedure AddGetTypeProc(AObj: TgirGType);
  const
    GetTypeTemplate = 'function %s: %s; cdecl; external;';
    GetTypeTemplateDyn = '%s: function:%s; cdecl;';
  var
    AType: String;
  begin
    AType:='TGType';
    if (AObj.GetTypeFunction = '') or (AObj.GetTypeFunction = 'none') or (AObj.GetTypeFunction = 'intern') then
      Exit;
    if not NameSpace.UsesGLib then
      AType := 'csize_t { TGType }';

    if not (goLinkDynamic in FOptions) then
      UnitFuncs.Add(Format(GetTypeTemplate, [AObj.GetTypeFunction, AType]))
    else
    begin
      UnitFuncs.Add(Format(GetTypeTemplateDyn, [AObj.GetTypeFunction, AType]));
      FDynamicEntryNames.Add(AObj.GetTypeFunction);
    end;
  end;

var
  TypeSect: TPDeclarationType;
  AddedBitSizedType: Boolean;
  ProperUnit: TPascalUnit = nil;
begin
  case AItem.ObjectType of
    otObject:      ProperUnit := FGroup.GetUnitForType(utTypes);
    otClassStruct: ProperUnit := FGroup.GetUnitForType(utTypes); //class structs go in types!
    otInterface:   ProperUnit := FGroup.GetUnitForType(utTypes);
    otGType:       ProperUnit := FGroup.GetUnitForType(utTypes);
    otClass :
      begin
        if goClasses in FOptions then
          ProperUnit := FGroup.GetUnitForType(utClasses)
        else if goObjects in FOptions then
          ProperUnit := FGroup.GetUnitForType(utObjects)
        else
          ProperUnit := Self;
      end;
  else
    WriteLn('Unknown ObjectType : ', AObjectType);
    Halt;
  end;
  if ProperUnit = nil then
  begin
    WriteLn('ProperUnit is not assigned! : ', AObjectType);
    Halt;
  end;
  if ProperUnit <> Self then
  begin
    ProperUnit.HandleObject(AItem, AObjectType);
    Exit;
  end;

  if AItem.CType = '' then
    Exit;
  // if any params use a type that is not written we must write it before we use it!!
  TypeDecl := TStringList.Create;
  UsedNAmes := TStringList.Create;
  UsedNames.Sorted:=True;
  UsedNames.Duplicates:=dupError;
  ResolveTypeTranslation(AItem);
  AItem.ImpliedPointerLevel:=1; //will only grow

  // forces it to write forward declarations if they are not yet.
  ProcessType(AItem);

  UnitFuncs := TStringList.Create;
  TypeFuncs := TStringList.Create;

  case AObjectType of
    gtObject :; // do nothing
    gtClass : ParentType:=ParenParams(GetParentType(TgirClass(AItem)));
    gtClassStruct : ;// do nothing;
    gtInterface: ;
    gtGType: ;
  else
    WriteLn('Got Object Type I don''t understand: ', GirTokenName[AObjectType]);
  end;

  if AItem.InheritsFrom(TgirGType) then
  begin
    AddGetTypeProc(TgirGType(AItem));
  end;
  TypeDecl.Add(IndentText(AItem.TranslatedName +' = object'+ParentType,2,0));

  // two passes to process the fields last for naming reasons first for methods/properties second for fields
  for i := 0 to Aitem.Fields.Count-1 do
    HandleFieldType(AItem.Fields.Field[i], True, AddedBitSizedType);
  if AItem.CType <> 'GInitiallyUnowned' then // empty type GInitiallyUnowned is empty and aliased to GObject which causes
                                             // object introspection to add the types again which causes size mismatches
                                             // since it's supposed to be empty...how many places does that happen...
  begin
    WrittenFields:=0;
    for i := 0 to Aitem.Fields.Count-1 do begin
      HandleFieldType(AItem.Fields.Field[i], False, AddedBitSizedType);
      if HasPackedBitfield(PackedBits) and (not AddedBitSizedType or (i = AItem.Fields.Count-1) )then
        WantTypeSection.Lines.Add(EndPackedBits(PackedBits));
    end;
  end;

  if TypeFuncs.Count > 0 then
    TypeDecl.AddStrings(TypeFuncs);

  TypeDecl.Add('  end;');

  TypeSect := WantTypeSection;

  TypeSect.Lines.AddStrings(TypeDecl);
  TypeDecl.Free;
  UsedNames.Free;

  if UnitFuncs.Count > 0 then
    FGroup.GetUnitForType(utFunctions).WantFunctionSection.Lines.AddStrings(UnitFuncs);
  UnitFuncs.Free;
  TypeFuncs.Free;

end;

procedure TPascalUnit.HandleUnion(AItem: TgirUnion);
var
  ProperUnit: TPascalUnit;
begin
  ProperUnit := FGroup.GetUnitForType(utTypes);
  if ProperUnit <> Self then
  begin
    ProperUnit.HandleUnion(AItem);
    Exit;
  end;
  ResolveTypeTranslation(AItem);
  WantTypeSection.Lines.Add(WriteUnion(AItem, False, 2));

end;

procedure TPascalUnit.WriteForwardDefinition(AType: TGirBaseType);
  procedure WriteForward;
  var
    TypeSect: TPDeclarationType;
  begin
    TypeSect := WantTypeSection;
    ResolveTypeTranslation(AType);
    AType.ImpliedPointerLevel := 1; // will only grow
    TypeSect.Lines.Add('');
    //TypeSect.Lines.Add('  { forward declaration for '+AType.TranslatedName+'}');
    WritePointerTypesForType(AType, AType.TranslatedName, AType.ImpliedPointerLevel, TypeSect.Lines);
  end;

begin
  if AType.InheritsFrom(TgirFuzzyType) and (TgirFuzzyType(AType).ResolvedType <> nil) then
  begin
    TgirFuzzyType(AType).ResolvedType.ImpliedPointerLevel := AType.ImpliedPointerLevel;
    AType := TgirFuzzyType(AType).ResolvedType;
  end;

  if AType.ForwardDefinitionWritten then
    Exit;

  WriteForward;
  case AType.ObjectType of
    otObject,
    otGType,
    otClass,
    otClassStruct:   ;

    otAlias:         ProcessType(AType, True);
    otCallback:      ProcessType(AType, True);
    otEnumeration:   ;
    otBitfield:      ;
    otRecord:        ;
    otFunction:      ;
    otNativeType     : ;
    otInterface:     ;
  end;
  Atype.ForwardDefinitionWritten:=True;
end;

procedure TPascalUnit.WriteWrapperForObject(ARoutineType, AObjectName,
  AObjectFunctionName: String; AParams: TgirParamList; AFunctionReturns: String; AFlatFunctionName: String; AWantSelf: Boolean);
const
  Decl = '%s %s.%s%s%s'+LineEnding;
  Body = 'begin'+LineEnding+
         '  %s%s(%s);'+LineEnding+
         'end;'+LineEnding;
var
  Params: String;
  CallParams: String;
  Code: TPCodeText;
  ResultStr: String = '';
  Args: String;
begin
  if AWantSelf then
  begin
    if AParams.Count = 0 then
      CallParams:='@self'
    else
      CallParams:='@self, ';
  end
  else
    CallParams:='';
  if (ARoutineType = 'function') or (ARoutineType='constructor') then
    ResultStr := 'Result := ';
  Params:=WriteFunctionParams(AParams, @Args);
  CallParams:=CallParams+Args;
  Code := TPCodeText.Create;
  Code.Content := Format(Decl, [ARoutineType, AObjectName, AObjectFunctionName, ParenParams(Params), AFunctionReturns])+
                  Format(Body, [ResultStr, FGroup.UnitForType[utFunctions].UnitFileName+'.'+AFlatFunctionName, CallParams]);
  ImplementationSection.Declarations.Add(Code);


end;

function TPascalUnit.WriteCallBack(AItem: TgirFunction; IsInObject: Boolean; AExistingUsedNames: TStringList = nil): String;
var
  RoutineType: String;
  Returns: String;
  CBName: String;
  Symbol: String;
  Params: String;
begin
  WriteFunctionTypeAndReturnType(AItem, RoutineType, Returns);

  if IsInObject then
  begin
    CBName:=SanitizeName(AItem.Name, AExistingUsedNames);
    Symbol := ': ';
  end
  else
  begin
    CBName:=MakePascalTypeFromCType(AItem.CType);
    Symbol := ' = ';
  end;

  Params := WriteFunctionParams(AItem.Params);

  Result := CBName+Symbol+RoutineType+ParenParams(Params)+Returns;

end;

procedure TPascalUnit.WriteFunctionTypeAndReturnType(AItem: TgirFunction;
  out AFunctionType, AFunctionReturnType: String);
begin
  ResolveTypeTranslation(AItem.Returns.VarType);
  if (AItem.Returns.VarType.CType = 'void') and (AItem.Returns.PointerLevel = 0) then
  begin
    AFunctionType:='procedure';
    AFunctionReturnType := '; cdecl;';
  end
  else
  begin
    AFunctionType:='function';
    AFunctionReturnType:= ': '+TypeAsString(AItem.Returns.VarType, AItem.Returns.PointerLevel, AItem.Returns.CType)+'; cdecl;' ;

    // will skip if written
    ProcessType(AItem.Returns.VarType);
  end;
end;

function TPascalUnit.WriteFunctionParams(AParams: TgirParamList; AArgs: PString = nil): String;
var
  i: Integer;
  ArgName: String;
  Dummy: Boolean;
begin
  Result := '';
  if AArgs <> nil then
    AArgs^ := '';
  for i := 0 to AParams.Count-1 do
    begin
      Result := Result+WriteParamAsString(AParams.Param[i], i, Dummy, @ArgName);
      if i < AParams.Count-1 then
      begin
        Result := Result +'; ';
        if AArgs <> nil then
          AArgs^:=AArgs^+ArgName+', ';
      end
      else
        if AArgs <> nil then
          AArgs^:=AArgs^+ArgName;
    end;
end;

function TPascalUnit.TypeAsString(AType: TGirBaseType; APointerLevel: Integer; ACTypeAsBackup: String = ''): String;
var
  BackupNoPointers: String;
  TranslatedName: String;
begin
  ResolveTypeTranslation(AType);
  TranslatedName := AType.TranslatedName;

  BackupNoPointers := StringReplace(ACTypeAsBackup, '*', '', [rfReplaceAll]);

  // some types are pointers but contain no "*" so it thinks it has a pointer level 0 when really it's 1
  if (APointerLevel = 0) and (ACTypeAsBackup = 'gpointer') and (TranslatedName <> '') and (TranslatedName <> 'gpointer') then
  begin
    APointerLevel := 1;
  end;

  if APointerLevel = 0 then
  begin
    Result := AType.TranslatedName;
    if Result = '' then
      Result := NameSpace.LookupTypeByName(BackupNoPointers, '').TranslatedName;
  end
  else
  begin
    if AType.CType = '' then
      AType.CType:=ACTypeAsBackup;
    Result := MakePascalTypeFromCType(AType.CType, APointerLevel);
  end;
  if APointerLevel > AType.ImpliedPointerLevel then
  begin
    WriteLn('Trying to use a pointerlevel > written level!');
    Halt;
  end;
end;

procedure TPascalUnit.AssembleUsedFieldNamesFromParent(const AParent: TgirClass; var AUsedNamesList: TStringList);
var
  Field: TGirBaseType;
  i: Integer;
begin
  if AParent = nil then
    Exit;

  AssembleUsedFieldNamesFromParent(AParent.ParentClass, AUsedNamesList);
  for i := 0 to AParent.Fields.Count-1 do
    begin
      Field := AParent.Fields.Field[i];
      case Field.ObjectType of
        otArray,
        otTypeParam,
        otCallback,
        otProperty:
            begin
              // adds name to list
              SanitizeName(Field.Name, AUsedNamesList);
            end;
      end;
    end;
end;

function TPascalUnit.WriteParamAsString(AParam: TgirTypeParam; AIndex: Integer; out ABitSizeSpecified: Boolean; AFirstParam: PString = nil; AExistingUsedNames: TStringList = nil): String;
var
  PT: String;
  PN: String;
  IsArray: Boolean;
  AnArray: TgirArray absolute AParam;
begin
  ABitSizeSpecified:=False;
  if AParam.VarType = nil then
  begin
    // is a varargs param
    Result := 'args: array of const';// 'args: varargs'; // varargs must be append to the function definition also this is more clear to the user
    exit;
  end;


  IsArray := AParam.InheritsFrom(TgirArray) ;

  //if Length(AParam.VarType.Name) < 1 then
  //begin
    //WriteLn('AParam.VarType.Name is empty. AParam.Name = ', AParam.Name,' AParam.CType = ', AParam.CType, ' AParam.VarType.CType = ',AParam.VarType.CType);
  //end;
  PT := '';
  if IsArray and (AnArray.FixedSize > 0) then
    PT := 'array [0..'+IntToStr(TgirArray(AParam).FixedSize-1)+'] of ' ;
  PT := PT+ TypeAsString(AParam.VarType, AParam.PointerLevel, AParam.CType);

  if IsArray and (AnArray.FixedSize = 0) then
    PN := AnArray.ParentFieldName
  else
    PN := AParam.Name;


  if PN = '' then
    PN := 'param'+IntToStr(AIndex);
  PN := SanitizeName(PN, AExistingUsedNames);

  if AFirstParam <> nil then
    AFirstParam^:=PN;

   if AParam.Bits > 0 then
  begin
    ABitSizeSpecified:=True;
    case AParam.Bits of
      //16: PT := 'guint16 { changed from '+PT+' to accomodate 16 bitsize requirement }';
      //32: PT := 'guint32 { changed from '+PT+' to accomodate 32 bitsize requirement }';
      1..32:
          PT := Format('guint%d { changed from %s to accomodate %d bitsize requirement }',[AParam.Bits, PT, AParam.Bits]);
    else
      WriteLn('WARNING: Bits are Set to [ ',AParam.Bits,' ]for: ' ,PN+': '+PT);
      PT +=' { ERROR : Bits are Set to [ '+IntToStr(AParam.Bits)+' ]  }';
    end;

  end;
  Result := PN +': '+PT;

  ProcessType(AParam.VarType, AParam.PointerLevel = 0); // will skip if written
end;

function TPascalUnit.WriteRecord(ARecord: TgirRecord; ABaseIndent: Integer = 0; AIsUnion: Boolean = False): String;
var
  PackedBits: TStringList = nil;
  PackedBitsCount: Integer = 0;
  AddedBitSizedType: Boolean;
  TypeDecl: TStringList;
  i: Integer;
  function AddField(AField: TGirBaseType): Boolean;
  var
    Param: String;
//    Iten
  begin
    Result := False;
    Param := WriteParamAsString(TgirTypeParam(AField),i, Result);
    if Result and not AIsUnion then
      PackedBitsAddEntry(PackedBits, ARecord, PackedBitsCount, Param, TypeDecl)
    else
      TypeDecl.Add(IndentText(Param+';',ABaseIndent+4,0));
  end;
var
  Field: TGirBaseType;
  UseName: String;
  Symbol: String;

begin
  TypeDecl := TStringList.Create;
  TypeDecl.Add('');
  if Not AIsUnion then
  begin
    UseName:=ARecord.TranslatedName;
    Symbol := ' = ';
  end
  else
  begin
    UseName:=ARecord.Name;
    Symbol:= ' : ';
  end;
  TypeDecl.Add(IndentText(UseName +Symbol+ 'record',ABaseIndent+2,0));

  // If a type size = 0 then this can cause problems for the compiler! bug 20265
  //if ARecord.Fields.Count = 0 then
  //  TypeDecl.Add(IndentText('Unknown: Pointer;', ABaseIndent+4,0));

  for i := 0 to ARecord.Fields.Count-1 do
    begin
      AddedBitSizedType:=False;
      Field := ARecord.Fields.Field[i];
      case Field.ObjectType of
        otArray,
        otTypeParam: AddedBitSizedType := AddField(Field);
        otCallback : TypeDecl.Add(IndentText(WriteCallBack(TgirCallback(Field),True),ABaseIndent+4,0));
        otUnion: TypeDecl.Add(IndentText(WriteUnion(TgirUnion(Field), True, ABaseIndent),ABaseIndent+4));
      else
        TypeDecl.Add(IndentText(Field.Name+ ' ' + Field.ClassName,4,0)); // this of course will make the compiler barf
      end;
      if HasPackedBitfield(PackedBits) and ((i = ARecord.Fields.Count-1)  or (not AddedBitSizedType)) then
        WantTypeSection.Lines.Add(EndPackedBits(PackedBits));

    end;
  TypeDecl.Add(IndentText('end;',ABaseIndent+2,1));
  Result := TypeDecl.Text;
end;

function TPascalUnit.WriteUnion(AUnion: TgirUnion; ASkipRecordName: Boolean; ABaseIndent: Integer
  ): String;
var
  Union: TStringList;
  i: Integer;
  Field: TGirBaseType;
  Dummy: Boolean;
begin
  Union := TStringList.Create;

  if not ASkipRecordName then
    Union.Add(IndentText(AUnion.TranslatedName+' = record', ABaseIndent,0));
  if AUnion.Fields.Count > 0 then
    Union.Add(IndentText('case longint of',ABaseIndent+2,0));
  for i := 0 to AUnion.Fields.Count-1 do
    begin
      Field := AUnion.Fields.Field[i];
      case Field.ObjectType of
        otArray,
        otTypeParam   : Union.Add(IndentText(IntToStr(i)+ ' : ' +ParenParams(WriteParamAsString(TgirTypeParam(Field),i, Dummy))+';',ABaseIndent+ 4,0));
        otCallback    : Union.Add(IndentText(IntToStr(i)+ ' : ' +ParenParams(WriteCallBack(TgirCallback(Field),True)),ABaseIndent+4,0));
        otRecord      : Union.Add(IndentText(IntToStr(i)+ ' : ' +ParenParams(WriteRecord(TgirRecord(Field),6, True))+';',ABaseIndent+4,0));
           //WriteFunction(AFunction, AItem, AIsMethod, AWantWrapperForObject, AFunctionList): String;
        otConstructor,
        otFunction    : Union.Add(IndentText('//'+WriteFunction(TgirFunction(Field), AUnion, False, False, WantFunctionSection.Lines), ABaseIndent+2,0));
        otMethod      : Union.Add(IndentText('//'+WriteFunction(TgirFunction(Field), AUnion, True, False, WantFunctionSection.Lines), ABaseIndent+2,0));
      else
        Union.Add('// Unhandled type for Union: '+ Field.ClassName); // this won't compile obviously
        WriteLn('Unhandled type for Union: ', Field.ClassName);
      end;

    end;
    if not ASkipRecordName then
      Union.Add(IndentText('end;', ABaseIndent));
    REsult := Union.Text;
    Union.Free;

end;

function TPascalUnit.ParenParams(const AParams: String; const AForceParens: Boolean = False): String;
begin
  Result := '';
  if (AParams <> '') or AForceParens then
    Result := '('+AParams+')';
end;

procedure TPascalUnit.WriteDynamicLoadUnloadProcs;
var
  Dyn: TStrings;
  Libs: TStringList;
  LibNames: array of string;
  InitCode: TPCodeText;
  FinalCode: TPCodeText;
   procedure AddLibVars;
   var
     Lib: String;
     i: Integer;
   begin
     Dyn.Add('var');
     SetLength(LibNames, Libs.Count);
     for i := 0 to Libs.Count-1 do
     begin
       Lib := Libs[i];
       LibNames[i] := SanitizeName(Lib);
       Dyn.Add('  '+LibNames[i] + ': TLibHandle;');
     end;
   end;

   procedure WriteLoadLibrary;
   var
     i: Integer;
   begin
     Dyn.Add('procedure LoadLibraries;');
     Dyn.Add('begin');
     for i := 0 to Libs.Count-1 do
       Dyn.Add('  '+LibNames[i]+' := SafeLoadLibrary('''+Libs[i]+''');');
     Dyn.Add('end;');
     Dyn.Add('');

     InitCode.Lines.Add('LoadLibraries;');

   end;
   procedure WriteLoadProcs;
   var
     i: Integer;
   begin
     Dyn.Add('procedure LoadProcs;');
     Dyn.Add('  procedure LoadProc(var AProc: Pointer; AName: String);');
     Dyn.Add('  var');
     Dyn.Add('    ProcPtr: Pointer;');
     Dyn.Add('  begin');
     Dyn.Add('    ProcPtr := GetProcedureAddress('+LibNames[0]+', AName);');
     if Libs.Count > 0 then
     begin
       for i := 1 to Libs.Count-1 do begin
       Dyn.Add('      if ProcPtr = nil then');
       Dyn.Add('        ProcPtr := GetProcedureAddress('+LibNames[i]+', AName);');
       end;
     end;
     Dyn.Add('    AProc := ProcPtr;');
     Dyn.Add('  end;');
     // Now the Main procedure starts
     Dyn.Add('begin');
     for i := 0 to FDynamicEntryNames.Count-1 do
     Dyn.Add('  LoadProc(Pointer('+FDynamicEntryNames[i]+'), '''+FDynamicEntryNames[i]+''');');
     Dyn.Add('end;');
     Dyn.Add('');

     InitCode.Lines.Add('LoadProcs;');

   end;

   procedure WriteUnloadLibrary;
   var
     Tmp: String;
   begin
     Dyn.Add('procedure UnloadLibraries;');
     Dyn.Add('begin');
     for Tmp in LibNames do
     begin
     Dyn.Add(' if '+ Tmp+ ' <> 0 then');
     Dyn.Add('   UnloadLibrary('+Tmp+');');
     Dyn.Add('   '+Tmp+' := 0;');
     end;
     for Tmp in FDynamicEntryNames do
     Dyn.Add('  '+Tmp+' := nil;');
     Dyn.Add('end;');
     Dyn.Add('');

     FinalCode.Lines.Add('UnloadLibraries;');
   end;

begin
  if FDynamicEntryNames.Count = 0 then
    Exit;
  Libs := GetLibs;
  if Libs.Count = 0 then
  begin
    Libs.Free;
    Exit;
  end;
  Dyn := FDynamicLoadUnloadSection.Lines;
  InitCode := TPCodeText.Create;
  FinalCode := TPCodeText.Create;
  InitializeSection.Declarations.Add(InitCode);
  FinalizeSection.Declarations.Add(FinalCode);


  AddLibVars;
  WriteLoadLibrary;
  WriteLoadProcs;
  WriteUnloadLibrary;
  Libs.Free;
end;

function TPascalUnit.GetLibs: TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter:=',';
  Result.StrictDelimiter:= True;
  Result.CommaText:=NameSpace.SharedLibrary;
end;

function TPascalUnit.SanitizeName(AName: String; AExistingUsedNames: TStringList = nil): String;
var
  PascalReservedWords : array[0..30] of String =
    ('begin', 'end', 'type', 'of', 'in', 'out', 'function', 'string','file', 'default',
     'procedure', 'string', 'boolean', 'array', 'set', 'destructor', 'destroy', 'program',
     'property', 'object', 'private', 'constructor', 'inline', 'result', 'interface',
     'const', 'raise', 'unit', 'label', 'xor', 'implementation');
  Name: String;
  Sanity: Integer = 0;
  Sucess: Boolean;
  TestName: String;
begin
  for Name in PascalReservedWords do
    if Name = LowerCase(AName) then
      Result := Aname+'_';
  If Result = '' then
    Result := AName;
  if AName = 'CSET_A_2_Z' then
    Result := 'CSET_A_2_Z_UPPER';
  if AName = 'CSET_a_2_z' then
    Result := 'CSET_a_2_z_lower';
  Result := StringReplace(Result, '-','_',[rfReplaceAll]);
  Result := StringReplace(Result, ' ','_',[rfReplaceAll]);
  Result := StringReplace(Result, '.','_',[rfReplaceAll]);

  if AExistingUsedNames <> nil then
  begin
    // AExistingUsedNames must be set to sorted and duplucate strings caues an error;
    TestName:=Result;
    repeat
      Inc(Sanity);
      try
        AExistingUsedNames.Add(TestName);
        Result := TestName;
        Sucess := True;
      except
        TestName := Result + IntToStr(Sanity);
        Sucess := False;
      end;

    until Sucess or (Sanity > 300);
  end;

end;

procedure TPascalUnit.ResolveTypeTranslation(ABaseType: TGirBaseType);
var
  RawName: String;
begin
  if ABaseType.TranslatedName = '' then
  begin
    RawName := ABaseType.CType;
    if RawName = '' then
      RawName:= ABaseType.Name;
    ABaseType.TranslatedName:=MakePascalTypeFromCType(RawName, 0);
  end;
end;

constructor TPascalUnit.Create(AGroup: TPascalUnitGroup; ANameSpace: TgirNamespace; AOptions: TgirOptions; AUnitType: TPascalUnitTypes);
const
  //CBasic = '#include <%s>'+LineEnding;
  PBasic = 'program %s_test;'+LineEnding+
           //'{$LINK %s_c_test}'+LineEnding+
           '{$MODE OBJFPC}'+LineEnding+
           'uses GLib2, GObject2, %s;'+LineEnding;
  GTypeSize = 'function GTypeSize(AType: TGType; out AClassSize: Integer): Integer;'+LineEnding+
              'var'                                       +LineEnding+
              '  Query: TGTypeQuery;'                     +LineEnding+
              'begin'                                     +LineEnding+
              '  g_type_query(AType, @Query);'            +LineEnding+
              '  AClassSize := Query.Class_Size;'         +LineEnding+
              '  GTypeSize := Query.instance_size;'       +LineEnding+
              '  if GTypeSize = 32767 then GTypeSize := 0;'       +LineEnding+
              '  if AClassSize = 32767 then AClassSize := 0;'       +LineEnding+
              'end;'+LineEnding;
begin
  ProcessLevel:=0;
  FGroup := AGroup;
  FOptions := AOptions;
  FUnitType:=AUnitType;
  FFinalizeSection := TPFinialization.Create(Self);
  FImplementationSection := TPImplementation.Create(Self);
  FInitializeSection := TPInitialize.Create(Self);
  FInterfaceSection := TPInterface.Create(Self, TPUses.Create, goLinkDynamic in FOptions);
  FDynamicLoadUnloadSection := TPCodeText.Create;
  FDynamicEntryNames := TStringList.Create;
  FDynamicEntryNames.Sorted:=True;
  FDynamicEntryNames.Duplicates := dupIgnore;
  FNameSpace := ANameSpace;
  if goWantTest in FOptions then
  begin
    //FTestCFile := TStringStream.Create('');
    //FTestCFile.WriteString(Format(CBasic, [FNameSpace.CIncludeName]));
    FTestPascalFile := TStringStream.Create('');
    FTestPascalFile.WriteString(Format(PBasic,[UnitName, UnitName, UnitName]));
    FTestPascalFile.WriteString(GTypeSize);
    FTestPascalBody := TStringList.Create;
    FTestPascalBody.Add('begin');
    FTestPascalBody.Add(' g_type_init();');

  end;
  ResolveFuzzyTypes;
end;

destructor TPascalUnit.Destroy;
begin
  if goWantTest in FOptions then
  begin
    FTestPascalFile.Free;
    //FTestCFile.Free;
    FTestPascalBody.Free;
  end;
  FFinalizeSection.Free;
  FImplementationSection.Free;
  FInitializeSection.Free;
  FInterfaceSection.Free;
  FDynamicLoadUnloadSection.Free;
  FDynamicEntryNames.Free;

  inherited Destroy;
end;

procedure TPascalUnit.ProcessConsts(AList: TList);
  function WriteConst(AConst: TgirConstant; Suffix: String = ''): String;
  begin
    if AConst.IsString then
      Result := SanitizeName(AConst.Name) + Suffix+' = '+QuotedStr(AConst.Value)+';'
    else
      Result := SanitizeName(AConst.Name) + Suffix+' = '+AConst.Value+';';
  end;

var
  NewConst: TPDeclarationConst;
  Item: TgirConstant;
  i: Integer;
  Consts: TStringList; // this is to check for duplicates
  Entry: String;
  Suffix: String;
  Sanity: Integer;
begin
  NewConst :=  WantConstSection;
  Consts := TStringList.Create;
  Consts.Sorted:=True;
  Consts.Duplicates:=dupError;


  for i := 0 to AList.Count-1 do
    begin
      Sanity := 0;
      Suffix := '';
      Item := TgirConstant(AList.Items[i]);
      //if Item.ClassType <> TgirConstant then ; // raise error
        Entry := LowerCase(SanitizeName(Item.Name));

      repeat
        try
          Consts.AddObject(Entry, TObject(PtrUInt(NewConst.Lines.Count)));
          break;
        except
          Suffix := '__'+IntToStr(Sanity);
          Entry := LowerCase(SanitizeName(Item.Name))+Suffix;
        end;
        Inc(Sanity);
      until Sanity > 10;

      NewConst.Lines.AddObject(IndentText(WriteConst(Item, Suffix), 2,0), Item);
    end;
end;

procedure TPascalUnit.ProcessTypes(AList: TFPHashObjectList);

var
  BaseType: TGirBaseType;
  i: Integer;
begin
  if AList.Count = 0 then
    Exit;

  for i := 0 to AList.Count-1 do
  begin
    BaseType := TGirBaseType(AList.Items[i]);
    ProcessType(BaseType);
  end;

end;

procedure TPascalUnit.ProcessFunctions(AList: TList);
var
  i: Integer;
  Func: TgirFunction;
begin
  for i := 0 to AList.Count-1 do
  begin
    Func := TgirFunction(AList.Items[i]);
    HandleFunction(Func);
  end;
end;

procedure TPascalUnit.GenerateUnit;
var
  i: Integer;
  NS: TgirNamespace;
  ImplementationUses: TPUses;
  NeedUnit: String;
begin
  for i := 0 to FNameSpace.RequiredNameSpaces.Count-1 do
  begin
    NS := TgirNamespace(FNameSpace.RequiredNameSpaces.Items[i]);
    NeedUnit:=CalculateUnitName(NS.NameSpace,NS.Version);

    if FUnitType = PascalUnitTypeAll then
      InterfaceSection.UsesSection.Units.Add(' '+NeedUnit)
    else
    begin
      InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Common');
      if (utClasses in FUnitType) then
        InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Classes');
      if (utObjects in FUnitType) then
        InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Objects');
    end;


    {case FUnitType of
      utSimple: InterfaceSection.UsesSection.Units.Add(' '+NeedUnit);
      utConsts: ; // do nothing

      utFunctions,
      utTypes:
        begin
          InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Consts');
          InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Types');
        end;

      utObjects,
      utClasses:
        begin
          InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Consts');
          InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Types');
          InterfaceSection.UsesSection.Units.Add(' '+NeedUnit+'Functions');
        end;
    end;}
  end;
  if utFunctions in FUnitType then
  begin
    if goLinkDynamic in FOptions then
    begin
      ImplementationUses := TPUses.Create;
      ImplementationUses.Units.Add('DynLibs');
      ImplementationSection.Declarations.Add(ImplementationUses);
    end
    else // Not linking dynamically
    begin
      i := Pos(',',NameSpace.SharedLibrary);
      if i > 0 then
        LibName:=Copy(NameSpace.SharedLibrary,1,i-1)
      else
        LibName:=NameSpace.SharedLibrary;
      WantConstSection.Lines.Add(IndentText(UnitName+'_library = '''+LibName+''';', 2));
    end;
  end;

  if NameSpace.NameSpace = 'GLib' then
    AddGLibSupportCode;

end;

function TPascalUnit.AsStream: TStringStream;
var
  Str: TStringStream absolute Result;
  Libs: TStringList;
  i: Integer;
begin
  Libs := GetLibs;
  Result := TStringStream.Create('');
  Str.WriteString(IndentText('{ This is an autogenerated unit using gobject introspection (gir2pascal). Do not Edit. }',0,1));
  Str.WriteString(IndentText('unit '+ UnitFileName+';',0,2));
  Str.WriteString(IndentText('{$MODE OBJFPC}{$H+}',0,2));
  if utTypes in FUnitType then
    Str.WriteString(IndentText('{$PACKRECORDS C}',0,1));
  //Str.WriteString(IndentText('{$BITPACKING ON}',0,1)); not needed since we set records that need it to bitpacked
  //Str.WriteString(IndentText('{$CALLING CDECL}',0,2));
  //if FUnitType in [utSimple, utObjects] then
  Str.WriteString(IndentText('{$MODESWITCH DUPLICATELOCALS+}',0,2));

  if (utFunctions in FUnitType) and not (goLinkDynamic in FOptions) then
    for i := 0 to Libs.Count-1 do
      Str.WriteString(IndentText('{$LINKLIB '+Libs.Strings[i]+'}',0,1));

  Libs.Free;

  Str.WriteString(InterfaceSection.AsString);
  Str.WriteString(ImplementationSection.AsString);

  if (goLinkDynamic in FOptions) then
  begin
    WriteDynamicLoadUnloadProcs;
    Str.WriteString(DynamicLoadUnloadSection.AsString);
  end;

  if InitializeSection.Declarations.Count > 0 then
     Str.WriteString(InitializeSection.AsString);

  if FinalizeSection.Declarations.Count > 0 then
     Str.WriteString(FinalizeSection.AsString);

  Str.WriteString('end.');

  Result.Position:=0;
end;

procedure TPascalUnit.Finish;
begin
  if (goWantTest in FOptions) then
  begin
    FTestPascalFile.WriteString(FTestPascalBody.Text);
    FTestPascalFile.WriteString('end.');
    //FTestCFile.Position:=0;
    FTestPascalFile.Position:=0;
  end;
end;

{ TPDeclarationList }

function TPDeclarationList.GetDeclarations(AIndex: Integer): TPDeclaration;
begin
  Result := TPDeclaration(Items[AIndex]);
end;

function TPDeclarationList.AsString: String;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    begin
      Result := Result+Declarations[i].AsString+LineEnding;
    end;
end;




end.
