{
girpascalwriter.pas
Copyright (C) 2011  Andrew Haines andrewd207@aol.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit girpascalwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, girNameSpaces, girObjects, girTokens, contnrs;

type
  TgirWriteEvent = procedure (Sender: TObject; AUnitName: AnsiString; AStream: TStringStream) of object;

  { TgirPascalWriter }

  TgirPascalWriter = class
  private
    FDefaultUnitExtension: String;
    FOnUnitWriteEvent: TgirWriteEvent;
    FNameSpaces: TgirNamespaces;
    FUnits: TList;
    FWantTest: Boolean;
  public
     constructor Create(ANameSpaces: TgirNamespaces; AWantTest: Boolean);
     procedure GenerateUnits;
     property OnUnitWriteEvent: TgirWriteEvent read FOnUnitWriteEvent write FOnUnitWriteEvent;
     property DefaultUnitExtension: String read FDefaultUnitExtension write FDefaultUnitExtension; // is .pas by default
  end;


implementation
uses girCTypesMapping;

type
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
    constructor Create; override;
    // nothing special for this one
  end;

  { TPCodeText }

  TPCodeText = class(TPDeclaration)
  private
    FContent: String;
  public
    function AsString: String; override;
    property Content: String read FContent write FContent;

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

  { TPInterface }

  TPInterface = class(TPCommonSections)
  private
    FConstSection: TPDeclarationConst;
    FFunctionSection: TPDeclarationFunctions;
    FUsesSection: TPUses;
  public
    constructor Create(AOwner: TObject; AUses: TPUses);
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

  { TPascalUnit }

  TPascalUnit = class
  private
    FLinkDynamic: Boolean;
    FFinalizeSection: TPFinialization;
    FImplementationSection: TPImplementation;
    FInitializeSection: TPInitialize;
    FInterfaceSection: TPInterface;
    FLibName: String;
    FNameSpace: TgirNamespace;
    FWantTest: Boolean;
    ProcessLevel: Integer; //used to know if to write forward definitions
    FTestCFile: TStringStream;
    FTestPascalFile: TStringStream;
    FTestPascalBody: TStringList;
    function GetUnitName: String;

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
    procedure AddTestType(APascalName: String; ACName: String);
  public
    constructor Create(ANameSpace: TgirNamespace; ALinkDynamic: Boolean; AWantTest: Boolean);
    destructor Destroy; override;
    procedure ProcessConsts(AList:TList); // of TgirBaseType descandants
    procedure ProcessTypes(AList:TFPHashObjectList); // of TgirBaseType descandants
    procedure ProcessFunctions(AList:TList);// of TgirFunction
    procedure GenerateUnit;
    function AsStream: TStringStream;
    procedure Finish;

    property InterfaceSection: TPInterface read FInterfaceSection;
    property ImplementationSection: TPImplementation read FImplementationSection;
    property InitializeSection: TPInitialize read FInitializeSection;
    property FinalizeSection: TPFinialization read FFinalizeSection;
    property UnitName: String read GetUnitName;
    property LibName: String read FLibName write FLibName;
    property NameSpace: TgirNamespace read FNameSpace;
  end;

function IndentText(const AText: String; Spaces: Integer = 0; LineEndingCount: Integer = 1): String;
begin
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

constructor TPDeclarationFunctions.Create;
begin
  inherited Create;
  Lines.Duplicates:=dupIgnore;
  Lines.Sorted:=True;
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


function TPCodeText.AsString: String;
begin
  Result := Content;
end;

{ TPDeclarationType }

function TPDeclarationType.AsString: String;
begin
  Result:= IndentText('type') + Lines.Text;
end;

{ TPDeclarationConst }

function TPDeclarationConst.AsString: String;
begin
  Result:= IndentText('const') + Lines.Text;
end;

{ TPUses }

constructor TPUses.Create;
begin
  Units := TStringList.Create;
  Units.StrictDelimiter:=True;
  Units.Delimiter:=',';
  Units.Add('CTypes');
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

constructor TPInterface.Create(AOwner: TObject; AUses: TPUses);
begin
  inherited Create(AOwner);
  FUsesSection := AUses;
  FConstSection := TPDeclarationConst.Create;
  FFunctionSection := TPDeclarationFunctions.Create;
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
const
  BitFRecord =
     '  TBitObject32 = object'                                      +LineEnding+
     '  protected'                                                  +LineEnding+
     '    procedure SetBit(AMask: Integer; AValue: DWord);'         +LineEnding+
     '    function GetBit(AMask: Integer): DWord;'                  +LineEnding+
     '  public'                                                     +LineEnding+
     '    Flags0: DWord;'                                           +LineEnding+
     '    procedure Init(AFlags: DWord);'                           +LineEnding+
     '  end;';

  BFRecordImpl :AnsiString =
     'procedure TBitObject32.Init(AFlags: DWord);'                  +LineEnding+
     'begin'                                                        +LineEnding+
     '  Flags0 := AFlags;'                                          +LineEnding+
     'end;'                                                         +LineEnding+
     ''                                                             +LineEnding+
     'procedure TBitObject32.SetBit(AMask: Integer; AValue: DWord);'+LineEnding+
     'begin'                                                        +LineEnding+
     '  if AValue <> 0 then'                                        +LineEnding+
     '  begin'                                                      +LineEnding+
     '    if (Flags0 and AMask) = 0 then'                           +LineEnding+
     '      Flags0 := Flags0 or AMask'                              +LineEnding+
     '  end'                                                        +LineEnding+
     '  else begin'                                                 +LineEnding+
     '    if (Flags0 and AMask) <> 0 then'                          +LineEnding+
     '      Flags0 := Flags0 xor AMask;'                            +LineEnding+
     '  end;'                                                       +LineEnding+
     'end;'                                                         +LineEnding+
     ''                                                             +LineEnding+
     'function TBitObject32.GetBit(AMask: Integer): DWord;'         +LineEnding+
     'begin'                                                        +LineEnding+
     '  Result := Flags0 and AMask;'                                +LineEnding+
     '  if Result > 1 then'                                         +LineEnding+
     '    Result := 1;'                                             +LineEnding+
     'end;';
var
  CodeText: TPCodeText;
  TypeSect: TPDeclarationType;
  i: Integer;
begin
  WantTypeSection.Lines.Add(BitFRecord);
  CodeText := TPCodeText.Create;
  CodeText.Content:=BFRecordImpl;
  ImplementationSection.Declarations.Add(CodeText);

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
  if (AType = nil) or (AType.Owner <> NameSpace) then
    Exit; // it's written in another Namespace

  if (AType.ObjectType = otFuzzyType) and (TgirFuzzyType(AType).ResolvedType <> nil) then
  begin
    TgirFuzzyType(AType).ResolvedType.ImpliedPointerLevel := AType.ImpliedPointerLevel;
    AType := TgirFuzzyType(AType).ResolvedType;
  end;

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
  if (AType.InheritsFrom(TgirRecord)) and (TgirRecord(AType).HasFields) then
    AddTestType(AType.TranslatedName, AType.CType);

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

procedure TPascalUnit.AddTestType(APascalName: String; ACName: String);
const
  CFunction = 'int GetSizeOf_%s(void)'+
              '{  return sizeof(%s); };'+LineEnding;
  PImport = 'function GetSizeOf_%s: LongInt; cdecl; external;'+LineEnding;
  PTest = 'procedure Test_%s;'                                                         +LineEnding+
          'var'                                                                        +LineEnding+
          '  PSize: Integer;'                                                          +LineEnding+
          '  CSize: Integer;'                                                          +LineEnding+
          'begin'                                                                      +LineEnding+
          '  PSize := SizeOf(%s);'                                                     +LineEnding+
          '  CSize := GetSizeOf_%s;'                                                   +LineEnding+
          '  if CSize = PSize then'                                                    +LineEnding+
          '    WriteLn(''%s Matches C Size: '',CSize)'                                 +LineEnding+
          '  else'                                                                     +LineEnding+
          '    WriteLn(''%s size ('',PSize,'') does NOT match %s size ('',CSize,'')'');' +LineEnding+
          'end;'                                                                       +LineEnding;
var
  CF: String;
  PI: String;
  PT: String;
begin
  if not FWantTest then
    Exit;
  if (ACName = '') or (ACName[1] = '_') then // we skip private types
    Exit;

  CF := Format(CFunction,[ACName, ACName]);
  PI := Format(PImport,  [ACName]);
  PT := Format(PTest,    [ACName, APascalName, ACName, APascalName, APascalName, ACName]);

  FTestCFile.WriteString(CF); // c sizeof wrapper
  FTestPascalFile.WriteString(PI); // c import
  FTestPascalFile.WriteString(PT); // pascal testproc
  FTestPascalBody.Add(Format('Test_%s;',[ACName])); //call pascal testproc
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
begin
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
  CType: TGirBaseType;
begin
  ResolveTypeTranslation(AItem);
  ResolveTypeTranslation(AItem.ForType);

  // some aliases are just for the parser to connect a name to an alias
  if AItem.CType = '' then
    Exit;
  ResolvedForName := aItem.ForType.TranslatedName;
  if ResolvedForName = '' then
    begin

      CType := NameSpace.LookupTypeByName('', AItem.ForType.CType);
      if CType <> nil then
         ResolvedForName := CType.TranslatedName;

      if ResolvedForName <> '' then
        aItem.ForType.TranslatedName := ResolvedForName
      else
        ResolvedForName := AItem.ForType.CType;
    end;

  WriteForwardDefinition(AItem);

  if AItem.Writing < msWritten then
    WantTypeSection.Lines.Add(IndentText(MakePascalTypeFromCType(AItem.CType)+' = '+ ResolvedForName+';' ,2,0));
end;

procedure TPascalUnit.HandleCallback(AItem: TgirCallback);
var
  TypeSect: TPDeclarationType;
  CB: String;
begin

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
begin
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

begin
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
  end;}

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

end;

procedure TPascalUnit.HandleRecord(AItem: TgirRecord);
  begin
  ResolveTypeTranslation(AItem);
  AItem.ImpliedPointerLevel:=1; // will not be decreased only will grow

  WriteForwardDefinition(AItem);

  WantTypeSection.Lines.Add(WriteRecord(AItem));

end;

procedure TPascalUnit.HandleOpaqueType(AItem: TgirFuzzyType);
var
  TypeSect: TPDeclarationType;
  Plain: String;
begin
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

end;

procedure TPascalUnit.HandleFunction(AItem: TgirFunction);
var
  RoutineType: String;
  Returns: String;
  Params: String;
  FuncSect: TPDeclarationFunctions;
  Postfix: String;
begin
  WriteFunctionTypeAndReturnType(AItem, RoutineType, Returns);
  Params := WriteFunctionParams(AItem.Params);
  Postfix := ' external;';// '+UnitName+'_library;';
  FuncSect := WantFunctionSection;
  FuncSect.Lines.Add(RoutineType +' '+ AItem.CIdentifier+ParenParams(Params)+Returns+Postfix);
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
begin
  Result := '';
  // we skip deprecated functions
  if AFunction.Deprecated and (CompareStr(AFunction.DeprecatedVersion, NameSpace.Version) >=  0) then
    Exit;

  // some abstract functions that are to be implemented by a module and shouldn't be declared. There is no indicator in the gir file that this is so :(
  if (AFunction.CIdentifier = 'g_io_module_query')
  or (AFunction.CIdentifier = 'g_io_module_load')
  or (AFunction.CIdentifier = 'g_io_module_unload')
  then
    Exit; // they are functions to be implemented by a runtime loadable module, they are not actually functions in glib/gmodule/gio

  if AWantWrapperForObject then
    InLineS:=' inline;';

  // this fills in the values for procedure/function and the return type
  WriteFunctionTypeAndReturnType(AFunction, RoutineType, Returns);

  // check if it is a constructor
  if AFunction.InheritsFrom(TgirConstructor) then
    Returns := ': '+MakePascalTypeFromCType(AItem.TranslatedName ,1)+'; cdecl;';

  Params := WriteFunctionParams(AFunction.Params);
  if Pos('array of const', Params) + Pos('va_list', Params) > 0 then
    Prefix:='//';
  Postfix := ' external;';// '+UnitName+'_library;';

  // first wrapper proc
  Entry := Prefix + RoutineType +' '+ SanitizeName(AFunction.Name, AExistingUsedNames)+ParenParams(Params)+Returns+InLineS;

  // no need to pass self that will not be used
  if (not AIsMethod) and AWantWrapperForObject then
    Entry := Entry + ' static;';

  // result will be written in the object declaration
  Result := Entry;

  // now make sure the flat proc has all the params it needs
  if AIsMethod then
  begin
    // methods do not include the first param for it's type so we have to add it
    if Params <> '' then
      Params := SanitizeName('A'+AItem.Name) +': '+TypeAsString(AItem, 1)+'; ' + Params
    else
      Params := SanitizeName('A'+AItem.Name) +': '+TypeAsString(AItem, 1);
  end;

  // this is the flat c procedure that a wrapper would call
  Entry := RoutineType +' '+ AFunction.CIdentifier+ParenParams(Params)+Returns;

  // takes care of duplicates
  AFunctionList.Add(Entry+Postfix);

  //RoutineType, AObjectName, AObjectFunctionName, AParams, AFunctionReturns, AFlatFunctionName, AWantSelf
  // writes the implementation of what we declared in the object
  if AWantWrapperForObject and  (Prefix = '') then
     WriteWrapperForObject(RoutineType, AItem.TranslatedName, SanitizeName(AFunction.Name), AFunction.Params, Returns, AFunction.CIdentifier, AIsMethod);
end;

procedure TPascalUnit.HandleObject(AItem: TgirObject; AObjectType: TGirToken);
var
  TypeDecl: TStringList;
  i: Integer;
  UnitFuncs,
  TypeFuncs: TStrings;
  ParentType: String ='';
  UsedNames: TStringList;
  WrittenFields: Integer;
  PackedBitsFieldCount: Integer = 0;
  PackedBits: TStringList = nil;


  function HasPackedBitfield: Boolean;
  begin
    HasPackedBitfield := PackedBits <> nil;
  end;

  procedure PackedBitsAddEntry (AEntry: String); // creates a new type to hold the packed bits
  const
    BitType = '  %sBitfield%d = bitpacked record';
  var
    BitEntry: String;
  begin
    if PackedBits = nil then
    begin
      PackedBits := TStringList.Create;
      PackedBits.Add(Format(BitType,[AItem.TranslatedName, PackedBitsFieldCount]));
      BitEntry := Format('    Bitfield%d : %sBitfield%d; { auto generated type }', [PackedBitsFieldCount, AItem.TranslatedName, PackedBitsFieldCount]);
      TypeDecl.Add(BitEntry);
      Inc(PackedBitsFieldCount);
    end;
    // now packed bits is assigned
    PackedBits.Add(Format('    %s;', [AEntry]));
  end;

  procedure EndPackedBits;
  begin
    if PackedBits = nil then
      Exit;
    PackedBits.Add('  end;');
    WantTypeSection.Lines.AddStrings(PackedBits);
    FreeAndNil(PackedBits);
  end;

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
        Result := Copy(Line, 1,FoundPos-1);
        Exit;
      end
      else
      if not SetFound then
      begin
        SetFound := Pos(LookingForSet+':', Line) > 0;
        SetFound := SetFound or (Pos(LookingForSet+'(', Line) > 0);
        // pascal properties cannot use functions for the set 'procedure'
        SetFound := SetFound and (Pos('proecedure ', Line) > 0);
      end;
      if SetFound and GetFound then
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
  begin
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
      PackedBitsAddEntry(Param)
    else
      TypeDecl.Add(IndentText(Param+';',4,0));
    AddField := ParamIsBitSized;
  end;

  procedure HandleFieldType(Field: TGirBaseType; AFirstPass: Boolean; out AddedBitSizedType: Boolean);
  var
    SetFound: Boolean;
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


        otGlibSignal : if AObjectType <> gtClass then TypeDecl.Add(IndentText(WriteCallBack(TgirCallback(Field),True, UsedNames),4,0)); // classes do not have signals They are in the class *struct*

        //WriteFunction(AFunction, AItem, AIsMethod, AWantWrapperForObject, AFunctionList): String;
        otFunction : TypeFuncs.Add(IndentText(WriteFunction(TgirFunction(Field), AItem, False, True, UnitFuncs, UsedNames),4,0));
        otMethod   : TypeFuncs.Add(IndentText(WriteFunction(TgirFunction(Field), AItem, True, True, UnitFuncs, UsedNames),4,0));
        otConstructor:TypeFuncs.Add(IndentText(WriteFunction(TgirConstructor(Field), AItem, False, True, UnitFuncs, UsedNames),4,0));
        otProperty : TypeFuncs.Add(IndentText(WriteMethodProperty(TgirProperty(Field), GetTypeForProperty(TgirProperty(Field), SetFound), SetFound),4,0));
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
  var
    AType: String;
  begin
    AType:='TGType';
    if AObj.GetTypeFunction = '' then
      Exit;
    if not NameSpace.UsesGLib then
      AType := 'csize_t { TGType }';

    UnitFuncs.Add(Format(GetTypeTemplate, [AObj.GetTypeFunction, AType]));
  end;

var
  TypeSect: TPDeclarationType;
  AddedBitSizedType: Boolean;
begin
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
      if (not AddedBitSizedType and HasPackedBitfield) or (i = AItem.Fields.Count-1) then
        EndPackedBits;
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
    WantFunctionSection.Lines.AddStrings(UnitFuncs);
  UnitFuncs.Free;
  TypeFuncs.Free;

end;

procedure TPascalUnit.HandleUnion(AItem: TgirUnion);
begin
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
                  Format(Body, [ResultStr, Self.UnitName+'.'+AFlatFunctionName, CallParams]);
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
    AFunctionReturnType:= ': '+TypeAsString(AItem.Returns.VarType, AItem.Returns.PointerLevel)+'; cdecl;' ;

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
begin
  ResolveTypeTranslation(AType);

  BackupNoPointers := StringReplace(ACTypeAsBackup, '*', '', [rfReplaceAll]);

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
  TypeDecl: TStringList;
  i: Integer;
  Field: TGirBaseType;
  UseName: String;
  Symbol: String;
  Dummy: Boolean;
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
  if ARecord.Fields.Count = 0 then
    TypeDecl.Add(IndentText('Unknown: Pointer;', ABaseIndent+4,0));

  for i := 0 to ARecord.Fields.Count-1 do
    begin
      Field := ARecord.Fields.Field[i];
      case Field.ObjectType of
        otArray,
        otTypeParam: TypeDecl.Add(IndentText(WriteParamAsString(TgirTypeParam(Field),i, Dummy)+';',ABaseIndent+4,0));
        otCallback : TypeDecl.Add(IndentText(WriteCallBack(TgirCallback(Field),True),ABaseIndent+4,0));
        otUnion: TypeDecl.Add(IndentText(WriteUnion(TgirUnion(Field), True, ABaseIndent),ABaseIndent+4));
      else
        TypeDecl.Add(IndentText(Field.Name+ ' ' + Field.ClassName,4,0)); // this of course will make the compiler barf
      end;

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
begin
  if ABaseType.TranslatedName = '' then
    ABaseType.TranslatedName:=MakePascalTypeFromCType(ABaseType.CType, 0);
end;

constructor TPascalUnit.Create(ANameSpace: TgirNamespace; ALinkDynamic: Boolean; AWantTest: Boolean);
const
  CBasic = '#include <%s>'+LineEnding;
  PBasic = 'program %s_test;'+LineEnding+
           '{$LINK %s_c_test}'+LineEnding+
           'uses %s;'+LineEnding;
begin
  ProcessLevel:=0;
  FWantTest:=AWantTest;
  FLinkDynamic := ALinkDynamic;
  FFinalizeSection := TPFinialization.Create(Self);
  FImplementationSection := TPImplementation.Create(Self);
  FInitializeSection := TPInitialize.Create(Self);
  FInterfaceSection := TPInterface.Create(Self, TPUses.Create);
  FNameSpace := ANameSpace;
  if FWantTest then
  begin
    FTestCFile := TStringStream.Create('');
    FTestCFile.WriteString(Format(CBasic, [FNameSpace.CIncludeName]));
    FTestPascalFile := TStringStream.Create('');
    FTestPascalFile.WriteString(Format(PBasic,[UnitName, UnitName, UnitName]));
    FTestPascalBody := TStringList.Create;
    FTestPascalBody.Add('begin');
  end;
  ResolveFuzzyTypes;
  GenerateUnit;
end;

destructor TPascalUnit.Destroy;
begin
  if FWantTest then
  begin
    FTestPascalFile.Free;
    FTestCFile.Free;
    FTestPascalBody.Free;
  end;
  FFinalizeSection.Free;
  FImplementationSection.Free;
  FInitializeSection.Free;
  FInterfaceSection.Free;

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
begin
  for i := 0 to FNameSpace.RequiredNameSpaces.Count-1 do
  begin
    NS := TgirNamespace(FNameSpace.RequiredNameSpaces.Items[i]);
    InterfaceSection.UsesSection.Units.Add(' '+CalculateUnitName(NS.NameSpace,NS.Version));
  end;

  i := Pos(',',NameSpace.SharedLibrary);
  if i > 0 then
    LibName:=Copy(NameSpace.SharedLibrary,1,i-1)
  else
    LibName:=NameSpace.SharedLibrary;
  WantConstSection.Lines.Add(IndentText(UnitName+'_library = '''+LibName+''';', 2));
  if NameSpace.NameSpace = 'GLib' then
    AddGLibSupportCode;

end;

function TPascalUnit.AsStream: TStringStream;
var
  Str: TStringStream absolute Result;
  Libs: TStringList;
  i: Integer;
begin
  Libs := TStringList.Create;
  Libs.Delimiter:=',';
  Libs.StrictDelimiter:= True;
  Libs.CommaText:=NameSpace.SharedLibrary;

  Result := TStringStream.Create('');
  Str.WriteString(IndentText('unit '+ UnitName+';',0,2));
  Str.WriteString(IndentText('{$MODE OBJFPC}{$H+}',0,2));
  Str.WriteString(IndentText('{$PACKRECORDS C}',0,1));
  Str.WriteString(IndentText('{$BITPACKING ON}',0,1));
  //Str.WriteString(IndentText('{$CALLING CDECL}',0,2));
  Str.WriteString(IndentText('{$MODESWITCH DUPLICATELOCALS+}',0,2));

  for i := 0 to Libs.Count-1 do
    Str.WriteString(IndentText('{$LINKLIB '+Libs.Strings[i]+'}',0,1));

  Libs.Free;

  Str.WriteString(InterfaceSection.AsString);
  Str.WriteString(ImplementationSection.AsString);

  if InitializeSection.Declarations.Count > 0 then
     Str.WriteString(InitializeSection.AsString);

  if FinalizeSection.Declarations.Count > 0 then
     Str.WriteString(FinalizeSection.AsString);

  Str.WriteString('end.');

  Result.Position:=0;
end;

procedure TPascalUnit.Finish;
begin
  if FWantTest then
  begin
    FTestPascalFile.WriteString(FTestPascalBody.Text);
    FTestPascalFile.WriteString('end.');
    FTestCFile.Position:=0;
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

{ TgirPascalWriter }

constructor TgirPascalWriter.Create(ANameSpaces: TgirNamespaces; AWantTest: Boolean);
begin
  FNameSpaces := ANameSpaces;
  FUnits := TList.Create;
  FDefaultUnitExtension:='.pas';
  FWantTest:=AWantTest;
end;

procedure TgirPascalWriter.GenerateUnits;
var
  i: Integer;
  FUnit: TPascalUnit;


begin
  for i := 0 to FNameSpaces.Count-1 do
    begin
      WriteLn(Format('Converting %s', [FNameSpaces.NameSpace[i].NameSpace]));
      FUnit := TPascalUnit.Create(FNameSpaces.NameSpace[i], False, FWantTest);
      FUnit.ProcessConsts(FNameSpaces.NameSpace[i].Constants);
      FUnit.ProcessTypes(FNameSpaces.NameSpace[i].Types);
      FUnit.ProcessFunctions(FNameSpaces.NameSpace[i].Functions);
      FUnit.Finish;
      FUnits.Add(FUnit);
      FOnUnitWriteEvent(Self, FUnit.UnitName+FDefaultUnitExtension, FUnit.AsStream);
      if FWantTest then
      begin
        FOnUnitWriteEvent(Self, FUnit.UnitName+'_test'+FDefaultUnitExtension, FUnit.FTestPascalFile);
        FOnUnitWriteEvent(Self, FUnit.UnitName+'_c_test.c', FUnit.FTestCFile);
      end;
    end;
end;

end.

