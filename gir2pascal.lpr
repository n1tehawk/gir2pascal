{
gir2pascal.lpr
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
program gir2pascal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, DOM, XMLRead, girNameSpaces, girFiles,
  girpascalwriter, girErrors, girCTypesMapping, girTokens, girObjects;

type

  { TGirConsoleConverter }

  TGirConsoleConverter = class(TCustomApplication)
  private
    FWriteCount: Integer;
    FPaths: TStringList;
    FOutPutDirectory : String;
    FFileToConvert: String;
    FOverWriteFiles: Boolean;
    FWantTest: Boolean;
    procedure AddDefaultPaths;
    procedure AddPaths(APaths: String);
    procedure VerifyOptions;
    procedure Convert;

    //callbacks
    function NeedGirFile(AGirFile: TObject; NamespaceName: String) : TXMLDocument;
    // AName is the whole name unit.pas or file.c
    procedure WriteFile(Sender: TObject;  AName: String; AStream: TStringStream);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGirConsoleConverter }

procedure TGirConsoleConverter.AddDefaultPaths;
begin
  FPaths.Add('/usr/share/gir-1.0/');
end;

procedure TGirConsoleConverter.AddPaths(APaths: String);
var
  Strs: TStringList;
  Str: String;
begin
  Strs := TStringList.Create;
  Strs.Delimiter:=':';
  Strs.StrictDelimiter:=True;
  Strs.DelimitedText:=APaths;

  // so we can add the delimiter
  for Str in Strs do
    FPaths.Add(IncludeTrailingPathDelimiter(Str));

  Strs.Free;
end;

procedure TGirConsoleConverter.VerifyOptions;
begin
  if not DirectoryExists(FOutPutDirectory) then
  begin
    WriteLn(Format('Output directory "%s" does not exist!', [FOutPutDirectory]));
    Terminate;
  end;
  if FFileToConvert = '' then
  begin
    WriteLn('No input file specified! See -h for options.');
    Terminate;
    Halt;
  end;
end;

function TGirConsoleConverter.NeedGirFile(AGirFile: TObject; NamespaceName: String): TXMLDocument;
var
  Sr: TSearchRec;
  Path: String;
begin
  Result := nil;
  for Path in FPaths do
  begin
    if FindFirst(Path+NamespaceName+'.gir', faAnyFile, Sr) = 0 then
    begin
      ReadXMLFile(Result, Path+Sr.Name);
      Exit;
    end;
    FindClose(Sr);
  end;
end;

procedure TGirConsoleConverter.WriteFile(Sender: TObject; AName: String; AStream: TStringStream);
var
  SStream: TFileStream;
  OutFileName: String;
begin
  Inc(FWriteCount);
  OutFileName:=FOutPutDirectory+LowerCase(AName);
  if not FileExists(OutFileName)
  or (FileExists(OutFileName) and FOverWriteFiles) then
  begin
    WriteLn(Format('Writing: %s', [OutFileName]));
    AStream.Position:=0;
    ForceDirectories(FOutPutDirectory);
    SStream := TFileStream.Create(OutFileName, fmCreate or fmOpenReadWrite);
    SStream.CopyFrom(AStream,AStream.Size);
    SStream.Free;
    AStream.Free;
  end
  else
  begin
    WriteLn(Format('File %s already exists! Stopping.', [OutFileName]));
    Terminate;
    Halt;
  end;
end;

procedure TGirConsoleConverter.Convert;
var
  Doc: TXMLDocument;
  girFile: TgirFile;
  Writer: TgirPascalWriter;
  StartTime, EndTime:TDateTime;
begin
  StartTime := Now;
  ReadXMLFile(Doc, FFileToConvert);

  girFile := TgirFile.Create(nil);
  girFile.OnNeedGirFile:=@NeedGirFile;
  girFile.ParseXMLDocument(Doc);
  Doc.Free;

  Writer := TgirPascalWriter.Create(girFile.NameSpaces, FWantTest);
  Writer.OnUnitWriteEvent:= @WriteFile;
  Writer.GenerateUnits;

  Writer.Free;
  EndTime := Now;

  EndTime := EndTime-StartTime;
  WriteLn(Format('Converted %d file(s) in %f seconds',[FWriteCount, DateTimeToTimeStamp(EndTime).Time / 1000]));
end;

procedure TGirConsoleConverter.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hnp:o:i:wt',['help','no-default','paths','output-directory', 'input', 'overwrite-files', 'test']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not HasOption('n', 'no-default') then
    AddDefaultPaths;

  if HasOption('o', 'output-directory') then
    FOutPutDirectory:=IncludeTrailingPathDelimiter(GetOptionValue('o', 'output-directory'))
  else
    FOutPutDirectory:=IncludeTrailingPathDelimiter(GetCurrentDir);

  FFileToConvert:=GetOptionValue('i','input');

  if HasOption('p', 'paths') then
    AddPaths(GetOptionValue('p', 'paths'));

  if HasOption('w', 'overwrite-files') then
    FOverWriteFiles:=True;

  FWantTest := HasOption('t', 'test');

  VerifyOptions;

  // does all the heavy lifting
  Convert;

  // stop program loop
  Terminate;
end;

constructor TGirConsoleConverter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPaths := TStringList.Create;
end;

destructor TGirConsoleConverter.Destroy;
begin
  FPaths.Free;
  inherited Destroy;
end;

procedure TGirConsoleConverter.WriteHelp;
begin
  Writeln('');
  writeln('    Usage: ',ExtractFileName(ExeName),' [options] -i filename');
  Writeln('');
  Writeln('');
  Writeln('      -i --input=            .gir filename to convert.');
  Writeln('      -o --output-directory=  Directory to write the resulting .pas files to. If not');
  Writeln('                              specified then the current working directory is used.');
  Writeln('      -w --overwrite-files    If the output .pas file(s) already exists then overwrite them.');
  Writeln('      -n --no-default         /usr/share/gir-1.0 is not added as a search location for ');
  Writeln('                              needed .gir files.');
  Writeln('      -p --paths=             List of paths seperated by ":" to search for needed .gir files.');
  Writeln('      -t --test               Creates a test program and a test c file per unit to verify struct sizes.');
  Writeln('');
end;

var
  Application: TGirConsoleConverter;

{$R *.res}

begin
  Application:=TGirConsoleConverter.Create(nil);
  Application.Run;
  Application.Free;
end.

