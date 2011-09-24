{
girfiles.pas
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
unit girFiles;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, XMLRead, DOM, girNameSpaces, girParser;

type

  { TgirFile }

  TgirFile = class(IgirParser)
  private
    FNameSpaces: TgirNamespaces;
    FOnNeedGirFile: TgirNeedGirFileEvent;
    FOwner: TObject;
    procedure ParseNode(ANode: TDomNode);
    procedure SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
    procedure SetOwner(const AValue: TObject);
    procedure ParseIncludeNode(ANode: TDomNode; AIncludes: TList);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure ParseXMLDocument(AXML: TXMLDocument);
    property NameSpaces: TgirNamespaces read FNameSpaces;
    property Owner: TObject read FOwner write SetOwner;
    property OnNeedGirFile: TgirNeedGirFileEvent read FOnNeedGirFile write SetOnNeedGirFile;

  end;

implementation
uses girErrors, girTokens;

{ TgirFile }


{ TgirFile }

procedure TgirFile.ParseNode(ANode: TDomNode);
var
  Node: TDomNode;
  NS: TgirNamespace;
  Includes: TList;
begin
  if ANode.NodeName <> 'repository' then
    girError(geError, 'Not a Valid Document Type!');

  Node := Anode.FirstChild;
  Ns := nil;
  Includes := TList.Create;

  while Node <> nil do begin
    case GirTokenNameToToken(Node.NodeName) of
      gtInclude: ParseIncludeNode(Node, Includes);
      gtNameSpace:
        begin
          NS := TgirNamespace.CreateFromRepositoryNode(NameSpaces, ANode, Includes);
          girError(geDebug, 'Adding Namespace '+NS.NameSpace+' to NameSpaces');
          FNameSpaces.Add(NS);
          girError(geDebug, 'Added Namespace '+NS.NameSpace);
          NS.ParseNode(Node);
        end;
      gtPackage, gtCInclude: ;// ignore for now
    else
      girError(geDebug, 'Unknown Node Type for Reposiotory: '+ node.NodeName);
    end;
    Node := Node.NextSibling;
  end;



  {ANode := ANode.FindNode('namespace');
  if ANode = nil then
    girError(geError, 'namespace node not found')
  else
  begin

  end;}
end;

procedure TgirFile.SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
begin
  FNameSpaces.OnNeedGirFile:=AValue;
  if FOnNeedGirFile=AValue then Exit;
  FOnNeedGirFile:=AValue;
end;

procedure TgirFile.SetOwner(const AValue: TObject);
begin
  if FOwner=AValue then exit;
  FOwner:=AValue;
end;

procedure TgirFile.ParseIncludeNode(ANode: TDomNode; AIncludes: TList);
var
  NS: TgirNamespace;
  NSName, NSVersion: String;
begin
  NSName := TDOMElement(ANode).GetAttribute('name');
  NSVersion := TDOMElement(ANode).GetAttribute('version');
  NS := FNameSpaces.FindNameSpace(NSName, NSVersion);
  if NS <> nil then
  begin
    AIncludes.Add(NS);
  end;
end;


constructor TgirFile.Create(AOwner: TObject);
begin
 Owner := AOwner;
 FNameSpaces := TgirNamespaces.Create(Self);
end;

destructor TgirFile.Destroy;
begin
  FNameSpaces.Free;
  inherited Destroy;
end;

procedure TgirFile.ParseXMLDocument(AXML: TXMLDocument);
begin
  Self.ParseNode(AXML.DocumentElement);
end;



end.

