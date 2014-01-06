{
girtokens.pas
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
unit girTokens;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TGirToken = (gtInvalid, gtAlias, gtConstant, gtRecord, gtBitField, gtEnumeration,
               gtCallback, gtUnion, gtFunction, gtReturnValue, gtType,
               gtParameters, gtParameter, gtInstanceParameter, gtMember, gtField, gtMethod, gtArray,
               gtDoc, gtConstructor, gtRepository, gtInclude, gtNameSpace, gtPackage,
               gtCInclude, gtClass, gtProperty, gtVirtualMethod, gtInterface,
               gtGlibSignal, gtImplements, gtPrerequisite,gtVarArgs, gtObject, gtClassStruct, gtGType);



var
  GirTokenName: array[TGirToken] of String = (
    'Invalid Name',
    'alias',
    'constant',
    'record',
    'bitfield',
    'enumeration',
    'callback',
    'union',
    'function',
    'return-value',
    'type',
    'parameters',
    'parameter',
    'instance-parameter',
    'member',
    'field',
    'method',
    'array',
    'doc',
    'constructor',
    'repository',
    'include',
    'namespace',
    'package',
    'c:include',
    'class',
    'property',
    'virtual-method',
    'interface',
    'glib:signal',
    'implements',
    'prerequisite',
    'varargs',
    'object',
    'classstruct',
    'gtype'
  );

  function GirTokenNameToToken(AName: String): TGirToken;

implementation

function GirTokenNameToToken(AName: String): TGirToken;
begin
  for Result in TGirToken do
    if GirTokenName[Result][1] <> AName[1] then
      continue
    else if GirTokenName[Result] = AName then
      Exit;
  Result := gtInvalid;
end;

end.

