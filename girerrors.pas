{
girerrors.pas
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
unit girErrors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGirError = (geError, geWarn, geInfo, geDebug, geFatal, geFuzzy);

  TgirErrorFunc = procedure(UserData: Pointer; AType: TgirError; AMsg: String);

  const
    geUnhandledNode = 'Unhandled node [%s] "%s"';
    geUnexpectedNodeType = 'Unexpected node [%s] type: found "%s" expected "%s"';
    geMissingNode = '[%s] Could not find child node "%s" while looking in node "%s"';

  var
    girErrorName: array[TGirError] of String =(
      'Error',
      'Warning',
      'Info',
      'Debug',
      'Fatal',
      'Fuzzy'
    );
    procedure girError(AType: TgirError; AMsg: String);

    //returns old handler
    function girSetErrorHandler(AHandler: TgirErrorFunc; AUserData: Pointer): TgirErrorFunc;

implementation

var
  UserData: Pointer;
  InternalHandler: TgirErrorFunc;

procedure girError(AType: TgirError; AMsg: String);
begin
  if InternalHandler <> nil then
  begin
    InternalHandler(UserData, AType, AMsg);
    Exit;
  end;
 // if AType = geDebug then
  //WriteLn(girErrorName[AType],': ', AMsg);

end;

function girSetErrorHandler(AHandler: TgirErrorFunc; AUserData: Pointer
  ): TgirErrorFunc;
begin
  Result := InternalHandler;
  InternalHandler:=AHandler;
  UserData:=AUserData;
end;

end.

