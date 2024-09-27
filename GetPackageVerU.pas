// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

unit GetPackageVerU;

interface

function Get_Package_Version (const sPackageXml: String;
                              out iVersion: Integer) : Boolean;

implementation

uses SysUtils,
{$IFDEF UNICODE}
     LibXmlParserU;
{$ELSE}
     LibXmlParser;
{$ENDIF}

const
    cDescriptor = 'pcdDescriptor';

(* ---- *)

function CheckVersion (const sLine, sSearchStr: String;
                       var iVersion: Integer) : Boolean;

var
    sVersion : String;
    iLen, iStart : Integer;

begin
    Result := false;

    if (Pos (sSearchStr, sLine) > 0) then
    begin
        iLen := Length (sLine);
        iStart := Pos ('="', sLine) + 2;

        if (iStart > 0) and (iStart < iLen) then
        begin
            sVersion := Copy (sLine, iStart, iLen - iStart);
            iVersion := StrToIntDef (sVersion, (-1));

            if (iVersion <> (-1)) then
                Result := true;
        end; { if }
    end { if }
end; { CheckVersion }

(* ---- *)

function CheckAttributes (const CurAttr: TAttrList;
                          out iVersion: Integer) : Boolean;

var
    iIndex : Integer;

begin
    Result := false;

    for iIndex := 0 to CurAttr.Count - 1 do
        if (CurAttr.Name (iIndex) = cDescriptor) then
        begin
            iVersion := StrToIntDef (CurAttr.Value (iIndex), (-1));

            if (iVersion > 0) then
            begin
                Result := true;
                Break;
            end; { if }
        end; { if }
end; { CheckAttributes }

(* ---- *)

function Get_Package_Version (const sPackageXml: String;
                              out iVersion: Integer) : Boolean;

const
    cPackage = 'Package';
    cVersion = 'version=';

var
    Parser : TXmlParser;

begin
    Result := false;

    Parser := TXmlParser.Create;

    try
        Parser.LoadFromFile (sPackageXml);

        while (Parser.Scan) do
        begin
            if (Parser.CurPartType = ptPI) and
               (Parser.CurName = cDescriptor) then
                Result := CheckVersion (Parser.CurContent, cVersion,
                                        iVersion{%H-})
            else if (Parser.CurPartType = ptStartTag) and
                    (Parser.CurName = cPackage) then
                Result := CheckAttributes (Parser.CurAttr, iVersion);

            if (Result) then
                Break;
        end; { while }

    finally
        Parser.Free;
    end; { try / finally }
end; { Get_Package_Version }

(* ---- *)

end.

