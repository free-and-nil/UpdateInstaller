// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

unit ProcessDataBaseXmlU;

interface

uses Contnrs, Classes,
     XmlBaseClassesU, PackageClassU, GlobalsU,
     OmniXML;

type
    TDatabase = class (TXmlBaseClass)
      private
        function CheckSystemCompatibility (const ParentNode: IXMLNode;
                                           out sComment: String) : Boolean;
        function Get_DB_Version (out iVersion: Integer) : Boolean;
        function GetValue (const Node: IXMLNode; const sName: String;
                           out sValue: String) : Boolean;
        function ProcessChildNode (const Node: IXMLNode;
                                   out Package: TPackage) : Boolean;

      public
        constructor Create (const sFileName: String);
        destructor Destroy; override;

        procedure ProcessXmlFile (const PackageList: TObjectList;
        						  const ID_List: TStringList = NIL);
    end; { TDatabase }

implementation

uses SysUtils,
     BaseTypesU, PasTools, LogToTempU;

ResourceString
    cCompatibleSystemFoundMsg = 'Matching system "%s" found in compatibility list';
//    cCompatiblityErrMsg = #13#10'The package is not compatible with this system';
    cDatabaseVerErrMsg = 'Unable to retrieve the database version from "%s"';
//    cIgnoringCompatiblityErrMsg = #13#10'Ignoring package incompatibility: ';
    cPackageMsg = #13#10'Package "%s"';
    cSystemNotCompatibleMsg =
                   'No match for hardware model "%s" and OS "Windows %s" found';
    cUnsupportedDatabaseVerMsg = 'Unsupported database version %d';

const
    cSystem = 'System';
    cSystemCompatibility = 'SystemCompatibility';

(* ---- *)

function TDatabase.CheckSystemCompatibility (const ParentNode: IXMLNode;
                                             out sComment: String) : Boolean;

    (* ---- *)

    function FormatSystemName (const sSystem: String) : String;

    var
        iPos, iLen : Integer;

    begin
        Assert (sSystem <> '');

        Result := sSystem;

        Result [1] := UpCase (Result [1]);

        iPos := 1;
        iLen := Length (Result);

        while (true) do
        begin
            iPos := NextPos (' ', Result, iPos);

            if (iPos > 0) and (iPos < iLen) then
            begin
                Inc (iPos);
                Result [iPos] := UpCase (Result [iPos]);
            end { if }
            else Break;
        end; { while }
    end; { FormatSystemName }

    (* ---- *)

    function CheckValues (const aAttr: TaString) : Boolean;

    var
        sOS, sMTM, sSystem : String;
        iPos : Integer;
        bModelCompatible : Boolean;

    begin
        Result := false;
        bModelCompatible := false;

        sMTM := UpperCase (aAttr [0]);

        if (Pos (sMTM, BiosModel) = 1) then
            bModelCompatible := true;

        if (bModelCompatible = false) then
            if (CompatibleSystemsCount > 0) and
               (IsCompatibleSystem (BiosSystemName, sSystem)) then
            begin
                bModelCompatible := true;
                sComment := Format (cCompatibleSystemFoundMsg,
                                    [FormatSystemName (sSystem)]);
            end; { if }

        if (bModelCompatible) then
            if not (IgnoreWindowsCompatibility) then
            begin
                iPos := Pos (' ', aAttr [1]);

                if (iPos > 0) then
                begin
                    sOS := Copy (aAttr [1], iPos + 1, Length (aAttr [1]) - iPos);

                    if (Pos (sOS, Get_OS_Ver) = 1) then
                        Result := true;
                end; { if }
            end { if }
            else Result := true;
    end; { CheckValues (const aAttr: TaString }

    (* ---- *)

    function CheckSystemNode (Node: IXMLNode) : Boolean;

    var
        aAttr : TaString;

    begin
        Result := false;

        repeat
            if (Node.NodeName = cSystem) then
            begin
                aAttr := GetAttrValues (Node, ['mtm', 'os']);

                if (Assigned (aAttr)) then
                    if (CheckValues (aAttr)) then
                    begin
                        Result := true;
                        Break;
                    end; { if }
            end; { if }

            Node := Node.NextSibling;
        until (Node = NIL);
    end; { CheckSystemNode }

    (* ---- *)

var
    Node : IXMLNode;

begin { TDatabase.CheckSystemCompatibility }
    Assert (ParentNode <> NIL);

    Result := false;

    Node := ParentNode.FirstChild;

    while (Node <> NIL) do
    begin
        if (Node.NodeName = cSystemCompatibility) and (Node.HasChildNodes) then
            if (CheckSystemNode (Node.FirstChild)) then
            begin
                Result := true;
                Break;
            end; {if }

        Node := Node.NextSibling;
    end; { while }
end; { TDatabase.CheckSystemCompatibility }

(* ---- *)

constructor TDatabase.Create (const sFileName: String);
begin
    inherited Create (sFileName);
end; { TDatabase.Create }

(* ---- *)

destructor TDatabase.Destroy;
begin
    inherited Destroy;
end; { TDatabase.Destroy }

(* ---- *)

procedure TDatabase.ProcessXmlFile (const PackageList: TObjectList;
        						    const ID_List: TStringList = NIL);

const
    cDatabaseVer = 301;

var
    iVersion, iIndex : Integer;
    ChildNode : IXMLNode;
    Package : TPackage;

begin
    if not (Get_DB_Version (iVersion)) then
        raise EXMLException.CreateFmt (cDatabaseVerErrMsg, [FileName]);

    if (iVersion <> cDatabaseVer) then
        raise EXMLException.CreateFmt (cUnsupportedDatabaseVerMsg, [iVersion]);

    ChildNode := XmlDoc.DocumentElement.FirstChild;

    while (ChildNode <> NIL) do
    begin
        if (ProcessChildNode (ChildNode, Package)) then
        	if not (Assigned (ID_List)) then
            	PackageList.Add (Package)
            else
                if (ID_List.Find (LowerCase (Package.ID), iIndex)) then
                	PackageList.Add (Package)
                else Package.Free;

        ChildNode := ChildNode.NextSibling;
    end; { while }
end; { TDatabase.ProcessXmlFile }

(* ---- *)

function TDatabase.Get_DB_Version (out iVersion: Integer) : Boolean;

var
    AttributeList : IXMLNamedNodeMap;
    sVersion : String;

begin
    Result := false;

    AttributeList := XmlDoc.DocumentElement.Attributes;

    if (AttributeList <> NIL) then
    begin
        sVersion := AttributeList.Item [0].NodeValue;

        iVersion := StrToIntDef (sVersion, (-1));

        if (iVersion > 0) then
            Result := true;
    end; { if }
end; { TDatabase.Get_DB_Version }

(* ---- *)

function TDatabase.GetValue (const Node: IXMLNode; const sName: String;
                             out sValue: String) : Boolean;

var
    ChildNode : IXMLNode;

begin
    Assert (Node <> NIL);
    Assert (sName <> '');

    Result := false;

    ChildNode := Node.FirstChild;

    while (ChildNode <> NIL) do
    begin
        if (ChildNode.NodeName = sName) then
            if (ChildNode.HasChildNodes) and
               (ChildNode.FirstChild.NodeName = '#text') then
            begin
                sValue := ChildNode.FirstChild.NodeValue;
                Result := sValue <> '';
                exit;
            end; { if }

        ChildNode := ChildNode.NextSibling;
    end; { while }
end; { TDatabase.GetValue }

(* ---- *)

function TDatabase.ProcessChildNode (const Node: IXMLNode;
                                     out Package: TPackage) : Boolean;

    (* ---- *)

    function CheckCompatibility (const sDescription: String;
                                 out bIsCompatible: Boolean;
                                 out sMsg: String) : Boolean;
    begin
        Result := false;
        sMsg := '';

        bIsCompatible := CheckSystemCompatibility (Node, sMsg);

        if (bIsCompatible) then
        begin
            Result := true;

            if (sMsg <> '') then
                LogMsg (cPackageMsg + ': ' + #13#10 + sMsg, [sDescription])
        end { if }
        else
        begin
            sMsg := Format (cSystemNotCompatibleMsg, [BiosModel, Get_OS_Ver]);

            if (IgnoreSystemCompatibility) then
                Result := true
            else LogMsg (cPackageMsg + ': ' + #13#10 + sMsg, [sDescription])
        end; { else }
    end; { CheckCompatibility }

    (* ---- *)

const
    cName = 'name';
    cID = 'id';
    cDescription = 'description';

var
    sValue, sMsg : String;
    bIsCompatible : Boolean;

begin { TDatabase.ProcessChildNode }
    Result := false;

    if (AttributesExist (Node, [cName, cID, cDescription])) then
        if (GetAttrValue (Node, cDescription, sValue)) then
        begin
            if (Action <> aUnpack) then
                if not (CheckCompatibility (sValue, bIsCompatible, sMsg)) then
                    exit;

            Package := TPackage.Create;
            Package.Description := sValue;

	        if (GetAttrValue (Node, cID, sValue)) then
            begin
            	Package.ID := sValue;

                Package.CompatibilityCheck := bIsCompatible;

                if (sMsg <> '') then
                    Package.CompatibilityMsg := sMsg;

                if (GetValue (Node, 'Version', sValue)) then
                begin
                    Package.PackageVersion := sValue;

                    if (GetValue (Node, 'LocalPath', sValue)) then
                        if (sValue <> '') then
                        begin
                            Package.LocalPath := sValue;
                            Result := true;
                        end; { if }
                end; { if }
            end; { if }

            if not (Result) then
                Package.Free;
        end; { if }
end; { TDatabase.ProcessChildNode }

(* ---- *)

end.
