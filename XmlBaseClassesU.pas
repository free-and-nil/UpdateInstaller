// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

unit XmlBaseClassesU;

interface

uses GlobalsU,
     BaseTypesU,
     OmniXML;

type
    TXmlBaseClass = class
      private
        FFileName : String;
        FTag : Integer;

      protected
        XmlDoc : IXMLDocument;

        function AttributeExists (const Node: IXMLNode;
                                  const sAttr: String) : Boolean;
        function AttributesExist (const Node: IXMLNode;
                                  const asAttr: array of String) : Boolean;
        function GetAttrValue (const Node: IXMLNode; const sAttr: String;
                               out sValue: String) : Boolean;
        function GetAttrValues (const Node: IXMLNode;
                                const asAttr: array of String) : TaString;
        function GetChildNodeValue (const ParentNode: IXMLNode;
                                    const sNodeName: String;
                                    out sValue: String) : Boolean;
        function GetFirstNamedNode (const Node: IXMLNode;
                                    out DataNode: IXMLNode) : Boolean;
        function GetNodeAssociatedValue (const Node: IXMLNode;
                                         out sValue: String) : Boolean;
        function GetNodeValue (const Node: IXMLNode;
                               out sValue: String) : Boolean;

      public
        constructor Create (const sFileName: String); overload;
        constructor Create; overload;
        destructor Destroy; override;

        function LoadXml (const sFileName: String) : Boolean;
        function SetNode (const sXmlNode: String) : IXMLNode;

{$IFDEF DEBUG}
        procedure ShowNodesAndAttributes;
{$ENDIF}

        property FileName : String read FFileName;
        property Tag : Integer read FTag write FTag;
    end; { TXmlBaseClass }

implementation

uses SysUtils,
     LogToTempU;

ResourceString
    cXmlLoadErrMsg = 'Error loading file "%s": "%s" at %d:%d';
    cXmlNodeNotFoundMsg = 'XML node "%s" not found';

(* ---- *)

function TXmlBaseClass.AttributeExists (const Node: IXMLNode;
                                        const sAttr: String) : Boolean;

var
    AttributeList : IXMLNamedNodeMap;

begin
    Assert (sAttr <> '');

    AttributeList := Node.Attributes;

    if (AttributeList <> NIL) then
        Result := AttributeList.GetNamedItem (sAttr) <> NIL
    else Result := false;
end; { TXmlBaseClass.AttributeExists }

(* ---- *)

function TXmlBaseClass.AttributesExist (const Node: IXMLNode;
                                        const asAttr: array of String) : Boolean;

var
    iIndex : Integer;
    AttributeList : IXMLNamedNodeMap;

begin
    Assert (Length (asAttr) > 0);

    AttributeList := Node.Attributes;

    if (AttributeList <> NIL) then
    begin
        Result := true;

        for iIndex := 0 to High (asAttr) do
            if (AttributeList.GetNamedItem (asAttr [iIndex]) = NIL) then
            begin
                Result := false;
                Break;
            end; { if }
    end { if }
    else Result := false;
end; { TXmlBaseClass.AttributesExist }

(* ---- *)

constructor TXmlBaseClass.Create (const sFileName: String);
begin
    Assert (sFileName <> '');

    inherited Create;

    if not (LoadXml (sFileName)) then
        with XmlDoc.ParseError do
            raise Exception.CreateFmt (cXmlLoadErrMsg,
                                       [sFileName, Reason, Line, LinePos]);
end; { TXmlBaseClass.Create }

(* ---- *)

constructor TXmlBaseClass.Create;
begin
    inherited Create;
end; { TXmlBaseClass.Create }

(* ---- *)

destructor TXmlBaseClass.Destroy;
begin
    XmlDoc := NIL;

    inherited;
end; { TXmlBaseClass.Destroy }

(* ---- *)

function TXmlBaseClass.LoadXml (const sFileName: String) : Boolean;
begin
    Assert (sFileName <> '');

    FFileName := sFileName;

    XmlDoc := CreateXMLDoc;

    XmlDoc.PreserveWhiteSpace := true;

    Result := XmlDoc.Load (sFileName);

    if not (Result) then
        with XmlDoc.ParseError do
            LogMsg (cXmlLoadErrMsg, [sFileName, Reason, Line, LinePos]);
end; { TXmlBaseClass.LoadXml }

(* ---- *)

function TXmlBaseClass.GetAttrValue (const Node: IXMLNode; const sAttr: String;
                                     out sValue: String) : Boolean;

var
    AttributeList : IXMLNamedNodeMap;
    ItemNode : IXMLNode;

begin
    Assert (sAttr <> '');

    Result := false;

    AttributeList := Node.Attributes;

    if (AttributeList <> NIL) then
    begin
        ItemNode := AttributeList.GetNamedItem (sAttr);

        if (ItemNode <> NIL) then
        begin
            Result := true;
            sValue := ItemNode.NodeValue;
        end; { if }
    end { if }
end; { TXmlBaseClass.GetAttrValue }

(* ---- *)

function TXmlBaseClass.GetAttrValues (const Node: IXMLNode;
                                      const asAttr: array of String) : TaString;

var
    iIndex : Integer;
    AttributeList : IXMLNamedNodeMap;
    ItemNode : IXMLNode;

begin
    Assert (Length (asAttr) > 0);

    SetLength (Result{%H-}, Length (asAttr));

    AttributeList := Node.Attributes;

    if (AttributeList <> NIL) then
        for iIndex := 0 to High (asAttr) do
        begin
            ItemNode := AttributeList.GetNamedItem (asAttr [iIndex]);

            if (ItemNode <> NIL) then
                Result [iIndex] := ItemNode.NodeValue
            else
            begin
                Result := NIL;
                exit;
            end; { else }
        end; { for }
end; { TXmlBaseClass.GetAttrValues }

(* ---- *)

function TXmlBaseClass.GetChildNodeValue (const ParentNode: IXMLNode;
                                          const sNodeName: String;
                                          out sValue: String) : Boolean;

var
    Node : IXMLNode;

begin
    Assert (ParentNode <> NIL);

    Result := false;

    if not (ParentNode.HasChildNodes) then
        exit;

    Node := ParentNode.FirstChild;

    while (Node <> NIL) do
    begin
        if (Node.NodeName = sNodeName) and (Node.HasChildNodes) then
        begin
            Result := true;
            sValue := Trim (Node.FirstChild.NodeValue);
            Break;
        end; { if }

        Node := Node.NextSibling;
    end; { while }
end; { TXmlBaseClass.GetChildNodeValue }

(* ---- *)

function TXmlBaseClass.GetFirstNamedNode (const Node: IXMLNode;
                                          out DataNode: IXMLNode) : Boolean;
begin
    Assert (Node <> NIL);

    DataNode := Node;

    repeat
        if (DataNode.NodeName <> cHashText) then
            Break
        else DataNode := Node.NextSibling;
    until (DataNode = NIL);

    Result := DataNode <> NIL;
end; { TXmlBaseClass.GetFirstNamedNode }

(* ---- *)

function TXmlBaseClass.GetNodeAssociatedValue (const Node: IXMLNode;
                                               out sValue: String) : Boolean;

var
    SiblingNode : IXMLNode;

begin
    Assert (Node <> NIL);

    Result := false;

    SiblingNode := Node.NextSibling;

    while (SiblingNode <> NIL) do
    begin
        if (SiblingNode.NodeName = cHashText) then
            if (Trim (SiblingNode.NodeValue) <> '') then
            begin
                Result := true;
                sValue := Trim (SiblingNode.NodeValue);
                Break;
            end; { if }

        SiblingNode := Node.NextSibling;
    end; { while }
end; { TXmlBaseClass.GetNodeAssociatedValue }

(* ---- *)

function TXmlBaseClass.GetNodeValue (const Node: IXMLNode;
                                     out sValue: String) : Boolean;
begin
    Assert (Node <> NIL);

    sValue := Trim (Node.NodeValue);
    Result := sValue <> '';
end; { TXmlBaseClass.GetNodeValue }

(* ---- *)

function TXmlBaseClass.SetNode (const sXmlNode: String) : IXMLNode;
begin
    Assert (sXmlNode <> '');

    Result := XmlDoc.SelectSingleNode (sXmlNode);

    if (Result = NIL) then
        raise EXMLException.CreateFmt (cXmlNodeNotFoundMsg, [sXmlNode]);
end; { TXmlBaseClass.SetNode }

(* ---- *)

{$IFDEF DEBUG}
procedure TXmlBaseClass.ShowNodesAndAttributes;

    (* ---- *)

    procedure ListAttributes (const XmlNode: IXMLNode; const sLevel: String);

    const
        cAttributeMsg = 'Attribute %d: Name = %s, Value = "%s"';

    var
        iAttribute : Integer;
        AttributeList : IXMLNamedNodeMap;

    begin
        if (XmlNode = NIL) then
            exit;

        //iterate through all attributes -> you MUST set the XmlNode to nil
        AttributeList := XmlNode.Attributes;

        if (AttributeList <> NIL) then
        begin
            for iAttribute := 0 to AttributeList.Length - 1 do
                with AttributeList.Item [iAttribute] do
                    WriteLn (Format (sLevel + cAttributeMsg,
                                     [iAttribute, NodeName, Trim (NodeValue)]));

            WriteLn;
        end; { if }
    end; { ListAttributes }

    (* ---- *)

(**
    procedure ListChildNodes (const ChildNodes: IXMLNodeList);

    var
        iChild : Integer;

    begin
        for iChild := 0 to ChildNodes.Length - 1 do
        begin
            XmlNode := ChildNodes.Item [iChild];
            WriteLn (Format ('Node %d name = %s, value = "%s"',
                             [iChild + 1, XmlNode.NodeName,
                              Trim (XmlNode.NodeValue)]));
            ListAttributes (XmlNode);

            if (XmlNode.HasChildNodes) then
                ListChildNodes (XmlNode.ChildNodes);
        end; { for }
    end; { ListChildNodes }
**)

    procedure ListChildNodes (const XmlNode: IXMLNode; const iLevel: Integer);

    var
        ChildNode : IXMLNode;
        iChild : Integer;
        sLevel : String;

    begin
        if not (XmlNode.HasChildNodes) then
            exit;

        iChild := 0;
        ChildNode := XmlNode.FirstChild;

        if (ilevel > 0) then
            sLevel := StringOfChar (' ', iLevel * 2);

        while (ChildNode <> NIL) do
        begin
            Inc (iChild);
            WriteLn (Format (sLevel + 'Node %d name = %s, value = "%s"',
                             [iChild, ChildNode.NodeName,
                              Trim (ChildNode.NodeValue)]));
            ListAttributes (ChildNode, sLevel);

            if (ChildNode.HasChildNodes) then
                ListChildNodes (ChildNode, iLevel + 1);

            ChildNode := ChildNode.NextSibling;
        end; { while }
    end; { ListChildNodes }

    (* ---- *)

var
    XmlRoot : IXMLNode;

begin { TXmlBaseClass.ShowNodesAndAttributes }
    XmlRoot := XmlDoc.DocumentElement;
    ListAttributes (XmlRoot, '');

    if not (XmlRoot.HasChildNodes) then
    begin
        WriteLn ('No childnodes found');
        exit;
    end; { if }

    ListChildNodes (XmlRoot, 0);
end; { TXmlBaseClass.ShowNodesAndAttributes }
{$ENDIF}

(* ---- *)

end.
