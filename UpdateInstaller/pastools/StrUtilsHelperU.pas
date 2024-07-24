//
//  Original author: Olaf Hess
//  This work is published from: Germany.
//
//  To the extent possible under law, Olaf Hess has waived all copyright and
//  related or neighboring rights to this source code:
//  http://creativecommons.org/publicdomain/zero/1.0/
//
//  Unless expressly stated otherwise, the person who associated a work with
//  this deed makes no warranties about the work, and disclaims liability for
//  all uses of the work, to the fullest extent permitted by applicable law.
//

{$I ..\switches.inc}

unit StrUtilsHelperU;

interface

{$IFNDEF DELPHI_XE_UP}
uses {$IFNDEF FPC} Classes, {$ENDIF} Types;

function SplitString (const sStr: String;
                      const chDelimiter: Char) : TStringDynArray;
  {$IFNDEF FPC}
    overload;
procedure SplitString (const sStr: String; const chDelimiter: Char;
                       const AddTo: TStrings; const ClearList: Boolean); overload;
  {$ENDIF}  // FPC
{$ENDIF}  // DELPHI_XE_UP

function LastPos (const sSubStr, sStr: String) : Integer;

implementation

uses {$IFNDEF DELPHI_XE_UP}
        {$IFDEF FPC} Classes, lazstringutils, {$ENDIF}  // {$IFDEF FPC}
        SysUtils, {$ENDIF}  // {$IFNDEF DELPHI_XE_UP}
     StrUtils;

(* ---- *)
{$IFNDEF DELPHI_XE_UP}
function SplitString (const sStr: String;
                      const chDelimiter: Char) : TStringDynArray;

var
    List : TStringList;
    iIndex : Integer;

begin
    List := TStringList.Create;

    try
        {$IFDEF FPC}lazstringutils.{$ENDIF}SplitString (sStr, chDelimiter, List,
                                                        false);
        SetLength (Result{%H-}, List.Count);

        for iIndex := 0 to List.Count - 1 do
            Result [iIndex] := List [iIndex];

    finally
        List.Free;
    end; { try / finally }
end; { SplitString }

(* ---- *)

{$IFNDEF FPC}
(**
function SplitString (const s: String; Delimiter: Char): TStrings;
begin
  Result := TStringList.Create;
  SplitString (s,Delimiter,Result,false);
end;
**)

{ Routine taken from "lazstringutils.pas", part of Lazarus}
procedure SplitString (const sStr: String; const chDelimiter: Char;
                       const AddTo: TStrings; const ClearList: Boolean);

var
    iLen, iStartPos, iEndPos: Integer;

begin
    if ClearList then
        AddTo.Clear;

    iLen := length (sStr);
    iStartPos := 1;
    iEndPos := 1;

    repeat
        if (iEndPos <= iLen) and (sStr [iEndPos] <> chDelimiter) then
            Inc (iEndPos)
        else
        begin
            if (iEndPos > iStartPos) then
                AddTo.Add (copy (sStr, iStartPos, iEndPos - iStartPos));

            iStartPos := iEndPos + 1;

            if (iStartPos > iLen) then
                exit;

            Inc (iEndPos);
        end;
    until false;
end; { SplitString }
{$ENDIF}  // {$IFNDEF FPC}

(* ---- *)

{$ENDIF}  // {$IFNDEF DELPHI_XE_UP}

function LastPos (const sSubStr, sStr: String) : Integer;

var
    iPos, iLen : Integer;

begin
    Result := 0;
    iPos := 1;
    iLen := Length (sStr);

    repeat
        iPos := PosEx (sSubStr, sStr, iPos);

        if (iPos > 0) then
        begin
            Result := iPos;

            if (iPos < iLen) then
                Inc (iPos)
            else iPos := 0;
        end; { if }
    until (iPos = 0);
end; { LastPos }

(* ---- *)

end.

