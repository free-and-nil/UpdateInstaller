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

unit RegistryHelperU;

interface

uses Windows;

const
	cHKLM = HKey_Local_Machine;
    cHKCU = HKey_Current_User;

{$IFNDEF DELPHI2007_UP}
    KEY_WOW64_32KEY        = $0200;
    KEY_WOW64_64KEY        = $0100;
{$ENDIF}

function GetSamDesired (const bDisableRedirection: Boolean = false;
                        const bReadOnly: Boolean = false) : REGSAM;
{$IFNDEF DEBUG}
	{$IFDEF SUPPORTS_INLINE}
																	inline;
	{$ENDIF}
{$ENDIF}

function SetRegKey (var sKey: String; out hRegKey: HKEY) : Boolean;

var
	b_x64 : Boolean = false;

implementation

uses SysUtils,
     Wow64U;

(* ---- *)

function GetSamDesired (const bDisableRedirection: Boolean = false;
                        const bReadOnly: Boolean = false) : REGSAM;

const
{$IFDEF WIN64}
	cKey_Wow64 = KEY_WOW64_32KEY;
{$ELSE}
	cKey_Wow64 = KEY_WOW64_64KEY;
{$ENDIF}

begin
	if (bDisableRedirection) and (b_x64) then
    begin
		if (bReadOnly) then
        	Result := KEY_READ or cKey_Wow64
        else Result := KEY_ALL_ACCESS or cKey_Wow64;
    end { if }
    else
		if (bReadOnly) then
        	Result := KEY_READ
        else Result := KEY_ALL_ACCESS;
end; { GetSamDesired }

(* ---- *)

function SetRegKey (var sKey: String; out hRegKey: HKEY) : Boolean;

const
    cHKey_Current_User = 'hkey_current_user\';
    cHKey_Local_Machine = 'hkey_local_machine\';

var
    iLen : Integer;

begin
    if (Pos (cHkey_Local_Machine, LowerCase (sKey)) = 1) then
    begin
        hRegKey := HKEY_LOCAL_MACHINE;
        iLen := Length (cHKey_Local_Machine);
    end { if }
    else if (Pos (cHKey_Current_User, LowerCase (sKey)) = 1) then
    begin
        hRegKey := HKEY_CURRENT_USER;
        iLen := Length (cHKey_Current_User);
    end { else }
    else iLen := 0;

    if (iLen > 0) then
    begin
        Result := true;
        Delete (sKey, 1, iLen);
    end { if }
    else Result := false;
end; { SetRegKey }

(* ---- *)

initialization
begin
{$IFDEF WIN64}
	b_x64 := true;
{$ELSE}
	b_x64 := IsWindows_x64;
{$ENDIF}
end; { initialization }

(* ---- *)

end.

