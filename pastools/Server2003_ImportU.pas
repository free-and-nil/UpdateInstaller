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

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit Server2003_ImportU;

interface

uses Windows;

const
    //
    // RRF - Registry Routine Flags (for RegGetValue)
    //

    RRF_RT_REG_NONE      = $00000001;  // restrict type to REG_NONE      (other data types will not return ERROR_SUCCESS)
    RRF_RT_REG_SZ        = $00000002;  // restrict type to REG_SZ        (other data types will not return ERROR_SUCCESS) (automatically converts REG_EXPAND_SZ to REG_SZ unless RRF_NOEXPAND is specified)
    RRF_RT_REG_EXPAND_SZ = $00000004;  // restrict type to REG_EXPAND_SZ (other data types will not return ERROR_SUCCESS) (must specify RRF_NOEXPAND or RegGetValue will fail with ERROR_INVALID_PARAMETER)
    RRF_RT_REG_BINARY    = $00000008;  // restrict type to REG_BINARY    (other data types will not return ERROR_SUCCESS)
    RRF_RT_REG_DWORD     = $00000010;  // restrict type to REG_DWORD     (other data types will not return ERROR_SUCCESS)
    RRF_RT_REG_MULTI_SZ  = $00000020;  // restrict type to REG_MULTI_SZ  (other data types will not return ERROR_SUCCESS)
    RRF_RT_REG_QWORD     = $00000040;  // restrict type to REG_QWORD     (other data types will not return ERROR_SUCCESS)

    RRF_RT_DWORD         = (RRF_RT_REG_BINARY or RRF_RT_REG_DWORD); // restrict type to *32-bit* RRF_RT_REG_BINARY or RRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
    RRF_RT_QWORD         = (RRF_RT_REG_BINARY or RRF_RT_REG_QWORD); // restrict type to *64-bit* RRF_RT_REG_BINARY or RRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
    RRF_RT_ANY           = $0000ffff;  // no type restriction

    RRF_NOEXPAND         = $10000000;  // do not automatically expand environment strings if value is of type REG_EXPAND_SZ
    RRF_ZEROONFAILURE    = $20000000;  // if pvData is not NULL, set content to all zeros on failure

(**
	// Funktioniert erst ab Windows 10
    RRF_SUBKEY_WOW6464KEY = $00010000;
    RRF_SUBKEY_WOW6432KEY = $00020000;
**)

var
    RegDeleteKeyEx : function (hKey: HKEY; lpSubKey: LPTSTR;
                               samDesired: REGSAM;
                               Reserved: DWORD) : LongInt; stdcall = NIL;

    RegGetValue : function (hKey: HKEY; lpSubKey, lpValue: LPTSTR;
    						dwFlags: DWord; pdwType: PDWORD; pvData: Pointer;
                            pcbData: PDWORD) : LongInt; stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU, GetServicePackU;

const
{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}

var
	hAdvApi32 : THandle = 0;
    iSP : Integer = 1;

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
	if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    	exit;

    if (Win32MajorVersion < 6) then
    	if (CheckWin32Version (5, 2)) then
    		iSP := GetServicePack
        else exit;

	hAdvApi32 := LoadLibrary (advapi32);

	if (hAdvApi32 <> 0) and (iSP >= 1) then
    begin  // Setzt Server 2003 SP1 voraus
        RegDeleteKeyEx := GPA (hAdvApi32, 'RegDeleteKeyEx' + AWSuffix);
        RegGetValue := GPA (hAdvApi32, 'RegGetValue' + AWSuffix);
    end; { if }
end; { initialization }

(* ---- *)

finalization
	if (hAdvApi32 <> 0) then
    	VerifyApi (FreeLibrary (hAdvApi32));
end.

