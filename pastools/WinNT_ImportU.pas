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

unit WinNT_ImportU;

interface

{$MINENUMSIZE 4}

uses Windows;

type
    NTSTATUS = ULONG;

{$IFNDEF DELPHI2009_UP}
    POSVersionInfoExA = ^TOSVersionInfoExA;
    POSVersionInfoExW = ^TOSVersionInfoExW;
    POSVersionInfoEx = POSVersionInfoExW;
    _OSVERSIONINFOEXA = record
      dwOSVersionInfoSize: DWORD;
      dwMajorVersion: DWORD;
      dwMinorVersion: DWORD;
      dwBuildNumber: DWORD;
      dwPlatformId: DWORD;
      { Maintenance AnsiString for PSS usage }
      szCSDVersion: array[0..127] of AnsiChar;
      wServicePackMajor: WORD;
      wServicePackMinor: WORD;
      wSuiteMask: WORD;
      wProductType: BYTE;
      wReserved:BYTE;
    end;
    _OSVERSIONINFOEXW = record
      dwOSVersionInfoSize: DWORD;
      dwMajorVersion: DWORD;
      dwMinorVersion: DWORD;
      dwBuildNumber: DWORD;
      dwPlatformId: DWORD;
      { Maintenance UnicodeString for PSS usage }
      szCSDVersion: array[0..127] of WideChar;
      wServicePackMajor: WORD;
      wServicePackMinor: WORD;
      wSuiteMask: WORD;
      wProductType: BYTE;
      wReserved:BYTE;
    end;
    _OSVERSIONINFOEX = _OSVERSIONINFOEXW;
    TOSVersionInfoExA = _OSVERSIONINFOEXA;
    TOSVersionInfoExW = _OSVERSIONINFOEXW;
{$IFDEF UNICODE}
    TOSVersionInfoEx = TOSVersionInfoExW;
{$ELSE}
    TOSVersionInfoEx = TOSVersionInfoExA;
{$ENDIF}
    OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
    OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
    OSVERSIONINFOEX = OSVERSIONINFOEXW;
    LPOSVERSIONINFOEXA = POSVERSIONINFOEXA;
    LPOSVERSIONINFOEXW = POSVERSIONINFOEXW;
    LPOSVERSIONINFOEX = LPOSVERSIONINFOEXW;
    RTL_OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
    PRTL_OSVERSIONINFOEXW = POSVERSIONINFOEXW;
{$ENDIF}

{$IFDEF DELPHI2010_UP}
    PTTokenUser = Windows.PTokenUser;
{$ELSE}
    PTTokenUser = ^TTokenUser;
    _TOKEN_USER = record
      User : TSIDAndAttributes;
    end;
    {$EXTERNALSYM _TOKEN_USER}
    TTokenUser = _TOKEN_USER;
    TOKEN_USER = _TOKEN_USER;
    {$EXTERNALSYM TOKEN_USER}
{$ENDIF}

const
	cSecur32 = 'Secur32.dll';

    // Lsa... functions
    STATUS_SUCCESS = NTSTATUS ($00000000);

var
	CommandLineToArgvW : function (lpCmdLine: PWideChar;
    							   var pNumArgs: Integer) : PPWideChar;
                                                                  stdcall = NIL;
	CreateEnvironmentBlock : function (out lpEnvironment: Pointer;
    								   hToken: THANDLE;
                                       bInherit: BOOL) : BOOL; stdcall = NIL;
    DestroyEnvironmentBlock : function (lpEnvironment: Pointer) : BOOL;
                                                                  stdcall = NIL;

{$IFNDEF DELPHI2010_UP}
	IsDebuggerPresent : function : BOOL; stdcall = NIL;
{$ENDIF}
	LsaFreeReturnBuffer : function (Buffer: Pointer) : NTSTATUS; stdcall = NIL;
    LsaNtStatusToWinError : function (Status: NTSTATUS) : ULONG; stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU;

const
	cShell32 = 'shell32.dll';
	cUserEnv = 'userenv.dll';

var
	hAdvApi32 : THandle = 0;
    hKernel32 : THandle = 0;
    hSecur32  : THandle = 0;
    hShell32 : THandle = 0;
    hUserEnv : THandle = 0;

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
	if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Win32MajorVersion < 4) then
        exit;

    hAdvApi32 := LoadLibrary (Windows.advapi32);

    if (hAdvApi32 <> 0) then
        LsaNtStatusToWinError := GPA (hAdvApi32, 'LsaNtStatusToWinError');

{$IFNDEF DELPHI2010_UP}
    hKernel32 := LoadLibrary (kernel32);

    if (hKernel32 <> 0) then
    begin
		IsDebuggerPresent := GPA (hKernel32, 'IsDebuggerPresent');
    end; { if }
{$ENDIF}

    hSecur32 := LoadLibrary (cSecur32);

    if (hSecur32 <> 0) then
    	LsaFreeReturnBuffer := GPA (hSecur32, 'LsaFreeReturnBuffer');

    hShell32 := LoadLibrary (cShell32);

    if (hShell32 <> 0) then
    	CommandLineToArgvW := GPA (hShell32, 'CommandLineToArgvW');

    hUserEnv := LoadLibrary (cUserEnv);

    if (hUserEnv <> 0) then
    begin
    	CreateEnvironmentBlock := GPA (hUserEnv, 'CreateEnvironmentBlock');
        DestroyEnvironmentBlock := GPA (hUserEnv, 'DestroyEnvironmentBlock');
    end; { if }
end; { initialization }

(* ---- *)

finalization
begin
    if (hAdvApi32 <> 0) then
    	VerifyApi (FreeLibrary (hAdvApi32));

	if (hKernel32 <> 0) then
		VerifyApi (FreeLibrary (hKernel32));

    if (hSecur32 <> 0) then
        VerifyApi (FreeLibrary (hSecur32));

    if (hShell32 <> 0) then
        VerifyApi (FreeLibrary (hShell32));

    if (hUserEnv <> 0) then
    	VerifyApi (FreeLibrary (hUserEnv));
end; { finalization }

(* ---- *)

end.
