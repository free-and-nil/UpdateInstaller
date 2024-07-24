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

unit Wow64U;

{$IFDEF DELPHI7_UP}
    {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses Windows;

const
    IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

{$IFNDEF DELPHI2007_UP}
    KEY_WOW64_64KEY = $0100;
{$ENDIF}

type
    LONG = LongInt;  // https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types

procedure ChangeWow64FSRedirection (const bDisableRedirection: Boolean);
function DeleteKeyEx (const HRootKey: HKey; const sKey: String;
                      const bDisableRedirection: Boolean = false) : Boolean;
function GetSysNativePath (sPath: String = '') : String;
function IsWindows_x64 : Boolean;
function IsWow64 : Boolean; overload;
function IsWow64 (const hProcess: THandle) : Boolean; overload;
function KeyExists (const HRootKey: HKey; const sKey: String;
                    const bDisableRedirection: Boolean = false) : Boolean;

var
    IsWow64Process : function (Handle: THandle; var Res: BOOL) : BOOL; stdcall = NIL;
    RegDeleteKeyEx : function (hKey: HKEY; lpSubKey: LPTSTR; samDesired: REGSAM;
                               Reserved: DWORD) : LONG; stdcall = NIL;
    Wow64DisableWow64FsRedirection : function (var OldValue: BOOL) : BOOL = NIL;
    Wow64EnableWow64FsRedirection : function (
                          Wow64FsEnableRedirection: BOOL) : BOOL; stdcall = NIL;
    Wow64RevertWow64FsRedirection : function (OldValue: BOOL) : BOOL; stdcall = NIL;

implementation

uses SysUtils,
     WinXP_ImportU, Win32ToolsU, PasTools;

var
    _bIsWow64 : Boolean = false;

(* ---- *)

procedure ChangeWow64FSRedirection (const bDisableRedirection: Boolean);
begin
    Win32Check (Wow64EnableWow64FsRedirection (not bDisableRedirection));
end; { ChangeWow64FSRedirection }

(* ---- *)

function DeleteKeyEx (const HRootKey: HKey; const sKey: String;
                      const bDisableRedirection: Boolean = false) : Boolean;

var
    samDesired : REGSAM;

begin
    Assert (sKey <> '');

    if (bDisableRedirection) then
        samDesired := KEY_WOW64_64KEY
    else samDesired := 0;

    Result := RegDeleteKeyEx (HRootKey, PChar (sKey),
                              samDesired, 0) = ERROR_SUCCESS;
end; { DeleteKeyEx }

(* ---- *)

function GetSysNativePath (sPath: String = '') : String;

const
    cSystem32 = '\system32';

var
    iPos : Integer;
    sLowerPath : String;

begin
    if (sPath = '') then
    	sPath := GetSystemDir;

    if (_bIsWow64) then
    begin
        sLowerPath := LowerCase (sPath);

        if (Pos (LowerCase (GetSystemDir), sLowerPath) > 0) then
        begin
            iPos := PosEx (cSystem32, sLowerPath, PosEx (':\', sPath) + 2);

            Result := sPath;
            Delete (Result, iPos, Length (cSystem32));
            Insert ('\sysnative', Result, iPos);
            exit;
        end; { if }
    end; { if }

    Result := sPath;
end; { GetSysNativePath }

(* ---- *)

function IsWindows_x64 : Boolean;

var
	SI : TSystemInfo;

begin
	if (CheckWin32Version (5, 2)) then  // Server 2003 / Windows XP x64 and up
    begin
		FillChar (SI{%H-}, SizeOf (TSystemInfo), #0);
        GetNativeSystemInfo (SI);

        Result := SI.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64;
    end { if }
    else Result := false;
end; { Iswindows_x64 }

(* ---- *)

function IsWow64 : Boolean;
begin
    Result := _bIsWow64;
end; { IsWow64 }

(* ---- *)

function IsWow64 (const hProcess: THandle) : Boolean; overload;

var
	bIsWow64Result : Bool;

begin
    if (Assigned (IsWow64Process)) then
    begin
        Win32Check (IsWow64Process (hProcess, bIsWow64Result{%H-}));
        Result := bIsWow64Result
    end { if }
    else Result := false;
end; { IsWow64 }

(* ---- *)

function KeyExists (const HRootKey: HKey; const sKey: String;
                    const bDisableRedirection: Boolean = false) : Boolean;
{ Test, ob ein Schlüssel existiert.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssels, dessen Existenz überprüft wird;
  <<- Result : True, wenn der Schlüssel existiert. }

    (* ---- *)

    function OpenKeyReadOnly (const HRootKey: HKEY; const sKey: String;
                              const bDisableRedirection: Boolean;
                              out Key: HKEY) : Boolean;

    var
        samDesired : REGSAM;

    begin
        Assert (sKey <> '');

        if (bDisableRedirection) then
            samDesired := KEY_READ or KEY_WOW64_64KEY
        else samDesired := KEY_READ;

        Result := RegOpenKeyEx (HRootKey, PChar (sKey), 0, samDesired,
                                Key{%H-}) = Error_Success;
    end; { OpenKeyReadOnly }

    (* ---- *)

    function CloseKey (const Key: HKEY) : Boolean;
    begin
        Assert (Key <> 0);

        Result := RegCloseKey (Key) = Error_Success;
    end; { CloseKey }

    (* ---- *)

var
    Key: HKEY;

begin { KeyExists }
    Assert (sKey <> '');

    Result := OpenKeyReadOnly (HRootKey, sKey, bDisableRedirection, Key);

    if (Result) then
        CloseKey (Key);
end; { KeyExists }

(* ---- *)

const
{$IFDEF UNICODE}
    cRegDeleteKeyEx = 'RegDeleteKeyExW';
{$ELSE}
    cRegDeleteKeyEx = 'RegDeleteKeyExA';
{$ENDIF}

var
    hAdvApi32 : THandle = 0;
    hKernel32 : THandle = 0;

initialization
    hKernel32 := LoadLibrary (kernel32);

    if (hKernel32 <> 0) and (CheckWin32Version (5, 1)) then
    begin
        IsWow64Process := GetProcAddress (hKernel32, 'IsWow64Process');

        _bIsWow64 := IsWow64 (GetCurrentProcess);

        if (_bIsWow64) then
        begin
            Wow64DisableWow64FsRedirection := GetProcAddress (hKernel32,
                                              'Wow64DisableWow64FsRedirection');
            Wow64EnableWow64FsRedirection := GetProcAddress (hKernel32,
                                               'Wow64EnableWow64FsRedirection');
            Wow64RevertWow64FsRedirection := GetProcAddress (hKernel32,
                                               'Wow64RevertWow64FsRedirection');

            if (Win32MajorVersion >= 6) then
            begin
                Assert (Assigned (Wow64DisableWow64FsRedirection));
                Assert (Assigned (Wow64EnableWow64FsRedirection));
                Assert (Assigned (Wow64RevertWow64FsRedirection));
            end; { if }
        end; { if }
    end; { if }

    hAdvApi32 := LoadLibrary (advapi32);

    if (hAdvApi32 <> 0) then
        if (CheckWin32Version (5, 2)) then
            RegDeleteKeyEx := GetProcAddress (hAdvApi32, cRegDeleteKeyEx);

finalization
    if (hKernel32 <> 0) then
        FreeLibrary (hKernel32);

    if (hAdvApi32 <> 0) then
        FreeLibrary (hAdvApi32);
end.

