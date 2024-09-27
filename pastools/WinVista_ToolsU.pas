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

{$IFDEF DELPHI7_UP}
	{$WARN SYMBOL_PLATFORM OFF}
	{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

unit WinVista_ToolsU;

interface

uses Windows;

const
	cKtmW32 = 'KtmW32.dll';

function GetTickCountEx : Int64;
function IsUserInLocalAdminsGroup : Boolean;
function IsVista : Boolean;
function LCIDToLocale (const LCID: LANGID) : String;
procedure SystemShutdownEx (const sComputerName: String = '';
						    const bRestart: Boolean = false;
                            const uTimeOut: UInt = 30;
                            const bForceSelfClosed: Boolean = false;
                            const bForceOthersClosed: Boolean = false;
                            const bInstallUpdates: Boolean = false;
                            const bRestartApps: Boolean = false;
                            const dwReason: DWord = 0;
                            const bAdvancedBootMenu: Boolean = false;
                            const sShutdownMsg: String = '');

implementation

uses SysUtils,
	 WinNT_ToolsU, VerifyU, WinVista_ImportU, PasTools;

(* ---- *)

function GetTickCountEx : Int64;
begin
    if (Win32MajorVersion >= 6) then
        Result := GetTickCount64
    else Result := GetTickCount;
end; { GetTickCountEx }

(* ---- *)

function IsUserInLocalAdminsGroup : Boolean;

const
	SECURITY_BUILTIN_DOMAIN_RID = $00000020;
	DOMAIN_ALIAS_RID_ADMINS = $00000220;
	SECURITY_NT_AUTHORITY : TSIDIdentifierAuthority =
													(Value: (0, 0, 0, 0, 0, 5));

var
	psidAdministrators : PSID;

begin
    Win32Check (AllocateAndInitializeSid (SECURITY_NT_AUTHORITY, 2,
										  SECURITY_BUILTIN_DOMAIN_RID,
										  DOMAIN_ALIAS_RID_ADMINS,
										  0, 0, 0, 0, 0, 0,
										  psidAdministrators{%H-}));

	try
		Result := IsUserInGroup (psidAdministrators);

	finally
		VerifyApi (FreeSid (psidAdministrators) = NIL);
	end; { try / finally }
end; { IsUserInLocalAdminsGroup }

(* ---- *)

function IsVista : Boolean;
begin
    Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    		  (Win32MajorVersion >= 6);
end; { IsVista }

(* ---- *)

function LCIDToLocale (const LCID: LANGID) : String;

var
    sLocale : WideString;
    iLen : Integer;

begin
	iLen := LCIDToLocaleName (LCID, PWideChar ({%H-}sLocale), 0, 0);

    if (iLen = 0) then
    	exit;

    SetLength (sLocale, iLen - 1);
    iLen := LCIDToLocaleName (LCID, PWideChar (sLocale), iLen, 0);

    if (iLen > 0) then
    	Result := String (sLocale);
end; { LCIDToLocale }

(* ---- *)

procedure SystemShutdownEx (const sComputerName: String = '';
						    const bRestart: Boolean = false;
                            const uTimeOut: UInt = 30;
                            const bForceSelfClosed: Boolean = false;
                            const bForceOthersClosed: Boolean = false;
                            const bInstallUpdates: Boolean = false;
                            const bRestartApps: Boolean = false;
                            const dwReason: DWord = 0;
                            const bAdvancedBootMenu: Boolean = false;
                            const sShutdownMsg: String = '');

var
    pchMachineName, pchShutdownMsg : PChar;
    dwResult, dwShutdownFlags : DWord;

begin
	AdjustPrivileges (SE_SHUTDOWN_NAME, true);

    pchMachineName := {%H-}iif (sComputerName <> '', PChar (sComputerName),
{$IFDEF FPC}
						   NIL);
{$ELSE}
  {$IFDEF DELPHI2007_UP}
    					   PChar (NIL));
  {$ELSE}
  						   NIL);
  {$ENDIF}
{$ENDIF}

    pchShutdownMsg := {%H-}iif (sShutdownMsg <> '', PChar (sShutdownMsg),
{$IFDEF FPC}
						   NIL);
{$ELSE}
  {$IFDEF DELPHI2007_UP}
    					   PChar (NIL));
  {$ELSE}
  						   NIL);
  {$ENDIF}
{$ENDIF}

    dwShutdownFlags := iif (bRestart, SHUTDOWN_RESTART, SHUTDOWN_POWEROFF);

    if (bForceSelfClosed) then
        dwShutdownFlags := dwShutdownFlags or SHUTDOWN_FORCE_SELF;

    if (bForceOthersClosed) then
        dwShutdownFlags := dwShutdownFlags or SHUTDOWN_FORCE_OTHERS;

    if (bInstallUpdates) then
        dwShutdownFlags := dwShutdownFlags or SHUTDOWN_INSTALL_UPDATES;

    if (bRestartApps) then
        dwShutdownFlags := dwShutdownFlags or SHUTDOWN_RESTARTAPPS;

    if (bAdvancedBootMenu) then
        dwShutdownFlags := dwShutdownFlags or SHUTDOWN_AdvancedBootMenu;

    dwResult := InitiateShutdown (pchMachineName, pchShutdownMsg, uTimeOut,
                                  dwShutdownFlags, dwReason);

    if (dwResult <> ERROR_SUCCESS) then
    begin
    	AdjustPrivileges (SE_SHUTDOWN_NAME, false);
        RaiseLastWin32Error
    end; { if }
end; { RestartSystemToAdvancedBootMenu }

(* ---- *)


end.
