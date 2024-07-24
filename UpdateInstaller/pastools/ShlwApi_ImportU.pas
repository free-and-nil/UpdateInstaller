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

unit ShlwApi_ImportU;

interface

uses Windows;

const
	// Values used by "IsOS"; returns TRUE/FALSE depending on question
	OS_WINDOWS                = 0;          // Windows 9x vs. NT
	OS_NT                     = 1;          // Windows 9x vs. NT
	OS_WIN95ORGREATER         = 2;          // Win95 or greater
	OS_NT4ORGREATER           = 3;          // NT4 or greater
	OS_WIN98ORGREATER         = 5;          // Win98 or greater
	OS_WIN98_GOLD             = 6;          // Win98 Gold (Version 4.10 build 1998)
	OS_WIN2000ORGREATER       = 7;          // Some derivative of Win2000

	// NOTE: these flags check explicitly for (dwMajorVersion == 5)
	OS_WIN2000PRO             = 8;          // Windows 2000 Professional (Workstation)
	OS_WIN2000SERVER          = 9;          // Windows 2000 Server
	OS_WIN2000ADVSERVER       = 10;         // Windows 2000 Advanced Server
	OS_WIN2000DATACENTER      = 11;         // Windows 2000 Data Center Server
	OS_WIN2000TERMINAL        = 12;         // Windows 2000 Terminal Server in "Application Server" mode (now simply called "Terminal Server")

	OS_EMBEDDED               = 13;         // Embedded Windows Edition
	OS_TERMINALCLIENT         = 14;         // Windows Terminal Client (eg user is comming in via tsclient)
	OS_TERMINALREMOTEADMIN    = 15;         // Terminal Server in "Remote Administration" mode
	OS_WIN95_GOLD             = 16;         // Windows 95 Gold (Version 4.0 Build 1995)
	OS_MEORGREATER            = 17;         // Windows Millennium (Version 5.0)
	OS_XPORGREATER            = 18;         // Windows XP or greater
	OS_HOME                   = 19;         // Home Edition (eg NOT Professional, Server, Advanced Server, or Datacenter)
	OS_PROFESSIONAL           = 20;         // Professional     (aka Workstation; eg NOT Server, Advanced Server, or Datacenter)
	OS_DATACENTER             = 21;         // Datacenter       (eg NOT Server, Advanced Server, Professional, or Personal)
	OS_ADVSERVER              = 22;         // Advanced Server  (eg NOT Datacenter, Server, Professional, or Personal)
	OS_SERVER                 = 23;         // Server           (eg NOT Datacenter, Advanced Server, Professional, or Personal)
	OS_TERMINALSERVER         = 24;         // Terminal Server - server running in what used to be called "Application Server" mode (now simply called "Terminal Server")
	OS_PERSONALTERMINALSERVER = 25;         // Personal Terminal Server - per/pro machine running in single user TS mode
	OS_FASTUSERSWITCHING      = 26;         // Fast User Switching
	OS_WELCOMELOGONUI         = 27;         // New friendly logon UI
	OS_DOMAINMEMBER           = 28;         // Is this machine a member of a domain (eg NOT a workgroup)
	OS_ANYSERVER              = 29;         // is this machine any type of server? (eg datacenter or advanced server or server)?
	OS_WOW6432                = 30;         // Is this process a 32-bit process running on an 64-bit platform?
	OS_WEBSERVER              = 31;         // Web Edition Server
	OS_SMALLBUSINESSSERVER    = 32;         // SBS Server
	OS_TABLETPC               = 33;         // Are we running on a TabletPC?
	OS_SERVERADMINUI          = 34;         // Should defaults lean towards those preferred by server administrators?
	OS_MEDIACENTER            = 35;         // eHome Freestyle Project
	OS_APPLIANCE              = 36;         // Windows .NET Appliance Server

var
	// http://www.geoffchappell.com/studies/windows/shell/shlwapi/api/isos/isos.htm
	IsOS : function (dwOS: DWord) : Bool; stdcall = NIL;

{$IFNDEF DELPHI2007_UP}
	PathIsDirectory : function (pszPath: PChar): BOOL; stdcall = NIL;
{$ENDIF}

    SHCopyKey : function (hkeySrc: HKEY; szSrcSubKey: LPTSTR; hkeyDest: HKEY;
						  fReserved: DWord) : DWord; stdcall = NIL;

	SHDeleteKey : function (hKey: HKEY;
    					    pszSubKey: LPCTSTR) : DWord; stdcall = NIL;

    SHMessageBoxCheck : function (hWindow: HWND; lpText, lpCaption: LPTSTR;
								  uType: UInt; iDefault: Integer;
                                  lpRegVal: LPTSTR) : Integer; stdcall = NIL;

    SHLoadIndirectString : function (pszSource, pszOutBuf: PWideChar;
    				  		 cchOutBuf: UINT;
                      		 var ppvReserved: Pointer) : HResult; stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU {, Win32ToolsU};

const
    cShlwApi  = 'shlwapi.dll';
{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}

{$IFDEF UNICODE}
    cSHMessageBoxCheck = MakeIntResourceA (191);
{$ELSE}
    cSHMessageBoxCheck = MakeIntResource (185);
{$ENDIF}

var
    hShlwApi  : THandle = 0;

(* ---- *)

initialization
begin
    hShlwApi := LoadLibrary (cShlwApi);

    if (hShlwApi = 0) then
    	exit;

//  IsOS := GetProcAddress (hShlWApi, 'IsOS');
    IsOS := GetProcAddress (hShlWApi, MAKEINTRESOURCE (437));

{$IFNDEF DELPHI2007_UP}
	PathIsDirectory := GetProcAddress  (hShlwApi, 'PathIsDirectory' + AWSuffix);
{$ENDIF}

//	SHMessageBoxCheck := GPA (hShlwapi, 'SHMessageBoxCheck' + AWSuffix);
    SHMessageBoxCheck := GetProcAddress (hShlwapi, cSHMessageBoxCheck);

    SHCopyKey := GetProcAddress  (hShlwApi, 'SHCopyKey' + AWSuffix);
    SHDeleteKey := GetProcAddress  (hShlwApi, 'SHDeleteKey' + AWSuffix);
    SHLoadIndirectString := GetProcAddress  (hShlwapi,
                                             'SHLoadIndirectString');
end; { initialization }

(* ---- *)

finalization
begin
    if (hShlwApi <> 0) then
    	VerifyApi (FreeLibrary (hShlwApi));
end; { finalization }

(* ---- *)

end.

