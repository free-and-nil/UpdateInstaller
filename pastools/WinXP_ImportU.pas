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

unit WinXP_ImportU;

{$MINENUMSIZE 4}

interface

uses Windows;

const
{$IFNDEF DELPHI_XE2_UP}
    PROCESSOR_ARCHITECTURE_INTEL            = 0;
    PROCESSOR_ARCHITECTURE_MIPS             = 1;
    PROCESSOR_ARCHITECTURE_ALPHA            = 2;
    PROCESSOR_ARCHITECTURE_PPC              = 3;
    PROCESSOR_ARCHITECTURE_SHX              = 4;
    PROCESSOR_ARCHITECTURE_ARM              = 5;
    PROCESSOR_ARCHITECTURE_IA64             = 6;
    PROCESSOR_ARCHITECTURE_ALPHA64          = 7;
    PROCESSOR_ARCHITECTURE_MSIL             = 8;
    PROCESSOR_ARCHITECTURE_AMD64            = 9;
    PROCESSOR_ARCHITECTURE_IA32_ON_WIN64    = 10;

    PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;
{$ENDIF}

    // This system metric is used in a Terminal Services environment. Its value
    // is nonzero if the current session is remotely controlled; otherwise, 0.
    SM_RemoteControl = $2001;  // GetSystemMetric

    SAFER_SCOPEID_MACHINE = 1;
    SAFER_SCOPEID_USER    = 2;
    SAFER_LEVELID_FULLYTRUSTED = $40000;
    SAFER_LEVELID_NORMALUSER   = $20000;
    SAFER_LEVELID_CONSTRAINED  = $10000;
    SAFER_LEVELID_UNTRUSTED    = $01000;
    SAFER_LEVELID_DISALLOWED   = $00000;

    SAFER_TOKEN_NULL_IF_EQUAL = $00000001;
    SAFER_TOKEN_COMPARE_ONLY  = $00000002;
    SAFER_TOKEN_MAKE_INERT    = $00000004;
    SAFER_TOKEN_WANT_FLAGS    = $00000008;

	SAFER_LEVEL_OPEN = 1;

const
	ATTACH_PARENT_PROCESS = DWord(-1);

type
{$IFNDEF FPC}
  {$IFNDEF DELPHI2009_UP}
    PLUID = ^TLUID;
    _LUID = record
        LowPart : DWord;
        HighPart : Integer;
    end; { _LUID }
    TLUID = _LUID;
    LUID = _LUID;
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHI2007_UP}
	ULONG_PTR = Cardinal;
	ULONGLONG = UInt64;
{$ENDIF}

{$IFNDEF DELPHI_XE_UP}
	PVOID = Pointer;
{$ENDIF}

	USHORT = Word;

    _LSA_UNICODE_STRING = record
        Length: USHORT;
        MaximumLength: USHORT;
        Buffer: LPWSTR;
    end; { _LSA_UNICODE_STRING }
    LSA_UNICODE_STRING = _LSA_UNICODE_STRING;

    _SECURITY_LOGON_TYPE = (
        seltFiller0, seltFiller1,
        Interactive,
        Network,
        Batch,
        Service,
        Proxy,
        Unlock,
        NetworkCleartext,
        NewCredentials,
        RemoteInteractive,
        CachedInteractive,
        CachedRemoteInteractive);
    SECURITY_LOGON_TYPE = _SECURITY_LOGON_TYPE;

    PSECURITY_LOGON_SESSION_DATA = ^SECURITY_LOGON_SESSION_DATA;
    _SECURITY_LOGON_SESSION_DATA = record
        Size: ULONG;
        LogonId: LUID;
        UserName: LSA_UNICODE_STRING;
        LogonDomain: LSA_UNICODE_STRING;
        AuthenticationPackage: LSA_UNICODE_STRING;
        LogonType: SECURITY_LOGON_TYPE;
        Session: ULONG;
        Sid: PSID;
        LogonTime: LARGE_INTEGER;
        LogonServer: LSA_UNICODE_STRING;
        DnsDomainName: LSA_UNICODE_STRING;
        Upn: LSA_UNICODE_STRING;
    end; { _SECURITY_LOGON_SESSION_DATA }
    SECURITY_LOGON_SESSION_DATA = _SECURITY_LOGON_SESSION_DATA;

const
    { Ist nur der Button "mb_OK" definiert, so gibt "MessageBoxTimeout" immer
      "1" zurück, auch wenn die Timeout-Zeit überschritten wurde! }
	MB_TimedOut = 32000;

type
	{ Doesn't work on XP if Vista manifest with IE version settings is used (?).
      MB_TIMEDOUT if no buttons clicked, otherwise "Result" will return the
      value of the button clicked. }
    SAFER_LEVEL_HANDLE = THANDLE;
    PSAFER_LEVEL_HANDLE = ^SAFER_LEVEL_HANDLE;
    TSaferLevelHandle = SAFER_LEVEL_HANDLE;
    PSaferLevelHandle = PSAFER_LEVEL_HANDLE;

{$IFNDEF DELPHI_XE2_UP}
type
    _LOGICAL_PROCESSOR_RELATIONSHIP = (RelationProcessorCore{ = 0}, RelationNumaNode{ = 1}, RelationCache{ = 2}, RelationProcessorPackage{ = 3}, RelationGroup{ = 4}, RelationAll = $FFFF);
    LOGICAL_PROCESSOR_RELATIONSHIP = _LOGICAL_PROCESSOR_RELATIONSHIP;
    TLogicalProcessorRelationship = LOGICAL_PROCESSOR_RELATIONSHIP;

const
	LTP_PC_SMT = $1;

type
    _PROCESSOR_CACHE_TYPE = (CacheUnified{ = 0}, CacheInstruction{ = 1}, CacheData{ = 2}, CacheTrace{ = 3});
    PROCESSOR_CACHE_TYPE = _PROCESSOR_CACHE_TYPE;
    TProcessorCacheType = PROCESSOR_CACHE_TYPE;

    _CACHE_DESCRIPTOR = record
      Level: BYTE;
      Associativity: BYTE;
      LineSize: WORD;
      Size: DWord;
      _Type: PROCESSOR_CACHE_TYPE;
    end;
    CACHE_DESCRIPTOR = _CACHE_DESCRIPTOR;
    PCACHE_DESCRIPTOR = ^_CACHE_DESCRIPTOR;
    TCacheDescriptor = _CACHE_DESCRIPTOR;
    PCacheDescriptor = PCACHE_DESCRIPTOR;

const
    CACHE_FULLY_ASSOCIATIVE = $FF;

type
    _SYSTEM_LOGICAL_PROCESSOR_INFORMATION = record
      ProcessorMask: ULONG_PTR;
      Relationship: LOGICAL_PROCESSOR_RELATIONSHIP;
      case Integer of
        0: (Flags: BYTE); // ProcessorCore
        1: (NodeNumber: DWord); // NumaNode
        2: (Cache: CACHE_DESCRIPTOR); //Cache
        3: (Reserved: array [0..1] of ULONGLONG);
    end;
    SYSTEM_LOGICAL_PROCESSOR_INFORMATION = _SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION = ^SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    TSystemLogicalProcessorInformation = SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    PSystemLogicalProcessorInformation = PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
{$ENDIF}

var
	AttachConsole : function (dwProcessId: DWord): BOOL; stdcall = NIL;

	(* Disables the window ghosting feature for the calling graphical user
       interface (GUI) process. Window ghosting is a Windows Manager feature
       that lets the user minimize, move, or close the main window of an
       application that is not responding. *)
    (* Solves the occasional problem with the disappearance of the modal
       window behind the main form (eg during various switching - I don't mean
       TForm.PopupMode) often in Windows 10. *)
	DisableProcessWindowsGhosting : procedure; stdcall = NIL;

    { XP SP1 / Server 2003 }
    GetFirmwareEnvironmentVariable : function (lpName, lpGuid: LPCTSTR;
                                               pBuffer: PVOID;
                                               nSize: DWord) : DWord;
                                               					  stdcall = NIL;
{$IFNDEF DELPHI_XE2_UP}
    GetLogicalProcessorInformation : function (
    	Buffer: PSystemLogicalProcessorInformation;
        var ReturnLength: DWord) : BOOL; stdcall = NIL;
{$ENDIF}

    GetNativeSystemInfo : procedure (var lpSystemInfo: TSystemInfo);
                                                                  stdcall = NIL;

    GetProcessImageFileName : function (hProcess: THandle;
    									lpImageFileName: LPTSTR;
                                        nSize: DWord) : DWord; stdcall = NIL;

{$IFNDEF DELPHI2010_UP}
    GetSystemTimes : function (
    				var lpIdleTime, lpKernelTime, lpUserTime: TFileTime) : BOOL;
            													  stdcall = NIL;
{$ENDIF}

    LsaEnumerateLogonSessions : function (var Count: ULONG;
                                          var List: PLUID) : LongInt;
                                                                  stdcall = NIL;

	LsaGetLogonSessionData : function (LogonId: PLUID; var
    	            ppLogonSessionData: PSECURITY_LOGON_SESSION_DATA) : LongInt;
                                                                  stdcall = NIL;

	MessageBoxTimeout : function (hWindow: HWND; lpText, lpCaption: LPTSTR;
								  uType: UInt; wLanguageId: WORD;
								  dwMilliseconds: DWord) : Integer;
                                                                  stdcall = NIL;

(**
    https://source.winehq.org/WineAPI/RtlComputeCrc32.html
    dwInitial [In]	Initial CRC value.
    pData 	  [In]	Data block.
    iLen 	  [In]	Length of the byte block.
**)
    RtlComputeCrc32 : function (dwInitial: DWord; const pData: Pointer;
                                iLen: Integer) : DWord; stdcall = NIL;

	SaferCreateLevel : function (dwScopeId, dwLevelId, OpenFlags: DWord;
        		                 pLevelHandle: PSAFER_LEVEL_HANDLE;
                                 lpReserved: Pointer) : BOOL; stdcall = NIL;

    SaferCloseLevel : function (hLevelHandle: SAFER_LEVEL_HANDLE) : BOOL;
                                                                  stdcall = NIL;

    SaferComputeTokenFromLevel : function (LevelHandle: SAFER_LEVEL_HANDLE;
                      InAccessToken: THANDLE; OutAccessToken: PHANDLE;
                      dwFlags: DWord; lpReserved: Pointer) : BOOL; stdcall = NIL;

    SetFirmwareEnvironmentVariable : function  (lpName, lpGuid: LPCTSTR;
                                                pValue: PVOID;
                                                nSize: DWord) : BOOL;
                                                				  stdcall = NIL;

	WTSGetActiveConsoleSessionId : function : DWord; stdcall = NIL;

implementation

uses SysUtils,
	 WinNT_ImportU, VerifyU;

const
{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}
    cNtDll = 'ntdll.dll';
	cPsApi = 'PSAPI.dll';

var
	hAdvApi32 : THandle = 0;
	hKernel32 : THandle = 0;
    hNtDll    : THandle = 0;
    hPsApi    : THandle = 0;
    hSecur32  : THandle = 0;
	hUser32   : THandle = 0;

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
	if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
		exit;

	if not (CheckWin32Version (5, 1)) then
		exit; { NT4 / Windows 2000 }

	hAdvApi32 := LoadLibrary (advapi32);

	if (hAdvApi32 <> 0) then
	begin
		SaferCloseLevel := GPA (hAdvApi32, 'SaferCloseLevel');
		SaferComputeTokenFromLevel := GPA (hAdvApi32,
        								   'SaferComputeTokenFromLevel');
		SaferCreateLevel := GPA (hAdvApi32, 'SaferCreateLevel');
	end; { if }

    hKernel32 := LoadLibrary (kernel32);

    if (hKernel32 <> 0) then
    begin
    	AttachConsole := GPA (hKernel32, 'AttachConsole');

        // Requires XP SP3 or Server 2003
        GetFirmwareEnvironmentVariable := GetProcAddress (hKernel32,
                                   'GetFirmwareEnvironmentVariable' + AWSuffix);

    	GetNativeSystemInfo := GPA (hKernel32, 'GetNativeSystemInfo');

{$IFNDEF DELPHI_XE2_UP}
        // Requires XP SP3 or Server 2003
        GetLogicalProcessorInformation := GetProcAddress (hKernel32,
        									  'GetLogicalProcessorInformation');
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
        // Requires XP SP1 or Server 2003
    	GetSystemTimes := GetProcAddress (hKernel32, 'GetSystemTimes');
{$ENDIF}

        // Requires XP SP1 or Server 2003
        SetFirmwareEnvironmentVariable := GetProcAddress (hKernel32,
                                   'SetFirmwareEnvironmentVariable' + AWSuffix);

        WTSGetActiveConsoleSessionId := GPA (hKernel32,
        									 'WTSGetActiveConsoleSessionId');
    end; { if }

    hNtDll := LoadLibrary (cNtDll);

    if (hNtDll <> 0) then
    begin
	    RtlComputeCrc32 := GPA (hNtDll, 'RtlComputeCrc32');
    end; { if }

    hPsApi := LoadLibrary (cPsApi);

    if (hPsApi <> 0) then
    begin
	    GetProcessImageFileName := GPA (hPsApi,
    									'GetProcessImageFileName' + AWSuffix);
    end; { if }

    hSecur32 := LoadLibrary (cSecur32);

    if (hSecur32 <> 0) then
    begin
    	LsaEnumerateLogonSessions := GPA (hSecur32, 'LsaEnumerateLogonSessions');
        LsaGetLogonSessionData := GPA (hSecur32, 'LsaGetLogonSessionData');
    end; { if }

	hUser32 := LoadLibrary (user32);

	if (hUser32 <> 0) then
    begin
    	DisableProcessWindowsGhosting := GPA (hUser32,
        									  'DisableProcessWindowsGhosting');
		MessageBoxTimeout := GPA (hUser32, 'MessageBoxTimeout' + AWSuffix);
    end; { if }
end; { initialization }

(* ---- *)

finalization
begin
    if (hAdvApi32 <> 0) then
    	VerifyApi (FreeLibrary (hAdvApi32));

    if (hKernel32 <> 0) then
        VerifyApi (FreeLibrary (hKernel32));

	if (hNtDll <> 0) then
    	VerifyApi (FreeLibrary (hNtDll));

	if (hPsApi <> 0) then
    	VerifyApi (FreeLibrary (hPsApi));

    if (hSecur32 <> 0) then
        VerifyApi (FreeLibrary (hSecur32));

    if (hUser32 <> 0) then
    	VerifyApi (FreeLibrary (hUser32));
end; { finalization }

(* ---- *)

end.

