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

unit WinVista_ImportU;

interface

{$MINENUMSIZE 4}

uses Windows;

const
    MSGFLT_ADD    = 1;
    MSGFLT_REMOVE = 2;

//
// =========================================
// Define GUIDs which represent well-known power settings
// =========================================
//
// Video settings
// --------------

// Specifies whether adaptive display dimming is turned on or off.
    GUID_VIDEO_ANNOYANCE_TIMEOUT : TGUID =
        '{82DBCF2D-CD67-40C5-BFDC-9F1A5CCD4663}';

// Specifies how much adaptive dim time out will be increased by.
    GUID_VIDEO_ADAPTIVE_PERCENT_INCREASE : TGUID =
        '{EED904DF-B142-4183-B10B-5A1197A37864}';

// Specifies if the monitor is currently being powered or not.
    GUID_MONITOR_POWER_ON : TGUID = '{02731015-4510-4526-99E6-E5A17EBD1AEA}';

// Monitor brightness policy when in normal state
    GUID_DEVICE_POWER_POLICY_VIDEO_BRIGHTNESS : TGUID =
        '{aded5e82-b909-4619-9949-f5d71dac0bcb}';

// Monitor brightness policy when in dim state
    GUID_DEVICE_POWER_POLICY_VIDEO_DIM_BRIGHTNESS : TGUID =
        '{f1fbfde2-a960-4165-9f88-50667911ce96}';

// Current Monitor brightness
    GUID_VIDEO_CURRENT_MONITOR_BRIGHTNESS : TGUID =
        '{8ffee2c6-2d01-46be-adb9-398addc5b4ff}';


// Specifies if the operating system should use ambient light sensor to change
// disply brightness adatively.
    GUID_VIDEO_ADAPTIVE_DISPLAY_BRIGHTNESS : TGUID =
        '{FBD9AA66-9553-4097-BA44-ED6E9D65EAB8}';

// Specifies a change in the current monitor's display state.
    GUID_CONSOLE_DISPLAY_STATE : TGUID =
        '{6fe69556-704a-47a0-8f24-c28d936fda47}';

// Defines a guid for enabling/disabling the ability to create display required
// power requests.
    GUID_ALLOW_DISPLAY_REQUIRED : TGUID = '{A9CEB8DA-CD46-44FB-A98B-02AF69DE4623}';

// Specifies the video power down timeout (in seconds) after the interactive
// console is locked (and sensors indicate UserNotPresent). Value 0
// effectively disables this feature.
    GUID_VIDEO_CONSOLE_LOCK_TIMEOUT : TGUID =
        '{8EC4B3A5-6868-48c2-BE75-4F3044BE88A7}';

    // Shutdown flags ("InitiateShutdown")
    SHUTDOWN_FORCE_OTHERS     = $00000001;
    SHUTDOWN_FORCE_SELF       = $00000002;
    SHUTDOWN_RESTART          = $00000004;
    SHUTDOWN_POWEROFF         = $00000008;
    SHUTDOWN_NOREBOOT         = $00000010;
    SHUTDOWN_GRACE_OVERRIDE   = $00000020;
    SHUTDOWN_INSTALL_UPDATES  = $00000040;
    SHUTDOWN_RESTARTAPPS      = $00000080;
    // https://stackoverflow.com/questions/34804023/how-to-reboot-windows-8-10-and-go-to-advanced-boot-options-with-winapi
    SHUTDOWN_AdvancedBootMenu = $00000400;  // Undocumented

type
{$IFNDEF DELPHI_2010_UP}
    USHORT = Word;
{$ENDIF}

    HPOWERNOTIFY = Pointer;
    PHPOWERNOTIFY = ^HPOWERNOTIFY;

    LONG = LongInt;

    //
    // IPv6 Internet address (RFC 2553)
    // This is an 'on-wire' format structure.
    //
    IN6_ADDR = record
      case Integer of
        0: (Byte: array [0..15] of UCHAR);
        1: (Word: array [0..7] of USHORT);
    end;
    TIn6Addr = IN6_ADDR;
    PIn6Addr = ^IN6_ADDR;

    in_addr6 = in6_addr;
    TInAddr6 = in_addr6;
    PInAddr6 = ^in_addr6;

{$IFDEF FPC}
const
	DEVICE_NOTIFY_WINDOW_HANDLE  = $00000000;

type
    PPOWERBROADCAST_SETTING = {%H-}^POWERBROADCAST_SETTING;
    POWERBROADCAST_SETTING = record
        PowerSetting: TGUID;
        DataLength: DWORD;
        Data: packed array[0..0] of UCHAR;
    end; { POWERBROADCAST_SETTING }
    TPowerBroadcastSetting = POWERBROADCAST_SETTING;
    PPowerBroadcastSetting = ^TPowerBroadcastSetting;
{$ENDIF}

(**
RegCopyTree
RegDeleteKeyValue
RegSetKeyValue

QueryFullProcessImageName
Windows 2000 = GetModuleFileName()
Windows XP x32 = GetProcessImageFileName()
Windows XP x64 = GetProcessImageFileName()
Windows Vista = QueryFullProcessImageName()
Windows 7 = QueryFullProcessImageName()
**)

const
    IO_REPARSE_TAG_SYMLINK = $A000000C;
    SYMBOLIC_LINK_FLAG_DIRECTORY = $1;
    PROCESS_QUERY_LIMITED_INFORMATION = $1000;

    MAX_STR_BLOCKREASON = 256;

var
    ChangeWindowMessageFilter : function (Message: UINT; dwFlag: DWORD) : BOOL;
                                                                  stdcall = NIL;

    CreateSymbolicLink : function (lpSymlinkFileName, lpTargetFileName: LPTSTR;
                                    dwFlags: DWORD) : BOOL; stdcall = NIL;

	FindFirstFileNameW : function (lpFileName: LPCWSTR; dwFlags: DWord;
                                	var StringLength: DWord;
                                    LinkName: LPWSTR) : THandle; stdcall = NIL;

	FindNextFileNameW : function (hFindStream: THandle;
    							   var StringLength: DWord;
                                   LinkName: LPWSTR) : BOOL; stdcall = NIL;

    GetFinalPathNameByHandle : function (hFile: THandle; lpszFilePath: LPTSTR;
                                         cchFilePath: DWORD;
                                         dwFlags: DWORD) : DWORD; stdcall = NIL;

{$IFNDEF FPC}
    {$IFDEF DELPHI2007_UP}
        GetTickCount64 : function : UInt64; stdcall = NIL;
    {$ELSE}
        GetTickCount64 : function : Int64; stdcall = NIL;
    {$ENDIF}
{$ENDIF}

    InitiateShutdown : function (lpMachineName, lpMessage: LPTSTR;
                                 dwGracePeriod, dwShutdownFlags,
                                        dwReason: DWord) : DWord; stdcall = NIL;

    LCIDToLocaleName : function (Locale: LCID; lpName: LPWSTR; cchName: Integer;
  								 dwFlags: DWord) : Integer; stdcall = NIL;

{$IFNDEF DELPHI2007_UP}
    // https://stackoverflow.com/questions/63501567/how-to-load-an-icon-from-resources-to-a-timage
    LoadIconWithScaleDown : function  (hinst: HINST; pszName: LPCWSTR;
                                       cx, cy: Integer;
                                       var phico: HICON) : HResult;
                                                                  stdcall = NIL;
{$ENDIF}

    LocaleNameToLCID : function (lpName: LPWSTR;
    							 dwFlags: DWord) : LCID; stdcall = NIL;


	QueryFullProcessImageName : function (hProcess: THandle; dwFlags: DWord;
    									  lpExeName: LPTSTR;
                                          var dwSize: DWord) : BOOL;
                                          						  stdcall = NIL;

    RegDeleteTree : function (hKey: HKEY; lpSubKey: LPTSTR) : LONG;
                                                                  stdcall = NIL;

    RegisterPowerSettingNotification :
                        function (hRecipient: THandle;
                                  const PowerSettingGuid: TGuid;
                                  dwFlags: DWord) : HPOWERNOTIFY; stdcall = NIL;

    RtlIpv6AddressToStringA : function (Addr: PIn6Addr;
    									S: PAnsiChar) : PAnsiChar;
                                                                  stdcall = NIL;

    RtlIpv6AddressToStringW : function (Addr: PIn6Addr;
    									S: PWideChar) : PWideChar;
                                                                  stdcall = NIL;

    RtlIpv6AddressToString : function (Addr: PIn6Addr;
    								   S: LPTSTR) : LPTSTR; stdcall = NIL;

{$IFNDEF DELPHI2010_UP}
    SetProcessDPIAware : function : BOOL; stdcall = NIL;

    SHGetKnownFolderPath : function (const rfid: TGuid; dwFlags: DWORD;
                                     hToken: THandle;
                                     var ppszPath: LPWSTR) : HRESULT;
                                                                  stdcall = NIL;
{$ENDIF}

    ShutdownBlockReasonCreate : function (hWindow: HWnd;
                                          pwszReason: PWideChar) : Bool;
                                                                  stdcall = NIL;

    ShutdownBlockReasonQuery : function (hWindow: HWnd;
                                         pwszBuff: PWideChar;
                                         var pcchBuff: DWord) : Bool;
                                                                  stdcall = NIL;

    ShutdownBlockReasonDestroy : function (hWindow: HWnd) : Bool; stdcall = NIL;

    UnregisterPowerSettingNotification :
                         function (Handle: HPOWERNOTIFY) : Bool;  stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU;

const
    cComCtrl32 = 'comctl32.dll';
	cNtDll = 'ntdll.dll';
	cShell32 = 'shell32.dll';

{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}

var
	hAdvApi32 : THandle = 0;
    hComCtl32 : THandle = 0;
	hKernel32 : THandle = 0;
    hNtDll 	  : THandle = 0;
    hShell32  : THandle = 0;
    hUser32   : THandle = 0;

{$I GetProcAddress.inc}

initialization
	if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Win32MajorVersion < 6) then
        exit;

	hAdvApi32 := LoadLibrary (advapi32);

	if (hAdvApi32 <> 0) then
    begin
        InitiateShutdown := GPA (hAdvApi32, 'InitiateShutdown' + AWSuffix);
        RegDeleteTree := GPA (hAdvApi32, 'RegDeleteTree' + AWSuffix);
    end; { if }

    hComCtl32 := LoadLibrary (cComCtrl32);

{$IFNDEF DELPHI2007_UP}
	if (hComCtl32 <> 0) then
        LoadIconWithScaleDown := GPA (hComCtl32, 'LoadIconWithScaleDown');
{$ENDIF}

    hKernel32 := LoadLibrary (kernel32);

    if (hKernel32 <> 0) then
    begin
        CreateSymbolicLink := GPA (hKernel32, 'CreateSymbolicLink' + AWSuffix);
    	FindFirstFileNameW := GPA (hKernel32, 'FindFirstFileNameW');
    	FindNextFileNameW := GPA (hKernel32, 'FindNextFileNameW');
        GetFinalPathNameByHandle := GPA (hKernel32,
                                         'GetFinalPathNameByHandle' + AWSuffix);
{$IFNDEF FPC}
        GetTickCount64 := GPA (hKernel32, 'GetTickCount64');
{$ENDIF}
        LCIDToLocaleName := GPA (hKernel32, 'LCIDToLocaleName');
        LocaleNameToLCID := GPA (hKernel32, 'LocaleNameToLCID');
        QueryFullProcessImageName := GPA (hKernel32,
        								'QueryFullProcessImageName' + AWSuffix);
    end; { if }

    hNtDll := LoadLibrary (cNtDll);

    if (hNtDll <> 0) then
    begin
        RtlIpv6AddressToStringA := GPA (hNtDll, 'RtlIpv6AddressToStringA');
        RtlIpv6AddressToStringW := GPA (hNtDll, 'RtlIpv6AddressToStringW');

{$IFDEF UNICODE}
        RtlIpv6AddressToString := RtlIpv6AddressToStringW;
{$ELSE}
        RtlIpv6AddressToString := RtlIpv6AddressToStringA;
{$ENDIF}
    end; { if }

{$IFNDEF DELPHI2010_UP}
    hShell32 := LoadLibrary (cShell32);

    if (hShell32 <> 0) then
    	SHGetKnownFolderPath := GPA (hShell32, 'SHGetKnownFolderPath');
{$ENDIF}

    hUser32 := LoadLibrary (user32);

    if (hUser32 <> 0) then
    begin
        ChangeWindowMessageFilter := GPA (hUser32, 'ChangeWindowMessageFilter');

        RegisterPowerSettingNotification := GPA (hUser32,
                                            'RegisterPowerSettingNotification');

{$IFNDEF DELPHI2010_UP}
        SetProcessDPIAware := GPA (hUser32, 'SetProcessDPIAware');
{$ENDIF}

        ShutdownBlockReasonCreate := GPA (hUser32, 'ShutdownBlockReasonCreate');

        ShutdownBlockReasonQuery := GPA (hUser32, 'ShutdownBlockReasonQuery');

        ShutdownBlockReasonDestroy := GPA (hUser32, 'ShutdownBlockReasonDestroy');

        UnregisterPowerSettingNotification := GPA (hUser32,
                                          'UnregisterPowerSettingNotification');
    end; { if }

finalization
	if (hAdvApi32 <> 0) then
    	VerifyApi (FreeLibrary (hAdvApi32));

	if (hComCtl32 <> 0) then
    	VerifyApi (FreeLibrary (hComCtl32));

	if (hKernel32 <> 0) then
		VerifyApi (FreeLibrary (hKernel32));

    if (hNtDll <> 0) then
    	VerifyApi (FreeLibrary (hNtDll));

	if (hShell32 <> 0) then
		VerifyApi (FreeLibrary (hShell32));

    if (hUser32 <> 0) then
        VerifyApi (FreeLibrary (hUser32));
end.
