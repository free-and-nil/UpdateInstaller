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

unit Win2000_ImportU;

{$MINENUMSIZE 4}

interface

uses Windows,
     WinNT_ImportU;

const
    //
    // Extended Name APIs for ADS
    //
    // Examples for the following formats assume a fictitous company
    // which hooks into the global X.500 and DNS name spaces as follows.
    //
    // Enterprise root domain in DNS is
    //
    //      widget.com
    //
    // Enterprise root domain in X.500 (RFC 1779 format) is
    //
    //      O=Widget, C=US
    //
    // There exists the child domain
    //
    //      engineering.widget.com
    //
    // equivalent to
    //
    //      OU=Engineering, O=Widget, C=US
    //
    // There exists a container within the Engineering domain
    //
    //      OU=Software, OU=Engineering, O=Widget, C=US
    //
    // There exists the user
    //
    //      CN=John Doe, OU=Software, OU=Engineering, O=Widget, C=US
    //
    // And this user's downlevel (pre-ADS) user name is
    //
    //      Engineering\JohnDoe

    // unknown name type
    NameUnknown = 0;

    // CN=John Doe, OU=Software, OU=Engineering, O=Widget, C=US
    NameFullyQualifiedDN = 1;

    // Engineering\JohnDoe
    NameSamCompatible = 2;

    // Probably "John Doe" but could be something else.  I.e. The
    // display name is not necessarily the defining RDN.
    NameDisplay = 3;

    // String-ized GUID as returned by IIDFromString().
    // eg: {4fa050f0-f561-11cf-bdd9-00aa003a77b6}
    NameUniqueId = 6;

    // engineering.widget.com/software/John Doe
    NameCanonical = 7;

    // johndoe@engineering.com
    NameUserPrincipal = 8;

    // Same as NameCanonical except that rightmost '/' is
    // replaced with '\n' - even in domain-only case.
    // eg: engineering.widget.com/software\nJohn Doe
    NameCanonicalEx = 9;

    // www/srv.engineering.com/engineering.com
    NameServicePrincipal = 10;

    { Used by "CreateProcessWithLogonW" }
	LOGON_WITH_PROFILE = 1;
	LOGON_NETCREDENTIALS_ONLY = 2;

	// LWA constants for SetLayeredWindowAttributes
	lwa_ColorKey        = 1;
	lwa_Alpha           = 2;

	// New extended window style for layering
	ws_Ex_Layered       = $80000;

    DS_FORCE_REDISCOVERY = $00000001;

    DS_DIRECTORY_SERVICE_REQUIRED  = $00000010;
    DS_DIRECTORY_SERVICE_PREFERRED = $00000020;
    DS_GC_SERVER_REQUIRED          = $00000040;
    DS_PDC_REQUIRED                = $00000080;
    DS_BACKGROUND_ONLY             = $00000100;
    DS_IP_REQUIRED                 = $00000200;
    DS_KDC_REQUIRED                = $00000400;
    DS_TIMESERV_REQUIRED           = $00000800;
    DS_WRITABLE_REQUIRED           = $00001000;
    DS_GOOD_TIMESERV_PREFERRED     = $00002000;
    DS_AVOID_SELF                  = $00004000;
    DS_ONLY_LDAP_NEEDED            = $00008000;

    DS_IS_FLAT_NAME = $00010000;
    DS_IS_DNS_NAME  = $00020000;

    DS_TRY_NEXTCLOSEST_SITE = $00040000;

    DS_RETURN_DNS_NAME  = $40000000;
    DS_RETURN_FLAT_NAME = DWord($80000000);

    DSGETDC_VALID_FLAGS =
      DS_FORCE_REDISCOVERY or
      DS_DIRECTORY_SERVICE_REQUIRED or
      DS_DIRECTORY_SERVICE_PREFERRED or
      DS_GC_SERVER_REQUIRED or
      DS_PDC_REQUIRED or
      DS_BACKGROUND_ONLY or
      DS_IP_REQUIRED or
      DS_KDC_REQUIRED or
	  DS_TIMESERV_REQUIRED or
      DS_WRITABLE_REQUIRED or
      DS_GOOD_TIMESERV_PREFERRED or
      DS_AVOID_SELF or
      DS_ONLY_LDAP_NEEDED or
      DS_IS_FLAT_NAME or
      DS_IS_DNS_NAME or
      DS_RETURN_FLAT_NAME or
      DS_RETURN_DNS_NAME;

{$IFNDEF DELPHI_XE2_UP}
    // Values used by "GetVersionEx" with a parameter of type "OsVersionInfoEx"
    VER_SERVER_NT                      = DWord($80000000);
    VER_WORKSTATION_NT                 = $40000000;
    VER_SUITE_SMALLBUSINESS            = $00000001;
    VER_SUITE_ENTERPRISE               = $00000002;
    VER_SUITE_BACKOFFICE               = $00000004;
    VER_SUITE_COMMUNICATIONS           = $00000008;
    VER_SUITE_TERMINAL                 = $00000010;
    VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
    VER_SUITE_EMBEDDEDNT               = $00000040;
    VER_SUITE_DATACENTER               = $00000080;
    VER_SUITE_SINGLEUSERTS             = $00000100;
    VER_SUITE_PERSONAL                 = $00000200;
    VER_SUITE_BLADE                    = $00000400;
    VER_SUITE_EMBEDDED_RESTRICTED      = $00000800;
    VER_SUITE_SECURITY_APPLIANCE       = $00001000;
	VER_SUITE_COMPUTE_SERVER		   = $00004000;

    VER_NT_WORKSTATION                 = $0000001;
    VER_NT_DOMAIN_CONTROLLER           = $0000002;
    VER_NT_SERVER                      = $0000003;
{$ENDIF}

    // Return values from "IsNetworkAlive"
    NETWORK_ALIVE_LAN  = $00000001;
    NETWORK_ALIVE_WAN  = $00000002;
    NETWORK_ALIVE_AOL  = $00000004;

{$IFNDEF DELPHI_XE3_UP}
    { "IsProcessorFeaturePresent" parameter ProcessorFeature  }
    PF_FLOATING_POINT_PRECISION_ERRATA  = 0;
    PF_FLOATING_POINT_EMULATED          = 1;
    PF_COMPARE_EXCHANGE_DOUBLE          = 2;
    PF_MMX_INSTRUCTIONS_AVAILABLE       = 3;
    PF_PPC_MOVEMEM_64BIT_OK             = 4;
    PF_ALPHA_BYTE_INSTRUCTIONS          = 5;
    PF_XMMI_INSTRUCTIONS_AVAILABLE      = 6;
    PF_3DNOW_INSTRUCTIONS_AVAILABLE     = 7;
    PF_RDTSC_INSTRUCTION_AVAILABLE      = 8;
    PF_PAE_ENABLED                      = 9;
    PF_XMMI64_INSTRUCTIONS_AVAILABLE    = 10;  // Windows 2000:  This feature is not supported
    PF_SSE_DAZ_MODE_AVAILABLE           = 11;
    PF_NX_ENABLED                       = 12;  // Windows XP/2000:  This feature is not supported until Windows XP with SP2 and Windows Server 2003 with SP1
    PF_SSE3_INSTRUCTIONS_AVAILABLE      = 13;  // Windows Server 2003 and Windows XP/2000:  This feature is not supported
    PF_COMPARE_EXCHANGE128              = 14;  // Windows Server 2003 and Windows XP/2000:  This feature is not supported
    PF_COMPARE64_EXCHANGE128            = 15;  // Windows Server 2003 and Windows XP/2000:  This feature is not supported
    PF_CHANNELS_ENABLED                 = 16;
{$ENDIF}

{$IFNDEF DELPHI_XE3_UP}
    PF_XSAVE_ENABLED                    = 17;  // Windows Server 2008, Windows Vista, Windows Server 2003, and Windows XP/2000:  This feature is not supported until Windows 7 and Windows Server 2008 R2.
    PF_ARM_VFP_32_REGISTERS_AVAILABLE   = 18;
    PF_SECOND_LEVEL_ADDRESS_TRANSLATION = 20;
    PF_VIRT_FIRMWARE_ENABLED            = 21;
    PF_RDWRFSGSBASE_AVAILABLE           = 22;
    PF_FASTFAIL_AVAILABLE               = 23;
    PF_ARM_DIVIDE_INSTRUCTION_AVAILABLE = 24;
    PF_ARM_64BIT_LOADSTORE_ATOMIC       = 25;
    PF_ARM_EXTERNAL_CACHE_AVAILABLE     = 26;
    PF_ARM_FMAC_INSTRUCTIONS_AVAILABLE  = 27;
{$ENDIF}

//
// Info levels for ChangeServiceConfig2 and QueryServiceConfig2
//

  SERVICE_CONFIG_DESCRIPTION     = 1;
  SERVICE_CONFIG_FAILURE_ACTIONS = 2;
  SERVICE_CONFIG_DELAYED_AUTO_START_INFO = 3; //VISTA
  SERVICE_CONFIG_FAILURE_ACTIONS_FLAG = 4;
  SERVICE_CONFIG_SERVICE_SID_INFO = 5;
  SERVICE_CONFIG_REQUIRED_PRIVILEGES_INFO = 6;
  SERVICE_CONFIG_PRESHUTDOWN_INFO = 7;
  SERVICE_CONFIG_TRIGGER_INFO = 8;
  SERVICE_CONFIG_PREFERRED_NODE = 9;

{$IFNDEF DELPHI_XE2_UP}
  // Constants used by "SetFilePointer" / "SetFilePointerEx"
  FILE_BEGIN = 0;
  FILE_CURRENT = 1;
  FILE_END = 2;
{$ENDIF}

type
	NET_API_STATUS = DWord;

  _SERVICE_DELAYED_AUTO_START_INFO = record
    {WARNING:
    The C struct uses a BOOL which is in fact an Integer.
    You can't use true and false here without casting to Integer first:
      fDelayedAutostart := Integer(true);
    Using Boolean or BOOL does not work since the record must be 4 bytes in size,
    otherwise random data behind the first byte is also interpreted by the functions.

    BOOL defines true as -1 and not 1. ChangeServiceConfig2 uses this record
    and refuses to work if -1 is supplied.

    See also BOOL declaration in JwaWinType.pas for more information.

    CW@2008
    }
    fDelayedAutostart : Integer;
  end;
  SERVICE_DELAYED_AUTO_START_INFO = _SERVICE_DELAYED_AUTO_START_INFO;
  TServiceDelayedAutoStartInfo = _SERVICE_DELAYED_AUTO_START_INFO;
  PServiceDelayedAutoStartInfo = ^TServiceDelayedAutoStartInfo;

{$IFNDEF DELPHI2007_UP}
    uLongLong = Int64;
    Long_Ptr = UInt;
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
	uShort = Word;
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
    LPByte = PByte;
	DWordLong = uLongLong;
{$ENDIF}

const
//
// CryptProtectData and CryptUnprotectData dwFlags
//
// for remote-access situations where ui is not an option
// if UI was specified on protect or unprotect operation, the call
// will fail and GetLastError() will indicate ERROR_PASSWORD_RESTRICTION
    CRYPTPROTECT_UI_FORBIDDEN = $1;

//
// per machine protected data -- any user on machine where CryptProtectData
// took place may CryptUnprotectData
    CRYPTPROTECT_LOCAL_MACHINE = $4;

//
// force credential synchronize during CryptProtectData()
// Synchronize is only operation that occurs during this operation
    CRYPTPROTECT_CRED_SYNC = $8;

type
    _CRYPTOAPI_BLOB = record
      cbData: DWord;
      pbData: PBYTE;
    end; { _CRYPTOAPI_BLOB }
    PTData_Blob = ^_CRYPTOAPI_BLOB;
    TData_Blob = _CRYPTOAPI_BLOB;

    PCRYPTPROTECT_PROMPTSTRUCT = ^CRYPTPROTECT_PROMPTSTRUCT;
    _CRYPTPROTECT_PROMPTSTRUCT = record
      cbSize: DWord;
      dwPromptFlags: DWord;
      hwndApp: HWND;
      szPrompt: LPCWSTR;
    end; { _CRYPTPROTECT_PROMPTSTRUCT }
    CRYPTPROTECT_PROMPTSTRUCT = _CRYPTPROTECT_PROMPTSTRUCT;
    TCryptProtectPromptStruct = CRYPTPROTECT_PROMPTSTRUCT;
    PCryptProtectPromptStruct = PCRYPTPROTECT_PROMPTSTRUCT;

const
    // The controllable aspects of the DefineDosDevice function.
    // This parameter can be one or more of the following values.
    DDD_RAW_TARGET_PATH       = $00000001;
    DDD_REMOVE_DEFINITION     = $00000002;
    DDD_EXACT_MATCH_ON_REMOVE = $00000004;
    DDD_NO_BROADCAST_SYSTEM   = $00000008;
    DDD_LUID_BROADCAST_DRIVE  = $00000010;

type
    PDOMAIN_CONTROLLER_INFOA = ^DOMAIN_CONTROLLER_INFOA;
    _DOMAIN_CONTROLLER_INFOA = record
      DomainControllerName: LPSTR;
      DomainControllerAddress: LPSTR;
      DomainControllerAddressType: ULONG;
      DomainGuid: TGUID;
      DomainName: LPSTR;
      DnsForestName: LPSTR;
      Flags: ULONG;
      DcSiteName: LPSTR;
      ClientSiteName: LPSTR;
    end;
    DOMAIN_CONTROLLER_INFOA = _DOMAIN_CONTROLLER_INFOA;
    TDomainControllerInfoA = DOMAIN_CONTROLLER_INFOA;
    PDomainControllerInfoA = PDOMAIN_CONTROLLER_INFOA;

    PDOMAIN_CONTROLLER_INFOW = ^DOMAIN_CONTROLLER_INFOW;
    _DOMAIN_CONTROLLER_INFOW = record
      DomainControllerName: LPWSTR;
      DomainControllerAddress: LPWSTR;
      DomainControllerAddressType: ULONG;
      DomainGuid: TGUID;
      DomainName: LPWSTR;
      DnsForestName: LPWSTR;
      Flags: ULONG;
      DcSiteName: LPWSTR;
      ClientSiteName: LPWSTR;
    end;
    DOMAIN_CONTROLLER_INFOW = _DOMAIN_CONTROLLER_INFOW;
    TDomainControllerInfoW = DOMAIN_CONTROLLER_INFOW;
    PDomainControllerInfoW = PDOMAIN_CONTROLLER_INFOW;

{$IFDEF UNICODE}
    DOMAIN_CONTROLLER_INFO = DOMAIN_CONTROLLER_INFOW;
    PDOMAIN_CONTROLLER_INFO = PDOMAIN_CONTROLLER_INFOW;
    TDomainControllerInfo = TDomainControllerInfoW;
    PDomainControllerInfo = PDomainControllerInfoW;
{$ELSE}
    DOMAIN_CONTROLLER_INFO = DOMAIN_CONTROLLER_INFOA;
    PDOMAIN_CONTROLLER_INFO = PDOMAIN_CONTROLLER_INFOA;
    TDomainControllerInfo = TDomainControllerInfoA;
    PDomainControllerInfo = PDomainControllerInfoA;
{$ENDIF UNICODE}

{$IFNDEF DELPHI2010_UP}
    _COMPUTER_NAME_FORMAT = (ComputerNameNetBIOS,
      ComputerNameDnsHostname, ComputerNameDnsDomain,
      ComputerNameDnsFullyQualified, ComputerNamePhysicalNetBIOS,
      ComputerNamePhysicalDnsHostname, ComputerNamePhysicalDnsDomain,
      ComputerNamePhysicalDnsFullyQualified, ComputerNameMax);
   TComputerNameFormat = _COMPUTER_NAME_FORMAT;
   COMPUTER_NAME_FORMAT = _COMPUTER_NAME_FORMAT;
{$ENDIF}

{$IFNDEF DELPHI7_UP}
    PBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;
    _BY_HANDLE_FILE_INFORMATION = record
        dwFileAttributes: DWord;
        ftCreationTime: FileTime;
        ftLastAccessTime: FileTime;
        ftLastWriteTime: FileTime;
        dwVolumeSerialNumber: DWord;
        nFileSizeHigh: DWord;
        nFileSizeLow: DWord;
        nNumberOfLinks: DWord;
        nFileIndexHigh: DWord;
        nFileIndexLow: DWord;
    end; { _BY_HANDLE_FILE_INFORMATION }

    BY_HANDLE_FILE_INFORMATION = _BY_HANDLE_FILE_INFORMATION;
    LPBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;
    TByHandleFileInformation = BY_HANDLE_FILE_INFORMATION;
    PByHandleFileInformation = PBY_HANDLE_FILE_INFORMATION;
{$ENDIF}

	EXTENDED_NAME_FORMAT = DWord;
	PEXTENDED_NAME_FORMAT = ^EXTENDED_NAME_FORMAT;
	TExtendedNameFormat = EXTENDED_NAME_FORMAT;
	PExtendedNameFormat = PEXTENDED_NAME_FORMAT;

{$IFNDEF Delphi2009_UP}
    LPMEMORYSTATUSEX = ^MEMORYSTATUSEX;
    _MEMORYSTATUSEX = record
        dwLength: DWord;
        dwMemoryLoad: DWord;
        ullTotalPhys: DWORDLONG;
        ullAvailPhys: DWORDLONG;
        ullTotalPageFile: DWORDLONG;
        ullAvailPageFile: DWORDLONG;
        ullTotalVirtual: DWORDLONG;
        ullAvailVirtual: DWORDLONG;
        ullAvailExtendedVirtual: DWORDLONG;
    end;
    MEMORYSTATUSEX = _MEMORYSTATUSEX;
    TMemoryStatusEx = MEMORYSTATUSEX;
    PMemoryStatusEx = LPMEMORYSTATUSEX;
{$ENDIF}

    //
    // Status of a workstation
    //
    _NETSETUP_JOIN_STATUS = (
      NetSetupUnknownStatus,
      NetSetupUnjoined,
      NetSetupWorkgroupName,
      NetSetupDomainName);
    NETSETUP_JOIN_STATUS = _NETSETUP_JOIN_STATUS;
    PNETSETUP_JOIN_STATUS = ^NETSETUP_JOIN_STATUS;
    TNetSetupJoinStatus = NETSETUP_JOIN_STATUS;
    PNetSetupJoinStatus = PNETSETUP_JOIN_STATUS;

const
    ES_SYSTEM_REQUIRED  =  DWord ($00000001);
    ES_DISPLAY_REQUIRED =  DWord ($00000002);
    ES_USER_PRESENT     =  DWord ($00000004);
    ES_CONTINUOUS       =  DWord ($80000000);
    ES_AWAYMODE_REQUIRED = DWord ($00000040);

type
    EXECUTION_STATE = DWord;

const
	// Values for function "VerSetConditionMask", parameter "dwConditionMask"
    VER_EQUAL = 1;
    VER_GREATER = 2;
    VER_GREATER_EQUAL = 3;
    VER_LESS = 4;
    VER_LESS_EQUAL = 5;
    VER_AND = 6;
    VER_OR = 7;

{$IFNDEF DELPHI2010_UP}
	// Values for function "VerSetConditionMask", parameter "dwTypeBitMask"
    VER_BUILDNUMBER = $00000004;
    VER_MAJORVERSION = $00000002;
    VER_MINORVERSION = $00000001;
    VER_PLATFORMID = $00000008;
    VER_SERVICEPACKMAJOR = $00000020;
    VER_SERVICEPACKMINOR = $00000010;
    VER_SUITENAME = $00000040;
    VER_PRODUCT_TYPE = $00000080;
{$ENDIF}

{$IFNDEF DELPHI_XE2_UP}
	TOKEN_ADJUST_SESSIONID = $0100;
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
type
    LPOverlapped_Completion_Routine = procedure (dwErrorCode: DWord;
      									    dwNumberOfBytesTransfered: DWord;
                                            lpOverlapped: POverlapped); stdcall;
    TThreadStartRoutine = function (lpThreadParameter: Pointer) : DWord; stdcall;
{$ENDIF}

type
	UILANGUAGE_ENUMPROC = function (lpUILanguageString: LPTSTR;
    								lParam: LONG_PTR) : BOOL; stdcall;

procedure VER_SET_CONDITION (var Mask: DWORDLONG;
							 TypeBitmask, ConditionMask: ULONG);

var
{$IFNDEF DELPHI2009_UP}
	// Keine API-Funktion; wird von "BindIoCompletionCallback" genutzt
    OverlappedCompletionRoutine : LPOverlapped_Completion_Routine = NIL;

	BindIoCompletionCallback : function (FileHandle: THandle;
    							   pfnCallback: LPOverlapped_Completion_Routine;
                                   Flags: ULONG) : BOOL; stdcall = NIL;
{$ENDIF}

    BlockInput : function (fBlockIt: Bool) : Bool; stdcall = NIL;

    CheckTokenMembership : function (TokenHandle: THandle; SidToCheck: PSID;
    								 var IsMember: BOOL): BOOL; stdcall = NIL;

	ConvertSidToStringSid : function (Sid: PSID;
									  var StringSid: LPTSTR) : BOOL;
                                      							  stdcall = NIL;

    ConvertStringSidToSid :  function (StringSid: LPTSTR;
    								   var Sid: PSID) : BOOL; stdcall = NIL;

	CreateProcessWithLogonW : function (
		lpUsername : LPCWSTR; lpDomain : LPCWSTR; lpPassword : LPCWSTR;
		dwLogonFlags : DWord; lpApplicationName : LPCWSTR;
		lpCommandLine : LPWSTR; dwCreationFlags : DWord;
		lpEnvironment : POINTER; lpCurrentDirectory : LPCWSTR;
		var lpStartupInfo : TStartupInfo;
		var lpProcessInfo : TProcessInformation) : Bool; stdcall = NIL;

    CryptProtectData : function (pDataIn: PTData_Blob; szDataDescr: LPCWSTR;
                                 pOptionalEntropy: PTData_Blob;
                                 pvReserved: Pointer;
                                 pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                                 dwFlags: DWord; pDataOut: PTData_Blob) : BOOL;
                                                                  stdcall = NIL;

    CryptUnprotectData : function (pDataIn: PTData_Blob;
                                   var pszDataDescr: LPWSTR;
                                   pOptionalEntropy: PTData_Blob;
                                   pvReserved: Pointer;
                                   pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                                   dwFlags: DWord;
                                   pDataOut: PTData_Blob) : BOOL; stdcall = NIL;

    DefineDosDevice : function (dwFlags: DWord;
                                lpDeviceName, lpTargetPath: LPTSTR) : Bool;
                                                                  stdcall = NIL;
    DeleteVolumeMountPoint : function (lpszVolumeMountPoint: LPTSTR) : Bool;
                                                                  stdcall = NIL;

    DsGetDcName : function (ComputerName, DomainName: LPTSTR; DomainGuid: PGUID;
    				 SiteName: LPTSTR; Flags: ULONG;
                     var DomainControllerInfo: PDOMAIN_CONTROLLER_INFO) : DWord;
                     											  stdcall = NIL;

    DsGetSiteName : function (ComputerName: LPTSTR;
    						  var SiteName: LPTSTR) : DWord; stdcall = NIL;

    EnumUILanguages : function (lpUILanguageEnumProc: UILANGUAGE_ENUMPROC;
  								dwFlags: DWord; lParam: LONG_PTR) : BOOL;
                                								  stdcall = NIL;

    // FlushFileBuffers : function (hFile: THandle) : Bool; stdcall = NIL;
{$IFNDEF DELPHI2010_UP}
    GetComputerNameEx : function (NameType: TComputerNameFormat;
    							  lpBuffer: LPTSTR; var nSize: DWord) : BOOL;
                                   								  stdcall = NIL;
{$ENDIF}


	GetConsoleWindow : function : HWnd; stdcall = NIL;

{$IFNDEF DELPHI2010_UP}
    GetDefaultPrinter : function (pszBuffer: LPTSTR;
    							  pcchBuffer: LPDWORD) : BOOL; stdcall = NIL;
{$ENDIF}

{$IFNDEF DELPHI7_UP}
    GetFileInformationByHandle : function (hFile: THandle;
    		  		  var lpFileInformation: BY_HANDLE_FILE_INFORMATION) : BOOL;
              													  stdcall = NIL;
{$ENDIF}

    GetFileSizeEx : function (hFile: THandle;
    						  var iFileSize: TLargeInteger) : Bool;
                              									  stdcall = NIL;

    GetLongPathName : function (ShortPathName, LongPathName: PChar;
    							cchBuffer: Integer) : Integer; stdcall = NIL;

	GetUserNameEx : function (NameFormat: EXTENDED_NAME_FORMAT;
							  lpNameBuffer: LPTSTR;
							  var nSize: ULONG) : ByteBool; stdcall = NIL;

    GetVolumeNameForVolumeMountPoint : function (lpszVolumeMountPoint: LPTSTR;
                                                 lpszVolumeName: LPTSTR;
                                                 cchBufferLength: DWord) : Bool;
                                                                  stdcall = NIL;

    GlobalMemoryStatusEx : function (
							var lpBuffer: MEMORYSTATUSEX) : BOOL; stdcall = NIL;

    // "InetIsOffline" funktioniert nicht zuverlässig
    IsNetworkAlive : function (out lpdwFlags: DWord) : BOOL; stdcall = NIL;

	IsUserAnAdmin : function () : Bool; stdcall = NIL;

	LookupAccountSid : function (lpSystemName: LPTSTR; Sid: PSID;
    							  Name: LPTSTR; var cchName: DWord;
                                  ReferencedDomainName: LPTSTR;
                                  var cchReferencedDomainName: DWord;
                                  var peUse: SID_NAME_USE) : BOOL; stdcall = NIL;

  	NetGetJoinInformation : function (lpServer: LPCWSTR;
    						var lpNameBuffer: LPWSTR;
                            BufferType: PNETSETUP_JOIN_STATUS) : NET_API_STATUS;
                            									  stdcall = NIL;

    ProcessIdToSessionId : function (dwProcessId: DWord;
    								 var pSessionId: DWord) : BOOL;
                                     							  stdcall = NIL;

    QueryServiceConfig2 : function (hService: THandle; dwInfoLevel: DWORD;
                                    lpBuffer: LPBYTE; cbBufSize: DWORD;
                                    var pcbBytesNeeded: DWORD): BOOL;
                                                                  stdcall = NIL;

{$IFNDEF DELPHI2009_UP}
	QueueUserWorkItem : function (func: TThreadStartRoutine; Context: Pointer;
    							  Flags: ULong): Bool; stdcall = NIL;
{$ENDIF}

	SetFilePointerEx : function (hFile: THandle;
    							 liDistanceToMove: TLargeInteger;
                                 lpNewFilePointer: PHandle;
  								 dwMoveMethod: DWord) : Bool; stdcall = NIL;

	SetLayeredWindowAttributes : function (Wnd: hWnd; crKey: ColorRef;
										   bAlpha: Byte;
										   dwFlags: DWord) : Bool; stdcall = NIL;

    // http://delphidabbler.com/tips/127
    SetThreadExecutionState : function (
                     esFlags: EXECUTION_STATE) : EXECUTION_STATE; stdcall = NIL;

    SetVolumeMountPoint : function (
                 		  lpszVolumeMountPoint, lpszVolumeName: LPTSTR) : Bool;
                 												  stdcall = NIL;

	SHSetFolderPath : function (csidl: Integer; hToken: THandle;
								dwFlags: DWord; pszPath: LPTSTR) : HRESULT;
																  stdcall = NIL;

    SwitchToThisWindow : procedure (hWnd: HWND; fAltTab: BOOL); stdcall = NIL;

	TranslateName : function (lpAccountName: LPTSTR;
					 AccountNameFormat, DesiredNameFormat: EXTENDED_NAME_FORMAT;
					 lpTranslatedName: LPTSTR; var nSize: ULONG) : ByteBool;
																  stdcall = NIL;

    VerifyVersionInfo : function (var lpVersionInformation: TOSVersionInfoEx;
  								  dwTypeMask: DWord;
                                  dwlConditionMask: DWORDLONG) : BOOL;
                                  								  stdcall = NIL;

    VerSetConditionMask : function (ConditionMask: ULONGLONG; TypeMask: DWord;
    								Condition: BYTE) : ULONGLONG; stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU;

const
	cAdvApi32 = Windows.advapi32;
    cCrypt32 = 'crypt32.dll';
    cNetApi32 = 'netapi32.dll';
	cSecur32  = 'secur32.dll';
    cSensapi  = 'sensapi.dll';
	cShell32  = 'shell32.dll';
{$IFNDEF FPC}
  {$IFNDEF DELPHI2010_UP}
    cWinSpool = 'winspool.drv';
  {$ENDIF}
{$ENDIF}

{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}

{$IFDEF UNICODE}
    cIsUserAnAdmin = MakeIntResourceA (680);
    cSHSetFolderPath = MakeIntResourceA (232);
{$ELSE}
    cIsUserAnAdmin = MakeIntResource (680);
    cSHSetFolderPath = MakeIntResource (231);
{$ENDIF}

var
	hAdvApi32 : THandle = 0;
    hCrypt32  : THandle = 0;
	hKernel32 : THandle = 0;
    hNetApi32 : THandle = 0;
	hSecur32  : THandle = 0;
    hSensapi  : THandle = 0;
	hShell32  : THandle = 0;
	hUser32   : THandle = 0;
{$IFNDEF DELPHI2010_UP}
    hWinSpool : THandle = 0;
{$ENDIF}

(* ---- *)

procedure VER_SET_CONDITION (var Mask: DWORDLONG;
							 TypeBitmask, ConditionMask: ULONG);
begin
	Mask := VerSetConditionMask (Mask, TypeBitmask, ConditionMask);
end; { VER_SET_CONDITION }

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
	if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Win32MajorVersion < 5) then
        exit;

    hAdvApi32 := LoadLibrary (cAdvApi32);

    if (hAdvApi32 <> 0) then
    begin
    	CheckTokenMembership := GPA (hAdvAPI32, 'CheckTokenMembership');

        ConvertSidToStringSid := GPA (hAdvAPI32,
                                            'ConvertSidToStringSid' + AWSuffix);

        ConvertStringSidToSid := GPA (hAdvApi32,
        									'ConvertStringSidToSid' + AWSuffix);

        CreateProcessWithLogonW := GPA (hAdvApi32, 'CreateProcessWithLogonW');

    	LookupAccountSid := GPA (hAdvApi32, 'LookupAccountSid' + AWSuffix);
        QueryServiceConfig2 := GPA (hAdvApi32, 'QueryServiceConfig2' + AWSuffix);
    end; { if }

    hCrypt32 := LoadLibrary (cCrypt32);

    if (hCrypt32 <> 0) then
    begin
        CryptProtectData := GPA (hCrypt32, 'CryptProtectData');
        CryptUnprotectData := GPA (hCrypt32, 'CryptUnprotectData');
    end; { if }

    hKernel32 := LoadLibrary (kernel32);

    if (hKernel32 <> 0) then
    begin
{$IFNDEF DELPHI2009_UP}
    	BindIoCompletionCallback := GPA (hKernel32, 'BindIoCompletionCallback');
{$ENDIF}
        DefineDosDevice := GPA (hKernel32, 'DefineDosDevice' + AWSuffix);
        DeleteVolumeMountPoint := GPA (hKernel32,
                                           'DeleteVolumeMountPoint' + AWSuffix);
        // FlushFileBuffers := GPA (hKernel32, 'FlushFileBuffers');
{$IFNDEF DELPHI2010_UP}
        GetComputerNameEx := GPA (hKernel32, 'GetComputerNameEx' + AWSuffix);
{$ENDIF}

		GetConsoleWindow := GPA (hKernel32, 'GetConsoleWindow');

{$IFNDEF DELPHI7_UP}
        GetFileInformationByHandle := GPA (hKernel32,
                                                  'GetFileInformationByHandle');
{$ENDIF}
        GetFileSizeEx := GPA (hKernel32, 'GetFileSizeEx');
        GetLongPathName := GPA (hKernel32, 'GetLongPathName' + AWSuffix);
        GetVolumeNameForVolumeMountPoint := GPA (hKernel32,
                                 'GetVolumeNameForVolumeMountPoint' + AWSuffix);
        GlobalMemoryStatusEx := GPA (hKernel32, 'GlobalMemoryStatusEx');
        ProcessIdToSessionId := GPA (hKernel32, 'ProcessIdToSessionId');
{$IFNDEF DELPHI2009_UP}
        QueueUserWorkItem := GPA (hKernel32, 'QueueUserWorkItem');
{$ENDIF}
		SetFilePointerEx := GPA (hKernel32, 'SetFilePointerEx');
        SetThreadExecutionState := GPA (hKernel32, 'SetThreadExecutionState');
        SetVolumeMountPoint := GPA (hKernel32, 'SetVolumeMountPoint' + AWSuffix);
        EnumUILanguages := GPA (hKernel32, 'EnumUILanguages' + AWSuffix);
        VerifyVersionInfo := GPA (hKernel32, 'VerifyVersionInfo' + AWSuffix);
        VerSetConditionMask := GPA (hKernel32, 'VerSetConditionMask')
    end; { if }

    hNetApi32 := LoadLibrary (cNetApi32);

    if (hNetApi32 <> 0) then
    begin
        DsGetDcName := GPA (hNetApi32, 'DsGetDcName' + AWSuffix);
        DsGetSiteName := GPA (hNetApi32, 'DsGetSiteName' + AWSuffix);
        NetGetJoinInformation := GPA (hNetApi32, 'NetGetJoinInformation');
    end; { if }

    hSecur32 := LoadLibrary (cSecur32);

    if (hSecur32 <> 0) then
    begin
        GetUserNameEx := GPA (hSecur32, 'GetUserNameEx' + AWSuffix);
        TranslateName := GPA (hSecur32, 'TranslateName' + AWSuffix);
    end; { if }

    hSensapi := LoadLibrary (cSensapi);

    if (hSensapi <> 0) then
        IsNetworkAlive := GPA (hSensapi, 'IsNetworkAlive');

    hShell32 := LoadLibrary (cShell32);

    if (hShell32 <> 0) then
    begin
        IsUserAnAdmin := GPA (hShell32, cIsUserAnAdmin);
        SHSetFolderPath := GPA (hShell32, cSHSetFolderPath);
    end; { if }

    hUser32 := LoadLibrary (user32);

    if (hUser32 <> 0) then
    begin
        BlockInput := GPA (hUser32, 'BlockInput');
        SetLayeredWindowAttributes := GPA (hUser32,
        										  'SetLayeredWindowAttributes');
        SwitchToThisWindow := GPA (hUser32, 'SwitchToThisWindow')
    end; { if }

{$IFNDEF DELPHI2010_UP}
    hWinSpool := LoadLibrary ('Winspool.drv');

    if (hWinSpool <> 0) then
        GetDefaultPrinter := GPA (hWinSpool, 'GetDefaultPrinter' + AWSuffix);
{$ENDIF}
end; { initialization }

(* ---- *)

finalization
begin
	if (hAdvApi32 <> 0) then
		VerifyApi (FreeLibrary (hAdvApi32));

    if (hCrypt32 <> 0) then
        VerifyApi (FreeLibrary (hCrypt32));

	if (hKernel32 <> 0) then
		VerifyApi (FreeLibrary (hKernel32));

    if (hNetApi32 <> 0) then
    	VerifyApi (FreeLibrary (hNetApi32));

	if (hSecur32 <> 0) then
		VerifyApi (FreeLibrary (hSecur32));

	if (hSensapi <> 0) then
		VerifyApi (FreeLibrary (hSensapi));

	if (hShell32 <> 0) then
		VerifyApi (FreeLibrary (hShell32));

    if (hUser32 <> 0) then
    	VerifyApi (FreeLibrary (hUser32));

{$IFNDEF DELPHI2010_UP}
    if (hWinSpool <> 0) then
    	VerifyApi (FreeLibrary (hWinSpool));
{$ENDIF}
end; { finalization }

(* ---- *)

end.

