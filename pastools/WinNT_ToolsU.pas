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
{$ELSE}
  {$IFDEF DELPHI7_UP}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}

unit WinNT_ToolsU;

{$MINENUMSIZE 4}

interface

uses
    Windows,
{$IFNDEF NO_VCL}
	Classes,
{$ENDIF}
{$IFDEF JEDI}
    JwaLmCons, JwaLmServer,
{$ELSE}
    Lmcons,
{$ENDIF}
    BaseTypesU;

const
	// "NetLocalGroupAddMembers" return values
    { One or more of the members specified do not exist.
      Therefore, no new members were added. }
    ERROR_MEMBER_IN_ALIAS = 1378;
    { One or more of the members specified were already members of the local
      group. No new members were added. }
	ERROR_NO_SUCH_MEMBER  = 1387;
    { One or more of the members cannot be added because their account type is
      invalid. No new members were added. }
    ERROR_INVALID_MEMBER  = 1388;

    cAdministrators_SID = 'S-1-5-32-544';

{$IFNDEF FPC}
    SE_CREATE_TOKEN_NAME                      = 'SeCreateTokenPrivilege';
    SE_ASSIGNPRIMARYTOKEN_NAME                = 'SeAssignPrimaryTokenPrivilege';
    SE_LOCK_MEMORY_NAME                       = 'SeLockMemoryPrivilege';
    SE_INCREASE_QUOTA_NAME                    = 'SeIncreaseQuotaPrivilege';
    SE_UNSOLICITED_INPUT_NAME                 = 'SeUnsolicitedInputPrivilege';
    SE_MACHINE_ACCOUNT_NAME                   = 'SeMachineAccountPrivilege';
    SE_TCB_NAME                               = 'SeTcbPrivilege';
    SE_SECURITY_NAME                          = 'SeSecurityPrivilege';
    SE_TAKE_OWNERSHIP_NAME                    = 'SeTakeOwnershipPrivilege';
    SE_LOAD_DRIVER_NAME                       = 'SeLoadDriverPrivilege';
    SE_SYSTEM_PROFILE_NAME                    = 'SeSystemProfilePrivilege';
    SE_SYSTEMTIME_NAME                        = 'SeSystemtimePrivilege';
    SE_PROF_SINGLE_PROCESS_NAME               = 'SeProfileSingleProcessPrivilege';
    SE_INC_BASE_PRIORITY_NAME                 = 'SeIncreaseBasePriorityPrivilege';
    SE_CREATE_PAGEFILE_NAME                   = 'SeCreatePagefilePrivilege';
    SE_CREATE_PERMANENT_NAME                  = 'SeCreatePermanentPrivilege';
    SE_BACKUP_NAME                            = 'SeBackupPrivilege';
    SE_RESTORE_NAME                           = 'SeRestorePrivilege';
    SE_SHUTDOWN_NAME                          = 'SeShutdownPrivilege';
    SE_DEBUG_NAME                             = 'SeDebugPrivilege';
    SE_AUDIT_NAME                             = 'SeAuditPrivilege';
    SE_SYSTEM_ENVIRONMENT_NAME                = 'SeSystemEnvironmentPrivilege';
    SE_CHANGE_NOTIFY_NAME                     = 'SeChangeNotifyPrivilege';
    SE_REMOTE_SHUTDOWN_NAME                   = 'SeRemoteShutdownPrivilege';
    SE_UNDOCK_NAME                            = 'SeUndockPrivilege';
    SE_SYNC_AGENT_NAME                        = 'SeSyncAgentPrivilege';
    SE_ENABLE_DELEGATION_NAME                 = 'SeEnableDelegationPrivilege';
    SE_MANAGE_VOLUME_NAME                     = 'SeManageVolumePrivilege';
    SE_IMPERSONATE_NAME                       = 'SeImpersonatePrivilege';
    SE_CREATE_GLOBAL_NAME                     = 'SeCreateGlobalPrivilege';
    SE_TRUSTED_CREDMAN_ACCESS_NAME            = 'SeTrustedCredManAccessPrivilege';
    SE_RELABEL_NAME                           = 'SeRelabelPrivilege';
    SE_INC_WORKING_SET_NAME                   = 'SeIncreaseWorkingSetPrivilege';
    SE_TIME_ZONE_NAME                         = 'SeTimeZonePrivilege';
    SE_CREATE_SYMBOLIC_LINK_NAME              = 'SeCreateSymbolicLinkPrivilege';
    SE_DELEGATE_SESSION_USER_IMPERSONATE_NAME = 'SeDelegateSessionUserImpersonatePrivilege';
{$ENDIF}

type
	TDebugPrivilege = class
      private
      	FEnabled : Boolean;
      	TP, TP_Prev : TTokenPrivileges;
    	hProcessToken : THandle;

      public
      	constructor Create;
        destructor Destroy; override;

        property Enabled : Boolean read FEnabled;
    end; { TDebugPrivilege }

    TBuiltinGroup = (bigAdmins, bigGuests, bigUsers, bigPowerUsers,
    				 bigRemoteDesktopUsers, bigNetworkConfigurationOperators);

function AdjustPrivileges (const Privilege: String; const Enabled: Boolean;
						   const bRaiseException: Boolean = true) : Boolean;

procedure CheckCredentials (const sUserName, sDomain, sPassword: String);

procedure CommandLineToArray (out asParams: TaString);

function DomainGroupGetUsers (const sGroup: WideString;
							  {$IFDEF NO_VCL}
                              out asUsers: TaString;
                              {$ELSE}
                              const UserList: TStrings;
                              {$ENDIF}
                              sLogonServer: WideString) : Boolean;
{ "sLogonServer" muss definiert sein. Unter NT4 muss der Name mit "\\"
  beginnen. Der Gruppenname darf den Domain-Namen nicht enthalten! Die
  Benutzernamen werden ohne vorgestellten Domänen-Namen zurückgegeben. }

procedure EnableDebugPrivilege;

function GetBuiltinGroupName (const BuiltinGroup: TBuiltinGroup) : String;

function GetComputerDomainName : String;
{ Liefert den Namen der Domain zurück, zu der der PC gehört. Muss nicht der
   Domain entsprechen, an der der Anwender angemeldet ist. Liefert den Namen
   der Workgroup zurück, wenn der PC zu keiner Domain gehört. }

function GetComputerSID : String;

function GetDomainAndUserName : String; overload;

function GetDomainAndUserName (out sDomainName, sUserName: String) : Boolean;
	overload;

procedure GetLoggedOnUsers (
							{$IFDEF NO_VCL}
						    out asUsers: TaString;
							{$ELSE}
						    const UserList: TStrings;
							{$ENDIF}
						    const sServerName: WideString = '');

function GetLogonServerName : String;

function GetMachineName (const bIncludeDomainName: Boolean = false) : String;
{ If the computer is not member of a domain and "bIncludeDomainName = true" the
  name of the workgroup is returned }

(**
function GetMachineSID (sComputerName: String) : String;
**)

function GetNetworkManagementError (const NetApiStatus: NET_API_STATUS) : String;

(**
function GetParamsArray : TaString;

function GetParamsStr (const bIncludeExePath: Boolean = false) : String;
**)

function GetProcessHandleFromId (const dwProcessId: DWord;
                                 out hProcess: THandle) : Boolean;

function GetProcessPrivileges (
                        	   {$IFDEF NO_VCL}
                               out asPrivileges: TaString
                               {$ELSE}
                               const PrivilegeList: TStrings
                               {$ENDIF}
                        						 		    ) : Boolean;

function GetProcessStartTime_DDT (hProcess: THandle
{$IFDEF SUPPORTS_OVERLOAD}
                                                    = 0
{$ENDIF}
                                                       ) : LongInt;
function GetProcessStartTime (hProcess: THandle
{$IFDEF SUPPORTS_OVERLOAD}
                                                = 0
{$ENDIF}
                                                   ) : TDateTime;

function GetProcessUserName (hProcess: THandle;
							 out sUserName: String) : Boolean;
{$IFDEF SUPPORTS_OVERLOAD}
                             								   overload;

function GetProcessUserName : String; overload;
{$ENDIF}

function GetShareNames (
                        {$IFDEF NO_VCL}
                        out asNames: TaString;
                        {$ELSE}
                        const ShareList: TStrings;
                        {$ENDIF}
                        const bIncludePath: Boolean = false;
                        const sLogonServer: WideString = '') : Boolean;

function GetUserLogonDomainName : String;
{ Liefert den Namen der Domain zurück, an der der Anwender angemeldet ist }

function GetUserSID (const sUserName: String;
					 out sSID: String) : Boolean; overload;

function GetUserSID (const sUserName: String;
					 out pToSID: PSID) : Boolean; overload;
{ "pToSID" muss mit "MemDispose" freigeben werden! }

function IsDomainController : Boolean;

function IsLocalAdministratorAccount : Boolean;

function IsPcInDomain : Boolean;

function IsPcInWorkgroup : Boolean;

function IsTokenInGroup (const hAccessToken: THandle;
						 const pGroup_SID: pSID;
                         const bGroupMustBeEnabled: Boolean = false) : Boolean;

function IsUserInGroup (const sGroup: String;
                        const bGroupMustBeEnabled: Boolean = false) : Boolean;
                        											   overload;

function IsUserInGroup (const pGroup_SID: pSID;
                        const bGroupMustBeEnabled: Boolean = false) : Boolean;
                        											   overload;

procedure LocalGroupAddUser (const sGroup: WideString; const pUserSID: PSID;
							 const sLogonServer: WideString = ''); overload;
{ "sUser" kann auch der Name einer Gruppe sein }

function LocalGroupAddUser (const sGroup: WideString; const pUserSID: PSID;
						    out NetApiStatus: NET_API_STATUS;
                            const sLogonServer: WideString = '') : Boolean;
{ "sUser" kann auch der Name einer Gruppe sein }					   overload;

procedure LocalGroupAddUser (const sGroup, sUser: WideString;
							 const sLogonServer: WideString = ''); overload;
{ "sUser" kann auch der Name einer Gruppe sein }

function LocalGroupAddUser (const sGroup, sUser: WideString;
							out NetApiStatus: NET_API_STATUS;
							const sLogonServer: WideString = '') : Boolean;
{ "sUser" kann auch der Name einer Gruppe sein } 					   overload;

function LocalGroupGetUsers (const sGroup: WideString;
							 {$IFDEF NO_VCL}
							 out asUsers: TaString;
							 {$ELSE}
							 const UserList: TStrings;
							 {$ENDIF}
							 const sLogonServer: WideString = '') : Boolean;
{ Für lokale Gruppen. Ist "UserList = TStrings", dann enthält das "Objects"-
  Feld eines Eintrag den Wert "SidTypeUser" für Benutzerkonten und
  "SidTypeGroup" für Gruppen. }

function LocalGroupRemoveUser (const sGroup, sUser: WideString;
							   const sLogonServer: WideString = '') : Boolean;

function ProcessStartedByCurrentUser (const dwProcessId: DWord) : Boolean;
{ "Debug"-Privileg muss für Prozess gesetzt sein! }

function RegWriteUnicodeStr (const HRootKey: HKey; const sKey: String;
							 const bCreateKey: Boolean;
							 const sValueName, sValue: WideString) : Boolean;

function SIDToString (const pToSID: PSID) : String;

procedure Subst (const chDrive: Char; const sPath: String);

function StrArrayAdd (var asUsers: TaString; const sStr: String) : Boolean;

function SystemShutdown (const sComputerName: String = '';
						 const bRestart: Boolean = false;
                         const uTimeOut: UInt = 30;
                         const bForceAppsClosed: Boolean = false;
                         const sShutdownMsg: String = '') : Boolean;

procedure SystemShutdownAbort (const sComputerName: String = '';
                               const bExceptionOnError: Boolean = false);

function UserAccountExists (const sUserName: WideString;
							const sDC: WideString = '') : Boolean;

function UserAccountIsEnabled (const sUserName: WideString;
                               const sDC: WideString = '') : Boolean;

implementation

uses SysUtils,
{$IFDEF JEDI}
	 JwaLmAccess, JwaLmWkSta, JwaLmApiBuf, Jwalmerr, JwaLmShare,
{$ELSE}
	 LMWkSta, LMErr, LMApiBuf, LMAccess, LMShare, Svrapi, LMServer,
{$ENDIF}
     { FunctionCallErrorStringsU, } Win32ToolsU, PasTools, VerifyU, LsaApiU,
     WinNT_ImportU;

type
	ENetworkManagementError = class (Exception);

{$IFDEF FPC}
function AdjustTokenPrivileges (TokenHandle: THandle; DisableAllPrivileges: BOOL;
                                var NewState: TTokenPrivileges;
                                BufferLength: DWORD;
                                pPreviousState: PTokenPrivileges;
                                pReturnLength: PDWORD) : BOOL;
                                 external advapi32 name 'AdjustTokenPrivileges';
{$ENDIF}

ResourceString
	cUnknowError ='Unknown Network Management API error.';
	cGroupNotFound = 'The local group specified by the groupname parameter does not exist.';
    cAccessDenied = 'The user does not have access to the requested information.';
    cNoSuchMember = 'One or more of the members specified do not exist. Therefore, no new members were added.';
    cMemberInAlias = 'One or more of the members specified were already members of the local group. No new members were added.';
    cInvalidMember = 'One or more of the members cannot be added because their account type is invalid. No new members were added.';

(* ---- *)

constructor TDebugPrivilege.Create;

var
    dwReturnLength : DWord;

begin
	inherited;

    if (OpenProcessToken (GetCurrentProcess,
                          TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
                          hProcessToken)) then
    begin
        TP.PrivilegeCount := 1;

        if (LookupPrivilegeValue (NIL, SE_DEBUG_NAME,
        						  TP.Privileges [0].LUID)) then
        begin
            TP.Privileges [0].Attributes := SE_PRIVILEGE_ENABLED;
            dwReturnLength := 0;

            if (AdjustTokenPrivileges (hProcessToken, false, TP,
            						   SizeOf (TTokenPrivileges),
                                       {$IFDEF FPC}@{$ENDIF}TP_Prev,
                                       {$IFDEF FPC}@{$ENDIF}dwReturnLength)) then
                FEnabled := true;
        end; { if }
    end; { if }
end; { TDebugPrivilege.Create }

(* ---- *)

destructor TDebugPrivilege.Destroy;

var
    dwReturnLength : DWord;

begin
	if (FEnabled) then
    begin
        dwReturnLength := 0;
    	VerifyApi (AdjustTokenPrivileges (hProcessToken, true, TP_Prev, 0, NIL,
                                          {$IFDEF FPC}@{$ENDIF}dwReturnLength));
    end; { if }

    if (hProcessToken <> 0) then
    	VerifyApi (CloseHandle (hProcessToken));

	inherited;
end; { TDebugPrivilege.Destroy }

(* ---- *)

procedure WkstaUserGetInfo (out pWkstaUserInfo: PWkstaUserInfo1);

var
    dwLevel : DWord;

begin
	pWkstaUserInfo := NIL;
	dwLevel := 1;

    Win32Check (NetWkstaUserGetInfo (NIL, dwLevel,
    								 LPBYTE (pWkstaUserInfo)) = NERR_Success);
end; { WkstaUserGetInfo }

(* ---- *)

function AdjustPrivileges (const Privilege: String; const Enabled: Boolean;
						   const bRaiseException: Boolean = true) : Boolean;

var
    hToken : THandle;
    TokenPriv, PrefTokenPriv : TOKEN_PRIVILEGES;
    dwLastError, dwReturn : DWord;

begin
	Result := false;
    dwLastError := 0;

    if (OpenProcessToken (GetCurrentProcess,
    					  TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
                          hToken{%H-})) then
        try
            FillChar (TokenPriv{%H-}, SizeOf (TOKEN_PRIVILEGES), #0);

            if (LookupPrivilegeValue (NIL, PChar (Privilege),
                                      TokenPriv.Privileges [0].Luid)) then
            begin
                TokenPriv.PrivilegeCount := 1; // one privilege to set

                if (Enabled) then
                    TokenPriv.Privileges [0].Attributes  := SE_PRIVILEGE_ENABLED
                else TokenPriv.Privileges [0].Attributes := 0;

                FillChar (PrefTokenPriv{%H-}, SizeOf (TOKEN_PRIVILEGES), #0);
                dwReturn := 0;

                if (AdjustTokenPrivileges (hToken, false, TokenPriv,
                						   SizeOf (TOKEN_PRIVILEGES),
                                           {$IFDEF FPC}@{$ENDIF}PrefTokenPriv,
                                           {$IFDEF FPC}@{$ENDIF}dwReturn)) then
                	if (GetLastError = ERROR_SUCCESS) then
                    	Result := true;
            end; { if }

            if (Result = false) and (dwLastError = 0) then
                dwLastError := GetLastError;

        finally
            VerifyApi (CloseHandle (hToken));
        end { try / finally }
    else dwLastError := GetLastError;

    if (Result = false) and (bRaiseException) then
    begin
      	SetLastError (dwLastError);
        RaiseLastWin32Error;
    end; { if }
end; { AdjustPrivileges }

(* ---- *)

procedure CheckCredentials (const sUserName, sDomain, sPassword: String);
{ Wird der Benutzernamen als "user@domain" angegeben, so muss "sDomain" leer
  sein. Ist "sDomain" = ".", so erfolgt die Authorisierung gegen den lokalen
  PC. Erzeugt Exception bei Fehler. }

var
	pszDomain : PChar;
    hToken : THandle;

begin
	Assert (sUserName <> '');

	if (sDomain <> '') then
    	pszDomain := PChar (sDomain)
    else pszDomain := NIL;

    Win32Check (LogonUser (PChar (sUserName), pszDomain, PChar (sPassword),
    			LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT,
                hToken{%H-}));

    VerifyApi (CloseHandle (hToken));{%H-}
end; { CheckCredentials }

(* ---- *)

procedure CommandLineToArray (out asParams: TaString);

type
    PTaPWideChar = ^TaPWideChar;
	TaPWideChar = array [0..0] of PWideChar;

var
	iIndex, iNumArgs : Integer;
    pCmdLineToArgv : PTaPWideChar;

begin
    pCmdLineToArgv := PTaPWideChar (CommandLineToArgvW (GetCommandLineW,
    													iNumArgs{%H-}));

    Win32Check (pCmdLineToArgv <> NIL);

    try
    	Assert (iNumArgs > 0);
{$IFDEF DEBUG}
    {$RANGECHECKS OFF}
{$ENDIF}

        SetLength (asParams{%H-}, iNumArgs);

        for iIndex := 0 to iNumArgs - 1 do
            asParams [iIndex] := pCmdLineToArgv^[iIndex];

{$IFDEF DEBUG}
    {$RANGECHECKS ON}
{$ENDIF}

    finally
    	VerifyApi (LocalFree ({%H-}HLOCAL (pCmdLineToArgv)) = 0);
    end; { try / finally }
end; { CommandLineToArray }{%H-}

(* ---- *)

function DomainGroupGetUsers (const sGroup: WideString;
							  {$IFDEF NO_VCL}
                              out asUsers: TaString;
                              {$ELSE}
                              const UserList: TStrings;
                              {$ENDIF}
                              sLogonServer: WideString) : Boolean;
{ "sLogonServer" muss definiert sein. Unter NT4 muss der Name mit "\\"
  beginnen. Der Gruppenname darf den Domain-Namen nicht enthalten! Die
  Benutzernamen werden ohne vorgestellten Domänen-Namen zurückgegeben. }

type
	TaUserGroup = array of TGroupUsersInfo0;

const
	PREF_LEN = 1024;

var
	pBuffer : LPBYTE;
    i : Integer;
	Res : NET_API_STATUS;
    dwRead, dwTotal : DWord;
    hRes : DWord;

begin
	Assert (sGroup <> '');
    Assert (sLogonServer <> '');
{$IFNDEF NO_VCL}
	Assert (UserList <> NIL);
	UserList.Clear;
{$ELSE}
    SetLength (asUsers, 0);
{$ENDIF}

	Result := true;

	hRes := 0;

	repeat
		Res := NetGroupGetUsers (PWideChar (sLogonServer), PWideChar (sGroup),
        						 0, pBuffer{%H-}, PREF_LEN,
{$IFDEF JEDI}
                              {%H-}   @dwRead, @dwTotal,
{$ELSE}
                                 dwRead{%H-}, dwTotal{%H-},
{$ENDIF}
                                 PDWord (@hRes));{%H-}

		if (Res = Error_Success) or (Res = ERROR_MORE_DATA) then
		begin
			if (dwRead > 0) then
				for i := 0 to dwRead - 1 do
					with TaUserGroup (pBuffer) [i] do
{$IFDEF NO_VCL}
						StrArrayAdd (asUsers, LowerCase (grui0_name));
{$ELSE}
						UserList.Add (grui0_name);
{$ENDIF}

			VerifyApi (NetApiBufferFree (pBuffer) = NErr_Success);
		end { if }
		else Result := false;
	until (Res <> ERROR_MORE_DATA);
end; { DomainGroupGetUsers }

(* ---- *)

procedure EnableDebugPrivilege;
begin
    AdjustPrivileges (SE_DEBUG_NAME, true);
end; { EnableDebugPrivilege }

(* ---- *)

function GetBuiltinGroupName (const BuiltinGroup: TBuiltinGroup) : String;

const
(**
	Wert entspricht der letzten Stelle der Well-Knows-SID:
    "Well-known security identifiers in Windows operating systems"
	http://support.microsoft.com/en-us/kb/243330
**)

//  DOMAIN_ALIAS_RID_ADMINS = $00000220;
    DOMAIN_ALIAS_RID_GUESTS = $00000222;
    DOMAIN_ALIAS_RID_USERS = $00000221;
//  DOMAIN_ALIAS_RID_PowerUsers = $00000223;
    DOMAIN_ALIAS_RID_RemoteDesktopUsers = $0000022B;
//  DOMAIN_ALIAS_RID_NetworkConfigurationOperators = $0000022C;
//    SECURITY_WORLD_RID = $00000000;
    SECURITY_BUILTIN_DOMAIN_RID = $00000020;
    SECURITY_NT_AUTHORITY : TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

var
    dwSubAuthority1 : DWord;
    dwName, dwDomainName : DWORD;
    SID_ID_Auth : SID_IDENTIFIER_AUTHORITY;
    sDomainName : String;
    pSID : Windows.PSID;
    sidType : SID_NAME_USE;

begin
	pSID := NIL;

    case BuiltinGroup of
        bigAdmins : dwSubAuthority1 := DOMAIN_ALIAS_RID_ADMINS;
        bigGuests : dwSubAuthority1 := DOMAIN_ALIAS_RID_GUESTS;
        bigUsers : dwSubAuthority1 := DOMAIN_ALIAS_RID_USERS;
        bigPowerUsers : dwSubAuthority1 := DOMAIN_ALIAS_RID_POWER_USERS;
        bigRemoteDesktopUsers :
        				 dwSubAuthority1 := DOMAIN_ALIAS_RID_RemoteDesktopUsers;
        bigNetworkConfigurationOperators :
        		  dwSubAuthority1 := DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS;
        else dwSubAuthority1 := 0;
    end; { case BuiltinGroup of }

    dwName := MAX_PATH;
    SetLength (Result{%H-}, dwName);
    dwDomainName := MAX_PATH;
    SetLength (sDomainName{%H-}, dwDomainName);

    SID_ID_Auth := SECURITY_NT_AUTHORITY;

    SID_ID_Auth.Value [5] := 5 ;

    Win32Check (AllocateAndInitializeSid (SID_ID_Auth, 2,
    									  SECURITY_BUILTIN_DOMAIN_RID,
    									  dwSubAuthority1, 0, 0, 0, 0, 0, 0,
                                          pSID));

    if (LookupAccountSid (NIL, pSID, PChar (Result), dwName,
    					  PChar (sDomainName), dwDomainName, sidType{%H-})) then
    begin
    	FreeSid (pSID);{%H-}
        SetLength (Result, dwName);
    end { if }
    else Result := '';
end; { GetBuiltinGroupName }

(* ---- *)

function GetComputerDomainName : String;
{ Liefert den Namen der Domain zurück, zu der der PC gehört. Muss nicht der
   Domain entsprechen, an der der Anwender angemeldet ist. Liefert den Namen
   der Workgroup zurück, wenn der PC zu keiner Domain gehört. }

var
    pWkstaInfo : PWkstaInfo100;
    dwLevel : DWord;

begin
	pWkstaInfo := NIL;

    dwLevel := 100;

    Win32Check (NetWkstaGetInfo (NIL, dwLevel,
    							 LPBYTE (pWkstaInfo)) = NERR_Success);

    Result := pWkstaInfo^.wki100_langroup;

    VerifyApi (NetApiBufferFree (pWkstaInfo) = NErr_Success);
end; { GetComputerDomainName }

(* ---- *)

function GetComputerSID : String;
// http://stackoverflow.com/questions/7641792/how-to-extract-computer-machine-sid

var
    Sid : PSID;
    cbSid : DWORD;
    cbReferencedDomainName : DWORD;
    ReferencedDomainName : String;
    peUse : SID_NAME_USE;
    bSuccess : BOOL;
    lpSystemName : String;
    lpAccountName : String;

begin
    Sid := NIL;

    try
        lpSystemName:='';
        lpAccountName := GetMachineName;

        cbSid := 0;
        cbReferencedDomainName := 0;

        // First call to LookupAccountName to get the buffer sizes.
        bSuccess := LookupAccountName (PChar (lpSystemName),
        							   PChar (lpAccountName), NIL, cbSid, NIL,
                                       cbReferencedDomainName, peUse{%H-});

        if not (bSuccess) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
        begin
            SetLength (ReferencedDomainName{%H-}, cbReferencedDomainName);
            Sid := AllocMem (cbSid);

            // Second call to LookupAccountName to get the SID.
            if (LookupAccountName (PChar (lpSystemName), PChar (lpAccountName),
            					   Sid, cbSid, PChar (ReferencedDomainName),
                                   cbReferencedDomainName, peUse)) then
            	Result := SIDToString (Sid)
            else RaiseLastOSError;
        end { if }
        else RaiseLastOSError;

    finally
    	if (Assigned (Sid)) then
       		FreeMem (Sid);
    end; { try / finally }
end; { GetComputerSID }

(* ---- *)

function GetDomainAndUserName : String;
{ Returns the username of the currently logged on user in the format
  "domainname\username"  }

var
	sDomainName, sUserName : String;

begin
	GetDomainAndUserName (sDomainName, sUserName);
	Result := String (sDomainName + '\' + sUserName);
end; { GetDomainAndUserName }

(* ---- *)

function GetDomainAndUserName (out sDomainName, sUserName: String) : Boolean;

var
	pWkstaUserInfo : PWkstaUserInfo1;

begin
	Result := true;

    try
    	WkstaUserGetInfo (pWkstaUserInfo);

        with pWkstaUserInfo^ do
        begin
        	sDomainName := String (wkui1_logon_domain);
            sUserName := String (wkui1_username);
        end; { with }

        VerifyApi (NetApiBufferFree (pWkstaUserInfo) = NErr_Success);

    except
    	Result := false;
    end; { try / except }
end; { GetDomainAndUserName }

(* ---- *)

procedure GetLoggedOnUsers (
{$IFDEF NO_VCL}
						    out asUsers: TaString;
{$ELSE}
						    const UserList: TStrings;
{$ENDIF}
						    const sServerName: WideString = '');

var
	pszServerName : PWideChar;
    dwStatus : NET_API_STATUS;
    pBuf : LPBYTE;
    pTmpBuf : PWkstaUserInfo1;
    dwEntriesRead, dwTotalEntries, dwResumeHandle, dwPrefMaxLen : DWord;
    iIndex : Integer;


begin
{$IFNDEF NO_VCL}
	Assert (UserList <> NIL);
    UserList.Clear;
{$ELSE}
    SetLength (asUsers, 0);
{$ENDIF}

    if (sServerName = '') then
    	pszServerName := NIL
    else pszServerName := PWideChar (sServerName);

    pBuf := NIL;
    dwPrefMaxLen := MAX_PREFERRED_LENGTH;
   	dwEntriesRead := 0;
   	dwTotalEntries := 0;
   	dwResumeHandle := 0;

    repeat
        dwStatus := NetWkstaUserEnum (pszServerName, 1, pBuf, dwPrefMaxLen,
{$IFDEF JEDI}
                                      @dwEntriesRead, @dwTotalEntries,
                                      @dwResumeHandle);
{$ELSE}
                                      dwEntriesRead, dwTotalEntries,
                                      dwResumeHandle);
{$ENDIF}

    	if (dwStatus = NERR_Success) or (dwStatus = ERROR_MORE_DATA) then
        begin
        	if (pBuf <> NIL) then
            begin
            	pTmpBuf := PWkstaUserInfo1 (pBuf);

                for iIndex := 0 to dwEntriesRead - 1 do
                begin
                    with pTmpBuf^ do
{$IFDEF NO_VCL}
                        StrArrayAdd (asUsers,
                        			 LowerCase (Format ('%s\%s',
                                    				    [wkui1_logon_domain,
                                                         wkui1_username])));
{$ELSE}
						UserList.Add (Format ('%s\%s', [wkui1_logon_domain,
                                                        wkui1_username]));
{$ENDIF}

                    Inc (pTmpBuf);
                end; { for }
            end; { if }
        end { if }
        else Win32Check (false);

        if (pBuf <> NIL) then
        begin
			VerifyApi (NetApiBufferFree (pBuf) = NErr_Success);
         	pBuf := NIL;
        end; { if }
    until (dwStatus <> ERROR_MORE_DATA);
end; { GetLoggedOnUsers }

(* ---- *)

function GetLogonServerName : String;

var
	pWkstaUserInfo : PWkstaUserInfo1;

begin
	Result := '';

    try
    	WkstaUserGetInfo (pWkstaUserInfo);
        Result := pWkstaUserInfo^.wkui1_logon_server;

    finally
        if (pWkstaUserInfo <> NIL) then
            VerifyApi (NetApiBufferFree (pWkstaUserInfo) = NErr_Success);
    end; { try / finally }
end; { GetLogonServerName }

(* ---- *)

function GetMachineName (const bIncludeDomainName: Boolean = false) : String;
{ If the computer is not member of a domain and "bIncludeDomainName = true" the
  name of the workgroup is returned }

var
    pWkstaInfo : PWkstaInfo100;

begin
	pWkstaInfo := NIL;

    Win32Check (NetWkstaGetInfo (NIL, 100,
                                 LPBYTE (pWkstaInfo)) = NERR_Success);

    if (bIncludeDomainName) then
    	Result := String (pWkstaInfo^.wki100_langroup) + '\' +
        		  String (pWkstaInfo^.wki100_computername)
	else Result := String (pWkstaInfo^.wki100_computername);

    VerifyApi (NetApiBufferFree (pWkstaInfo) = NErr_Success);
end; { GetMachineName }

(* ---- *)

(**
function GetMachineSID (sComputerName: String) : String;
{ Domain-SID <> local machine SID except for domain controllers. Domain name
  should in most cases be ommited! }

var
    pSID : Windows.PSID;
    dwSidLen, dwReferencedDomainName : DWord;
    peUse : SID_NAME_USE;
    iPos : Integer;
    sReferencedDomainName : String;

begin
	Assert (sComputerName <> '');

    Result := '';

	{ Ermitteln der Domain-SID unktioniert nur, wenn der PC Mitglied einer
      Domäne und mit dem Netzwerk verbunden ist, anderenfalls Fehlermeldung:
      "The trust relationship between this workstation and the primary domain
      failed". Domain-SID <> local SID; special case domain controllers! }
    if (Pos ('\', sComputerName) > 0) then
    	if (sComputerName [Length (sComputerName)] <> '$') then
			sComputerName := sComputerName + '$';  // "$" at the end necessary!

    dwReferencedDomainName := MAX_PATH;
    SetLength (sReferencedDomainName, dwReferencedDomainName);
    dwSidLen := 0;
    pSID := NIL;

    try
	    while (LookupAccountName (NIL, PChar (sComputerName), pSID, dwSidLen,
        						  PChar (sReferencedDomainName),
                                  dwReferencedDomainName, peUse) = false) do
            case GetLastError of
                ERROR_INSUFFICIENT_BUFFER : GetMem (pSID, dwSidLen);

                ERROR_NONE_MAPPED :
                    begin { Computer not in a domain -> remove domain.
                            Error msg: "No mapping between account names and
                            			security IDs was done". }
                        iPos := Pos ('\', sComputerName) + 1;
                    	Assert (iPos > 0);
                        sComputerName := Copy (sComputerName, iPos,
                                               Length (sComputerName) - iPos);
                    end; { case ERROR_NONE_MAPPED }

                else RaiseLastOSError;
            end; { case }

	    Result := SIDToString (pSID);

    finally
    	if (pSID <> NIL) then
	        FreeMem (pSID);
    end; { try / finally }
end; { GetMachineSID }
**)

(* ---- *)

function GetNetworkManagementError (const NetApiStatus: NET_API_STATUS) : String;
begin
    Result := '';

    case NetApiStatus of
        NERR_GroupNotFound : Result := cGroupNotFound;
        ERROR_ACCESS_DENIED : Result := cAccessDenied;
        ERROR_NO_SUCH_MEMBER : Result := cNoSuchMember;
        ERROR_MEMBER_IN_ALIAS : Result := cMemberInAlias;
        ERROR_INVALID_MEMBER : Result := cInvalidMember;
    	else Result := cUnknowError;
    end; { case }
end; { GetNetworkManagementError }

(* ---- *)

(**
function GetParamsArray : TaString;

type
    PTsArglist = ^TsArglist;
    TsArglist = array [0..0] of PWideString;

var
    iArgs, iIndex : Integer;
    psArglist : PTsArglist;

begin
    psArglist := PTsArglist (CommandLineToArgvW (GetCommandLineW, iArgs));

    Win32Check (psArglist <> NIL);

    try
        SetLength (Result, iArgs);

{$IFDEF DEBUG}
    {$RANGECHECKS OFF}
{$ENDIF}
        for iIndex := 0 to iArgs - 1 do
            Result [iIndex] := Trim (WideString ((psArglist [iIndex])));
{$IFDEF DEBUG}
    {$RANGECHECKS ON}
{$ENDIF}

    finally
        VerifyApi (LocalFree (THandle (psArglist)) = 0);
    end; { try / finally }
end; { GetParamsArray }
**)

(* ---- *)

(**
function GetParamsStr (const bIncludeExePath: Boolean = false) : String;

var
    iIndex, iCount, iOffset : Integer;
    aParams : TaString;

begin
    aParams := GetParamsArray;

    iCount := High (aParams);

    if (iCount = 1) and (bIncludeExePath = false) then
        exit;

    iOffset := iif (bIncludeExePath, 0, 1);

    for iIndex := iOffset to iCount - 1 do
        if (Pos (' ', aParams [iIndex]) > 0) then
            Result := Result + '"' + aParams [iIndex] + '" '
        else Result := Result + aParams [iIndex] + ' ';

    SetLength (Result, Pred (Length (Result)));
end; { GetParamsStr }
**)

(* ---- *)

function GetProcessHandleFromId (const dwProcessId: DWord;
                                 out hProcess: THandle) : Boolean;
{ Der aktuelle Prozeß muss das Privileg "SeDebugPrivilege" haben, sonst kann
  der Parameter "PROCESS_QUERY_INFORMATION" nicht verwendet werden!
  Zurückgegebenes Handle muss mit "CloseHandle" geschlossen werden. }

begin
    Assert (dwProcessId <> 0);

    // Handle muss mit "CloseHandle" geschlossen werden!
    hProcess := OpenProcess (PROCESS_QUERY_INFORMATION, false, dwProcessId);

    Result := hProcess <> 0;
end; { GetProcessHandleFromId }

(* ---- *)

function GetProcessPrivileges (
                        	   {$IFDEF NO_VCL}
                               out asPrivileges: TaString
                               {$ELSE}
                               const PrivilegeList: TStrings
                               {$ENDIF}
                        						 		) : Boolean;

	(* ---- *)

    function GetPrivilegeName (Luid: TLargeInteger;
    						   out sName: String) : Boolean;
{$IFDEF SUPPORTS_INLINE}
		inline;
{$ENDIF}

	var
    	dwLen : DWord;

    begin
    	dwLen := MAX_PATH;
		SetLength (sName{%H-}, dwLen);

        if (LookupPrivilegeName (NIL, Luid, PChar (sName), dwLen)) then
        	SetLength (sName, dwLen);

        Result := sName <> '';
    end; { GetPrivilegeName }

    (* ---- *)

    function PrivilegeEnabled (const dwAttr: DWord) : Boolean;
{$IFDEF SUPPORTS_INLINE}
		inline;
{$ENDIF}
    begin
    	Result := ((dwAttr and SE_PRIVILEGE_ENABLED) <> 0) or
        		  ((dwAttr and SE_PRIVILEGE_ENABLED_BY_DEFAULT) <> 0)
    end; { PrivilegeEnabled }

    (* ---- *)

var
	hProcessToken : THandle;
    dwSize: DWord;
    pPrivileges : PTokenPrivileges;
    iIndex : Integer;
    sPrivilege : String;

begin { GetProcessPrivileges }
	Result := false;

    pPrivileges := NIL;

    if (OpenProcessToken (GetCurrentProcess, Token_Query, hProcessToken{%H-})) then
        try
            if (GetTokenInformation (hProcessToken, TokenPrivileges,
                                     pPrivileges, 0, dwSize{%H-}) = false) and
               (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
            begin
                GetMem (pPrivileges, dwSize);

                if (GetTokenInformation (hProcessToken, TokenPrivileges,
                						 pPrivileges, dwSize, dwSize)) then
                	for iIndex := 0 to pPrivileges^.PrivilegeCount - 1 do
{$IFDEF DEBUG}
	{$R-}
{$ENDIF}
                        with pPrivileges^.Privileges [iIndex] do
{$IFDEF DEBUG}
	{$R+}
{$ENDIF}
	                        if (PrivilegeEnabled (Attributes)) then
    	                    	if (GetPrivilegeName (Luid, sPrivilege)) then
	{$IFDEF NO_VCL}
    							StrArrayAdd (asPrivileges, sPrivilege);
	{$ELSE}
                               	PrivilegeList.Add (sPrivilege);
	{$ENDIF}
			end; { if }

        finally
            if (pPrivileges <> NIL) then
                FreeMem (pPrivileges);

            VerifyApi (CloseHandle (hProcessToken));
        end; { try / finally }
end; { GetProcessPrivileges }

(* ---- *)

function GetProcessStartTime_DDT (hProcess: THandle
{$IFDEF SUPPORTS_OVERLOAD}
                                                    = 0
{$ENDIF}
                                                       ) : LongInt;
{ "_DDT" = "DosDateTime" }

var
    ftCreationTime, ftExitTime, ftKernelTime, ftUserTime, ftLocalTime : TFileTime;

begin
    if (hProcess = 0) then
        hProcess := GetCurrentProcess;

    Win32Check (GetProcessTimes (hProcess, ftCreationTime{%H-}, ftExitTime{%H-},
                                ftKernelTime{%H-}, ftUserTime{%H-}));
{%H-}{%H-}
    Win32Check (FileTimeToLocalFileTime (ftCreationTime{%H-}, ftLocalTime{%H-}));

    Win32Check (FileTimeToDosDateTime (ftLocalTime, LongRec (Result).Hi,{%H-}
                                       LongRec ({%H-}Result{%H-}).Lo));
end; { GetProcessStartTime_DDT }
{%H-}
(* ----- *)

function GetProcessStartTime (hProcess: THandle
{$IFDEF SUPPORTS_OVERLOAD}
                                                = 0
{$ENDIF}
                                                   ) : TDateTime;
begin
    Result := FileDateToDateTime (GetProcessStartTime_DDT (hProcess));
end; { GetProcessStartTime }

(* ---- *)

function GetProcessUserName (hProcess: THandle;
							 out sUserName: String) : Boolean;
{ Ist "hProcess" nicht vom aufrufenden Prozeß, das Handle durch Aufruf von
  "GetProcessHandleFromId" ermitteln }

var
	hProcessToken : THandle;
    dwSize: DWord;
    pTokenUser : PTTokenUser;
    sName, sReferencedDomainName : String;
    dwName, dwReferencedDomainName : DWord;
    peUse : SID_NAME_USE;

begin
	Result := false;
    pTokenUser := NIL;

    if (hProcess = 0) then
		hProcess := GetCurrentProcess;

    if (OpenProcessToken (hProcess, Token_Query, hProcessToken{%H-})) then
        try
            if (GetTokenInformation (hProcessToken, TokenUser,
                                     pTokenUser, 0, dwSize{%H-}) = false) and
               (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
            begin
                GetMem (pTokenUser, dwSize);

                if (GetTokenInformation (hProcessToken, TokenUser, pTokenUser,
                						 dwSize, dwSize)) then
                begin
                    dwName := MAX_PATH;
                    dwReferencedDomainName := MAX_PATH;

                    SetLength (sName{%H-}, dwName);
                    SetLength (sReferencedDomainName{%H-},
                               dwReferencedDomainName);

                    if (LookupAccountSid (NIL, pTokenUser^.User.Sid,
                    					  PChar (sName), dwName,
                                          PChar (sReferencedDomainName),
                                          dwReferencedDomainName,
                                          peUse{%H-})) then
                    begin
                        SetLength (sName, dwName);
                        SetLength (sReferencedDomainName,
                        		   dwReferencedDomainName);

                        if (sReferencedDomainName <> '') then
                            sUserName := sReferencedDomainName + '\' + sName
                        else sUserName := sName;

						Result := true;
                    end; { if }
                end { if }
           end; { if }

        finally
            if (pTokenUser <> NIL) then
                FreeMem (pTokenUser);

            VerifyApi (CloseHandle (hProcessToken));
        end { try / finally }
    else RaiseLastOSError;
end; { GetProcessUserName }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function GetProcessUserName : String;
begin
    GetProcessUserName (0, Result);
end; { GetProcessUserName }
{$ENDIF}

(* ---- *)

function GetShareNames (
                        {$IFDEF NO_VCL}
                        out asNames: TaString;
                        {$ELSE}
                        const ShareList: TStrings;
                        {$ENDIF}
                        const bIncludePath: Boolean = false;
                        const sLogonServer: WideString = '') : Boolean;

	(* ---- *)

    procedure AddShareWin9x (const pShare: Pointer);

	var
    	psPath : PChar;

    begin
    	with PShareInfo50 (pShare)^ do
        begin
			if (bIncludePath) then
            begin
				psPath := StrAlloc (lStrLen (shi50_path));
                lstrcpy (psPath, PChar (shi50_path));
{$IFDEF NO_VCL}
				StrArrayAdd (asNames, shi50_netname);
{$ELSE}
                ShareList.AddObject (shi50_netname, Pointer (psPath));
{$ENDIF}
			end { if }
            else
{$IFDEF NO_VCL}
				 StrArrayAdd (asNames, shi50_netname);
{$ELSE}
				 ShareList.Add (shi50_netname);
{$ENDIF}
        end; { with }
    end; { AddShareWin9x }

	(* ---- *)

    procedure AddShareWinNt (const pShare: Pointer);

	var
    	sName : String;
{$IFNDEF NO_VCL}
		sPath : String;
    	psPath : PChar;
{$ENDIF}

    begin
    	with PShareInfo502 (pShare)^ do
        begin
        	sName := shi502_netname;

			if (bIncludePath) then
            begin
{$IFDEF NO_VCL}
				StrArrayAdd (asNames, sName);
{$ELSE}
				sPath := String (WideString (shi502_path));
				psPath := StrAlloc (lStrLenW (shi502_path));
                lstrcpy (psPath, PChar (sPath));

                ShareList.AddObject (sName, Pointer (psPath));
{$ENDIF}
			end { if }
            else
{$IFDEF NO_VCL}
				 StrArrayAdd (asNames, sName);
{$ELSE}
				 ShareList.Add (sName);
{$ENDIF}
        end; { with }
    end; { AddShareWinNt }

    (* ---- *)

var
    Status : NET_API_STATUS;
    pBuffer, pShare : Pointer;
    dwEntriesRead, dwTotalAvail, dwLevel, dwResumeHandle, dwRecSize : DWord;
    iIndex : Integer;

begin { GetShareNames }
	Result := false;

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
    	dwLevel := 502;
        dwRecSize := SizeOf (TShareInfo502);
    end { if }
    else
    begin
    	dwLevel := 50;
        dwRecSize := SizeOf (TShareInfo50);
    end; { else }

    dwResumeHandle := 0;

    repeat
    	Status := LMShare.NetShareEnum (PWideChar (sLogonServer), dwLevel,
        								pBuffer{%H-}, MAX_PREFERRED_LENGTH,
                                        dwEntriesRead{%H-}, dwTotalAvail{%H-},
                            {%H-}            PDWord (@dwResumeHandle));
{%H-}{%H-}
        if (Status = Error_Success) or (Status = Error_More_Data) then
        begin
        	Result := true;

        	pShare := pBuffer;

        	for iIndex := 1 to dwEntriesRead do
			begin
                if (Win32Platform = VER_PLATFORM_WIN32_NT) then
                    AddShareWinNt (pShare)
                else AddShareWin9x (pShare);

{$IFDEF WIN64}
				Inc ({%H-}NativeUInt (pShare), dwRecSize);
{$ELSE}
				Inc ({%H-}UInt (pShare), dwRecSize);
{$ENDIF}
            end; { for }

        	NetApiBufferFree (pBuffer);
        end; { if }
    until (Status <> Error_More_Data);
end; { GetShareNames }

(* ---- *)

function GetUserLogonDomainName : String;
{ Liefert den Namen der Domain zurück, an der der Anwender angemeldet ist. Muss
  nicht der Domain entsprechen, zu der der PC gehört. }

var
	pWkstaUserInfo : PWkstaUserInfo1;

begin
	Result := '';

    pWkstaUserInfo := NIL;

    try
    	WkstaUserGetInfo (pWkstaUserInfo);
		Result := pWkstaUserInfo^.wkui1_logon_domain;

    finally
        if (pWkstaUserInfo <> NIL) then
            VerifyApi (NetApiBufferFree (pWkstaUserInfo) = NErr_Success);
    end; { try / finally }
end; { GetUserLogonDomainName }

(* ---- *)

function GetUserSID (const sUserName: String; out sSID: String) : Boolean;
{ Liefert die SID eines Benutzers oder einer Gruppe zurück.
  ->> sUserName : Muss das Format "domain\username" haben!
  ->> sSID : Wird mit SID gefüllt.
  <<- True, wenn Erfolg }

var
	pToSID : PSID;

begin
	Assert (sUserName <> '');
    Assert (Pos ('\', sUserName) > 1);

    Result := false;

	if (GetUserSID (sUserName, pToSID)) then
    begin
        sSID := SIDtoString (pToSid);
        Result := sSID <> '';

		if (pToSID <> NIL) then
			MemDispose (pToSID);
    end; { if }
end; { GetUserSID }

(* ---- *)

function GetUserSID (const sUserName: String;
					 out pToSID: PSID) : Boolean;
{ Liefert die SID eines Benutzers oder einer Gruppe zurück.
  ->> sUserName : Muss das Format "domain\username" haben!
  ->> pToSID : Wird mit SID gefüllt. Muss mit "MemDispose" freigegeben werden!
  <<- True, wenn Erfolg }

var
    dwSID, dwDomain : DWord;
    SID_Use: SID_NAME_USE;
    sDomain : String;
    bLookupResult : Boolean;

begin
	Assert (sUserName <> '');
    Assert (Pos ('\', sUserName) > 1);

	Result := false;

    if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    	exit; { Requires Windows NT }

    dwSID := 0;
	dwDomain := 0;

    pToSID := NIL;
    sDomain := '';

    repeat
		bLookupResult := LookupAccountName (NIL, PChar (sUserName),
        									pToSID, dwSID, PChar (sDomain),
                                            dwDomain, SID_Use{%H-});

        if (bLookupResult = false) then{%H-}
            if (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
            begin
                if (pToSID <> NIL) then
                    MemDispose (pToSID);

                pToSID := MemAlloc (dwSID);

                SetLength (sDomain, dwDomain);
			end { if }
            else Break;
    until (bLookupResult = true);

	if (bLookupResult = true) then
		if (IsValidSid (pToSID)) then
			if (SID_Use in [SidTypeUser, SidTypeGroup]) then
            	Result := true;

    if not (Result) then
        MemDispose (pToSID);
end; { GetUserSID }

(* ---- *)

function IsDomainController : Boolean;

var
	nStatus : NET_API_STATUS;
{$IFDEF JEDI}
    pSI : LPBYTE;
{$ENDIF}
    pServerInfo : ^SERVER_INFO_101;

begin
	Result := false;

{$IFDEF JEDI}
	nStatus := NetServerGetInfo (NIL, 101, pSI);
    pServerInfo := Pointer (pSI);
{$ELSE}
	nStatus := NetServerGetInfo (NIL, 101, Pointer ({%H-}pServerInfo));
{$ENDIF}
{%H-}
    if (nStatus = NERR_Success) then
    begin
    	Result := (pServerInfo^.sv101_type and SV_TYPE_DOMAIN_CTRL <> 0) or
        		  (pServerInfo^.sv101_type and SV_TYPE_DOMAIN_BAKCTRL <> 0);
    	NetApiBufferFree (pServerInfo);
    end; { if }
end; { IsDomainController }

(* ---- *)

function IsLocalAdministratorAccount : Boolean;
{ How Can I Determine if the Local Administrator Account has been Renamed on
  a Computer?
  http://blogs.technet.com/b/heyscriptingguy/archive/2005/07/22/how-can-i-determine-if-the-local-administrator-account-has-been-renamed-on-a-computer.aspx
}

var
	bElevated : Boolean;
    sSID : String;
    iPos : Integer;

begin
	Result := false;

    if not (IsUserLocalAdmin (bElevated)) then
    	exit;

    // Check for local logon
	if (LowerCase (GetLogonServerName) <> LowerCase (GetMachineName)) then
    	exit;

    if not (GetUserSID (GetDomainAndUserName, sSID)) then
    	exit;

    iPos := LastPos ('-', sSID);

    if (iPos > 0) and (Pos ('S-1-5-', sSID) = 1) then
    begin
	    Delete (sSID, 1, iPos);

        if (sSID = '500') then
        	Result := true;
    end; { if }
end; { IsLocalAdministratorAccount }

(* ---- *)

function IsPcInDomain : Boolean;
{ Windows 2000 and up: use "NetGetJoinInformation" }

var
    ComputerName : TLSAUnicodeStr;
    Attributes : TLsaObjectAttributes;
    PolicyHandle : LSA_HANDLE;
    Status : NTStatus;
    pBuffer : Pointer;
    pPADI : PPolicyAccountDomainInfo;

begin
	Result := false;

    if not (Assigned (LsaOpenPolicy)) then
    	exit;  // Required NT4 Workstation SP3 or up

    ComputerName := TLsaUnicodeStr.CreateFromStr ('');

    try
        FillChar (Attributes{%H-}, SizeOf (TLsaObjectAttributes), 0);

        Status := LsaOpenPolicy (ComputerName.Value, Attributes,
                                 POLICY_VIEW_LOCAL_INFORMATION,
                                 PolicyHandle{%H-});

        if (Status = STATUS_SUCCESS) then
            try
                Status := LsaQueryInformationPolicy (PolicyHandle,
                                          		 PolicyPrimaryDomainInformation,
                                                 pBuffer{%H-});

                if (Status = STATUS_SUCCESS) then
                    try
                        pPADI := pBuffer;

                        if (pPADI.DomainSID <> NIL) then
	                        Result := true;

                    finally
                        VerifyApi (LsaFreeMemory (pBuffer) = STATUS_SUCCESS);
                    end; { try / finally }

            finally
                VerifyApi (LsaClose (PolicyHandle) = STATUS_SUCCESS);
            end; { try / finally }

    finally
    	ComputerName.Free;
    end; { try / finally }
end; { IsPcInDomain }

(* ---- *)

function IsPcInWorkgroup : Boolean;
begin
	Result := IsPcInDomain = false;
end; { IsPcInWorkgroup }

(* ---- *)

function IsTokenInGroup (const hAccessToken: THandle;
						 const pGroup_SID: pSID;
                         const bGroupMustBeEnabled: Boolean = false) : Boolean;

const
	SE_GROUP_ENABLED = $00000004;

var
	ptgGroups : PTokenGroups;
	iGroups : Integer;
	dwInfoBufferSize, dwBufSize, dwAttributes : DWord;

begin
    Assert (hAccessToken <> 0);
	Assert (IsValidSID (pGroup_SID));

	Result := false;

	dwBufSize := 0;
	ptgGroups := NIL;
	dwInfoBufferSize := 0;

    { Puffergröße ermitteln; Aufruf gibt "false" zurück }
    GetTokenInformation (hAccessToken, TokenGroups, ptgGroups, dwBufSize,
                         dwInfoBufferSize);

    if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
        RaiseLastOSError;

	dwBufSize := dwInfoBufferSize;
	GetMem (ptgGroups, dwBufSize);

	try
		Win32Check (GetTokenInformation (hAccessToken, TokenGroups, ptgGroups,
										 dwBufSize, dwInfoBufferSize));

{$IFDEF DEBUG}
	{$R-}
{$ENDIF}
		for iGroups := 0 to ptgGroups.GroupCount - 1 do
			if (EqualSid (pGroup_SID, ptgGroups.Groups [iGroups].Sid)) then
			begin
            	if (bGroupMustBeEnabled) then
                begin
                	dwAttributes := ptgGroups.Groups [iGroups].Attributes;

                	if ((dwAttributes and SE_GROUP_ENABLED) = 0) then
                		Continue;
                end; { if }

				Result := true;
				Break;
			end; { if }
{$IFDEF DEBUG}
	{$R+}
{$ENDIF}

	finally
		FreeMem (ptgGroups);
	end; { try / finally }
end; { IsTokenInGroup }

(* ---- *)

function IsUserInGroup (const sGroup: String;
                        const bGroupMustBeEnabled: Boolean = false) : Boolean;
{ Checks against the currently logged on user;
  "sGroup" has to have the format "domain\group name" }

var
	pGroup_SID : pSID;

begin
	if not (GetUserSID (sGroup, pGroup_SID)) then
		RaiseLastOSError;

	try
		Result := IsUserInGroup (pGroup_SID, bGroupMustBeEnabled);

    finally
    	MemDispose (pGroup_SID)
    end; { try / finally }
end; { IsUserInGroup }

(* ---- *)

function IsUserInGroup (const pGroup_SID: pSID;
                        const bGroupMustBeEnabled: Boolean = false) : Boolean;
{ Checks against the currently logged on user }

var
	hAccessToken : THandle;

begin
	Assert (IsValidSID (pGroup_SID));

	hAccessToken := 0;

	if not (OpenThreadToken (GetCurrentThread, TOKEN_QUERY, true,
							 hAccessToken)) then
		if (GetLastError <> ERROR_NO_TOKEN) then
			RaiseLastWin32Error
		else Win32Check (OpenProcessToken (GetCurrentProcess, TOKEN_QUERY,
										   hAccessToken));

    try
    	Result := IsTokenInGroup (hAccessToken, pGroup_SID, bGroupMustBeEnabled)

    finally
		VerifyApi (CloseHandle (hAccessToken));
    end; { try / finally }
end; { IsUserInGroup }

(* ---- *)

procedure LocalGroupAddUser (const sGroup: WideString; const pUserSID: PSID;
                             const sLogonServer: WideString = '');

var
	LocalGroupMembersInfo : TLocalGroupMembersInfo0;
    NetApiStatus : NET_API_STATUS;

begin
	Assert (sGroup <> '');
	Assert (pUserSID <> NIL);

	LocalGroupMembersInfo.lgrmi0_sid := pUserSID;

    NetApiStatus := NetLocalGroupAddMembers (PWideChar (sLogonServer),
    								   		 PWideChar (sGroup), 0,
                                       		 LPBYTE (@LocalGroupMembersInfo), 1);

	if (NetApiStatus <> NERR_Success) then
    	raise ENetworkManagementError.Create (
        							  GetNetworkManagementError (NetApiStatus));
end; { LocalGroupAddUser }

(* ---- *)

function LocalGroupAddUser (const sGroup: WideString; const pUserSID: PSID;
						    out NetApiStatus: NET_API_STATUS;
                            const sLogonServer: WideString = '') : Boolean;

var
	LocalGroupMembersInfo : TLocalGroupMembersInfo0;

begin
	Assert (sGroup <> '');
	Assert (pUserSID <> NIL);

	LocalGroupMembersInfo.lgrmi0_sid := pUserSID;

    NetApiStatus := NetLocalGroupAddMembers (PWideChar (sLogonServer),
    								   		 PWideChar (sGroup), 0,
                                       		 LPBYTE (@LocalGroupMembersInfo), 1);

    Result := NetApiStatus = NERR_Success;
end; { LocalGroupAddUser }

(* ---- *)

procedure LocalGroupAddUser (const sGroup, sUser: WideString;
							 const sLogonServer: WideString = '');
{ "sUser" kann auch der Name einer Gruppe sein }

var
	LocalGroupMembersInfo : TLocalGroupMembersInfo3;
    NetApiStatus : NET_API_STATUS;

begin
	Assert (sGroup <> '');
	Assert (sUser <> '');

	LocalGroupMembersInfo.lgrmi3_domainandname := PWideChar (sUser);

    NetApiStatus := NetLocalGroupAddMembers (PWideChar (sLogonServer),
    								   		 PWideChar (sGroup), 3,
                                             LPBYTE (@LocalGroupMembersInfo), 1);

	if (NetApiStatus <> NERR_Success) then
    	raise ENetworkManagementError.Create (
        							  GetNetworkManagementError (NetApiStatus));
end; { LocalGroupAddUser }

(* ---- *)

function LocalGroupAddUser (const sGroup, sUser: WideString;
							out NetApiStatus: NET_API_STATUS;
							const sLogonServer: WideString = '') : Boolean;
                            										   overload;
{ "sUser" kann auch der Name einer Gruppe sein }

var
	LocalGroupMembersInfo : TLocalGroupMembersInfo3;

begin
	Assert (sGroup <> '');
	Assert (sUser <> '');

	LocalGroupMembersInfo.lgrmi3_domainandname := PWideChar (sUser);

    NetApiStatus := NetLocalGroupAddMembers (PWideChar (sLogonServer),
    								   		 PWideChar (sGroup), 3,
                                             LPBYTE (@LocalGroupMembersInfo), 1);

    Result := NetApiStatus = NERR_Success;
end; { LocalGroupAddUser }

(* ---- *)

function LocalGroupGetUsers (const sGroup: WideString;
{$IFDEF NO_VCL}
						     out asUsers: TaString;
{$ELSE}
						     const UserList: TStrings;
{$ENDIF}
							 const sLogonServer: WideString = '') : Boolean;
{ Für lokale Gruppen. Ist "UserList = TStrings", dann enthält das "Objects"-
  Feld eines Eintrag den Wert "SidTypeUser" für Benutzerkonten und
  "SidTypeGroup" für Gruppen. }

type
	TaUserGroup = array of TLocalGroupMembersInfo2;

const
	PREF_LEN = 1024;

var
	pBuffer : LPBYTE;
    pszLogonServer : PWideChar;
    i : Integer;
	Res : NET_API_STATUS;
    dwRead, dwTotal : DWord;
    hRes : DWord;

begin
	Assert (sGroup <> '');
{$IFNDEF NO_VCL}
	Assert (UserList <> NIL);
	UserList.Clear;
{$ELSE}
    SetLength (asUsers, 0);
{$ENDIF}

	Result := true;

	hRes := 0;

    if (sLogonServer <> '') then
        pszLogonServer := PWideChar (sLogonServer)
    else pszLogonServer := NIL;

	repeat
		Res := NetLocalGroupGetMembers (pszLogonServer, PWideChar (sGroup), 2,
        								pBuffer{%H-}, PREF_LEN,
{$IFDEF JEDI}
                            {%H-}            @dwRead, @dwTotal,
{$ELSE}
                                        dwRead{%H-}, dwTotal{%H-},
{$ENDIF}
                                        PDWord (@hRes));{%H-}

		if (Res = Error_Success) or (Res = ERROR_MORE_DATA) then
		begin
			if (dwRead > 0) then
				for i := 0 to dwRead - 1 do
					with TaUserGroup (pBuffer) [i] do
{$IFDEF NO_VCL}
						StrArrayAdd (asUsers, LowerCase (lgrmi2_domainandname));
{$ELSE}
						UserList.AddObject (lgrmi2_domainandname,
											TObject (lgrmi2_sidusage));
{$ENDIF}

			VerifyApi (NetApiBufferFree (pBuffer) = NErr_Success);
		end { if }
		else Result := false;
	until (Res <> ERROR_MORE_DATA);
end; { LocalGroupGetUsers }

(* ---- *)

function LocalGroupRemoveUser (const sGroup, sUser: WideString;
							   const sLogonServer: WideString = '') : Boolean;

var
	LocalGroupMembersInfo : TLocalGroupMembersInfo3;

begin
	Assert (sGroup <> '');
	Assert (sUser <> '');

	LocalGroupMembersInfo.lgrmi3_domainandname := PWideChar (sUser);

	Result := NetLocalGroupDelMembers (PWideChar (sLogonServer),
									   PWideChar (sGroup), 3,
									   LPBYTE (@LocalGroupMembersInfo),
                                       1) = NERR_Success
end; { LocalGroupRemoveUser }

(* ---- *)

function ProcessStartedByCurrentUser (const dwProcessId: DWord) : Boolean;
{ "Debug"-Privileg muss für Prozess gesetzt sein -> "EnableDebugPrivilege" ! }

var
    hProcess : THandle;
    sUserName : String;

begin
	Assert (dwProcessId <> 0);

    Result := false;

    if (GetProcessHandleFromId (dwProcessId, hProcess)) then
        try
        	if (GetProcessUserName (hProcess, sUserName)) then
            begin
            	sUserName := LowerCase (sUserName);

                if (sUserName = LowerCase (GetDomainAndUserName)) then
                	Result := true;
            end; { if }

        finally
            VerifyApi (CloseHandle (hProcess));
        end; { try / finally }
end; { ProcessStartedByCurrentUser }

(* ---- *)

function RegWriteUnicodeStr (const HRootKey: HKey; const sKey: String;
							 const bCreateKey: Boolean;
							 const sValueName, sValue: WideString) : Boolean;

var
	hOpenKey : HKey;
	lResult : LongInt;
	dwDisposition : DWord;

begin
	Assert (sKey <> '');
	Assert (sValueName <> '');

	hOpenKey := 0;

	if (bCreateKey) then
		lResult := RegCreateKeyEx (HRootKey, PChar (sKey), 0, NIL,
								   REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NIL,
								   hOpenKey, PDWord (@dwDisposition))
	else lResult := RegOpenKeyEx (HRootKey, PChar (sKey), 0, KEY_ALL_ACCESS,
								  hOpenKey);

	Result := lResult = Error_Success;

	if (Result) then
	begin
		Result := RegSetValueExW (hOpenKey, PWideChar (sValueName), 0, REG_SZ,
								  PWideChar (sValue),
								  Length (sValue) * 2) = ERROR_SUCCESS;

		RegCloseKey (hOpenKey);
	end; { if }
end; { RegWriteUnicodeStr }

(* ---- *)

function SIDToString (const pToSID: PSID) : String;

const
    cSID_REVISION = 1;  { Siehe WINNT.H }
    cSize = 200;

var
    sResult, sTemp : String;
    SIA : TSIDIdentifierAuthority;
    dwSubAuthorities, dwSIA : DWord;
    iLen, iIndex : Integer;

begin
	{ Windows NT4, siehe "Converting a Binary SID to String Format"
      http://msdn.microsoft.com/library/default.asp?url=
                                      /library/en-us/security/accctrl_199w.asp }
    // Get the identifier authority value from the SID
    SIA := GetSidIdentifierAuthority (pToSID)^;

    // Get the number of subauthorities in the SID
    dwSubAuthorities := PDWord (GetSidSubAuthorityCount (pToSID))^;

    // Add 'S' prefix and revision number to the string
    sResult := Format ('S-%d-', [cSID_REVISION]);

    // Add SID identifier authority to the string

    SetLength (sTemp{%H-}, cSize);

    if (SIA.Value [0] <> 0) or (SIA.Value [1] <> 0) then
(**
      n = wsprintf( p, "0x%02hx%02hx%02hx%02hx%02hx%02hx",
      (USHORT) psia->Value[0], (USHORT) psia->Value[1],
      (USHORT) psia->Value[2], (USHORT) psia->Value[3],
      (USHORT) psia->Value[4], (USHORT) psia->Value[5] );
**)
        iLen := wvsprintf (PChar (sTemp),
                           '0x%02hx%02hx%02hx%02hx%02hx%02hx',
                           PChar (@SIA.Value [0]))
    else
    begin
(**
      n = wsprintf( p, "%lu", ( (ULONG) psia->Value[5] ) +
      ( (ULONG) psia->Value[4] << 8 ) + ( (ULONG) psia->Value[3] << 16 ) +
      ( (ULONG) psia->Value[2] << 24 ) );
**)
        with SIA do
            dwSIA := Value [5] + (Value [4] shl 8) +
                     (Value [3] shl 16) + (Value [2] shl 24);

        iLen := wvsprintf (PChar (sTemp), '%lu', PChar (@dwSIA));
     end; { else }

    SetLength (sTemp, iLen);

    sResult := sResult + sTemp;

     // Add SID subauthorities to the string.
(**
     for ( i = 0; i < dwSubAuthorities; ++ i )
     {
      n = wsprintf( p, "-%lu", *GetSidSubAuthority( ps, i ) );
      size += n;
      p += n;
     }
**)
    for iIndex := 0 to dwSubAuthorities - 1 do
    begin
        SetLength (sTemp, cSize);

        iLen := wvsprintf (PChar (sTemp), '-%lu',
                           PChar (GetSidSubAuthority (pToSID, iIndex)));

        SetLength (sTemp, iLen);
        sResult := sResult + sTemp;
    end; { for }

    Result := sResult;
end; { SIDToString }

(* ---- *)

procedure Subst (const chDrive: Char; const sPath: String);
{ Wirft Exception bei Fehler }

var
    dwFlags, dwResult : DWord;
    sPathName, sDrive : String;

begin
	Assert (AnsiChar (UpCase (chDrive)) in ['A'..'Z']);

    sDrive := UpCase (chDrive) + ':';

    if (LowerCase (sPath) = '/d') then
    begin
    	SetLength (sPathName{%H-}, MAX_PATH);
        dwFlags := DDD_REMOVE_DEFINITION or DDD_RAW_TARGET_PATH;

        dwResult := QueryDOSDevice (PChar (sDrive), PChar (sPathName), MAX_PATH);

        if (dwResult > 0) then
        	SetLength (sPathName, dwResult)
        else RaiseLastWin32Error;
    end { if }
    else
    begin
	    dwFlags := 0;

        if not (DirectoryExists (sPath)) then
        	RaiseLastWin32Error;

        sPathName := sPath;
    end; { else }

    Win32Check (DefineDOSDevice (dwFlags, PChar (sDrive), PChar (sPathName)));
end; { Subst }

(* ---- *)

function StrArrayAdd (var asUsers: TaString; const sStr: String) : Boolean;

var
    iIndex, iCount : Integer;

begin
    iCount := Length (asUsers);

    if (iCount > 0) then
        for iIndex := 0 to High (asUsers) do
            if (asUsers [iIndex] = sStr) then
            begin
              	Result := false;
                exit;
            end;

    SetLength (asUsers, iCount + 1);
    asUsers [High (asUsers)] := sStr;
    Result := true;
end; { StrArrayAdd }

(* ---- *)

function SystemShutdown (const sComputerName: String = '';
						 const bRestart: Boolean = false;
						 const uTimeOut: UInt = 30;
						 const bForceAppsClosed: Boolean = false;
						 const sShutdownMsg: String = '') : Boolean;

var
    pchMachineName, pchShutdownMsg : PChar;

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

    Result := InitiateSystemShutdown (pchMachineName, pchShutdownMsg, uTimeOut,
    								  bForceAppsClosed, bRestart);

    if not (Result) then
    	AdjustPrivileges (SE_SHUTDOWN_NAME, false);
end; { SystemShutdown }

(* ---- *)

procedure SystemShutdownAbort (const sComputerName: String = '';
                               const bExceptionOnError: Boolean = false);

var
    pchMachineName : PChar;


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

    try
        if not (AbortSystemShutdown  (pchMachineName)) then
            if (bExceptionOnError) then
                RaiseLastWin32Error;

    finally
        AdjustPrivileges (SE_SHUTDOWN_NAME, false);
    end; { try / finally }
end; { SystemShutdownAbort }

(* ---- *)

function UserAccountExists (const sUserName: WideString;
							const sDC: WideString = '') : Boolean;
{ Überprüft, ob das Benutzerkonto existiert
  ->> sUserName : Muss das Format "domain\username" haben!
  ->> sDC : optionaler DC-Name als UNC-Name.
  <<- True, wenn Erfolg }

var
    pUserInfo0 : ^TUserInfo0;

begin
    Assert (sUserName <> '');

    Result := false;

    if (NetUserGetInfo (PWideChar (sDC), PWideChar (sUserName), 1,
                        LPBYTE ({%H-}pUserInfo0)) = NERR_Success) then
    begin
        Result := true;{%H-}

        VerifyApi (NetApiBufferFree (pUserInfo0) = NErr_Success);
    end; { if }
end; { UserAccountExists }

(* ---- *)

function UserAccountIsEnabled (const sUserName: WideString;
                               const sDC: WideString = '') : Boolean;
{ ->> sUserName : Benutzername ohne vorgestellten Domänennamen.
  ->> sDC : optionaler DC-Name als UNC-Name.
  <<- Result : True, wenn Konto aktiv ist. }

var
    pUserInfo1 : ^TUserInfo1;

begin
    Assert (sUserName <> '');

    Result := false;

    if (NetUserGetInfo (PWideChar (sDC), PWideChar (sUserName), 1,
                        LPBYTE ({%H-}pUserInfo1)) = NERR_Success) then
    begin
        Result := (pUserInfo1^.usri1_flags and UF_ACCOUNTDISABLE) = 0;

        VerifyApi (NetApiBufferFree (pUserInfo1) = NErr_Success);
    end; { if }
end; { UserAccountIsEnabled }

(* ---- *)

end.
