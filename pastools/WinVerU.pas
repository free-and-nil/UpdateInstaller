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

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}

unit WinVerU;

interface

const
    cWindowsKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
    cWinNT_Key = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';

type
	TProductType = (ptWorkstation, ptDomainController, ptServer);

function Get_IE_Version (out sVersion: String) : Boolean;
function GetOwnerAndOrganization (out sOwner, sOrganization: String) : Boolean;
function GetProductType : TProductType;
function GetServicePack : String;
function GetServicePackNo : Integer;
function IsDomainController : Boolean;
function IsServer : Boolean;
function IsTerminalServer : Boolean;
(**
function IsWindowsPE : Boolean;
**)
function IsWorkstation : Boolean;

implementation

uses SysUtils, Windows, Registry,
	 RegistryHelperU, RegistryApiU, ShlwApi_ImportU, Win32ToolsU, BaseTypesU,
     WinNT_ImportU;

ResourceString
	cUnknowValueMsg = '"%s" value unknown';

const
	cProductOptionsKey = 'SYSTEM\CurrentControlSet\Control\ProductOptions';
    cProductSuite = 'ProductSuite';

(* ---- *)

function Get_IE_Version (out sVersion: String) : Boolean;

const
    cKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE';

var
    sPath : String;

begin
    Result := false;

    if not (RegKeyExists (cHKLM, cKey)) then
        exit;

    sPath := RegReadStr (cHKLM, cKey, '');

    if (sPath <> '') then
		if (FileExists (sPath)) then
        begin
		    Result := true;
	    	sVersion := GetVersionInfo (sPath);
        end; { if }
end; { Get_IE_Version }

(* ---- *)

function GetOwnerAndOrganization (out sOwner, sOrganization: String) : Boolean;

var
    sKey : String;

begin
    Result := false;

    with TRegistry.Create do
        try
            RootKey := HKey_Local_Machine;

            if (Win32Platform = VER_PLATFORM_WIN32_NT) then
                sKey := cWinNT_Key
            else sKey := cWindowsKey;

            if (OpenKeyReadOnly (sKey)) then
            begin
                Result := true;

                sOwner := ReadString ('RegisteredOwner');
                sOrganization := ReadString ('RegisteredOrganization');
            end; { if }

        finally
            Free;
        end; { try / finally }
end; { GetOwnerAndOrganization }

(* ---- *)

function GetProductType : TProductType;

    (* ---- *)

    procedure GetProdcutTypeFromRegistry;

    const
        cValueName = 'ProductType';

    var
        sValue : String;

    begin
        sValue := RegReadStr (cHKLM, cProductOptionsKey, cValueName);

        if (sValue = '') then
            RaiseLastWin32Error;

        if (sValue = 'WinNT') then
            Result := ptWorkstation
        else if (sValue = 'LanmanNT') then
            Result := ptDomainController
        else if (sValue = 'ServerNT') then
            Result := ptServer
        else raise EOsError.CreateFmt (cUnknowValueMsg, [cValueName]);
    end; { GetProdcutTypeFromRegistry; }

    (* ---- *)

    function CallGetVersionEx (out ProductType: TProductType) : Boolean;

{$IFNDEF DELPHI_XE2_UP}
	const
        VER_NT_WORKSTATION                 = $0000001;
        VER_NT_DOMAIN_CONTROLLER           = $0000002;
        VER_NT_SERVER                      = $0000003;
{$ENDIF}

    var
    	VerInfoEx : TOSVersionInfoEx;
        pVerInfo : POSVersionInfo;

    begin
    	FillChar (VerInfoEx{%H-}, SizeOf (TOSVersionInfoEx), #0);
        VerInfoEx.dwOSVersionInfoSize := SizeOf (TOSVersionInfoEx);

        pVerInfo := POSVersionInfo (@VerInfoEx);

        if (GetVersionEx (pVerInfo^)) then
        begin
	        Result := true;

        	if (VerInfoEx.wProductType = VER_NT_WORKSTATION) then
            	ProductType := ptWorkstation
            else if (VerInfoEx.wProductType = VER_NT_DOMAIN_CONTROLLER) then
            	ProductType := ptDomainController
            else if (VerInfoEx.wProductType = VER_NT_SERVER) then
            	ProductType := ptServer
            else raise EOsError.CreateFmt (cUnknowValueMsg, ['GetVersionEx']);
        end { if }
        else Result := false;
    end; { CallGetVersionEx }

    (* ---- *)

begin { GetProductType }
    if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    	Result := ptWorkstation  // Windows 9x
    else
    	if not (CallGetVersionEx (Result)) then
        	GetProdcutTypeFromRegistry;
end; { GetProductType }

(* ---- *)

function GetServicePack : String;

var
    VersionInfo : TOSVersionInfo;

begin
	FillChar (VersionInfo{%H-}, SizeOf (TOSVersionInfo), #0);
    VersionInfo.dwOSVersionInfoSize := SizeOf (TOSVersionInfo);
    Win32Check (GetVersionEx (VersionInfo));
	Result := Trim (VersionInfo.szCSDVersion);
end; { GetServicePack }

(* ---- *)

function GetServicePackNo : Integer;

var
	sSP : String;
    chVer : AnsiChar;
    VerInfoEx : TOSVersionInfoEx;
    pVerInfo : POSVersionInfo;

begin
	Result := 0;

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
    	FillChar (VerInfoEx{%H-}, SizeOf (TOSVersionInfoEx), #0);
        VerInfoEx.dwOSVersionInfoSize := SizeOf (TOSVersionInfoEx);

        pVerInfo := POSVersionInfo (@VerInfoEx);

        if (GetVersionEx (pVerInfo^)) then
        begin
			Result := VerInfoEx.wServicePackMajor;
            exit;
        end; { if }
    end; { if }

    sSP := GetServicePack;

    if (sSP <> '') then
    begin
        chVer := AnsiChar (sSP [Length (sSP)]);

        if (chVer in ['0'..'9']) then
            Result := StrToInt (String (chVer))
    end; { if }
end; { GetServicePackNo }

(* ---- *)

function IsDomainController : Boolean;
begin
	Result := GetProductType = ptDomainController;
end; { IsDomainController }

(* ---- *)

function IsServer : Boolean;
begin
	Result := GetProductType = ptServer;
end; { IsServer }

(* ---- *)

function IsTerminalServer : Boolean;

var
	asValue : TaString;
    iIndex : Integer;

begin
	Result := false;

    if not (IsServer) then
    	exit;

    if (Assigned (IsOS)) then
    	Result := IsOS (OS_TERMINALSERVER)
    else
    begin
        if (RegReadMulti_SZ (cHKLM, cProductOptionsKey, cProductSuite,
        					 asValue)) then
            for iIndex := 0 to High (asValue) do
                if (Pos ('Terminal Server', asValue [iIndex]) > 0) then
                begin
                    Result := true;
                    Break;
                end; { if }
    end; { else }
end; { IsTerminalServer }

(* ---- *)

(**
function IsWindowsPE : Boolean;
begin
	Result := RegReadStr (HKey_Local_Machine, cWinNT_Key,
    					  'EditionID') = 'WindowsPE'
end; { IsWindowsPE }
**)

(* ---- *)

function IsWorkstation : Boolean;
begin
	Result := GetProductType = ptWorkstation;
end; { IsWorkstation }

(* ---- *)

end.

