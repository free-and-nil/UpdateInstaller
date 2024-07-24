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

unit WindowsDisplayName_WMI_U;

interface

function GetWindowsDisplayName : String;
function GetWindowsInfo : String;

implementation

uses Windows, SysUtils, ActiveX, ComObj, Variants,
     RegistryApiU, PasTools, Wow64U;

ResourceString
	cWin9x_VersionInfo = '%s (version = %s)';
    cW2K_VersionInfo = '%s (build %s)';
    cW10_VersionInfo = '%s release %s (build %d)';

const
    cWinNT_Key = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';

(* ---- *)

function GetPlatform : String;
begin
    Result := iif (IsWindows_x64, 'x64', 'x86');
end; { GetPlatform }

(* ---- *)

function GetWindowsDisplayName : String;

const
    cNameSpace = 'root\CIMV2';
    cWin32_OperatingSystem_Query = 'SELECT Caption FROM Win32_OperatingSystem';

var
    FSWbemLocator, FWMIService, FWbemObjectSet, FWbemObject : OLEVariant;
    oEnum  : IEnumvariant;
    iValue : LongWord;
    sQuery : WideString;

begin
    FSWbemLocator := CreateOleObject ('WbemScripting.SWbemLocator');
    FWMIService   := FSWbemLocator.ConnectServer ('localhost',
                                                  cNameSpace, '', '');

    sQuery := cWin32_OperatingSystem_Query;
    FWbemObjectSet := FWMIService.ExecQuery (sQuery, 'WQL', 0);

    oEnum := IUnknown (FWbemObjectSet._NewEnum) as IEnumVariant;

    while (oEnum.Next (1, FWbemObject, iValue) = 0) do
        if (Length ({%H-}FWbemObject{%H-}.Caption) > 0) then
            Result := {%H-}String (FWbemObject.Caption);

    FWbemObject := {%H-}Unassigned;
    FWbemObjectSet := {%H-}Unassigned;
end; { GetWindowsDisplayName }

(* ---- *)

function GetWindowsInfo_Win10_Win11 (const sName: String;
                                     const iBuildNo: Integer) : String;

const
    cReleaseId = 'ReleaseId';
    cDisplayVersion = 'DisplayVersion';

var
    sRelease : String;

begin
    if (RegValueExists (HKey_Local_Machine, cWinNT_Key,
                        cDisplayVersion)) then
        sRelease := RegReadStr (HKey_Local_Machine, cWinNT_Key,
                                cDisplayVersion)
    else if (RegValueExists (HKey_Local_Machine, cWinNT_Key,
                             cReleaseId)) then
        sRelease := RegReadStr (HKey_Local_Machine, cWinNT_Key,
                                cReleaseId)
    else sRelease := '';

    if (iBuildNo < 22000) then  // Windows 10
        Result := Format (cW10_VersionInfo, [sName, sRelease, iBuildNo])
    else Result := Format (cW10_VersionInfo,
                           [GetWindowsDisplayName, sRelease, iBuildNo]);
end; { GetWindowsInfo_Win10_Win11 }

(* ---- *)

function GetWindowsInfo : String;

const
    cBuildNo = 'CurrentBuildNumber';
    cProductName = 'ProductName';
    cVersionNumber = 'VersionNumber';
    cWin9x_Key = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
    cNT4 = 'Windows NT4 ';
    cProductType = 'ProductType';
    cWinNT = 'WinNT';
    cWorkstation = 'Workstation';
    cServer = 'Server';
    cProductOptionsKey = 'SYSTEM\CurrentControlSet\Control\ProductOptions';

var
    sName, sVerNo, sBuildNo, sProductType, sSP : String;
    iBuildNo : Integer;

begin
    if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
    begin
        sName := RegReadStr (HKey_Local_Machine, cWin9x_Key, cProductName);
        sVerNo := RegReadStr (HKey_Local_Machine, cWin9x_Key, cVersionNumber);

        Result := Format (cWin9x_VersionInfo, [sName, sVerNo]);
        exit;
    end; { if }

    if (Win32MajorVersion = 4) then
    begin
        if (RegReadStr (HKey_Local_Machine, cProductOptionsKey,
                        cProductType) = cWinNT) then
            sProductType := cWorkStation
        else sProductType := cServer;

        sSP := Trim (Win32CSDVersion);

        Result := cNT4 + sProductType + iif (sSP <> '', ' ' + sSP, '');
        exit;
    end; { if }

    sName := RegReadStr (HKey_Local_Machine, cWinNT_Key, cProductName);

    sBuildNo := RegReadStr (HKey_Local_Machine, cWinNT_Key, cBuildNo);
    iBuildNo := StrToIntDef (sBuildNo, (-1));

    if (iBuildNo = (-1)) or (iBuildNo <= 9600) then  // Windows 8.1 or below
    begin
        if (CheckWin32Version (5, 1)) then
            Result := Format (cW2K_VersionInfo + ' %s',
                              [sName, sBuildNo, GetPlatform])
        else Result := Format (cW2K_VersionInfo, [sName, sBuildNo]);
    end { if }
    else Result := GetWindowsInfo_Win10_Win11 (sName, iBuildNo) + ' ' +
                   GetPlatform;
end; { GetWindowsInfo }

(* ---- *)

end.
