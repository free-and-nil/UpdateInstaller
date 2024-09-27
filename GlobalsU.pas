// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

unit GlobalsU;

{$R \pas-win\ExecutionLevel_RequireAdministrator_CmdLine_Win10.RES}

interface

uses Windows;

const
    cCheckParam = 'Check';
    cCompatibleSystems = 'CompatibleSystems';
    cExeExt = '.exe';
    cFilesDir = '\$Files';
    cHashText = '#text';
    cInstallParam = 'Install';
    cPnpUtilExe = '\pnputil.exe';
    cUnpackParam = 'Unpack';
    cUpdateBiosParam = 'UpdateBios';
    cUpdateFirmwareParam = 'UpdateFirmware';

type
    TAction = (aCheck, aUnpack, aInstall);
    TaDWord = array of DWord;
    TCheckResult = (crUndefined, crTrue, crFalse);

ResourceString
    cDirMissingMsg = 'Folder "%s" not found';
    cFailureMsg = 'Failure';
    cProcessReturnCodeMsg = '[%s] Process return code = %u';
    cProcessExecutionErrMsg = 'Unable to start the process: %s';
    cSuccessMsg = 'Success';

function Action : TAction;
function AppName : String;
function BiosModel : String;
function BiosSystemName : String;
function BiosVersion : String;
function BoolToCheckResult (const bResult: Boolean) : TCheckResult;
function CheckAll : Boolean;
function CompatibleSystemsCount : Integer;
function DebugMode : Boolean;
procedure EnableDebugMode;
function GetEmbeddedControllerVersion (out uMajor, uMinor: UInt) : Boolean;
function GetIniFileName : String;
function GetInstallerLogFileName (const sPrefix: String) : String;
function Get_OS_BuildNo : UInt;
function Get_OS_MajorVer : UInt;
function Get_OS_MinorVer : UInt;
function Get_OS_Ver : String;
function GlobalLogFolder : String;
function IgnoreExistingFolders : Boolean;
function IgnorePackage : Boolean;
function IgnorePackageName : String;
function IgnoreSystemCompatibility : Boolean;
function IgnoreWarning : Boolean;
function IgnoreWindowsCompatibility : Boolean;
function IsCompatibleSystem (const sSystem: String;
                             out sMatch: String) : Boolean;
function IsDpInstExe (const sCmd: String) : Boolean;
function IsWindows10 : Boolean;
function ListAll : Boolean;
function ListSkippedPackages : Boolean;
procedure LogVerbose (const sMsg: String;
                      const bNewLine: Boolean = true); overload;
procedure LogVerbose (const sMsg: String; const Args: array of const;
                      const bNewLine: Boolean = true); overload;
function PromptForExecution : Boolean;
function SystemManufacturer : String;
procedure SetAction (const Action: TAction);
procedure SetBiosInfo (const sManufacturer, sName, sModel, sVersion: String);
procedure SetCheckAll;
procedure SetCompatibleSystems (const sSystems: String);
procedure SetEmbeddedControllerVersion (const uMajor, uMinor: UInt);
procedure SetGlobalLogFolder (const sFolder: String);
procedure SetIgnoreExistingFolders;
procedure SetIgnorePackage (const sPackage: String);
procedure SetIgnoreSystemCompatibility;
procedure SetIgnoreWarning;
procedure SetIgnoreWindowsCompatibility;
procedure SetIniFile (const sFileName: String);
procedure SetPromptForExecution (const bPrompt: Boolean);
procedure SetUpdateInstallerLogFileName;
procedure SetListAll;
procedure SetListSkippedPackages;
procedure SetSinglePackage (const sPackage: String);
procedure SetUpdateAll;
procedure SetUpdateBios;
procedure SetUpdateFirmware;
procedure SetVerboseLogging (const bEnable: Boolean);
procedure SetVeryVerboseLogging;
function SinglePackage : Boolean;
function SinglePackageName : String;
function UpdateAll : Boolean;
function UpdateBios : Boolean;
function UpdateFirmware : Boolean;
function ValueInArray (const dwValue: DWord; const aValues: TaDWord) : Boolean;
function VerboseLogging : Boolean;
function VeryVerboseLogging : Boolean;
function WinExecAndWait (const sCmdLine: String;
                         out dwExitCode: DWord) : Boolean;
function YesNoPrompt (const sMsg: String;
                      const bLogging: Boolean = false) : Boolean;

implementation

uses SysUtils, DateUtils, Classes, Types,
{$IFNDEF FPC}
     StrUtils, ConsoleU,
{$ENDIF}
     ConsoleOutputU,
     LogToTempU, NativeApi_ImportU, Wow64U, Win32ToolsU, PasTools, BaseTypesU,
     ImageHlpU, StrUtilsHelperU;

ResourceString
    cChangingLogFileMsg = 'Changing log file to "%s"';
    cCompatibleModelsErrorMsg =
                  'The parameter "/CompatibleModels" contains invalid entries';
    cExecutingCmdMsg = '[%s] Executing the command'#13#10'%s';

const
    cCmdExe = '\cmd.exe';
    cMsiExecExe = '\msiexec.exe /i ';

var
    _Action : TAction;
    asCompatibleSystems : TaString = NIL;
    bDebugMode : Boolean = false;
    bCheckAll : Boolean = false;
    bIgnoreExistingFolders : Boolean = false;
    bIgnoreSystemCompatibility : Boolean = false;
    bIgnoreWarning : Boolean = false;
    bIgnoreWindowsCompatibility : Boolean = true {false};
    bListAll : Boolean = false;
    bListSkippedPackages : Boolean = false;
    bPromptForExecution : Boolean = false;
    bUpdateAll : Boolean = false;
    bUpdateBios : Boolean = false;
    bUpdateFirmware : Boolean = false;
    bVerboseLogging : Boolean = false;
    bVeryVerboseLogging : Boolean = false;
    sAppName : String = '';
    sBiosManufacturer : String = '';
    sBiosSystemName : String = '';
    sBiosModel : String = '';
    sBiosVersion : String = '';
    sGlobalLogFolder : String = '';
    sIgnorePackage : String = '';
    sIniFile : String = '';
    sOS_Ver : String = '';
    sSinglePackage : String = '';
    uEmbeddedMajor : UInt = 0;
    uEmbeddedMinor : UInt = 0;
    uOS_MajorVer, uOS_MinorVer, uOS_BuildNumber : UInt;

(* ---- *)

function Action : TAction;
begin
    Result := _Action;
end; { Action }

(* ---- *)

function AppName : String;
begin
	Result := sAppName;
end; { AppName }

(* ---- *)

function BiosModel : String;
begin
    Result := sBiosModel;
end; { BiosModel }

(* ---- *)

function BiosSystemName : String;
begin
    Result := sBiosSystemName;
end; { BiosSystemName }

(* ---- *)

function BiosVersion : String;
begin
    Result := sBiosVersion;
end; { BiosVersion }

(* ---- *)

function BoolToCheckResult (const bResult: Boolean) : TCheckResult;
begin
    if (bResult) then
        Result := crTrue
    else Result := crFalse
end; { BoolToCheckResult }

(* ---- *)

function CheckAll : Boolean;
begin
    Result := bCheckAll;
end; { CheckAll }

(* ---- *)

function CompatibleSystemsCount : Integer;
begin
    if (Assigned (asCompatibleSystems)) then
        Result := Length (asCompatibleSystems)
    else Result := 0;
end; { CompatibleSystemsCount }

(* ---- *)

function DebugMode : Boolean;
begin
    Result := bDebugMode;
end; { DebugMode }

(* ---- *)

procedure EnableDebugMode;
begin
    bDebugMode := true;
end; { EnableDebugMode }

(* ---- *)

function GetEmbeddedControllerVersion (out uMajor, uMinor: UInt) : Boolean;
begin
    uMajor := uEmbeddedMajor;
    uMinor := uEmbeddedMinor;

    Result := (uMajor <> $FF) and (uMinor <> $FF);
end; { GetEmbeddedControllerVersion }

(* ---- *)

function GetIniFileName : String;
begin
    Result := sIniFile;
end; { GetIniFileName }

(* ---- *)

function GetInstallerLogFileName (const sPrefix: String) : String;

const
	cFileName = '%s_%s_%.4d%.2d%.2d_%.2d%.2d%.2d.log';

var
	wYear, wMonth, wDay, wHour, wMin, wSec, wMSec : Word;
    sAction : String;

begin
	DecodeDateTime (Now, wYear, wMonth, wDay, wHour, wMin, wSec, wMSec);

    case Action of
        aCheck : sAction := cCheckParam;
        aInstall : sAction := cInstallParam;
        aUnpack : sAction := cUnpackParam;
        else sAction := ''
    end; { case Action of }

    if (UpdateBios) then
        sAction := sAction + '_' + cUpdateBiosParam
    else if (UpdateFirmware) then
        sAction := sAction + '_' + cUpdateFirmwareParam;

    Result := Format (cFileName,
                      [sPrefix, sAction, wYear, wMonth, wDay, wHour, wMin, wSec])
end; { GetInstallerLogFileName }

(* ---- *)

function Get_OS_BuildNo : UInt;
begin
    Result := uOS_BuildNumber;
end; { Get_OS_BuildNo }

(* ---- *)

function Get_OS_MajorVer : UInt;
begin
    Result := uOS_MajorVer;
end; { Get_OS_MajorVer }

(* ---- *)

function Get_OS_MinorVer : UInt;
begin
    Result := uOS_MinorVer;
end; { Get_OS_MinorVer }

(* ---- *)

function Get_OS_Ver : String;
begin
    Result := sOS_Ver;
end; { Get_OS_Ver }

(* ---- *)

function GlobalLogFolder : String;
begin
    Result := sGlobalLogFolder;
end; { GlobalLogFolder }

(* ---- *)

function IgnoreExistingFolders : Boolean;
begin
    Result := bIgnoreExistingFolders;
end; { IgnoreExistingFolders }

(* ---- *)

function IgnorePackage : Boolean;
begin
    Result := sIgnorePackage <> '';
end; { IgnorePackage }

(* ---- *)

function IgnorePackageName : String;
begin
    Result := sIgnorePackage;
end; { IgnorePackageName }

(* ---- *)

function IgnoreSystemCompatibility : Boolean;
begin
    Result := bIgnoreSystemCompatibility;
end; { IgnoreSystemCompatibility }

(* ---- *)

function IgnoreWarning : Boolean;
begin
    Result := bIgnoreWarning;
end; { IgnoreWarning }

(* ---- *)

function IgnoreWindowsCompatibility : Boolean;
begin
    Result := bIgnoreWindowsCompatibility;
end; { IgnoreWindowsCompatibility }

(* ---- *)

function IsCompatibleSystem (const sSystem: String;
                             out sMatch: String) : Boolean;

var
    iIndex : Integer;

begin
    Result := false;

    if (Length (asCompatibleSystems) > 0) then
        for iIndex := 0 to High (asCompatibleSystems) do
            if (MatchString (asCompatibleSystems [iIndex], sSystem)) then
            begin
                Result := true;
                sMatch := asCompatibleSystems [iIndex];
                Break;
            end; { if }
end; { IsCompatibleSystem }

(* ---- *)

function IsDpInstExe (const sCmd: String) : Boolean;
begin
    Result := (Pos ('dpinst.exe', sCmd) > 0) or
              (Pos ('dpinst_x86.exe', sCmd) > 0) or
              (Pos ('dpinst_x64.exe', sCmd) > 0);
end; { IsDpInstExe }

(* ---- *)

function IsWindows10 : Boolean;
begin
    Result := Get_OS_MajorVer = 10
end; { IsWindows10 }

(* ---- *)

function ListAll : Boolean;
begin
    Result := bListAll;
end; { ListAll }

(* ---- *)

function ListSkippedPackages : Boolean;
begin
    Result := bListSkippedPackages
end; { ListSkippedPackages }

(* ---- *)


procedure LogVerbose (const sMsg: String; const bNewLine: Boolean);
begin
    if (bVerboseLogging) then
        LogMsg (sMsg, bNewLine)
    else
    begin
        DisableLogFunction;
        LogMsg (sMsg, bNewLine);
        EnableLogFunction;
    end;
end; { TPackage.LogVerbose }

(* ---- *)

procedure LogVerbose (const sMsg: String; const Args: array of const;
                      const bNewLine: Boolean);
begin
    if (bVerboseLogging) then
        LogMsg (sMsg, Args, bNewLine)
    else
    begin
        DisableLogFunction;
        LogMsg (sMsg, Args, bNewLine);
        EnableLogFunction;
    end;
end; { TPackage.LogVerbose }

(* ---- *)

function PromptForExecution : Boolean;
begin
    Result := bPromptForExecution;
end; { PromptForExecution }

(* ---- *)

function SystemManufacturer : String;
begin
	Result := sBiosManufacturer;
end; { SystemManufacturer }

(* ---- *)

procedure SetAction (const Action: TAction);
begin
    _Action := Action;
end; { SetAction }

(* ---- *)

procedure SetBiosInfo (const sManufacturer, sName, sModel, sVersion: String);
begin
	sBiosManufacturer := sManufacturer;
    sBiosModel := UpperCase (sModel);
    sBiosSystemName := LowerCase (sName);
    sBiosVersion := UpperCase (sVersion);
end; { SetBiosInfo }

(* ---- *)

procedure SetCheckAll;
begin
    bCheckAll := true;
end; { SetCheckAll }

(* ---- *)

procedure SetCompatibleSystems (const sSystems: String);

var
    iIndex : Integer;
    SystemsList : TStringList;

begin
    Assert (sSystems <> '');

    SystemsList := TStringList.Create;

    try
        SystemsList.StrictDelimiter := true;
        SystemsList.CommaText := sSystems;

        if (SystemsList.Count > 0) then
        begin
            SetLength (asCompatibleSystems, SystemsList.Count);

            for iIndex := 0 to SystemsList.Count - 1 do
                asCompatibleSystems [iIndex] := LowerCase (SystemsList [iIndex])
        end { if }
        else raise Exception.Create (cCompatibleModelsErrorMsg);

    finally
        SystemsList.Free;
    end; { try / finally }
end; { SetCompatibleSystems }

(* ---- *)

procedure SetEmbeddedControllerVersion (const uMajor, uMinor: UInt);
begin
    uEmbeddedMajor := uMajor;
    uEmbeddedMinor := uMinor;
end; { SetEmbeddedControllerVersion }

(* ---- *)

procedure SetGlobalLogFolder (const sFolder: String);
begin
    Assert (sFolder <> '');

    sGlobalLogFolder := sFolder;
end; { SetGlobalLogFolder }

(* ---- *)

procedure SetIgnoreExistingFolders;
begin
    bIgnoreExistingFolders := true;
end; { SetIgnoreExistingFolders }

(* ---- *)

procedure SetIgnorePackage (const sPackage: String);
begin
    Assert (sPackage <> '');

    sIgnorePackage := sPackage;
end; { SetIgnorePackage }

(* ---- *)

procedure SetIgnoreSystemCompatibility;
begin
    bIgnoreSystemCompatibility := true;
end; { SetIgnoreSystemCompatibility }

(* ---- *)

procedure SetIgnoreWarning;
begin
    bIgnoreWarning := true;
end; { SetIgnoreWarning }

(* ---- *)

procedure SetIgnoreWindowsCompatibility;
begin
    bIgnoreWindowsCompatibility := true;
end; { SetIgnoreWindowsCompatibility }

(* ---- *)

procedure SetIniFile (const sFileName: String);
begin
	Assert (sFileName <> '');

    sIniFile := sFileName;
end; { SetIniFile }

(* ---- *)

procedure SetPromptForExecution (const bPrompt: Boolean);
begin
    bPromptForExecution := bPrompt;
end; { SetPromptForExecution }

(* ---- *)

procedure SetUpdateInstallerLogFileName;

var
    sFileName : String;

begin
    Assert (sGlobalLogFolder <> '');

    sFileName := IncludeTrailingBackslash (sGlobalLogFolder) +
                 GetInstallerLogFileName (sAppName);

    DisableLogFunction;
    LogMsg (cChangingLogFileMsg, [sFileName]);
    EnableLogFunction;

    SetLogFileName (sFileName);
end; { SetUpdateInstallerLogFileName }

(* ---- *)

procedure SetListAll;
begin
    bListAll := true;
end; { SetListAll }

(* ---- *)

procedure SetListSkippedPackages;
begin
    bListSkippedPackages := true
end; { SetListSkippedPackages }

(* ---- *)


procedure SetSinglePackage (const sPackage: String);
begin
    Assert (sPackage <> '');

    sSinglePackage := sPackage;
end; { SetSinglePackage }

(* ---- *)

procedure SetUpdateAll;
begin
    bUpdateAll := true;
end; { SetUpdateAll }

(* ---- *)

procedure SetUpdateBios;
begin
    bUpdateBios := true;
end; { SetUpdateBios }

(* ---- *)

procedure SetUpdateFirmware;
begin
    bUpdateFirmware := true;
end; { SetUpdateFirmware }

(* ---- *)

procedure SetVerboseLogging (const bEnable: Boolean);
begin
    bVerboseLogging := bEnable;
end; { VerboseLogging }

(* ---- *)

procedure SetVeryVerboseLogging;
begin
    bVeryVerboseLogging := true;
    SetVerboseLogging (true);
end; { SetVeryVerboseLogging }

(* ---- *)

function SinglePackage : Boolean;
begin
    Result := sSinglePackage <> '';
end; { SinglePackage }

(* ---- *)

function SinglePackageName : String;
begin
    Result := sSinglePackage;
end; { SinglePackageName }

(* ---- *)

function UpdateAll : Boolean;
begin
    Result := bUpdateAll;
end; { UpdateAll }

(* ---- *)

function UpdateBios : Boolean;
begin
    Result := bUpdateBios;
end; { UpdateBios }

(* ---- *)

function UpdateFirmware : Boolean;
begin
    Result := bUpdateFirmware;
end; { UpdateFirmware }

(* ---- *)

function ValueInArray (const dwValue: DWord; const aValues: TaDWord) : Boolean;

var
    iIndex : Integer;

begin
    for iIndex := 0 to High (aValues) do
        if (aValues [iIndex] = dwValue) then
        begin
            Result := true;
            exit;
        end; { if }

    Result := false;
end; { ValueInArray }

(* ---- *)

function VerboseLogging : Boolean;
begin
    Result := bVerboseLogging;
end; { VerboseLogging }

(* ---- *)

function VeryVerboseLogging : Boolean;
begin
    Result := bVeryVerboseLogging;
end; { VeryVerboseLogging }

(* ---- *)

function WinExecAndWait (const sCmdLine: String;
                         out dwExitCode: DWord) : Boolean;

    (* ---- *)

(**
    function IsBatchFile (out iStartPos: Integer) : Boolean;
    begin
        if (Pos (cCmdExe, sCmdLine) > 0) then
        begin
            Result := true;
            iStartPos := Pos ('"', sCmdLine);
        end { if }
        else Result := false;
    end; { IsBatchFile }
**)

    (* ---- *)

(**
    function ExtractPath (const sPath: String) : String;

    var
        iStartPos, iEndPos : Integer;

    begin
        Assert (Pos ('\', sPath) > 0);

        iStartPos := 1;

        if (sPath [1] = '"') or (IsBatchFile (iStartPos)) then
        begin
            Inc (iStartPos);
            iEndPos := PosEx ('"', sPath, iStartPos + 1);

            Assert (iEndPos > 0);

            Result := ExtractFileDir (Copy (sPath, iStartPos,
                                            Pred (iEndPos - iStartPos)));
        end { else }
        else
        begin
            iEndPos := Pos (' ', sPath);

            if (iEndPos > 0) then
                Result := ExtractFileDir (Copy (sPath, 1, iEndPos - 1))
            else Result := ExtractFileDir (sPath);
        end; { else }
    end; { ExtractPath }

**)

    (* ---- *)

    function ConsoleOutput (const sCmd, sPath: String) : Boolean;

        (* ---- *)

        function GetLogFileName : String;

        var
            asStrings : TStringDynArray;
            sFileName : String;

        begin
            asStrings := SplitString (sPath, '\');

            if (Pos (asStrings [High (asStrings)], cFilesDir) = 2) then
                sFileName := asStrings [High (asStrings) - 1]
            else sFileName := asStrings [High (asStrings)];

            Result := AddBackSlash (sGlobalLogFolder) +
                      GetInstallerLogFileName (sFileName);
        end; { GetLogFileName }

        (* ---- *)

    var
        sLogFile : String;
        ConsoleOutput : TConsoleOutput;

    begin { ConsoleOutput }
        sLogFile := GetLogFileName;

        ConsoleOutput := TConsoleOutput.Create (sLogfile);

        try
            Result := CaptureConsoleOutput (sCmd, ConsoleOutput.OutputProc,
                                            dwExitCode, sPath, 8192);

        finally
            ConsoleOutput.Free;
        end; { try / finally }
    end; { ConsoleOutput }

    (* ---- *)

    function ExtractPath (const sPath: String) : String;

    var
        iPos : Integer;

    begin
        Assert (Pos ('\', sPath) > 0);

        if (Pos (cPnpUtilExe, sPath) > 0) then
        begin
            iPos := LastPos (' ', sPath);
            Result := ExtractFileDir (Copy (sPath,
                                            iPos + 1, Length (sPath) - iPos));
        end { if }
        else
        begin
            iPos := Pos (' ', sPath);

            if (iPos > 0) then
                Result := ExtractFileDir (Copy (sPath, 1, iPos - 1))
            else Result := ExtractFileDir (sPath);
        end; { else }
    end; { ExtractPath }

    (* ---- *)

    function IsCommandLineTool (const sCmd: String) : Boolean;

    var
        iExtPos, iBackslash : Integer;
        sExePath : String;

    begin
        Result := false;

        Assert (sCmd <> '');

        iExtPos := Pos ('.exe', sCmd);

        iBackslash := PrevPos ('\', sCmd, iExtPos);

        if (iBackslash = 0) then
            exit;

        sExePath := Copy (sCmd, 1, iExtPos + 3);

        if (IsDpInstExe (LowerCase (sCmd))) then
            Result := false  // true
        else if (FileExists (sExePath)) then
            Result := IsConsoleApp (sExePath)
        else Result := false;
    end; { InspectExe }

    (* ---- *)

var
    sPath, sCmd : String;
    bCaptureOutput : Boolean;

begin { WinExecAndWait }
    sPath := ExtractPath (sCmdLine);

    if not (DirectoryExists (sPath)) then
    begin
        LogMsg (cDirMissingMsg, [sPath]);
        Result := false;
        exit;
    end; { if }

    if (Pos ('.bat', LowerCase (sCmdLine)) > 0) or
       (Pos ('.cmd', LowerCase (sCmdLine)) > 0) then
    begin
        bCaptureOutput := true;
        sCmd := GetSysNativePath (GetSystemDir) + cCmdExe + ' /c ' + sCmdLine;
    end { if }
    else if (Pos ('.msi', LowerCase (sCmdLine)) > 0) then
    begin
        bCaptureOutput := false;
        sCmd := GetSysNativePath (GetSystemDir) + cMsiExecExe + sCmdLine;
    end { if }
    else
    begin
        sCmd := sCmdLine;
        bCaptureOutput := IsCommandLineTool (LowerCase (sCmd));
    end; { else }

    LogVerbose (cExecutingCmdMsg, [TimeToStr (Now), sCmd]);

    if (bCaptureOutput) then
        Result := ConsoleOutput (sCmd, sPath)
    else Result := WinExecAndWait32 (sCmd, dwExitCode, sPath);
end; { WinExecAndWait }

(* ---- *)

function YesNoPrompt (const sMsg: String;
                      const bLogging: Boolean = false) : Boolean;

var
    chKey : Char;

begin
{$IFNDEF DELPHI_10_2_UP}
    Result := false;
{$ENDIF}

    Write (sMsg);

{$IFDEF FPC}
    repeat
        Read (chKey);
    until (CharInSet (UpCase (chKey), ['Y', 'N']));

    Result := UpCase (chKey) = 'Y';
{$ELSE}
    while (true) do
    begin
        chKey := ReadKey;

        case UpCase (chKey) of
            'Y' : Result := true;
            'N' : Result := false;
            else Continue;
        end; { case UpCase (chKey) of }

        Break;
    end; { while }
{$ENDIF}

    if (bLogging) then
        LogMsg ('');
end; { YesNoPrompt }

(* ---- *)

procedure InitVars;

var
    VI : TOSVersionInfoW;

begin
    VI.dwOSVersionInfoSize := SizeOf (TOSVersionInfoW);

    Win32Check (RtlGetVersion (VI) = STATUS_SUCCESS);

    uOS_MajorVer := VI.dwMajorVersion;
    uOS_MinorVer := VI.dwMinorVersion;
    uOS_BuildNumber := VI.dwBuildNumber;

    if (uOS_MajorVer = 6) and (uOS_MinorVer = 1) then
        sOS_ver := '7'
    else if (uOS_MajorVer = 6) and (uOS_MinorVer = 2) then
        sOS_ver := '8'
    else if (uOS_MajorVer = 6) and (uOS_MinorVer = 3) then
        sOS_ver := '81'
    else if (uOS_MajorVer = 10) then
    begin
        if (uOS_BuildNumber >= 22000) then
            sOS_ver := '11'
        else sOS_ver := '10'
    end; { else if }

    sAppName := ExtractFileName (ParamStr (0));
    SetLength (sAppName, Length (sAppName) - 4);
end; { InitVars }

(* ---- *)

initialization
begin
    InitVars;
end; { initialization }

(* ---- *)

end.

