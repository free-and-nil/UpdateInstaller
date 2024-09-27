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

{$IFNDEF DEBUG}
    {$IFDEF CPUX64}
        {$MESSAGE FATAL 'This program must be compiled with the 32 bit compiler!'}
    {$ENDIF}
{$ENDIF}

unit MainU;

interface

function Main (out bRebootRequired: Boolean) : Integer;

implementation

uses SysUtils, Contnrs, Classes, Windows,
     CheckParamsU, ProcessFilesU, GlobalsU, PackageClassU,
     LogToTempU, Win32ToolsU, Delphi_T, Delphi32ToolsU, PasTools, Wow64U,
     DebugHelperU, SMBIOS_U, WindowsDisplayName_WMI_U;

const
	cPackages = 'Packages';

ResourceString
    cBiosVerReadMsg = #13#10'%sBIOS version = %s; release date = %s; model = %s';
    cCheckingDependenciesMsg = 'checking dependencies';
    cCompatibleSystemsParamMsg =
              #13#10'Parameter "CompatibleModels" specified: %d model(s) added';
{$IFDEF DEBUG}
    cContinueMsg = 'Continue (y/n)? ';
{$ENDIF}
    cDatabaseFilesMsg = #13#10'Copying database XML files to target directory';
    cEnvironmentVarErrorMsg = 'Unable to resolve the environment variable "%s"';
    cFileNotFoundMsg = 'File "%s" not found';
    cFirmwareRequiresRestartMsg =
               'Firmware package installed; immediate system restart requested';
    cID_ListLoadedMsg = #13#10'%d package ID(s) loaded from "%s"';
    cIgnoringWindowsVersionChecksMsg = 'Ignoring Windows version checks';
    cIniDeleteErrMsg = 'Unable to delete the existing file "%s"';
    cIniReadErrMsg = 'Error reading package information from "%s"';
    cIniSaveErrMsg = 'Error saving package IDs to "%s"';
    cIniSaveSuccessMsg = '%d package ID(s) saved to "%s"';
    cInstalledMsg = #13#10'%d of %d package(s) installed';
    cInstallFailedMsg = '; %d failed:';
    cInstallingMsg = 'installing packages';
    cInstallingPackageMsg = #13#10'> Installing "%s" ...';
{$IFNDEF DEBUG}
    cIsNetworkDriveMsg = 'The data files must not reside on a network path';
{$ENDIF}
    cMovedToTheFrontMsg = #13#10'Moved "%s" to the front of the installation list';
    cMovedToTheEndMsg = #13#10'Moved "%s" to the end of the installation list';
    cNoLenovoSystemMsg = 'This computer is not a Lenovo system (manufacturer = "%s")';
    cPathChangedMsg = 'Path "%s" changed to "%s"';
    cPathContainsSpacesMsg = 'The path "%s" contains spaces; unable to continue';
    cParametersMsg = 'Parameters';
    cSkippingBiosUpdateMsg = 'Skipping BIOS update installation';
    cSkippingFirmwareInstallation = 'Skipping firmware package installation';
    cSkippingNormalPackagesMsg = 'Skipping installation of normal packages';
    cTargetFolderNotEmptyMsg = 'The folder "%s" is not empty; unable to continue';
    cTerminateMsg = 'and close the program';
    cStartTimeMsg = 'Start time: %s';
    cUnpackedMsg = #13#10'%d of %d package(s) unpacked';
    cUnpackingMsg = 'unpacking files';
    cUnpackingPackageMsg = 'Unpacking "%s"';
    cUnsupportedOperatingSystemMsg = 'Unsupported operating system';
{$IFNDEF DEBUG}
    cUsageWarningMsg = #13#10 +
        'This program is provided as is, expressly without a warranty of any kind.' +
        #13#10#13#10 +
        'Improper use of this program can lead to permanent damage to this computer.' +
        #13#10'You use it at your own risk. Continue (y/n)? ';
{$ENDIF}
    cXmlErrorSkippingPackageMsg = #13#10'XML error: skipping package "%s"';

const
    cChipsetPackage = ' chipset ';
    cHotkeyFeaturePackage = 'Hotkey Features Integration Package';
    cMonitorFilePackage = 'Monitor File';

    cResultDefault     = $FF;
    cResultWinVerError = $FE;
    cResultFile_IO_Err = $FD;

{$IFNDEF UNICODE}
type
    EDirectoryNotFoundException = class (Exception);
    EFileNotFoundException = class (Exception);
{$ENDIF}

(* ---- *)

procedure CheckEnvironmentVars;

    (* ---- *)

    procedure CheckVariable (const sName: String);
    begin
        Assert (sName <> '');

        if (GetEnvironmentVar (sName) = '') then
            raise Exception.CreateFmt (cEnvironmentVarErrorMsg, [sName]);
    end; { CheckVariable }

    (* ---- *)

begin { CheckEnvironmentVars }
    CheckVariable ('ProgramFiles');

    if (IsWow64) then
        CheckVariable ('ProgramW6432');
end; { CheckEnvironmentVars }

(* ---- *)

function DB_XmlPathEqualsWorkDir (const sDatabaseXml, sFolder: String) : Boolean;

var
    sDB_Path : String;

begin
    Assert (sDatabaseXml <> '');
    Assert (sFolder <> '');

    sDB_Path := LowerCase (ExtractFilePath (sDatabaseXml));

    Result := sDB_Path = IncludeTrailingBackslash (LowerCase (sFolder));
end; { DB_XmlPathEqualsWorkDir }

(* ---- *)

function IsTargetFolderEmpty (const sPath: String) : Boolean;
begin
    if (DirectoryIsEmpty (sPath, false)) then
        Result := true
    else
    begin
        Result := false;
        LogMsg (cTargetFolderNotEmptyMsg, [sPath]);
    end; { else }
end; { IsTargetFolderEmpty }

(* ---- *)

function SavePackageListToIni (const PackageList: TObjectList) : Boolean;

var
	ID_List : TStringList;
    iIndex, iCount : Integer;
    sIniFile : String;

begin
	Assert (Assigned (PackageList));

    iCount := 0;
    sIniFile := GetIniFileName;

    if (FileExists (sIniFile)) then
        Result := SysUtils.DeleteFile (sIniFile)
    else Result := true;

    if (Result) then
    begin
        ID_List := TStringList.Create;

        try
            for iIndex := 0 to PackageList.Count - 1 do
                with TPackage (PackageList [iIndex]) do
                    if (IsInstalled = false) and (TargetPresent = crTrue) then
                        ID_List.Add (ID);

            Result := SaveListToIni (sIniFile, cPackages, ID_List);
            iCount := ID_List.Count;

        finally
            ID_List.Free;
        end; { try / finally }
    end; { if }

    if (Result) then
        LogMsg (cIniSaveSuccessMsg, [iCount, sIniFile])
    else LogMsg (cIniSaveErrMsg, [sIniFile]);
end; { SavePackageListToIni }

(* ---- *)

procedure VerifyParams (const sDatabaseXml, sLogFolder, sFolder: String);

    (* ---- *)

    procedure CheckForFolder (const sFolder: String);
    begin
        Assert (sFolder <> '');

        if (sFolder <> '') then
            if not (DirectoryExists (sFolder)) then
                if not (ForceDirectories (sFolder)) then
                    raise EDirectoryNotFoundException.CreateFmt (cDirMissingMsg,
                                                                 [sFolder]);
    end; { CheckForFolder }

    (* ---- *)

begin { VerifyParams }
    Assert (sDatabaseXml <> '');
    Assert (sLogFolder <> '');

    if not (FileExists (sDatabaseXml)) then
        raise EFileNotFoundException.CreateFmt (cFileNotFoundMsg,
                                                [sDatabaseXml]);

    CheckForFolder (sLogFolder);

    if (sFolder <> '') then
        CheckForFolder (sFolder);
end; { VerifyParams }

(* ---- *)

procedure WaitForKeyPress (const sMsg: String);
begin
    WriteLn;
    Write (Format ('Press [Enter] to continue %s...',
                   [iif (sMsg = '', '', sMsg + ' ')]));
    ReadLn;
end; { WaitForKeyPress }

(* ---- *)

function Main (out bRebootRequired: Boolean) : Integer;

    (* ---- *)

    function CheckPaths (const sDatabaseXml, sFolder: String) : Boolean;

        (* ---- *)

        function IsNetworkPath (const sPath: String) : Boolean;
        begin
            Assert (sPath <> '');

			Result := false;

{$IFNDEF DEBUG}
            if (Pos ('\\', sPath) = 1) or
               (GetDriveType (sPath [1]) = dtRemote) then
            begin
                Result := true;
                LogMsg (cIsNetworkDriveMsg);
            end; { if }
{$ENDIF}
        end; { IsNetworkPath }

        (* ---- *)

        function PathContainsSpaces (const sPath: String;
                                     const bFileName: Boolean = false) : Boolean;
        begin
            Assert (sPath <> '');

            if (Pos (' ',
                     iif (bFileName, ExtractFileDir (sPath), sPath)) > 0) then
            begin
                Result := true;
                LogMsg (cPathContainsSpacesMsg, [sPath]);
            end { if }
            else Result := false;
        end; { PathContainsSpaces }

        (* ---- *)

    begin { CheckPaths }
        Assert (sDatabaseXml <> '');

        if ((sFolder <> '') and (PathContainsSpaces (sFolder))) or
           (PathContainsSpaces (sDatabaseXml, true)) or
           ((Action <> aUnpack) and (IsNetworkPath (sDatabaseXml))) then
            Result := false
        else Result := true;
    end; { CheckPaths }

    (* ---- *)

    procedure ExpandPaths (var sDatabaseXml, sLogFolder, sFolder: String);

        (* ---- *)

        function IsRelativePath (const sPath: String) : Boolean;
        begin
            if (Pos ('\', sPath) = 0) then
                Result := true
            else
                if (Length (sPath) > 1) then
                    Result := sPath [2] <> ':'
                else Result := true;
        end; { ContainsDriveLetter }

        (* ---- *)

        function ChangePath (const sPath: String) : String;
        begin
            Result := ExpandFileName (sPath);
            LogMsg (cPathChangedMsg, [sPath, Result]);
        end; { ChangePath }

        (* ---- *)

    begin { ExpandPaths }
        if (IsRelativePath (sDatabaseXml)) then
            sDatabaseXml := ChangePath (sDatabaseXml);

        if (IsRelativePath (sLogFolder)) then
            sLogFolder := ChangePath (sLogFolder);

        if (sFolder <> '') then
            if (IsRelativePath (sFolder)) then
                sFolder := ChangePath (sFolder);
    end; { ExpandPaths }

    (* ---- *)

    function Fill_ID_List_From_IniFile (const ID_List: TStringList) : Boolean;

    var
        sIniFile : String;

    begin
    	Assert (Assigned (ID_List));

    	Result := false;

        sIniFile := GetIniFileName;

		if not (FileExists (sIniFile)) then
        begin
        	LogMsg (cFileNotFoundMsg, [sIniFile]);
			exit;
        end; { if }

        if (LoadListFromIni (sIniFile, cPackages, ID_List)) then
        begin
            Result := true;
            LogMsg (cID_ListLoadedMsg, [ID_List.Count, sIniFile]);
        end { if }
        else LogMsg (cIniReadErrMsg, [sIniFile]);
    end; { Fill_ID_List_From_IniFile }

    (* ---- *)

    procedure FreeList (var List: TObjectList);

    var
        iIndex : Integer;

    begin
        for iIndex := 0 to List.Count - 1 do
            TPackage (List [iIndex]).Free;

        FreeAndNil (List);
    end; { FreeList }

    (* ---- *)

    procedure GetBiosInformation (out bLenovoSystem: Boolean);

    var
        sVersion, sModel, sSystemName, sManufacturer : String;
        uMajor, uMinor : UInt;
        sReleaseDate : String;
        SMBios : TSMBios;
        SysInfo : TSystemInformation;
        BiosInfo : TBiosInformation;

    begin
(**
{$IFNDEF FPC}
        Win32Check (CoInitialize (NIL) = S_OK);
{$ENDIF}

        try
            if (Get_BiosInfo (sVersion, sSerial, uReleaseDate)) and
               (Get_PC_Info (sManufacturer, sModel, sSystemName)) then
            begin
                LogVerbose (cBiosVerReadMsg,
                            [sSystemName, sVersion, uReleaseDate, sModel]);
                SetBiosInfo (sSystemName, sModel, sVersion);

                if (IsWindows10) then
                begin
                    Get_BIOS_InfoU.GetEmbeddedControllerVersion (uMajor, uMinor);
                    SetEmbeddedControllerVersion (uMajor, uMinor);
                end; { if }
            end { if }
            else raise Exception.Create (cBiosVerReadErrMsg);

        finally
{$IFNDEF FPC}
            CoUninitialize;
{$ENDIF}
**)
        SMBios := TSMBios.Create;

        try
            SysInfo := SMBios.SysInfo;
            sManufacturer := TrimRight (String (SysInfo.ManufacturerStr));
            sModel := TrimRight (String (SysInfo.ProductNameStr));
            sSystemName := TrimRight (String (SysInfo.VersionStr));

            BiosInfo := SMBios.BIOSInfo;
            sVersion := TrimRight (String (BiosInfo.VersionStr));
            sReleaseDate := TrimRight (String (BiosInfo.ReleaseDateStr));

            LogVerbose (cBiosVerReadMsg,
                        [iif (sSystemName <> '', sSystemName + '; ', ''),
                         sVersion, sReleaseDate, sModel]);
            SetBiosInfo (sManufacturer, sSystemName, sModel, sVersion);

            uMajor :=
             BiosInfo.RAWBiosInformation.EmbeddedControllerFirmwareMajorRelease;
            uMinor :=
             BiosInfo.RAWBiosInformation.EmbeddedControllerFirmwareMinorRelease;

            SetEmbeddedControllerVersion (uMajor, uMinor);

        finally
            SMBios.Free;
        end; { try / finally }

        bLenovoSystem := LowerCase (sManufacturer) = 'lenovo';
    end; { GetBiosInformation }

    (* ---- *)

    function ProcessExtractFilesCmd (const PackageList: TObjectList;
                                     const sFolder: String) : Boolean;

    var
        Package : TPackage;
        iIndex, iCount : Integer;

    begin
        Assert (sFolder <> '');

        Result := true;
        iCount := 0;

        for iIndex := 0 to PackageList.Count - 1 do
        begin
            Package := TPackage (PackageList [iIndex]);

            if ((Package.ExtractCommand <> '') and
                (Package.IsInstalled = false)) or
               (Action = aUnpack) then
                if not (Package.XmlError) then
                begin
                    LogMsg (#13#10 + cUnpackingPackageMsg +
                            iif (Action = aUnpack,
                                 Format (' (%d/%d)',
                                         [iIndex + 1, PackageList.Count]), ''),
                            [Package.Description]);

                    if (UnpackFiles (Package, sFolder)) then
                        Inc (iCount)
                    else
                    begin
                        Result := false;
                        Break;
                    end; { else }
               end { if }
               else LogMsg (cXmlErrorSkippingPackageMsg, [Package.Description]);
        end; { for }

        LogMsg (cUnpackedMsg, [iCount, PackageList.Count]);
    end; { ProcessExtractFilesCmd }

    (* ---- *)

    function ProcessInstallFilesCmd (const PackageList: TObjectList;
                                     const sFolder: String;
                                     const bFilesUnpacked: Boolean) : Integer;

        (* ---- *)

        function ContinueExecution : Boolean;

        var
            chKey : Char;

        begin
            WriteLn;
            Write ('Continue Y/N ? ');
            Read (chKey);

            Result := UpCase (chKey) = 'Y';
            Flush (System.Input);
        end; { ContinueExecution }

        (* ---- *)

        procedure CheckForChipsetPackage;

        var
            iIndex : Integer;
            Package : TPackage;

        begin
            for iIndex := 0 to PackageList.Count - 1 do
            begin
                Package := TPackage (PackageList [iIndex]);

                if (Pos (cChipsetPackage,
                         LowerCase (Package.Description)) > 0) and
                   (Package.IsInstalled = false) then
                begin
                    PackageList.Delete (iIndex);
                    PackageList.Insert (0, Package);
                    LogVerbose (cMovedToTheFrontMsg, [Package.Description]);
                    Break;
                end; { if }
            end; { if }
        end; { CheckForChipsetPackage }

        (* ---- *)

        procedure MovePackagesToEndOfList;

        var
            iIndex, iCount : Integer;
            Package : TPackage;
            DeleteList : TList;

        begin
            iCount := PackageList.Count - 1;

            DeleteList := TList.Create;

            try
                for iIndex := 0 to iCount do
                begin
                    Package := TPackage (PackageList [iIndex]);

                    if ((Pos (cHotkeyFeaturePackage, Package.Description) > 0) or
                        (Pos (cMonitorFilePackage, Package.Description) > 0)) and
                       (Package.IsInstalled = false) and
                       (Package.TargetPresent = crTrue) then
                    begin
                        DeleteList.Add ({%H-}Pointer (iIndex));
                        PackageList.Add (Package);
                        LogVerbose (cMovedToTheEndMsg, [Package.Description]);
                    end; { if }
                end; { if }

                if (DeleteList.Count > 0) then
                    for iIndex := DeleteList.Count - 1 downto 0 do
                        PackageList.Delete ({%H-}Integer (DeleteList [iIndex]));

            finally
                DeleteList.Free;
            end; { try / finally }
        end; { MovePackagesToEndOfList }

        (* ---- *)

    var
        Package : TPackage;
        iIndex, iCount, iError : Integer;
        crResult : TCheckResult;
        FailedList : TStringList;

    begin { ProcessInstallFilesCmd }
        Assert (sFolder <> '');

        Result := 0;
        iCount := 0;
        iError := 0;

        CheckForChipsetPackage;
        MovePackagesToEndOfList;

        FailedList := CreateSortedStringList;

        try
            for iIndex := 0 to PackageList.Count - 1 do
            begin
                Package := TPackage (PackageList [iIndex]);

                if (Package.IsInstalled = false) and
                   (Package.TargetPresent = crTrue) then
                begin
                    LogMsg (cInstallingPackageMsg, [Package.Description]);

                    if (Package.PackageType = ptSystemBios) and
                       (UpdateBios = false) then
                    begin
                        LogMsg (cSkippingBiosUpdateMsg);
                        Continue;
                    end { if }
                    else if (Package.PackageType = ptFirmware) and
                            ((UpdateFirmware = false) or UpdateBios) then
                    begin
                        LogMsg (cSkippingFirmwareInstallation);
                        Continue;
                    end { if }
                    else if (Package.PackageType <> ptFirmware) and
                            (Package.PackageType <> ptSystemBios) and
                            (UpdateFirmware or UpdateBios) then
                    begin
                        LogMsg (cSkippingNormalPackagesMsg);
                        Continue;
                    end; { if }

                    Inc (iCount);

                    crResult := InstallPackage (Package, sFolder,
                                                bFilesUnpacked);

                    case crResult of
                        crTrue :
                            begin
                                Inc (Result);

                                if (Package.RebootRequired) then
                                begin
                                    bRebootRequired := true;

                                    if (UpdateFirmware and not UpdateAll) or
                                       (UpdateBios) then
                                    begin
                                        LogVerbose (cFirmwareRequiresRestartMsg);
                                        Break;
                                    end; { if }
                                end; { if }
                            end; { case crTrue }

                        crFalse :
                            begin
                                Inc (iError);
                                FailedList.Add (Package.Description);
                            end;  { case crFalse }
                    end; { case crResult of }
                end; { if }
            end; { for }

            LogMsg (cInstalledMsg +
                    iif (iError > 0, Format (cInstallFailedMsg, [iError]), ''),
                    [Result, iCount]);

            if (iError > 0) then
                for iIndex := 0 to FailedList.Count - 1 do
                    LogMsg ('> %s', [FailedList [iIndex]]);

        finally
            FailedList.Free;
        end; { try / finally }
    end; { ProcessInstallFilesCmdl }

    (* ---- *)

    procedure SaveResultsToIni (const PackageList: TObjectList);

    var
        iIndex : Integer;
        ResultList : TStringList;

    begin
        ResultList := TStringList.Create;

        try
            for iIndex := 0 to PackageList.Count - 1 do
                with TPackage (PackageList [iIndex]) do
                    if (InstallResult = crTrue) then
                        ResultList.Add (ID);

            SaveListToIni (GetIniFileName, cSuccessMsg, ResultList);

            for iIndex := 0 to PackageList.Count - 1 do
                with TPackage (PackageList [iIndex]) do
                    if (InstallResult = crFalse) then
                        ResultList.Add (ID);

            SaveListToIni (GetIniFileName, cFailureMsg, ResultList);

        finally
            ResultList.Free;
        end; { try / finally }
    end; { SaveResultsToIni }

    (* ---- *)

    procedure ShowStartMsg (const sMsg: String);
    begin
        Assert (sMsg <> '');

        LogVerbose (#13#10 + '---> ' + UpCase (sMsg [1]) +
                    Copy (sMsg, 2, Length (sMsg) - 1) + ' <---');
    end; { ShowStartMsg }

    (* ---- *)

    procedure WaitForRemoteDebugger;
    begin
        if not (IsDebuggerPresent) then
        begin
            Write (#13#10'Attach the Remote Debugger now ...');
            ReadLn;
        end; { if }
    end; { WaitForRemoteDebugger; }

    (* ---- *)

var
    sDatabaseXml, sFolder, sLogFolder : String;
    bReboot, bLenovoSystem, bFilesUnpacked : Boolean;
    PackageList : TObjectList;
    bContinue : Boolean;
    ID_List : TStringList;
    uInstallationCount : UInt;

begin { Main }
    bRebootRequired := false;
    DisableLogFunction;
    LogMsg (GetParams);
    EnableLogFunction;

    Result := cResultDefault;
    ID_List := NIL;

    if (ParamCount = 0) or
       ((ParamCount = 1) and (Pos ('?', ParamStr (1)) > 0)) then
    begin
        ShowParams;
        exit;
    end; { if }

{$IFDEF REMOTEDEBUGGER}
    WaitForRemoteDebugger;
{$ENDIF}

    if not (CheckWin32Version (6, 1)) then
    begin
        Result := cResultWinVerError;
        LogMsg (cUnsupportedOperatingSystemMsg);
        exit;
    end; { if }

    try
        ProcessParams (sDatabaseXml, sFolder, sLogFolder, bReboot);
        ExpandPaths (sDatabaseXml, sLogFolder, sFolder);
        VerifyParams (sDatabaseXml, sLogFolder, sFolder);

        SetGlobalLogFolder (sLogFolder);
        SetUpdateInstallerLogFileName;

        CheckEnvironmentVars;

        LogMsg (#13#10'%s v%s' {$IFDEF CPUX64} + ' x64' {$ENDIF}
        		{$IFDEF DEBUG} + ' [DEBUG]' {$ENDIF} + #13#10,
                [AppName, GetVersionInfo]);

        LogVerbose (cStartTimeMsg, [DateTimeToStr (Now)]);

        LogVerbose (#13#10'%s: ' + GetParams + #13#10, [cParametersMsg]);
        LogVerbose (GetWindowsInfo);

{$IFNDEF DEBUG}
        if (Action <> aUnpack) then
            if (IgnoreWarning = false) then
                if not (YesNoPrompt (cUsageWarningMsg, true)) then
                    exit;
{$ENDIF}

		if (Action <> aCheck) then
        	bFilesUnpacked := DB_XmlPathEqualsWorkDir (sDatabaseXml, sFolder)
        else bFilesUnpacked := false;

        if not (CheckPaths (sDatabaseXml, sFolder)) then
            exit;

        if (Action <> aCheck) then
            if (bFilesUnpacked = false) and
               (IsTargetFolderEmpty (sFolder) = false) and
               (IgnoreExistingFolders = false) then
                exit;

        GetBiosInformation (bLenovoSystem);

        if (bLenovoSystem = false) and (Action <> aUnpack) then
        begin
			LogMsg (#13#10 + cNoLenovoSystemMsg, [SystemManufacturer]);

{$IFDEF DEBUG}
            if (YesNoPrompt (cContinueMsg)) then
            begin
                SetIgnoreSystemCompatibility;
                WriteLn;
            end { if }
            else
{$ENDIF}
            exit;
        end; { if }

        Result := 1;

        if (IgnoreWindowsCompatibility) then
            LogMsg (cIgnoringWindowsVersionChecksMsg);

        if (CheckForParam (cCompatibleSystems)) then
            LogVerbose (cCompatibleSystemsParamMsg, [CompatibleSystemsCount]);

        PackageList := TObjectList.Create;

        // "true" releases memory if entries get moved
        PackageList.OwnsObjects := false;

        try
            ShowStartMsg (cCheckingDependenciesMsg);

            if (GetIniFileName <> '') then
            	if (Action = aCheck) then
            	begin
                    if (FileExists (GetIniFileName)) then
                        if not (SysUtils.DeleteFile (GetIniFileName)) then
                        begin
                            LogMsg (cIniDeleteErrMsg, [GetIniFileName]);
                            Result := cResultFile_IO_Err;
                            exit;
                        end; { if }
                end { if }
                else
                begin
	            	ID_List := CreateSortedStringList (dupIgnore, true);

					if not (Fill_ID_List_From_IniFile (ID_List)) then
                		exit;
                end; { else }

            if (ProcessXmlFiles (sDatabaseXml, PackageList, uInstallationCount,
                                 ID_List)) then
                if (Action = aCheck) then
                begin
                    Result := uInstallationCount;

                    if (GetIniFileName <> '') then
                    	if not (SavePackageListToIni (PackageList)) then
                        	Result := cResultFile_IO_Err;
                end { if }
                else
                begin
                    if (bFilesUnpacked = false) then
                    begin
                        if (DebugMode) then
                            WaitForKeyPress (cUnpackingMsg);

                        if (Action = aUnpack) then
                        begin
                            ShowStartMsg (cUnpackingMsg);
                            LogMsg (cDatabaseFilesMsg);
                            CopyFiles (ExtractFilePath (sDatabaseXml) + '*.*',
                                       sFolder, true);
                        end; { if }

                        bContinue := ProcessExtractFilesCmd (PackageList,
                                                             sFolder);
                    end { if }
                    else bContinue := true;

                    if (bContinue) then
                        if (Action = aInstall) then
                        begin
                            if (DebugMode) then
                                WaitForKeyPress (cInstallingMsg);

                            ShowStartMsg (cInstallingMsg);
                            Result := ProcessInstallFilesCmd (PackageList,
                                                              sFolder,
                                                              bFilesUnpacked);

                            if (GetIniFileName <> '') then
                                SaveResultsToIni (PackageList);
                        end { if }
                        else Result := 0
                end; { else }

            if (DebugMode) and (DebuggerPresent = false) then
                WaitForKeyPress (cTerminateMsg);

        finally
            FreeList (PackageList);
            ID_List.Free;
        end; { try / finally }

    except
        on E: Exception do
            LogMsg ('%s: %s', [E.ClassName, E.Message]);
    end; { try / except }
end; { Main }

(* ---- *)

end.
