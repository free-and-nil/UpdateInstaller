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

unit ProcessFilesU;

interface

uses Contnrs, Classes, Windows,
     PackageClassU, ProcessDataBaseXmlU, GlobalsU;

function InstallPackage (const Package: TPackage;
                         const sWorkFolder: String;
                         const bFilesUnpacked: Boolean) : TCheckResult;

function ProcessXmlFiles (const sDatabaseXml: String;
                          const PackageList: TObjectList;
                          out uInstallationCount: UInt;
                          const ID_List: TStringList = NIL) : Boolean;

function UnpackFiles (const Package: TPackage;
                      const sParentFolder: String) : Boolean;

implementation

uses SysUtils, StrUtils, Types,
     LogToTempU, DeviceViewerU, PasTools, Win32ToolsU, Delphi_T, Wow64U,
     StrUtilsHelperU;

ResourceString
    cBatchFileModifiedMsg = 'Batch file "%s": PAUSE command removed';
    cBiosUpdateParamsRemovedMsg = 'BIOS update: changed parameters to "%s"';
    cCreateFolderErrMsg = 'Unable to create the folder "%s"';
    cDevicesRetrievingMsg = 'Retrieving installed devices ...';
    cDoneMsg = ' done';
    cDpInstParamAddedMsg =
       'Added parameter "/C" ("dump output to console") to "dpinst.exe" params';
    cExecutePromptMsg = 'Execute the command: %s [y/n]?';
    cFileChangedErrMsg = 'Error "%s" modifying file "%s": %s';
    cFileCopyErrMsg = 'Error copying "%s" to "%s": %s';
    cFilesCopiedMsg = '%d file(s) copied';
    cFlash_x64_CmdFoundMsg = 'x64 BIOS flash batch file "%s" found';
    cIgnoringExistingFolderMsg = '; continuing without unpacking any files';
    cInstallationCheckErrMsg =
      'Cannot perform installation check; unable to process the path'#13#10'%s';
    cInstallationFileMissingMsg =
               			   'Installer "%s" not found in the target folder "%s"';
    cInstallCmdEqualsExtractCmdMsg =
                       'The installation command equals the extraction command';
    cInstallUsingPnpUtilMsg = 'Performing installation using "pnputil.exe"';
    cInstallUsingSetupCmdMsg = 'Performing installation using the file "%s"';
//  cIntel_ME_UpdateExeFoundMsg = 'Intel ME Firmware update EXE found';
    cMoreThanOneBatchFileFoundMsg = '%d batch files found in "%s"';
    cMoveFileErrMsg = 'Error moving the installation file "%s" to "%s%s": %s';
    cMoveFileSuccessMsg = 'Installation file "%s" moved to "%s%s"';
    cNoExeFileMsg = 'The unpacking command "%s" does not contain an .EXE name';
    cNoPackagesFoundMsg = #13#10'No packages to install found';
    cOnePackageFoundMsg = '1 package found:'#13#10;
    cPackageIgnoredCountMsg = '%d package(s) ignored ("%s")'#13#10;
    cPackageNotFoundMsg = 'Package "%s" not found; exiting program';
    cPackagePathMissingMsg = 'Variable "%PackagePath%" missing from "ExtractCommand"';
    cPackageProcessingErrMsg = 'Error processing package:'#13#10'%s'#13#10;
    cPackagesFoundMsg = 'Packages found: %d'#13#10;
    cPackageToIgnoreFoundMsg = 'Skipping package "%s"';
    cPackageXmlNotFoundMsg = 'Package file "%s" not found';
    cProcessedMsg = #13#10'%d of %d packages processed';
    cProcessedErrMsg = ' (%d skipped due to error)';
    cProcessing_DB_Msg = 'Processing database file "%s"'#13#10;
    cRebootRequestedMsg = '; system reboot requested';
//    cRebootTypeUnsupportedMsg = 'Reboot type %d unsupported; skipping package';

    cSccmParamToFlashCmdAddedMsg = 'Parameter "/sccm" added to BIOS flash command';
    cSkippedBecauseMsg = '%d package(s) skipped because %s:';
    cSkippedPackgesMsg = #13#10'Skipped packages: %d';
    cSkipReasonNotCheckedMsg = 'of a package type mismatch';
    cSkipReasonPrerequisitesMsg = 'the prerequisites check failed';
    cSkipReasonDetectInstallMsg = 'the installation check failed';
    cSsdCmdSilentParamAddedMsg =
                          'Parameter "/A" added to SSD firmware update command';
    cSupportedBiosParamFoundMsg = 'Supported BIOS update parameter "%s" found';
    cTargetFolderExistsMsg = 'The target folder "%s" already exists';
    cToBeInstalledCountMsg = '; %d must be installed';
    cTooManyBiosUpdateFilesMsg =
          'Failure: more than one BIOS update packages marked for installation';
    cUnableToUnpackMsg = ': Unable to unpack the files';
//    cUnknownExtMsg = '"%s": Unknown file extension';
    cUnpackCmdMissingMsg =
        'The "<ExtractCommand>" section in the package file is empty';
    cThinkPadBiosUpdateErrMsg = 'ThinkPad BIOS update: too many parameters [%s]';
    cThinkPadBiosUpdateParamErrMsg =
        'ThinkPad BIOS update: Unknown parameter [%s])';
    cUnsupportedBiosParamFoundMsg = 'Unsupported BIOS update parameter "%s" found';

const
    cPackagePath = '%PACKAGEPATH%';
    cPnpUtilParams = ' -i -a ';
    cPnpUtilParamsWin10 = ' /install /subdirs /add-driver ';

(* ---- *)

function ExecuteCmd (const sCmd: String; out dwExitCode: DWord) : Boolean;
begin
    if (PromptForExecution) then
        if not (YesNoPrompt (Format (cExecutePromptMsg, [sCmd]), true)) then
        begin
            Result := false;
            exit;
        end; { if }

    Result := WinExecAndWait (sCmd, dwExitCode);

    if (Result) then
        LogVerbose (cProcessReturnCodeMsg, [TimeToStr (Now), dwExitCode])
    else LogVerbose (cProcessExecutionErrMsg, [GetLastErrorMsg]);
end; { ExecuteCmd }

(* ---- *)

function GetInstallationCount (const PackageList: TObjectList;
                               const PT: TPackageType = ptNoPackageType) : UInt;

var
    iIndex : Integer;
    Package : TPackage;

begin
    Result := 0;

    for iIndex := 0 to PackageList.Count - 1 do
    begin
        Package := TPackage (PackageList [iIndex]);

        if (Package.TargetPresent = crTrue) and
           (Package.IsInstalled = false) then
            if (PT = ptNoPackageType) then
                Inc (Result)
            else if (PT = Package.PackageType) then
                Inc (Result);
    end; { for }
end; { GetInstallationCount }

(* ---- *)

function InstallPackage (const Package: TPackage;
                         const sWorkFolder: String;
                         const bFilesUnpacked: Boolean) : TCheckResult;

const
    cBiosFlashCmd = 'flash.cmd';
    cSsdFlashCmd = 'fwwinsd.exe';
    cSccmParam = '/sccm';
    cWinUpTpExe = 'winuptp.exe';
    cWinUpTp64Exe = 'winuptp64.exe';
    cMeUpdateCmd = 'meupdate.cmd';

    (* ---- *)

    procedure CheckBiosFlashCmdParams (var sCmd: String;
                                       const sPackagePath : String);

        (* ---- *)

        function Find_Flash_x64_Cmd (out sFlashCmd: String) : Boolean;

        var
            FileList : TStringList;
            iIndex : Integer;

        begin
            Result := false;

            FileList := TStringList.Create;

            try
                FillFileList (FileList, sPackagePath + '\flash*.cmd',
                              false, false);

                if (FileList.Count > 0) then
                    for iIndex := 0 to FileList.Count - 1 do
                        if (Pos ('64', FileList [iIndex]) > 0) then
                        begin
                            Result := true;
                            sFlashCmd := FileList [iIndex];
                            LogVerbose (cFlash_x64_CmdFoundMsg, [sFlashCmd]);
                            Break;
                        end; { if }

            finally
                FileList.Free;
            end; { try / finally }
        end; { Find_Flash_x64_Cmd }

        (* ---- *)

    var
        sFlash_x64_Cmd : String;
        iPos : Integer;

    begin { CheckBiosFlashCmdParams }
        if (Pos (cSccmParam, LowerCase (sCmd)) = 0) then
        begin
            sCmd := sCmd + ' ' + cSccmParam;
            LogVerbose (cSccmParamToFlashCmdAddedMsg);
        end; { if }

        if (IsWindows_x64) and (Find_Flash_x64_Cmd (sFlash_x64_Cmd)) then
        begin
            iPos := Pos (cBiosFlashCmd, LowerCase (sCmd));

            if (iPos > 0) then
            begin
                Delete (sCmd, iPos, Length (cBiosFlashCmd));
                Insert (sFlash_x64_Cmd, sCmd, iPos);
            end; { if }
        end; { if }
    end; { CheckBiosFlashCmdParams }

    (* ---- *)

    function CheckThinkPadBiosUpdateParam (var sCmd: String;
                                           const iExePos: Integer) : Boolean;
    // https://thinkdeploy.blogspot.com/2017/11/tpm-firmware-update-utility.html

        (* ---- *)

        function GetParams (const sStr: String;
                            var asStr: TStringDynArray) : Boolean;

        begin
            asStr := SplitString (sStr, ' ');

            Result := asStr <> NIL;
        end; { OnlyOneParam }

        (* ---- *)

    const
        cExeExt = '.exe ';
        cSupportedParams = 1;
        cKnowParamsHigh = 8;
        asKnownParams : array [1..cKnowParamsHigh] of String =
            (('-s'), ('/s'),     // Silent, no restart
             ('-r'), ('/r'),     // Interactive with restart
             ('-f'), ('/f'),     // Force update even if running on battery
             ('-sr'), ('/sr'));  // Silent with restart

    var
        sParams : String;
        iStart, iParam, iKnownParam, iRemovedParams : Integer;
        asParams : TStringDynArray {$IFDEF FPC} = NIL {$ENDIF};
        bFound : Boolean;

    begin { CheckThinkPadBiosUpdateParam }
        Assert (sCmd <> '');

        Result := false;
        sParams := '';
        bFound := false;

        iStart := PosEx (cExeExt, sCmd, iExePos) + Length (cExeExt);

        if not (GetParams (Copy (LowerCase (TrimRight (sCmd)), iStart,
                                 Length (sCmd) - Pred (iStart)), asParams)) then
        begin
            LogMsg (cThinkPadBiosUpdateErrMsg, [ExtractFileName (sCmd)]);
            exit;
        end; { if }

        iRemovedParams := 0;

        for iParam := 0 to High (asParams) do
        begin
            bFound := false;

            for iKnownParam := 1 to cKnowParamsHigh do
                if (asParams [iParam] = asKnownParams [iKnownParam]) then
                begin
                    bFound := true;

                    if (iKnownParam <= cSupportedParams) then
                    begin
                        LogMsg (cSupportedBiosParamFoundMsg, [asParams [iParam]]);
                        sParams := sParams + ' ' + asKnownParams [iKnownParam];
                    end { if }
                    else
                    begin
                        LogMsg (cUnsupportedBiosParamFoundMsg,
                                [asParams [iParam]]);
                        Inc (iRemovedParams);
                    end; { else }

                    Break;
                end; { if }

            if not (bFound) then
            begin
                LogMsg (cThinkPadBiosUpdateParamErrMsg, [asParams [iParam]]);
                Break;
            end; { if }
        end; { for }

        if (bFound) then
        begin
            Result := true;

            if (sParams = '') then
                sParams := ' ' + asKnownParams [1];

            SetLength (sCmd, Pos (cExeExt, sCmd) + 3);
            sCmd := sCmd + sParams;

            if (iRemovedParams > 0) then
                LogMsg (cBiosUpdateParamsRemovedMsg, [TrimLeft (sParams)]);
        end; { if }
    end; { CheckThinkPadBiosUpdateParam }

    (* ---- *)

    function GetPackagePath : String;

    var
        asPaths : TStringDynArray;

    begin
        asPaths := SplitString (Package.PackageXml, '\');

        Result := IncludeTrailingBackslash (sWorkFolder) +
                  asPaths [Pred (High (asPaths))] +
                  iif (bFilesUnpacked, cFilesDir, '');
    end; { GetPackagePath }

    (* ---- *)

    function InstallCmdEqualsExtractCmd : Boolean;

    var
        sExtractCmd : String;
        iPos : Integer;

    begin
        iPos := Pos ('.', Package.ExtractCommand);
        sExtractCmd := LowerCase (Copy (Package.ExtractCommand, 1, iPos + 3));

        iPos :=  LastPos ('\', sExtractCmd);

        if (iPos > 0) then
            Delete (sExtractCmd, 1, iPos);

        if (Pos (sExtractCmd, LowerCase (Package.InstallCmd)) > 0) then
        begin
            Result := true;
            LogMsg (cInstallCmdEqualsExtractCmdMsg);
        end { if }
        else Result := false;
    end; { InstallCmdEqualsExtractCmd }

    (* ---- *)

    function InstallInfFile (const sPackagePath: String) : Boolean;

    var
        sCmd : String;
        dwReturnCode : DWord;

    begin
        Assert (sPackagePath <> '');

        Result := false;

        sCmd := GetSysNativePath (GetSystemDir) + cPnpUtilExe + cPnpUtilParams +
                iif (Pos ('%', Package.InstallCmd) > 0, Package.InstallCmd,
                     sPackagePath + '\' + Package.InstallCmd);

        sCmd := ReplaceStr (sCmd, cPackagePath, sPackagePath);

        if (ExecuteCmd (sCmd, dwReturnCode)) then
        begin
            Package.InstallResultValues := dwReturnCode;

            // https://docs.microsoft.com/en-us/windows-hardware/drivers/devtest/pnputil-return-values
            if (ValueInArray (dwReturnCode, TaDWord.Create (0, 3010))) then
            begin
                Result := true;
                Package.RebootRequired := dwReturnCode = 3010;
            end; { if }
        end; { if }
    end; { InstallInfFile }

    (* ---- *)

    function RunProgram (const sPackagePath: String) : Boolean;

        (* ---- *)

        function FindBatchFile (out sFile: String;
                                const sFileName: String = '') : Boolean;

        var
            FileList : TStringList;

        begin
            Result := false;

            FileList := TStringList.Create;

            try
                if (sFileName <> '') then
                    FillFileList (FileList, sPackagePath + '\' + sFileName,
                                  false, false)
                else
                begin
                    FillFileList (FileList, sPackagePath + '\*.cmd',
                                  false, false);

                    if (FileList.Count = 0) then
                        FillFileList (FileList, sPackagePath + '\*.bat', false,
                                      false);
                end; { if }

                if (FileList.Count > 0) then
                begin
                    Result := true;

                    if (FileList.Count > 1) then
                        LogVerbose (cMoreThanOneBatchFileFoundMsg,
                                    [FileList.Count, sPackagePath]);

                    sFile := FileList [FileList.Count - 1];
                end; { if }

            finally
                FileList.Free;
            end; { try / finally }
        end; { FindBatchFile }

        (* ---- *)

        function Find_INF_Files : Boolean;

        var
            FileList : TStringList;

        begin
            Result := false;

            FileList := TStringList.Create;

            try
                FillFileList (FileList, sPackagePath + '\*.inf',
                              IsWindows10, false);

                if (FileList.Count > 0) then
                begin
                    Result := true;
                    LogMsg (cInstallUsingPnpUtilMsg);
                end; { if }

            finally
                FileList.Free;
            end; { try / finally }
        end; { Find_INF_Files }

        (* ---- *)

        function IsIntel_ME_FirmwareUpdate (out sBatchFile: String) : Boolean;

        const
            cME_UpdateExe = 'fwupdate.exe';

        begin
            Result := Pos ('\' + cME_UpdateExe,
                           LowerCase (Package.InstallCmd)) > 0;

            if (Result) then
            begin
                if not (FindBatchFile (sBatchFile, cMeUpdateCmd)) then
                    sBatchFile := '';
            end { if }
            else Result := FindBatchFile (sBatchFile, cMeUpdateCmd);
        end; { IsIntel_ME_FirmwareUpdate }

        (* ---- *)

        procedure EditBatchFile (const sFileName: String);

        const
            cRemark = 'rem ';

        var
            sLine : String;
            iIndex : Integer;
            bChanged : Boolean;
            FileList : TStringList;

        begin
            FileList := TStringList.Create;

            try
                bChanged := false;

                try
                    FileList.LoadFromFile (sFileName);

                    for iIndex := 0 to FileList.Count - 1 do
                    begin
                        sLine := LowerCase (FileList [iIndex]);

                        if (Pos ('pause', sLine) > 0) or
                           (Pos ('shutdown', sLine) > 0) then
                            if (Pos (cRemark, sLine) <> 1) then
                            begin
                                bChanged := true;
                                FileList [iIndex] := cRemark + FileList [iIndex]
                            end; { if }
                    end; { for }

                    if (bChanged) then
                    begin
                        FileList.SaveToFile (sFileName);
                        LogVerbose (cBatchFileModifiedMsg, [sFileName]);
                    end; { if }

                except
                    on E: Exception do
                        LogMsg (cFileChangedErrMsg,
                                [E.ClassName, sFileName, E.Message]);
                end; { try / except }

            finally
                FileList.Free;
            end; { try / finally }
        end; { EditBatchFile }

        (* ---- *)

        procedure CheckRebootRequired (const sUpdateCmd: String;
                                       const dwReturnCode: DWord);

            (* ---- *)

            function ProcessDpInstResult : Boolean;
            // http://www.msierrors.com/tag/dpinst-return-codes/

            var
                sResult : String;

            begin
                sResult := Format ('%x', [dwReturnCode]);

                if (Length (sResult) = 8) then
                    Result := Copy (sResult, 1, 2) = '40'
                else Result := false;
            end; { ProcessDpInstResult }

            (* ---- *)

        begin { CheckRebootRequired }
            Assert (sUpdateCmd <> '');

            if (Pos ('.msi', sUpdateCmd) > 0) then
                Package.RebootRequired := dwReturnCode = 3010
            else if (IsDpInstExe (sUpdateCmd)) then
                Package.RebootRequired := ProcessDpInstResult
            else Package.RebootRequired := Package.RebootType > 0;
        end; { CheckRebootRequired }

        (* ---- *)

        function Is_SSD_FirmwareUpdateCmd (out iPos: Integer) : Boolean;
        begin
            iPos := Pos (cSsdFlashCmd, LowerCase (Package.InstallCmd));
            Result := iPos > 0;
        end; { Is_SSD_FirmwareUpdateCmd }

        (* ---- *)

        function Format_SSD_FirmwareUpdateCmd (const iPos: Integer) : String;
        begin
            if (Length (Package.InstallCmd) = (Pred (iPos) +
                                               Length (cSsdFlashCmd))) then
            begin
                LogVerbose (cSsdCmdSilentParamAddedMsg);
                Result := Package.InstallCmd + ' /A';
            end { if }
            else Result := Package.InstallCmd;
        end; { Format_SSD_FirmwareUpdateCmd }

        (* ---- *)

        procedure CheckDpInstParams (var sCmd: String);

        const
            cCmd = '/C';

        var
            iPos : Integer;

        begin
            Assert (sCmd <> '');

            iPos := Pos (cCmd, UpperCase (sCmd));

            if (iPos > 0) then
                if (iPos = Pred (Length (sCmd))) or (sCmd [iPos + 2] = ' ') then
                    exit;

            sCmd := sCmd + ' ' + cCmd;
            LogVerbose (cDpInstParamAddedMsg);
        end; { CheckDpInstParams }

        (* ---- *)

        function IsWinUpTp (const sCmd: String; var iPos: Integer) : Boolean;
        begin
            iPos := Pos (cWinUpTpExe, sCmd);

            if (iPos = 0) then
                iPos := Pos (cWinUpTp64Exe, sCmd);

            Result := iPos > 0;
        end; { IsWinUpTp }

        (* ---- *)

    var
        sCmd, sBatchFile : String;
        dwReturnCode : DWord;
        bUseOriginalCmd, bIntel_ME_FirmwareUpdate, bPnpUtil, bBatch : Boolean;
        iPos : Integer;

    begin { RunProgram }
        Assert (sPackagePath <> '');

        Result := false;

        bPnpUtil := false;
        bBatch := false;

        if (Package.PackageType = ptFirmware) then
        begin
            bIntel_ME_FirmwareUpdate := IsIntel_ME_FirmwareUpdate (sBatchFile);

            if (bIntel_ME_FirmwareUpdate) and (sBatchFile <> '') then
                bBatch := true;
        end { if }
        else bIntel_ME_FirmwareUpdate := false;

        if (InstallCmdEqualsExtractCmd) or
           (bIntel_ME_FirmwareUpdate and bBatch) then
        begin
            bUseOriginalCmd := false;

            if not (bIntel_ME_FirmwareUpdate) then
                bBatch := FindBatchFile (sBatchFile);

            if (bBatch) then
            begin
                LogMsg (cInstallUsingSetupCmdMsg, [sBatchFile]);

                EditBatchFile (sPackagePath + '\' + sBatchFile);
                sCmd := sPackagePath + '\' + sBatchFile +
                        iif (bIntel_ME_FirmwareUpdate, ' /s', '');
            end { if }
            else if (Find_INF_Files) then
            begin
                bPnpUtil := true;
                sCmd := GetSysNativePath (GetSystemDir) + cPnpUtilExe +
                        iif (IsWindows10, cPnpUtilParamsWin10, cPnpUtilParams) +
                        sPackagePath + '\*.inf';
            end { else }
            else bUseOriginalCmd := true;
        end { if }
        else if (Is_SSD_FirmwareUpdateCmd (iPos)) then
        begin
            sCmd := Format_SSD_FirmwareUpdateCmd (iPos);
            bUseOriginalCmd := false;
        end { else if }
        else bUseOriginalCmd := true;

        if (bUseOriginalCmd) then
        begin
            sCmd := Package.InstallCmd;

            if (Package.PackageType = ptSystemBios) then
            begin
                if (IsWinUpTp (LowerCase (sCmd), iPos)) then
                begin  // ThinkPad
                    if not (CheckThinkPadBiosUpdateParam (sCmd, iPos)) then
                        exit;
                end { if }
                else if (Pos (cBiosFlashCmd, LowerCase (sCmd)) > 0) then
                    CheckBiosFlashCmdParams (sCmd, sPackagePath);
            end { if }
            else if (IsDpInstExe (LowerCase (sCmd))) then
                CheckDpInstParams (sCmd);
        end; { if }

        if (Pos (cPackagePath, sCmd) > 0) then
            sCmd := ReplaceStr (sCmd, cPackagePath, sPackagePath);

        if (sCmd [2] <> ':') then
            sCmd := sPackagePath + '\' + sCmd;

        if (ExecuteCmd (sCmd, dwReturnCode)) then
        begin
            Package.InstallResultValues := dwReturnCode;

            if (bPnpUtil) then
                case dwReturnCode of
                    0, 259 : Result := true;
                    3010 :
                        begin
                            Result := true;
                            Package.RebootRequired := true;
                        end; { case 3010 }
                end { case dwReturnCode of }
            else if (ValueInArray (dwReturnCode, Package.InstallSuccess)) then
            begin
                Result := true;
                CheckRebootRequired (LowerCase (sCmd), dwReturnCode);
            end; { if }
        end; { if }
    end; { RunProgram }

    (* ---- *)

var
    sPackagePath : String;

begin { InstallPackage }
    Assert (sWorkFolder <> '');

    sPackagePath := GetPackagePath;

    if (DirectoryExists (sPackagePath)) then
    begin
        if (Package.InstallType = itCmd) then
            Result := BoolToCheckResult (RunProgram (sPackagePath))
        else Result := BoolToCheckResult (InstallInfFile (sPackagePath));
    end { if }
    else
    begin
        Result := crFalse;
        LogMsg (cDirMissingMsg, [sPackagePath]);
    end; { else }

    Package.InstallResult := Result;

    if (Result = crTrue) then
        LogMsg (cSuccessMsg +
                iif (Package.RebootRequired, cRebootRequestedMsg, ''))
    else LogMsg (cFailureMsg);
end; { InstallPackage }

(* ---- *)

function ProcessXmlFiles (const sDatabaseXml: String;
                          const PackageList: TObjectList;
                          out uInstallationCount: UInt;
                          const ID_List: TStringList = NIL) : Boolean;

    (* ---- *)

    function FindPackageToIgnore : Integer;

    var
        iIndex : Integer;
        sPackageToIgnore : String;
        Package : TPackage;

    begin
        Result := 0;

        sPackageToIgnore := LowerCase (IgnorePackageName);

        for iIndex := PackageList.Count - 1 downto 0 do
        begin
            Package := TPackage (PackageList [iIndex]);

            if (Pos (sPackageToIgnore, LowerCase (Package.Description)) = 1) then
            begin
                if (Result = 0) then
                    LogMsg ('');

                LogMsg (cPackageToIgnoreFoundMsg, [Package.Description]);
                Inc (Result);
                Package.Free;
                PackageList.Delete (iIndex);
            end; { if }
        end; { for }

        if (Result = 0) then
            LogMsg (cPackageNotFoundMsg, [IgnorePackageName])
        else LogMsg ('');
    end; { FindPackageToIgnore }

    (* ---- *)

    function FindSinglePackage : Boolean;

    var
        iIndex : Integer;
        sSinglePackage : String;
        Package : TPackage;

    begin
        sSinglePackage := LowerCase (SinglePackageName);

        for iIndex := PackageList.Count - 1 downto 0 do
        begin
            Package := TPackage (PackageList [iIndex]);

            if (Pos (sSinglePackage, LowerCase (Package.Description)) <> 1) then
            begin
                Package.Free;
                PackageList.Delete (iIndex);
            end; { if }
        end; { for }

        Result := PackageList.Count > 0;

        if not (Result) then
        	LogMsg (cPackageNotFoundMsg, [SinglePackageName]);
    end; { FindSinglePackage }

    (* ---- *)

    function FormatList (const PackageList: TObjectList) : Integer;

    var
        SortedList : TStringList;
        iIndex, iIgnored : Integer;

    begin
        Result := 0;
        iIgnored := 0;

        if (PackageList.Count > 0) then
        begin
            if (SinglePackage) then
                if not (FindSinglePackage) then
                    exit;

            if (IgnorePackage) then
                iIgnored := FindPackageToIgnore;
        end; { if }

        if (PackageList.Count = 0) then
            LogMsg (cNoPackagesFoundMsg)
        else if (PackageList.Count = 1) then
            LogMsg (cOnePackageFoundMsg)
        else LogMsg (cPackagesFoundMsg, [PackageList.Count]);

        if (PackageList.Count > 0) then
        begin
            SortedList := CreateSortedStringList (dupAccept);

            try
                for iIndex := 0 to PackageList.Count - 1 do
                    SortedList.Add ('> ' +
                    				TPackage (PackageList [iIndex]).Description);

                LogMsg (SortedList.Text);

            finally
                SortedList.Free;
            end; { try / finally }
        end; { if }

        if (iIgnored > 0) then
            LogMsg (cPackageIgnoredCountMsg, [iIgnored, IgnorePackageName]);

        Result := PackageList.Count;
    end; { FormatList }

    (* ---- *)

    function GetPackageXmlPath (const sDB_Path, sPackageXmlSubPath: String;
                                out sFullPath: String) : Boolean;
    begin
        Assert (sDB_Path <> '');

        if (sPackageXmlSubPath = '') then
            Result := false
        else
        begin
            Result := true;

            if (sPackageXmlSubPath [1] = '\') then
                sFullPath := sDB_Path + sPackageXmlSubPath
            else sFullPath := sDB_Path + '\' + sPackageXmlSubPath;
        end; { else }
    end; { GetPackageXmlPath }

    (* ---- *)

    function GetSkippedPackagesCount : Integer;

    var
        iIndex : Integer;
        SR : TSkippedReason;

    begin
        Result := 0;

        for iIndex := 0 to PackageList.Count - 1 do
        begin
            SR := TPackage (PackageList [iIndex]).SkippedReason;

            if (SR <> srNotChecked) and (SR <> srNotSkipped) then
                Inc (Result);
        end; { for }
    end; { GetSkippedPackagesCount }

    (* ---- *)

    procedure ListPackages (const bSkippedPackages: Boolean);

        (* ---- *)

        procedure ListSkipReason (const List: TStrings);

            (* ---- *)

            function GetSkipReason (const Reason: TSkippedReason) : String;
            begin
                case Reason of
                    srNotChecked : Result := cSkipReasonNotCheckedMsg;
                    srPrerequisites : Result := cSkipReasonPrerequisitesMsg;
                    srDetectInstall : Result := cSkipReasonDetectInstallMsg;
                    srNotSkipped : Result := '-';
                end; { case Reason of }
            end; { GetSkipReason ( }

            (* ---- *)

        var
            iIndex, iReason : Integer;
            SkippedList : TStringList;
            SR : TSkippedReason;

        begin { ListSkipReason }
            SkippedList := TStringList.Create;

            try
                for iReason := 0 to Integer (srNotSkipped) - 1 do
                begin
                    SkippedList.Clear;

                    SR := TSkippedReason (iReason);

                    for iIndex := 0 to List.Count - 1 do
                        if (UInt (List.Objects [iIndex]) AND UInt (SR) <> 0) then
                            SkippedList.Add (Format ('%s', [List [iIndex]]));

                    if (SkippedList.Count > 0) then
                    begin
                        LogMsg (cSkippedBecauseMsg,
                                [SkippedList.Count, GetSkipReason (SR)]);
                        LogMsg (SkippedList.Text);
                    end; { if }
                end; { for }

            finally
                SkippedList.Free;
            end; { try / finally }
        end; { ListSkipReason }

        (* ---- *)

    var
        iIndex : Integer;
        List : TStringList;
        sMsg : String;

    begin { ListInstallationRequiredPackages }
        LogMsg ('');

        List := CreateSortedStringList;

        try
            for iIndex := 0 to PackageList.Count - 1 do
                with TPackage (PackageList [iIndex]) do
                begin
                    sMsg := Format ('> %s%s', [Description,
                                               iif (VerboseLogging,
                                                    ' (v' + PackageVersion + ')',
                                                    '')]);

                    if (bSkippedPackages) and
                       (SkippedReason <> srNotSkipped) then
                        List.AddObject (sMsg, TObject (SkippedReason))
                    else if (TargetPresent = crTrue) and
                            (IsInstalled = false) then
                        List.Add (sMsg);
                end; { with }

            if (bSkippedPackages) then
                ListSkipReason (List)
            else LogMsg (List.Text);

        finally
            List.Free;
        end; { try / finally }
    end; { ListInstallationRequiredPackages }

    (* ---- *)

var
    Database : TDatabase;
    iIndex, iInstallNeededCount, iProcessed, iError, iSkipped : Integer;
    sDB_Path, sPackageXml : String;
    Devices : TDevices;

begin { ProcessXmlFiles }
    Assert (sDatabaseXml <> '');

    Result := true;

    Database := TDatabase.Create (sDatabaseXml);

    try
        LogMsg (#13#10 + cProcessing_DB_Msg, [sDatabaseXml]);
        Database.ProcessXmlFile (PackageList, ID_List);

        sDB_Path := ExtractFileDir (sDatabaseXml);

        if (FormatList (PackageList) = 0) then
            exit;

        if (Action <> aUnpack) then
        begin
            Write (cDevicesRetrievingMsg);
            Devices := TDevices.Create (true);
            Devices.Hardware_IDs_ToLower;
            WriteLn (cDoneMsg);
        end { if }
        else Devices := NIL;

        try
            iProcessed := 0;
            iError := 0;

            for iIndex := 0 to PackageList.Count - 1 do
                with TPackage (PackageList [iIndex]) do
                    if (GetPackageXmlPath (sDB_Path, LocalPath,
                                           sPackageXml)) then
                        try
                            if (ProcessPackageXml (sPackageXml, Devices)) then
                                Inc (iProcessed);

                        except
                            on E: Exception do
                            begin
                                Inc (iError);
                                XmlError := true;
                                LogMsg (cPackageProcessingErrMsg, [E.Message]);
                            end; { on E: Exception do }
                        end { try / except }
                    else LogMsg (cPackageXmlNotFoundMsg, [LocalPath]);

        finally
            FreeAndNil (Devices);
        end; { try / finally }

        if (Action <> aUnpack) then
        begin
            uInstallationCount := GetInstallationCount (PackageList);

            LogMsg (cProcessedMsg +
                    iif (iError > 0, Format (cProcessedErrMsg, [iError]), '') +
                    cToBeInstalledCountMsg +
                    iif (uInstallationCount > 0, ':', ''),
                    [iProcessed, PackageList.Count, uInstallationCount]);

            if (uInstallationCount > 0) then
            begin
                ListPackages (false);

                if (UpdateBios) then
                begin
                    iInstallNeededCount := GetInstallationCount (PackageList,
                                                                 ptSystemBios);

                    if (iInstallNeededCount > 1) then
                    begin
                        Result := false;
                        LogMsg (cTooManyBiosUpdateFilesMsg);
                    end; { if }
                end; { if }
            end; { if }

            if (ListSkippedPackages) then
            begin
                iSkipped := GetSkippedPackagesCount;

                if (iSkipped > 0) then
                begin
                    LogMsg (cSkippedPackgesMsg, [iSkipped]);
                    ListPackages (true);
                end; { if }
            end; { if }
        end { if }
        else
        begin
            Result := iError = 0;
            LogMsg (cProcessedMsg, [iProcessed, PackageList.Count]);
        end; { else }

    finally
        Database.Free;
    end; { try / finally }
end; { ProcessXmlFiles }

(* ---- *)

function UnpackFiles (const Package: TPackage;
                      const sParentFolder: String) : Boolean;

    (* ---- *)

    procedure CheckForInstallationFiles (const sTargetFolder: String);

    var
        sFile, sPath : String;
        iPos : Integer;

    begin
        Assert (sTargetFolder <> '');

        sFile := Package.InstallCmd;

        iPos := Pos ('\', sFile);

        if (iPos > 0) then
        begin
            Delete (sFile, 1, iPos);

            iPos := Pos (' ', sFile);

            if (iPos > 0) then
                SetLength (sFile, iPos - 1);

            if (Action = aUnpack) then
                sPath := sTargetFolder + cFilesDir
            else sPath := sTargetFolder;

            if not (FileExists (sPath + '\' + sFile)) then
                LogMsg (cInstallationFileMissingMsg, [sFile, sPath]);
        end { if }
        else LogVerbose (cInstallationCheckErrMsg, [sFile]);
    end; { CheckForInstallationFiles }

    (* ---- *)

    function CopyDataFiles (const sTargetPath: String;
                            const bArchiveExists: Boolean) : Boolean;

    var
        FileList : TStringList;
        sFile, sArchive, sSourcePath : String;
        iCount : Integer;

    begin
        Assert (sTargetPath <> '');

        Result := true;

        if (bArchiveExists) then
            sArchive := LowerCase (Copy (Package.ExtractCommand, 1,
                                         Pos ('.', Package.ExtractCommand) + 3))
        else sArchive := '';

        FileList := CreateSortedStringList;

        try
            sSourcePath := ExtractFilePath (Package.PackageXml);

            FillFileList (FileList, sSourcePath + '*.*',
                          false, false, false, true, true);

            Assert (FileList.Count > 0);

            iCount := 0;

            for sFile in FileList do
                if (sFile <> sArchive) then
                    if (CopyFile (PChar (sSourcePath + sFile),
                                  PChar (sTargetPath + sFile), true)) then
                        Inc (iCount)
                    else
                    begin
                        Result := false;
                        LogMsg (cFileCopyErrMsg, [sSourcePath + sFile,
                                                  sTargetPath + sFile,
                                                  GetLastErrorMsg]);
                        Break;
                    end; { else }

            LogVerbose (cFilesCopiedMsg, [iCount]);

        finally
            FileList.Free;
        end; { try / finally }
    end; { CopyDataFiles }

    (* ---- *)

    function CreateFolders (const sPath: String) : Boolean;
    begin
        Result := false;

        if (ForceDirectories (sPath)) then
        begin
            if (Action = aUnpack) and (Package.ExtractCommand <> '') then
            begin
                if (CreateDir (sPath + cFilesDir)) then
                    Result := true
                else LogMsg (cCreateFolderErrMsg, [sPath + cFilesDir]);
            end { if }
            else Result := true;
        end { if }
        else LogMsg (cCreateFolderErrMsg, [sPath]);
    end; { CreateFolders }

    (* ---- *)

    function FormatUnpackCmd (const sTargetPath: String;
                              out sUnpackCmd: String) : Boolean;

    const
        cPackagePath = '%packagepath%';

    var
        iPos : Integer;
        sExtractCmd, sSourcePath, sFullPath : String;

    begin
        Assert (sTargetPath <> '');

        Result := false;

        sExtractCmd := Package.ExtractCommand;

        iPos := Pos (cPackagePath, LowerCase (sExtractCmd));

        if (iPos > 0) then
        begin
            Delete (sExtractCmd, iPos, Length (cPackagePath));

            if (Action = aUnpack) then
                sFullPath := sTargetPath + cFilesDir
            else sFullPath := sTargetPath;

            if (Action = aUnpack) then
                Insert (sFullPath, sExtractCmd, iPos)
            else Insert (sFullPath, sExtractCmd, iPos);

            sSourcePath := ExtractFileDir (Package.PackageXml);

            sUnpackCmd := sSourcePath + '\' + sExtractCmd;
            Result := true;
        end { if }
        else LogMsg (cPackagePathMissingMsg);
    end; { FormatUnpackCmd }

    (* ---- *)

    function GetExeName (const sExtractCmd: String;
                         out sExeName: String) : Boolean;

    var
        iPos : Integer;

    begin
        Result := false;

        iPos := Pos (cExeExt, LowerCase (sExtractCmd));

        if (iPos = 0) then
            LogVerbose (cNoExeFileMsg, [sExtractCmd])
        else
        begin
            Result := true;
            sExeName := Copy (sExtractCmd, 1, iPos + 3);
        end; { else }
    end; { GetExeName }

    (* ---- *)    (* ---- *)

    function GetPackagePathFromXmlPath : String;

    var
        asPaths : TStringDynArray;

    begin
        asPaths := SplitString (Package.PackageXml, '\');

        Result := IncludeTrailingBackslash (sParentFolder) +
                  asPaths [Pred (High (asPaths))];
    end; { GetPackagePathFromXmlPath }

    (* ---- *)

    function GetTargetPath (out sTargetPath: String) : Boolean;
    begin
        Result := false;

        if (Package.ExtractCommand <> '') then
            sTargetPath := GetPackagePathFromXmlPath
        else
        begin
            LogMsg (cUnpackCmdMissingMsg);

            if (Action = aUnpack) then
                sTargetPath := GetPackagePathFromXmlPath
            else exit;
        end; { else }

        Result := true;
    end; { GetTargetPath }

    (* ---- *)

    function MoveInstaller (const sTargetPath: String) : Boolean;

    var
        sFileName : String;
        iPos : Integer;

    begin
        Result := false;

        sFileName := LowerCase (ExtractFileName (Package.InstallCmd));

        iPos := Pos (cExeExt, sFileName);

        if (Length (sFileName) > (iPos + 3)) then
            SetLength (sFileName, iPos + 3);

        if (FileExists (sTargetPath + '\' + sFileName)) then
        begin
            if not (DirectoryExists (sTargetPath + cFilesDir)) then
                if not (CreateDir (sTargetPath + cFilesDir)) then
                begin
                    LogMsg (cCreateFolderErrMsg, [sTargetPath + cFilesDir]);
                    exit;
                end; { if }

            Result := RenameFile (sTargetPath + '\' + sFileName,
                                  sTargetPath + cFilesDir + '\' + sFileName);

            if (Result) then
                LogMsg (cMoveFileSuccessMsg, [sFileName, sTargetPath, cFilesDir])
            else LogMsg (cMoveFileErrMsg, [sFileName, sTargetPath, cFilesDir,
                                           GetLastErrorMsg (GetLastError)])
        end { if }
        else LogMsg (cInstallationFileMissingMsg, [sFileName, sTargetPath]);
    end; { MoveInstaller }

    (* ---- *)

var
    sTargetPath, sUnpackCmd : String;
    bContinue, bFolderExists : Boolean;
    dwReturnCode, dwSuccessCode : DWord;

begin { UnpackFiles }
    Assert (sParentFolder <> '');

    Result := false;

    if (GetTargetPath (sTargetPath)) then
    begin
        bContinue := true;

        if (DirectoryExists (sTargetPath)) then
        begin
            bFolderExists := true;

            if (IgnoreExistingFolders) then
                LogMsg (cTargetFolderExistsMsg + cIgnoringExistingFolderMsg,
                        [sTargetPath])
            else
            begin
                bContinue := false;
                LogMsg (cTargetFolderExistsMsg + cUnableToUnpackMsg,
                        [sTargetPath])
            end { else }
        end { if }
        else bFolderExists := false;

        if (bContinue) then
            if (bFolderExists) then
                Result := true
            else if (CreateFolders (sTargetPath)) then
            begin
                if (Action = aUnpack) then
                    bContinue := CopyDataFiles (sTargetPath + '\',
                                                Package.ExtractCommand <> '')
                else bContinue := true;

                if (bContinue) then
                    if (Package.ExtractCommand <> '') then
                    begin
                        if (Pos ('-s -ex -f',  // Lenovo BIOS Update Utility
                                 LowerCase (Package.ExtractCommand)) > 0) then
                            dwSuccessCode := 12
                        else dwSuccessCode := 0;

                        if (FormatUnpackCmd (sTargetPath, sUnpackCmd)) then
                        	if (ExecuteCmd (sUnpackCmd, dwReturnCode)) then
                                if (dwReturnCode = dwSuccessCode) then
                                begin
                                    Result := true;
                                    CheckForInstallationFiles (sTargetPath);
                                end; { if }
                    end { if }
                    else
                        if (Pos (cExeExt,
                                 LowerCase (Package.InstallCmd)) > 0) then
                            Result := MoveInstaller (sTargetPath)
                        else Result := true;
            end; { if }
    end; { if }

    if (Result) then
        LogMsg (cSuccessMsg)
    else LogMsg (cFailureMsg);
end; { UnpackFiles }

(* ---- *)

end.

