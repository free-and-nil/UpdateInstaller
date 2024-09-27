// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

{$WARN SYMBOL_PLATFORM OFF}

unit CheckParamsU;

interface

uses GlobalsU;

procedure ProcessParams (out sDatabaseXml, sTempFolder, sLogFolder: String;
                         out {%H-}bReboot: Boolean);

procedure ShowParams;

implementation

uses SysUtils,
     CmdLineU, LogToTempU, DebugHelperU, Win32ToolsU;

ResourceString
    cActionUndefinedMsg =
         'Action missing: "/Check", "/Unpack", or "/Install". Aborting program';

(* ---- *)

procedure ProcessParams (out sDatabaseXml, sTempFolder, sLogFolder: String;
                         out bReboot: Boolean);

const
    cCheckAll = 'CheckAll';
    cDebug = 'debug';
    cIgnoreExistingFolders = 'IgnoreExistingFolders';
    cIgnorePackage = 'IgnorePackage';
    cIgnoreSystemCompatibility = 'IgnoreSystemCompatibility';
    cIgnoreWarning = 'IgnoreWarning';
    cIgnoreWindowsVersionCheck = 'IgnoreWindowsVersionCheck';
    cListAll = 'ListAll';
    cListSkippedPackages = 'ListSkippedPackages';
    cLoadFromIni = 'LoadFromIni';
    cLogDir = 'LogDir';
    cObsoleteParamFoundMsg = 'Obsolete command line parameter "/%s" found';
    cPromptForExecution = 'PromptForExecution';
    cRelaxedSystemCompatibilityCheck = 'RelaxedSystemCompatibilityCheck';
    cSaveToIni = 'SaveToIni';
    cSinglePackage = 'SinglePackage';
    cTargetDir = 'TargetDir';
    cUpdateAll = 'UpdateAll';
    cVerbose = 'Verbose';
    cVeryVerbose = 'VeryVerbose';
    cWorkDir = 'WorkDir';

var
    CmdLineParams : ICmdLineParams;
    iIndex : Integer;

begin
    CmdLineParams := TCmdLineParams.Create (puaError);

    with CmdLineParams do
    begin
        with AddParam (cCheckParam, true, vkNextParam) do
            CannotBeUsedWith ([cUnpackParam, cInstallParam]);

        AddParamValue (cCheckParam);

        with AddParam (cUnpackParam, true, vkNextParam) do
        begin
            CannotBeUsedWith ([cCheckParam, cInstallParam]);
            MustBeUsedWith ([cTargetDir]);
        end; { with }

        AddParamValue (cUnpackParam);

        with AddParam (cTargetDir, true, vkNextParam) do
            MustBeUsedWith ([cUnpackParam]);

        AddParamValue (cTargetDir);

        with AddParam (cInstallParam, true, vkNextParam) do
        begin
            CannotBeUsedWith ([cCheckParam, cUnpackParam]);
            MustBeUsedWith ([cWorkDir]);
        end; { with }

        AddParamValue (cInstallParam);

        with AddParam (cWorkDir, true, vkNextParam) do
            MustBeUsedWith ([cInstallParam]);

        AddParamValue (cWorkDir);

(**
        with AddParam (cReboot, true) do
        begin
            CannotBeUsedWith ([cCheckParam, cUnpackParam]);
            MustBeUsedWith ([cInstallParam]);
        end; { with }
**)

        with AddParam (cSinglePackage, true, vkNextParam) do
            CannotBeUsedWith ([cUpdateAll]);

        AddParamValue (cSinglePackage);

        with AddParam (cIgnorePackage, true, vkNextParam) do
            CannotBeUsedWith ([cUnpackParam, cSinglePackage]);

        AddParamValue (cIgnorePackage);

        AddParam (cLogDir, false, vkNextParam);
        AddParamValue (cLogDir);

        AddParam (cVerbose, true);
        AddParam (cVeryVerbose, true);
        AddParam (cDebug, true);
        AddParam (cListAll, true);
        AddParam (cIgnoreWarning, true);
        AddParam (cPromptForExecution, true);

        with AddParam (cIgnoreSystemCompatibility, true) do
            CannotBeUsedWith ([cUpdateFirmwareParam, cUpdateBiosParam]);

        with AddParam (cRelaxedSystemCompatibilityCheck, true) do
            CannotBeUsedWith ([cUnpackParam, cIgnoreSystemCompatibility]);

        with AddParam (cListSkippedPackages, true) do
            CannotBeUsedWith ([cUnpackParam]);

        with AddParam (cUpdateFirmwareParam, true) do
            CannotBeUsedWith ([cUnpackParam, cUpdateBiosParam]);

        with AddParam (cUpdateAll, true) do
        begin
            CannotBeUsedWith ([cCheckParam, cUnpackParam]);
            MustBeUsedWith ([cUpdateFirmwareParam]);
        end; { with }

        with AddParam (cSaveToIni, true, vkNextParam) do
        begin
            CannotBeUsedWith ([cInstallParam, cUnpackParam]);
            MustBeUsedWith ([cCheckParam]);
        end; { with }

        AddParamValue (cSaveToIni);

        with AddParam (cLoadFromIni, true, vkNextParam) do
        begin
            CannotBeUsedWith ([cCheckParam]);
            MustBeUsedWith ([cInstallParam, cUnpackParam]);
        end; { with }

        AddParamValue (cLoadFromIni);

        with AddParam (cIgnoreExistingFolders, true) do
        begin
            CannotBeUsedWith ([cCheckParam]);
            MustBeUsedWith ([cUnpackParam, cInstallParam]);
        end; { with }

        with AddParam (cCheckAll, true) do
        begin
            CannotBeUsedWith ([cUnpackParam, cInstallParam]);
            MustBeUsedWith ([cCheckParam]);
        end; { with }

        with AddParam (cIgnoreWindowsVersionCheck, true) do
        begin
            CannotBeUsedWith ([cUnpackParam]);
            MustBeUsedWith ([cCheckParam, cInstallParam]);
        end; { with }

        with AddParam (cUpdateBiosParam, true) do
            CannotBeUsedWith ([cUnpackParam, cUpdateFirmwareParam, cUpdateAll]);

        with AddParam (cCompatibleSystems, true, vkNextParam) do
            CannotBeUsedWith ([cUnpackParam]);

        AddParamValue (cCompatibleSystems);

        ParseParams;

        if (ParamExists (cCheckParam, iIndex)) then
        begin
            SetAction (aCheck);
            sTempFolder := '';
        end { if }
        else if (ParamExists (cUnpackParam, iIndex)) then
        begin
            SetAction (aUnpack);
            sTempFolder := ParamValue [GetParamIndex (cTargetDir)];
        end { else if }
        else if (ParamExists (cInstallParam, iIndex)) then
        begin
            SetAction (aInstall);
            sTempFolder := ParamValue [GetParamIndex (cWorkDir)];
        end { else if }
        else raise EParamCheck.Create (cActionUndefinedMsg);

        sDatabaseXml := ParamValue [iIndex];

        if (ParamExists (cVerbose)) then
            SetVerboseLogging (true);

        if (ParamExists (cVeryVerbose)) then
            SetVeryVerboseLogging;

        if (ParamExists (cDebug)) or (DebuggerPresent) then
            EnableDebugMode;

        if (ParamExists (cUpdateFirmwareParam)) then
            SetUpdateFirmware;

        if (ParamExists (cUpdateAll)) then
            SetUpdateAll;

        if (ParamExists (cLoadFromIni, iIndex)) then
            SetIniFile (ParamValue [GetParamIndex (cLoadFromIni)]);

        if (ParamExists (cSaveToIni, iIndex)) then
            SetIniFile (ParamValue [GetParamIndex (cSaveToIni)]);

        if (ParamExists (cIgnoreExistingFolders)) then
            SetIgnoreExistingFolders;

        if (ParamExists (cCheckAll)) then
            SetCheckAll;

        if (ParamExists (cUpdateBiosParam)) then
            SetUpdateBios;

        if (ParamExists (cListAll)) then
            SetListAll;

        if (ParamExists (cListSkippedPackages)) then
            SetListSkippedPackages;

        if (ParamExists (cSinglePackage, iIndex)) then
            SetSinglePackage (ParamValue [GetParamIndex (cSinglePackage)]);

        if (ParamExists (cIgnorePackage, iIndex)) then
            SetIgnorePackage (ParamValue [GetParamIndex (cIgnorePackage)]);

        if (ParamExists (cCompatibleSystems, iIndex)) then
            SetCompatibleSystems (
                               ParamValue [GetParamIndex (cCompatibleSystems)]);

        if (ParamExists (cIgnoreWarning)) then
            SetIgnoreWarning;

        if (ParamExists (cIgnoreSystemCompatibility)) then
            SetIgnoreSystemCompatibility;

        if (ParamExists (cRelaxedSystemCompatibilityCheck)) then
            LogVerbose (cObsoleteParamFoundMsg,
                        [cRelaxedSystemCompatibilityCheck]);

        if (ParamExists (cPromptForExecution)) then
            SetPromptForExecution (true);

        if (ParamExists (cIgnoreWindowsVersionCheck)) then
            SetIgnoreWindowsCompatibility;

        sLogFolder := ParamValue [GetParamIndex (cLogDir)];

//        bReboot := ParamExists (cReboot);
    end; { with }
end; { ProcessParams }

(* ---- *)

procedure ShowParams;
begin
    WriteLn;
    WriteLn (Format ('%s v%s' {$IFDEF CPUX64} + ' x64' {$ENDIF}
        		     {$IFDEF DEBUG} + ' [DEBUG]' {$ENDIF},
                     [AppName, GetVersionInfo]));

    WriteLn;
    WriteLn ('LenovoInstaller.exe /Check [db.xml] /LogDir [path]');
    WriteLn ('LenovoInstaller.exe /Unpack [db.xml] /TargetDir [path] /LogDir [path]');
    WriteLn ('LenovoInstaller.exe /Install [db.xml] /WorkDir [path] /LogDir [path]');
    WriteLn;
    WriteLn ('Optional parameters:');
    WriteLn ('"/CheckAll" (with "/check")');
    WriteLn ('"/CompatibleSystems XXXX,YYYY,ZZZZ" (with "/Install" and "/Check")');
    WriteLn ('"/IgnoreExistingFolders" (with "/unpack")');
    WriteLn ('"/IgnorePackage [name]" (with "/Install" and "/Check")');
    WriteLn ('"/IgnoreSystemCompatibility" (with "/Install" and "/Check")');
    WriteLn ('"/IgnoreWarning" (with "/Install")');
    WriteLn ('"/IgnoreWindowsVersionCheck" (with "/Install" and "/Check")');
    WriteLn ('"/ListAll"');
    WriteLn ('"/ListSkippedPackages"');
    WriteLn ('"/LoadFromIni" (with "/Unpack" or "/Install")');
    WriteLn ('"/PromptForExecution"');
//  WriteLn ('"/RelaxedSystemCompatibilityCheck"');
	WriteLn ('"/SaveToIni" (with "/Check")');
    WriteLn ('"/SinglePackage [name]"');
    WriteLn ('"/UpdateBios"');
    WriteLn ('"/UpdateFirmware"');
    WriteLn ('"/UpdateAll" (with "/UpdateFirmware")');
    WriteLn ('"/Verbose" (print detailed debug information)');
    WriteLn ('"/VeryVerbose" (print very detailed debug information)');
end; { ShowParams }

(* ---- *)

end.

