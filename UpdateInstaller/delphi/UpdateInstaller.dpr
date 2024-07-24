// Copyright (c) 2021 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\..\switches.inc}

{$WARN SYMBOL_PLATFORM OFF}

{$IFNDEF DELPHI2007_UP}
    {$MESSAGE FATAL 'This program must be compiled with Delphi 2007 or newer'}
{$ENDIF}

program UpdateInstaller;

{$APPTYPE CONSOLE}
{$R *.RES}

uses
  SysUtils,
  Windows,
  MainU in '..\MainU.pas',
  LogToTempU in '..\..\..\PASTOOLS\LogToTempU.pas',
  XmlBaseClassesU in '..\XmlBaseClassesU.pas',
  ProcessDataBaseXmlU in '..\ProcessDataBaseXmlU.pas',
  CmdLineU in '..\..\..\PASTOOLS\CmdLineU.pas',
  ProcessFilesU in '..\ProcessFilesU.pas',
  CheckParamsU in '..\CheckParamsU.pas',
  GlobalsU in '..\GlobalsU.pas',
  PackageClassU in '..\PackageClassU.pas',
  LibXmlParserU in 'D:\CompLib\XMLParser\v2\LibXmlParserU.pas',
  GetPackageVerU in '..\GetPackageVerU.pas',
  DeviceViewerU in '..\..\..\PASTOOLS\DeviceViewerU.pas',
  pastools in '..\..\..\PASTOOLS\pastools.pas',
  ServicesU in '..\..\..\PASTOOLS\ServicesU.pas',
  Win2000_ImportU in '..\..\..\PASTOOLS\Win2000_ImportU.pas',
  WinVista_ImportU in '..\..\..\PASTOOLS\WinVista_ImportU.pas',
  WinVista_ToolsU in '..\..\..\PASTOOLS\WinVista_ToolsU.pas',
  RegistryApiU in '..\..\..\PASTOOLS\RegistryApiU.pas',
  NativeApi_ImportU in '..\..\..\PASTOOLS\NativeApi_ImportU.pas',
  RegistryHelperU in '..\..\..\pastools\RegistryHelperU.pas',
  BaseTypesU in '..\..\..\PASTOOLS\BaseTypesU.pas',
  Win32ToolsU in '..\..\..\PASTOOLS\Win32ToolsU.pas',
  Delphi_T in '..\..\..\PASTOOLS\Delphi_T.pas',
  Delphi32ToolsU in '..\..\..\PASTOOLS\Delphi32ToolsU.pas',
  StrUtilsHelperU in '..\..\..\PASTOOLS\StrUtilsHelperU.pas',
  DebugHelperU in '..\..\..\PASTOOLS\DebugHelperU.pas',
  ConsoleU in '..\..\..\PASTOOLS\ConsoleU.pas',
  OmniXML in 'D:\CompLib\OmniXml\OmniXML.pas',
  ImageHlpU in '..\..\..\PASTOOLS\ImageHlpU.pas',
  ConsoleOutputU in '..\ConsoleOutputU.pas',
  SMBIOS_U in '..\..\..\PASTOOLS\SMBIOS_U.pas',
  WindowsDisplayName_WMI_U in '..\..\..\PASTOOLS\WindowsDisplayName_WMI_U.pas',
  Wow64U in '..\..\..\PASTOOLS\Wow64U.pas';

const
    IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = $0400;

{$SetPEFlags IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP}  // Do not swap program to disk

ResourceString
    cReturnCodeMsg = #13#10'Exit code = %d';

var
    iReturnCode : Integer = 0;
    bReboot : Boolean = false;

begin { UpdateInstaller }
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;

    DeleteExistingLogFile;

    iReturnCode := Main (bReboot);

    if (Action = aInstall) and (bReboot) then
        iReturnCode := 3010;

    if (DebugHook <> 0) then
    begin
        WriteLn;
        Write ('Press [Enter] to continue ...');
        ReadLn;
    end; { if }

    LogMsg (cReturnCodeMsg, [iReturnCode]);

    if (DebugHook = 0) then
        Halt (iReturnCode);
end.
