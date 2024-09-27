// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

program UpdateInstaller;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Windows, XmlBaseClassesU, ProcessFilesU, ProcessDataBaseXmlU,
  PackageClassU, MainU, GlobalsU, GetPackageVerU, CheckParamsU,
  LogToTempU, Win32ToolsU, DebugHelperU, OmniXML, LibXmlParser
  { you can add units after this };

const
    {%H-}IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = $0400;

{$SetPEFlags IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP}  // Do not swap program to disk

ResourceString
    cReturnCodeMsg = #13#10'Exit code = %d';

var
    iReturnCode : Integer = 0;
    bReboot : Boolean;

{$R *.res}

begin { UpdateInstaller }
    iReturnCode := Main (bReboot);

    if (Action = aInstall) and (bReboot) then
        iReturnCode := 3010;

    if (DebuggerPresent) then
    begin
        WriteLn;
        Write ('Press [Enter] to continue ...');
        ReadLn;
    end; { if }

    LogMsg (cReturnCodeMsg, [iReturnCode]);

    Halt (iReturnCode);
end.

