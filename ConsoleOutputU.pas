// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit ConsoleOutputU;

interface

uses Classes;

type
    TConsoleOutput = class
      private
        sLogFile : String;
        OutputList : TStringList;

      public
        constructor Create (const sLogFile: String);
        destructor Destroy; override;

        procedure OutputProc (const sStr: String);
    end; { TConsoleOutput }

implementation

uses SysUtils,
     GlobalsU,
     LogToTempU;

const
    cDashCount = 30;

ResourceString
    cNoOutputFoundMsg = '*** The program did not generate any output ***';
    cOutputStartMsg = ' Output start ';
    cOutputEndMsg   = '- Output end -';
    cSavingOutputMsg = 'Saving program output to "%s"';

(* ---- *)

constructor TConsoleOutput.Create (const sLogFile: String);
begin
    inherited Create;

    Self.sLogFile := sLogFile;
    OutputList := TStringList.Create;

    if (VeryVerboseLogging) then
        LogMsg (cSavingOutputMsg, [sLogFile]);

    LogVerbose (StringOfChar ('-', cDashCount) + cOutputStartMsg +
                StringOfChar ('-', cDashCount));
end; { TConsoleOutput.Create }

(* ---- *)

destructor TConsoleOutput.Destroy;
begin
    if (OutputList.Count = 0) or
       ((OutputList.Count = 1) and (Trim (OutputList [0]) = '')) then
        LogMsg (cNoOutputFoundMsg)
    else
        if (VeryVerboseLogging) then
            OutputList.SaveToFile (sLogFile);

    LogVerbose (StringOfChar ('-', cDashCount) + cOutputEndMsg +
                StringOfChar ('-', cDashCount));

    OutputList.Free;

    inherited;
end; { TConsoleOutput.Destroy }

(* ---- *)

procedure TConsoleOutput.OutputProc (const sStr: String);
begin
    OutputList.Add (sStr);
    LogVerbose (sStr);
end; { TConsoleOutput.OutputProc }

(* ---- *)

end.
