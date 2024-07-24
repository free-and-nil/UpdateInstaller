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

{$IFDEF DELPHI7_UP}
	{$WARN SYMBOL_DEPRECATED OFF}
	{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

unit ServicesU;

interface

uses Classes;

type
    TCurrentState = (csStopped, csStartPending, csStopPending, csRunning,
                     csContinuePending, csPausePending, csPaused);
	TStartType = (stBootStart, stSystemStart, stAutoStart, stAutomaticDelayed,
                  stDemandStart, stServiceDisabled);
    TServiceType = (stFileSystemDriver, stKernelDriver, stWin32Service);

	TService = class
      private
        FAccount : String;
        FActive : Boolean;
        FCurrentState : TCurrentState;
        FDescription : String;
        FDisplayName : String;
        FImagePath : String;
      	FName : String;
        FServiceType : TServiceType;
        FStartType : TStartType;
//        FWow64 : Boolean;

      public
        function GetFullPath (const bDisableRedirector: Boolean = false) : String;
        function GetStartType : String;

        property Account : String read FAccount;
        property Active : Boolean read FActive;
        property CurrentState : TCurrentState read FCurrentState;
        property Description : String read FDescription;
        property DisplayName : String read FDisplayName;
        property ImagePath : String read FImagePath;
      	property Name : String read FName;
        property ServiceType : TServiceType read FServiceType;
        property StartType : TStartType read FStartType;
//        property Wow64 : Boolean read FWow64;
    end; { TService }

    TServices = class;

    TServicesEnumerator = class
      private
        FIndex: Integer;
        FServices : TServices;

      public
        constructor Create (const Services: TServices);
        function GetCurrent : TService;
        function MoveNext : Boolean;
        property Current : TService read GetCurrent;
    end; { TServicesEnumerator }

    TServices = class
      private
      	{ b_x64, } bIncludeDrivers : Boolean;
        ServiceList : TStringList;

        function GetCount : Integer;
        function GetService (iIndex: Integer) : TService;
		procedure RefreshList;

      public
        constructor Create (const bIncludeDrivers: Boolean = false);
        destructor Destroy; override;

        function GetEnumerator : TServicesEnumerator;
        function ServiceRunning (const sName: String) : Boolean;

		property Count : Integer read GetCount;
        property Services [iIndex: Integer] : TService read GetService; default;
    end; { TServices }

implementation

uses Windows, SysUtils,
{$IFNDEF FPC}
     WinSvc,
{$ENDIF}
     Delphi_T, VerifyU, PasTools, Wow64U, Win2000_ImportU;

(* ---- *)

function TService.GetFullPath (const bDisableRedirector: Boolean = false) : String;

const
    cSystemRoot = 'systemroot';
    cSystem32 = 'system32\';

var
    iPos, iLen : Integer;
    sImagePath, sSystemRoot : String;

begin
    iLen := Length (FImagePath);

    if (iLen < 2) or (FImagePath [2] = ':') then
        Result := FImagePath
    else
    begin
        sImagePath := LowerCase (FImagePath);

        sSystemRoot := GetEnvironmentVariable (cSystemRoot);

        iPos := Pos ('\' + cSystemRoot, sImagePath);

        if (iPos = 1) then
            Result := sSystemRoot + Copy (FImagePath, Length (cSystemRoot) + 2,
                                          iLen - Succ (Length (cSystemRoot)))
        else
        begin
            iPos := Pos (cSystem32, sImagePath);

            if (iPos = 1) then
                Result := GetEnvironmentVariable (cSystemRoot) + '\' + FImagePath
            else Result := FImagePath;
        end; { else }
    end; { if }

    if (bDisableRedirector) then
        Result := GetSysNativePath (Result);
end; { TService.GetFullPath }

(* ---- *)

function TService.GetStartType : String;
begin
	case FStartType of
        stAutomaticDelayed : Result := 'AutomaticDelayed';
        stAutoStart : Result := 'Automatic';
        stBootStart : Result := 'BootStart';
        stDemandStart : Result := 'Manual';
        stServiceDisabled : Result := 'Disabled';
        stSystemStart : Result := 'SystemStart';
    end; { case FStartType of }
end; { TService.GetStartType }

(* ---- *)

constructor TServicesEnumerator.Create (const Services: TServices);
begin
	inherited Create;

    FIndex := (-1);
    FServices := Services;
end; { TServicesEnumerator.Create }

(* ---- *)

function TServicesEnumerator.GetCurrent : TService;
begin
	Result := TService (FServices.ServiceList.Objects [FIndex]);
end; { TServicesEnumerator.GetCurrent }

(* ---- *)

function TServicesEnumerator.MoveNext : Boolean;
begin
	if (FIndex < Pred (FServices.ServiceList.Count)) then
    begin
		Result := true;
        Inc (FIndex);
    end { if }
    else Result := false;
end; { TServicesEnumerator.MoveNext }

(* ---- *)

function TServices.GetCount : Integer;
begin
	Result := ServiceList.Count;
end; { TServices.GetCount }

(* ---- *)

function TServices.GetService (iIndex: Integer): TService;
begin
	Result := TService (ServiceList.Objects [iIndex]);
end; { TServices.GetService }

(* ---- *)

procedure TServices.RefreshList;

    (* ---- *)

    function GetServiceStatus (const ESS: TEnumServiceStatus) : TService;
    begin
    	Result := TService.Create;

        Result.FName := ESS.lpServiceName;
        Result.FDisplayName := ESS.lpDisplayName;
        Result.FActive := ESS.ServiceStatus.dwCurrentState <> SERVICE_STOPPED;

        with Result do
            case ESS.ServiceStatus.dwCurrentState of
                SERVICE_STOPPED : FCurrentState := csStopped;
                SERVICE_START_PENDING : FCurrentState := csStartPending;
                SERVICE_STOP_PENDING : FCurrentState := csStopPending;
                SERVICE_RUNNING : FCurrentState := csRunning;
                SERVICE_CONTINUE_PENDING : FCurrentState := csContinuePending;
                SERVICE_PAUSE_PENDING : FCurrentState := csPausePending;
                SERVICE_PAUSED : FCurrentState := csPaused;
            end; { case ESS.ServiceStatus.dwCurrentState of }

		ServiceList.AddObject (Result.FName, Result);
    end; { GetServiceStatus }

    (* ---- *)

    procedure QueryConfig (const hSCM: SC_Handle; const Service: TService);

        (* ---- *)

        procedure FormatImagePath;

        var
            iLen : Integer;

        begin
            with Service do
            begin
                iLen := Length (FImagePath);

                if (iLen > 1) then
                    if (FImagePath [1] = '"') and (FImagePath [iLen] = '"') then
                        FImagePath := Copy (FImagePath, 2, iLen - 2);
            end; { with }
        end; { FormatImagePath }

        (* ---- *)

        procedure SetValues (const QSC: TQueryServiceConfig);
        begin
            Service.FImagePath := QSC.lpBinaryPathName;
            FormatImagePath;

            Service.FAccount := QSC.lpServiceStartName;

            case QSC.dwStartType of
                SERVICE_BOOT_START : Service.FStartType := stBootStart;
                SERVICE_SYSTEM_START : Service.FStartType := stSystemStart;
                SERVICE_AUTO_START : Service.FStartType := stAutoStart;
                SERVICE_DEMAND_START : Service.FStartType := stDemandStart;
                SERVICE_DISABLED : Service.FStartType := stServiceDisabled;
            end; { case QSC.dwStartType of }

            if (QSC.dwServiceType and SERVICE_FILE_SYSTEM_DRIVER <> 0) then
                Service.FServiceType := stFileSystemDriver
            else if (QSC.dwServiceType and SERVICE_KERNEL_DRIVER <> 0) then
                Service.FServiceType := stKernelDriver
            else (** if (QSC.dwServiceType and SERVICE_WIN32_OWN_PROCESS <> 0) or
                    (QSC.dwServiceType and SERVICE_WIN32_SHARE_PROCESS <> 0) then **)
                Service.FServiceType := stWin32Service;

            // Service.FWow64 := R
        end; { SetValues }

        (* ---- *)

        procedure QueryConfig2 (const hService: SC_HANDLE);

        var
            SDASI : TServiceDelayedAutoStartInfo;
            dwBytesNeeded : DWord;

        begin
            if (QueryServiceConfig2 (hService,
                                     SERVICE_CONFIG_DELAYED_AUTO_START_INFO,
                                     Pointer (@SDASI),
                                     SizeOf (TServiceDelayedAutoStartInfo),
                                     dwBytesNeeded{%H-})) then
            begin
                if (SDASI.fDelayedAutostart = Integer (true)) then
                    Service.FStartType := stAutomaticDelayed;
            end { if }
            else RaiseLastWin32Error;
        end; { QueryConfig2 }

        (* ---- *)

    var
        hService : SC_Handle;
        dwBufSize, dwBytesNeeded : DWord;
        pQSC : PQueryServiceConfig;

    begin { QueryConfig }
        hService := OpenService (hSCM, PChar (Service.FName),
                                 SERVICE_QUERY_CONFIG);

        if (hService = 0) then
            RaiseLastWin32Error;

        try
            pQSC := NIL;

            QueryServiceConfig (hService, pQSC, 0,
                                {$IFDEF FPC}@{$ENDIF}dwBytesNeeded);

            if (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
            begin
                pQSC := MemAlloc (dwBytesNeeded);
                dwBufSize := dwBytesNeeded;

                Win32Check (QueryServiceConfig (hService, pQSC, dwBufSize,
                                           {$IFDEF FPC}@{$ENDIF}dwBytesNeeded));

                SetValues (pQSC^);
            end { if }
            else RaiseLastWin32Error;

            if (Win32MajorVersion >= 6) then
                QueryConfig2 (hService);

        finally
            if (pQSC <> NIL) then
                MemDispose (Pointer (pQSC));

            VerifyApi (CloseServiceHandle (hService));
        end; { try / finally }
    end; { QueryConfig }

    (* ---- *)

const
	cnMaxServices = 65536 div SizeOf (TEnumServiceStatus);

{$IFDEF FPC}
const
    SERVICE_STATE_ALL = SERVICE_ACTIVE or SERVICE_INACTIVE;
{$ENDIF}

type
  	TaServiceStatus = array [0..cnMaxServices] of TEnumServiceStatus;
    PTaServiceStatus = ^TaServiceStatus;

var
	iIndex : Integer;
    hSCM : SC_Handle;
    dwResumeHandle, dwBufSize, dwBytesNeeded, dwServicesReturned, dwType : DWord;
    paServiceStatus : PTaServiceStatus;
    pESS : PEnumServiceStatus;
    Service : TService;
    bEnumResult : Boolean;

begin { TServices.RefreshList }
    hSCM := OpenSCManager (NIL, NIL, SC_MANAGER_ENUMERATE_SERVICE);

    if (hSCM = 0) then
    	RaiseLastWin32Error;

    try
        dwResumeHandle := 0;
        bEnumResult := false;

        if (bIncludeDrivers) then
            dwType := SERVICE_WIN32 or SERVICE_DRIVER
        else dwType := SERVICE_WIN32;

        repeat
            pESS := NIL;
            dwBufSize := 0;

            EnumServicesStatus (hSCM, dwType, SERVICE_STATE_ALL,
                                pESS{$IFNDEF FPC}^{$ENDIF}, dwBufSize,
                                {$IFDEF FPC}@{$ENDIF}dwBytesNeeded,
                                {$IFDEF FPC}@{$ENDIF}dwServicesReturned,
                                {$IFDEF FPC}@{$ENDIF}dwResumeHandle);

            if (GetLastError = ERROR_MORE_DATA) then
            begin
                paServiceStatus := MemAlloc (dwBytesNeeded);

                try
                    pESS := PEnumServiceStatus (paServiceStatus);
                    dwBufSize := dwBytesNeeded;

                    bEnumResult := EnumServicesStatus (hSCM, dwType,
                                                       SERVICE_STATE_ALL,
                                                     pESS{$IFNDEF FPC}^{$ENDIF},
                                                     dwBufSize,
                                        {$IFDEF FPC}@{$ENDIF}dwBytesNeeded,
                                        {$IFDEF FPC}@{$ENDIF}dwServicesReturned,
                                        {$IFDEF FPC}@{$ENDIF}dwResumeHandle);

                    if (bEnumResult) then
                        for iIndex := 0 to dwServicesReturned - 1 do
                        begin
                            Service := GetServiceStatus (paServiceStatus^[iIndex]);
                            QueryConfig (hSCM, Service);
                        end { for }
                    else if (GetLastError <> ERROR_MORE_DATA) then
                        RaiseLastWin32Error;

                finally
                    MemDispose (Pointer (paServiceStatus));
                end; { try / finally }
            end { if }
            else RaiseLastWin32Error;
        until (bEnumResult);

    finally
        VerifyApi (CloseServiceHandle (hSCM));
    end; { try / finally }
end; { TServices.RefreshList }

(* ---- *)

constructor TServices.Create (const bIncludeDrivers: Boolean = false);
begin
	inherited Create;

    Self.bIncludeDrivers := bIncludeDrivers;
//    b_x64 := IsWindows_x64;

    ServiceList := CreateSortedStringList (dupAccept);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    	RefreshList;
end; { TServices.Create }

(* ---- *)

destructor TServices.Destroy;

var
	iIndex : Integer;

begin
	for iIndex := 0 to ServiceList.Count - 1 do
    	ServiceList.Objects [iIndex].Free;

    ServiceList.Free;

  	inherited;
end; { TServices.Destroy }

(* ---- *)

function TServices.GetEnumerator : TServicesEnumerator;
begin
	Result := TServicesEnumerator.Create (Self);
end; { TServices.GetEnumerator }

(* ---- *)

function TServices.ServiceRunning (const sName: String) : Boolean;

var
	iIndex : Integer;

begin
	Assert (sName <> '');

	Result := ServiceList.Find (Trim (sName), iIndex);
end; { TServices.ServiceRunning }

(* ---- *)

end.

