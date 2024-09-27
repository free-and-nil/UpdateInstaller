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

{$MINENUMSIZE 4}

unit NativeApi_ImportU;

interface

uses Windows,
{$IFNDEF FPC}
     BaseTypesU,
{$ENDIF}
	 WinNT_ImportU;

type
	NTSTATUS = LONG;

{$IFNDEF FPC}
  {$IFNDEF DELPHI2010_UP}
    USHORT = Word;
  {$ENDIF}
  {$IFNDEF DELPHI2007_UP}
    ULONGLONG = UInt64;
  {$ENDIF}
{$ENDIF}

const
	SystemProcessIdInformation = 88;
	STATUS_SUCCESS = NTSTATUS($00000000);
    STATUS_INFO_LENGTH_MISMATCH = NTSTATUS($C0000004);
    STATUS_BUFFER_TOO_SMALL = NTSTATUS($C0000023);

	VARIABLE_INFORMATION_NAMES = 1;
	VARIABLE_INFORMATION_VALUES = 2;

type
	PVOID = Pointer;

    _SYSTEM_INFORMATION_CLASS = (
      SystemBasicInformation,
      SystemProcessorInformation,
      SystemPerformanceInformation,
      SystemTimeOfDayInformation,
      SystemNotImplemented1,
      SystemProcessesAndThreadsInformation,
      SystemCallCounts,
      SystemConfigurationInformation,
      SystemProcessorTimes,
      SystemGlobalFlag,
      SystemNotImplemented2,
      SystemModuleInformation,
      SystemLockInformation,
      SystemNotImplemented3,
      SystemNotImplemented4,
      SystemNotImplemented5,
      SystemHandleInformation,
      SystemObjectInformation,
      SystemPagefileInformation,
      SystemInstructionEmulationCounts,
      SystemInvalidInfoClass1,
      SystemCacheInformation,
      SystemPoolTagInformation,
      SystemProcessorStatistics,
      SystemDpcInformation,
      SystemNotImplemented6,
      SystemLoadImage,
      SystemUnloadImage,
      SystemTimeAdjustment,
      SystemNotImplemented7,
      SystemNotImplemented8,
      SystemNotImplemented9,
      SystemCrashDumpInformation,
      SystemExceptionInformation,
      SystemCrashDumpStateInformation,
      SystemKernelDebuggerInformation,
      SystemContextSwitchInformation,
      SystemRegistryQuotaInformation,
      SystemLoadAndCallImage,
      SystemPrioritySeparation,
      SystemNotImplemented10,
      SystemNotImplemented11,
      SystemInvalidInfoClass2,
      SystemInvalidInfoClass3,
      SystemTimeZoneInformation,
      SystemLookasideInformation,
      SystemSetTimeSlipEvent,
      SystemCreateSession,
      SystemDeleteSession,
      SystemInvalidInfoClass4,
      SystemRangeStartInformation,
      SystemVerifierInformation,
      SystemAddVerifier,
      SystemSessionProcessesInformation);
    SYSTEM_INFORMATION_CLASS = _SYSTEM_INFORMATION_CLASS;
    TSystemInformationClass = SYSTEM_INFORMATION_CLASS;

    _KEY_INFORMATION_CLASS = (
      KeyBasicInformation,
      KeyNodeInformation,
      KeyFullInformation,
      KeyNameInformation);
    KEY_INFORMATION_CLASS = _KEY_INFORMATION_CLASS;
    TKeyInformationClass = KEY_INFORMATION_CLASS;

  	_KEY_NAME_INFORMATION = record
    	NameLength : ULong;
        Name : array [0..0] of WChar;
    end; { _KEY_NAME_INFORMATION }
    KEY_NAME_INFORMATION = _KEY_NAME_INFORMATION;
    PKEY_NAME_INFORMATION = ^_KEY_NAME_INFORMATION;
    TKeyNameInformation = _KEY_NAME_INFORMATION;
    PTKeyNameInformation = ^TKeyNameInformation;

	_UNICODE_STRING = record
      Length: USHORT;
      MaximumLength: USHORT;
      Buffer: PWideChar;
    end; { _UNICODE_STRING }
    UNICODE_STRING = _UNICODE_STRING;
    PUNICODE_STRING = ^UNICODE_STRING;
    PCUNICODE_STRING = PUNICODE_STRING;
    TUnicodeString = UNICODE_STRING;
    PUnicodeString = ^TUnicodeString;

    _SYSTEM_BASIC_INFORMATION = record // Information Class 0
      Unknown: ULONG;
      MaximumIncrement: ULONG;
      PhysicalPageSize: ULONG;
      NumberOfPhysicalPages: ULONG;
      LowestPhysicalPage: ULONG;
      HighestPhysicalPage: ULONG;
      AllocationGranularity: ULONG;
      LowestUserAddress: ULONG;
      HighestUserAddress: ULONG;
      ActiveProcessors: ULONG;
      NumberProcessors: UCHAR;
    end;
    SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
    PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
    TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
    PSystemBasicInformation = ^TSystemBasicInformation;

    _SYSTEM_PERFORMANCE_INFORMATION = record // Information Class 2
      IdleTime: LARGE_INTEGER;
      ReadTransferCount: LARGE_INTEGER;
      WriteTransferCount: LARGE_INTEGER;
      OtherTransferCount: LARGE_INTEGER;
      ReadOperationCount: ULONG;
      WriteOperationCount: ULONG;
      OtherOperationCount: ULONG;
      AvailablePages: ULONG;
      TotalCommittedPages: ULONG;
      TotalCommitLimit: ULONG;
      PeakCommitment: ULONG;
      PageFaults: ULONG;
      WriteCopyFaults: ULONG;
      TransistionFaults: ULONG;
      Reserved1: ULONG;
      DemandZeroFaults: ULONG;
      PagesRead: ULONG;
      PageReadIos: ULONG;
      Reserved2: array[0..1] of ULONG;
      PagefilePagesWritten: ULONG;
      PagefilePageWriteIos: ULONG;
      MappedFilePagesWritten: ULONG;
      MappedFilePageWriteIos: ULONG;
      PagedPoolUsage: ULONG;
      NonPagedPoolUsage: ULONG;
      PagedPoolAllocs: ULONG;
      PagedPoolFrees: ULONG;
      NonPagedPoolAllocs: ULONG;
      NonPagedPoolFrees: ULONG;
      TotalFreeSystemPtes: ULONG;
      SystemCodePage: ULONG;
      TotalSystemDriverPages: ULONG;
      TotalSystemCodePages: ULONG;
      SmallNonPagedLookasideListAllocateHits: ULONG;
      SmallPagedLookasideListAllocateHits: ULONG;
      Reserved3: ULONG;
      MmSystemCachePage: ULONG;
      PagedPoolPage: ULONG;
      SystemDriverPage: ULONG;
      FastReadNoWait: ULONG;
      FastReadWait: ULONG;
      FastReadResourceMiss: ULONG;
      FastReadNotPossible: ULONG;
      FastMdlReadNoWait: ULONG;
      FastMdlReadWait: ULONG;
      FastMdlReadResourceMiss: ULONG;
      FastMdlReadNotPossible: ULONG;
      MapDataNoWait: ULONG;
      MapDataWait: ULONG;
      MapDataNoWaitMiss: ULONG;
      MapDataWaitMiss: ULONG;
      PinMappedDataCount: ULONG;
      PinReadNoWait: ULONG;
      PinReadWait: ULONG;
      PinReadNoWaitMiss: ULONG;
      PinReadWaitMiss: ULONG;
      CopyReadNoWait: ULONG;
      CopyReadWait: ULONG;
      CopyReadNoWaitMiss: ULONG;
      CopyReadWaitMiss: ULONG;
      MdlReadNoWait: ULONG;
      MdlReadWait: ULONG;
      MdlReadNoWaitMiss: ULONG;
      MdlReadWaitMiss: ULONG;
      ReadAheadIos: ULONG;
      LazyWriteIos: ULONG;
      LazyWritePages: ULONG;
      DataFlushes: ULONG;
      DataPages: ULONG;
      ContextSwitches: ULONG;
      FirstLevelTbFills: ULONG;
      SecondLevelTbFills: ULONG;
      SystemCalls: ULONG;
    end;
    SYSTEM_PERFORMANCE_INFORMATION = _SYSTEM_PERFORMANCE_INFORMATION;
    PSYSTEM_PERFORMANCE_INFORMATION = ^SYSTEM_PERFORMANCE_INFORMATION;
    TSystemPerformanceInformation = SYSTEM_PERFORMANCE_INFORMATION;
    PSystemPerformanceInformation = ^TSystemPerformanceInformation;

    // http://www.exploit-monday.com/2013/06/undocumented-ntquerysysteminformation.html
    _SYSTEM_TIME_OF_DAY_INFORMATION = record // Information Class 3
      BootTime: LARGE_INTEGER;
      CurrentTime: LARGE_INTEGER;
      TimeZoneBias: LARGE_INTEGER;
      CurrentTimeZoneId: ULONG;
      Reserved: ULONG;
      BootTimeBias: ULONGLONG;
      SleepTimeBias: ULONGLONG;
    end;
    SYSTEM_TIME_OF_DAY_INFORMATION = _SYSTEM_TIME_OF_DAY_INFORMATION;
    PSYSTEM_TIME_OF_DAY_INFORMATION = ^SYSTEM_TIME_OF_DAY_INFORMATION;

    TSystemTimeOfDayInformation = SYSTEM_TIME_OF_DAY_INFORMATION;
    PSystemTimeOfDayInformation = ^TSystemTimeOfDayInformation;

      // http://www.delphipraxis.net/35225-enumprocesses-2.html
   	_SYSTEM_PROCESS_INFORMATION = record
        NextEntryOffset: ULONG;
        NumberOfThreads: ULONG;
        SpareLi1, SpareLi2, SpareLi3: TLargeInteger;
        CreateTime, UserTime, KernelTime: TLargeInteger;
        ImageName: UNICODE_STRING;
        BasePriority: ULONG;
        UniqueProcessId: THandle;
        InheritedFromUniqueProcessId: THandle;
        HandleCount: ULONG;
        SessionId: ULONG;
        SpareUl3: ULONG;
        PeekVirtualSize: ULONG;
        VirtualSize: ULONG;
        PageFaultCount: ULONG;
        PeakWorkingSetSize: ULONG;
        WorkingSetSize: ULONG;
        QuotaPeakPagedPoolUsage: ULONG;
        QuotaPagedPoolUsage: ULONG;
        QuotaPeakNonPagedPoolUsage: ULONG;
        QuotaNonPagedPoolUsage: ULONG;
        PagefileUsage: ULONG;
        PeakPagefileUsage: ULONG;
        PrivatePageCount: ULONG;
    end; { _SYSTEM_PROCESS_INFORMATION }

(**
	_SYSTEM_INFORMATION_CLASS = DWORD;
    SYSTEM_INFORMATION_CLASS = _SYSTEM_INFORMATION_CLASS;
**)

    PTSystemProcessInformation = ^TSystemProcessInformation;
    TSystemProcessInformation = _SYSTEM_PROCESS_INFORMATION;

	_SYSTEM_PROCESS_ID_INFORMATION = record
    	ProcessId : THandle;
        ImageName : Unicode_String;
    end; { _SYSTEM_PROCESS_ID_INFORMATION }
    PTSystemProcessIdInformation = ^TSystemProcessIdInformation;
    TSystemProcessIdInformation = _SYSTEM_PROCESS_ID_INFORMATION;

function NT_SUCCESS (Status: NTSTATUS) : BOOL;

var
	// http://www.codewarrior.cn/ntdoc/winxp/ex/NtEnumerateSystemEnvironmentValuesEx.htm
    NtEnumerateSystemEnvironmentValuesEx :
        function (InformationClass: UInt; Buffer: Pointer;
                  var BufferLen: DWord) : NTSTATUS; stdcall = NIL;

	NtQueryKey : function (KeyHandle: THandle;
    					   KeyInformationClass: KEY_INFORMATION_CLASS;
                           KeyInformation: PVOID; KeyInformationLength: ULONG;
                           ResultLength: PULONG) : NTSTATUS; stdcall = NIL;

	{ SystemTime : UTC time represented as 8 bytes length integer. This value
    			   means number of 100-nanosecond units since 1600, 1 January.
                   Time is incremented 10.000.000 times per second. }
	NtQuerySystemTime : function (var SystemTime: TLargeInteger) : NTSTATUS;
    														      stdcall = NIL;

	NtQuerySystemInformation : function (
    						   SystemInformationClass: TSystemInformationClass;
                               SystemInformation: PVOID;
                               SystemInformationLength: ULONG;
                               ReturnLength: PULONG) : NTSTATUS; stdcall = NIL;

    // RTL_OSVERSIONINFOW = OSVersionInfoW;
    // RTL_OSVERSIONINFOEXW = OSVersionInfoExW
    RtlGetVersion : function (var lpVersionInfo: TOSVersionInfoW) : NTSTATUS;
    														      stdcall = NIL;

implementation

uses SysUtils;

const
	cNtDll = 'ntdll.dll';

var
	hNtDll : THandle = 0;

(* ---- *)

function NT_SUCCESS (Status: NTSTATUS) : BOOL;
begin
	Result := Status >= 0;
end; { NT_SUCCESS }

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
	if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Win32MajorVersion < 4) then
        exit;

    hNtDll := LoadLibrary (cNtDll);

    if (hNtDll = 0) then
        exit;

    NtQueryKey := GPA (hNtDll, 'NtQueryKey');
    NtQuerySystemInformation := GPA (hNtDll, 'NtQuerySystemInformation');
    NtQuerySystemTime := GPA (hNtDll, 'NtQuerySystemTime');

    if (Win32MajorVersion >= 5) then
    begin
        RtlGetVersion := GPA (hNtDll, 'RtlGetVersion');

        if (Win32MinorVersion >= 1) or (Win32MajorVersion >= 6) then
            NtEnumerateSystemEnvironmentValuesEx := GPA (hNtDll,
                                        'NtEnumerateSystemEnvironmentValuesEx');
    end; { if }
end; { initialization }

(* ---- *)

finalization
begin
    if (hNtDll <> 0) then
    	FreeLibrary (hNtDll);
end; { finalization }

(* ---- *)

end.
