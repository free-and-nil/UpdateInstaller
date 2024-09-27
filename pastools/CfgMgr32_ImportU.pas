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

unit CfgMgr32_ImportU;

{$MINENUMSIZE 4}

interface

uses Windows;

const
    //--------------------------------------------------------------
    // Configuration Manager return status codes
    //--------------------------------------------------------------

    CR_SUCCESS                  = $00000000;
    CR_DEFAULT                  = $00000001;
    CR_OUT_OF_MEMORY            = $00000002;
    CR_INVALID_POINTER          = $00000003;
    CR_INVALID_FLAG             = $00000004;
    CR_INVALID_DEVNODE          = $00000005;
    CR_INVALID_DEVINST          = CR_INVALID_DEVNODE;
    CR_INVALID_RES_DES          = $00000006;
    CR_INVALID_LOG_CONF         = $00000007;
    CR_INVALID_ARBITRATOR       = $00000008;
    CR_INVALID_NODELIST         = $00000009;
    CR_DEVNODE_HAS_REQS         = $0000000A;
    CR_DEVINST_HAS_REQS         = CR_DEVNODE_HAS_REQS;
    CR_INVALID_RESOURCEID       = $0000000B;
    CR_DLVXD_NOT_FOUND          = $0000000C;   // WIN 95 ONLY
    CR_NO_SUCH_DEVNODE          = $0000000D;
    CR_NO_SUCH_DEVINST          = CR_NO_SUCH_DEVNODE;
    CR_NO_MORE_LOG_CONF         = $0000000E;
    CR_NO_MORE_RES_DES          = $0000000F;
    CR_ALREADY_SUCH_DEVNODE     = $00000010;
    CR_ALREADY_SUCH_DEVINST     = CR_ALREADY_SUCH_DEVNODE;
    CR_INVALID_RANGE_LIST       = $00000011;
    CR_INVALID_RANGE            = $00000012;
    CR_FAILURE                  = $00000013;
    CR_NO_SUCH_LOGICAL_DEV      = $00000014;
    CR_CREATE_BLOCKED           = $00000015;
    CR_NOT_SYSTEM_VM            = $00000016;   // WIN 95 ONLY
    CR_REMOVE_VETOED            = $00000017;
    CR_APM_VETOED               = $00000018;
    CR_INVALID_LOAD_TYPE        = $00000019;
    CR_BUFFER_SMALL             = $0000001A;
    CR_NO_ARBITRATOR            = $0000001B;
    CR_NO_REGISTRY_HANDLE       = $0000001C;
    CR_REGISTRY_ERROR           = $0000001D;
    CR_INVALID_DEVICE_ID        = $0000001E;
    CR_INVALID_DATA             = $0000001F;
    CR_INVALID_API              = $00000020;
    CR_DEVLOADER_NOT_READY      = $00000021;
    CR_NEED_RESTART             = $00000022;
    CR_NO_MORE_HW_PROFILES      = $00000023;
    CR_DEVICE_NOT_THERE         = $00000024;
    CR_NO_SUCH_VALUE            = $00000025;
    CR_WRONG_TYPE               = $00000026;
    CR_INVALID_PRIORITY         = $00000027;
    CR_NOT_DISABLEABLE          = $00000028;
    CR_FREE_RESOURCES           = $00000029;
    CR_QUERY_VETOED             = $0000002A;
    CR_CANT_SHARE_IRQ           = $0000002B;
    CR_NO_DEPENDENT             = $0000002C;
    CR_SAME_RESOURCES           = $0000002D;
    CR_NO_SUCH_REGISTRY_KEY     = $0000002E;
    CR_INVALID_MACHINENAME      = $0000002F;   // NT ONLY
    CR_REMOTE_COMM_FAILURE      = $00000030;   // NT ONLY
    CR_MACHINE_UNAVAILABLE      = $00000031;   // NT ONLY
    CR_NO_CM_SERVICES           = $00000032;   // NT ONLY
    CR_ACCESS_DENIED            = $00000033;   // NT ONLY
    CR_CALL_NOT_IMPLEMENTED     = $00000034;
    CR_INVALID_PROPERTY         = $00000035;
    CR_DEVICE_INTERFACE_ACTIVE  = $00000036;
    CR_NO_SUCH_DEVICE_INTERFACE = $00000037;
    CR_INVALID_REFERENCE_STRING = $00000038;
    CR_INVALID_CONFLICT_LIST    = $00000039;
    CR_INVALID_INDEX            = $0000003A;
    CR_INVALID_STRUCTURE_SIZE   = $0000003B;
    NUM_CR_RESULTS              = $0000003C;

type
    //
    // Device Instance Handle data type
    //
    PDEVNODE = ^DEVNODE;
    DEVNODE = DWORD;
    PDEVINST = ^DEVINST;
    DEVINST = DWORD;

	//
    // Standardized Return Value data type
    //
    RETURN_TYPE = DWORD;
    CONFIGRET = RETURN_TYPE;

var
	CM_Enumerate_Classes : function (ulClassIndex: ULONG; var ClassGuid: TGUID;
    								 ulFlags: ULONG) : CONFIGRET; stdcall = NIL;

    CM_Get_Device_ID : function (dnDevInst: DEVINST; Buffer: LPTSTR;
    							 BufferLen: ULONG;
                                 ulFlags: ULONG) : CONFIGRET; stdcall = NIL;

	CM_Get_DevNode_Status : function (var ulStatus: ULONG;
    								  var ulProblemNumber: ULONG;
                                      dnDevInst: DEVINST;
                                      ulFlags: ULONG) : CONFIGRET; stdcall = NIL;

    CM_Locate_DevNode : function (var dnDevInst: DWord; pDeviceID: PChar;
                                  ulFlags: ULONG) : DWord; stdcall = NIL;

    CM_Reenumerate_DevNode : function (dnDevInst: DWord;
                                       ulFlags: ULong) : DWord; stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU;

const
	cCfgMgr32 = 'cfgmgr32.dll';

{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}

var
    hCfgMgr32 : THandle = 0;

(* ---- *)

{$I .\GetProcAddress.inc}

(* ---- *)

initialization
begin
	hCfgMgr32 := LoadLibrary (cCfgMgr32);

    if (hCfgMgr32 = 0) then
    	exit;

    @CM_Enumerate_Classes := GPA (hCfgMgr32, 'CM_Enumerate_Classes');
//    CM_Get_Device_ID := GPA (hCfgMgr32, 'CM_Get_Device_ID');
    @CM_Get_DevNode_Status := GPA (hCfgMgr32, 'CM_Get_DevNode_Status');

    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
    begin  // Windows 2000 only
        @CM_Locate_DevNode := GPA (hCfgMgr32, 'CM_Locate_DevNode' + AWSuffix);
        @CM_Reenumerate_DevNode := GPA (hCfgMgr32, 'CM_Reenumerate_DevNode');
    end; { if }
end; { initialization }

(* ---- *)

finalization
begin
    if (hCfgMgr32 <> 0) then
    	VerifyApi (FreeLibrary (hCfgMgr32));
end; { finalization }

(* ---- *)

end.

