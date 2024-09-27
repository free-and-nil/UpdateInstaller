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

unit Cfg_ImportU;

interface

const
//
// Device Instance status flags, returned by call to CM_Get_DevInst_Status
//
     DN_ROOT_ENUMERATED = $00000001; // Was enumerated by ROOT
     DN_DRIVER_LOADED   = $00000002; // Has Register_Device_Driver
     DN_ENUM_LOADED     = $00000004; // Has Register_Enumerator
     DN_STARTED         = $00000008; // Is currently configured
     DN_MANUAL          = $00000010; // Manually installed
     DN_NEED_TO_ENUM    = $00000020; // May need reenumeration
     DN_NOT_FIRST_TIME  = $00000040; // Has received a config
     DN_HARDWARE_ENUM   = $00000080; // Enum generates hardware ID
     DN_LIAR            = $00000100; // Lied about can reconfig once
     DN_HAS_MARK        = $00000200; // Not CM_Create_DevInst lately
     DN_HAS_PROBLEM     = $00000400; // Need device installer
     DN_FILTERED        = $00000800; // Is filtered
     DN_MOVED           = $00001000; // Has been moved
     DN_DISABLEABLE     = $00002000; // Can be disabled
     DN_REMOVABLE       = $00004000; // Can be removed
     DN_PRIVATE_PROBLEM = $00008000; // Has a private problem
     DN_MF_PARENT       = $00010000; // Multi function parent
     DN_MF_CHILD        = $00020000; // Multi function child
     DN_WILL_BE_REMOVED = $00040000; // DevInst is being removed

//
// Windows 4 OPK2 Flags
//
     DN_NOT_FIRST_TIMEE  = $00080000; // S: Has received a config enumerate
     DN_STOP_FREE_RES    = $00100000; // S: When child is stopped, free resources
     DN_REBAL_CANDIDATE  = $00200000; // S: Don't skip during rebalance
     DN_BAD_PARTIAL      = $00400000; // S: This devnode's log_confs do not have same resources
     DN_NT_ENUMERATOR    = $00800000; // S: This devnode's is an NT enumerator
     DN_NT_DRIVER        = $01000000; // S: This devnode's is an NT driver
//
// Windows 4.1 Flags
//
     DN_NEEDS_LOCKING    = $02000000; // S: Devnode need lock resume processing
     DN_ARM_WAKEUP       = $04000000; // S: Devnode can be the wakeup device
     DN_APM_ENUMERATOR   = $08000000; // S: APM aware enumerator
     DN_APM_DRIVER       = $10000000; // S: APM aware driver
     DN_SILENT_INSTALL   = $20000000; // S: Silent install
     DN_NO_SHOW_IN_DM    = $40000000; // S: No show in device manager
     DN_BOOT_LOG_PROB    = $80000000; // S: Had a problem during preassignment of boot log conf

//
// Windows NT Flags
//
// These are overloaded on top of unused Win 9X flags
//
// DN_LIAR                       = $00000100)            // Lied about can reconfig once

// #if (NTDDI_VERSION >= NTDDI_WINXP)

    //  DN_NOT_FIRST_TIME            = $00000040)            // Has Register_Enumerator
     DN_DRIVER_BLOCKED             = DN_NOT_FIRST_TIME;      // One or more drivers are blocked from loading for this Devnode
    // DN_MOVED                      = $00001000)            // Has been moved
     DN_LEGACY_DRIVER              = DN_MOVED;               // This device is using a legacy driver
    // DN_HAS_MARK                   = $00000200)            // Not CM_Create_DevInst lately
     DN_CHILD_WITH_INVALID_ID      = DN_HAS_MARK;            // One or more children have invalid ID(s)

// #elif (NTDDI_VERSION >= NTDDI_WIN2K)

	 DN_NEED_RESTART     = $00000100;             // System needs to be restarted for this Devnode to work properly

implementation

end.

