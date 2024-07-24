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

unit Regstr_ImportU;

interface

const
    CONFIGFLAG_DISABLED              = $00000001;  // Set if disabled
    CONFIGFLAG_REMOVED               = $00000002;  // Set if a present hardware enum device deleted
    CONFIGFLAG_MANUAL_INSTALL        = $00000004;  // Set if the devnode was manually installed
    CONFIGFLAG_IGNORE_BOOT_LC        = $00000008;  // Set if skip the boot config
    CONFIGFLAG_NET_BOOT              = $00000010;  // Load this devnode when in net boot
    CONFIGFLAG_REINSTALL             = $00000020;  // Redo install
    CONFIGFLAG_FAILEDINSTALL         = $00000040;  // Failed the install
    CONFIGFLAG_CANTSTOPACHILD        = $00000080;  // Can't stop/remove a single child
    CONFIGFLAG_OKREMOVEROM           = $00000100;  // Can remove even if rom.
    CONFIGFLAG_NOREMOVEEXIT          = $00000200;  // Don't remove at exit.
    CONFIGFLAG_FINISH_INSTALL        = $00000400;  // Complete install for devnode running 'raw'
    CONFIGFLAG_NEEDS_FORCED_CONFIG   = $00000800;  // This devnode requires a forced config
    CONFIGFLAG_NETBOOT_CARD          = $00001000;  // This is the remote boot network card
    CONFIGFLAG_PARTIAL_LOG_CONF      = $00002000;  // This device has a partial logconfig
    CONFIGFLAG_SUPPRESS_SURPRISE     = $00004000;  // Set if unsafe removals should be ignored
    CONFIGFLAG_VERIFY_HARDWARE       = $00008000;  // Set if hardware should be tested for logo failures
    CONFIGFLAG_FINISHINSTALL_UI      = $00010000;  // Show the finish install wizard pages for the installed device.
    CONFIGFLAG_FINISHINSTALL_ACTION  = $00020000;  // Call installer with DIF_FINISHINSTALL_ACTION in client context.

implementation

end.

