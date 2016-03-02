Haste porting guide
===================

This document briefly describes the steps that need to be taken to port new
versions of base/GHC to Haste.

  * Custom GHC.Prim interface needs to be updated for the new GHC version.

  * GHC.HastePrim and Haste.Handle should be included in base.
  
  * GHC.IO.Handle should be updated to use Haste's definitions for handle
    functions.
  
  * Basic Show instances should make use of GHC.HastePrim for performance
    reasons. These are found in GHC.Float and GHC.Show.

  * System.CPUTime must be patched to not use `getrusage`.

  * hFlush, hPutStr, hPutStrLn, hPutChar, stdin, stdout and stderr should
    be imlemented using their counterparts in GHC.Handle.
    These are found in GHC.IO.Handle.Text, GHC.IO.Handle.FD and GHC.IO.Handle.

  * Word/pointer/etc. sizes need to be modified in ghcautoconf.h and MachDeps.h
    to match those expected of a 32 bit platform.

  * In the browser, Float and Double are the same thing. This means that the
    FLT_* constants in ieee-flpt.h must be modified to match their DBL_*
    counterparts.

To enable booting on Windows:

  * Ensure that Windows-specific primops are enabled when appropriate
    through appropriate #ifdefs in the file containing primOpInfo.

  * Enable any POSIX-specific foreign imports required to build OS stuff
    on Windows as well in base; the functionality depending on this is
    meaningless in the browser anyway, but we need the types for Haste.App and
    similar compatibility matters.
    This happens in GHC.IO.FD (isTerminal) and System.Posix.Internals.
