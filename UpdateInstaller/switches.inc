(**
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}

    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_LIBRARY OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
    {$WARN UNIT_DEPRECATED OFF}
    {$WARN UNIT_LIBRARY OFF}
    {$WARN UNIT_PLATFORM OFF}

    {$WARN GARBAGE OFF}  // Removes warning about code behind end.
**)

{$IFDEF VER70}  {* Borland Pascal 7 *}
	{$DEFINE BP7}
    {$DEFINE 16BIT}
{$ENDIF}  {* Borland Pascal 7 *}

{$IFDEF VER80}  {* Delphi 1.0 *}
	{$DEFINE DELPHI1}
    {$DEFINE 16BIT}
{$ENDIF}  {* Delphi 1.0 *}

{$IFDEF VER90}  {* Delphi 2.0 *}
	{$DEFINE DELPHI2}
{$ENDIF}  {* Delphi 2.0 *}

{$IFDEF VER100}  {* Delphi 3.0 *}
	{$DEFINE DELPHI3}
{$ENDIF}  {* Delphi 3.0 *}

{$IFDEF VER120}  {* Delphi 4.0 *}
	{$DEFINE DELPHI4}
{$ENDIF}  {* Delphi 4.0 *}

{$IFDEF VER130}  {* Delphi 5.0 *}
	{$DEFINE DELPHI5}
{$ENDIF}  {* Delphi 5.0 *}

{$IFDEF VER140}  {* Delphi 6.0 *}
	{$DEFINE DELPHI6}
{$ENDIF}  {* Delphi 6.0 *}

{$IFDEF VER150}  {* Delphi 7.0 *}
// RTLVersion = 15.00;
	{$DEFINE DELPHI7}
{$ENDIF}  {* Delphi 7.0 *}

{$IFDEF VER160}  {* Delphi 8.0 for .net *}
	{$DEFINE DELPHI8}
{$ENDIF}  {* Delphi 8.0 for .net *}

{$IFDEF VER170}  {* Delphi 2005 / 9.0 for .net & Win32 *}
	{$DEFINE DELPHI2005}
{$ENDIF}  {* Delphi 2005 / 9.0 for .net & Win32 *}

{$IFDEF VER180}  // Delphi 2006 / Turbo Delphi / Delphi 2007
// RTLVersion = 18.00;
	{$DEFINE DELPHI2006}

    {$IFDEF VER185}  // Delphi 2007 only
        {* Delphi 2007 for Win32 *}
		{$DEFINE DELPHI2007}
	{$ENDIF}
{$ENDIF}  // Delphi 2006 / Turbo Delphi / Delphi 2007

{$IFDEF VER200}  {* Delphi 2009 for Win32 *}
// RTLVersion = 20.00;
	{$DEFINE DELPHI2009}

    {$STRINGCHECKS OFF}
{$ENDIF}  {* Delphi 2009 for Win32 *}

{$IFDEF VER210}  {* Delphi 2010 for Win32 *}
// RTLVersion = 21.00;
	{$DEFINE DELPHI2010}

    // http://docwiki.embarcadero.com/RADStudio/en/WEAKLINKRTTI_directive_%28Delphi%29
    // http://docwiki.embarcadero.com/RADStudio/en/RTTI_directive_%28Delphi%29
    // To turn RTTI off try putting these two lines at the top of your DPR, before the USES clause:
    // http://tech.turbu-rpg.com/279/smaller-cleaner-rtti-probably-not-coming
    // {$WEAKLINKRTTI ON}
    // {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}


    // {$HIGHCHARUNICODE ON}
    // http://docwiki.embarcadero.com/RADStudio/en/HIGHCHARUNICODE_directive_%28Delphi%29
    // When HIGHCHARUNICODE is ON: #$xx 2-digit and #$xxxx 4-digit literals are parsed as WideChar.
{$ENDIF}  {* Delphi 2010 for Win32 *}

{$IFDEF VER220}  {* Delphi XE for Win32 *}
// RTLVersion = 22.00;
// COMPILERVERSION = 22.0

	{$DEFINE DELPHI_XE}
{$ENDIF}  {* Delphi XE for Win32 *}

{$IFDEF VER230}  {* Delphi XE2 for Win32 *}
// RTLVersion = 23.00;
// COMPILERVERSION = 23.0
//
	{$DEFINE DELPHI_XE2}
{$ENDIF}  {* Delphi XE2 for Win32 *}

{$IFDEF VER240}  {* Delphi XE3 for Win32 *}
// RTLVersion = 24.00;
// COMPILERVERSION = 24.0
//
	{$DEFINE DELPHI_XE3}
{$ENDIF}  {* Delphi XE3 for Win32 *}

{$IFDEF VER250}  {* Delphi XE4 for Win32 *}
// RTLVersion = 25.00;
// COMPILERVERSION = 25.0
//
	{$DEFINE DELPHI_XE4}
{$ENDIF}  {* Delphi XE4 for Win32 *}

{$IFDEF VER260}  {* Delphi XE5 for Win32 *}
// RTLVersion = 26.00;
// COMPILERVERSION = 26.0
//
	{$DEFINE DELPHI_XE5}
{$ENDIF}  {* Delphi XE5 for Win32 *}

{$IFDEF VER270}  {* Delphi XE6 for Win32 *}
// RTLVersion = 27.00;
// COMPILERVERSION = 27.0

	{$DEFINE DELPHI_XE6}
{$ENDIF}  {* Delphi XE6 for Win32 *}

{$IFDEF VER280}  {* Delphi XE7 for Win32 *}
// RTLVersion = 28.00;
// COMPILERVERSION = 28.0

	{$DEFINE DELPHI_XE7}
{$ENDIF}  {* Delphi XE7 for Win32 *}

{$IFDEF VER290}  {* Delphi XE8 for Win32 *}
// RTLVersion = 29.00;
// COMPILERVERSION = 29.0

	{$DEFINE DELPHI_XE8}
{$ENDIF}  {* Delphi XE8 for Win32 *}

{$IFDEF VER300}  {* Delphi 10 Seattle *}
// RTLVersion = 30.00;
// COMPILERVERSION = 30.00

	{$DEFINE DELPHI_10_0}
{$ENDIF}  {* Delphi 10 Seattle *}

{$IFDEF VER310}  {* Delphi 10.1 Berlin *}
// RTLVersion = 31.00;
// COMPILERVERSION = 31.00

	{$DEFINE DELPHI_10_1}
{$ENDIF}  {* Delphi 10.1 Berlin *}

{$IFDEF VER320}  {* Delphi 10.2 Tokyo *}
// RTLVersion = 32.00;
// COMPILERVERSION = 32.00

	{$DEFINE DELPHI_10_2}
{$ENDIF}  {* Delphi 10.2 Tokyo *}

{$IFDEF VER330}  {* Delphi 10.3 Rio *}
// RTLVersion = 33.00;
// COMPILERVERSION = 33.00

	{$DEFINE DELPHI_10_3}
{$ENDIF}  {* Delphi 10.3 Rio *}

{$IFDEF VER340}  {* Delphi 10.4 Sydney *}
// RTLVersion = 33.00;
// COMPILERVERSION = 33.00

	{$DEFINE DELPHI_10_4}
{$ENDIF}  {* Delphi 10.4 Sydney *}

{$IFDEF DELPHI_10_4} {$DEFINE DELPHI_10_4_UP} {$DEFINE DELPHI_10_3} {$ENDIF}
{$IFDEF DELPHI_10_3} {$DEFINE DELPHI_10_3_UP} {$DEFINE DELPHI_10_2} {$ENDIF}
{$IFDEF DELPHI_10_2} {$DEFINE DELPHI_10_2_UP} {$DEFINE DELPHI_10_1} {$ENDIF}
{$IFDEF DELPHI_10_1} {$DEFINE DELPHI_10_1_UP} {$DEFINE DELPHI_10_0} {$ENDIF}
{$IFDEF DELPHI_10_0} {$DEFINE DELPHI_10_0_UP} {$DEFINE DELPHI_XE8} {$ENDIF}
{$IFDEF DELPHI_XE8} {$DEFINE DELPHI_XE8_UP} {$DEFINE DELPHI_XE7} {$ENDIF}
{$IFDEF DELPHI_XE7} {$DEFINE DELPHI_XE7_UP} {$DEFINE DELPHI_XE6} {$ENDIF}
{$IFDEF DELPHI_XE6} {$DEFINE DELPHI_XE6_UP} {$DEFINE DELPHI_XE5} {$ENDIF}
{$IFDEF DELPHI_XE5} {$DEFINE DELPHI_XE5_UP} {$DEFINE DELPHI_XE4} {$ENDIF}
{$IFDEF DELPHI_XE4} {$DEFINE DELPHI_XE4_UP} {$DEFINE DELPHI_XE3} {$ENDIF}
{$IFDEF DELPHI_XE3} {$DEFINE DELPHI_XE3_UP} {$DEFINE DELPHI_XE2} {$ENDIF}
{$IFDEF DELPHI_XE2} {$DEFINE DELPHI_XE2_UP} {$DEFINE DELPHI_XE} {$ENDIF}
{$IFDEF DELPHI_XE} {$DEFINE DELPHI_XE_UP} {$DEFINE DELPHI2010} {$ENDIF}
{$IFDEF DELPHI2010} {$DEFINE DELPHI2010_UP} {$DEFINE DELPHI2009} {$ENDIF}
{$IFDEF DELPHI2009} {$DEFINE DELPHI2009_UP} {$DEFINE DELPHI2007} {$ENDIF}
{$IFDEF DELPHI2007} {$DEFINE DELPHI2007_UP} {$DEFINE DELPHI2006} {$ENDIF}
{$IFDEF DELPHI2006} {$DEFINE DELPHI2006_UP} {$DEFINE DELPHI2005} {$ENDIF}
{$IFDEF DELPHI2005} {$DEFINE DELPHI2005_UP} {$DEFINE DELPHI8} {$ENDIF}
{$IFDEF DELPHI8} {$DEFINE DELPHI8_UP} {$DEFINE DELPHI7} {$ENDIF}
{$IFDEF DELPHI7} {$DEFINE DELPHI7_UP} {$DEFINE DELPHI6} {$ENDIF}
{$IFDEF DELPHI6} {$DEFINE DELPHI6_UP} {$DEFINE DELPHI5} {$ENDIF}
{$IFDEF DELPHI5} {$DEFINE DELPHI5_UP} {$DEFINE DELPHI4} {$ENDIF}
{$IFDEF DELPHI4} {$DEFINE DELPHI4_UP} {$DEFINE DELPHI3} {$ENDIF}
{$IFDEF DELPHI3} {$DEFINE DELPHI3_UP} {$DEFINE DELPHI2} {$ENDIF}
{$IFDEF DELPHI2} {$DEFINE DELPHI2_UP} {$DEFINE DELPHI1} {$ENDIF}
{$IFDEF DELPHI1} {$DEFINE DELPHI1_UP} {$ENDIF}

{$IFDEF FPC}
//	{$MODE DELPHI}
//	{$MODE OBJFPC}
	{$LONGSTRINGS ON}  // $H+
	{$DEFINE DELPHI2_UP}
	{$DEFINE DELPHI3_UP}
	{$DEFINE DELPHI4_UP}
	{$DEFINE DELPHI5_UP}
	{$DEFINE DELPHI6_UP}
	{$DEFINE DELPHI7_UP}
	{$DEFINE DELPHI2005_UP}
	{$DEFINE DELPHI2006_UP}
	{$DEFINE DELPHI2007_UP}
    {$DEFINE SUPPORTS_POINTERMATH}
{$ENDIF}

{$IFDEF DELPHI3_UP}
    {$DEFINE SUPPORTS_ASSERT}
{$ENDIF}

{$IFDEF DELPHI4_UP}
    {$DEFINE SUPPORTS_DEFAULTPARAMS}
    {$DEFINE SUPPORTS_INT64}
    {$DEFINE SUPPORTS_OVERLOAD}
{$ENDIF}

{$IFDEF DELPHI2005_UP}
    {$DEFINE SUPPORTS_REGION}
{$ENDIF}

{$IFDEF DELPHI2007_UP}
    {$DEFINE SUPPORTS_INLINE}
{$ENDIF}

{$IFDEF DELPHI2009_UP}
    {$DEFINE SUPPORTS_POINTERMATH}
{$ENDIF}

{$IFDEF SUPPORTS_POINTERMATH}
	{$POINTERMATH ON}
{$ENDIF}

{$ALIGN ON}            // $A+
{$BOOLEVAL OFF}        // $B-
{$IOCHECKS OFF}        // $I-
{$OPENSTRINGS OFF}     // $P-
{$TYPEDADDRESS ON}     // $T+
{$VARSTRINGCHECKS OFF} // $V-
{$EXTENDEDSYNTAX ON}   // $X+

{$IFDEF DEBUG}
    {$DEBUGINFO ON}      // $D+
    {$ASSERTIONS ON}     // $C+
    {$OVERFLOWCHECKS ON} // $Q+
    {$RANGECHECKS ON}    // $R+
    {$STACKFRAMES ON}    // $W+

    {$IFNDEF FPC}
        {$IMPORTEDDATA ON}  // $L+
        {$REFERENCEINFO ON} // $Y+
    {$ENDIF FPC}

    {$IFDEF 16BIT}
    	{$S+} // Stack-Overflow Checking (16 bit)
    {$ENDIF DELPHI2}
{$ELSE}  // {$IFDEF DEBUG}
    {$ASSERTIONS OFF}    // $C-
    {$OVERFLOWCHECKS OFF} // $Q-
    {$RANGECHECKS OFF}    // $R-

    {$IFDEF DEBUGINFO}
        {$IFNDEF FPC}
            {$IMPORTEDDATA ON}  // $L+
            {$REFERENCEINFO ON} // $Y+
        {$ENDIF FPC}

        {$DEBUGINFO ON}   // $D+
        {$STACKFRAMES ON} // $W+
    {$ELSE}  // {$IFDEF DEBUGINFO}
        {$IFNDEF FPC}
            {$IMPORTEDDATA OFF}  // $L-
            {$REFERENCEINFO OFF} // $Y-
        {$ENDIF FPC}

        {$DEBUGINFO OFF}      // $D-
        {$STACKFRAMES OFF}    // $W-
    {$ENDIF}  // {$IFDEF DEBUGINFO}

	{$IFDEF madExcept}
		{$STACKFRAMES ON}    // $W+
	{$ENDIF}

    {$IFNDEF FPC}
        {$REFERENCEINFO OFF} // $Y-
    {$ENDIF}

	{$IFNDEF DELPHI2}
    	{$S-} // Stack-Overflow Checking (16 bit)
    {$ENDIF DELPHI2}
{$ENDIF}  // {$IFDEF DEBUG}

{$IFDEF DELPHI2_UP}
    {$WRITEABLECONST ON} // $J+

    {$IFDEF DEBUG}
        {$OPTIMIZATION OFF}
    {$ELSE}
        {$IFDEF NO_OPT}
            {$OPTIMIZATION OFF}
        {$ELSE}
            {$OPTIMIZATION ON}
        {$ENDIF}
    {$ENDIF}

    {$LONGSTRINGS ON}
    {$HINTS ON}
    {$WARNINGS ON}
{$ELSE}  // {$IFDEF DELPHI2}
	// Switches valid for 16 bit compilers
    {$E+} // Emulation Switch
    {$F-} // Force Far Calls Switch
    {$K+} // Smart Callbacks Switch
    {$N-} // Numeric Coprocessor Switch
    {$G+} // Generate 80286 Code Switch
{$ENDIF}  // {$IFDEF DELPHI2}

{$IFNDEF FPC}
    {$IFDEF DELPHI7_UP}
	    {$IFNDEF CLR}
	        {$WARN UNSAFE_CODE OFF}
	        {$WARN UNSAFE_TYPE OFF}
	        {$WARN UNSAFE_CAST OFF}
	    {$ENDIF}
    {$ENDIF}

    {$C Moveable Demandload Discardable} { Code Segment attributes }
{$ENDIF}

// EOF SWITCHES.INC
