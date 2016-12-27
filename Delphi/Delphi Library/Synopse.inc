{
    This file is part of Synopse framework.
}

{$ifdef UNICODE}
  {$undef ENHANCEDRTL} // Delphi 2009 and up don't have our Enhanced Runtime library
  {$define HASVARUSTRING}
  {$define HASCODEPAGE}
  { due to a bug in Delphi 2009+, we need to fake inheritance of record,
    since TDynArrayHashed = object(TDynArray) fails to initialize
    http://blog.synopse.info/post/2011/01/29/record-and-object-issue-in-Delphi-2010 }
{$endif}


    {$define CPUINTEL} // Delphi only for Intel by now

  {$ifdef CPUX64}
    {$define CPU64} // Delphi compiler for 64 bit CPU
    {$define CPU64DELPHI}
    {$undef CPU32}

  {$else CPUX64}
    {$define CPU32} // Delphi compiler for 32 bit CPU
    {$undef CPU64}
    {$define CPUX86} // for compatibility with older versions of Delphi
  {$endif CPUX64}

    {$define HASINTERFACERTTI} // interface RTTI
      {$ifdef VER140}
        {$define ISDELPHI6ANDUP} // Delphi 6 or newer
        {$define DELPHI6OROLDER}
        {$define NOVARCOPYPROC}
        {$undef ENHANCEDRTL} // Delphi 6 doesn't have our Enhanced Runtime library
      {$else}
        {$define ISDELPHI7ANDUP} // Delphi 7 or newer
        {$define WITHUXTHEME}   // VCL handle UI theming
        {$warn UNSAFE_CODE OFF} // Delphi for .Net does not exist any more!
        {$warn UNSAFE_TYPE OFF}
        {$warn UNSAFE_CAST OFF}
        {$warn DUPLICATE_CTOR_DTOR OFF} // avoid W1029 unneeded hints
      {$endif}

    {$if CompilerVersion >= 18}
      {$define ISDELPHI2006ANDUP} // Delphi 2006 or newer
      {$define HASNEWFILEAGE}
      {$define HASINLINE}
      {$define HASREGION}
      {$define HASFASTMM4}
    {$ifend}
    {$ifdef VER180}
      {$define ISDELPHI20062007} // to circumvent some specific bugs
    {$endif}
    {$ifdef VER185}
      {$define ISDELPHI20062007}
    {$endif}
    {$if CompilerVersion > 18}
      {$define ISDELPHI2007ANDUP} // Delphi 2007 or newer
    {$ifend}
    {$if CompilerVersion = 20}
      {$define ISDELPHI2009} // Delphi 2009 has specific compilation issues :(
      // for Delphi 2009 and up, use UNICODE conditional :)
    {$ifend}
    {$if CompilerVersion >= 21.0}
      // Delphi 2010/XE: Reduce EXE size by disabling much RTTI
      {$define ISDELPHI2010}

      {$define HASTTHREADSTART}
      {$define HASINTERFACEASTOBJECT}
      {$ifdef NEWRTTINOTUSED}
        {$WEAKLINKRTTI ON}
        {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
      {$endif NEWRTTINOTUSED}
    {$ifend}
    {$if CompilerVersion >= 22.0}
      {$define ISDELPHIXE}
    {$ifend}
    {$if CompilerVersion >= 23.0}
      // Delphi XE2 has some cross-platform features
      // e.g. {$ifdef ISDELPHIXE2}VCL.Graphics{$else}Graphics{$endif}
      {$define ISDELPHIXE2}
      {$define HASVARUSTRARG}
    {$ifend}
    {$if CompilerVersion >= 24.0}
      {$define ISDELPHIXE3}
    {$ifend}
    {$if CompilerVersion >= 25.0}
      {$define ISDELPHIXE4}
      {$define HASAESNI}
    {$ifend}
    {$if CompilerVersion >= 26.0}
      {$define ISDELPHIXE5}
      {$define PUBLISHRECORD}
      // if defined, will handle RTTI available only since Delphi XE5 for
      // record published properties 
    {$ifend}
    {$if CompilerVersion >= 27.0}
      {$define ISDELPHIXE6}
    {$ifend}
    {$if CompilerVersion >= 28.0}
      {$define ISDELPHIXE7}
    {$ifend}
    {$if CompilerVersion >= 29.0}
      {$define ISDELPHIXE8}
    {$ifend}
    {$if CompilerVersion >= 30.0}
      {$define ISDELPHI10}
    {$ifend}
    {$if CompilerVersion >= 31.0}
      {$define ISDELPHI101}
    {$ifend}
    {$if CompilerVersion >= 32.0}
      {$define ISDELPHI102}
    {$ifend}

{$R-} // disable Range checking in our code
{$S-} // disable Stack checking in our code
{$X+} // expect extended syntax
{$W-} // disable stack frame generation
{$Q-} // disable overflow checking in our code
{$B-} // expect short circuit boolean
{$V-} // disable Var-String Checking
{$T-} // Typed @ operator
{$Z1} // enumerators stored as byte by default

{$P+} // Open string params


{$ifdef VER150}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$ifdef CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// see http://synopse.info/fossil/tktview?name=6593f0fbd1
{$ifndef WITHUXTHEME}
  {$define EXTENDEDTOSTRING_USESTR} // no TFormatSettings before Delphi 6
{$endif}


{$ifdef CPU64}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText() much slower in x64 mode
{$endif}



