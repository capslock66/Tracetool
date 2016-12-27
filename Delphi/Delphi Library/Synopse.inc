{
    This file is part of Synopse framework.
    Version 1.7
}

{.$define PUREPASCAL}
// define this if your compiler doesn't support Delphi's x86 asm
// - is set automaticaly in case of a 64 bits compiler

{$ifdef LVCL}
   // NormToUpper[] exists only in our enhanced RTL
  {$define OWNNORMTOUPPER}
  // LVCL does not support variants
  {$define NOVARIANTS}
{$endif}

{$ifdef UNICODE}
  {$undef ENHANCEDRTL} // Delphi 2009 and up don't have our Enhanced Runtime library
  {$define HASVARUSTRING}
  {$define HASCODEPAGE}
{$endif}

  {$ifndef PUREPASCAL}
    {$define CPUINTEL} // Delphi only for Intel by now
  {$endif}
  {$ifdef CPUX64}
    {$define CPU64} // Delphi compiler for 64 bit CPU
    {$define CPU64DELPHI}
    {$undef CPU32}
    {$define PUREPASCAL}   // no x86 32 bit asm to be used
  {$else CPUX64}
    {$define CPU32} // Delphi compiler for 32 bit CPU
    {$undef CPU64}
    {$define CPUX86} // for compatibility with older versions of Delphi
  {$endif CPUX64}

  // defines if exceptions shall not be intercepted
  {.$define NOEXCEPTIONINTERCEPT}

  {$IFDEF CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
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

    {$if CompilerVersion >= 17}
    {$ifend}
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
  {$ELSE}
    // Delphi 5 or older
    {$define DELPHI6OROLDER}
    {$define DELPHI5OROLDER}
    {$define MSWINDOWS}
    {$define NOVARIANTS}
    {$define NOVARCOPYPROC}
    {$undef ENHANCEDRTL} // Delphi 5 doesn't have our Enhanced Runtime library
    {$undef DOPATCHTRTL}
  {$ENDIF}


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
{$ifdef LVCL}
  {$define EXTENDEDTOSTRING_USESTR} // no FloatToText implemented in LVCL
{$endif}
{$ifdef CPU64}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText() much slower in x64 mode
{$endif}

{$ifndef CPUINTEL}
  {$define NOSQLITE3STATIC}
{$endif}
{$ifdef ANDROID}
  {$define NOSQLITE3STATIC}
{$endif}
{$ifdef BSD}
  {$ifdef Darwin}
    {$define NOSQLITE3STATIC}
  {$else}
    // not yet sure if needed, but it works !
    {$define NOSQLITE3STATIC}
  {$endif}
{$endif}

{$ifdef NOSQLITE3STATIC}
  // our proprietary crypto expects a patched SQlite3.c statically linked
  {$define NOSQLITE3ENCRYPT}
{$endif}

{.$define SQLVIRTUALLOGS}
// enable low-level logging of SQlite3 virtual table query planner costs
// -> to be used only for internal debugging

{$ifdef MSWINDOWS}
  /// define this to publish TWinINet / TWinHttp / TWinHttpAPI classes
  // and TSQLHttpClientWinINet / TSQLHttpClientWinHTTP classes 
  {$define USEWININET}
  // our current IOCP pattern is Windows-specific:
  //  with Thread Pool: 3394 requests / second (each request with 4 KB of data)
  //  without the Pool: 140/s in the IDE (i.e. one core), 2637/s on a dual core
  // but less needed under Linux, since thread creation sounds much cheaper
  // see http://www.akkadia.org/drepper/nptl-design.pdf
  {$define USETHREADPOOL}
  {.$define ONLYUSEHTTPSOCKET} // for testing (no benefit vs http.sys)
  {.$define USELIBCURL}     // for testing under Windows (no benefit vs WinHTTP)
{$else}
  // http.sys server is Windows-specific
  {$define ONLYUSEHTTPSOCKET}
  {$ifndef ANDROID}
  // cross-platform libcurl has a great API -> TCurlHttp and TSQLHttpClientCurl
  {$define USELIBCURL}
  {$endif}
{$endif}

// define this to avoid sending "X-Powered-By: Synopse mORMot" HTTP header
{.$define NOXPOWEREDNAME}
