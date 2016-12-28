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

  {$ELSE}
    // Delphi 5 or older
    {$define DELPHI6OROLDER}
    {$define DELPHI5OROLDER}
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


  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}


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
