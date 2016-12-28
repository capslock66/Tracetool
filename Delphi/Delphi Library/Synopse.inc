{
    This file is part of Synopse framework.
    Version 1.7
}

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






