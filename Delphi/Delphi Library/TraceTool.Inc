// unit TraceTool.inc ;
// used by viewer and client

// version 12.8

{$ifdef CLR}  // The common language runtime symbol is only defined for the .NET platform.
  {$define DELPHI.NET}
  {$ifdef VER160}
    {$define DELPHI.NET_1}
  {$endif VER160}

  // Compiler defines common to all .NET versions.
  {$ifdef DELPHI.NET_1}
    {$define DELHI.NET_1_UP}
  {$endif DELPHI.NET_1}
{$endif CLR}

{$ifdef CPUX64}
  {$define CPU64} // Delphi compiler for 64 bit CPU
  {$define CPU64DELPHI}
  {$undef CPU32}
{$else CPUX64}
  {$define CPU32} // Delphi compiler for 32 bit CPU
  {$undef CPU64}
  {$define CPUX86} // for compatibility with older versions of Delphi
{$endif CPUX64}

{$IFDEF CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
   {$ifdef VER140}
   {$else}
      {$warn UNSAFE_CODE OFF} // Delphi for .Net does not exist any more!
      {$warn UNSAFE_TYPE OFF}
      {$warn UNSAFE_CAST OFF}
      {$warn DUPLICATE_CTOR_DTOR OFF} // avoid W1029 unneeded hints
   {$endif}

   {$ifdef VER150}
     {$WARN SYMBOL_DEPRECATED OFF}
     {$WARN UNSAFE_TYPE OFF}
     {$WARN UNSAFE_CODE OFF}
     {$WARN UNSAFE_CAST OFF}
   {$ENDIF}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}


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

{$ifdef Win32}

// Compiler defines not specific to a particlular platform.
// http://docwiki.embarcadero.com/RADStudio/Berlin/en/Compiler_Versions
// http://docwiki.embarcadero.com/RADStudio/Berlin/en/Conditional_compilation_(Delphi)

  {$ifdef VER310}                 // rad Berlin
    {$define COMPILER_24}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define COMPILER_19_UP}
    {$define COMPILER_20_UP}
    {$define COMPILER_21_UP}
    {$define COMPILER_22_UP}
    {$define COMPILER_23_UP}
    {$define COMPILER_24_UP}
    {$define DELPHI}
    {$define DELPHI_24}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
    {$define DELPHI_19_UP}
    {$define DELPHI_20_UP}
    {$define DELPHI_21_UP}
    {$define DELPHI_22_UP}
    {$define DELPHI_23_UP}
    {$define DELPHI_24_UP}
  {$endif VER310}

  {$ifdef VER300}                 // rad Seattle
    {$define COMPILER_23}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define COMPILER_19_UP}
    {$define COMPILER_20_UP}
    {$define COMPILER_21_UP}
    {$define COMPILER_22_UP}
    {$define COMPILER_23_UP}
    {$define DELPHI}
    {$define DELPHI_23}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
    {$define DELPHI_19_UP}
    {$define DELPHI_20_UP}
    {$define DELPHI_21_UP}
    {$define DELPHI_22_UP}
    {$define DELPHI_23_UP}
  {$endif VER300}

  {$ifdef VER290}                 // rad XE8
    {$define COMPILER_22}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define COMPILER_19_UP}
    {$define COMPILER_20_UP}
    {$define COMPILER_21_UP}
    {$define COMPILER_22_UP}
    {$define DELPHI}
    {$define DELPHI_22}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
    {$define DELPHI_19_UP}
    {$define DELPHI_20_UP}
    {$define DELPHI_21_UP}
    {$define DELPHI_22_UP}
  {$endif VER290}

  {$ifdef VER280}                 // rad XE7
    {$define COMPILER_21}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define COMPILER_19_UP}
    {$define COMPILER_20_UP}
    {$define COMPILER_21_UP}
    {$define DELPHI}
    {$define DELPHI_21}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
    {$define DELPHI_19_UP}
    {$define DELPHI_20_UP}
    {$define DELPHI_21_UP}
  {$endif VER280}

  {$ifdef VER270}                 // rad XE6
    {$define COMPILER_20}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define COMPILER_19_UP}
    {$define COMPILER_20_UP}
    {$define DELPHI}
    {$define DELPHI_20}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
    {$define DELPHI_19_UP}
    {$define DELPHI_20_UP}
  {$endif VER270}

  {$ifdef VER260}                 // rad XE5
    {$define COMPILER_19}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define COMPILER_19_UP}
    {$define DELPHI}
    {$define DELPHI_19}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
    {$define DELPHI_19_UP}
  {$endif VER260}

  {$ifdef VER250}                 // rad XE4
    {$define COMPILER_18}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define COMPILER_18_UP}
    {$define DELPHI}
    {$define DELPHI_18}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
    {$define DELPHI_18_UP}
  {$endif VER250}

  {$ifdef VER240}                 // rad XE3
    {$define COMPILER_17}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define COMPILER_17_UP}
    {$define DELPHI}
    {$define DELPHI_17}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
    {$define DELPHI_17_UP}
  {$endif VER240}

  {$ifdef VER230}                 // rad XE2
    {$define COMPILER_16}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define COMPILER_16_UP}
    {$define DELPHI}
    {$define DELPHI_16}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
    {$define DELPHI_16_UP}
  {$endif VER230}

  {$ifdef VER220}                 // delpho XE1
    {$define COMPILER_15}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define COMPILER_15_UP}
    {$define DELPHI}
    {$define DELPHI_15}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
    {$define DELPHI_15_UP}
  {$endif VER220}

  {$ifdef VER210}                 // delphi 2010
    {$define COMPILER_14}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define COMPILER_13_UP}
    {$define COMPILER_14_UP}
    {$define DELPHI}
    {$define DELPHI_14}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
    {$define DELPHI_13_UP}
    {$define DELPHI_14_UP}
  {$endif VER210}

  {$ifdef VER200}  					 // delphi 2009
    {$define COMPILER_12}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define COMPILER_12_UP}
    {$define DELPHI}
    {$define DELPHI_12}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
    {$define DELPHI_12_UP}
  {$endif VER200}

  {$ifdef VER190}                 // delphi 2007
    {$define COMPILER_11}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define COMPILER_11_UP}
    {$define DELPHI}
    {$define DELPHI_11}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
    {$define DELPHI_11_UP}
  {$endif VER190}

  {$ifdef VER180}                 // delphi 2006
    {$define COMPILER_10}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define COMPILER_10_UP}
    {$define DELPHI}
    {$define DELPHI_10}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
    {$define DELPHI_10_UP}
  {$endif VER180}


  {$ifdef VER170}
    {$define COMPILER_9}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define COMPILER_9_UP}
    {$define DELPHI}
    {$define DELPHI_9}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
    {$define DELPHI_9_UP}
  {$endif VER170}
  
  {$ifdef VER160}
    {$define COMPILER_8}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define COMPILER_8_UP}
    {$define DELPHI}
    {$define DELPHI_8}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
    {$define DELPHI_8_UP}
  {$endif VER160}

  {$ifdef VER150}
    {$define COMPILER_7}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
    {$define DELPHI}
    {$define DELPHI_7}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
  {$endif}

  {$ifdef VER140}
    {$define COMPILER_6}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$ifdef BCB}
      {$define CPPB}
      {$define CPPB_6}
      {$define CPPB_3_UP}
      {$define CPPB_4_UP}
      {$define CPPB_5_UP}
      {$define CPPB_6_UP}
    {$else}
      {$define DELPHI}
      {$define DELPHI_6}
      {$define DELPHI_2_UP}
      {$define DELPHI_3_UP}
      {$define DELPHI_4_UP}
      {$define DELPHI_5_UP}
      {$define DELPHI_6_UP}
    {$endif}
  {$endif}

  {$ifdef VER130}
    {$define COMPILER_5}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$ifdef BCB}
      {$define CPPB}
      {$define CPPB_5}
      {$define CPPB_3_UP}
      {$define CPPB_4_UP}
      {$define CPPB_5_UP}
    {$else}
      {$define DELPHI}
      {$define DELPHI_5}
      {$define DELPHI_2_UP}
      {$define DELPHI_3_UP}
      {$define DELPHI_4_UP}
      {$define DELPHI_5_UP}
    {$endif}
  {$endif}

  {$ifdef VER125}
    {$define COMPILER_4}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define CPPB}
    {$define CPPB_4}
    {$define CPPB_3_UP}
    {$define CPPB_4_UP}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
  {$endif}

  {$ifdef VER120}
    {$define COMPILER_4}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define DELPHI}
    {$define DELPHI_4}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
  {$endif}

  {$ifdef VER110}
    {$define COMPILER_3}
    {$define CPPB}
    {$define CPPB_3}
    {$define CPPB_3_UP}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
  {$endif}

  {$ifdef VER100}
    {$define COMPILER_3}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define DELPHI}
    {$define DELPHI_3}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
  {$endif}

  {$ifdef VER93}
    {$define COMPILER_2} // C++ Builder v1 compiler is really v2
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define CPPB}
    {$define CPPB_1}
  {$endif}

  {$ifdef VER90}
    {$define COMPILER_2}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define DELPHI}
    {$define DELPHI_2}
    {$define DELPHI_2_UP}
  {$endif}

  {$ifdef VER80}
    {$define COMPILER_1}
    {$define COMPILER_1_UP}
    {$define DELPHI}
    {$define DELPHI_1}
  {$endif}
{$endif Win32}



