{*******************************************************}
{                                                       }
{       More Tools Foundation Classes Library           }
{                                                       }
{        Copyright (c) 1999-2002 Feedis Team            }
{                                                       }
{*******************************************************}

{$IFDEF VER150}
  {$DEFINE COMPILER7}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$IFDEF BCB}
    {$DEFINE BCB6}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$IFDEF BCB}
    {$DEFINE BCB5}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
  {$DEFINE BCB}
{$ENDIF}


{$IFDEF DELPHI7}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
{$ENDIF}

{$IFDEF DELPHI6}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE DELPHI5_UP}
{$ENDIF}

