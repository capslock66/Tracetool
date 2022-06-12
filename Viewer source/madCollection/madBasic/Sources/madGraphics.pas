// ***************************************************************
//  madGraphics.pas           version: 1.1.0  ·  date: 2012-04-03
//  -------------------------------------------------------------
//  gray scaling, smooth stretching, alpha blending, ...
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2012 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2012-04-03 1.1.0 (1) added x64 support
//                  (2) fixed bug in GrayScale function
// 2001-03-04 1.0   initial release

unit madGraphics;

{$I mad.inc}
{$R-}{$Q-}

{ $define newantiringing}
{ $define oldantiringing}

interface

uses Windows, Graphics, madTypes, madStrings;

// ***************************************************************

type TGrayPercent = (gp100, gp75, gp50, gp0);
procedure GrayScale (bmp: TBitmap; percent: TGrayPercent = gp100);

// ***************************************************************

type
  TStretchQuality = (sqLow, sqHigh, sqVeryHigh);

procedure StretchBitmap (srcBmp, dstBmp : TBitmap;
                         srcMsk, dstMsk : TBitmap;
                         quality        : TStretchQuality = sqHigh); overload;
procedure StretchBitmap (bmp, msk       : TBitmap;
                         newWidth       : integer;
                         newHeight      : integer;
                         quality        : TStretchQuality = sqHigh); overload;

// ***************************************************************

procedure AlphaBlend (src1, src2, dst : TBitmap;
                      alpha           : cardinal = 128); overload;
procedure AlphaBlend (src, dst        : TBitmap;
                      alpha           : cardinal = 128;
                      msk             : TBitmap  = nil;
                      x               : integer  = 0;
                      y               : integer  = 0  ); overload;

// ***************************************************************

procedure Draw (imageList, index : cardinal;
                dst              : TBitmap;
                x                : integer         = 0;
                y                : integer         = 0;
                width            : integer         = 0;
                height           : integer         = 0;
                grayPercent      : TGrayPercent    = gp0;
                alpha            : cardinal        = 256;
                stretchQuality   : TStretchQuality = sqHigh); overload;

procedure Draw (bmp, msk         : TBitmap;
                dst              : TBitmap;
                x                : integer         = 0;
                y                : integer         = 0;
                width            : integer         = 0;
                height           : integer         = 0;
                grayPercent      : TGrayPercent    = gp0;
                alpha            : cardinal        = 256;
                stretchQuality   : TStretchQuality = sqHigh); overload;

// ***************************************************************

implementation

uses SysUtils, Classes, Math, CommCtrl;

// ***************************************************************

function GetPixelFormat(bmp: TBitmap) : integer;
var ds : TDibSection;
begin
  result := 0;
  if GetObject(bmp.handle, sizeOf(TDibSection), @ds) <> 0 then begin
    result := ds.dsBmih.biBitCount;
    if result = 16 then
      if ds.dsBitFields[2] = $1F then begin
        if (ds.dsBitFields[1] = $3E0) and (ds.dsBitFields[0] = $7C00) then
          result := 15
        else if (ds.dsBitFields[1] <> $7E0) or (ds.dsBitFields[0] <> $F800) then
          result := 0;
      end else
        result := 0;
  end;
end;

function ForcePixelFormat(const bmps: array of TBitmap; pixelFormat: TPixelFormat) : boolean;
var i1 : integer;
begin
  result := true;
  for i1 := 0 to high(bmps) do
    if (bmps[i1].PixelFormat <> pixelFormat) and
       ((pixelFormat <> pf15bit) or (GetPixelFormat(bmps[i1]) <> 15)) then begin
      bmps[i1].PixelFormat := pixelFormat;
      if (bmps[i1].PixelFormat <> pixelFormat) and
         ((pixelFormat <> pf15bit) or (GetPixelFormat(bmps[i1]) <> 15)) then begin
        result := false;
        exit;
      end;
    end;
end;

// ***************************************************************

type
  TXtab32 = array [0..2 * 255 + 4 * 255 + 1 * 255] of cardinal;
  TXtab16 = array [0..2 * 031 + 2 * 063 + 1 * 031] of word;
  TXtab15 = array [0..2 * 031 + 4 * 031 + 1 * 031] of word;

var
  xtab32      : array [TGrayPercent] of TXtab32;
  xtab16      : array [TGrayPercent] of TXtab16;
  xtab15      : array [TGrayPercent] of TXtab15;
  xtab32Ready : array [TGrayPercent] of boolean = (false, false, false, false);
  xtab16Ready : array [TGrayPercent] of boolean = (false, false, false, false);
  xtab15Ready : array [TGrayPercent] of boolean = (false, false, false, false);

//32bit: 160ms                                     -> 100ms 130 125
//24bit: 140ms                                     -> 120ms 165 155
//16bit: 245ms  (355ms for the non-asm variant)    -> 100ms 130 140
//15bit: not supported by my graphics card         -> 100ms 130 170

procedure GrayScale(bmp: TBitmap; percent: TGrayPercent = gp100);

  {$ifdef win64}

    procedure GrayScaleLine15_100(line: TPWord; width: integer; const xtab: TXtab15);
    var w1 : word;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        w1 := line^;
        line^ := xtab[(w1 shr 9) and $fe + (w1 shr 3) and $7c + w1 and $1f];
        inc(line);
      end;
    end;

    procedure GrayScaleLine15_75(line: TPWord; width: integer; const xtab: TXtab15);
    var w1 : word;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        w1 := line^;
        line^ := (w1 shr 2) and $1ce7 + xtab[(w1 shr 9) and $fe + (w1 shr 3) and $7c + w1 and $1f];
        inc(line);
      end;
    end;

    procedure GrayScaleLine15_50(line: TPWord; width: integer; const xtab: TXtab15);
    var w1 : word;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        w1 := line^;
        line^ := (w1 shr 1) and $3def + xtab[(w1 shr 9) and $fe + (w1 shr 3) and $7c + w1 and $1f];
        inc(line);
      end;
    end;

    procedure GrayScaleLine16_100(line: TPWord; width: integer; const xtab: TXtab16);
    var w1 : word;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        w1 := line^;
        line^ := xtab[(w1 shr 10) and $fe + (w1 shr 4) and $7e + w1 and $1f];
        inc(line);
      end;
    end;

    procedure GrayScaleLine16_75(line: TPWord; width: integer; const xtab: TXtab16);
    var w1 : word;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        w1 := line^;
        line^ := (w1 shr 2) and $39e7 + xtab[(w1 shr 10) and $fe + (w1 shr 4) and $7e + w1 and $1f];
        inc(line);
      end;
    end;

    procedure GrayScaleLine16_50(line: TPWord; width: integer; const xtab: TXtab16);
    var w1 : word;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        w1 := line^;
        line^ := (w1 shr 1) and $7bef + xtab[(w1 shr 10) and $fe + (w1 shr 4) and $7e + w1 and $1f];
        inc(line);
      end;
    end;

    procedure GrayScaleLine24_100(line: TPAByte; width: integer; const xtab: TXTab32);
    var b1 : byte;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        b1 := xtab[line[0] shl 1 + line[1] shl 2 + line[2]];
        line[0] := b1;
        line[1] := b1;
        line[2] := b1;
        inc(NativeUInt(line), 3);
      end;
    end;

    procedure GrayScaleLine24_75(line: TPAByte; width: integer; const xtab: TXTab32);
    var c1 : cardinal;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        c1 := (TPCardinal(line)^ shr 2) and $003f3f3f + xtab[line[0] shl 1 + line[1] shl 2 + line[2]];
        line[0] := byte(c1);
        c1 := c1 shr 8;
        line[1] := byte(c1);
        c1 := c1 shr 8;
        line[2] := byte(c1);
        inc(NativeUInt(line), 3);
      end;
    end;

    procedure GrayScaleLine24_50(line: TPAByte; width: integer; const xtab: TXTab32);
    var c1 : cardinal;
        i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        c1 := (TPCardinal(line)^ shr 1) and $007f7f7f + xtab[line[0] shl 1 + line[1] shl 2 + line[2]];
        line[0] := byte(c1);
        c1 := c1 shr 8;
        line[1] := byte(c1);
        c1 := c1 shr 8;
        line[2] := byte(c1);
        inc(NativeUInt(line), 3);
      end;
    end;

    procedure GrayScaleLine32_100(line: TPAByte; width: integer; const xtab: TXTab32);
    var i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        TPCardinal(line)^ := xtab[line[0] shl 1 + line[1] shl 2 + line[2]];
        inc(NativeUInt(line), 4);
      end;
    end;

    procedure GrayScaleLine32_75(line: TPAByte; width: integer; const xtab: TXTab32);
    var i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        TPCardinal(line)^ := (TPCardinal(line)^ shr 2) and $003f3f3f + xtab[line[0] shl 1 + line[1] shl 2 + line[2]];
        inc(NativeUInt(line), 4);
      end;
    end;

    procedure GrayScaleLine32_50(line: TPAByte; width: integer; const xtab: TXTab32);
    var i1 : integer;
    begin
      for i1 := 0 to width - 1 do begin
        TPCardinal(line)^ := (TPCardinal(line)^ shr 1) and $007f7f7f + xtab[line[0] shl 1 + line[1] shl 2 + line[2]];
        inc(NativeUInt(line), 4);
      end;
    end;

  {$else}

    procedure GrayScaleLine15_100(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  ebx

      mov   edi,   eax
      mov   ebx,   edx
      shl   ebx,   1
      add   ebx,   eax
     @0:
      movzx eax,   word[edi]
      mov   edx,   eax
      and   eax,   $000003E0
      shr   eax,   3
      and   edx,   $0000001F
      add   eax,   edx
      movzx edx,   word[edi]
      shr   edx,   9
      and   edx,   $FE
      add   eax,   edx
      mov   ax,    [eax*2+ecx]
      mov   [edi], ax
      add   edi,   2
      cmp   ebx,   edi
      jnz   @0

      pop   ebx
      pop   edi
    end;

    procedure GrayScaleLine15_75(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,   eax
      mov   esi,   edx
      shl   esi,   1
      add   esi,   eax
     @0:
      movzx eax,   word[edi]
      mov   edx,   eax
      mov   ebx,   eax
      shr   ebx,   2
      and   ebx,   $1CE7
      and   edx,   $0000001F
      and   eax,   $000003E0
      shr   eax,   3
      add   edx,   eax
      movzx eax,   word[edi]
      shr   eax,   9
      and   eax,   $FE
      add   edx,   eax
      add   bx,    [edx*2+ecx]
      mov   [edi], bx
      add   edi,   2
      cmp   esi,   edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine15_50(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,   eax
      mov   esi,   edx
      shl   esi,   1
      add   esi,   eax
     @0:
      movzx eax,   word[edi]
      mov   edx,   eax
      mov   ebx,   eax
      shr   ebx,   1
      and   ebx,   $3DEF
      and   edx,   $0000001F
      and   eax,   $000003E0
      shr   eax,   3
      add   edx,   eax
      movzx eax,   word[edi]
      shr   eax,   9
      and   eax,   $FE
      add   edx,   eax
      add   bx,    [edx*2+ecx]
      mov   [edi], bx
      add   edi,   2
      cmp   esi,   edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine16_100(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  ebx

      mov   edi,   eax
      mov   ebx,   edx
      shl   ebx,   1
      add   ebx,   eax
     @0:
      movzx eax,   word[edi]
      mov   edx,   eax
      and   eax,   $000007E0
      shr   eax,   4
      and   edx,   $0000001F
      add   eax,   edx
      movzx edx,   word[edi]
      shr   edx,   10
      and   edx,   $FE
      add   eax,   edx
      mov   ax,    [eax*2+ecx]
      mov   [edi], ax
      add   edi,   2
      cmp   ebx,   edi
      jnz   @0

      pop   ebx
      pop   edi
    end;

    procedure GrayScaleLine16_75(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,   eax
      mov   esi,   edx
      shl   esi,   1
      add   esi,   eax
     @0:
      movzx eax,   word[edi]
      mov   edx,   eax
      mov   ebx,   eax
      shr   ebx,   2
      and   ebx,   $39E7
      and   edx,   $0000001F
      and   eax,   $000007E0
      shr   eax,   4
      add   edx,   eax
      movzx eax,   word[edi]
      shr   eax,   10
      and   eax,   $FE
      add   edx,   eax
      add   bx,    [edx*2+ecx]
      mov   [edi], bx
      add   edi,   2
      cmp   esi,   edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine16_50(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,   eax
      mov   esi,   edx
      shl   esi,   1
      add   esi,   eax
     @0:
      movzx eax,   word[edi]
      mov   edx,   eax
      mov   ebx,   eax
      shr   ebx,   1
      and   ebx,   $7BEF
      and   edx,   $0000001F
      and   eax,   $000007E0
      shr   eax,   4
      add   edx,   eax
      movzx eax,   word[edi]
      shr   eax,   10
      and   eax,   $FE
      add   edx,   eax
      add   bx,    [edx*2+ecx]
      mov   [edi], bx
      add   edi,   2
      cmp   esi,   edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine24_100(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  ebx

      mov   edi,     eax
      mov   ebx,     edx
      add   ebx,     edx
      add   ebx,     edx
      add   ebx,     eax
     @0:
      movzx edx,     byte[edi+2]
      movzx eax,     byte[edi+1]
      shl   eax,     2
      add   edx,     eax
      movzx eax,     byte[edi]
      add   edx,     eax
      add   edx,     eax
      mov   edx,     [edx*4+ecx]
      mov   [edi],   dl
      mov   [edi+1], dl
      mov   [edi+2], dl
      add   edi,     3
      cmp   ebx,     edi
      jnz   @0

      pop   ebx
      pop   edi
    end;

    procedure GrayScaleLine24_75(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,     eax
      mov   esi,     edx
      add   esi,     edx
      add   esi,     edx
      add   esi,     eax
     @0:
      movzx eax,     byte[edi+2]
      mov   ebx,     [edi]
      shr   ebx,     2
      and   ebx,     $003F3F3F
      movzx edx,     byte[edi+1]
      shl   edx,     2
      add   eax,     edx
      movzx edx,     byte[edi]
      add   eax,     edx
      add   eax,     edx
      add   ebx,     [eax*4+ecx]
      mov   [edi],   bl
      shr   ebx,     8
      mov   [edi+1], bl
      mov   [edi+2], bh
      add   edi,     3
      cmp   esi,     edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine24_50(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,     eax
      mov   esi,     edx
      add   esi,     edx
      add   esi,     edx
      add   esi,     eax
     @0:
      movzx eax,     byte[edi+2]
      mov   ebx,     [edi]
      shr   ebx,     1
      and   ebx,     $007F7F7F
      movzx edx,     byte[edi+1]
      shl   edx,     2
      add   eax,     edx
      movzx edx,     byte[edi]
      add   eax,     edx
      add   eax,     edx
      add   ebx,     [eax*4+ecx]
      mov   [edi],   bl
      shr   ebx,     8
      mov   [edi+1], bl
      mov   [edi+2], bh
      add   edi,     3
      cmp   esi,     edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine32_100(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  ebx

      mov   edi,   eax
      mov   ebx,   edx
      shl   ebx,   2
      add   ebx,   eax
     @0:
      movzx eax,   byte[edi+2]
      movzx edx,   byte[edi+1]
      shl   edx,   2
      add   eax,   edx
      movzx edx,   byte[edi]
      add   eax,   edx
      add   eax,   edx
      mov   eax,   [eax*4+ecx]
      mov   [edi], eax
      add   edi,   4
      cmp   ebx,   edi
      jnz   @0

      pop   ebx
      pop   edi
    end;

    procedure GrayScaleLine32_75(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,   eax
      mov   esi,   edx
      shl   esi,   2
      add   esi,   eax
     @0:
      movzx eax,   byte[edi+2]
      mov   ebx,   [edi]
      shr   ebx,   2
      and   ebx,   $003F3F3F
      movzx edx,   byte[edi+1]
      shl   edx,   2
      add   eax,   edx
      movzx edx,   byte[edi]
      add   eax,   edx
      add   eax,   edx
      add   ebx,   [eax*4+ecx]
      mov   [edi], ebx
      add   edi,   4
      cmp   esi,   edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

    procedure GrayScaleLine32_50(line: pointer; width: integer; var xtab);
    asm
      push  edi
      push  esi
      push  ebx

      mov   edi,   eax
      mov   esi,   edx
      shl   esi,   2
      add   esi,   eax
     @0:
      movzx eax,   byte[edi+2]
      mov   ebx,   [edi]
      shr   ebx,   1
      and   ebx,   $007F7F7F
      movzx edx,   byte[edi+1]
      shl   edx,   2
      add   eax,   edx
      movzx edx,   byte[edi]
      add   eax,   edx
      add   eax,   edx
      add   ebx,   [eax*4+ecx]
      mov   [edi], ebx
      add   edi,   4
      cmp   esi,   edi
      jnz   @0

      pop   ebx
      pop   esi
      pop   edi
    end;

  {$endif}

  procedure CheckXTab32(gp: TGrayPercent);
  var i1   : integer;
      gray : integer;
  begin
    if not xtab32Ready[gp] then begin
      for i1 := 0 to high(xtab32[gp]) do begin
        gray := round(i1 / 7);
        case gp of
          gp50 : gray := gray div 2;
          gp75 : gray := gray - gray div 4;
        end;
        xtab32[gp][i1] := gray shl 16 + gray shl 8 + gray;
      end;
      xtab32Ready[gp] := true;
    end;
  end;

  procedure CheckXTab16(gp: TGrayPercent);
  var i1   : integer;
      gray : integer;
  begin
    if not xtab16Ready[gp] then begin
      for i1 := 0 to high(xtab16[gp]) do begin
        gray := round(i1 * 31 / high(xtab16[gp]));
        case gp of
          gp50 : gray := gray div 2;
          gp75 : gray := gray - gray div 4;
        end;
        xtab16[gp][i1] := gray shl 11 + gray shl 6 + gray;
      end;
      xtab16Ready[gp] := true;
    end;
  end;

  procedure CheckXTab15(gp: TGrayPercent);
  var i1   : integer;
      gray : integer;
  begin
    if not xtab15Ready[gp] then begin
      for i1 := 0 to high(xtab15[gp]) do begin
        gray := round(i1 * 31 / high(xtab15[gp]));
        case gp of
          gp50 : gray := gray div 2;
          gp75 : gray := gray - gray div 4;
        end;
        xtab15[gp][i1] := gray shl 10 + gray shl 5 + gray;
      end;
      xtab15Ready[gp] := true;
    end;
  end;

var iy, iw, ih : integer;
    line       : pointer;
    lineDif    : integer;
    casei      : integer;
begin
  if percent <> gp0 then
    with bmp do begin
      iw := Width;
      ih := Height;
      if (iw > 0) and (ih > 0) then begin
        if GetPixelFormat(bmp) < 15 then PixelFormat := pf32bit;
        case GetPixelFormat(bmp) of
          15 : begin casei := $150; CheckXTab15(percent) end;
          16 : begin casei := $160; CheckXTab16(percent) end;
          24 : begin casei := $240; CheckXTab32(percent) end;
          32 : begin casei := $320; CheckXtab32(percent) end;
          else exit;
        end;
        inc(casei, ord(percent));
        line := ScanLine[0];
        if ih > 1 then
          lineDif := NativeInt(ScanLine[1]) - NativeInt(line)
        else
          lineDif := 0;
        for iy := 0 to Height - 1 do begin
          case casei of
            $150 : GrayScaleLine15_100(line, iw, xtab15[gp100]);
            $151 : GrayScaleLine15_75 (line, iw, xtab15[gp75 ]);
            $152 : GrayScaleLine15_50 (line, iw, xtab15[gp50 ]);
            $160 : GrayScaleLine16_100(line, iw, xtab16[gp100]);
            $161 : GrayScaleLine16_75 (line, iw, xtab16[gp75 ]);
            $162 : GrayScaleLine16_50 (line, iw, xtab16[gp50 ]);
            $240 : GrayScaleLine24_100(line, iw, xtab32[gp100]);
            $241 : GrayScaleLine24_75 (line, iw, xtab32[gp75 ]);
            $242 : GrayScaleLine24_50 (line, iw, xtab32[gp50 ]);
            $320 : GrayScaleLine32_100(line, iw, xtab32[gp100]);
            $321 : GrayScaleLine32_75 (line, iw, xtab32[gp75 ]);
            $322 : GrayScaleLine32_50 (line, iw, xtab32[gp50 ]);
          end;
          inc(NativeInt(line), lineDif);
        end;
      end;
    end;
end;

// ***************************************************************

type
  TContributorPixel = record
    pixel  : integer;  // source pixel
    weight : integer;  // weight of this source pixel
  end;

  // list of source pixels contributing to the destination pixel
  TContributorPixels = record
    itemCount : integer;
    items     : array of TContributorPixel;
  end;

  // list for the full width/height of a bitmap
  TContributorList  = array of TContributorPixels;

  // which resampling filter shall be used?
  TResamplingFilter = (
    // 1 tap
    rfNearest,       // Nearest Neighbor, aka. Point Sampling
                     // - additional ringing = 0
                     // - hide source ringing = 0
                     // - aliasing = 10
                     // - sharpness = 10

    // 2 tap
    rfBilinear,      // Bilinear
                     // - additional ringing = 0
                     // - hide source ringing = 3
                     // - aliasing = 5
                     // - sharpness = 5

    rfBicubic50, rfBicubic60, rfBicubic75,
                     // Bicubic, aka. Catmull-Rom (rfBicubic50)
                     // - additional ringing = 1, 2, 3.5
                     // - hide source ringing = 0
                     // - aliasing = 4.5, 3.5, 3
                     // - sharpness = 7.25, 7.50, 7.75

    rfMitchell,      // Mitchell-Netravali
                     // - additional ringing = 1
                     // - hide source ringing = 1
                     // - aliasing = 4
                     // - sharpness = 5

    rfSoftCubic100, rfSoftCubic80, rfSoftCubic70, rfSoftCubic60, rfSoftCubic50,
                     // Cubic B-Spline
                     // - additional ringing = 0, 0, 0.25, 0.5, 0.75
                     // - hide source ringing = 9, 7, 6, 3, 1
                     // - aliasing = 0, 0.25, 1, 2, 2.5 
                     // - sharpness = 0.5, 2, 3, 3.5, 4

    // 3-4 tap
    rfLanczos3, rfLanczos4,
                     // Lanczos
                     // - additional ringing = 6, 8
                     // - hide source ringing = 0
                     // - aliasing = 2, 1
                     // - sharpness = 8.5, 9
    rfSpline36, rfSpline64
                     // Spline
                     // - additional ringing = 4.5, 5
                     // - hide source ringing = 0
                     // - aliasing = 2.5, 2.3
                     // - sharpness = 8, 8.1

    // spline16  -> bicubic50
    // blackman3 -> bicubic60
    // blackman4 -> spline36
    // gaussian  -> softCubic
  );

  // full list structure including src and dst information
  TScaleList = record
    filter          : TResamplingFilter;
    src, dst        : integer;
    contributorList : TContributorList;
  end;

const
  CResamplingFilterNames : array [TResamplingFilter] of AnsiString =
    ('Nearest',
     'Bilinear',
     'Bicubic50', 'Bicubic60', 'Bicubic75',
     'Mitchell',
     'SoftCubic100', 'SoftCubic80', 'SoftCubic70', 'SoftCubic60', 'SoftCubic50',
     'Lanczos3', 'Lanczos4',
     'Spline36', 'Spline64');

var
  ScaleListCache   : array of TScaleList;
  ScaleListSection : TRTLCriticalSection;

procedure StretchBitmap_(srcBmp, dstBmp : TBitmap;
                         srcMsk, dstMsk : TBitmap;
                         quality        : TStretchQuality = sqHigh;
                         filter         : TResamplingFilter = rfLanczos4); overload;

  procedure GetContributorList(var result: TContributorList; src_, dst_: integer; filter: TResamplingFilter);

    function Cubic(weight, param1, param2: double) : double;
    var P0, P1, P2, P3 : double;
    begin
      result := 0;
      if weight <= 2 then begin
        if weight < 1 then begin
          P0 := (+ 1 - 1/3 * param1 - 0 * param2);
          P1 := 0;
          P2 := (- 3 + 2   * param1 + 1 * param2);
          P3 := (+ 2 - 3/2 * param1 - 1 * param2);
        end else begin
          P0 := (- 0 + 4/3 * param1 + 4 * param2);
          P1 := (+ 0 - 2   * param1 - 8 * param2);
          P2 := (- 0 + 1   * param1 + 5 * param2);
          P3 := (+ 0 - 1/6 * param1 - 1 * param2);
        end;
        result := P0 + P1 * weight + P2 * weight * weight + P3 * weight * weight * weight;
      end;
    end;

    function GetFilterCoeffs(weight: double; filter: TResamplingFilter) : double;
    begin
      result := 0;
      case filter of
        rfNearest        : if weight < 1 then
                             result := 1 - weight;
        rfBilinear       : if weight < 1 then
                             result := 1 - weight;
        rfBicubic50      : result := Cubic(weight, 0.00, 0.50);
        rfBicubic60      : result := Cubic(weight, 0.00, 0.60);
        rfBicubic75      : result := Cubic(weight, 0.00, 0.75);
        rfSoftCubic100   : result := Cubic(weight, 1.00, 0.00);
        rfSoftCubic80    : result := Cubic(weight, 0.80, 0.20);
        rfSoftCubic70    : result := Cubic(weight, 0.70, 0.30);
        rfSoftCubic60    : result := Cubic(weight, 0.60, 0.40);
        rfSoftCubic50    : result := Cubic(weight, 0.50, 0.50);
        rfMitchell       : result := Cubic(weight, 1/3, 1/3);
        rfLanczos3       : if weight = 0 then
                             result := 1
                           else
                             if weight < 3 then begin
                               weight := weight * Pi;
                               result := (sin(weight) / weight) * (sin(weight / 3) / (weight / 3));  // result := 3 * sin(weight) * sin(weight / 3) / (weight * weight);
                             end;
        rfLanczos4       : if weight = 0 then
                             result := 1
                           else
                             if weight < 4 then begin
                               weight := weight * Pi;
                               result := (sin(weight) / weight) * (sin(weight / 4) / (weight / 4));
                             end;
        rfSpline36       : if weight < 3 then
                             if weight < 1 then
                               result := ((13 / 11 * weight - 453 / 209) * weight - 3 / 209) * weight + 1
                             else
                               if weight < 2 then
                                 result := ((-6 / 11  * (weight - 1) + 270 / 209) * (weight - 1) - 156 / 209) * (weight - 1)
                               else
                                 result := ((1 / 11  * (weight - 2) - 45 / 209) * (weight - 2) + 26 / 209) * (weight - 2);
        rfSpline64       : if weight < 4 then
                             if weight < 1 then
                               result := ((49 / 41 * weight - 6387 / 2911) * weight - 3 / 2911) * weight + 1
                             else
                               if weight < 2 then
                                 result := ((-24 / 41 * (weight - 1) + 4032 / 2911) * (weight - 1) - 2328 / 2911) * (weight - 1)
                               else
                                 if weight < 3 then
                                   result := ((6 / 41 * (weight - 2) - 1008 / 2911) * (weight - 2) + 582 / 2911) * (weight - 2)
                                 else
                                   result := ((-1 / 41 * (weight - 3) +  168 / 2911) * (weight - 3) - 97 / 2911) * (weight - 3);
//        rfGaussian25     : if weight < 4 then
//                             result := Power(2, - 2.5 * weight * weight);
      end;
    end;

    type TDADouble = array of double;

    procedure OptimizeItems(var result: TContributorList; index: integer; const floatWeights: TDADouble);
    var totalI  : integer;
        totalF  : double;
        highest : integer;
        i1, i2  : integer;
    begin
      with result[index] do begin
        totalF := 0;
        for i2 := 0 to itemCount - 1 do
          totalF := totalF + floatWeights[i2];
        totalI := 0;
        highest := -1;
        i1 := 0;
        for i2 := 0 to itemCount - 1 do begin
          items[i1].weight := round(floatWeights[i2] / totalF * $10000);
          if abs(items[i1].weight) > 1 then begin
            items[i1].pixel := items[i2].pixel;
            inc(totalI, items[i1].weight);
            if (highest = -1) or (items[i1].weight > items[highest].weight) then
              highest := i1;
            inc(i1);
          end;
        end;
        itemCount := i1;
        if totalI <> $10000 then
          dec(items[highest].weight, totalI - $10000);
      end;
    end;

    procedure AddItem(var result: TContributorList; index: integer; pixel, maxPixel: integer; var floatWeights, offsets: TDADouble; offset: double; filter: TResamplingFilter);
    var i1, i2 : integer;
        weight : double;
    begin
      with result[index] do begin
        weight := GetFilterCoeffs(offset, filter);
        if weight <> 0 then begin
          if pixel < 0 then
            pixel := 0
          else
            if pixel >= maxPixel then
              pixel := maxPixel - 1;
          floatWeights[itemCount] := weight;
          offsets[itemCount] := offset;
          items[itemCount].pixel := pixel;
          for i1 := 0 to itemCount - 1 do
            if abs(offsets[i1]) > abs(offset) then begin
              for i2 := itemCount downto i1 + 1 do begin
                floatWeights[i2] := floatWeights[i2 - 1];
                offsets[i2] := offsets[i2 - 1];
                items[i2].pixel := items[i2 - 1].pixel;
              end;
              floatWeights[i1] := weight;
              offsets[i1] := offset;
              items[i1].pixel := pixel;
              break;
            end;
          inc(itemCount);
        end;
      end;
    end;

  var scale        : double;
      capacity     : integer;
      i1, i2       : integer;
      floatWeights : TDADouble;
      offsets      : TDADouble;
  begin
    if ScaleListCache = nil then
      InitializeCriticalSection(ScaleListSection);
    EnterCriticalSection(ScaleListSection);
    try
      for i1 := 0 to high(ScaleListCache) do
        if (ScaleListCache[i1].filter = filter) and (ScaleListCache[i1].src = src_) and (ScaleListCache[i1].dst = dst_) then begin
          result := ScaleListCache[i1].contributorList;
          exit;
        end;
      if dst_ < src_ then begin
        SetLength(result, dst_);
        capacity := 8 * src_ div dst_ + 1;
        SetLength(floatWeights, capacity);
        SetLength(offsets, capacity);
        scale := dst_ / src_;
        for i1 := 0 to dst_ - 1 do
          with result[i1] do begin
            SetLength(items, capacity);
            itemCount := 0;
            for i2 := ((i1 - 4) * src_ - dst_ + 1) div dst_ to ((i1 + 4) * src_ + dst_ - 1) div dst_ do
              AddItem(result, i1, i2, src_, floatWeights, offsets, abs(i1 + 0.5 - (i2 + 0.5) * scale), filter);
            if filter = rfNearest then
              itemCount := 1;
            OptimizeItems(result, i1, floatWeights);
          end;
      end else begin
        SetLength(result, dst_);
        SetLength(floatWeights, 9);
        SetLength(offsets, 9);
        scale := src_ / dst_;
        for i1 := 0 to dst_ - 1 do
          with result[i1] do begin
            SetLength(items, 17);
            itemCount := 0;
            for i2 := (i1 * src_ - 5 * dst_ + 1) div dst_ to (i1 * src_ + 5 * dst_ - 1) div dst_ do
              AddItem(result, i1, i2, src_, floatWeights, offsets, abs((i1 + 0.5) * scale - (i2 + 0.5)), filter);
            if filter = rfNearest then
              itemCount := 1;
            OptimizeItems(result, i1, floatWeights);
          end;
      end;
      i1 := Length(ScaleListCache);
      SetLength(ScaleListCache, i1 + 1);
      ScaleListCache[i1].filter := filter;
      ScaleListCache[i1].src := src_;
      ScaleListCache[i1].dst := dst_;
      ScaleListCache[i1].contributorList := result;
    finally LeaveCriticalSection(ScaleListSection) end;
  end;

  procedure Resampling15;  // 370 / 195
  var ix, iy, ic     : integer;
      weight         : integer;
      clx, cly       : TContributorList;
      fr, fg, fb, fm : integer;
      sbBits, dbBits : NativeInt;
      smBits, dmBits : NativeInt;
      sbDif,  dbDif  : integer;
      smDif,  dmDif  : integer;
      sbLine, tbBuf  : PWordArray;
      smLine, tmBuf  : PByteArray;
      dbPix          : ^word;
      dmPix          : ^byte;
      pcp            : ^TContributorPixel;
      dw1            : integer;
  begin
    GetContributorList(clx, srcBmp.Width,  dstBmp.Width,  rfMitchell);
    GetContributorList(cly, srcBmp.Height, dstBmp.Height, rfMitchell);
    tbBuf  := AllocMem(srcBmp.Height * 2 + srcBmp.Height * 1);
    sbBits := NativeInt(srcBmp.ScanLine[0]);
    dbBits := NativeInt(dstBmp.ScanLine[0]);
    if srcBmp.Height > 1 then
      sbDif := NativeInt(srcBmp.ScanLine[1]) - sbBits
    else
      sbDif := 0;
    if dstBmp.Height > 1 then
      dbDif := NativeInt(dstBmp.ScanLine[1]) - dbBits
    else
      dbDif := 0;
    if srcMsk <> nil then begin
      tmBuf  := pointer(NativeInt(tbBuf) + srcBmp.Height * 2);
      smBits := NativeInt(srcMsk.ScanLine[0]);
      dmBits := NativeInt(dstMsk.ScanLine[0]);
      if srcMsk.Height > 1 then
        smDif := NativeInt(srcMsk.ScanLine[1]) - smBits
      else
        smDif := 0;
      if dstMsk.Height > 1 then
        dmDif := NativeInt(dstMsk.ScanLine[1]) - dmBits
      else
        dmDif := 0;
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        dmPix  := pointer(tmBuf );
        sbLine := pointer(sbBits);
        smLine := pointer(smBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          fm  := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - smLine^[pcp^.pixel])) div 256;
            inc(fm, weight);
            dw1 := sbLine^[pcp^.pixel];
            inc(fr, (dw1 shr 10   ) * weight);
            inc(fg, (dw1 and $03E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $7C00
          else                        fr := (fr shr 6) and $7C00;
          if      fg <         0 then
          else if fg > $03E00000 then inc(fr, $03E0)
          else                        inc(fr, (fg shr 16) and $03E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(dbPix);
          if      fm <         0 then dmPix^ := 255
          else if fm > $0000FF00 then dmPix^ := 0
          else                        dmPix^ := 255 - fm shr 8;
          inc(dmPix);
          inc(NativeInt(sbLine), sbDif);
          inc(NativeInt(smLine), smDif);
        end;
        dbPix := pointer(dbBits + ix * 2);
        dmPix := pointer(dmBits + ix    );
        for iy := 0 to dstBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          fm  := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - tmBuf^[pcp^.pixel])) div 256;
            inc(fm, weight);
            dw1 := tbBuf^[pcp^.pixel];
            inc(fr, (dw1 shr 10   ) * weight);
            inc(fg, (dw1 and $03E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $7C00
          else                        fr := (fr shr 6) and $7C00;
          if      fg <         0 then
          else if fg > $03E00000 then inc(fr, $03E0)
          else                        inc(fr, (fg shr 16) and $03E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(NativeInt(dbPix), dbDif);
          if      fm <         0 then dmPix^ := 255
          else if fm > $0000FF00 then dmPix^ := 0
          else                        dmPix^ := 255 - fm shr 8;
          inc(dmPix, dmDif);
        end;
      end;
    end else
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        sbLine := pointer(sbBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := pcp^.weight;
            dw1 := sbLine^[pcp^.pixel];
            inc(fr, (dw1 shr 10   ) * weight);
            inc(fg, (dw1 and $03E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $7C00
          else                        fr := (fr shr 6) and $7C00;
          if      fg <         0 then
          else if fg > $03E00000 then inc(fr, $03E0)
          else                        inc(fr, (fg shr 16) and $03E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(dbPix);
          inc(NativeInt(sbLine), sbDif);
        end;
        dbPix := pointer(dbBits + ix * 2);
        for iy := 0 to dstBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := pcp^.weight;
            dw1 := tbBuf^[pcp^.pixel];
            inc(fr, (dw1 shr 10   ) * weight);
            inc(fg, (dw1 and $03E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $7C00
          else                        fr := (fr shr 6) and $7C00;
          if      fg <         0 then
          else if fg > $03E00000 then inc(fr, $03E0)
          else                        inc(fr, (fg shr 16) and $03E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(NativeInt(dbPix), dbDif);
        end;
      end;
    FreeMem(tbBuf);
  end;

  procedure Resampling16;  // 370 / 195
  var ix, iy, ic     : integer;
      weight         : integer;
      clx, cly       : TContributorList;
      fr, fg, fb, fm : integer;
      sbBits, dbBits : NativeInt;
      smBits, dmBits : NativeInt;
      sbDif,  dbDif  : integer;
      smDif,  dmDif  : integer;
      sbLine, tbBuf  : PWordArray;
      smLine, tmBuf  : PByteArray;
      dbPix          : ^word;
      dmPix          : ^byte;
      pcp            : ^TContributorPixel;
      dw1            : integer;
  begin
    GetContributorList(clx, srcBmp.Width,  dstBmp.Width,  rfMitchell);
    GetContributorList(cly, srcBmp.Height, dstBmp.Height, rfMitchell);
    tbBuf  := AllocMem(srcBmp.Height * 2 + srcBmp.Height * 1);
    sbBits := NativeUInt(srcBmp.ScanLine[0]);
    dbBits := NativeUInt(dstBmp.ScanLine[0]);
    if srcBmp.Height > 1 then
      sbDif := NativeInt(srcBmp.ScanLine[1]) - sbBits
    else
      sbDif := 0;
    if dstBmp.Height > 1 then
      dbDif := NativeInt(dstBmp.ScanLine[1]) - dbBits
    else
      dbDif := 0;
    if srcMsk <> nil then begin
      tmBuf  := pointer(NativeInt(tbBuf) + srcBmp.Height * 2);
      smBits := NativeInt(srcMsk.ScanLine[0]);
      dmBits := NativeInt(dstMsk.ScanLine[0]);
      if srcMsk.Height > 1 then
        smDif := NativeInt(srcMsk.ScanLine[1]) - smBits
      else
        smDif := 0;
      if dstMsk.Height > 1 then
        dmDif := NativeInt(dstMsk.ScanLine[1]) - dmBits
      else
        dmDif := 0;
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        dmPix  := pointer(tmBuf );
        sbLine := pointer(sbBits);
        smLine := pointer(smBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          fm  := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - smLine^[pcp^.pixel])) div 256;
            inc(fm, weight);
            dw1 := sbLine^[pcp^.pixel];
            inc(fr, (dw1 shr 11   ) * weight);
            inc(fg, (dw1 and $07E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $F800
          else                        fr := (fr shr 5) and $F800;
          if      fg <         0 then
          else if fg > $07E00000 then inc(fr, $07E0)
          else                        inc(fr, (fg shr 16) and $07E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(dbPix);
          if      fm <         0 then dmPix^ := 255
          else if fm > $0000FF00 then dmPix^ := 0
          else                        dmPix^ := 255 - fm shr 8;
          inc(dmPix);
          inc(NativeInt(sbLine), sbDif);
          inc(NativeInt(smLine), smDif);
        end;
        dbPix := pointer(dbBits + ix * 2);
        dmPix := pointer(dmBits + ix    );
        for iy := 0 to dstBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          fm  := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - tmBuf^[pcp^.pixel])) div 256;
            inc(fm, weight);
            dw1 := tbBuf^[pcp^.pixel];
            inc(fr, (dw1 shr 11   ) * weight);
            inc(fg, (dw1 and $07E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $F800
          else                        fr := (fr shr 5) and $F800;
          if      fg <         0 then
          else if fg > $07E00000 then inc(fr, $07E0)
          else                        inc(fr, (fg shr 16) and $07E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(NativeInt(dbPix), dbDif);
          if      fm <         0 then dmPix^ := 255
          else if fm > $0000FF00 then dmPix^ := 0
          else                        dmPix^ := 255 - fm shr 8;
          inc(dmPix, dmDif);
        end;
      end;
    end else
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        sbLine := pointer(sbBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := pcp^.weight;
            dw1 := sbLine^[pcp^.pixel];
            inc(fr, (dw1 shr 11   ) * weight);
            inc(fg, (dw1 and $07E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $F800
          else                        fr := (fr shr 5) and $F800;
          if      fg <         0 then
          else if fg > $07E00000 then inc(fr, $07E0)
          else                        inc(fr, (fg shr 16) and $07E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(dbPix);
          inc(NativeInt(sbLine), sbDif);
        end;
        dbPix := pointer(dbBits + ix * 2);
        for iy := 0 to dstBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := pcp^.weight;
            dw1 := tbBuf^[pcp^.pixel];
            inc(fr, (dw1 shr 11   ) * weight);
            inc(fg, (dw1 and $07E0) * weight);
            inc(fb, (dw1 and $001F) * weight);
            inc(pcp);
          end;
          if      fr <         0 then fr := 0
          else if fr > $001F0000 then fr := $F800
          else                        fr := (fr shr 5) and $F800;
          if      fg <         0 then
          else if fg > $07E00000 then inc(fr, $07E0)
          else                        inc(fr, (fg shr 16) and $07E0);
          if      fb <         0 then
          else if fb > $001F0000 then inc(fr, $001F)
          else                        inc(fr, fb shr 16);
          dbPix^ := word(fr);
          inc(NativeInt(dbPix), dbDif);
        end;
      end;
    FreeMem(tbBuf);
  end;

  procedure Resampling24;  // 385 / 200
  type TRGB = packed record b, g, r: byte; end;
  var ix, iy, ic     : integer;
      weight         : integer;
      clx, cly       : TContributorList;
      artx, arty     : integer;  // anti ringing threshold x/y
      fr, fg, fb, fm : integer;
      rl, gl, bl     : integer;
      rh, gh, bh     : integer;
      sbBits, dbBits : NativeInt;
      smBits, dmBits : NativeInt;
      sbDif,  dbDif  : integer;
      smDif,  dmDif  : integer;
      sbLine, tbBuf  : PByteArray;
      hBuf, lBuf     : PByteArray;
      smLine, tmBuf  : PByteArray;
      dbPix          : ^byte;
      hPix, lPix     : ^byte;
      dmPix          : ^byte;
      pcp            : ^TContributorPixel;
      rgb            : TRGB;
  begin
    tbBuf := AllocMem(srcBmp.Height * 4 + srcBmp.Height * 1);
    sbBits := NativeInt(srcBmp.ScanLine[0]);
    dbBits := NativeInt(dstBmp.ScanLine[0]);
    if srcBmp.Height > 1 then
      sbDif := NativeInt(srcBmp.ScanLine[1]) - sbBits
    else
      sbDif := 0;
    if dstBmp.Height > 1 then
      dbDif := NativeInt(dstBmp.ScanLine[1]) - dbBits - 2
    else
      dbDif := 0;
    if srcMsk <> nil then begin
      GetContributorList(clx, srcBmp.Width,  dstBmp.Width,  rfMitchell);
      GetContributorList(cly, srcBmp.Height, dstBmp.Height, rfMitchell);
      tmBuf := pointer(NativeInt(tbBuf) + srcBmp.Height * 4);
      smBits := NativeInt(srcMsk.ScanLine[0]);
      dmBits := NativeInt(dstMsk.ScanLine[0]);
      if srcMsk.Height > 1 then
        smDif := NativeInt(srcMsk.ScanLine[1]) - smBits
      else
        smDif := 0;
      if dstMsk.Height > 1 then
        dmDif := NativeInt(dstMsk.ScanLine[1]) - dmBits
      else
        dmDif := 0;
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        sbLine := pointer(sbBits);
        dmPix  := pointer(tmBuf );
        smLine := pointer(smBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr := 0;
          fg := 0;
          fb := 0;
          fm := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - smLine^[pcp^.pixel])) div 256;
            inc(fm, weight);
            rgb := TRGB(pointer(@sbLine^[pcp^.pixel * 3])^);
            inc(fr, rgb.r * weight);
            inc(fg, rgb.g * weight);
            inc(fb, rgb.b * weight);
            inc(pcp);
          end;
          if      fb <       0 then dbPix^ := 0
          else if fb > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fb shr 16;
          inc(dbPix);
          if      fg <       0 then dbPix^ := 0
          else if fg > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fg shr 16;
          inc(dbPix);
          if      fr <       0 then dbPix^ := 0
          else if fr > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fr shr 16;
          inc(dbPix, 2);
          if      fm <       0 then dmPix^ := 255
          else if fm > $00FF00 then dmPix^ := 0
          else                      dmPix^ := 255 - fm shr 8;
          inc(dmPix);
          inc(NativeInt(sbLine), sbDif);
          inc(NativeInt(smLine), smDif);
        end;
        dbPix := pointer(dbBits + ix * 3);
        dmPix := pointer(dmBits + ix    );
        for iy := 0 to dstBmp.Height - 1 do begin
          fr := 0;
          fg := 0;
          fb := 0;
          fm := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - tmBuf^[pcp^.pixel])) div 256;
            inc(fm, weight);
            rgb := TRGB(pointer(@tbBuf^[pcp^.pixel * 4])^);
            inc(fr, rgb.r * weight);
            inc(fg, rgb.g * weight);
            inc(fb, rgb.b * weight);
            inc(pcp);
          end;
          if      fb <       0 then dbPix^ := 0
          else if fb > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fb shr 16;
          inc(dbPix);
          if      fg <       0 then dbPix^ := 0
          else if fg > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fg shr 16;
          inc(dbPix);
          if      fr <       0 then dbPix^ := 0
          else if fr > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fr shr 16;
          inc(dbPix, dbDif);
          if      fm <       0 then dmPix^ := 255
          else if fm > $00FF00 then dmPix^ := 0
          else                      dmPix^ := 255 - fm shr 8;
          inc(dmPix, dmDif);
        end;
      end;
    end else begin
      hBuf := AllocMem(srcBmp.Height * 4 + srcBmp.Height * 1);
      lBuf := AllocMem(srcBmp.Height * 4 + srcBmp.Height * 1);
      if srcBmp.Width = dstBmp.Width then
        GetContributorList(clx, srcBmp.Width, dstBmp.Width, rfNearest)
      else
        GetContributorList(clx, srcBmp.Width, dstBmp.Width, filter);
      if srcBmp.Height = dstBmp.Height then
        GetContributorList(cly, srcBmp.Height, dstBmp.Height, rfNearest)
      else
        GetContributorList(cly, srcBmp.Height, dstBmp.Height, filter);
      if srcBmp.Width < dstBmp.Width then
        artx := 2
      else
        artx := Ceil(srcBmp.Width / dstBmp.Width);
      if srcBmp.Height < dstBmp.Height then
        arty := 2
      else
        arty := Ceil(srcBmp.Height / dstBmp.Height);
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        hPix   := pointer(hBuf  );
        lPix   := pointer(lBuf  );
        sbLine := pointer(sbBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr := 0;
          fg := 0;
          fb := 0;
          rl := 255;
          gl := 255;
          bl := 255;
          rh := 0;
          gh := 0;
          bh := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := pcp^.weight;
            rgb := TRGB(pointer(@sbLine^[pcp^.pixel * 3])^);
            if ic < artx then begin
              if rgb.r > rh then
                rh := rgb.r;
              if rgb.r < rl then
                rl := rgb.r;
              if rgb.g > gh then
                gh := rgb.g;
              if rgb.g < gl then
                gl := rgb.g;
              if rgb.b > bh then
                bh := rgb.b;
              if rgb.b < bl then
                bl := rgb.b;
              inc(fr, rgb.r * weight);
              inc(fg, rgb.g * weight);
              inc(fb, rgb.b * weight);
            end else begin
              {$ifndef newantiringing}
                inc(fr, rgb.r * weight);
                inc(fg, rgb.g * weight);
                inc(fb, rgb.b * weight);
              {$else}
                if rh - rl > 32 then
                  inc(fr, rgb.r * weight)
                else
                  if rgb.r > rh then
                    inc(fr, (rh + (rgb.r - rh) * (rh - rl) div 32) * weight)
                  else
                    if rgb.r < rl then
                      inc(fr, (rl - (rl - rgb.r) * (rh - rl) div 32) * weight)
                    else
                      inc(fr, rgb.r * weight);
                if gh - gl > 32 then
                  inc(fg, rgb.g * weight)
                else
                  if rgb.g > gh then
                    inc(fg, (gh + (rgb.g - gh) * (gh - gl) div 32) * weight)
                  else
                    if rgb.g < gl then
                      inc(fg, (gl - (gl - rgb.g) * (gh - gl) div 32) * weight)
                    else
                      inc(fg, rgb.g * weight);
                if bh - bl > 32 then
                  inc(fb, rgb.b * weight)
                else
                  if rgb.b > bh then
                    inc(fb, (bh + (rgb.b - bh) * (bh - bl) div 32) * weight)
                  else
                    if rgb.b < bl then
                      inc(fb, (bl - (bl - rgb.b) * (bh - bl) div 32) * weight)
                    else
                      inc(fb, rgb.b * weight);
              {$endif}
            end;
            inc(pcp);
          end;
          if      fb <       0 then fb := 0
          else if fb > $FF0000 then fb := 255
          else                      fb := (fb + $8000) shr 16;
          dbPix^ := fb;
          inc(dbPix);
          hPix^ := bh;
          lPix^ := bl;
          inc(hPix);
          inc(lPix);
          if      fg <       0 then fg := 0
          else if fg > $FF0000 then fg := 255
          else                      fg := (fg + $8000) shr 16;
          dbPix^ := fg;
          inc(dbPix);
          hPix^ := gh;
          lPix^ := gl;
          inc(hPix);
          inc(lPix);
          if      fr <       0 then fr := 0
          else if fr > $FF0000 then fr := 255
          else                      fr := (fr + $8000) shr 16;
          dbPix^ := fr;
          inc(dbPix, 2);
          hPix^ := rh;
          lPix^ := rl;
          inc(hPix, 2);
          inc(lPix, 2);
          inc(NativeInt(sbLine), sbDif);
        end;
        dbPix := pointer(dbBits + ix * 3);
        for iy := 0 to dstBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          rl := 255;
          gl := 255;
          bl := 255;
          rh := 0;
          gh := 0;
          bh := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := pcp^.weight;
            rgb := TRGB(pointer(@tbBuf^[pcp^.pixel * 4])^);
            if ic < arty then begin
              inc(fr, rgb.r * weight);
              inc(fg, rgb.g * weight);
              inc(fb, rgb.b * weight);
              rgb := TRGB(pointer(@hBuf^[pcp^.pixel * 4])^);
              if rgb.r > rh then
                rh := rgb.r;
              if rgb.g > gh then
                gh := rgb.g;
              if rgb.b > bh then
                bh := rgb.b;
              rgb := TRGB(pointer(@lBuf^[pcp^.pixel * 4])^);
              if rgb.r < rl then
                rl := rgb.r;
              if rgb.g < gl then
                gl := rgb.g;
              if rgb.b < bl then
                bl := rgb.b;
            end else begin
              {$ifndef newantiringing}
                inc(fr, rgb.r * weight);
                inc(fg, rgb.g * weight);
                inc(fb, rgb.b * weight);
              {$else}
                if rh - rl > 32 then
                  inc(fr, rgb.r * weight)
                else
                  if rgb.r > rh then
                    inc(fr, (rh + (rgb.r - rh) * (rh - rl) div 32) * weight)
                  else
                    if rgb.r < rl then
                      inc(fr, (rl - (rl - rgb.r) * (rh - rl) div 32) * weight)
                    else
                      inc(fr, rgb.r * weight);
                if gh - gl > 32 then
                  inc(fg, rgb.g * weight)
                else
                  if rgb.g > gh then
                    inc(fg, (gh + (rgb.g - gh) * (gh - gl) div 32) * weight)
                  else
                    if rgb.g < gl then
                      inc(fg, (gl - (gl - rgb.g) * (gh - gl) div 32) * weight)
                    else
                      inc(fg, rgb.g * weight);
                if bh - bl > 32 then
                  inc(fb, rgb.b * weight)
                else
                  if rgb.b > bh then
                    inc(fb, (bh + (rgb.b - bh) * (bh - bl) div 32) * weight)
                  else
                    if rgb.b < bl then
                      inc(fb, (bl - (bl - rgb.b) * (bh - bl) div 32) * weight)
                    else
                      inc(fb, rgb.b * weight);
              {$endif}
            end;
            inc(pcp);
          end;
          if      fb <       0 then fb := 0
          else if fb > $FF0000 then fb := 255
          else                      fb := (fb + $8000) shr 16;
          {$ifdef oldantiringing}
            if fb > bh then
              fb := bh
            else
              if fb < bl then
                fb := bl;
          {$endif}
          dbPix^ := fb;
          inc(dbPix);
          if      fg <       0 then fg := 0
          else if fg > $FF0000 then fg := 255
          else                      fg := (fg + $8000) shr 16;
          {$ifdef oldantiringing}
            if fg > gh then
              fg := gh
            else
              if fg < gl then
                fg := gl;
          {$endif}
          dbPix^ := fg;
          inc(dbPix);
          if      fr <       0 then fr := 0
          else if fr > $FF0000 then fr := 255
          else                      fr := (fr + $8000) shr 16;
          {$ifdef oldantiringing}
            if fr > rh then
              fr := rh
            else
              if fr < rl then
                fr := rl;
          {$endif}
          dbPix^ := fr;
          inc(dbPix, dbDif);
        end;
      end;
      FreeMem(lBuf);
      FreeMem(hBuf);
    end;
    FreeMem(tbBuf);
  end;

  procedure Resampling32;  // 375 / 200
  type TRGB = packed record b, g, r, a: byte; end;
  var ix, iy, ic     : integer;
      weight         : integer;
      clx, cly       : TContributorList;
      artx, arty     : integer;  // anti ringing threshold x/y
      fr, fg, fb, fm : integer;
      rl, gl, bl     : integer;
      rh, gh, bh     : integer;
      sbBits, dbBits : NativeInt;
      smBits, dmBits : NativeInt;
      sbDif,  dbDif  : integer;
      smDif,  dmDif  : integer;
      sbLine, tbBuf  : PByteArray;
      hBuf, lBuf     : PByteArray;
      smLine, tmBuf  : PByteArray;
      dbPix          : ^byte;
      hPix, lPix     : ^byte;
      dmPix          : ^byte;
      pcp            : ^TContributorPixel;
      rgb            : TRGB;
  begin
    tbBuf := AllocMem(srcBmp.Height * 4 + srcBmp.Height * 1);
    sbBits := NativeInt(srcBmp.ScanLine[0]);
    dbBits := NativeInt(dstBmp.ScanLine[0]);
    if srcBmp.Height > 1 then
      sbDif := NativeInt(srcBmp.ScanLine[1]) - sbBits
    else
      sbDif := 0;
    if dstBmp.Height > 1 then
      dbDif := NativeInt(dstBmp.ScanLine[1]) - dbBits - 2
    else
      dbDif := 0;
    if srcMsk <> nil then begin
      GetContributorList(clx, srcBmp.Width,  dstBmp.Width,  rfMitchell);
      GetContributorList(cly, srcBmp.Height, dstBmp.Height, rfMitchell);
      tmBuf := pointer(NativeInt(tbBuf) + srcBmp.Height * 4);
      smBits := NativeInt(srcMsk.ScanLine[0]);
      dmBits := NativeInt(dstMsk.ScanLine[0]);
      if srcMsk.Height > 1 then
        smDif := NativeInt(srcMsk.ScanLine[1]) - smBits
      else
        smDif := 0;
      if dstMsk.Height > 1 then
        dmDif := NativeInt(dstMsk.ScanLine[1]) - dmBits
      else
        dmDif := 0;
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        sbLine := pointer(sbBits);
        dmPix  := pointer(tmBuf );
        smLine := pointer(smBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr := 0;
          fg := 0;
          fb := 0;
          fm := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - smLine^[pcp^.pixel])) div 256;
            inc(fm, weight);
            rgb := TRGB(pointer(@sbLine^[pcp^.pixel * 4])^);
            inc(fr, rgb.r * weight);
            inc(fg, rgb.g * weight);
            inc(fb, rgb.b * weight);
            inc(pcp);
          end;
          if      fb <       0 then dbPix^ := 0
          else if fb > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fb shr 16;
          inc(dbPix);
          if      fg <       0 then dbPix^ := 0
          else if fg > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fg shr 16;
          inc(dbPix);
          if      fr <       0 then dbPix^ := 0
          else if fr > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fr shr 16;
          inc(dbPix, 2);
          if      fm <       0 then dmPix^ := 255
          else if fm > $00FF00 then dmPix^ := 0
          else                      dmPix^ := 255 - fm shr 8;
          inc(dmPix);
          inc(NativeInt(sbLine), sbDif);
          inc(NativeInt(smLine), smDif);
        end;
        dbPix := pointer(dbBits + ix * 4);
        dmPix := pointer(dmBits + ix    );
        for iy := 0 to dstBmp.Height - 1 do begin
          fr := 0;
          fg := 0;
          fb := 0;
          fm := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := (pcp^.weight * (256 - tmBuf^[pcp^.pixel])) div 256;
            inc(fm, weight);
            rgb := TRGB(pointer(@tbBuf^[pcp^.pixel * 4])^);
            inc(fr, rgb.r * weight);
            inc(fg, rgb.g * weight);
            inc(fb, rgb.b * weight);
            inc(pcp);
          end;
          if      fb <       0 then dbPix^ := 0
          else if fb > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fb shr 16;
          inc(dbPix);
          if      fg <       0 then dbPix^ := 0
          else if fg > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fg shr 16;
          inc(dbPix);
          if      fr <       0 then dbPix^ := 0
          else if fr > $FF0000 then dbPix^ := 255
          else                      dbPix^ := fr shr 16;
          inc(dbPix, dbDif);
          if      fm <       0 then dmPix^ := 255
          else if fm > $00FF00 then dmPix^ := 0
          else                      dmPix^ := 255 - fm shr 8;
          inc(dmPix, dmDif);
        end;
      end;
    end else begin
      hBuf := AllocMem(srcBmp.Height * 4 + srcBmp.Height * 1);
      lBuf := AllocMem(srcBmp.Height * 4 + srcBmp.Height * 1);
      if srcBmp.Width = dstBmp.Width then
        GetContributorList(clx, srcBmp.Width, dstBmp.Width, rfNearest)
      else
        GetContributorList(clx, srcBmp.Width, dstBmp.Width, filter);
      if srcBmp.Height = dstBmp.Height then
        GetContributorList(cly, srcBmp.Height, dstBmp.Height, rfNearest)
      else
        GetContributorList(cly, srcBmp.Height, dstBmp.Height, filter);
      if srcBmp.Width < dstBmp.Width then
        artx := 2
      else
        artx := srcBmp.Width div dstBmp.Width + 1;
      if srcBmp.Height < dstBmp.Height then
        arty := 2
      else
        arty := srcBmp.Height div dstBmp.Height + 1;
      for ix := 0 to dstBmp.Width - 1 do begin
        dbPix  := pointer(tbBuf );
        hPix   := pointer(hBuf  );
        lPix   := pointer(lBuf  );
        sbLine := pointer(sbBits);
        for iy := 0 to srcBmp.Height - 1 do begin
          fr := 0;
          fg := 0;
          fb := 0;
          rl := 255;
          gl := 255;
          bl := 255;
          rh := 0;
          gh := 0;
          bh := 0;
          pcp := @clx[ix].items[0];
          for ic := 0 to clx[ix].itemCount - 1 do begin
            weight := pcp^.weight;
            rgb := TRGB(pointer(@sbLine^[pcp^.pixel * 4])^);
            if ic < artx then begin
              if rgb.r > rh then
                rh := rgb.r;
              if rgb.r < rl then
                rl := rgb.r;
              if rgb.g > gh then
                gh := rgb.g;
              if rgb.g < gl then
                gl := rgb.g;
              if rgb.b > bh then
                bh := rgb.b;
              if rgb.b < bl then
                bl := rgb.b;
            end;
            inc(fr, rgb.r * weight);
            inc(fg, rgb.g * weight);
            inc(fb, rgb.b * weight);
            inc(pcp);
          end;
          if      fb <       0 then fb := 0
          else if fb > $FF0000 then fb := 255
          else                      fb := (fb + $8000) shr 16;
          dbPix^ := fb;
          inc(dbPix);
          hPix^ := bh;
          lPix^ := bl;
          inc(hPix);
          inc(lPix);
          if      fg <       0 then fg := 0
          else if fg > $FF0000 then fg := 255
          else                      fg := (fg + $8000) shr 16;
          dbPix^ := fg;
          inc(dbPix);
          hPix^ := gh;
          lPix^ := gl;
          inc(hPix);
          inc(lPix);
          if      fr <       0 then fr := 0
          else if fr > $FF0000 then fr := 255
          else                      fr := (fr + $8000) shr 16;
          dbPix^ := fr;
          inc(dbPix, 2);
          hPix^ := rh;
          lPix^ := rl;
          inc(hPix, 2);
          inc(lPix, 2);
          inc(NativeInt(sbLine), sbDif);
        end;
        dbPix := pointer(dbBits + ix * 4);
        for iy := 0 to dstBmp.Height - 1 do begin
          fr  := 0;
          fg  := 0;
          fb  := 0;
          rl := 255;
          gl := 255;
          bl := 255;
          rh := 0;
          gh := 0;
          bh := 0;
          pcp := @cly[iy].items[0];
          for ic := 0 to cly[iy].itemCount - 1 do begin
            weight := pcp^.weight;
            rgb := TRGB(pointer(@tbBuf^[pcp^.pixel * 4])^);
            inc(fr, rgb.r * weight);
            inc(fg, rgb.g * weight);
            inc(fb, rgb.b * weight);
            if ic < arty then begin
              rgb := TRGB(pointer(@hBuf^[pcp^.pixel * 4])^);
              if rgb.r > rh then
                rh := rgb.r;
              if rgb.g > gh then
                gh := rgb.g;
              if rgb.b > bh then
                bh := rgb.b;
              rgb := TRGB(pointer(@lBuf^[pcp^.pixel * 4])^);
              if rgb.r < rl then
                rl := rgb.r;
              if rgb.g < gl then
                gl := rgb.g;
              if rgb.b < bl then
                bl := rgb.b;
            end;
            inc(pcp);
          end;
          if      fb <       0 then fb := 0
          else if fb > $FF0000 then fb := 255
          else                      fb := (fb + $8000) shr 16;
          {$ifdef antiringing}
          if fb > bh then
            fb := bh
          else
            if fb < bl then
              fb := bl;
          {$endif}
          dbPix^ := fb;
          inc(dbPix);
          if      fg <       0 then fg := 0
          else if fg > $FF0000 then fg := 255
          else                      fg := (fg + $8000) shr 16;
          {$ifdef antiringing}
          if fg > gh then
            fg := gh
          else
            if fg < gl then
              fg := gl;
          {$endif}
          dbPix^ := fg;
          inc(dbPix);
          if      fr <       0 then fr := 0
          else if fr > $FF0000 then fr := 255
          else                      fr := (fr + $8000) shr 16;
          {$ifdef antiringing}
          if fr > rh then
            fr := rh
          else
            if fr < rl then
              fr := rl;
          {$endif}
          dbPix^ := fr;
          inc(dbPix, dbDif);
        end;
      end;
      FreeMem(lBuf);
      FreeMem(hBuf);
    end;
    FreeMem(tbBuf);
  end;

  procedure Bilinear15;  // 2820 -> 540
  var ix, iy                   : integer;
      x, y, xdif, ydif         : integer;
      xp1, xp2, yp             : integer;
      wy, wyi, wx              : integer;
      w11, w21, w12, w22       : integer;
      sbBits, sbLine1, sbLine2 : NativeInt;
      smBits, smLine1, smLine2 : PByteArray;
      dbLine                   : NativeInt;
      dmLine                   : ^byte;
      sbLineDif, dbLineDif     : integer;
      smLineDif, dmLineDif     : integer;
      w                        : integer;
  begin
    y := 0;
    xdif := (srcBmp.Width  shl 16) div dstBmp.Width;
    ydif := (srcBmp.Height shl 16) div dstBmp.Height;
    sbBits := NativeInt(srcBmp.ScanLine[0]);
    if srcBmp.Height > 1 then
      sbLineDif := NativeInt(srcBmp.ScanLine[1]) - sbBits
    else
      sbLineDif := 0;
    dbLine := NativeInt(dstBmp.ScanLine[0]);
    if dstBmp.Height > 1 then
      dbLineDif := NativeInt(dstBmp.ScanLine[1]) - dbLine - 2 * dstBmp.Width
    else
      dbLineDif := 0;
    if srcMsk <> nil then begin
      smBits := srcMsk.ScanLine[0];
      if srcMsk.Height > 1 then
        smLineDif := NativeInt(srcMsk.ScanLine[1]) - NativeInt(smBits)
      else
        smLineDif := 0;
      dmLine := dstMsk.ScanLine[0];
      if dstMsk.Height > 1 then
        dmLineDif := NativeInt(dstMsk.ScanLine[1]) - NativeInt(dmLine) - 1 * dstBmp.Width
      else
        dmLineDif := 0;
    end else begin
      smBits    := nil;
      smLineDif := 0;
      dmLine    := nil;
      dmLineDif := 0;
    end;
    w := srcBmp.Width - 1;
    for iy := 0 to dstBmp.Height - 1 do begin
      yp := y shr 16;
      sbLine1 := NativeInt(sbBits) + sbLineDif * yp;
      smLine1 := pointer(NativeInt(smBits) + smLineDif * yp);
      if yp < srcBmp.Height - 1 then begin
        sbLine2 := NativeInt(sbLine1) + sbLineDif;
        smLine2 := pointer(NativeInt(smLine1) + smLineDif);
      end else begin
        sbLine2 := sbLine1;
        smLine2 := smLine1;
      end;
      x   := 0;
      wy  :=      y  and $FFFF;
      wyi := (not y) and $FFFF;
      for ix := 0 to dstBmp.Width - 1 do begin
        xp1 := x shr 16;
        if xp1 < w then
          xp2 := xp1 + 1
        else
          xp2 := xp1;
        wx  := x and $FFFF;
        w21 := (wyi * wx) shr 16; w11 := wyi - w21;
        w22 := (wy  * wx) shr 16; w12 := wy  - w22;
        if smLine1 <> nil then begin
          w11 := (w11 * (256 - smLine1^[xp1])) shr 8;
          w21 := (w21 * (256 - smLine1^[xp2])) shr 8;
          w12 := (w12 * (256 - smLine2^[xp1])) shr 8;
          w22 := (w22 * (256 - smLine2^[xp2])) shr 8;
          dmLine^ := 255 - byte((w11 + w21 + w12 + w22) shr 8);
        end;
        xp1 := xp1 * 2;
        xp2 := xp2 * 2;
        PWord(dbLine)^ := ( ( ( (PWord(sbLine1 + xp1)^ and $001F) * w11 + (PWord(sbLine1 + xp2)^ and $001F) * w21 +
                                (PWord(sbLine2 + xp1)^ and $001F) * w12 + (PWord(sbLine2 + xp2)^ and $001F) * w22   ) shr 16) and $001F) or
                          ( ( ( (PWord(sbLine1 + xp1)^ and $03E0) * w11 + (PWord(sbLine1 + xp2)^ and $03E0) * w21 +
                                (PWord(sbLine2 + xp1)^ and $03E0) * w12 + (PWord(sbLine2 + xp2)^ and $03E0) * w22   ) shr 16) and $03E0) or
                          ( ( ( (PWord(sbLine1 + xp1)^ and $7C00) * w11 + (PWord(sbLine1 + xp2)^ and $7C00) * w21 +
                                (PWord(sbLine2 + xp1)^ and $7C00) * w12 + (PWord(sbLine2 + xp2)^ and $7C00) * w22   ) shr 16) and $7C00);
        inc(dbLine, 2);
        inc(dmLine);
        inc(x, xdif);
      end;
      inc(NativeInt(dbLine), dbLineDif);
      inc(NativeInt(dmLine), dmLineDif);
      inc(y, ydif);
    end;
  end;

  procedure Bilinear16;  // 2440 -> 540
  var ix, iy                   : integer;
      x, y, xdif, ydif         : integer;
      xp1, xp2, yp             : integer;
      wy, wyi, wx              : integer;
      w11, w21, w12, w22       : integer;
      sbBits, sbLine1, sbLine2 : NativeInt;
      smBits, smLine1, smLine2 : PByteArray;
      dbLine                   : NativeInt;
      dmLine                   : ^byte;
      sbLineDif, dbLineDif     : integer;
      smLineDif, dmLineDif     : integer;
      w                        : integer;
  begin
    y := 0;
    xdif := (srcBmp.Width  shl 16) div dstBmp.Width;
    ydif := (srcBmp.Height shl 16) div dstBmp.Height;
    sbBits := NativeInt(srcBmp.ScanLine[0]);
    if srcBmp.Height > 1 then
      sbLineDif := NativeInt(srcBmp.ScanLine[1]) - sbBits
    else
      sbLineDif := 0;
    dbLine := NativeInt(dstBmp.ScanLine[0]);
    if dstBmp.Height > 1 then
      dbLineDif := NativeInt(dstBmp.ScanLine[1]) - dbLine - 2 * dstBmp.Width
    else
      dbLineDif := 0;
    if srcMsk <> nil then begin
      smBits := srcMsk.ScanLine[0];
      if srcMsk.Height > 1 then
        smLineDif := NativeInt(srcMsk.ScanLine[1]) - NativeInt(smBits)
      else
        smLineDif := 0;
      dmLine := dstMsk.ScanLine[0];
      if dstMsk.Height > 1 then
        dmLineDif := NativeInt(dstMsk.ScanLine[1]) - NativeInt(dmLine) - 1 * dstBmp.Width
      else
        dmLineDif := 0;
    end else begin
      smBits    := nil;
      smLineDif := 0;
      dmLine    := nil;
      dmLineDif := 0;
    end;
    w := srcBmp.Width - 1;
    for iy := 0 to dstBmp.Height - 1 do begin
      yp := y shr 16;
      sbLine1 := NativeInt(sbBits) + sbLineDif * yp;
      smLine1 := pointer(NativeInt(smBits) + smLineDif * yp);
      if yp < srcBmp.Height - 1 then begin
        sbLine2 := NativeInt(sbLine1) + sbLineDif;
        smLine2 := pointer(NativeInt(smLine1) + smLineDif);
      end else begin
        sbLine2 := sbLine1;
        smLine2 := smLine1;
      end;
      x   := 0;
      wy  :=      y  and $FFFF;
      wyi := (not y) and $FFFF;
      for ix := 0 to dstBmp.Width - 1 do begin
        xp1 := x shr 16;
        if xp1 < w then
          xp2 := xp1 + 1
        else
          xp2 := xp1;
        wx  := x and $FFFF;
        w21 := (wyi * wx) shr 16; w11 := wyi - w21;
        w22 := (wy  * wx) shr 16; w12 := wy  - w22;
        if smLine1 <> nil then begin
          w11 := (w11 * (256 - smLine1^[xp1])) shr 8;
          w21 := (w21 * (256 - smLine1^[xp2])) shr 8;
          w12 := (w12 * (256 - smLine2^[xp1])) shr 8;
          w22 := (w22 * (256 - smLine2^[xp2])) shr 8;
          dmLine^ := 255 - byte((w11 + w21 + w12 + w22) shr 8);
        end;
        xp1 := xp1 * 2;
        xp2 := xp2 * 2;
        PWord(dbLine)^ := ( ( ( (PWord(sbLine1 + xp1)^ and $001F) * w11 + (PWord(sbLine1 + xp2)^ and $001F) * w21 +
                                (PWord(sbLine2 + xp1)^ and $001F) * w12 + (PWord(sbLine2 + xp2)^ and $001F) * w22   ) shr 16) and $001F) or
                          ( ( ( (PWord(sbLine1 + xp1)^ and $07E0) * w11 + (PWord(sbLine1 + xp2)^ and $07E0) * w21 +
                                (PWord(sbLine2 + xp1)^ and $07E0) * w12 + (PWord(sbLine2 + xp2)^ and $07E0) * w22   ) shr 16) and $07E0) or
                          ( ( ( (PWord(sbLine1 + xp1)^ and $F800) * w11 + (PWord(sbLine1 + xp2)^ and $F800) * w21 +
                                (PWord(sbLine2 + xp1)^ and $F800) * w12 + (PWord(sbLine2 + xp2)^ and $F800) * w22   ) shr 16) and $F800);
        inc(dbLine, 2);
        inc(dmLine);
        inc(x, xdif);
      end;
      inc(NativeInt(dbLine), dbLineDif);
      inc(NativeInt(dmLine), dmLineDif);
      inc(y, ydif);
    end;
  end;

  procedure Bilinear24;  // 550 -> 310
  var ix, iy                   : integer;
      x, y, xdif, ydif         : integer;
      xp1, xp2, yp             : integer;
      wy, wyi, wx              : integer;
      w11, w21, w12, w22       : integer;
      sbBits, sbLine1, sbLine2 : PByteArray;
      smBits, smLine1, smLine2 : PByteArray;
      dbLine                   : PByteArray;
      dmLine                   : ^byte;
      sbLineDif, dbLineDif     : integer;
      smLineDif, dmLineDif     : integer;
      w                        : integer;
  begin
    y := 0;
    xdif := (srcBmp.Width  shl 16) div dstBmp.Width;
    ydif := (srcBmp.Height shl 16) div dstBmp.Height;
    sbBits := srcBmp.ScanLine[0];
    if srcBmp.Height > 1 then
      sbLineDif := NativeInt(srcBmp.ScanLine[1]) - NativeInt(sbBits)
    else
      sbLineDif := 0;
    dbLine := dstBmp.ScanLine[0];
    if dstBmp.Height > 1 then
      dbLineDif := NativeInt(dstBmp.ScanLine[1]) - NativeInt(dbLine) - 3 * dstBmp.Width
    else
      dbLineDif := 0;
    if srcMsk <> nil then begin
      smBits := srcMsk.ScanLine[0];
      if srcMsk.Height > 1 then
        smLineDif := NativeInt(srcMsk.ScanLine[1]) - NativeInt(smBits)
      else
        smLineDif := 0;
      dmLine := dstMsk.ScanLine[0];
      if dstMsk.Height > 1 then
        dmLineDif := NativeInt(dstMsk.ScanLine[1]) - NativeInt(dmLine) - 1 * dstBmp.Width
      else
        dmLineDif := 0;
    end else begin
      smBits    := nil;
      smLineDif := 0;
      dmLine    := nil;
      dmLineDif := 0;
    end;
    w := srcBmp.Width - 1;
    for iy := 0 to dstBmp.Height - 1 do begin
      yp := y shr 16;
      sbLine1 := pointer(NativeInt(sbBits) + sbLineDif * yp);
      smLine1 := pointer(NativeInt(smBits) + smLineDif * yp);
      if yp < srcBmp.Height - 1 then begin
        sbLine2 := pointer(NativeInt(sbLine1) + sbLineDif);
        smLine2 := pointer(NativeInt(smLine1) + smLineDif);
      end else begin
        sbLine2 := sbLine1;
        smLine2 := smLine1;
      end;
      x   := 0;
      wy  :=      y  and $FFFF;
      wyi := (not y) and $FFFF;
      for ix := 0 to dstBmp.Width - 1 do begin
        xp1 := x shr 16;
        if xp1 < w then
          xp2 := xp1 + 1
        else
          xp2 := xp1;
        wx  := x and $FFFF;
        w21 := (wyi * wx) shr 16; w11 := wyi - w21;
        w22 := (wy  * wx) shr 16; w12 := wy  - w22;
        if smLine1 <> nil then begin
          w11 := (w11 * (256 - smLine1^[xp1])) shr 8;
          w21 := (w21 * (256 - smLine1^[xp2])) shr 8;
          w12 := (w12 * (256 - smLine2^[xp1])) shr 8;
          w22 := (w22 * (256 - smLine2^[xp2])) shr 8;
          dmLine^ := 255 - byte((w11 + w21 + w12 + w22) shr 8);
        end;
        xp1 := xp1 * 3;
        xp2 := xp2 * 3;
        dbLine^[0] := (sbLine1[xp1    ] * w11 + sbLine1[xp2    ] * w21 + sbLine2[xp1    ] * w12 + sbLine2[xp2    ] * w22) shr 16;
        dbLine^[1] := (sbLine1[xp1 + 1] * w11 + sbLine1[xp2 + 1] * w21 + sbLine2[xp1 + 1] * w12 + sbLine2[xp2 + 1] * w22) shr 16;
        dbLine^[2] := (sbLine1[xp1 + 2] * w11 + sbLine1[xp2 + 2] * w21 + sbLine2[xp1 + 2] * w12 + sbLine2[xp2 + 2] * w22) shr 16;
        inc(NativeInt(dbLine), 3);
        inc(dmLine);
        inc(x, xdif);
      end;
      inc(NativeInt(dbLine), dbLineDif);
      inc(NativeInt(dmLine), dmLineDif);
      inc(y, ydif);
    end;
  end;

  procedure Bilinear32;  // 525 -> 305
  var ix, iy                   : integer;
      x, y, xdif, ydif         : integer;
      xp1, xp2, yp             : integer;
      wy, wyi, wx              : integer;
      w11, w21, w12, w22       : integer;
      sbBits, sbLine1, sbLine2 : PByteArray;
      smBits, smLine1, smLine2 : PByteArray;
      dbLine                   : PByteArray;
      dmLine                   : ^byte;
      sbLineDif, dbLineDif     : integer;
      smLineDif, dmLineDif     : integer;
      w                        : integer;
  begin
    y := 0;
    xdif := (srcBmp.Width  shl 16) div dstBmp.Width;
    ydif := (srcBmp.Height shl 16) div dstBmp.Height;
    sbBits := srcBmp.ScanLine[0];
    if srcBmp.Height > 1 then
      sbLineDif := NativeInt(srcBmp.ScanLine[1]) - NativeInt(sbBits)
    else
      sbLineDif := 0;
    dbLine := dstBmp.ScanLine[0];
    if dstBmp.Height > 1 then
      dbLineDif := NativeInt(dstBmp.ScanLine[1]) - NativeInt(dbLine) - 4 * dstBmp.Width
    else
      dbLineDif := 0;
    if srcMsk <> nil then begin
      smBits := srcMsk.ScanLine[0];
      if srcMsk.Height > 1 then
        smLineDif := NativeInt(srcMsk.ScanLine[1]) - NativeInt(smBits)
      else
        smLineDif := 0;
      dmLine := dstMsk.ScanLine[0];
      if dstMsk.Height > 1 then
        dmLineDif := NativeInt(dstMsk.ScanLine[1]) - NativeInt(dmLine) - 1 * dstBmp.Width
      else
        dmLineDif := 0;
    end else begin
      smBits    := nil;
      smLineDif := 0;
      dmLine    := nil;
      dmLineDif := 0;
    end;
    w := srcBmp.Width - 1;
    for iy := 0 to dstBmp.Height - 1 do begin
      yp := y shr 16;
      sbLine1 := pointer(NativeInt(sbBits) + sbLineDif * yp);
      smLine1 := pointer(NativeInt(smBits) + smLineDif * yp);
      if yp < srcBmp.Height - 1 then begin
        sbLine2 := pointer(NativeInt(sbLine1) + sbLineDif);
        smLine2 := pointer(NativeInt(smLine1) + smLineDif);
      end else begin
        sbLine2 := sbLine1;
        smLine2 := smLine1;
      end;
      x   := 0;
      wy  :=      y  and $FFFF;
      wyi := (not y) and $FFFF;
      for ix := 0 to dstBmp.Width - 1 do begin
        xp1 := x shr 16;
        if xp1 < w then
          xp2 := xp1 + 1
        else
          xp2 := xp1;
        wx  := x and $FFFF;
        w21 := (wyi * wx) shr 16; w11 := wyi - w21;
        w22 := (wy  * wx) shr 16; w12 := wy  - w22;
        if smLine1 <> nil then begin
          w11 := (w11 * (256 - smLine1^[xp1])) shr 8;
          w21 := (w21 * (256 - smLine1^[xp2])) shr 8;
          w12 := (w12 * (256 - smLine2^[xp1])) shr 8;
          w22 := (w22 * (256 - smLine2^[xp2])) shr 8;
          dmLine^ := 255 - byte((w11 + w21 + w12 + w22) shr 8);
        end;
        xp1 := xp1 * 4;
        xp2 := xp2 * 4;
        dbLine^[0] := (sbLine1[xp1    ] * w11 + sbLine1[xp2    ] * w21 + sbLine2[xp1    ] * w12 + sbLine2[xp2    ] * w22) shr 16;
        dbLine^[1] := (sbLine1[xp1 + 1] * w11 + sbLine1[xp2 + 1] * w21 + sbLine2[xp1 + 1] * w12 + sbLine2[xp2 + 1] * w22) shr 16;
        dbLine^[2] := (sbLine1[xp1 + 2] * w11 + sbLine1[xp2 + 2] * w21 + sbLine2[xp1 + 2] * w12 + sbLine2[xp2 + 2] * w22) shr 16;
        inc(NativeInt(dbLine), 4);
        inc(dmLine);
        inc(x, xdif);
      end;
      inc(NativeInt(dbLine), dbLineDif);
      inc(NativeInt(dmLine), dmLineDif);
      inc(y, ydif);
    end;
  end;

begin
  if (srcMsk <> nil) and
     ((dstMsk = nil) or
      (srcMsk.PixelFormat <> pf8Bit) or (srcMsk.Width <> srcBmp.Width) or (srcMsk.Height <> srcBmp.Height) ) then
    srcMsk := nil;
  if srcMsk <> nil then begin
    dstMsk.PixelFormat := pf8bit;
    dstMsk.Width       := dstBmp.Width;
    dstMsk.Height      := dstBmp.Height;
  end else
    dstMsk := nil;
  case quality of
    sqLow      : dstBmp.Canvas.StretchDraw(Rect(0, 0, dstBmp.Width, dstBmp.Height), srcBmp);
    sqHigh     : case GetPixelFormat(dstBmp) of
                   15 : if ForcePixelFormat([srcBmp], pf15bit) then Bilinear15;
                   16 : if ForcePixelFormat([srcBmp], pf16bit) then Bilinear16;
                   24 : if ForcePixelFormat([srcBmp], pf24bit) then Bilinear24;
                   32 : if ForcePixelFormat([srcBmp], pf32bit) then Bilinear32;
                 end;
    sqVeryHigh : case GetPixelFormat(dstBmp) of
                   15 : if ForcePixelFormat([srcBmp], pf15bit) then Resampling15;
                   16 : if ForcePixelFormat([srcBmp], pf16bit) then Resampling16;
                   24 : if ForcePixelFormat([srcBmp], pf24bit) then Resampling24;
                   32 : if ForcePixelFormat([srcBmp], pf32bit) then Resampling32;
                 end;
  end;
end;

procedure StretchBitmap(srcBmp, dstBmp : TBitmap;
                        srcMsk, dstMsk : TBitmap;
                        quality        : TStretchQuality = sqHigh);
begin
  StretchBitmap_(srcBmp, dstBmp, srcMsk, dstMsk, quality);
end;

procedure StretchBitmap_(bmp, msk  : TBitmap;
                         newWidth  : integer;
                         newHeight : integer;
                         quality   : TStretchQuality = sqHigh;
                         filter    : TResamplingFilter = rfLanczos4); overload;
var newBmp, newMsk : TBitmap;
begin
  newBmp := TBitmap.Create;
  try
    if GetPixelFormat(bmp) = 15 then
      newBmp.PixelFormat := pf15Bit
    else
      newBmp.PixelFormat := bmp.PixelFormat;
    newBmp.Width  := newWidth;
    newBmp.Height := newHeight;
    newMsk := nil;
    try
      if (msk <> nil) and ((msk.PixelFormat <> pf8Bit) or (msk.Width <> bmp.Width) or (msk.Height <> bmp.Height)) then
        msk := nil;
      if msk <> nil then
        newMsk := TBitmap.Create;
      StretchBitmap_(bmp, newBmp, msk, newMsk, quality, filter);
      bmp.Assign(newBmp);
      if msk <> nil then
        msk.Assign(newMsk);
    finally newMsk.Free end;
  finally newBmp.Free end;
end;

procedure StretchBitmap(bmp, msk  : TBitmap;
                        newWidth  : integer;
                        newHeight : integer;
                        quality   : TStretchQuality = sqHigh);
begin
  StretchBitmap_(bmp, msk, newWidth, newHeight, quality);
end;

// ***************************************************************

procedure AlphaBlend_(src1, src2, msk, dst : TBitmap;
                      alpha                : cardinal = 128;
                      x                    : integer  = 0;
                      y                    : integer  = 0  );
var w, h, s1x, s1y, s1ld, s2x, s2y, s2ld, dx, dy, dld, mld : integer;

  procedure AlphaBlend15(src1, src2, dst: TBitmap; alpha: cardinal);  // 395
  var src1Line, src2Line, dstLine : ^cardinal;
      mskLine                     : ^byte;
      ix, iy, iw                  : integer;
      i1, i2                      : cardinal;
  begin
    src1Line := src1.ScanLine[s1y];
    src2Line := src2.ScanLine[s2y];
    dstLine  := dst .ScanLine[ dy];
    if msk <> nil then begin
      mskLine := msk.ScanLine[s1y];
      if h > 1 then begin
        s1ld := NativeInt(src1.ScanLine[s1y + 1]) - NativeInt(src1Line) - w * 2;
        s2ld := NativeInt(src2.ScanLine[s2y + 1]) - NativeInt(src2Line) - w * 2;
        dld  := NativeInt(dst .ScanLine[ dy + 1]) - NativeInt( dstLine) - w * 2;
        mld  := NativeInt(msk .ScanLine[s1y + 1]) - NativeInt( mskLine) - w;
      end else begin
        s1ld := 0;
        s2ld := 0;
        dld  := 0;
        mld  := 0;
      end;
      inc(NativeInt(src1Line), s1x * 2);
      inc(NativeInt(src2Line), s2x * 2);
      inc(NativeInt( dstLine),  dx * 2);
      inc(NativeInt( mskLine), s1x    );
      for iy := 0 to h - 1 do begin
        for ix := 0 to w - 1 do begin
          if mskLine^ < 255 then begin
            i1 := 256 - (((cardinal(256) - mskLine^) * alpha) shr 8);
            i2 :=      ((alpha * (PWord(src1Line)^ and $001F) + i1 * (PWord(src2Line)^ and $001F)) shr 8) and $001F;
            i2 := i2 + ((alpha * (PWord(src1Line)^ and $03E0) + i1 * (PWord(src2Line)^ and $03E0)) shr 8) and $03E0;
            i2 := i2 + ((alpha * (PWord(src1Line)^ and $7C00) + i1 * (PWord(src2Line)^ and $7C00)) shr 8) and $7C00;
            PWord(dstLine)^ := i2;
          end;
          inc(NativeInt(src1Line), 2);
          inc(NativeInt(src2Line), 2);
          inc(NativeInt( dstLine), 2);
          inc(mskLine);
        end;
        inc(NativeInt(src1Line), s1ld);
        inc(NativeInt(src2Line), s2ld);
        inc(NativeInt( dstLine),  dld);
        inc(NativeInt( mskLine),  mld);
      end;
    end else begin
      iw := w div 2;
      if h > 1 then begin
        s1ld := NativeInt(src1.ScanLine[s1y + 1]) - NativeInt(src1Line) - iw * 4;
        s2ld := NativeInt(src2.ScanLine[s2y + 1]) - NativeInt(src2Line) - iw * 4;
        dld  := NativeInt(dst .ScanLine[ dy + 1]) - NativeInt( dstLine) - iw * 4;
      end else begin
        s1ld := 0;
        s2ld := 0;
        dld  := 0;
      end;
      inc(NativeInt(src1Line), s1x * 2);
      inc(NativeInt(src2Line), s2x * 2);
      inc(NativeInt( dstLine),  dx * 2);
      for iy := 0 to h - 1 do begin
        for ix := 0 to iw - 1 do begin
          i1 := (src2Line^ and $001F001F);
          i2 :=      (((((src1Line^ and $001F001F)       - i1) * alpha) shr 8 + i1)      ) and $001F001F;
          i1 := (src2Line^ and $03E003E0) shr 4;
          i2 := i2 + (((((src1Line^ and $03E003E0) shr 4 - i1) * alpha) shr 8 + i1) shl 4) and $03E003E0;
          i1 := (src2Line^ and $7C007C00) shr 8;
          i2 := i2 + (((((src1Line^ and $7C007C00) shr 8 - i1) * alpha) shr 8 + i1) shl 8) and $7C007C00;
          dstLine^ := i2;
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
        end;
        inc(NativeInt(src1Line), s1ld);
        inc(NativeInt(src2Line), s2ld);
        inc(NativeInt( dstLine),  dld);
      end;
      if odd(w) then begin
        src1Line := pointer(NativeInt(src1.ScanLine[s1y]) + iw * 4 + s1x * 2);
        src2Line := pointer(NativeInt(src2.ScanLine[s2y]) + iw * 4 + s2x * 2);
         dstLine := pointer(NativeInt(dst .ScanLine[ dy]) + iw * 4 +  dx * 2);
        inc(s1ld, iw * 4);
        inc(s2ld, iw * 4);
        inc( dld, iw * 4);
        for iy := 0 to h - 1 do begin
          i1 := PWord(src2Line)^ and $001F;
          i2 :=      ((((PWord(src1Line)^ and $001F) - i1) * alpha) shr 8 + i1) and $001F;
          i1 := PWord(src2Line)^ and $03E0;
          i2 := i2 + ((((PWord(src1Line)^ and $03E0) - i1) * alpha) shr 8 + i1) and $03E0;
          i1 := PWord(src2Line)^ and $7C00;
          i2 := i2 + ((((PWord(src1Line)^ and $7C00) - i1) * alpha) shr 8 + i1) and $7C00;
          PWord(dstLine)^ := i2;
          inc(NativeInt(src1Line), s1ld);
          inc(NativeInt(src2Line), s2ld);
          inc(NativeInt( dstLine),  dld);
        end;
      end;
    end;
  end;

  procedure AlphaBlend16(src1, src2, dst: TBitmap; alpha: cardinal);  // 395
  var src1Line, src2Line, dstLine : ^cardinal;
      mskLine                     : ^byte;
      ix, iy, iw                  : integer;
      i1, i2                      : cardinal;
  begin
    src1Line := src1.ScanLine[s1y];
    src2Line := src2.ScanLine[s2y];
     dstLine := dst .ScanLine[ dy];
    if msk <> nil then begin
      mskLine := msk.ScanLine[s1y];
      if h > 1 then begin
        s1ld := NativeInt(src1.ScanLine[s1y + 1]) - NativeInt(src1Line) - w * 2;
        s2ld := NativeInt(src2.ScanLine[s2y + 1]) - NativeInt(src2Line) - w * 2;
        dld  := NativeInt(dst .ScanLine[ dy + 1]) - NativeInt( dstLine) - w * 2;
        mld  := NativeInt(msk .ScanLine[s1y + 1]) - NativeInt( mskLine) - w;
      end else begin
        s1ld := 0;
        s2ld := 0;
        dld  := 0;
        mld  := 0;
      end;
      inc(NativeInt(src1Line), s1x * 2);
      inc(NativeInt(src2Line), s2x * 2);
      inc(NativeInt( dstLine),  dx * 2);
      inc(NativeInt( mskLine), s1x    );
      for iy := 0 to h - 1 do begin
        for ix := 0 to w - 1 do begin
          if mskLine^ < 255 then begin
            i1 := 256 - (((cardinal(256) - mskLine^) * alpha) shr 8);
            i2 :=      ((alpha * (PWord(src1Line)^ and $001F) + i1 * (PWord(src2Line)^ and $001F)) shr 8) and $001F;
            i2 := i2 + ((alpha * (PWord(src1Line)^ and $07E0) + i1 * (PWord(src2Line)^ and $07E0)) shr 8) and $07E0;
            i2 := i2 + ((alpha * (PWord(src1Line)^ and $F800) + i1 * (PWord(src2Line)^ and $F800)) shr 8) and $F800;
            PWord(dstLine)^ := i2;
          end;
          inc(NativeInt(src1Line), 2);
          inc(NativeInt(src2Line), 2);
          inc(NativeInt( dstLine), 2);
          inc(mskLine);
        end;
        inc(NativeInt(src1Line), s1ld);
        inc(NativeInt(src2Line), s2ld);
        inc(NativeInt( dstLine),  dld);
        inc(NativeInt( mskLine),  mld);
      end;
    end else begin
      iw := w div 2;
      if h > 1 then begin
        s1ld := NativeInt(src1.ScanLine[s1y + 1]) - NativeInt(src1Line) - iw * 4;
        s2ld := NativeInt(src2.ScanLine[s2y + 1]) - NativeInt(src2Line) - iw * 4;
        dld  := NativeInt(dst .ScanLine[ dy + 1]) - NativeInt( dstLine) - iw * 4;
      end else begin
        s1ld := 0;
        s2ld := 0;
        dld  := 0;
      end;
      inc(NativeInt(src1Line), s1x * 2);
      inc(NativeInt(src2Line), s2x * 2);
      inc(NativeInt( dstLine),  dx * 2);
      for iy := 0 to h - 1 do begin
        for ix := 0 to iw - 1 do begin
          i1 := (src2Line^ and $001F001F);
          i2 :=      (((((src1Line^ and $001F001F)       - i1) * alpha) shr 8 + i1)      ) and $001F001F;
          i1 := (src2Line^ and $07E007E0) shr 4;
          i2 := i2 + (((((src1Line^ and $07E007E0) shr 4 - i1) * alpha) shr 8 + i1) shl 4) and $07E007E0;
          i1 := (src2Line^ and $F800F800) shr 8;
          i2 := i2 + (((((src1Line^ and $F800F800) shr 8 - i1) * alpha) shr 8 + i1) shl 8) and $F800F800;
          dstLine^ := i2;
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
        end;
        inc(NativeInt(src1Line), s1ld);
        inc(NativeInt(src2Line), s2ld);
        inc(NativeInt( dstLine),  dld);
      end;
      if odd(w) then begin
        src1Line := pointer(NativeInt(src1.ScanLine[s1y]) + iw * 4 + s1x * 2);
        src2Line := pointer(NativeInt(src2.ScanLine[s2y]) + iw * 4 + s2x * 2);
         dstLine := pointer(NativeInt(dst .ScanLine[ dy]) + iw * 4 +  dx * 2);
        inc(s1ld, iw * 4);
        inc(s2ld, iw * 4);
        inc( dld, iw * 4);
        for iy := 0 to h - 1 do begin
          i1 := PWord(src2Line)^ and $001F;
          i2 :=      ((((PWord(src1Line)^ and $001F) - i1) * alpha) shr 8 + i1) and $001F;
          i1 := PWord(src2Line)^ and $07E0;
          i2 := i2 + ((((PWord(src1Line)^ and $07E0) - i1) * alpha) shr 8 + i1) and $07E0;
          i1 := PWord(src2Line)^ and $F800;
          i2 := i2 + ((((PWord(src1Line)^ and $F800) - i1) * alpha) shr 8 + i1) and $F800;
          PWord(dstLine)^ := i2;
          inc(NativeInt(src1Line), s1ld);
          inc(NativeInt(src2Line), s2ld);
          inc(NativeInt( dstLine),  dld);
        end;
      end;
    end;
  end;

  procedure AlphaBlend24(src1, src2, dst: TBitmap; alpha: cardinal);  // 770
  var src1Line, src2Line, dstLine : PByte;
      mskLine                     : ^byte;
      ix, iy, iw                  : integer;
      i1                          : cardinal;
  begin
    src1Line := src1.ScanLine[s1y];
    src2Line := src2.ScanLine[s2y];
     dstLine := dst .ScanLine[ dy];
    iw       := w - 1;
    if h > 1 then begin
      s1ld := NativeInt(src1.ScanLine[s1y + 1]) - NativeInt(src1Line) - w * 3;
      s2ld := NativeInt(src2.ScanLine[s2y + 1]) - NativeInt(src2Line) - w * 3;
      dld  := NativeInt(dst .ScanLine[ dy + 1]) - NativeInt( dstLine) - w * 3;
    end else begin
      s1ld := 0;
      s2ld := 0;
      dld  := 0;
    end;
    inc(NativeInt(src1Line), s1x * 3);
    inc(NativeInt(src2Line), s2x * 3);
    inc(NativeInt( dstLine),  dx * 3);
    if msk <> nil then begin
      mskLine := msk.ScanLine[s1y];
      if h > 1 then
        mld := NativeInt(msk.ScanLine[s1y + 1]) - NativeInt( mskLine) - w
      else
        mld := 0;
      inc(NativeInt(mskLine), s1x);
    end else begin
      mskLine := nil;
      mld     := 0;
    end;
    for iy := 0 to h - 1 do begin
      if mskLine <> nil then begin
        for ix := 0 to iw do begin
          if mskLine^ < 255 then begin
            i1 := 256 - (((cardinal(256) - mskLine^) * alpha) shr 8);
            dstLine^ := byte((alpha * src1Line^ + i1 * src2Line^) shr 8);
            inc(src1Line);
            inc(src2Line);
            inc( dstLine);
            dstLine^ := byte((alpha * src1Line^ + i1 * src2Line^) shr 8);
            inc(src1Line);
            inc(src2Line);
            inc( dstLine);
            dstLine^ := byte((alpha * src1Line^ + i1 * src2Line^) shr 8);
            inc(src1Line);
            inc(src2Line);
            inc( dstLine);
          end else begin
            inc(src1Line, 3);
            inc(src2Line, 3);
            inc( dstLine, 3);
          end;
          inc(mskLine);
        end;
      end else
        for ix := 0 to iw do begin
          i1 := src2Line^;
          dstLine^ := byte(((src1Line^ - i1) * alpha) shr 8 + i1);
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
          i1 := src2Line^;
          dstLine^ := byte(((src1Line^ - i1) * alpha) shr 8 + i1);
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
          i1 := src2Line^;
          dstLine^ := byte(((src1Line^ - i1) * alpha) shr 8 + i1);
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
        end;
      inc(NativeInt(src1Line), s1ld);
      inc(NativeInt(src2Line), s2ld);
      inc(NativeInt( dstLine),  dld);
      inc(NativeInt( mskLine),  mld);
    end;
  end;

  procedure AlphaBlend32(src1, src2, dst: TBitmap; alpha: cardinal);  // 630
  var src1Line, src2Line, dstLine : ^cardinal;
      mskLine                     : ^byte;
      ix, iy, iw                  : integer;
      i1, i2                      : cardinal;
  begin
    src1Line := src1.ScanLine[s1y];
    src2Line := src2.ScanLine[s2y];
     dstLine := dst .ScanLine[ dy];
    iw       := w - 1;
    if h > 1 then begin
      s1ld := NativeInt(src1.ScanLine[s1y + 1]) - NativeInt(src1Line) - w * 4;
      s2ld := NativeInt(src2.ScanLine[s2y + 1]) - NativeInt(src2Line) - w * 4;
      dld  := NativeInt(dst .ScanLine[ dy + 1]) - NativeInt( dstLine) - w * 4;
    end else begin
      s1ld := 0;
      s2ld := 0;
      dld  := 0;
    end;
    inc(NativeInt(src1Line), s1x * 4);
    inc(NativeInt(src2Line), s2x * 4);
    inc(NativeInt( dstLine),  dx * 4);
    if msk <> nil then begin
      mskLine := msk.ScanLine[s1y];
      if h > 1 then
        mld := NativeInt(msk.ScanLine[s1y + 1]) - NativeInt( mskLine) - w
      else
        mld := 0;
      inc(NativeInt(mskLine), s1x);
    end else begin
      mskLine := nil;
      mld     := 0;
    end;
    for iy := 0 to h - 1 do begin
      if mskLine <> nil then begin
        for ix := 0 to iw do begin
          if mskLine^ < 255 then begin
            i1 := 256 - (((cardinal(256) - mskLine^) * alpha) shr 8);
            i2 :=      ((alpha * (src1Line^ and $00FF00FF) + i1 * (src2Line^ and $00FF00FF)) shr 8) and $00FF00FF;
            i2 := i2 + ((alpha * (src1Line^ and $0000FF00) + i1 * (src2Line^ and $0000FF00)) shr 8) and $0000FF00;
            dstLine^ := i2;
          end;
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
          inc( mskLine);
        end;
      end else
        for ix := 0 to iw do begin
          i1 := src2Line^ and $00FF00FF;
          i2 :=      ((((src1Line^ and $00FF00FF) - i1) * alpha) shr 8 + i1) and $00FF00FF;
          i1 := src2Line^ and $0000FF00;
          i2 := i2 + ((((src1Line^ and $0000FF00) - i1) * alpha) shr 8 + i1) and $0000FF00;
          dstLine^ := i2;
          inc(src1Line);
          inc(src2Line);
          inc( dstLine);
        end;
      inc(NativeInt(src1Line), s1ld);
      inc(NativeInt(src2Line), s2ld);
      inc(NativeInt( dstLine),  dld);
      inc(NativeInt( mskLine),  mld);
    end;
  end;

begin
  if dst = src2 then begin
    if x < 0 then begin
      w   := Min(src1.Width + x, dst.Width);
      s1x := -x;
      dx  := 0;
    end else  begin
      w   := Min(src1.Width, dst.Width - x);
      s1x := 0;
      dx  := x;
    end;
    if y < 0 then begin
      h   := Min(src1.Height + y, dst.Height);
      s1y := -y;
      dy  := 0;
    end else  begin
      h   := Min(src1.Height, dst.Height - y);
      s1y := 0;
      dy  := y;
    end;
    s2x := dx;
    s2y := dy;
    if (msk <> nil) and ((msk.PixelFormat <> pf8Bit) or (msk.Width <> src1.Width) or (msk.Height <> src1.Height)) then
      msk := nil;  
  end else begin
    w := Min(src1.Width,  Min(src2.Width,  dst.Width ));
    h := Min(src1.Height, Min(src2.Height, dst.Height));
    s1x := 0; s2x := 0; dx := 0;
    s1y := 0; s2y := 0; dy := 0;
  end;
  case GetPixelFormat(dst) of
    15 : if ForcePixelFormat([src1, src2], pf15bit) then AlphaBlend15(src1, src2, dst, alpha);
    16 : if ForcePixelFormat([src1, src2], pf16bit) then AlphaBlend16(src1, src2, dst, alpha);
    24 : if ForcePixelFormat([src1, src2], pf24bit) then AlphaBlend24(src1, src2, dst, alpha);
    32 : if ForcePixelFormat([src1, src2], pf32bit) then AlphaBlend32(src1, src2, dst, alpha);
  end;
end;

procedure AlphaBlend(src1, src2, dst : TBitmap;
                     alpha           : cardinal = 128);
begin
  AlphaBlend_(src1, src2, nil, dst, alpha);
end;

procedure AlphaBlend(src, dst : TBitmap;
                     alpha    : cardinal = 128;
                     msk      : TBitmap  = nil;
                     x        : integer  = 0;
                     y        : integer  = 0  );
begin
  AlphaBlend_(src, dst, msk, dst, alpha, x, y);
end;

// ***************************************************************

procedure Draw(imageList, index : cardinal;
               dst              : TBitmap;
               x                : integer         = 0;
               y                : integer         = 0;
               width            : integer         = 0;
               height           : integer         = 0;
               grayPercent      : TGrayPercent    = gp0;
               alpha            : cardinal        = 256;
               stretchQuality   : TStretchQuality = sqHigh);
var bmp, msk : TBitmap;
    ii       : TImageInfo;
begin
  if ImageList_GetImageInfo(imageList, index, ii) then begin
    bmp := TBitmap.Create;
    try
      case GetPixelFormat(dst) of
        15 : bmp.PixelFormat := pf15bit;
        16 : bmp.PixelFormat := pf16bit;
        24 : bmp.PixelFormat := pf24bit;
        else bmp.PixelFormat := pf32bit;
      end;
      bmp.Width  := ii.rcImage.Right  - ii.rcImage.Left;
      bmp.Height := ii.rcImage.Bottom - ii.rcImage.Top;
      ImageList_Draw(imageList, index, bmp.Canvas.Handle, 0, 0, ILD_NORMAL);
      msk := nil;
      try
        if ii.hbmMask <> 0 then begin
          msk := TBitmap.Create;
          msk.PixelFormat := pf8bit;
          msk.Width       := ii.rcImage.Right  - ii.rcImage.Left;
          msk.Height      := ii.rcImage.Bottom - ii.rcImage.Top;
          ImageList_Draw(imageList, index, msk.Canvas.Handle, 0, 0, ILD_MASK);
        end;
        if ((width <> 0) and (width <> bmp.Width)) or ((height <> 0) and (height <> bmp.Height)) then begin
          if width  = 0 then width  := bmp.Width;
          if height = 0 then height := bmp.Height;
          StretchBitmap(bmp, msk, width, height, stretchQuality);
        end;
        GrayScale(bmp, grayPercent);
        AlphaBlend(bmp, dst, alpha, msk, x, y);
      finally msk.Free end;
    finally bmp.Free end;
  end;
end;

procedure Draw(bmp, msk       : TBitmap;
               dst            : TBitmap;
               x              : integer         = 0;
               y              : integer         = 0;
               width          : integer         = 0;
               height         : integer         = 0;
               grayPercent    : TGrayPercent    = gp0;
               alpha          : cardinal        = 256;
               stretchQuality : TStretchQuality = sqHigh);
begin
  if width  = 0 then width  := bmp.Width;
  if height = 0 then height := bmp.Height;
  if (width <> bmp.Width) or (height <> bmp.Height) then
    StretchBitmap(bmp, msk, width, height, stretchQuality);
  GrayScale(bmp, grayPercent);
  AlphaBlend(bmp, dst, alpha, msk, x, y);
end;

// ***************************************************************

type
  TYCbCr = packed record y, cb, cr, helper : single end;
  TAYCbCr = array [0..maxInt shr 4 - 1] of TYCbCr;
  TPAYCbCr = ^TAYCbCr;

function Sign(const value: double) : integer;
begin
  if ((PInt64(@value)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000) then
    result := 0
  else
    if ((PInt64(@value)^ and $8000000000000000) = $8000000000000000) then
      result := -1
    else
      result := +1;
end;

procedure ICBI1Plane(src, dst: TPAYCbCr; width, height: integer; const stepSizes: array of integer; coef1, coef2: double);
const
  GM = 5.0;   // Weight for Isophote smoothing energy (default = 5.0 -> 19.0)
  BT = -1.0;  // Weight for Curvature enhancement energy (default = -1.0 -> -3.0)
  AL = 1.0;   // Weight for Curvature Continuity energy (default = 1.0 -> 0.7)
  TS = 100;   // Threshold on image change for stopping iterations (default = 100)

var D1, D2, D3, C1, C2 : array of array of single;
    pixels : array of array of TYCbCr;
    i1, i2 : integer;
    p1, p2 : single;
    step : integer;
    g : integer;
    diff : integer;
    EN1, EN2, EN3, EN4, EN5, EN6 : single;
    EA1, EA2, EA3, EA4, EA5, EA6 : single;
    ES1, ES2, ES3, ES4, ES5, ES6 : single;
    EN, EA, ES : single;
    EISO : double;
    newWidth, newHeight : integer;
    ix, iy : integer;
    maxValue, testValue : single;
begin
  newWidth  := width  * 2 - 1;
  newHeight := height * 2 - 1;

  SetLength(D1, newWidth, newHeight);
  SetLength(D2, newWidth, newHeight);
  SetLength(D3, newWidth, newHeight);
  SetLength(C1, newWidth, newHeight);
  SetLength(C2, newWidth, newHeight);

  SetLength(pixels, newWidth, newHeight);
  for ix := 0 to width - 1 do
    for iy := 0 to height - 1 do
      pixels[ix * 2, iy * 2] := src[iy * width + ix];

  ix := 1;
  iy := newHeight - 1;
  for i1 := 1 to width - 1 do begin
    pixels[ix, 0 ].y  := (pixels[ix + 1, 0 ].y  + pixels[ix - 1, 0 ].y ) / 2;
    pixels[ix, 0 ].cb := (pixels[ix + 1, 0 ].cb + pixels[ix - 1, 0 ].cb) / 2;
    pixels[ix, 0 ].cr := (pixels[ix + 1, 0 ].cr + pixels[ix - 1, 0 ].cr) / 2;
    pixels[ix, iy].y  := (pixels[ix + 1, iy].y  + pixels[ix - 1, iy].y ) / 2;
    pixels[ix, iy].cb := (pixels[ix + 1, iy].cb + pixels[ix - 1, iy].cb) / 2;
    pixels[ix, iy].cr := (pixels[ix + 1, iy].cr + pixels[ix - 1, iy].cr) / 2;
    inc(ix, 2);
  end;
  ix := newWidth - 1;
  iy := 1;
  for i1 := 1 to height - 1 do begin
    pixels[0,  iy].y  := (pixels[0,  iy + 1].y  + pixels[0,  iy - 1].y ) / 2;
    pixels[0,  iy].cb := (pixels[0,  iy + 1].cb + pixels[0,  iy - 1].cb) / 2;
    pixels[0,  iy].cr := (pixels[0,  iy + 1].cr + pixels[0,  iy - 1].cr) / 2;
    pixels[ix, iy].y  := (pixels[ix, iy + 1].y  + pixels[ix, iy - 1].y ) / 2;
    pixels[ix, iy].cb := (pixels[ix, iy + 1].cb + pixels[ix, iy - 1].cb) / 2;
    pixels[ix, iy].cr := (pixels[ix, iy + 1].cr + pixels[ix, iy - 1].cr) / 2;
    inc(iy, 2);
  end;

  ix := 1;
  for i1 := 1 to width - 1 do begin
    iy := 1;
    for i2 := 1 to height - 1 do begin
      p1 := (pixels[ix - 1, iy - 1].y + pixels[ix + 1, iy + 1].y) / 2;
      p2 := (pixels[ix + 1, iy - 1].y + pixels[ix - 1, iy + 1].y) / 2;
      if (i1 > 1) and (i1 < width - 1) and (i2 > 1) and (i2 < height - 1) then begin
        if abs(pixels[ix - 1, iy - 3].y + pixels[ix - 3, iy - 1].y + pixels[ix + 1, iy + 3].y + pixels[ix + 3, iy + 1].y + (2 * p2) - (6 * p1)) >
           abs(pixels[ix - 3, iy + 1].y + pixels[ix - 1, iy + 3].y + pixels[ix + 3, iy - 1].y + pixels[ix + 1, iy - 3].y + (2 * p1) - (6 * p2)) then begin
          if (ix > 2) and (iy > 2) and (ix < newWidth - 3) and (iy < newHeight - 3) then
            p1 := (pixels[ix - 1, iy - 1].y + pixels[ix + 1, iy + 1].y) * coef1 +
                  (pixels[ix - 3, iy - 3].y + pixels[ix + 3, iy + 3].y) * coef2;
          pixels[ix, iy].y := p1;
          pixels[ix, iy].cb := (pixels[ix - 1, iy - 1].cb + pixels[ix + 1, iy + 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix - 1, iy - 1].cr + pixels[ix + 1, iy + 1].cr) / 2;
        end else begin
          if (ix > 2) and (iy > 2) and (ix < newWidth - 3) and (iy < newHeight - 3) then
            p2 := (pixels[ix + 1, iy - 1].y + pixels[ix - 1, iy + 1].y) * coef1 +
                  (pixels[ix + 3, iy - 3].y + pixels[ix - 3, iy + 3].y) * coef2;
          pixels[ix, iy].y := p2;
          pixels[ix, iy].cb := (pixels[ix + 1, iy - 1].cb + pixels[ix - 1, iy + 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix + 1, iy - 1].cr + pixels[ix - 1, iy + 1].cr) / 2;
        end;
      end else
        if abs(pixels[ix - 1, iy - 1].y - pixels[ix + 1, iy + 1].y) < abs(pixels[ix + 1, iy - 1].y - pixels[ix - 1, iy + 1].y) then begin
          pixels[ix, iy].y := p1;
          pixels[ix, iy].cb := (pixels[ix - 1, iy - 1].cb + pixels[ix + 1, iy + 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix - 1, iy - 1].cr + pixels[ix + 1, iy + 1].cr) / 2;
        end else begin
          pixels[ix, iy].y := p2;
          pixels[ix, iy].cb := (pixels[ix + 1, iy - 1].cb + pixels[ix - 1, iy + 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix + 1, iy - 1].cr + pixels[ix - 1, iy + 1].cr) / 2;
        end;
      inc(iy, 2);
    end;
    inc(ix, 2);
  end;

  // iterative refinement
  for g := 0 to high(stepSizes) do begin
    diff := 0;
    step := stepSizes[g];

    // computation of derivatives
    for ix := 3 to newWidth - 4 do begin
      iy := 4 - (ix and 1);
      for i2 := 2 to height - 2 do begin
        C1[ix, iy] := (pixels[ix - 1, iy - 1].y - pixels[ix + 1, iy + 1].y) / 2;
        C2[ix, iy] := (pixels[ix + 1, iy - 1].y - pixels[ix - 1, iy + 1].y) / 2;
        D1[ix, iy] := pixels[ix - 1, iy - 1].y + pixels[ix + 1, iy + 1].y - 2 * pixels[ix, iy].y;
        D2[ix, iy] := pixels[ix + 1, iy - 1].y + pixels[ix - 1, iy + 1].y - 2 * pixels[ix, iy].y;
        D3[ix, iy] := (pixels[ix, iy - 2].y - pixels[ix - 2, iy].y + pixels[ix, iy + 2].y - pixels[ix + 2, iy].y) / 2;
        inc(iy, 2);
      end;
    end;

    ix := 5;
    for i1 := 3 to width - 3 do begin
      iy := 5;
      for i2 := 3 to height - 3 do begin
        EN1 := abs(D1[ix, iy] - D1[ix + 1, iy + 1]) + abs(D1[ix, iy] - D1[ix - 1, iy - 1]);
        EN2 := abs(D1[ix, iy] - D1[ix + 1, iy - 1]) + abs(D1[ix, iy] - D1[ix - 1, iy + 1]);
        EN3 := abs(D2[ix, iy] - D2[ix + 1, iy + 1]) + abs(D2[ix, iy] - D2[ix - 1, iy - 1]);
        EN4 := abs(D2[ix, iy] - D2[ix + 1, iy - 1]) + abs(D2[ix, iy] - D2[ix - 1, iy + 1]);
        EN5 := abs(pixels[ix - 2, iy - 2].y + pixels[ix + 2, iy + 2].y - 2 * pixels[ix, iy].y);
        EN6 := abs(pixels[ix + 2, iy - 2].y + pixels[ix - 2, iy + 2].y - 2 * pixels[ix, iy].y);

        EA1 := abs(D1[ix, iy] - D1[ix + 1, iy + 1] - 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy - 1] - 3 * step);
        EA2 := abs(D1[ix, iy] - D1[ix + 1, iy - 1] - 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy + 1] - 3 * step);
        EA3 := abs(D2[ix, iy] - D2[ix + 1, iy + 1] - 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy - 1] - 3 * step);
        EA4 := abs(D2[ix, iy] - D2[ix + 1, iy - 1] - 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy + 1] - 3 * step);
        EA5 := abs(pixels[ix - 2, iy - 2].y + pixels[ix + 2, iy + 2].y - 2 * pixels[ix, iy].y - 2 * step);
        EA6 := abs(pixels[ix + 2, iy - 2].y + pixels[ix - 2, iy + 2].y - 2 * pixels[ix, iy].y - 2 * step);

        ES1 := abs(D1[ix, iy] - D1[ix + 1, iy + 1] + 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy - 1] + 3 * step);
        ES2 := abs(D1[ix, iy] - D1[ix + 1, iy - 1] + 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy + 1] + 3 * step);
        ES3 := abs(D2[ix, iy] - D2[ix + 1, iy + 1] + 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy - 1] + 3 * step);
        ES4 := abs(D2[ix, iy] - D2[ix + 1, iy - 1] + 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy + 1] + 3 * step);
        ES5 := abs(pixels[ix - 2, iy - 2].y + pixels[ix + 2, iy + 2].y - 2 * pixels[ix, iy].y + 2 * step);
        ES6 := abs(pixels[ix + 2, iy - 2].y + pixels[ix - 2, iy + 2].y - 2 * pixels[ix, iy].y + 2 * step);

        if (C1[ix, iy] <> 0) or (C2[ix, iy] <> 0) then begin
          EISO := (C1[ix, iy] * C1[ix, iy] * D2[ix, iy] - 2 * C1[ix, iy] * C2[ix, iy] * D3[ix, iy] + C2[ix, iy] * C2[ix, iy] * D1[ix, iy]) / (C1[ix, iy] * C1[ix, iy] + C2[ix, iy] * C2[ix, iy]);
          if abs(EISO) < 0.2 then
            EISO := 0;
        end else
          EISO := 0;

        EN := (AL * (EN1 + EN2 + EN3 + EN4)) + (BT * (EN5 + EN6));
        EA := (AL * (EA1 + EA2 + EA3 + EA4)) + (BT * (EA5 + EA6)) - (GM * sign(EISO));
        ES := (AL * (ES1 + ES2 + ES3 + ES4)) + (BT * (ES5 + ES6)) + (GM * sign(EISO));

        if (EN > EA) and (ES > EA) then begin
          if pixels[ix, iy].y + step > 255 then
            pixels[ix, iy].y := 255
          else
            pixels[ix, iy].y := pixels[ix, iy].y + step;

          maxValue := pixels[ix - 1, iy - 1].y;
          testValue := pixels[ix - 1, iy + 1].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix + 1, iy + 1].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix + 1, iy - 1].y;
          if testValue > maxValue then maxValue := testValue;
          if pixels[ix, iy].y > maxValue then
            pixels[ix, iy].y := maxValue;

          diff := diff + step;
        end else
          if (EN > ES) and (EA > ES) then begin
            if pixels[ix, iy].y <= step then
              pixels[ix, iy].y := 0
            else
              pixels[ix, iy].y := pixels[ix, iy].y - step;

            maxValue := pixels[ix - 1, iy - 1].y;
            testValue := pixels[ix - 1, iy + 1].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix + 1, iy + 1].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix + 1, iy - 1].y;
            if testValue < maxValue then maxValue := testValue;
            if pixels[ix, iy].y < maxValue then
              pixels[ix, iy].y := maxValue;

            diff := diff + step;
          end;

        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

    if diff < TS then
      break;
  end;

  for ix := 1 to newWidth - 2 do begin
    iy := 1 + (ix and 1);
    for i2 := 1 to height - 1 do begin
      p1 := (pixels[ix - 1, iy].y + pixels[ix + 1, iy].y) / 2;
      p2 := (pixels[ix, iy - 1].y + pixels[ix, iy + 1].y) / 2;
      if (ix > 1) and (ix < newWidth - 3) and (iy > 1) and (iy < height - 3) then begin
        if abs(pixels[ix - 2, iy - 1].y + pixels[ix - 2, iy + 1].y + pixels[ix + 2, iy + 1].y + pixels[ix + 2, iy - 1].y + (2 * p2) - (6 * p1)) >
           abs(pixels[ix - 1, iy + 2].y + pixels[ix + 1, iy + 2].y + pixels[ix + 1, iy - 2].y + pixels[ix - 1, iy - 2].y + (2 * p1) - (6 * p2)) then begin
          if (ix > 2) and (iy > 2) and (ix < newWidth - 3) and (iy < newHeight - 3) then
            p1 := (pixels[ix - 1, iy].y + pixels[ix + 1, iy].y) * coef1 +
                  (pixels[ix - 3, iy].y + pixels[ix + 3, iy].y) * coef2;
          pixels[ix, iy].y := p1;
          pixels[ix, iy].cb := (pixels[ix - 1, iy].cb + pixels[ix + 1, iy].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix - 1, iy].cr + pixels[ix + 1, iy].cr) / 2;
        end else begin
          if (ix > 2) and (iy > 2) and (ix < newWidth - 3) and (iy < newHeight - 3) then
            p2 := (pixels[ix, iy - 1].y + pixels[ix, iy + 1].y) * coef1 +
                  (pixels[ix, iy - 3].y + pixels[ix, iy + 3].y) * coef2;
          pixels[ix, iy].y := p2;
          pixels[ix, iy].cb := (pixels[ix, iy - 1].cb + pixels[ix, iy + 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix, iy - 1].cr + pixels[ix, iy + 1].cr) / 2;
        end;
      end else
        if abs(pixels[ix - 1, iy].y - pixels[ix + 1, iy].y) < abs(pixels[ix, iy - 1].y - pixels[ix, iy + 1].y) then begin
          pixels[ix, iy].y := p1;
          pixels[ix, iy].cb := (pixels[ix - 1, iy].cb + pixels[ix + 1, iy].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix - 1, iy].cr + pixels[ix + 1, iy].cr) / 2;
        end else begin
          pixels[ix, iy].y := p2;
          pixels[ix, iy].cb := (pixels[ix, iy - 1].cb + pixels[ix, iy + 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix, iy - 1].cr + pixels[ix, iy + 1].cr) / 2;
        end;
      inc(iy, 2);
    end;
  end;

  // iterative refinement
  for g := 0 to high(stepSizes) do begin
    diff := 0;
    step := stepSizes[g];

    // computation of derivatives
    for ix := 1 to newWidth - 3 do
      for iy := 1 to newHeight - 3 do begin
        C1[ix, iy] := (pixels[ix, iy - 1].y - pixels[ix, iy + 1].y) / 2;
        C2[ix, iy] := (pixels[ix - 1, iy].y - pixels[ix + 1, iy].y) / 2;
        D1[ix, iy] := pixels[ix, iy - 1].y + pixels[ix, iy + 1].y - 2 * pixels[ix, iy].y;
        D2[ix, iy] := pixels[ix + 1, iy].y + pixels[ix - 1, iy].y - 2 * pixels[ix, iy].y;
        D3[ix, iy] := (pixels[ix - 1, iy - 1].y - pixels[ix - 1, iy + 1].y + pixels[ix + 1, iy + 1].y - pixels[ix + 1, iy - 1].y) / 2;
      end;

    for ix := 2 to newWidth - 3 do begin
      iy := 3 - (ix and 1);
      for i2 := 1 to height - 2 do begin
        EN1 := abs(D1[ix, iy] - D1[ix, iy + 1]) + abs(D1[ix, iy] - D1[ix, iy - 1]);
        EN2 := abs(D1[ix, iy] - D1[ix + 1, iy]) + abs(D1[ix, iy] - D1[ix - 1, iy]);
        EN3 := abs(D2[ix, iy] - D2[ix, iy + 1]) + abs(D2[ix, iy] - D2[ix, iy - 1]);
        EN4 := abs(D2[ix, iy] - D2[ix + 1, iy]) + abs(D2[ix, iy] - D2[ix - 1, iy]);
        EN5 := abs(pixels[ix, iy - 2].y + pixels[ix, iy + 2].y - 2 * pixels[ix, iy].y);
        EN6 := abs(pixels[ix + 2, iy].y + pixels[ix - 2, iy].y - 2 * pixels[ix, iy].y);

        EA1 := abs(D1[ix, iy] - D1[ix, iy + 1] - 3 * step) + abs(D1[ix, iy] - D1[ix, iy - 1] - 3 * step);
        EA2 := abs(D1[ix, iy] - D1[ix + 1, iy] - 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy] - 3 * step);
        EA3 := abs(D2[ix, iy] - D2[ix, iy + 1] - 3 * step) + abs(D2[ix, iy] - D2[ix, iy - 1] - 3 * step);
        EA4 := abs(D2[ix, iy] - D2[ix + 1, iy] - 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy] - 3 * step);
        EA5 := abs(pixels[ix, iy - 2].y + pixels[ix, iy + 2].y - 2 * pixels[ix, iy].y - 2 * step);
        EA6 := abs(pixels[ix + 2, iy].y + pixels[ix - 2, iy].y - 2 * pixels[ix, iy].y - 2 * step);

        ES1 := abs(D1[ix, iy] - D1[ix, iy + 1] + 3 * step) + abs(D1[ix, iy] - D1[ix, iy - 1] + 3 * step);
        ES2 := abs(D1[ix, iy] - D1[ix + 1, iy] + 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy] + 3 * step);
        ES3 := abs(D2[ix, iy] - D2[ix, iy + 1] + 3 * step) + abs(D2[ix, iy] - D2[ix, iy - 1] + 3 * step);
        ES4 := abs(D2[ix, iy] - D2[ix + 1, iy] + 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy] + 3 * step);
        ES5 := abs(pixels[ix, iy - 2].y + pixels[ix, iy + 2].y - 2 * pixels[ix, iy].y + 2 * step);
        ES6 := abs(pixels[ix + 2, iy].y + pixels[ix - 2, iy].y - 2 * pixels[ix, iy].y + 2 * step);

        if (C1[ix, iy] <> 0) or (C2[ix, iy] <> 0) then begin
          EISO := (C1[ix, iy] * C1[ix, iy] * D2[ix, iy] - 2 * C1[ix, iy] * C2[ix, iy] * D3[ix, iy] + C2[ix, iy] * C2[ix, iy] * D1[ix, iy]) / (C1[ix, iy] * C1[ix, iy] + C2[ix, iy] * C2[ix, iy]);
          if abs(EISO) < 0.2 then
            EISO := 0;
        end else
          EISO := 0;

        EN := (AL * (EN1 + EN2 + EN3 + EN4)) + (BT * (EN5 + EN6));
        EA := (AL * (EA1 + EA2 + EA3 + EA4)) + (BT * (EA5 + EA6)) - (GM * sign(EISO));
        ES := (AL * (ES1 + ES2 + ES3 + ES4)) + (BT * (ES5 + ES6)) + (GM * sign(EISO));

        if (EN > EA) and (ES > EA) then begin
          if pixels[ix, iy].y + step > 255 then
            pixels[ix, iy].y := 255
          else
            pixels[ix, iy].y := pixels[ix, iy].y + step;

          maxValue := pixels[ix, iy - 1].y;
          testValue := pixels[ix - 1, iy].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix, iy + 1].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix + 1, iy].y;
          if testValue > maxValue then maxValue := testValue;
          if pixels[ix, iy].y > maxValue then
            pixels[ix, iy].y := maxValue;

          diff := diff + step;
        end else
          if (EN > ES) and (EA > ES) then begin
            if pixels[ix, iy].y <= step then
              pixels[ix, iy].y := 0
            else
              pixels[ix, iy].y := pixels[ix, iy].y - step;

            maxValue := pixels[ix, iy - 1].y;
            testValue := pixels[ix - 1, iy].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix, iy + 1].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix + 1, iy].y;
            if testValue < maxValue then maxValue := testValue;
            if pixels[ix, iy].y < maxValue then
              pixels[ix, iy].y := maxValue;

            diff := diff + step;
          end;

        inc(iy, 2);
      end;
    end;

    if diff < TS then
      break;
  end;

  for ix := 0 to newWidth - 1 do
    for iy := 0 to newHeight - 1 do
      dst[iy * newWidth + ix] := pixels[ix, iy];
end;

procedure GeometricPlane(src, dst: TPAYCbCr; width, height: integer);

  function CalcMedian(var median: array of single; count: integer) : single;

    procedure InternalQuickSort(l, r: integer);
    var i1, i2, i3 : integer;
        swap       : single;
    begin
      repeat
        i1 := l;
        i2 := r;
        i3 := (l + r) shr 1;
        repeat
          while true do begin
            if median[i1] >= median[i3] then
              break;
            inc(i1);
          end;
          while true do begin
            if median[i2] <= median[i3] then
              break;
            dec(i2);
          end;
          if i1 <= i2 then begin
            swap       := median[i1];
            median[i1] := median[i2];
            median[i2] := swap;
            if      i3 = i1 then i3 := i2
            else if i3 = i2 then i3 := i1;
            inc(i1);
            dec(i2);
          end;
        until i1 > i2;
        if l < i2 then InternalQuickSort(l, i2);
        l := i1;
      until i1 >= r;
    end;

  begin
    InternalQuickSort(0, count - 1);
    if odd(count) then
      result := median[count div 2]
    else
      result := (median[count div 2 - 1] + median[count div 2]) / 2;
  end;

const threshold = 8;
var pixels : array of array of TYCbCr;
    i1, i2 : integer;
    p00, p01, p10, p11 : single;
    meanY, meanCb, meanCr, dev1 : single;
    newWidth, newHeight : integer;
    ix, iy : integer;
    medianY, medianCb, medianCr : array [0..7] of single;
    mcY, mcCb, mcCr : integer;
begin
  newWidth  := width  * 2 - 1;
  newHeight := height * 2 - 1;

  // step 1: spread source pixels to destination raster

  SetLength(pixels, newWidth, newHeight);
  for ix := 0 to newWidth - 1 do
    for iy := 0 to newHeight - 1 do begin
      pixels[ix, iy].y  := -1;
      pixels[ix, iy].cb := -1;
      pixels[ix, iy].cr := -1;
    end;
  for ix := 0 to width - 1 do
    for iy := 0 to height - 1 do
      pixels[ix * 2, iy * 2] := src[iy * width + ix];

  // step 2: first interpolation based on geometrical shapes

  ix := 1;
  for i1 := 1 to width - 1 do begin
    iy := 1;
    for i2 := 1 to height - 1 do begin
      p00 := pixels[ix - 1, iy - 1].y;
      p01 := pixels[ix - 1, iy + 1].y;
      p10 := pixels[ix + 1, iy - 1].y;
      p11 := pixels[ix + 1, iy + 1].y;
      if max(max(max(p00, p01), p10), p11) - min(min(min(p00, p01), p10), p11) < threshold then begin
        // constant region
        meanY  := (p00 + p01 + p10 + p11) / 4;
        meanCb := (pixels[ix - 1, iy - 1].cb + pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb + pixels[ix + 1, iy + 1].cb) / 4;
        meanCr := (pixels[ix - 1, iy - 1].cr + pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr + pixels[ix + 1, iy + 1].cr) / 4;
        pixels[ix, iy].y  := meanY;
        pixels[ix, iy].cb := meanCb;
        pixels[ix, iy].cr := meanCr;
        if pixels[ix - 1, iy].y = -1 then begin
          pixels[ix - 1, iy].y  := meanY;
          pixels[ix - 1, iy].cb := meanCb;
          pixels[ix - 1, iy].cr := meanCr;
        end;
        if pixels[ix + 1, iy].y = -1 then begin
          pixels[ix + 1, iy].y  := meanY;
          pixels[ix + 1, iy].cb := meanCb;
          pixels[ix + 1, iy].cr := meanCr;
        end;
        if pixels[ix, iy - 1].y  = -1 then begin
          pixels[ix, iy - 1].y  := meanY;
          pixels[ix, iy - 1].cb := meanCb;
          pixels[ix, iy - 1].cr := meanCr;
        end;
        if pixels[ix, iy + 1].y = -1 then begin
          pixels[ix, iy + 1].y  := meanY;
          pixels[ix, iy + 1].cb := meanCb;
          pixels[ix, iy + 1].cr := meanCr;
        end;
      end else
        if (abs(p00 - p11) > threshold) and (abs(p00 - p11) > abs(p01 - p10)) then begin
          // top left corner
          meanY  := (p01 + p10 + p11) / 3;
          meanCb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb + pixels[ix + 1, iy + 1].cb) / 3;
          meanCr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr + pixels[ix + 1, iy + 1].cr) / 3;
          pixels[ix, iy].y  := (p01 + p10) / 2;
          pixels[ix, iy].cb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb) / 2;
          pixels[ix, iy].cr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr) / 2;
          dev1 := abs(sqrt(((meanY - p01) * (meanY - p01) + (meanY - p10) * (meanY - p10) + (meanY - p11) * (meanY - p11)) / 3));
          if dev1 * 2 < threshold then begin
            if pixels[ix, iy + 1].y = -1 then begin
              pixels[ix, iy + 1].y  := meanY;
              pixels[ix, iy + 1].cb := meanCb;
              pixels[ix, iy + 1].cr := meanCr;
            end;
            if pixels[ix + 1, iy].y = -1 then begin
              pixels[ix + 1, iy].y  := meanY;
              pixels[ix + 1, iy].cb := meanCb;
              pixels[ix + 1, iy].cr := meanCr;
            end;
          end;
        end else
          if (abs(p01 - p10) > threshold) and (abs(p01 - p10) > abs(p00 - p11)) then begin
            // top right corner
            meanY  := (p11 + p10 + p00) / 3;
            meanCb := (pixels[ix + 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb + pixels[ix - 1, iy - 1].cb) / 3;
            meanCr := (pixels[ix + 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr + pixels[ix - 1, iy - 1].cr) / 3;
            pixels[ix, iy].y  := (p00 + p11) / 2;
            pixels[ix, iy].cb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb) / 2;
            pixels[ix, iy].cr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr) / 2;
            dev1 := abs(sqrt(((meanY - p11) * (meanY - p11) + (meanY - p10) * (meanY - p10) + (meanY - p00) * (meanY - p00)) / 3));
            if dev1 * 2 < threshold then begin
              if pixels[ix, iy - 1].y = -1 then begin
                pixels[ix, iy - 1].y  := meanY;
                pixels[ix, iy - 1].cb := meanCb;
                pixels[ix, iy - 1].cr := meanCr;
              end;
              if pixels[ix + 1, iy].y = -1 then begin
                pixels[ix + 1, iy].y  := meanY;
                pixels[ix + 1, iy].cb := meanCb;
                pixels[ix + 1, iy].cr := meanCr;
              end;
            end;
          end else
            if abs(p00 - p11) > threshold then begin
              // bottom right corner
              meanY  := (p01 + p10 + p00) / 3;
              meanCb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb + pixels[ix - 1, iy - 1].cb) / 3;
              meanCr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr + pixels[ix - 1, iy - 1].cr) / 3;
//              pixels[ix, iy].y  := meanY;
//              pixels[ix, iy].cb := meanCb;
//              pixels[ix, iy].cr := meanCr;
              pixels[ix, iy].y  := (p01 + p10) / 2;
              pixels[ix, iy].cb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb) / 2;
              pixels[ix, iy].cr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr) / 2;
              dev1 := abs(sqrt(((meanY - p01) * (meanY - p01) + (meanY - p10) * (meanY - p10) + (meanY - p00) * (meanY - p00)) / 3));
              if dev1 * 2 < threshold then begin
                if pixels[ix, iy - 1].y = -1 then begin
                  pixels[ix, iy - 1].y  := meanY;
                  pixels[ix, iy - 1].cb := meanCb;
                  pixels[ix, iy - 1].cr := meanCr;
                end;
                if pixels[ix - 1, iy].y = -1 then begin
                  pixels[ix - 1, iy].y  := meanY;
                  pixels[ix - 1, iy].cb := meanCb;
                  pixels[ix - 1, iy].cr := meanCr;
                end;
              end;
            end else
              if abs(p01 - p10) > threshold then begin
                // bottom left corner
                meanY  := (p00 + p11 + p01) / 3;
                meanCb := (pixels[ix - 1, iy - 1].cb + pixels[ix + 1, iy + 1].cb + pixels[ix - 1, iy + 1].cb) / 3;
                meanCr := (pixels[ix - 1, iy - 1].cr + pixels[ix + 1, iy + 1].cr + pixels[ix - 1, iy + 1].cr) / 3;
//                pixels[ix, iy].y  := meanY;
//                pixels[ix, iy].cb := meanCb;
//                pixels[ix, iy].cr := meanCr;
                pixels[ix, iy].y  := (p00 + p11) / 2;
                pixels[ix, iy].cb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy - 1].cb) / 2;
                pixels[ix, iy].cr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy - 1].cr) / 2;
                dev1 := abs(sqrt(((meanY - p00) * (meanY - p00) + (meanY - p11) * (meanY - p11) + (meanY - p01) * (meanY - p01)) / 3));
                if dev1 * 2 < threshold then begin
                  if pixels[ix, iy + 1].y = -1 then begin
                    pixels[ix, iy + 1].y  := meanY;
                    pixels[ix, iy + 1].cb := meanCb;
                    pixels[ix, iy + 1].cr := meanCr;
                  end;
                  if pixels[ix - 1, iy].y = -1 then begin
                    pixels[ix - 1, iy].y  := meanY;
                    pixels[ix - 1, iy].cb := meanCb;
                    pixels[ix - 1, iy].cr := meanCr;
                  end;
                end;
              end else
                if (abs(p00 - p11) > threshold) and (abs(p01 - p10) > threshold) and ((p00 - p11) * (p01 - p10) > 0) then begin
                  // horizontal half square
                  pixels[ix, iy].y  := (p00 + p01) / 2;
                  pixels[ix, iy].cb := (pixels[ix - 1, iy - 1].cb + pixels[ix - 1, iy + 1].cb) / 2;
                  pixels[ix, iy].cr := (pixels[ix - 1, iy - 1].cr + pixels[ix - 1, iy + 1].cr) / 2;
                  if pixels[ix - 1, iy].y = -1 then begin
                    pixels[ix - 1, iy].y  := (p00 + p01) / 2;
                    pixels[ix - 1, iy].cb := (pixels[ix - 1, iy - 1].cb + pixels[ix - 1, iy + 1].cb) / 2;
                    pixels[ix - 1, iy].cr := (pixels[ix - 1, iy - 1].cr + pixels[ix - 1, iy + 1].cr) / 2;
                  end;
                  if pixels[ix, iy - 1].y = -1 then begin
                    pixels[ix, iy - 1].y  := p00;
                    pixels[ix, iy - 1].cb := pixels[ix - 1, iy - 1].cb;
                    pixels[ix, iy - 1].cr := pixels[ix - 1, iy - 1].cr;
                  end;
                  if pixels[ix, iy + 1].y = -1 then begin
                    pixels[ix, iy + 1].y  := p01;
                    pixels[ix, iy + 1].cb := pixels[ix - 1, iy + 1].cb;
                    pixels[ix, iy + 1].cr := pixels[ix - 1, iy + 1].cr;
                  end;
                  if pixels[ix + 1, iy].y = -1 then begin
                    pixels[ix + 1, iy].y  := (p10 + p11) / 2;
                    pixels[ix + 1, iy].cb := (pixels[ix + 1, iy - 1].cb + pixels[ix + 1, iy + 1].cb) / 2;
                    pixels[ix + 1, iy].cr := (pixels[ix + 1, iy - 1].cr + pixels[ix + 1, iy + 1].cr) / 2;
                  end;
                end else
                  if (abs(p00 - p11) > threshold) and (abs(p01 - p10) > threshold) and ((p00 - p11) * (p01 - p10) < 0) then begin
                    // vertical half square
                    pixels[ix, iy].y  := (p00 + p10) / 2;
                    pixels[ix, iy].cb := (pixels[ix - 1, iy - 1].cb + pixels[ix + 1, iy - 1].cb) / 2;
                    pixels[ix, iy].cr := (pixels[ix - 1, iy - 1].cr + pixels[ix + 1, iy - 1].cr) / 2;
                    if pixels[ix, iy - 1].y = -1 then begin
                      pixels[ix, iy - 1].y  := (p00 + p10) / 2;
                      pixels[ix, iy - 1].cb := (pixels[ix - 1, iy - 1].cb + pixels[ix + 1, iy - 1].cb) / 2;
                      pixels[ix, iy - 1].cr := (pixels[ix - 1, iy - 1].cr + pixels[ix + 1, iy - 1].cr) / 2;
                    end;
                    if pixels[ix - 1, iy].y = -1 then begin
                      pixels[ix - 1, iy].y  := p00;
                      pixels[ix - 1, iy].cb := pixels[ix - 1, iy - 1].cb;
                      pixels[ix - 1, iy].cr := pixels[ix - 1, iy - 1].cr;
                    end;
                    if pixels[ix + 1, iy].y = -1 then begin
                      pixels[ix + 1, iy].y  := p10;
                      pixels[ix + 1, iy].cb := pixels[ix + 1, iy - 1].cb;
                      pixels[ix + 1, iy].cr := pixels[ix + 1, iy - 1].cr;
                    end;
                    if pixels[ix, iy + 1].y = -1 then begin
                      pixels[ix, iy + 1].y  := (p01 + p11) / 2;
                      pixels[ix, iy + 1].cb := (pixels[ix - 1, iy + 1].cb + pixels[ix + 1, iy + 1].cb) / 2;
                      pixels[ix, iy + 1].cr := (pixels[ix - 1, iy + 1].cr + pixels[ix + 1, iy + 1].cr) / 2;
                    end;
                  end;
      inc(iy, 2);
    end;
    inc(ix, 2);
  end;

  // step 3: 2nd phase interpolation

  for ix := 1 to newWidth - 2 do
    for iy := 1 to newHeight - 2 do
      if (pixels[ix, iy].y = -1) and
         (pixels[ix - 1, iy].y <> -1) and (pixels[ix + 1, iy].y <> -1) and (pixels[ix, iy - 1].y <> -1) and (pixels[ix, iy + 1].y <> -1) and
         (abs( max(max(max(pixels[ix - 1, iy].y, pixels[ix + 1, iy].y), pixels[ix, iy - 1].y), pixels[ix, iy + 1].y) -
               min(min(min(pixels[ix - 1, iy].y, pixels[ix + 1, iy].y), pixels[ix, iy - 1].y), pixels[ix, iy + 1].y)   ) < 14) then begin
        pixels[ix, iy].y  := (pixels[ix - 1, iy].y  + pixels[ix + 1, iy].y  + pixels[ix, iy - 1].y  + pixels[ix, iy + 1].y ) / 4;
        pixels[ix, iy].cb := (pixels[ix - 1, iy].cb + pixels[ix + 1, iy].cb + pixels[ix, iy - 1].cb + pixels[ix, iy + 1].cb) / 4;
        pixels[ix, iy].cr := (pixels[ix - 1, iy].cr + pixels[ix + 1, iy].cr + pixels[ix, iy - 1].cr + pixels[ix, iy + 1].cr) / 4;
      end;

  for ix := 1 to newWidth - 2 do
    for iy := 1 to newHeight - 2 do
      if pixels[ix, iy].y = -1 then
        if (pixels[ix - 1, iy].y <> -1) and (pixels[ix + 1, iy].y <> -1) and (pixels[ix, iy - 1].y = -1) and (pixels[ix, iy + 1].y = -1) then begin
          if abs(pixels[ix - 1, iy].y - pixels[ix + 1, iy].y) < 14 then begin
            pixels[ix, iy].y  := (pixels[ix - 1, iy].y  + pixels[ix + 1, iy].y ) / 2;
            pixels[ix, iy].cb := (pixels[ix - 1, iy].cb + pixels[ix + 1, iy].cb) / 2;
            pixels[ix, iy].cr := (pixels[ix - 1, iy].cr + pixels[ix + 1, iy].cr) / 2;
          end;
        end else
          if (pixels[ix, iy - 1].y <> -1) and (pixels[ix, iy + 1].y <> -1) and (pixels[ix - 1, iy].y = -1) and (pixels[ix + 1, iy].y = -1) and
             (abs(pixels[ix, iy - 1].y - pixels[ix, iy + 1].y) < 14) then begin
            pixels[ix, iy].y  := (pixels[ix, iy - 1].y  + pixels[ix, iy + 1].y ) / 2;
            pixels[ix, iy].cb := (pixels[ix, iy - 1].cb + pixels[ix, iy + 1].cb) / 2;
            pixels[ix, iy].cr := (pixels[ix, iy - 1].cr + pixels[ix, iy + 1].cr) / 2;
          end else
            if (pixels[ix, iy - 1].y <> -1) and (pixels[ix, iy + 1].y <> -1) and (pixels[ix - 1, iy].y <> -1) and (pixels[ix + 1, iy].y <> -1) then
              if (abs(pixels[ix, iy - 1].y - pixels[ix, iy + 1].y) > 4) and
                 (abs(pixels[ix, iy - 1].y - pixels[ix, iy + 1].y) > abs(pixels[ix - 1, iy].y - pixels[ix + 1, iy].y)) then begin
                pixels[ix, iy].y  := (pixels[ix + 1, iy].y  + pixels[ix - 1, iy].y ) / 2;
                pixels[ix, iy].cb := (pixels[ix + 1, iy].cb + pixels[ix - 1, iy].cb) / 2;
                pixels[ix, iy].cr := (pixels[ix + 1, iy].cr + pixels[ix - 1, iy].cr) / 2;
              end else
                if (abs(pixels[ix + 1, iy].y - pixels[ix - 1, iy].y) > 4) and
                   (abs(pixels[ix + 1, iy].y - pixels[ix - 1, iy].y) > abs(pixels[ix, iy + 1].y - pixels[ix, iy - 1].y)) then begin
                  pixels[ix, iy].y  := (pixels[ix, iy + 1].y  + pixels[ix, iy - 1].y ) / 2;
                  pixels[ix, iy].cb := (pixels[ix, iy + 1].cb + pixels[ix, iy - 1].cb) / 2;
                  pixels[ix, iy].cr := (pixels[ix, iy + 1].cr + pixels[ix, iy - 1].cr) / 2;
                end;

  // step 4: fill remaining pixels with median of surrounding pixels

  for ix := 0 to newWidth - 1 do begin
    for iy := 0 to newHeight - 1 do begin
      if pixels[ix, iy].y < 0 then begin
        mcY  := 0;
        mcCb := 0;
        mcCr := 0;
        if ix > 0 then begin
          medianY[mcY] := pixels[ix - 1, iy].y;
          if medianY[mcY] >= 0 then inc(mcY);
          medianCb[mcCb] := pixels[ix - 1, iy].cb;
          if medianCb[mcCb] >= 0 then inc(mcCb);
          medianCr[mcCr] := pixels[ix - 1, iy].cr;
          if medianCr[mcCr] >= 0 then inc(mcCr);
        end;
        if ix < newWidth - 1 then begin
          medianY[mcY] := pixels[ix + 1, iy].y;
          if medianY[mcY] >= 0 then inc(mcY);
          medianCb[mcCb] := pixels[ix + 1, iy].cb;
          if medianCb[mcCb] >= 0 then inc(mcCb);
          medianCr[mcCr] := pixels[ix + 1, iy].cr;
          if medianCr[mcCr] >= 0 then inc(mcCr);
        end;
        if iy > 0 then begin
          medianY[mcY] := pixels[ix, iy - 1].y;
          if medianY[mcY] >= 0 then inc(mcY);
          medianCb[mcCb] := pixels[ix, iy - 1].cb;
          if medianCb[mcCb] >= 0 then inc(mcCb);
          medianCr[mcCr] := pixels[ix, iy - 1].cr;
          if medianCr[mcCr] >= 0 then inc(mcCr);
        end;
        if iy < newHeight - 1 then begin
          medianY[mcY] := pixels[ix, iy + 1].y;
          if medianY[mcY] >= 0 then inc(mcY);
          medianCb[mcCb] := pixels[ix, iy + 1].cb;
          if medianCb[mcCb] >= 0 then inc(mcCb);
          medianCr[mcCr] := pixels[ix, iy + 1].cr;
          if medianCr[mcCr] >= 0 then inc(mcCr);
        end;
        if mcY < 3 then begin
          if (ix > 0) and (iy > 0) then begin
            medianY[mcY] := pixels[ix - 1, iy - 1].y;
            if medianY[mcY] >= 0 then inc(mcY);
          end;
          if (ix > 0) and (iy < newHeight - 1) then begin
            medianY[mcY] := pixels[ix - 1, iy + 1].y;
            if medianY[mcY] >= 0 then inc(mcY);
          end;
          if (ix < newWidth - 1) and (iy > 0) then begin
            medianY[mcY] := pixels[ix + 1, iy - 1].y;
            if medianY[mcY] >= 0 then inc(mcY);
          end;
          if (ix < newWidth - 1) and (iy < newHeight - 1) then begin
            medianY[mcY] := pixels[ix + 1, iy + 1].y;
            if medianY[mcY] >= 0 then inc(mcY);
          end;
        end;
        if mcCb < 3 then begin
          if (ix > 0) and (iy > 0) then begin
            medianCb[mcCb] := pixels[ix - 1, iy - 1].cb;
            if medianCb[mcCb] >= 0 then inc(mcCb);
          end;
          if (ix > 0) and (iy < newHeight - 1) then begin
            medianCb[mcCb] := pixels[ix - 1, iy + 1].cb;
            if medianCb[mcCb] >= 0 then inc(mcCb);
          end;
          if (ix < newWidth - 1) and (iy > 0) then begin
            medianCb[mcCb] := pixels[ix + 1, iy - 1].cb;
            if medianCb[mcCb] >= 0 then inc(mcCb);
          end;
          if (ix < newWidth - 1) and (iy < newHeight - 1) then begin
            medianCb[mcCb] := pixels[ix + 1, iy + 1].cb;
            if medianCb[mcCb] >= 0 then inc(mcCb);
          end;
        end;
        if mcCr < 3 then begin
          if (ix > 0) and (iy > 0) then begin
            medianCr[mcCr] := pixels[ix - 1, iy - 1].cr;
            if medianCr[mcCr] >= 0 then inc(mcCr);
          end;
          if (ix > 0) and (iy < newHeight - 1) then begin
            medianCr[mcCr] := pixels[ix - 1, iy + 1].cr;
            if medianCr[mcCr] >= 0 then inc(mcCr);
          end;
          if (ix < newWidth - 1) and (iy > 0) then begin
            medianCr[mcCr] := pixels[ix + 1, iy - 1].cr;
            if medianCr[mcCr] >= 0 then inc(mcCr);
          end;
          if (ix < newWidth - 1) and (iy < newHeight - 1) then begin
            medianCr[mcCr] := pixels[ix + 1, iy + 1].cr;
            if medianCr[mcCr] >= 0 then inc(mcCr);
          end;
        end;
        if mcY > 0 then
          pixels[ix, iy].y := CalcMedian(medianY, mcY);
        if mcCb > 0 then
          pixels[ix, iy].cb := CalcMedian(medianCb, mcCb);
        if mcCr > 0 then
          pixels[ix, iy].cr := CalcMedian(medianCr, mcCr);
      end;
    end;
  end;

  for ix := 0 to newWidth - 1 do
    for iy := 0 to newHeight - 1 do
      dst[iy * newWidth + ix] := pixels[ix, iy];
end;

procedure FuzzyElaPlane(src, dst: TPAYCbCr; width, height: integer);

  function verySmall(value: single) : single;
  begin
    result := 1.0 - value / 8.0;
    if result < 0.0 then
      result := 0.0;
  end;

  function small(value: single) : single;
  begin
    result := 1.0 - value / 16.0;
    if result < 0.0 then
      result := 0.0;
  end;

  function large(value: single) : single;
  begin
    result := value / 64.0;
    if result > 1.0 then
      result := 1.0;
  end;

var pixels : array of array of TYCbCr;
    i1, i2 : integer;
    pa, pb, pc, pd, pe, pf : single;
    paf, pbe, pcd : single;
    p1, p2, p3, p4 : single;
    newWidth, newHeight : integer;
    ix, iy : integer;
begin
  newWidth  := width  * 2 - 1;
  newHeight := height * 2 - 1;

  // step 1: spread source pixels to destination raster

  SetLength(pixels, newWidth, newHeight);
  for ix := 0 to newWidth - 1 do
    for iy := 0 to newHeight - 1 do begin
      pixels[ix, iy].y  := -1;
      pixels[ix, iy].cb := -1;
      pixels[ix, iy].cr := -1;
    end;
  for ix := 0 to width - 1 do
    for iy := 0 to height - 1 do
      pixels[ix * 2, iy * 2] := src[iy * width + ix];
  for iy := 1 to newHeight - 2 do
    if odd(iy) then begin
      pixels[0, iy].y  := (pixels[0, iy - 1].y  + pixels[0, iy + 1].y ) / 2.0;
      pixels[0, iy].cb := (pixels[0, iy - 1].cb + pixels[0, iy + 1].cb) / 2.0;
      pixels[0, iy].cr := (pixels[0, iy - 1].cr + pixels[0, iy + 1].cr) / 2.0;
      pixels[newWidth - 1, iy].y  := (pixels[newWidth - 1, iy - 1].y  + pixels[newWidth - 1, iy + 1].y ) / 2.0;
      pixels[newWidth - 1, iy].cb := (pixels[newWidth - 1, iy - 1].cb + pixels[newWidth - 1, iy + 1].cb) / 2.0;
      pixels[newWidth - 1, iy].cr := (pixels[newWidth - 1, iy - 1].cr + pixels[newWidth - 1, iy + 1].cr) / 2.0;
    end;
  for ix := 1 to newWidth - 2 do
    if odd(ix) then begin
      pixels[ix, 0].y  := (pixels[ix - 1, 0].y  + pixels[ix + 1, 0].y ) / 2.0;
      pixels[ix, 0].cb := (pixels[ix - 1, 0].cb + pixels[ix + 1, 0].cb) / 2.0;
      pixels[ix, 0].cr := (pixels[ix - 1, 0].cr + pixels[ix + 1, 0].cr) / 2.0;
      pixels[ix, newHeight - 1].y  := (pixels[ix - 1, newHeight - 1].y  + pixels[ix + 1, newHeight - 1].y ) / 2.0;
      pixels[ix, newHeight - 1].cb := (pixels[ix - 1, newHeight - 1].cb + pixels[ix + 1, newHeight - 1].cb) / 2.0;
      pixels[ix, newHeight - 1].cr := (pixels[ix - 1, newHeight - 1].cr + pixels[ix + 1, newHeight - 1].cr) / 2.0;
    end;

  // step 2: first interpolation

  ix := 2;
  for i1 := ix to width - 1 do begin
    iy := 1;
    for i2 := iy to height - 1 do begin
      pa := pixels[ix - 2, iy - 1].y;
      pb := pixels[ix,     iy - 1].y;
      pc := pixels[ix + 2, iy - 1].y;
      pd := pixels[ix - 2, iy + 1].y;
      pe := pixels[ix,     iy + 1].y;
      pf := pixels[ix + 2, iy + 1].y;
      paf := abs(pa - pf);
      pbe := abs(pb - pe);
      pcd := abs(pc - pd);
      p1 := min(min(small(paf), large(pbe)), large(pcd));
      p2 := min(min(large(paf), large(pbe)), small(pcd));
      p3 := min(min(verySmall(paf), large(pbe)), verySmall(pcd));
      p4 := 1 - p1 - p2 - p3;
      pixels[ix, iy].y := p1 * (pa + pf                   ) / 2.0 +
                          p2 * (          pc + pd         ) / 2.0 +
                          p3 * (pa + pf + pc + pd         ) / 4.0 +
                          p4 * (                   pb + pe) / 2.0;
      pa := pixels[ix - 2, iy - 1].cb;
      pb := pixels[ix,     iy - 1].cb;
      pc := pixels[ix + 2, iy - 1].cb;
      pd := pixels[ix - 2, iy + 1].cb;
      pe := pixels[ix,     iy + 1].cb;
      pf := pixels[ix + 2, iy + 1].cb;
      paf := abs(pa - pf);
      pbe := abs(pb - pe);
      pcd := abs(pc - pd);
      p1 := min(min(small(paf), large(pbe)), large(pcd));
      p2 := min(min(large(paf), large(pbe)), small(pcd));
      p3 := min(min(verySmall(paf), large(pbe)), verySmall(pcd));
      p4 := 1 - p1 - p2 - p3;
      pixels[ix, iy].cb := p1 * (pa + pf                   ) / 2.0 +
                           p2 * (          pc + pd         ) / 2.0 +
                           p3 * (pa + pf + pc + pd         ) / 4.0 +
                           p4 * (                   pb + pe) / 2.0;
      pa := pixels[ix - 2, iy - 1].cr;
      pb := pixels[ix,     iy - 1].cr;
      pc := pixels[ix + 2, iy - 1].cr;
      pd := pixels[ix - 2, iy + 1].cr;
      pe := pixels[ix,     iy + 1].cr;
      pf := pixels[ix + 2, iy + 1].cr;
      paf := abs(pa - pf);
      pbe := abs(pb - pe);
      pcd := abs(pc - pd);
      p1 := min(min(small(paf), large(pbe)), large(pcd));
      p2 := min(min(large(paf), large(pbe)), small(pcd));
      p3 := min(min(verySmall(paf), large(pbe)), verySmall(pcd));
      p4 := 1 - p1 - p2 - p3;
      pixels[ix, iy].cr := p1 * (pa + pf                   ) / 2.0 +
                           p2 * (          pc + pd         ) / 2.0 +
                           p3 * (pa + pf + pc + pd         ) / 4.0 +
                           p4 * (                   pb + pe) / 2.0;
      inc(iy, 2);
    end;
    inc(ix, 2);
  end;

  // step 3: 2nd phase interpolation

  ix := 1;
  for i1 := ix to width - 1 do begin
    for iy := 1 to newHeight - 2 do begin
      pa := pixels[ix - 1, iy + 1].y;
      pb := pixels[ix - 1, iy    ].y;
      pc := pixels[ix - 1, iy - 1].y;
      pd := pixels[ix + 1, iy + 1].y;
      pe := pixels[ix + 1, iy    ].y;
      pf := pixels[ix + 1, iy - 1].y;
      paf := abs(pa - pf);
      pbe := abs(pb - pe);
      pcd := abs(pc - pd);
      p1 := min(min(small(paf), large(pbe)), large(pcd));
      p2 := min(min(large(paf), large(pbe)), small(pcd));
      p3 := min(min(verySmall(paf), large(pbe)), verySmall(pcd));
      p4 := 1 - p1 - p2 - p3;
      pixels[ix, iy].y := p1 * (pa + pf                   ) / 2.0 +
                          p2 * (          pc + pd         ) / 2.0 +
                          p3 * (pa + pf + pc + pd         ) / 4.0 +
                          p4 * (                   pb + pe) / 2.0;
      pa := pixels[ix - 1, iy + 1].cb;
      pb := pixels[ix - 1, iy    ].cb;
      pc := pixels[ix - 1, iy - 1].cb;
      pd := pixels[ix + 1, iy + 1].cb;
      pe := pixels[ix + 1, iy    ].cb;
      pf := pixels[ix + 1, iy - 1].cb;
      paf := abs(pa - pf);
      pbe := abs(pb - pe);
      pcd := abs(pc - pd);
      p1 := min(min(small(paf), large(pbe)), large(pcd));
      p2 := min(min(large(paf), large(pbe)), small(pcd));
      p3 := min(min(verySmall(paf), large(pbe)), verySmall(pcd));
      p4 := 1 - p1 - p2 - p3;
      pixels[ix, iy].cb := p1 * (pa + pf                   ) / 2.0 +
                           p2 * (          pc + pd         ) / 2.0 +
                           p3 * (pa + pf + pc + pd         ) / 4.0 +
                           p4 * (                   pb + pe) / 2.0;
      pa := pixels[ix - 1, iy + 1].cr;
      pb := pixels[ix - 1, iy    ].cr;
      pc := pixels[ix - 1, iy - 1].cr;
      pd := pixels[ix + 1, iy + 1].cr;
      pe := pixels[ix + 1, iy    ].cr;
      pf := pixels[ix + 1, iy - 1].cr;
      paf := abs(pa - pf);
      pbe := abs(pb - pe);
      pcd := abs(pc - pd);
      p1 := min(min(small(paf), large(pbe)), large(pcd));
      p2 := min(min(large(paf), large(pbe)), small(pcd));
      p3 := min(min(verySmall(paf), large(pbe)), verySmall(pcd));
      p4 := 1 - p1 - p2 - p3;
      pixels[ix, iy].cr := p1 * (pa + pf                   ) / 2.0 +
                           p2 * (          pc + pd         ) / 2.0 +
                           p3 * (pa + pf + pc + pd         ) / 4.0 +
                           p4 * (                   pb + pe) / 2.0;
    end;
    inc(ix, 2);
  end;

  for ix := 0 to newWidth - 1 do
    for iy := 0 to newHeight - 1 do
      dst[iy * newWidth + ix] := pixels[ix, iy];
end;

procedure PRPlane(src, dst: TPAYCbCr; width, height: integer; const icbiStepSizes: array of integer; sharpenIterations: integer);

  type
    TMatrix2 = record case boolean of
                 false : (arr: array [0..1] of array [0..1] of double);
                 true  : (flt: array [0..3] of double);
               end;

  type
    TA9Int = array [0..8] of double;
    TA9x2Int = array [0..8] of array [0..1] of double;

  function solve(var p: TA9Int; var n: TA9x2Int; size: integer; var pixel: single) : boolean;
  var i, j, k, final : integer;
      a, r : array [0..1] of double;
      CtC, CtCi : TMatrix2;
      s : double;
      det : double;
      min, max : double;
  begin
    min := 255;
    max := 0;

    // apply weights
    for i := 0 to size - 1 do begin
      if p[i] > max then
        max := p[i];
      if p[i] < min then
        min := p[i];
      p[i] := p[i] * n[i, 1];
      n[i, 0] := n[i, 0] * n[i, 1];
    end;

    // compute covariance matrix CtC := Ct * C (2x9 * 9x2 = 2x2)
    for i := 0 to 1 do
      for j := i to 1 do begin
        s := 0;
        for k := 0 to size - 1 do
          s := s + n[k][i] * n[k][j];
        CtC.arr[i][j] := s;
        CtC.arr[j][i] := s;
      end;

    det := CtC.arr[0, 0] * CtC.arr[1, 1] - CtC.arr[0, 1] * CtC.arr[1, 0];
    if det > 1.0 then begin
      // determinate is big enough

      // invert matrix
      det := 1 / det;
      CtCi.arr[0, 0] := + CtC.arr[1, 1] * det;
      CtCi.arr[0, 1] := - CtC.arr[0, 1] * det;
      CtCi.arr[1, 0] := - CtC.arr[1, 0] * det;
      CtCi.arr[1, 1] := + CtC.arr[0, 0] * det;

      // compute matrix r[] := Ct * p (2x9 * 9x1 = 2x1)
      for i := 0 to 1 do begin
        s := 0;
        for k := 0 to size - 1 do
          s := s + n[k][i] * p[k];
        r[i] := s;
      end;

      // compute weights (CtCi[] * r[]) (2x2 * 2x1 = 2x1)
      for i := 0 to 1 do begin
        s := 0;
        for k := 0 to 1 do
          s := s + CtCi.arr[i][k] * r[k];
        a[i] := s;
      end;

    end else begin
      // determinate is dangerously small (weird results)
      // so we set alpha to 1 and calculate beta, only
      
      a[0] := 1;
      a[1] := 0;
      for i := 0 to size - 1 do
        a[1] := a[1] + p[i] - n[i, 0];
    end;

    // calculate interpolated pixel
    pixel := a[0] * pixel + a[1];
    if pixel > max then
      pixel := max
    else
      if pixel < min then
        pixel := min;
    final := round(pixel);
    if final > 255 then
      final := 255
    else
      if final < 0 then
        final := 0;

    pixel := final;
    result := true;
  end;

  procedure CalcWeights(var weights: TA9Int; size: integer);
  var i1      : integer;
      biggest : double;
      sum     : double;
  begin
    biggest := 0;
    sum := 0;
    for i1 := 0 to size - 1 do begin
      weights[i1] := weights[i1] / 2;
      if weights[i1] < 1 then
        weights[i1] := 3 * weights[i1] * weights[i1] * weights[i1] - 6 * weights[i1] * weights[i1] + 4
      else
        weights[i1] := - weights[i1] * weights[i1] * weights[i1] + 6 * weights[i1] * weights[i1] - 12 * weights[i1] + 8;
      sum := sum + weights[i1];
      if weights[i1] > biggest then
        biggest := weights[i1];
    end;
    for i1 := 0 to size - 1 do
      weights[i1] := weights[i1] / sum;
  end;

var pixels : array of array of TYCbCr;
    i1, i2, i4 : integer;
    p00, p02, p20, p22 : double;
    p10, p01, p12, p21 : double;
    w0022, w0220 : double;
    w1012, w0121 : double;
    newWidth, newHeight : integer;
    ix, iy : integer;

const
  GM = 5.0;  // Weight for Isophote smoothing energy (default = 5.0 -> 19.0)
  BT = -1.0;  // Weight for Curvature enhancement energy (default = -1.0 -> -3.0)
  AL = 1.0;   // Weight for Curvature Continuity energy (default = 1.0 -> 0.7)
  TS = 100;   // Threshold on image change for stopping iterations (default = 100)

var D1, D2, D3, C1, C2 : array of array of single;
    i3 : integer;
    step : integer;
    g : integer;
    diff : integer;
    EN1, EN2, EN3, EN4, EN5, EN6 : single;
    EA1, EA2, EA3, EA4, EA5, EA6 : single;
    ES1, ES2, ES3, ES4, ES5, ES6 : single;
    EN, EA, ES : single;
    EISO : double;
    maxValue, testValue : single;
    orgPixels : TA9Int;
    newPixels : TA9x2Int;
    weights : TA9Int;
begin
  newWidth  := width  * 2 - 1;
  newHeight := height * 2 - 1;

  // step 1: spread source pixels to destination raster
  SetLength(pixels, newWidth, newHeight);
  for ix := 0 to width - 1 do
    for iy := 0 to height - 1 do
      pixels[ix * 2, iy * 2] := src[iy * width + ix];

  ix := 1;
  iy := newHeight - 1;
  for i1 := 1 to width - 1 do begin
    pixels[ix, 0 ].y  := (pixels[ix + 1, 0 ].y  + pixels[ix - 1, 0 ].y ) / 2;
    pixels[ix, 0 ].cb := (pixels[ix + 1, 0 ].cb + pixels[ix - 1, 0 ].cb) / 2;
    pixels[ix, 0 ].cr := (pixels[ix + 1, 0 ].cr + pixels[ix - 1, 0 ].cr) / 2;
    pixels[ix, iy].y  := (pixels[ix + 1, iy].y  + pixels[ix - 1, iy].y ) / 2;
    pixels[ix, iy].cb := (pixels[ix + 1, iy].cb + pixels[ix - 1, iy].cb) / 2;
    pixels[ix, iy].cr := (pixels[ix + 1, iy].cr + pixels[ix - 1, iy].cr) / 2;
    inc(ix, 2);
  end;
  ix := newWidth - 1;
  iy := 1;
  for i1 := 1 to height - 1 do begin
    pixels[0,  iy].y  := (pixels[0,  iy + 1].y  + pixels[0,  iy - 1].y ) / 2;
    pixels[0,  iy].cb := (pixels[0,  iy + 1].cb + pixels[0,  iy - 1].cb) / 2;
    pixels[0,  iy].cr := (pixels[0,  iy + 1].cr + pixels[0,  iy - 1].cr) / 2;
    pixels[ix, iy].y  := (pixels[ix, iy + 1].y  + pixels[ix, iy - 1].y ) / 2;
    pixels[ix, iy].cb := (pixels[ix, iy + 1].cb + pixels[ix, iy - 1].cb) / 2;
    pixels[ix, iy].cr := (pixels[ix, iy + 1].cr + pixels[ix, iy - 1].cr) / 2;
    inc(iy, 2);
  end;

  // step 2: interpolate +1+1 pixels
  ix := 1;
  for i1 := 1 to width - 1 do begin
    iy := 1;
    for i2 := 1 to height - 1 do begin
      p00 := pixels[ix - 1, iy - 1].y;
      p02 := pixels[ix - 1, iy + 1].y;
      p20 := pixels[ix + 1, iy - 1].y;
      p22 := pixels[ix + 1, iy + 1].y;
      w0022 := (p00 - p22) / 255;
      w0022 := exp(- w0022 * w0022);
      w0220 := (p02 - p20) / 255;
      w0220 := exp(- w0220 * w0220);
      if (w0022 <> 0) or (w0220 <> 0) then begin
        pixels[ix, iy].y  := (w0022 * p00 + w0022 * p22 + w0220 * p02 + w0220 * p20) / (w0022 * 2 + w0220 * 2);
        pixels[ix, iy].cb := (w0022 * pixels[ix - 1, iy - 1].cb + w0022 * pixels[ix + 1, iy + 1].cb + w0220 * pixels[ix - 1, iy + 1].cb + w0220 * pixels[ix + 1, iy - 1].cb) / (w0022 * 2 + w0220 * 2);
        pixels[ix, iy].cr := (w0022 * pixels[ix - 1, iy - 1].cr + w0022 * pixels[ix + 1, iy + 1].cr + w0220 * pixels[ix - 1, iy + 1].cr + w0220 * pixels[ix + 1, iy - 1].cr) / (w0022 * 2 + w0220 * 2);
      end else begin
        pixels[ix, iy].y  := p00;
        pixels[ix, iy].cb := pixels[ix - 1, iy - 1].cb;
        pixels[ix, iy].cr := pixels[ix - 1, iy - 1].cr;
      end;
      inc(iy, 2);
    end;
    inc(ix, 2);
  end;

  // step 3: interpolate +1+0 and +0+1 pixels
  for ix := 1 to newWidth - 2 do begin
    iy := 1 + (ix and 1);
    for i2 := 1 to height - 1 do begin
      p10 := pixels[ix, iy - 1].y;
      p01 := pixels[ix - 1, iy].y;
      p21 := pixels[ix + 1, iy].y;
      p12 := pixels[ix, iy + 1].y;
      w1012 := (p10 - p12) / 255;
      w1012 := exp(- w1012 * w1012);
      w0121 := (p01 - p21) / 255;
      w0121 := exp(- w0121 * w0121);
      if (w1012 <> 0) or (w0121 <> 0) then begin
        pixels[ix, iy].y  := (w1012 * p10 + w1012 * p12 + w0121 * p01 + w0121 * p21) / (w1012 * 2 + w0121 * 2);
        pixels[ix, iy].cb := (w1012 * pixels[ix, iy - 1].cb + w1012 * pixels[ix, iy + 1].cb + w0121 * pixels[ix - 1, iy].cb + w0121 * pixels[ix + 1, iy].cb) / (w1012 * 2 + w0121 * 2);
        pixels[ix, iy].cr := (w1012 * pixels[ix, iy - 1].cr + w1012 * pixels[ix, iy + 1].cr + w0121 * pixels[ix - 1, iy].cr + w0121 * pixels[ix + 1, iy].cr) / (w1012 * 2 + w0121 * 2);
      end else begin
        pixels[ix, iy].y  := p10;
        pixels[ix, iy].cb := pixels[ix, iy - 1].cb;
        pixels[ix, iy].cr := pixels[ix, iy - 1].cr;
      end;
      inc(iy, 2);
    end;
  end;

  // step 4: reinterpolate +0+0 pixels
  ix := 2;
  for i1 := 2 to width - 2 do begin
    iy := 2;
    for i2 := 2 to height - 2 do begin
      p00 := pixels[ix - 1, iy - 1].y;
      p02 := pixels[ix - 1, iy + 1].y;
      p20 := pixels[ix + 1, iy - 1].y;
      p22 := pixels[ix + 1, iy + 1].y;
      w0022 := (p00 - p22) / 255;
      w0022 := exp(- w0022 * w0022);
      w0220 := (p02 - p20) / 255;
      w0220 := exp(- w0220 * w0220);
      p10 := pixels[ix, iy - 1].y;
      p01 := pixels[ix - 1, iy].y;
      p21 := pixels[ix + 1, iy].y;
      p12 := pixels[ix, iy + 1].y;
      w1012 := (p10 - p12) / 255;
      w1012 := exp(- w1012 * w1012);
      w0121 := (p01 - p21) / 255;
      w0121 := exp(- w0121 * w0121);
      if (w0022 <> 0) or (w0220 <> 0) or (w1012 <> 0) or (w0121 <> 0) then begin
        pixels[ix, iy].y  := (w0022 * p00 + w0022 * p22 + w0220 * p02 + w0220 * p20 + w1012 * p10 + w1012 * p12 + w0121 * p01 + w0121 * p21) / (w0022 * 2 + w0220 * 2 + w1012 * 2 + w0121 * 2);
        pixels[ix, iy].cb := (w0022 * pixels[ix - 1, iy - 1].cb + w0022 * pixels[ix + 1, iy + 1].cb + w0220 * pixels[ix - 1, iy + 1].cb + w0220 * pixels[ix + 1, iy - 1].cb + w1012 * pixels[ix, iy - 1].cb + w1012 * pixels[ix, iy + 1].cb + w0121 * pixels[ix - 1, iy].cb + w0121 * pixels[ix + 1, iy].cb) / (w0022 * 2 + w0220 * 2 + w1012 * 2 + w0121 * 2);
        pixels[ix, iy].cr := (w0022 * pixels[ix - 1, iy - 1].cr + w0022 * pixels[ix + 1, iy + 1].cr + w0220 * pixels[ix - 1, iy + 1].cr + w0220 * pixels[ix + 1, iy - 1].cr + w1012 * pixels[ix, iy - 1].cr + w1012 * pixels[ix, iy + 1].cr + w0121 * pixels[ix - 1, iy].cr + w0121 * pixels[ix + 1, iy].cr) / (w0022 * 2 + w0220 * 2 + w1012 * 2 + w0121 * 2);
      end else begin
        pixels[ix, iy].y  := p00;
        pixels[ix, iy].cb := pixels[ix - 1, iy - 1].cb;
        pixels[ix, iy].cr := pixels[ix - 1, iy - 1].cr;
      end;
      pixels[ix, iy].helper := pixels[ix, iy].y;
      inc(iy, 2);
    end;
    inc(ix, 2);
  end;

  for i4 := 1 to sharpenIterations do begin

    // step 5: sharpen +1+1 pixels
    weights[0] := 1;
    weights[1] := 1;
    weights[2] := 1;
    weights[3] := 1;
    CalcWeights(weights, 4);
    ix := 1;
    for i1 := 1 to width - 1 do begin
      iy := 1;
      for i2 := 1 to height - 1 do begin
        i3 := (ix - 1) shr 1 + (iy - 1) shr 1 * width;
        // distances: Sqrt(2)
        orgPixels[0] := src[i3            ].y;
        orgPixels[1] := src[i3     + width].y;
        orgPixels[2] := src[i3 + 1        ].y;
        orgPixels[3] := src[i3 + 1 + width].y;
        newPixels[0, 0] := pixels[ix - 1, iy - 1].y;
        newPixels[1, 0] := pixels[ix - 1, iy + 1].y;
        newPixels[2, 0] := pixels[ix + 1, iy - 1].y;
        newPixels[3, 0] := pixels[ix + 1, iy + 1].y;
        newPixels[0, 1] := weights[0];
        newPixels[1, 1] := weights[1];
        newPixels[2, 1] := weights[2];
        newPixels[3, 1] := weights[3];
        solve(orgPixels, newPixels, 4, pixels[ix, iy].y);
        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

    // step 6: sharpen +1+0 pixels
    weights[0] := Sqrt(5);
    weights[1] := Sqrt(5);
    weights[2] := 1;
    weights[3] := 1;
    weights[4] := Sqrt(5);
    weights[5] := Sqrt(5);
    CalcWeights(weights, 6);
    ix := 1;
    for i1 := 1 to width - 1 do begin
      iy := 2;
      for i2 := 1 to height - 3 do begin
        i3 := (ix - 1) shr 1 + (iy - 2) shr 1 * width;
        // distances: 1, Sqrt(5)
        orgPixels[0] := src[i3                  ].y;
        orgPixels[1] := src[i3 + 1              ].y;
        orgPixels[2] := src[i3     + width      ].y;
        orgPixels[3] := src[i3 + 1 + width      ].y;
        orgPixels[4] := src[i3     + width shl 1].y;
        orgPixels[5] := src[i3 + 1 + width shl 1].y;
        newPixels[0, 0] := pixels[ix - 1, iy - 2].y;
        newPixels[1, 0] := pixels[ix + 1, iy - 2].y;
        newPixels[2, 0] := pixels[ix - 1, iy    ].y;
        newPixels[3, 0] := pixels[ix + 1, iy    ].y;
        newPixels[4, 0] := pixels[ix - 1, iy + 2].y;
        newPixels[5, 0] := pixels[ix + 1, iy + 2].y;
        newPixels[0, 1] := weights[0];
        newPixels[1, 1] := weights[1];
        newPixels[2, 1] := weights[2];
        newPixels[3, 1] := weights[3];
        newPixels[4, 1] := weights[4];
        newPixels[5, 1] := weights[5];
        solve(orgPixels, newPixels, 6, pixels[ix, iy].y);
        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

    // step 7: sharpen +0+1 pixels
    weights[0] := Sqrt(5);
    weights[1] := 1;
    weights[2] := Sqrt(5);
    weights[3] := Sqrt(5);
    weights[4] := 1;
    weights[5] := Sqrt(5);
    CalcWeights(weights, 6);
    ix := 2;
    for i1 := 1 to width - 3 do begin
      iy := 1;
      for i2 := 1 to height - 1 do begin
        i3 := (ix - 2) shr 1 + (iy - 1) shr 1 * width;
        orgPixels[0] := src[i3            ].y;
        orgPixels[1] := src[i3 + 1        ].y;
        orgPixels[2] := src[i3 + 2        ].y;
        orgPixels[3] := src[i3     + width].y;
        orgPixels[4] := src[i3 + 1 + width].y;
        orgPixels[5] := src[i3 + 2 + width].y;
        newPixels[0, 0] := pixels[ix - 2, iy - 1].y;
        newPixels[1, 0] := pixels[ix,     iy - 1].y;
        newPixels[2, 0] := pixels[ix + 2, iy - 1].y;
        newPixels[3, 0] := pixels[ix - 2, iy + 1].y;
        newPixels[4, 0] := pixels[ix,     iy + 1].y;
        newPixels[5, 0] := pixels[ix + 2, iy + 1].y;
        newPixels[0, 1] := weights[0];
        newPixels[1, 1] := weights[1];
        newPixels[2, 1] := weights[2];
        newPixels[3, 1] := weights[3];
        newPixels[4, 1] := weights[4];
        newPixels[5, 1] := weights[5];
        solve(orgPixels, newPixels, 6, pixels[ix, iy].y);
        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

    // step 8: save current +0+0 pixels to helper field
    ix := 2;
    for i1 := 2 to width - 2 do begin
      iy := 2;
      for i2 := 2 to height - 2 do begin
        pixels[ix, iy].helper := pixels[ix, iy].y;
        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

    // step 9: sharpen +0+0 pixels
    weights[0] := Sqrt(8);
    weights[1] := 2;
    weights[2] := Sqrt(8);
    weights[3] := 2;
    weights[4] := 0;
    weights[5] := 2;
    weights[6] := Sqrt(8);
    weights[7] := 2;
    weights[8] := Sqrt(8);
    CalcWeights(weights, 9);
    ix := 2;
    for i1 := 2 to width - 2 do begin
      iy := 2;
      for i2 := 2 to height - 2 do begin
        i3 := (ix - 2) shr 1 + (iy - 2) shr 1 * width;
        // distances: 0, 2, Sqrt(8)
        orgPixels[0] := src[i3                  ].y;
        orgPixels[1] := src[i3 + 1              ].y;
        orgPixels[2] := src[i3 + 2              ].y;
        orgPixels[3] := src[i3     + width      ].y;
        orgPixels[4] := src[i3 + 1 + width      ].y;
        orgPixels[5] := src[i3 + 2 + width      ].y;
        orgPixels[6] := src[i3     + width shl 1].y;
        orgPixels[7] := src[i3 + 1 + width shl 1].y;
        orgPixels[8] := src[i3 + 2 + width shl 1].y;
        newPixels[0, 0] := pixels[ix - 2, iy - 2].helper;
        newPixels[1, 0] := pixels[ix,     iy - 2].helper;
        newPixels[2, 0] := pixels[ix + 2, iy - 2].helper;
        newPixels[3, 0] := pixels[ix - 2, iy    ].helper;
        newPixels[4, 0] := pixels[ix,     iy    ].helper;
        newPixels[5, 0] := pixels[ix + 2, iy    ].helper;
        newPixels[6, 0] := pixels[ix - 2, iy + 2].helper;
        newPixels[7, 0] := pixels[ix,     iy + 2].helper;
        newPixels[8, 0] := pixels[ix + 2, iy + 2].helper;
        newPixels[0, 1] := weights[0];
        newPixels[1, 1] := weights[1];
        newPixels[2, 1] := weights[2];
        newPixels[3, 1] := weights[3];
        newPixels[4, 1] := weights[4];
        newPixels[5, 1] := weights[5];
        newPixels[6, 1] := weights[6];
        newPixels[7, 1] := weights[7];
        newPixels[8, 1] := weights[8];
        solve(orgPixels, newPixels, 9, pixels[ix, iy].y);
        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

  end;

  SetLength(D1, newWidth, newHeight);
  SetLength(D2, newWidth, newHeight);
  SetLength(D3, newWidth, newHeight);
  SetLength(C1, newWidth, newHeight);
  SetLength(C2, newWidth, newHeight);

  // ICBI - iterative refinement (1)
  for g := 0 to high(icbiStepSizes) do begin
    diff := 0;
    step := icbiStepSizes[g];

    // computation of derivatives
    for ix := 3 to newWidth - 4 do begin
      iy := 4 - (ix and 1);
      for i2 := 2 to height - 2 do begin
        C1[ix, iy] := (pixels[ix - 1, iy - 1].y - pixels[ix + 1, iy + 1].y) / 2;
        C2[ix, iy] := (pixels[ix + 1, iy - 1].y - pixels[ix - 1, iy + 1].y) / 2;
        D1[ix, iy] := pixels[ix - 1, iy - 1].y + pixels[ix + 1, iy + 1].y - 2 * pixels[ix, iy].y;
        D2[ix, iy] := pixels[ix + 1, iy - 1].y + pixels[ix - 1, iy + 1].y - 2 * pixels[ix, iy].y;
        D3[ix, iy] := (pixels[ix, iy - 2].y - pixels[ix - 2, iy].y + pixels[ix, iy + 2].y - pixels[ix + 2, iy].y) / 2;
        inc(iy, 2);
      end;
    end;

    ix := 5;
    for i1 := 3 to width - 3 do begin
      iy := 5;
      for i2 := 3 to height - 3 do begin
        EN1 := abs(D1[ix, iy] - D1[ix + 1, iy + 1]) + abs(D1[ix, iy] - D1[ix - 1, iy - 1]);
        EN2 := abs(D1[ix, iy] - D1[ix + 1, iy - 1]) + abs(D1[ix, iy] - D1[ix - 1, iy + 1]);
        EN3 := abs(D2[ix, iy] - D2[ix + 1, iy + 1]) + abs(D2[ix, iy] - D2[ix - 1, iy - 1]);
        EN4 := abs(D2[ix, iy] - D2[ix + 1, iy - 1]) + abs(D2[ix, iy] - D2[ix - 1, iy + 1]);
        EN5 := abs(pixels[ix - 2, iy - 2].y + pixels[ix + 2, iy + 2].y - 2 * pixels[ix, iy].y);
        EN6 := abs(pixels[ix + 2, iy - 2].y + pixels[ix - 2, iy + 2].y - 2 * pixels[ix, iy].y);

        EA1 := abs(D1[ix, iy] - D1[ix + 1, iy + 1] - 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy - 1] - 3 * step);
        EA2 := abs(D1[ix, iy] - D1[ix + 1, iy - 1] - 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy + 1] - 3 * step);
        EA3 := abs(D2[ix, iy] - D2[ix + 1, iy + 1] - 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy - 1] - 3 * step);
        EA4 := abs(D2[ix, iy] - D2[ix + 1, iy - 1] - 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy + 1] - 3 * step);
        EA5 := abs(pixels[ix - 2, iy - 2].y + pixels[ix + 2, iy + 2].y - 2 * pixels[ix, iy].y - 2 * step);
        EA6 := abs(pixels[ix + 2, iy - 2].y + pixels[ix - 2, iy + 2].y - 2 * pixels[ix, iy].y - 2 * step);

        ES1 := abs(D1[ix, iy] - D1[ix + 1, iy + 1] + 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy - 1] + 3 * step);
        ES2 := abs(D1[ix, iy] - D1[ix + 1, iy - 1] + 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy + 1] + 3 * step);
        ES3 := abs(D2[ix, iy] - D2[ix + 1, iy + 1] + 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy - 1] + 3 * step);
        ES4 := abs(D2[ix, iy] - D2[ix + 1, iy - 1] + 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy + 1] + 3 * step);
        ES5 := abs(pixels[ix - 2, iy - 2].y + pixels[ix + 2, iy + 2].y - 2 * pixels[ix, iy].y + 2 * step);
        ES6 := abs(pixels[ix + 2, iy - 2].y + pixels[ix - 2, iy + 2].y - 2 * pixels[ix, iy].y + 2 * step);

        if (C1[ix, iy] <> 0) or (C2[ix, iy] <> 0) then begin
          EISO := (C1[ix, iy] * C1[ix, iy] * D2[ix, iy] - 2 * C1[ix, iy] * C2[ix, iy] * D3[ix, iy] + C2[ix, iy] * C2[ix, iy] * D1[ix, iy]) / (C1[ix, iy] * C1[ix, iy] + C2[ix, iy] * C2[ix, iy]);
          if abs(EISO) < 0.2 then
            EISO := 0;
        end else
          EISO := 0;

        EN := (AL * (EN1 + EN2 + EN3 + EN4)) + (BT * (EN5 + EN6));
        EA := (AL * (EA1 + EA2 + EA3 + EA4)) + (BT * (EA5 + EA6)) - (GM * sign(EISO));
        ES := (AL * (ES1 + ES2 + ES3 + ES4)) + (BT * (ES5 + ES6)) + (GM * sign(EISO));

        if (EN > EA) and (ES > EA) then begin
          if pixels[ix, iy].y + step > 255 then
            pixels[ix, iy].y := 255
          else
            pixels[ix, iy].y := pixels[ix, iy].y + step;

          maxValue := pixels[ix - 1, iy - 1].y;
          testValue := pixels[ix - 1, iy + 1].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix + 1, iy + 1].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix + 1, iy - 1].y;
          if testValue > maxValue then maxValue := testValue;
          if pixels[ix, iy].y > maxValue then
            pixels[ix, iy].y := maxValue;

          diff := diff + step;
        end else
          if (EN > ES) and (EA > ES) then begin
            if pixels[ix, iy].y <= step then
              pixels[ix, iy].y := 0
            else
              pixels[ix, iy].y := pixels[ix, iy].y - step;

            maxValue := pixels[ix - 1, iy - 1].y;
            testValue := pixels[ix - 1, iy + 1].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix + 1, iy + 1].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix + 1, iy - 1].y;
            if testValue < maxValue then maxValue := testValue;
            if pixels[ix, iy].y < maxValue then
              pixels[ix, iy].y := maxValue;

            diff := diff + step;
          end;

        inc(iy, 2);
      end;
      inc(ix, 2);
    end;

    if diff < TS then
      break;
  end;

  // ICBI - iterative refinement (2)
  for g := 0 to high(icbiStepSizes) do begin
    diff := 0;
    step := icbiStepSizes[g];

    // computation of derivatives
    for ix := 1 to newWidth - 3 do
      for iy := 1 to newHeight - 3 do begin
        C1[ix, iy] := (pixels[ix, iy - 1].y - pixels[ix, iy + 1].y) / 2;
        C2[ix, iy] := (pixels[ix - 1, iy].y - pixels[ix + 1, iy].y) / 2;
        D1[ix, iy] := pixels[ix, iy - 1].y + pixels[ix, iy + 1].y - 2 * pixels[ix, iy].y;
        D2[ix, iy] := pixels[ix + 1, iy].y + pixels[ix - 1, iy].y - 2 * pixels[ix, iy].y;
        D3[ix, iy] := (pixels[ix - 1, iy - 1].y - pixels[ix - 1, iy + 1].y + pixels[ix + 1, iy + 1].y - pixels[ix + 1, iy - 1].y) / 2;
      end;

    for ix := 2 to newWidth - 3 do begin
      iy := 3 - (ix and 1);
      for i2 := 1 to height - 2 do begin
        EN1 := abs(D1[ix, iy] - D1[ix, iy + 1]) + abs(D1[ix, iy] - D1[ix, iy - 1]);
        EN2 := abs(D1[ix, iy] - D1[ix + 1, iy]) + abs(D1[ix, iy] - D1[ix - 1, iy]);
        EN3 := abs(D2[ix, iy] - D2[ix, iy + 1]) + abs(D2[ix, iy] - D2[ix, iy - 1]);
        EN4 := abs(D2[ix, iy] - D2[ix + 1, iy]) + abs(D2[ix, iy] - D2[ix - 1, iy]);
        EN5 := abs(pixels[ix, iy - 2].y + pixels[ix, iy + 2].y - 2 * pixels[ix, iy].y);
        EN6 := abs(pixels[ix + 2, iy].y + pixels[ix - 2, iy].y - 2 * pixels[ix, iy].y);

        EA1 := abs(D1[ix, iy] - D1[ix, iy + 1] - 3 * step) + abs(D1[ix, iy] - D1[ix, iy - 1] - 3 * step);
        EA2 := abs(D1[ix, iy] - D1[ix + 1, iy] - 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy] - 3 * step);
        EA3 := abs(D2[ix, iy] - D2[ix, iy + 1] - 3 * step) + abs(D2[ix, iy] - D2[ix, iy - 1] - 3 * step);
        EA4 := abs(D2[ix, iy] - D2[ix + 1, iy] - 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy] - 3 * step);
        EA5 := abs(pixels[ix, iy - 2].y + pixels[ix, iy + 2].y - 2 * pixels[ix, iy].y - 2 * step);
        EA6 := abs(pixels[ix + 2, iy].y + pixels[ix - 2, iy].y - 2 * pixels[ix, iy].y - 2 * step);

        ES1 := abs(D1[ix, iy] - D1[ix, iy + 1] + 3 * step) + abs(D1[ix, iy] - D1[ix, iy - 1] + 3 * step);
        ES2 := abs(D1[ix, iy] - D1[ix + 1, iy] + 3 * step) + abs(D1[ix, iy] - D1[ix - 1, iy] + 3 * step);
        ES3 := abs(D2[ix, iy] - D2[ix, iy + 1] + 3 * step) + abs(D2[ix, iy] - D2[ix, iy - 1] + 3 * step);
        ES4 := abs(D2[ix, iy] - D2[ix + 1, iy] + 3 * step) + abs(D2[ix, iy] - D2[ix - 1, iy] + 3 * step);
        ES5 := abs(pixels[ix, iy - 2].y + pixels[ix, iy + 2].y - 2 * pixels[ix, iy].y + 2 * step);
        ES6 := abs(pixels[ix + 2, iy].y + pixels[ix - 2, iy].y - 2 * pixels[ix, iy].y + 2 * step);

        if (C1[ix, iy] <> 0) or (C2[ix, iy] <> 0) then begin
          EISO := (C1[ix, iy] * C1[ix, iy] * D2[ix, iy] - 2 * C1[ix, iy] * C2[ix, iy] * D3[ix, iy] + C2[ix, iy] * C2[ix, iy] * D1[ix, iy]) / (C1[ix, iy] * C1[ix, iy] + C2[ix, iy] * C2[ix, iy]);
          if abs(EISO) < 0.2 then
            EISO := 0;
        end else
          EISO := 0;

        EN := (AL * (EN1 + EN2 + EN3 + EN4)) + (BT * (EN5 + EN6));
        EA := (AL * (EA1 + EA2 + EA3 + EA4)) + (BT * (EA5 + EA6)) - (GM * sign(EISO));
        ES := (AL * (ES1 + ES2 + ES3 + ES4)) + (BT * (ES5 + ES6)) + (GM * sign(EISO));

        if (EN > EA) and (ES > EA) then begin
          if pixels[ix, iy].y + step > 255 then
            pixels[ix, iy].y := 255
          else
            pixels[ix, iy].y := pixels[ix, iy].y + step;

          maxValue := pixels[ix, iy - 1].y;
          testValue := pixels[ix - 1, iy].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix, iy + 1].y;
          if testValue > maxValue then maxValue := testValue;
          testValue := pixels[ix + 1, iy].y;
          if testValue > maxValue then maxValue := testValue;
          if pixels[ix, iy].y > maxValue then
            pixels[ix, iy].y := maxValue;

          diff := diff + step;
        end else
          if (EN > ES) and (EA > ES) then begin
            if pixels[ix, iy].y <= step then
              pixels[ix, iy].y := 0
            else
              pixels[ix, iy].y := pixels[ix, iy].y - step;

            maxValue := pixels[ix, iy - 1].y;
            testValue := pixels[ix - 1, iy].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix, iy + 1].y;
            if testValue < maxValue then maxValue := testValue;
            testValue := pixels[ix + 1, iy].y;
            if testValue < maxValue then maxValue := testValue;
            if pixels[ix, iy].y < maxValue then
              pixels[ix, iy].y := maxValue;

            diff := diff + step;
          end;

        inc(iy, 2);
      end;
    end;

    if diff < TS then
      break;
  end;

  for ix := 0 to newWidth - 1 do
    for iy := 0 to newHeight - 1 do
      dst[iy * newWidth + ix] := pixels[ix, iy];
end;

procedure ICBI(bmp: TBitmap; steps: integer);
var src : array of TYCbCr;
    dst : array of TYCbCr;
    i1, i2 : integer;
    pixel : integer;
    red, green, blue : integer;
    y, cb, cr : single;
begin
  SetLength(src, bmp.Width * bmp.Height);
  SetLength(dst, bmp.Width * 2 * bmp.Height * 2);
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      pixel := bmp.Canvas.Pixels[i1, i2];
      red := pixel and $ff;
      green := (pixel shr 8) and $ff;
      blue := (pixel shr 16) and $ff;
      y := red * 0.299 + green * 0.587 + blue * 0.114;
      cb := $80 + red * -0.1687358916478555 + green * -0.3312641083521445 + blue * 0.5;
      cr := $80 + red * 0.5 + green * -0.4186875891583452 + blue * -0.0813124108416548;
      if y < 0 then
        y := 0
      else
        if y > 255 then
          y := 255;
      if cb < 0 then
        cb := 0
      else
        if cb > 255 then
          cb := 255;
      if cr < 0 then
        cr := 0
      else
        if cr >= 255 then
          cr := 255;
      src[i2 * bmp.Width + i1].y := y;
      src[i2 * bmp.Width + i1].cb := cb;
      src[i2 * bmp.Width + i1].cr := cr;
    end;
  case steps of
    0 :  ICBI1Plane(@src[0], @dst[0], bmp.Width, bmp.Height, [],         0.5625, -0.0625);
    1 :  ICBI1Plane(@src[0], @dst[0], bmp.Width, bmp.Height, [8],        0.5625, -0.0625);
    2 :  ICBI1Plane(@src[0], @dst[0], bmp.Width, bmp.Height, [6, 10],    0.5625, -0.0625);
    3 :  ICBI1Plane(@src[0], @dst[0], bmp.Width, bmp.Height, [3, 6, 10], 0.5625, -0.0625);
    else ICBI1Plane(@src[0], @dst[0], bmp.Width, bmp.Height, [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 6, 6, 6, 6, 10, 10, 10, 10], 0.5625, -0.0625);
  end;
  bmp.Width := bmp.Width * 2 - 1;
  bmp.Height := bmp.Height * 2 - 1;
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      y := dst[i2 * bmp.Width + i1].y;
      cb := dst[i2 * bmp.Width + i1].cb - $80;
      cr := dst[i2 * bmp.Width + i1].cr - $80;
      red := round(y + cr * 1.402);
      green := round(y + cb * -0.3441362862010221 + cr * -0.7141362862010221);
      blue := round(y + cb * 1.772);
      if red < 0 then
        red := 0
      else
        if red > 255 then
          red := 255;
      if green < 0 then
        green := 0
      else
        if green > 255 then
          green := 255;
      if blue < 0 then
        blue := 0
      else
        if blue > 255 then
          blue := 255;
      bmp.Canvas.Pixels[i1, i2] := blue shl 16 + green shl 8 + red;
    end;
end;

procedure PR(bmp: TBitmap; icbiSteps, sharpenIterations: integer);
var src, dst : array of TYCbCr;
    srcY, dstY : array of byte;
    i1, i2 : integer;
    pixel : integer;
    red, green, blue : integer;
    y, cb, cr : single;
begin
  SetLength(src, bmp.Width * bmp.Height);
  SetLength(dst, bmp.Width * 2 * bmp.Height * 2);
  SetLength(srcY, bmp.Width * bmp.Height);
  SetLength(dstY, bmp.Width * 2 * bmp.Height * 2);
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      pixel := bmp.Canvas.Pixels[i1, i2];
      red := pixel and $ff;
      green := (pixel shr 8) and $ff;
      blue := (pixel shr 16) and $ff;
      y := red * 0.299 + green * 0.587 + blue * 0.114;
      cb := $80 + red * -0.1687358916478555 + green * -0.3312641083521445 + blue * 0.5;
      cr := $80 + red * 0.5 + green * -0.4186875891583452 + blue * -0.0813124108416548;
      if y < 0 then
        y := 0
      else
        if y > 255 then
          y := 255;
      if cb < 0 then
        cb := 0
      else
        if cb > 255 then
          cb := 255;
      if cr < 0 then
        cr := 0
      else
        if cr >= 255 then
          cr := 255;
      src[i2 * bmp.Width + i1].y := y;
      src[i2 * bmp.Width + i1].cb := cb;
      src[i2 * bmp.Width + i1].cr := cr;
      srcY[i2 * bmp.Width + i1] := round(y);
    end;

  case icbiSteps of
    0 :  PRPlane(@src[0], @dst[0], bmp.Width, bmp.Height, [], sharpenIterations);
    1 :  PRPlane(@src[0], @dst[0], bmp.Width, bmp.Height, [8], sharpenIterations);
    2 :  PRPlane(@src[0], @dst[0], bmp.Width, bmp.Height, [6, 10], sharpenIterations);
    3 :  PRPlane(@src[0], @dst[0], bmp.Width, bmp.Height, [3, 6, 10], sharpenIterations);
    else PRPlane(@src[0], @dst[0], bmp.Width, bmp.Height, [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 6, 6, 6, 6, 10, 10, 10, 10], sharpenIterations);
  end;

  bmp.Width := bmp.Width * 2 - 1;
  bmp.Height := bmp.Height * 2 - 1;
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      y := dst[i2 * bmp.Width + i1].y;
      if y < 0 then
        y := 0;
      cb := dst[i2 * bmp.Width + i1].cb;
      if cb < 0 then
        cb := -$80
      else
        cb := cb - $80;
      cr := dst[i2 * bmp.Width + i1].cr;
      if cr < 0 then
        cr := -$80
      else
        cr := cr - $80;
      red := round(y + cr * 1.402);
      green := round(y + cb * -0.3441362862010221 + cr * -0.7141362862010221);
      blue := round(y + cb * 1.772);
      if red < 0 then
        red := 0
      else
        if red > 255 then
          red := 255;
      if green < 0 then
        green := 0
      else
        if green > 255 then
          green := 255;
      if blue < 0 then
        blue := 0
      else
        if blue > 255 then
          blue := 255;
      bmp.Canvas.Pixels[i1, i2] := blue shl 16 + green shl 8 + red;
    end;
end;

procedure Geometric(bmp: TBitmap);
var src, dst : array of TYCbCr;
    srcY, dstY : array of byte;
    i1, i2 : integer;
    pixel : integer;
    red, green, blue : integer;
    y, cb, cr : single;
begin
  SetLength(src, bmp.Width * bmp.Height);
  SetLength(dst, bmp.Width * 2 * bmp.Height * 2);
  SetLength(srcY, bmp.Width * bmp.Height);
  SetLength(dstY, bmp.Width * 2 * bmp.Height * 2);
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      pixel := bmp.Canvas.Pixels[i1, i2];
      red := pixel and $ff;
      green := (pixel shr 8) and $ff;
      blue := (pixel shr 16) and $ff;
      y := red * 0.299 + green * 0.587 + blue * 0.114;
      cb := $80 + red * -0.1687358916478555 + green * -0.3312641083521445 + blue * 0.5;
      cr := $80 + red * 0.5 + green * -0.4186875891583452 + blue * -0.0813124108416548;
      if y < 0 then
        y := 0
      else
        if y > 255 then
          y := 255;
      if cb < 0 then
        cb := 0
      else
        if cb > 255 then
          cb := 255;
      if cr < 0 then
        cr := 0
      else
        if cr >= 255 then
          cr := 255;
      src[i2 * bmp.Width + i1].y := y;
      src[i2 * bmp.Width + i1].cb := cb;
      src[i2 * bmp.Width + i1].cr := cr;
      srcY[i2 * bmp.Width + i1] := round(y);
    end;

  GeometricPlane(@src[0], @dst[0], bmp.Width, bmp.Height);

  bmp.Width := bmp.Width * 2 - 1;
  bmp.Height := bmp.Height * 2 - 1;
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      y := dst[i2 * bmp.Width + i1].y;
      if y < 0 then
        y := 0;
      cb := dst[i2 * bmp.Width + i1].cb;
      if cb < 0 then
        cb := 0
      else
        cb := cb - $80;
      cr := dst[i2 * bmp.Width + i1].cr;
      if cr < 0 then
        cr := 0
      else
        cr := cr - $80;
      red := round(y + cr * 1.402);
      green := round(y + cb * -0.3441362862010221 + cr * -0.7141362862010221);
      blue := round(y + cb * 1.772);
      if red < 0 then
        red := 0
      else
        if red > 255 then
          red := 255;
      if green < 0 then
        green := 0
      else
        if green > 255 then
          green := 255;
      if blue < 0 then
        blue := 0
      else
        if blue > 255 then
          blue := 255;
      bmp.Canvas.Pixels[i1, i2] := blue shl 16 + green shl 8 + red;
    end;
end;

procedure FuzzyEla(bmp: TBitmap);
var src, dst : array of TYCbCr;
    srcY, dstY : array of byte;
    i1, i2 : integer;
    pixel : integer;
    red, green, blue : integer;
    y, cb, cr : single;
begin
  SetLength(src, bmp.Width * bmp.Height);
  SetLength(dst, bmp.Width * 2 * bmp.Height * 2);
  SetLength(srcY, bmp.Width * bmp.Height);
  SetLength(dstY, bmp.Width * 2 * bmp.Height * 2);
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      pixel := bmp.Canvas.Pixels[i1, i2];
      red := pixel and $ff;
      green := (pixel shr 8) and $ff;
      blue := (pixel shr 16) and $ff;
      y := red * 0.299 + green * 0.587 + blue * 0.114;
      cb := $80 + red * -0.1687358916478555 + green * -0.3312641083521445 + blue * 0.5;
      cr := $80 + red * 0.5 + green * -0.4186875891583452 + blue * -0.0813124108416548;
      if y < 0 then
        y := 0
      else
        if y > 255 then
          y := 255;
      if cb < 0 then
        cb := 0
      else
        if cb > 255 then
          cb := 255;
      if cr < 0 then
        cr := 0
      else
        if cr >= 255 then
          cr := 255;
      src[i2 * bmp.Width + i1].y := y;
      src[i2 * bmp.Width + i1].cb := cb;
      src[i2 * bmp.Width + i1].cr := cr;
      srcY[i2 * bmp.Width + i1] := round(y);
    end;

  FuzzyElaPlane(@src[0], @dst[0], bmp.Width, bmp.Height);

  bmp.Width := bmp.Width * 2 - 1;
  bmp.Height := bmp.Height * 2 - 1;
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      y := dst[i2 * bmp.Width + i1].y;
      if y < 0 then
        y := 0;
      cb := dst[i2 * bmp.Width + i1].cb;
      if cb < 0 then
        cb := 0
      else
        cb := cb - $80;
      cr := dst[i2 * bmp.Width + i1].cr;
      if cr < 0 then
        cr := 0
      else
        cr := cr - $80;
      red := round(y + cr * 1.402);
      green := round(y + cb * -0.3441362862010221 + cr * -0.7141362862010221);
      blue := round(y + cb * 1.772);
      if red < 0 then
        red := 0
      else
        if red > 255 then
          red := 255;
      if green < 0 then
        green := 0
      else
        if green > 255 then
          green := 255;
      if blue < 0 then
        blue := 0
      else
        if blue > 255 then
          blue := 255;
      bmp.Canvas.Pixels[i1, i2] := blue shl 16 + green shl 8 + red;
    end;
end;

procedure NediPlane(src: TPAByte; width, height, srcPitch: integer; dst: TPAByte; dstPitch: integer);

  const
    threshold4 : double = 1000;
    threshold8 : double = 2000;
    threshold16 : double = 4000;
    constraint : integer = 2;
    mvar : integer = 16 shl 4;
    coef1 : double = 0.5625;
    coef2 : double = -0.0625;

  function CheckVariance(const pc: array of integer) : boolean;
  var sum, sumsq : integer;
  begin
    sum := pc[0] + pc[1] + pc[2] + pc[3];
    sumsq := pc[0] * pc[0] + pc[1] * pc[1] + pc[2] * pc[2] + pc[3] * pc[3];
    result := sumsq shl 2 - sum * sum > mvar;
  end;

  function Interpolate(p1, p2, p3, p4: byte) : byte;
  var i1 : integer;
  begin
    i1 := Round((p2 + p3) * coef1 + (p1 + p4) * coef2);
    if i1 < 0 then
      i1 := 0
    else
      if i1 > 255 then
        i1 := 255;
    result := i1;
  end;

  type
    TMatrix4 = record case boolean of
                 false : (arr: array [0..3] of array [0..3] of double);
                 true  : (flt: array [0..15] of double);
               end;
    TMatrix8 = record case boolean of
                 false : (arr: array [0..7] of array [0..7] of double);
                 true  : (flt: array [0..63] of double);
               end;

  function InvertMatrix4(var src: TMatrix4; var dst: TMatrix4) : boolean;
  // from doom3 sdk
  const MATRIX_INVERSE_EPSILON = 1e-14;
  var r0, r1, r2, r3 : array [0..1] of array [0..1] of double;
      a, det, invDet : double;
  begin
    result := false;

    // inverse r0
    det := src.flt[0*4+0] * src.flt[1*4+1] - src.flt[0*4+1] * src.flt[1*4+0];
    if abs(det) < MATRIX_INVERSE_EPSILON then
      exit;

    invDet := 1 / det;
    r0[0][0] :=   src.flt[1*4+1] * invDet;
    r0[0][1] := - src.flt[0*4+1] * invDet;
    r0[1][0] := - src.flt[1*4+0] * invDet;
    r0[1][1] :=   src.flt[0*4+0] * invDet;

    // r1 := r0 * m1;
    r1[0][0] := r0[0][0] * src.flt[0*4+2] + r0[0][1] * src.flt[1*4+2];
    r1[0][1] := r0[0][0] * src.flt[0*4+3] + r0[0][1] * src.flt[1*4+3];
    r1[1][0] := r0[1][0] * src.flt[0*4+2] + r0[1][1] * src.flt[1*4+2];
    r1[1][1] := r0[1][0] * src.flt[0*4+3] + r0[1][1] * src.flt[1*4+3];

    // r2 := m2 * r1;
    r2[0][0] := src.flt[2*4+0] * r1[0][0] + src.flt[2*4+1] * r1[1][0];
    r2[0][1] := src.flt[2*4+0] * r1[0][1] + src.flt[2*4+1] * r1[1][1];
    r2[1][0] := src.flt[3*4+0] * r1[0][0] + src.flt[3*4+1] * r1[1][0];
    r2[1][1] := src.flt[3*4+0] * r1[0][1] + src.flt[3*4+1] * r1[1][1];

    // r3 := r2 - m3;
    r3[0][0] := r2[0][0] - src.flt[2*4+2];
    r3[0][1] := r2[0][1] - src.flt[2*4+3];
    r3[1][0] := r2[1][0] - src.flt[3*4+2];
    r3[1][1] := r2[1][1] - src.flt[3*4+3];

    // inverse r3
    det := r3[0][0] * r3[1][1] - r3[0][1] * r3[1][0];
    if det < MATRIX_INVERSE_EPSILON then
      exit;

    invDet := 1 / det;
    a := r3[0][0];
    r3[0][0] :=   r3[1][1] * invDet;
    r3[0][1] := - r3[0][1] * invDet;
    r3[1][0] := - r3[1][0] * invDet;
    r3[1][1] :=   a * invDet;

    // r2 := m2 * r0;
    r2[0][0] := src.flt[2*4+0] * r0[0][0] + src.flt[2*4+1] * r0[1][0];
    r2[0][1] := src.flt[2*4+0] * r0[0][1] + src.flt[2*4+1] * r0[1][1];
    r2[1][0] := src.flt[3*4+0] * r0[0][0] + src.flt[3*4+1] * r0[1][0];
    r2[1][1] := src.flt[3*4+0] * r0[0][1] + src.flt[3*4+1] * r0[1][1];

    // m2 := r3 * r2;
    dst.flt[2*4+0] := r3[0][0] * r2[0][0] + r3[0][1] * r2[1][0];
    dst.flt[2*4+1] := r3[0][0] * r2[0][1] + r3[0][1] * r2[1][1];
    dst.flt[3*4+0] := r3[1][0] * r2[0][0] + r3[1][1] * r2[1][0];
    dst.flt[3*4+1] := r3[1][0] * r2[0][1] + r3[1][1] * r2[1][1];

    // m0 := r0 - r1 * m2;
    dst.flt[0*4+0] := r0[0][0] - r1[0][0] * dst.flt[2*4+0] - r1[0][1] * dst.flt[3*4+0];
    dst.flt[0*4+1] := r0[0][1] - r1[0][0] * dst.flt[2*4+1] - r1[0][1] * dst.flt[3*4+1];
    dst.flt[1*4+0] := r0[1][0] - r1[1][0] * dst.flt[2*4+0] - r1[1][1] * dst.flt[3*4+0];
    dst.flt[1*4+1] := r0[1][1] - r1[1][0] * dst.flt[2*4+1] - r1[1][1] * dst.flt[3*4+1];

    // m1 := r1 * r3;
    dst.flt[0*4+2] := r1[0][0] * r3[0][0] + r1[0][1] * r3[1][0];
    dst.flt[0*4+3] := r1[0][0] * r3[0][1] + r1[0][1] * r3[1][1];
    dst.flt[1*4+2] := r1[1][0] * r3[0][0] + r1[1][1] * r3[1][0];
    dst.flt[1*4+3] := r1[1][0] * r3[0][1] + r1[1][1] * r3[1][1];

    // m3 := -r3;
    dst.flt[2*4+2] := -r3[0][0];
    dst.flt[2*4+3] := -r3[0][1];
    dst.flt[3*4+2] := -r3[1][0];
    dst.flt[3*4+3] := -r3[1][1];

    result := true;
  end;

  function InvertMatrix8(const src: TMatrix8; var dst: TMatrix8) : boolean;
  const initdst : TMatrix8 = (flt: (1, 0, 0, 0, 0, 0, 0, 0,
                                    0, 1, 0, 0, 0, 0, 0, 0,
                                    0, 0, 1, 0, 0, 0, 0, 0,
                                    0, 0, 0, 1, 0, 0, 0, 0,
                                    0, 0, 0, 0, 1, 0, 0, 0,
                                    0, 0, 0, 0, 0, 1, 0, 0,
                                    0, 0, 0, 0, 0, 0, 1, 0,
                                    0, 0, 0, 0, 0, 0, 0, 1));
  var ctc : TMatrix8;
      best, i1, i2, i3 : integer;
      temp, scale : double;
  begin
    result := false;
    ctc := src;
    dst := initdst;
    for i1 := 0 to 7 do begin
      best := i1;
      for i2 := i1 + 1 to 7 do
        if abs(ctc.arr[best][i1]) < abs(ctc.arr[i2][i1]) then
          best := i2;
      if best <> i1 then
        for i3 := 0 to 7 do begin
          temp := ctc.arr[i1][i3];
          ctc.arr[i1][i3] := ctc.arr[best][i3];
          ctc.arr[best][i3] := temp;
          temp := dst.arr[i1][i3];
          dst.arr[i1][i3] := dst.arr[best][i3];
          dst.arr[best][i3] := temp;
        end;
      if ctc.arr[i1][i1] = 0 then
        exit;

      temp := ctc.arr[i1][i1];
      for i3 := 0 to 7 do
        if i3 <> i1 then begin
          scale := -(ctc.arr[i3][i1]/temp);
          for i2 := 0 to 7 do begin
            ctc.arr[i3][i2] := ctc.arr[i3][i2] + (ctc.arr[i1][i2] * scale);
            dst.arr[i3][i2] := dst.arr[i3][i2] + (dst.arr[i1][i2] * scale);
          end;
          ctc.arr[i3][i1] := 0;
        end;
    end;
    for i1 := 0 to 7 do
      if ctc.arr[i1][i1] <> 1 then begin
        scale := 1 / ctc.arr[i1][i1];
        for i3 := 0 to 7 do
          dst.arr[i1][i3] := dst.arr[i1][i3] * scale;
        ctc.arr[i1][i1] := 1;
      end;

    result := true;
  end;

  type
    TA256Int = array [0..255] of integer;
    TA256x4Int = array [0..255] of array [0..3] of integer;

  function ConditionNumber(const CtC, CtCi: TMatrix4) : double;
  // uses the max row sum norm since its easy to compute
  var highest1, highest2, temp : double;
      i : integer;
  begin
    highest1 := abs(CtCi.arr[0][0]) + abs(CtCi.arr[0][1]) + abs(CtCi.arr[0][2]) + abs(CtCi.arr[0][3]);
    highest2 := abs(CtC.arr[0][0]) + abs(CtC.arr[0][1]) + abs(CtC.arr[0][2]) + abs(CtC.arr[0][3]);
    for i := 1 to 3 do begin
      temp := abs(CtCi.arr[i][0]) + abs(CtCi.arr[i][1]) + abs(CtCi.arr[i][2]) + abs(CtCi.arr[i][3]);
      if temp > highest1 then
        highest1 := temp;
      temp := abs(CtC.arr[i][0]) + abs(CtC.arr[i][1]) + abs(CtC.arr[i][2]) + abs(CtC.arr[i][3]);
      if temp > highest2 then
        highest2 := temp;
    end;
    result := highest1 * highest2;
  end;

  function NEDI_4x4(const p: TA256Int; const n: TA256x4Int; var output: byte) : boolean;
  var i, j, k, final : integer;
      a, r : array [0..3] of double;
      CtC, CtCi : TMatrix4;
      s : double;
  begin
    result := false;

    // compute covariance matrix CtC := Ct * C (4x16 * 16x4 = 4x4)
    for i := 0 to 3 do
      for j := i to 3 do begin
        s := 0;
        for k := 0 to 15 do
          s := s + n[k][i] * n[k][j];
        CtC.arr[i][j] := s;
        CtC.arr[j][i] := s;
      end;

    // compute CtCi (CtC inverse)
    if not InvertMatrix4(CtC, CtCi) then
      exit;

    // compute condition number
    if (threshold4 > 0) and (ConditionNumber(CtC, CtCi) >= threshold4) then
      exit;

    // compute matrix r[] := Ct * p (4x16 * 16x1 = 4x1)
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 15 do
        s := s + n[k][i] * p[k];
      r[i] := s;
    end;

    // compute weights (CtCi[] * r[]) (4x4 * 4x1 = 4x1)
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 3 do
        s := s + CtCi.arr[i][k] * r[k];
      a[i] := s;
    end;

    // calculate interpolated pixel
    final := round(p[3] * a[0] + p[6] * a[1] + p[9] * a[2] + p[12] * a[3]);
    if ( (constraint = 1) and
         ((final > 255) or (final < 0)) ) or
       ( (constraint = 2) and
         ( (final > max(max(p[3], p[6]), max(p[9], p[12])) + 5) or
           (final < min(min(p[3], p[6]), min(p[9], p[12])) - 5)    ) ) then
      exit;

    if final > 255 then
      final := 255
    else
      if final < 0 then
        final := 0;

    output := final;
    result := true;
  end;

  function NEDI_8x8(const p: TA256Int; const n: TA256x4Int; var output: byte) : boolean;
  var i, j, k, final : integer;
      a, r : array [0..3] of double;
      CtC, CtCi : TMatrix4;
      s : double;
  begin
    result := false;

    // compute covariance matrix CtC := Ct * C (4x64 * 64x4 = 4x4)
    for i := 0 to 3 do
      for j := i to 3 do begin
        s := 0;
        for k := 0 to 63 do
          s := s + n[k][i] * n[k][j];
        CtC.arr[i][j] := s;
        CtC.arr[j][i] := s;
      end;

    // compute CtCi (CtC inverse)
    if not InvertMatrix4(CtC, CtCi) then
      exit;

    // compute condition number
    if (threshold8 > 0) and (ConditionNumber(CtC, CtCi) >= threshold8) then
      exit;

    // compute matrix r[] := Ct * p (4x64 * 64x1 = 4x1)
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 63 do
        s := s + n[k][i] * p[k];
      r[i] := s;
    end;

    // compute weights (CtCi[] * r[]) (4x4 * 4x1 = 4x1)
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 3 do
        s := s + CtCi.arr[i][k] * r[k];
      a[i] := s;
    end;

    // calculate interpolated pixel
    final := round(p[15] * a[0] + p[26] * a[1] + p[37] * a[2] + p[48] * a[3]);
    if ( (constraint = 1) and
         ((final > 255) or (final < 0)) ) or
       ( (constraint = 2) and
         ( (final > max(max(p[15], p[26]), max(p[37], p[48])) + 5) or
           (final < min(min(p[15], p[26]), min(p[37], p[48])) - 5)    ) ) then
      exit;

    if final > 255 then
      final := 255
    else
      if final < 0 then
        final := 0;

    output := final;
    result := true;
  end;

  function NEDI_16x16(const p: TA256Int; const n: TA256x4Int; var output: byte) : boolean;
  var i, j, k, final : integer;
      a, r : array [0..3] of double;
      CtC, CtCi : TMatrix4;
      s : double;
  begin
    result := false;

    // compute covariance matrix CtC := Ct * C (4x256 * 256x4 = 4x4)
    for i := 0 to 3 do
      for j := i to 3 do begin
        s := 0;
        for k := 0 to 255 do
          s := s + n[k][i] * n[k][j];
        CtC.arr[i][j] := s;
        CtC.arr[j][i] := s;
      end;

    // compute CtCi (CtC inverse)
    if not InvertMatrix4(CtC, CtCi) then
      exit;

    // compute condition number
    if (threshold16 > 0) and (ConditionNumber(CtC, CtCi) >= threshold16) then
      exit;

    // compute matrix r[] := Ct * p (4x256 * 256x1 = 4x1)
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 255 do
        s := s + n[k][i] * p[k];
      r[i] := s;
    end;

    // compute weights (CtCi[] * r[]) (4x4 * 4x1 = 4x1)
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 3 do
        s := s + CtCi.arr[i][k] * r[k];
      a[i] := s;
    end;

    // windowPat256:
    // 000 001 004 005 016 017 020 021 064 065 068 069 080 081 084 085
    // 002 003 006 007 018 019 022 023 066 067 070 071 082 083 086 087
    // 008 009 012 013 024 025 028 029 072 073 076 077 088 089 092 093
    // 010 011 014 015 026 027 030 031 074 075 078 079 090 091 094 095
    // 032 033 036 037 048 049 052 053 096 097 100 101 112 113 116 117
    // 034 035 038 039 050 051 054 055 098 099 102 103 114 115 118 119
    // 040 041 044 045 056 057 060 061 104 105 108 109 120 121 124 125
    // 042 043 046 047 058 059 062 063_106 107 110 111 122 123 126 127
    // 128 129 132 133 144 145 148 149 192 193 196 197 208 209 212 213
    // 130 131 134 135 146 147 150 151 194 195 198 199 210 211 214 215
    // 136 137 140 141 152 153 156 157 200 201 204 205 216 217 220 221
    // 138 139 142 143 154 155 158 159 202 203 206 207 218 219 222 223
    // 160 161 164 165 176 177 180 181 224 225 228 229 240 241 244 245
    // 162 163 166 167 178 179 182 183 226 227 230 231 242 243 246 247
    // 168 169 172 173 184 185 188 189 232 233 236 237 248 249 252 253
    // 170 171 174 175 186 187 190 191 234 235 238 239 250 251 254 255

    // calculate interpolated pixel
    final := round(p[63] * a[0] + p[106] * a[1] + p[149] * a[2] + p[192] * a[3]);
    if ( (constraint = 1) and
         ((final > 255) or (final < 0)) ) or
       ( (constraint = 2) and
         ( (final > max(max(p[63], p[106]), max(p[149], p[192])) + 5) or
           (final < min(min(p[63], p[106]), min(p[149], p[192])) - 5)    ) ) then
      exit;

    if final > 255 then
      final := 255
    else
      if final < 0 then
        final := 0;

    output := final;
    result := true;
  end;

var dstPitch2, dstPitch3 : integer;
    x, y, u, v, temp, temp1, temp2 : integer;
    dst2, dstp, dstpp, dstn, dstnn : TPAByte;
    width2, height2 : integer;
    p : TA256Int;
    n : TA256x4Int;
    localPat1, localPat2 : array [0..3] of integer;
    windowPat161, windowPat162 : array [0..15] of integer;
    windowPat641, windowPat642 : array [0..63] of integer;
    windowPat2561, windowPat2562 : array [0..255] of integer;
    success : boolean;
begin
  width2 := width shl 1;
  height2 := height shl 1;
  dstPitch2 := dstPitch * 2;
  dstPitch3 := dstPitch * 3;

  // copy over original pixels
  dst2 := dst;
  for y := 0 to height - 1 do begin
    for x := 0 to width - 1 do
      dst2[x shl 1] := src[x];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    src  := pointer(NativeInt(src ) + srcPitch);
  end;

  // interpolate areas where NEDI can't be used
  dst2 := dst;
  temp2 := width2 - 4;
  y := 0;
  while y < 11 do begin
    dst2[1] := (dst2[0] + dst2[2] + 1) shr 1;
    x := 3;
    while x < temp2 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2[x] := (dst2[x - 1] + dst2[x + 1] + 1) shr 1;
    inc(x, 2);
    dst2[x] := dst2[x - 1];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  temp1 := height2 - 12;
  temp2 := width2 - 4;
  while y < temp1 do begin
    dst2[1] := (dst2[0] + dst2[2] + 1) shr 1;
    x := 3;
    while x < 9 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    x := width2 - 9;
    while x < temp2 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2[x] := (dst2[x - 1] + dst2[x + 1] + 1) shr 1;
    inc(x, 2);
    dst2[x] := dst2[x - 1];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  temp2 := width2 - 4;
  while y < height2 do begin
    dst2[1] := (dst2[0] + dst2[2] + 1) shr 1;
    x := 3;
    while x < temp2 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2[x] := (dst2[x - 1] + dst2[x + 1] + 1) shr 1;
    inc(x, 2);
    dst2[x] := dst2[x - 1];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  dst2  := pointer(NativeInt(dst ) + dstPitch);
  dstp  := pointer(NativeInt(dst2) - dstPitch);
  dstn  := pointer(NativeInt(dst2) + dstPitch);
//  dstpp := pointer(NativeInt(dstp) - dstPitch2);
  dstnn := pointer(NativeInt(dstn) + dstPitch2);
  for x := 0 to width2 - 1 do
    dst2[x] := (dstp[x] + dstn[x] + 1) shr 1;
  dst2 := pointer(NativeInt(dst2) + dstPitch2);
  dstpp := dstp;
  dstp := dstn;
  dstn := dstnn;
  dstnn := pointer(NativeInt(dstnn) + dstPitch2);
  y := 3;
  while y < 9 do begin
    for x := 0 to width2 - 1 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstpp := dstp;
    dstp := dstn;
    dstn := dstnn;
    dstnn := pointer(NativeInt(dstnn) + dstPitch2);
    inc(y, 2);
  end;
  temp1 := height2 - 10;
  while y < temp1 do begin
    for x := 0 to 8 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    for x := width2 - 9 to width2 - 1 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstpp := dstp;
    dstp := dstn;
    dstn := dstnn;
    dstnn := pointer(NativeInt(dstnn) + dstPitch2);
    inc(y, 2);
  end;
  temp1 := height2 - 4;
  while y < temp1 do begin
    for x := 0 to width2 - 1 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstpp := dstp;
    dstp := dstn;
    dstn := dstnn;
    dstnn := pointer(NativeInt(dstnn) + dstPitch2);
    inc(y, 2);
  end;
  for x := 0 to width2 - 1 do
    dst2[x] := (dstp[x] + dstn[x] + 1) shr 1;
  dstp := dstn;
  dst2 := pointer(NativeInt(dst2) + dstPitch2);
  for x := 0 to width2 - 1 do
    dst2[x] := dstp[x];

  // NEDI
  localPat1[0] := -dstPitch2 - 2;
  localPat1[1] := -dstPitch2 + 2;
  localPat1[2] := dstPitch2 - 2;
  localPat1[3] := dstPitch2 + 2;
  localPat2[0] := -2;
  localPat2[1] := -dstPitch2;
  localPat2[2] := dstPitch2;
  localPat2[3] := 2;
  windowPat161[0] := -dstPitch3 - 3;
  windowPat161[1] := -dstPitch3 - 1;
  windowPat161[2] := -dstPitch - 3;
  windowPat161[3] := -dstPitch - 1;
  windowPat161[4] := -dstPitch3 + 1;
  windowPat161[5] := -dstPitch3 + 3;
  windowPat161[6] := -dstPitch + 1;
  windowPat161[7] := -dstPitch + 3;
  windowPat161[8] := dstPitch - 3;
  windowPat161[9] := dstPitch - 1;
  windowPat161[10] := dstPitch3 - 3;
  windowPat161[11] := dstPitch3 - 1;
  windowPat161[12] := dstPitch + 1;
  windowPat161[13] := dstPitch + 3;
  windowPat161[14] := dstPitch3 + 1;
  windowPat161[15] := dstPitch3 + 3;
  windowPat162[0] := -3;
  windowPat162[1] := -dstPitch - 2;
  windowPat162[2] := dstPitch - 2;
  windowPat162[3] := -1;
  windowPat162[4] := -dstPitch2 - 1;
  windowPat162[5] := -dstPitch3;
  windowPat162[6] := -dstPitch;
  windowPat162[7] := -dstPitch2 + 1;
  windowPat162[8] := dstPitch2 - 1;
  windowPat162[9] := dstPitch;
  windowPat162[10] := dstPitch3;
  windowPat162[11] := dstPitch2 + 1;
  windowPat162[12] := 1;
  windowPat162[13] := -dstPitch + 2;
  windowPat162[14] := dstPitch + 2;
  windowPat162[15] := 3;
  for v := 0 to 3 do
    for u := 0 to 15 do begin
      windowPat641[v shl 4 + u] := windowPat161[u] + localPat1[v] shl 1;
      windowPat642[v shl 4 + u] := windowPat162[u] + localPat2[v] shl 1;
    end;
  for v := 0 to 3 do
    for u := 0 to 63 do begin
      windowPat2561[v shl 6 + u] := windowPat641[u] + localPat1[v] shl 2;
      windowPat2562[v shl 6 + u] := windowPat642[u] + localPat2[v] shl 2;
    end;
  dst2 := pointer(NativeInt(dst) + 9 * dstPitch);
  temp1 := height2 - 9;
  temp2 := width2 - 9;
  y := 9;
  while y < temp1 do begin
    x := 9;
    while x < temp2 do begin
      success := false;
      if CheckVariance([dst2[x + windowPat161[3]], dst2[x + windowPat161[6]], dst2[x + windowPat161[9]], dst2[x + windowPat161[12]]]) then begin
        for u := 0 to 15 do begin
          p[u] := dst2[x + windowPat161[u]];
          n[u][0] := dst2[x + windowPat161[u] + localPat1[0]];
          n[u][1] := dst2[x + windowPat161[u] + localPat1[1]];
          n[u][2] := dst2[x + windowPat161[u] + localPat1[2]];
          n[u][3] := dst2[x + windowPat161[u] + localPat1[3]];
        end;
        success := NEDI_4x4(p, n, dst2[x]);
        if not success then begin
          for u := 0 to 63 do begin
            p[u] := dst2[x + windowPat641[u]];
            n[u][0] := dst2[x + windowPat641[u] + localPat1[0]];
            n[u][1] := dst2[x + windowPat641[u] + localPat1[1]];
            n[u][2] := dst2[x + windowPat641[u] + localPat1[2]];
            n[u][3] := dst2[x + windowPat641[u] + localPat1[3]];
          end;
          success := NEDI_8x8(p, n, dst2[x]);
          if (not success) and (x > 16) and (y > 16) and (x + 16 < width2) and (y + 16 < height2) then begin
            for u := 0 to 255 do begin
              p[u] := dst2[x + windowPat2561[u]];
              n[u][0] := dst2[x + windowPat2561[u] + localPat1[0]];
              n[u][1] := dst2[x + windowPat2561[u] + localPat1[1]];
              n[u][2] := dst2[x + windowPat2561[u] + localPat1[2]];
              n[u][3] := dst2[x + windowPat2561[u] + localPat1[3]];
            end;
            success := NEDI_16x16(p, n, dst2[x]);
          end;
        end;
      end;
      if not success then
        dst2[x] := Interpolate(Interpolate(dst2[x + windowPat161[ 0]], dst2[x + windowPat161[ 1]], dst2[x + windowPat161[ 4]], dst2[x + windowPat161[ 5]]),
                               Interpolate(dst2[x + windowPat161[ 2]], dst2[x + windowPat161[ 3]], dst2[x + windowPat161[ 6]], dst2[x + windowPat161[ 7]]),
                               Interpolate(dst2[x + windowPat161[ 8]], dst2[x + windowPat161[ 9]], dst2[x + windowPat161[12]], dst2[x + windowPat161[13]]),
                               Interpolate(dst2[x + windowPat161[10]], dst2[x + windowPat161[11]], dst2[x + windowPat161[14]], dst2[x + windowPat161[15]]));
      inc(x, 2);
    end;
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  dst2 := pointer(NativeInt(dst) + 9 * dstPitch);
  for y := 9 to temp1 - 1 do begin
    temp := y and 1;
    x := 9 + temp;
    while x < temp2 do begin
      success := false;
      if CheckVariance([dst2[x + windowPat162[3]], dst2[x + windowPat162[6]], dst2[x + windowPat162[9]], dst2[x + windowPat162[12]]]) then begin
        for u := 0 to 15 do begin
          p[u] := dst2[x + windowPat162[u]];
          n[u][0] := dst2[x + windowPat162[u] + localPat2[0]];
          n[u][1] := dst2[x + windowPat162[u] + localPat2[1]];
          n[u][2] := dst2[x + windowPat162[u] + localPat2[2]];
          n[u][3] := dst2[x + windowPat162[u] + localPat2[3]];
        end;
        success := NEDI_4x4(p, n, dst2[x]);
        if not success then begin
          for u := 0 to 63 do begin
            p[u] := dst2[x + windowPat642[u]];
            n[u][0] := dst2[x + windowPat642[u] + localPat2[0]];
            n[u][1] := dst2[x + windowPat642[u] + localPat2[1]];
            n[u][2] := dst2[x + windowPat642[u] + localPat2[2]];
            n[u][3] := dst2[x + windowPat642[u] + localPat2[3]];
          end;
          success := NEDI_8x8(p, n, dst2[x]);
          if (not success) and (x > 16) and (y > 16) and (x + 16 < width2) and (y + 16 < height2) then begin
            for u := 0 to 255 do begin
              p[u] := dst2[x + windowPat2562[u]];
              n[u][0] := dst2[x + windowPat2562[u] + localPat2[0]];
              n[u][1] := dst2[x + windowPat2562[u] + localPat2[1]];
              n[u][2] := dst2[x + windowPat2562[u] + localPat2[2]];
              n[u][3] := dst2[x + windowPat2562[u] + localPat2[3]];
            end;
            success := NEDI_16x16(p, n, dst2[x]);
          end;
        end;
      end;
      if not success then
        if temp <> 0 then
          dst2[x] := Interpolate(dst2[x + windowPat162[5]], dst2[x + windowPat162[6]], dst2[x + windowPat162[9]], dst2[x + windowPat162[10]])
        else
          dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2 := pointer(NativeInt(dst2) + dstPitch);
  end;
end;

procedure ZhaoXinLiPlane(src: TPAByte; width, height, srcPitch: integer; dst: TPAByte; dstPitch: integer);

  const
    mvar : integer = 8 shl 4;
    coef1 : double = 0.5625;
    coef2 : double = -0.0625;

  function CheckVariance(const pc: array of integer) : boolean;
  var sum, sumsq : integer;
  begin
    sum := pc[0] + pc[1] + pc[2] + pc[3];
    sumsq := pc[0] * pc[0] + pc[1] * pc[1] + pc[2] * pc[2] + pc[3] * pc[3];
    result := sumsq shl 2 - sum * sum > mvar;
  end;

  function Interpolate(p1, p2, p3, p4: byte) : byte;
  var i1 : integer;
  begin
    i1 := Round((p2 + p3) * coef1 + (p1 + p4) * coef2);
    if i1 < 0 then
      i1 := 0
    else
      if i1 > 255 then
        i1 := 255;
    result := i1;
  end;

  type
    TMatrix12 = record case boolean of
                  false : (arr: array [0..11] of array [0..11] of double);
                  true  : (flt: array [0..143] of double);
                end;

  function InvertMatrix12(const src: TMatrix12; var dst: TMatrix12) : boolean;
  const initdst : TMatrix12 = (flt: (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1));
  var ctc : TMatrix12;
      best, i1, i2, i3 : integer;
      temp, scale : double;
  begin
    result := false;
    ctc := src;
    dst := initdst;
    for i1 := 0 to 11 do begin
      best := i1;
      for i2 := i1 + 1 to 11 do
        if abs(ctc.arr[best][i1]) < abs(ctc.arr[i2][i1]) then
          best := i2;
      if best <> i1 then
        for i3 := 0 to 11 do begin
          temp := ctc.arr[i1][i3];
          ctc.arr[i1][i3] := ctc.arr[best][i3];
          ctc.arr[best][i3] := temp;
          temp := dst.arr[i1][i3];
          dst.arr[i1][i3] := dst.arr[best][i3];
          dst.arr[best][i3] := temp;
        end;
      if ctc.arr[i1][i1] = 0 then
        exit;

      temp := ctc.arr[i1][i1];
      for i3 := 0 to 11 do
        if i3 <> i1 then begin
          scale := -(ctc.arr[i3][i1]/temp);
          for i2 := 0 to 11 do begin
            ctc.arr[i3][i2] := ctc.arr[i3][i2] + (ctc.arr[i1][i2] * scale);
            dst.arr[i3][i2] := dst.arr[i3][i2] + (dst.arr[i1][i2] * scale);
          end;
          ctc.arr[i3][i1] := 0;
        end;
    end;
    for i1 := 0 to 11 do
      if ctc.arr[i1][i1] <> 1 then begin
        scale := 1 / ctc.arr[i1][i1];
        for i3 := 0 to 11 do
          dst.arr[i1][i3] := dst.arr[i1][i3] * scale;
        ctc.arr[i1][i1] := 1;
      end;

    result := true;
  end;

  type
    TA60Int = array [0..59] of integer;
    TA60x12Int = array [0..59] of array [0..11] of integer;

  function NEDI_8x8(var p, q: TA60Int; var n: TA60x12Int; var output: byte) : boolean;
  var i, j, k, final : integer;
      a, r : array [0..11] of double;
      CtC, CtCi : TMatrix12;
      s : double;
  begin
    result := false;

    // compute covariance matrix CtC := Ct * C (12x60 * 60x12 = 12x12)
    for i := 0 to 11 do
      for j := i to 11 do begin
        s := 0;
        for k := 0 to 59 do
          s := s + n[k][i] * n[k][j];
        CtC.arr[i][j] := s;
        CtC.arr[j][i] := s;
      end;

    // compute CtCi (CtC inverse)
    if not InvertMatrix12(CtC, CtCi) then
      exit;

    // compute matrix r[] := Ct * p (12x60 * 60x1 = 12x1)
    for i := 0 to 11 do begin
      s := 0;
      for k := 0 to 59 do
        s := s + n[k][i] * p[k];
      r[i] := s;
    end;

    // compute weights (CtCi[] * r[]) (12x12 * 12x1 = 12x1)
    for i := 0 to 11 do begin
      s := 0;
      for k := 0 to 11 do
        s := s + CtCi.arr[i][k] * r[k];
      a[i] := s;
    end;

    // windowPat64:
    // 00 01 02 03 04 05 06 07
    // 08 09 10 11 12 13 14 15
    // 16 17 18 19 20 21 22 23
    // 24 25 26 27_28 29 30 31
    // 32 33 34 35 36 37 38 39
    // 40 41 42 43 44 45 46 47
    // 48 49 50 51 52 53 54 55
    // 56 57 58 59 60 61 62 63

    // windowPat60:
    //    00 01 02 03 04 05
    // 06 07 08 09 10 11 12 13
    // 14 15 16 17 18 19 20 21
    // 22 23 24 25_26 27 28 29
    // 30 31 32 33 34 35 36 37
    // 38 39 40 41 42 43 44 45
    // 46 47 48 49 50 51 52 53
    //    54 55 56 57 58 59

    // calculate interpolated pixel
    final := round( q[25]                    * a[ 0] +
                    q[26]                    * a[ 1] +
                    q[33]                    * a[ 2] +
                    q[34]                    * a[ 3] +
                   (q[24] + q[25] + 1) div 2 * a[ 4] +
                   (q[26] + q[27] + 1) div 2 * a[ 5] +
                   (q[32] + q[33] + 1) div 2 * a[ 6] +
                   (q[34] + q[35] + 1) div 2 * a[ 7] +
                   (q[17] + q[25] + 1) div 2 * a[ 8] +
                   (q[18] + q[26] + 1) div 2 * a[ 9] +
                   (q[33] + q[41] + 1) div 2 * a[10] +
                   (q[34] + q[42] + 1) div 2 * a[11]   );
    if final > 255 then
      final := 255
    else
      if final < 0 then
        final := 0;

    output := final;
    result := true;
  end;

var dstPitch2, dstPitch3 : integer;
    dstLowPass, dstlp2 : TPAByte;
    x, y, u, v, temp, temp1, temp2 : integer;
    dst2, dstp, dstpp, dstn, dstnn : TPAByte;
    width2, height2 : integer;
    p, q : TA60Int;
    n : TA60x12Int;
    localPat1, localPat2 : array [0..11] of integer;
    windowPat1, windowPat2 : array [0..59] of integer;
    success : boolean;
    index : integer;
    weights : array [0..10] of double;
    weight1, weight2 : double;
    sum : double;
    f1, f2 : integer;
begin
  width2 := width shl 1;
  height2 := height shl 1;
  dstPitch2 := dstPitch * 2;
  dstPitch3 := dstPitch * 3;
  dstLowPass := pointer(LocalAlloc(LPTR, width2 * height2));

 // copy over original pixels
  dst2 := dst;
  for y := 0 to height - 1 do begin
    for x := 0 to width - 1 do
      dst2[x shl 1] := src[x];
  dst2 := pointer(NativeInt(dst2) + dstPitch2);
  src  := pointer(NativeInt(src ) + srcPitch);
  end;

 // interpolate areas where NEDI can't be used
  dst2 := dst;
  temp2 := width2 - 4;
  y := 0;
  while y < 11 do begin
    dst2[1] := (dst2[0] + dst2[2] + 1) shr 1;
    x := 3;
    while x < temp2 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2[x] := (dst2[x - 1] + dst2[x + 1] + 1) shr 1;
    inc(x, 2);
    dst2[x] := dst2[x - 1];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  temp1 := height2 - 12;
  temp2 := width2 - 4;
  while y < temp1 do begin
    dst2[1] := (dst2[0] + dst2[2] + 1) shr 1;
    x := 3;
    while x < 9 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    x := width2 - 9;
    while x < temp2 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2[x] := (dst2[x - 1] + dst2[x + 1] + 1) shr 1;
    inc(x, 2);
    dst2[x] := dst2[x - 1];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  temp2 := width2 - 4;
  while y < height2 do begin
    dst2[1] := (dst2[0] + dst2[2] + 1) shr 1;
    x := 3;
    while x < temp2 do begin
      dst2[x] := Interpolate(dst2[x - 3], dst2[x - 1], dst2[x + 1], dst2[x + 3]);
      inc(x, 2);
    end;
    dst2[x] := (dst2[x - 1] + dst2[x + 1] + 1) shr 1;
    inc(x, 2);
    dst2[x] := dst2[x - 1];
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    inc(y, 2);
  end;
  dst2  := pointer(NativeInt(dst ) + dstPitch);
  dstp  := pointer(NativeInt(dst2) - dstPitch);
  dstn  := pointer(NativeInt(dst2) + dstPitch);
  dstpp := pointer(NativeInt(dstp) - dstPitch2);
  dstnn := pointer(NativeInt(dstn) + dstPitch2);
  for x := 0 to width2 - 1 do
    dst2[x] := (dstp[x] + dstn[x] + 1) shr 1;
  dst2 := pointer(NativeInt(dst2) + dstPitch2);
  dstpp := dstp;
  dstp := dstn;
  dstn := dstnn;
  dstnn := pointer(NativeInt(dstnn) + dstPitch2);
  y := 3;
  while y < 9 do begin
    for x := 0 to width2 - 1 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstpp := dstp;
    dstp := dstn;
    dstn := dstnn;
    dstnn := pointer(NativeInt(dstnn) + dstPitch2);
    inc(y, 2);
  end;
  temp1 := height2 - 10;
  while y < temp1 do begin
    for x := 0 to 8 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    for x := width2 - 9 to width2 - 1 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstpp := dstp;
    dstp := dstn;
    dstn := dstnn;
    dstnn := pointer(NativeInt(dstnn) + dstPitch2);
    inc(y, 2);
  end;
  temp1 := height2 - 4;
  while y < temp1 do begin
    for x := 0 to width2 - 1 do
      dst2[x] := Interpolate(dstpp[x], dstp[x], dstn[x], dstnn[x]);
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstpp := dstp;
    dstp := dstn;
    dstn := dstnn;
    dstnn := pointer(NativeInt(dstnn) + dstPitch2);
    inc(y, 2);
  end;
  for x := 0 to width2 - 1 do
    dst2[x] := (dstp[x] + dstn[x] + 1) shr 1;
  dstp := dstn;
  dst2 := pointer(NativeInt(dst2) + dstPitch2);
  for x := 0 to width2 - 1 do
    dst2[x] := dstp[x];

  // NEDI
  weights[ 0] := Power(2, - 0.5 * 5 * 5);
  weights[ 1] := Power(2, - 0.5 * 4 * 4);
  weights[ 2] := Power(2, - 0.5 * 3 * 3);
  weights[ 3] := Power(2, - 0.5 * 2 * 2);
  weights[ 4] := Power(2, - 0.5 * 1 * 1);
  weights[ 5] := Power(2, - 0.5 * 0 * 0);
  weights[ 6] := Power(2, - 0.5 * 1 * 1);
  weights[ 7] := Power(2, - 0.5 * 2 * 2);
  weights[ 8] := Power(2, - 0.5 * 3 * 3);
  weights[ 9] := Power(2, - 0.5 * 4 * 4);
  weights[10] := Power(2, - 0.5 * 5 * 5);
  localPat1[ 0] := - 2 - dstPitch * 2;
  localPat1[ 1] := + 2 - dstPitch * 2;
  localPat1[ 2] := - 2 + dstPitch * 2;
  localPat1[ 3] := + 2 + dstPitch * 2;
  localPat1[ 4] := - 4 - dstPitch * 2;
  localPat1[ 5] := + 4 - dstPitch * 2;
  localPat1[ 6] := - 4 + dstPitch * 2;
  localPat1[ 7] := + 4 + dstPitch * 2;
  localPat1[ 8] := - 2 - dstPitch * 4;
  localPat1[ 9] := + 2 - dstPitch * 4;
  localPat1[10] := - 2 + dstPitch * 4;
  localPat1[11] := + 2 + dstPitch * 4;
  localPat2[ 0] := - 2 - dstPitch * 0;
  localPat2[ 1] := + 0 - dstPitch * 2;
  localPat2[ 2] := - 0 + dstPitch * 2;
  localPat2[ 3] := + 2 + dstPitch * 0;
  localPat2[ 4] := - 4 + dstPitch * 2;
  localPat2[ 5] := + 2 - dstPitch * 4;
  localPat2[ 6] := - 2 + dstPitch * 4;
  localPat2[ 7] := + 4 - dstPitch * 2;
  localPat2[ 8] := - 4 - dstPitch * 2;
  localPat2[ 9] := - 2 - dstPitch * 4;
  localPat2[10] := + 2 + dstPitch * 4;
  localPat2[11] := + 4 + dstPitch * 2;
  windowPat1[ 0] := - 5 - dstPitch * 7;
  windowPat1[ 1] := - 3 - dstPitch * 7;
  windowPat1[ 2] := - 1 - dstPitch * 7;
  windowPat1[ 3] := + 1 - dstPitch * 7;
  windowPat1[ 4] := + 3 - dstPitch * 7;
  windowPat1[ 5] := + 5 - dstPitch * 7;
  windowPat1[ 6] := - 7 - dstPitch * 5;
  windowPat1[ 7] := - 5 - dstPitch * 5;
  windowPat1[ 8] := - 3 - dstPitch * 5;
  windowPat1[ 9] := - 1 - dstPitch * 5;
  windowPat1[10] := + 1 - dstPitch * 5;
  windowPat1[11] := + 3 - dstPitch * 5;
  windowPat1[12] := + 5 - dstPitch * 5;
  windowPat1[13] := + 7 - dstPitch * 5;
  windowPat1[14] := - 7 - dstPitch * 3;
  windowPat1[15] := - 5 - dstPitch * 3;
  windowPat1[16] := - 3 - dstPitch * 3;
  windowPat1[17] := - 1 - dstPitch * 3;
  windowPat1[18] := + 1 - dstPitch * 3;
  windowPat1[19] := + 3 - dstPitch * 3;
  windowPat1[20] := + 5 - dstPitch * 3;
  windowPat1[21] := + 7 - dstPitch * 3;
  windowPat1[22] := - 7 - dstPitch * 1;
  windowPat1[23] := - 5 - dstPitch * 1;
  windowPat1[24] := - 3 - dstPitch * 1;
  windowPat1[25] := - 1 - dstPitch * 1;
  windowPat1[26] := + 1 - dstPitch * 1;
  windowPat1[27] := + 3 - dstPitch * 1;
  windowPat1[28] := + 5 - dstPitch * 1;
  windowPat1[29] := + 7 - dstPitch * 1;
  windowPat1[30] := - 7 + dstPitch * 1;
  windowPat1[31] := - 5 + dstPitch * 1;
  windowPat1[32] := - 3 + dstPitch * 1;
  windowPat1[33] := - 1 + dstPitch * 1;
  windowPat1[34] := + 1 + dstPitch * 1;
  windowPat1[35] := + 3 + dstPitch * 1;
  windowPat1[36] := + 5 + dstPitch * 1;
  windowPat1[37] := + 7 + dstPitch * 1;
  windowPat1[38] := - 7 + dstPitch * 3;
  windowPat1[39] := - 5 + dstPitch * 3;
  windowPat1[40] := - 3 + dstPitch * 3;
  windowPat1[41] := - 1 + dstPitch * 3;
  windowPat1[42] := + 1 + dstPitch * 3;
  windowPat1[43] := + 3 + dstPitch * 3;
  windowPat1[44] := + 5 + dstPitch * 3;
  windowPat1[45] := + 7 + dstPitch * 3;
  windowPat1[46] := - 7 + dstPitch * 5;
  windowPat1[47] := - 5 + dstPitch * 5;
  windowPat1[48] := - 3 + dstPitch * 5;
  windowPat1[49] := - 1 + dstPitch * 5;
  windowPat1[50] := + 1 + dstPitch * 5;
  windowPat1[51] := + 3 + dstPitch * 5;
  windowPat1[52] := + 5 + dstPitch * 5;
  windowPat1[53] := + 7 + dstPitch * 5;
  windowPat1[54] := - 5 + dstPitch * 7;
  windowPat1[55] := - 3 + dstPitch * 7;
  windowPat1[56] := - 1 + dstPitch * 7;
  windowPat1[57] := + 1 + dstPitch * 7;
  windowPat1[58] := + 3 + dstPitch * 7;
  windowPat1[59] := + 5 + dstPitch * 7;
  windowPat2[ 0] := - 6 - dstPitch * 1;
  windowPat2[ 1] := - 5 - dstPitch * 2;
  windowPat2[ 2] := - 4 - dstPitch * 3;
  windowPat2[ 3] := - 3 - dstPitch * 4;
  windowPat2[ 4] := - 2 - dstPitch * 5;
  windowPat2[ 5] := - 1 - dstPitch * 6;
  windowPat2[ 6] := - 6 + dstPitch * 1;
  windowPat2[ 7] := - 5 - dstPitch * 0;
  windowPat2[ 8] := - 4 - dstPitch * 1;
  windowPat2[ 9] := - 3 - dstPitch * 2;
  windowPat2[10] := - 2 - dstPitch * 3;
  windowPat2[11] := - 1 - dstPitch * 4;
  windowPat2[12] := - 0 - dstPitch * 5;
  windowPat2[13] := + 1 - dstPitch * 6;
  windowPat2[14] := - 5 + dstPitch * 2;
  windowPat2[15] := - 4 + dstPitch * 1;
  windowPat2[16] := - 3 - dstPitch * 0;
  windowPat2[17] := - 2 - dstPitch * 1;
  windowPat2[18] := - 1 - dstPitch * 2;
  windowPat2[19] := - 0 - dstPitch * 3;
  windowPat2[20] := + 1 - dstPitch * 4;
  windowPat2[21] := + 2 - dstPitch * 5;
  windowPat2[22] := - 4 + dstPitch * 3;
  windowPat2[23] := - 3 + dstPitch * 2;
  windowPat2[24] := - 2 + dstPitch * 1;
  windowPat2[25] := - 1 - dstPitch * 0;
  windowPat2[26] := - 0 - dstPitch * 1;
  windowPat2[27] := + 1 - dstPitch * 2;
  windowPat2[28] := + 2 - dstPitch * 3;
  windowPat2[29] := + 3 - dstPitch * 4;
  windowPat2[30] := - 3 + dstPitch * 4;
  windowPat2[31] := - 2 + dstPitch * 3;
  windowPat2[32] := - 1 + dstPitch * 2;
  windowPat2[33] := - 0 + dstPitch * 1;
  windowPat2[34] := + 1 - dstPitch * 0;
  windowPat2[35] := + 2 - dstPitch * 1;
  windowPat2[36] := + 3 - dstPitch * 2;
  windowPat2[37] := + 4 - dstPitch * 3;
  windowPat2[38] := - 2 + dstPitch * 5;
  windowPat2[39] := - 1 + dstPitch * 4;
  windowPat2[40] := - 0 + dstPitch * 3;
  windowPat2[41] := + 1 + dstPitch * 2;
  windowPat2[42] := + 2 + dstPitch * 1;
  windowPat2[43] := + 3 - dstPitch * 0;
  windowPat2[44] := + 4 - dstPitch * 1;
  windowPat2[45] := + 5 - dstPitch * 2;
  windowPat2[46] := - 1 + dstPitch * 6;
  windowPat2[47] := - 0 + dstPitch * 5;
  windowPat2[48] := + 1 + dstPitch * 4;
  windowPat2[49] := + 2 + dstPitch * 3;
  windowPat2[50] := + 3 + dstPitch * 2;
  windowPat2[51] := + 4 + dstPitch * 1;
  windowPat2[52] := + 5 - dstPitch * 0;
  windowPat2[53] := + 6 - dstPitch * 1;
  windowPat2[54] := + 1 + dstPitch * 6;
  windowPat2[55] := + 2 + dstPitch * 5;
  windowPat2[56] := + 3 + dstPitch * 4;
  windowPat2[57] := + 4 + dstPitch * 3;
  windowPat2[58] := + 5 + dstPitch * 2;
  windowPat2[59] := + 6 + dstPitch * 1;

  // low pass filter
  dst2 := pointer(NativeInt(dst));
  dstlp2 := pointer(NativeInt(dstLowPass));
  y := 0;
  while y < height2 do begin
    x := 0;
    while x < width2 do begin
      sum := 0;
      weight2 := 0;
      for f1 := -5 to +5 do
        if (x + f1 * 2 >= 0) and (x + f1 * 2 < width2) then
          for f2 := -5 to +5 do 
            if (y + f2 * 2 >= 0) and (y + f2 * 2 < height2) then begin
              weight1 := weights[f1 + 5] * weights[f2 + 5];
              weight2 := weight2 + weight1;
              sum := sum + dst2[x + f1 * 2 + f2 * dstPitch2] * weight1;
            end;
      u := Round(sum / weight2);
      if u < 0 then
        dstlp2[x] := 0
      else
        if u > 255 then
          dstlp2[x] := 255
        else
          dstlp2[x] := u;
      inc(x, 2);
    end;
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstlp2 := pointer(NativeInt(dstlp2) + dstPitch2);
    inc(y, 2);
  end;

  // first interpolation step
  dst2 := pointer(NativeInt(dst) + 9 * dstPitch);
  dstlp2 := pointer(NativeInt(dstLowPass) + 9 * dstPitch);
  temp1 := height2 - 9;
  temp2 := width2 - 9;
  y := 9;
  while y < temp1 do begin
    x := 9;
    while x < temp2 do begin
      success := false;
      if CheckVariance([dstlp2[x + windowPat1[25]], dstlp2[x + windowPat1[26]], dstlp2[x + windowPat1[33]], dstlp2[x + windowPat1[34]]]) then begin
        for u := 0 to 59 do begin
          index := x + windowPat1[u];
          p[u] := dstlp2[index];
          q[u] := dst2[index];
          for v := 0 to 11 do
            try
              n[u][v] := dstlp2[x + windowPat1[u] + localPat1[v]];
            except
              n[u][v] := 0;
            end;
        end;
        success := NEDI_8x8(p, q, n, dst2[x]);
      end;
      if not success then
        dst2[x] := Interpolate(Interpolate(dst2[x + windowPat1[16]], dst2[x + windowPat1[17]], dst2[x + windowPat1[18]], dst2[x + windowPat1[19]]),
                               Interpolate(dst2[x + windowPat1[24]], dst2[x + windowPat1[25]], dst2[x + windowPat1[26]], dst2[x + windowPat1[27]]),
                               Interpolate(dst2[x + windowPat1[32]], dst2[x + windowPat1[33]], dst2[x + windowPat1[34]], dst2[x + windowPat1[35]]),
                               Interpolate(dst2[x + windowPat1[40]], dst2[x + windowPat1[41]], dst2[x + windowPat1[42]], dst2[x + windowPat1[43]]));
      inc(x, 2);
    end;
    dst2 := pointer(NativeInt(dst2) + dstPitch2);
    dstlp2 := pointer(NativeInt(dstlp2) + dstPitch2);
    inc(y, 2);
  end;

  // low pass filter
  dst2 := pointer(NativeInt(dst));
  dstlp2 := pointer(NativeInt(dstLowPass));
  for y := 0 to height2 - 1 do begin
    x := 0 + y and 1;
    while x < width2 do begin
      sum := 0;
      weight2 := 0;
      for f1 := -5 to +5 do
        for f2 := -5 to +5 do
          if (x + f1 + f2 >= 0) and (x + f1 + f2 < width2 ) and
             (y - f1 + f2 >= 0) and (y - f1 + f2 < height2) then begin
            weight1 := weights[f1 + 5] * weights[f2 + 5];
            weight2 := weight2 + weight1;
            sum := sum + dst2[x + f1 * (1 - dstPitch) + f2 * (1 + dstPitch)] * weight1;
          end;
      u := Round(sum / weight2);
      if u < 0 then
        dstlp2[x] := 0
      else
        if u > 255 then
          dstlp2[x] := 255
        else
          dstlp2[x] := u;
      inc(x, 2);
    end;
    dst2 := pointer(NativeInt(dst2) + dstPitch);
    dstlp2 := pointer(NativeInt(dstlp2) + dstPitch);
  end;

  // second interpolation step
  dst2 := pointer(NativeInt(dst) + 9 * dstPitch);
  dstlp2 := pointer(NativeInt(dstLowPass) + 9 * dstPitch);
  temp1 := height2 - 9;
  temp2 := width2 - 9;
  for y := 9 to temp1 - 1 do begin
    temp := y and 1;
    x := 9 + temp;
    while x < temp2 do begin
      success := false;
      if CheckVariance([dstlp2[x + windowPat2[25]], dstlp2[x + windowPat2[26]], dstlp2[x + windowPat2[33]], dstlp2[x + windowPat2[34]]]) then begin
        for u := 0 to 59 do begin
          index := x + windowPat2[u];
          p[u] := dstlp2[index];
          q[u] := dst2[index];
          for v := 0 to 11 do
            try
              n[u][v] := dstlp2[x + windowPat2[u] + localPat2[v]];
            except
              n[u][v] := 0;
            end;
        end;
        success := NEDI_8x8(p, q, n, dst2[x]);
      end;
      if not success then
        dst2[x] := Interpolate(Interpolate(dst2[x + windowPat2[16]], dst2[x + windowPat2[17]], dst2[x + windowPat2[18]], dst2[x + windowPat2[19]]),
                               Interpolate(dst2[x + windowPat2[24]], dst2[x + windowPat2[25]], dst2[x + windowPat2[26]], dst2[x + windowPat2[27]]),
                               Interpolate(dst2[x + windowPat2[32]], dst2[x + windowPat2[33]], dst2[x + windowPat2[34]], dst2[x + windowPat2[35]]),
                               Interpolate(dst2[x + windowPat2[40]], dst2[x + windowPat2[41]], dst2[x + windowPat2[42]], dst2[x + windowPat2[43]]));
      inc(x, 2);
    end;
    dst2 := pointer(NativeInt(dst2) + dstPitch);
    dstlp2 := pointer(NativeInt(dstlp2) + dstPitch);
  end;

  LocalFree(NativeUInt(dstLowPass));
end;

procedure Nedi(bmp: TBitmap);
var srcY, srcCb, srcCr, dstY, dstCb, dstCr : TPAByte;
    i1, i2 : integer;
    pixel : integer;
    red, green, blue : integer;
    y, cb, cr : single;
begin
  srcY  := pointer(LocalAlloc(LPTR, bmp.Width * bmp.Height));
  srcCb := pointer(LocalAlloc(LPTR, bmp.Width * bmp.Height));
  srcCr := pointer(LocalAlloc(LPTR, bmp.Width * bmp.Height));
  dstY  := pointer(LocalAlloc(LPTR, bmp.Width * bmp.Height * 4));
  dstCb := pointer(LocalAlloc(LPTR, bmp.Width * bmp.Height * 4));
  dstCr := pointer(LocalAlloc(LPTR, bmp.Width * bmp.Height * 4));
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      pixel := bmp.Canvas.Pixels[i1, i2];
      red := pixel and $ff;
      green := (pixel shr 8) and $ff;
      blue := (pixel shr 16) and $ff;
      y := red * 0.299 + green * 0.587 + blue * 0.114;
      cb := $80 + red * -0.1687358916478555 + green * -0.3312641083521445 + blue * 0.5;
      cr := $80 + red * 0.5 + green * -0.4186875891583452 + blue * -0.0813124108416548;
      y := round(y);
      cb := round(cb);
      cr := round(cr);
      if y < 0 then
        y := 0
      else
        if y > 255 then
          y := 255;
      if cb < 0 then
        cb := 0
      else
        if cb > 255 then
          cb := 255;
      if cr < 0 then
        cr := 0
      else
        if cr >= 255 then
          cr := 255;
      srcY [i2 * bmp.Width + i1] := trunc(y);
      srcCb[i2 * bmp.Width + i1] := trunc(cb);
      srcCr[i2 * bmp.Width + i1] := trunc(cr);
    end;

  ZhaoXinLiPlane(srcY,  bmp.Width, bmp.Height, bmp.Width, dstY,  bmp.Width * 2);
  ZhaoXinLiPlane(srcCb, bmp.Width, bmp.Height, bmp.Width, dstCb, bmp.Width * 2);
  ZhaoXinLiPlane(srcCr, bmp.Width, bmp.Height, bmp.Width, dstCr, bmp.Width * 2);

  bmp.Width := bmp.Width * 2;
  bmp.Height := bmp.Height * 2;
  for i1 := 0 to bmp.Width - 1 do
    for i2 := 0 to bmp.Height - 1 do begin
      y := dstY [i2 * bmp.Width + i1];
      cb := dstCb[i2 * bmp.Width + i1] - $80;
      cr := dstCr[i2 * bmp.Width + i1] - $80;
      red := round(y + cr * 1.402);
      green := round(y + cb * -0.3441362862010221 + cr * -0.7141362862010221);
      blue := round(y + cb * 1.772);
      if red < 0 then
        red := 0                           
      else
        if red > 255 then
          red := 255;
      if green < 0 then
        green := 0
      else
        if green > 255 then
          green := 255;
      if blue < 0 then
        blue := 0
      else
        if blue > 255 then
          blue := 255;
      bmp.Canvas.Pixels[i1, i2] := blue shl 16 + green shl 8 + red;
    end;
end;

procedure MakeTransparentBmp;
var bufp   : TPACardinal;
    buf    : TPACardinal;
    bufn   : TPACardinal;
    c1     : dword;
    bmp1   : TBitmap;
    i1, i2 : integer;
    r, g, b, y : dword;
begin
  bmp1 := TBitmap.Create;
  bmp1.LoadFromFile('c:\desktop\org.bmp');
  bmp1.PixelFormat := pf32bit;
  for i1 := 0 to bmp1.Width - 1 do
    for i2 := 0 to bmp1.Height - 1 do begin
      buf := bmp1.Scanline[i2];
      if i2 > 0 then
        bufp := bmp1.Scanline[i2 - 1]
      else
        bufp := buf;
      if i2 < bmp1.Height - 1 then
        bufn := bmp1.Scanline[i2 + 1]
      else
        bufn := buf;
      c1 := buf[i1];
      if (c1 <> 0) and
         ( (bufp[i1] = $ffffff) or (bufn[i1] = $ffffff) or
           ((i1 > 0) and (bufp[i1 - 1] = $ffffff)) or ((i1 < bmp1.Width - 1) and (bufp[i1 + 1] = $ffffff)) or
           ((i1 > 0) and (buf [i1 - 1] = $ffffff)) or ((i1 < bmp1.Width - 1) and (buf [i1 + 1] = $ffffff)) or
           ((i1 > 0) and (bufn[i1 - 1] = $ffffff)) or ((i1 < bmp1.Width - 1) and (bufn[i1 + 1] = $ffffff))    ) then begin
        if c1 <> $ffffff then begin
          r := c1 and $ff;
          g := (c1 shr 8) and $ff;
          b := (c1 shr 16) and $ff;
          y := $80;
          r := (r + 1) div 2;
          g := (g + 1) div 2;
          b := (b + 1) div 2;
          c1 := (y shl 24) or (b shl 16) or (g shl 8) or r;
        end;
      end else
        c1 := $ff000000 or c1;
      buf[i1] := c1;
    end;
  bmp1.SaveToFile('c:\desktop\transparent.bmp');
  ExitProcess(0);
end;

(*
function SavePngFile(width, height, bitdepth: integer; alpha: boolean; pixels: pointer; const pngFile: AnsiString) : boolean;

  function SwapDword(dw: dword) : dword;
  begin
    result := (dw shr 24)               +
              (dw shr  8) and $0000ff00 +
              (dw shl  8) and $00ff0000 +
              (dw shl 24);
  end;

  function SwapWord(w: word) : word;
  begin
    result := (w shr 8) + (w shl 8) and $ff00;
  end;

  function GetPngHeader : AnsiString;
  begin
    SetLength(result, 13);
    TPACardinal(result)[0] := SwapDword(width);
    TPACardinal(result)[1] := SwapDword(height);
    TPAByte(result)[8] := bitdepth;
    if alpha then
      TPAByte(result)[9] := 6
    else
      TPAByte(result)[9] := 2;
  end;

  function CompressBitmap(buf: pointer; size: integer) : AnsiString;

    function Adler32(buf: PByte; len: integer) : dword;
    const Base = dword(65521); // largest prime smaller than 65536
          NMAX = 3854;         // Code with signed 32 bit integer
    var c1, c2 : dword;
        i1     : integer;
    begin
      c1 := 1;
      c2 := 0;
      while len > 0 do begin
        if len < NMAX then
             i1 := len
        else i1 := NMAX;
        dec(len, i1);
        while i1 > 0 do begin
          inc(c1, buf^);
          inc(c2, c1);
          inc(buf);
          dec(i1);
        end;
        c1 := c1 mod Base;
        c2 := c2 mod Base;
      end;
      result := (c2 shl 16) or c1;
    end;

  var i1   : integer;
      pdw  : ^dword;
  begin
    SetLength(result, size * 11 div 10 + 12);
    i1 := Compress(buf, pointer(result), size, length(result));
    if i1 > 0 then begin
      SetLength(result, i1);
      result := #$78#$01 + result + 'adlr';
      pointer(pdw) := PAnsiChar(result) + length(result) - 4;
      pdw^ := SwapDword(Adler32(buf, size));
    end;
  end;

  procedure AddPngPart(var png: AnsiString; const name, data: AnsiString);
  var pdw : ^dword;
      crc : dword;
  begin
    png := png + 'len ' + name + data + 'crc ';
    pointer(pdw) := PAnsiChar(png) + Length(png) - 4 - Length(data) - 8;
    crc := not UpdateCrc32($ffffffff, pointer(NativeUInt(pdw) + 4)^, 4 + Length(data));
    pdw^ := SwapDword(Length(data));
    pointer(pdw) := PAnsiChar(png) + Length(png) - 4;
    pdw^ := SwapDword(crc);
  end;

var pngStr     : AnsiString;
    channels   : integer;
    size       : integer;
    buf        : AnsiString;
    src        : TPWord;
    dst        : TPWord;
    ix, iy, ic : integer;
    fh         : THandle;
    c1         : dword;
begin
  result := false;
  if alpha then
    channels := 4
  else
    channels := 3;
  size := width * height * (bitdepth div 8) * channels + height;
  SetLength(buf, size);
  src := pixels;
  dst := pointer(buf);
  for iy := 0 to height - 1 do begin
    TPByte(dst)^ := 0;
    NativeUInt(dst) := NativeUInt(dst) + 1;
    for ix := 0 to width - 1 do
      for ic := 0 to channels - 1 do begin
        dst^ := SwapWord(src^);
        inc(src);
        inc(dst);
      end;
  end;
  pngStr := #$89#$50#$4e#$47#$0d#$0a#$1a#$0a;
  AddPngPart(pngStr, 'IHDR', GetPngHeader);
  AddPngPart(pngStr, 'IDAT', CompressBitmap(pointer(buf), size));
  AddPngPart(pngStr, 'IEND', '');
  fh := CreateFileA(PAnsiChar(pngFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  if fh <> INVALID_HANDLE_VALUE then begin
    result := WriteFile(fh, pointer(pngStr)^, Length(pngStr), c1, nil) and (c1 = dword(Length(pngStr)));
    CloseHandle(fh);
  end;
end;
*)

end.
