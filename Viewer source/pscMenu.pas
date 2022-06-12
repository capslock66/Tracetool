{*****************************************************************************
  Author         : Serhiy Perevoznyk
  Description    : Custom menu drawing

 *****************************************************************************}


unit PSCMenu;

interface

uses
  system.UITypes, Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls,  Forms,
  Menus,  StdCtrls, Commctrl  ;

type
  TPSCMenu = class(TComponent)
  private
    FActive: boolean;
    FForm: TScrollingWinControl;
    FFont: TFont;
    FColor: TColor;
    FIconBackColor: TColor;
    FMenuBarColor: TColor;
    FCheckedColor: TColor;
    FSeparatorColor: TColor;
    FSelectBorderColor: TColor;
    FSelectColor: TColor;
    FDisabledColor: TColor;
    FSelectFontColor: TColor;
    FIconWidth: integer;
    FDrawSelect: boolean;
    FUseSystemColors: boolean;

    FFColor : TColor;
    FFIconBackColor : TColor;
    FFSelectColor : TColor;
    FFSelectBorderColor : TColor;
    FFSelectFontColor : TColor;
    FCheckedAreaColor : TColor;
    FCheckedAreaSelectColor : TColor;
    FFCheckedColor: TColor;
    FFMenuBarColor : TColor;
    FFDisabledColor : TColor;
    FFSeparatorColor : TColor;
    FMenuBorderColor : TColor;
    FMenuShadowColor: TColor;

    Is16Bit: boolean;
    FOverrideOwnerDraw: boolean;
    FGradient: boolean;
    FGrayLevel: byte;
    FDimLevel: byte;
    procedure SetActive(const Value: boolean);
    procedure SetForm(const Value: TScrollingWinControl);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetIconBackColor(const Value: TColor);
    procedure SetMenuBarColor(const Value: TColor);
    procedure SetCheckedColor(const Value: TColor);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetSelectColor(const Value: TColor);
    procedure SetSelectBorderColor(const Value: TColor);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetSelectFontColor(const Value: TColor);
    procedure SetIconWidth(const Value: integer);
    procedure SetDrawSelect(const Value: boolean);
    procedure SetUseSystemColors(const Value: boolean);
    procedure SetOverrideOwnerDraw(const Value: boolean);
    procedure SetGradient(const Value: boolean);
  protected
    procedure InitItems(wForm: TWinControl; Enable, Update: boolean);
    procedure DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure MenueDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure ActivateMenuItem(MenuItem: TMenuItem);
    procedure SetGlobalColor(ACanvas: TCanvas);
    procedure DrawTopMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; BckColor:Tcolor; IsRightToLeft: boolean);
    procedure DrawCheckedItem(FMenuItem: TMenuItem; Selected, Enabled, HasImgLstBitmap: boolean; ACanvas: TCanvas; CheckedRect: TRect);
    procedure DrawTheText(Sender: TObject; txt, ShortCuttext: string;
       ACanvas: TCanvas; TextRect: TRect;
       Selected, Enabled, Default, TopMenu, IsRightToLeft: boolean;
       var TxtFont: TFont; TextFormat: integer);
    procedure DrawIcon(Sender: TObject; ACanvas: TCanvas; B: TBitmap;
     IconRect: Trect; Hot, Selected, Enabled, Checked, FTopMenu,
     IsRightToLeft: boolean);
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    function GetImageExtent(MenuItem: TMenuItem): TPoint;
    function TopMenuFontColor(ACanvas: TCanvas; Color: TColor): TColor;
    procedure DrawGradient(ACanvas: TCanvas; ARect: TRect;
     IsRightToLeft: boolean);
    procedure DrawWindowBorder(hWnd: HWND; IsRightToLeft: boolean);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Form: TScrollingWinControl read FForm write SetForm;
  published
    property DimLevel: Byte read FDimLevel write FDimLevel;
    property GrayLevel: Byte read FGrayLevel write FGrayLevel;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
    property IconBackColor: TColor read FIconBackColor write SetIconBackColor;
    property MenuBarColor: TColor read FMenuBarColor write SetMenuBarColor;
    property SelectColor: TColor read FSelectColor write SetSelectColor;
    property SelectBorderColor: TColor read FSelectBorderColor   write SetSelectBorderColor;
    property SelectFontColor: TColor read FSelectFontColor   write SetSelectFontColor;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor;
    property SeparatorColor: TColor read FSeparatorColor   write SetSeparatorColor;
    property CheckedColor: TColor read FCheckedColor write SetCheckedColor;
    property IconWidth: integer read FIconWidth write SetIconWidth;
    property DrawSelect: boolean read FDrawSelect write SetDrawSelect;
    property UseSystemColors: boolean read FUseSystemColors   write SetUseSystemColors;
    property OverrideOwnerDraw: boolean read FOverrideOwnerDraw  write SetOverrideOwnerDraw;
    property Gradient: boolean read FGradient write SetGradient;
    property Active: boolean read FActive write SetActive;
  end;


implementation

uses unt_utility ;

{ TPSCMenu}

constructor TPSCMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  GetSystemMenuFont(FFont);
  FForm := Owner as TScrollingWinControl;
  FActive := false;
  FUseSystemColors := true;
  FColor := clBtnFace;
  FIconBackColor := clBtnFace;
  FSelectColor := clHighlight;
  FSelectBorderColor := clHighlight;
  FMenuBarColor := clBtnFace;
  FDisabledColor := clInactiveCaption;
  FSeparatorColor := clBtnFace;
  FCheckedColor := clHighlight;
  FSelectFontColor := FFont.Color;
  FGrayLevel := 10;
  FDimLevel := 30;
  FIconWidth := 24;
  FDrawSelect := true;
end;

destructor TPSCMenu.Destroy;
begin
  InitItems(FForm, false, false);
  FFont.Free;
  inherited;
end;


{to check for new sub items}
procedure TPSCMenu.ActivateMenuItem(MenuItem: TMenuItem);

  procedure Activate(MenuItem: TMenuItem);
  begin
    if (MenuItem.Tag <> 999) then
    if addr(MenuItem.OnDrawItem) <> addr(TPSCMenu.DrawItem) then
    begin
      if (not assigned(MenuItem.OnDrawItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnDrawItem := DrawItem;
      if (not assigned(MenuItem.OnMeasureItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnMeasureItem := MeasureItem;
    end
  end;

var
  i, j: integer;
begin
  Activate(MenuItem);
  for i := 0 to MenuItem.Parent.Count -1 do
  begin
    Activate(MenuItem.Parent.Items[i]);
    for j := 0 to MenuItem.Parent.Items[i].Count - 1 do
      ActivateMenuItem(MenuItem.Parent.Items[i].Items[j]);
  end;
end;

procedure TPSCMenu.InitItems(wForm: TWinControl; Enable, Update: boolean );

  procedure Activate(MenuItem: TMenuItem);
  begin
    if Enable then
    begin
      if (MenuItem.Tag <> 999) then
      begin
        if (not assigned(MenuItem.OnDrawItem)) or (FOverrideOwnerDraw) then
          MenuItem.OnDrawItem := DrawItem;
        if (not assigned(MenuItem.OnMeasureItem)) or (FOverrideOwnerDraw) then
          MenuItem.OnMeasureItem := MeasureItem;
      end;
    end
    else
    begin
      if addr(MenuItem.OnDrawItem) = addr(TPSCMenu.DrawItem) then
        MenuItem.OnDrawItem := nil;
      if addr(MenuItem.OnMeasureItem) = addr(TPSCMenu.MeasureItem) then
        MenuItem.OnMeasureItem := nil;
    end;
  end;

  procedure ItrateMenu(MenuItem: TMenuItem);
  var
    i: integer;
  begin
    Activate(MenuItem);
    for i := 0 to MenuItem.Count - 1 do
      ItrateMenu(MenuItem.Items[i]);
  end;

var
  i, x: integer;
  Comp: TComponent;

begin
  if (csDestroying in wForm.ComponentState) then
   Exit;
  for i := 0 to wForm.ComponentCount - 1 do
  begin
    Comp := wForm.Components[i];

    if (Comp is TMainMenu) and  (TMainMenu(Comp).Tag <> 999)then
    begin
      for x := 0 to TMainMenu(Comp).Items.Count - 1 do
      begin
        TMainMenu(Comp).OwnerDraw := Enable;
        Activate(TMainMenu(Comp).Items[x]);
        ItrateMenu(TMainMenu(Comp).Items[x]);
      end;
    end;

    if (Comp is TPopupMenu)  then
    begin
      for x := 0 to TPopupMenu(Comp).Items.Count - 1 do
      begin
        TPopupMenu(Comp).OwnerDraw := Enable;
        Activate(TPopupMenu(Comp).Items[x]);
        ItrateMenu(TPopupMenu(Comp).Items[x]);
      end;
    end;
  end;
end;

procedure TPSCMenu.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
begin
  if FActive then
    MenueDrawItem(Sender, ACanvas, ARect, Selected);
end;



function TPSCMenu.GetImageExtent(MenuItem: TMenuItem): TPoint;
var
  HasImgLstBitmap: boolean;
  B: TBitmap;
  FTopMenu: boolean;
begin
  FTopMenu := false;
  B := TBitmap.Create;
  B.Width := 0;
  B.Height := 0;
  Result.x := 0;
  Result.Y := 0;
  HasImgLstBitmap := false;

 { if (FForm is TForm) and ((FForm as TForm).Menu <> nil) then
    if MenuItem.GetParentComponent.Name = (FForm as TForm).Menu.Name then
  }
   if MenuItem.GetParentComponent is TMainMenu then
    begin
      FTopMenu := true;
  //    if (FForm as TForm).Menu.Images <> nil then
        if MenuItem.ImageIndex <> -1 then
          HasImgLstBitmap := true;

    end;

  if (MenuItem.Parent.GetParentMenu.Images <> nil)
  or (MenuItem.Parent.SubMenuImages <> nil)
  then
  begin
    if MenuItem.ImageIndex <> -1 then
      HasImgLstBitmap := true
    else
      HasImgLstBitmap := false;
  end;

  if HasImgLstBitmap then
  begin
    if MenuItem.Parent.SubMenuImages <> nil then
      MenuItem.Parent.SubMenuImages.GetBitmap(MenuItem.ImageIndex, B)
    else
      MenuItem.Parent.GetParentMenu.Images.GetBitmap(MenuItem.ImageIndex, B)
  end
  else
    if MenuItem.Bitmap.Width > 0 then
      B.Assign(TBitmap(MenuItem.Bitmap));

  Result.x := B.Width;
  Result.Y := B.Height;

  if not FTopMenu then
    if Result.x < FIconWidth then
      Result.x := FIconWidth;

  B.Free;
end;

procedure TPSCMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  s: string;
  W, H: integer;
  P: TPoint;
  IsLine: boolean;
  OSVersionInfo: TOSVersionInfo;
begin
  if FActive then
  begin
    S := Trim(TMenuItem(Sender).Caption);

    if S = '-' then IsLine := true else IsLine := false;
    if IsLine then
      S := '';

    if Trim(ShortCutToText(TMenuItem(Sender).ShortCut)) <> '' then
      S := S + ShortCutToText(TMenuItem(Sender).ShortCut) + 'WWW';

    ACanvas.Font.Assign(FFont);
    W := ACanvas.TextWidth(s);
    Inc(W, 5);
    if pos('&', s) > 0 then
      W := W - ACanvas.TextWidth('&');

    P := GetImageExtent(TMenuItem(Sender));
    if P.X > 0 then
      W := W + P.x ;


    //Add 8 pixels for win2k
    if (FForm is TForm) and ((FForm as TForm).Menu <> nil) then
      if TMenuItem(Sender).GetParentComponent.Name = (FForm as TForm).Menu.Name then
      begin
        GetVersionEx(OSVersionInfo);
        if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
          Inc(W, 8);
      end;

    if Width < W then
      Width := W;

    if IsLine then
      Height := 4
    else
    begin
      H := ACanvas.TextHeight(s) + Round(ACanvas.TextHeight(s) * 0.75);
      if P.y + 6 > H then
        H := P.y + 6;

      if Height < H then
        Height := H;
    end;
  end;

end;

procedure TPSCMenu.MenueDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
var
  txt: string;
  B: TBitmap;
  IconRect, TextRect, CheckedRect: TRect;
  i, X1, X2: integer;
  TextFormat: integer;
  HasImgLstBitmap: boolean;
  HasBitmap: boolean;
  FMenuItem: TMenuItem;
  FMenu: TMenu;
  FTopMenu: boolean;
  IsLine: boolean;
  ImgListHandle: HImageList;  {Commctrl.pas}
  ImgIndex: integer;
  ItemBitmap : TBitmap;
  FCanvas : TCanvas;
  FRect : TRect;
begin
  ItemBitmap := TBitmap.Create;
  ItemBitmap.Width := ARect.Right - ARect.Left;
  ItemBitmap.Height := ARect.Bottom - ARect.Top;
  FCanvas := ItemBitmap.Canvas;
  FRect.Left := 0;
  FRect.Top := 0;
  FRect.Right := ItemBitmap.Width;
  FRect.Bottom := ItemBitmap.Height;

  FTopMenu := false;
  FMenuItem := TMenuItem(Sender);

  SetGlobalColor(FCanvas);

  if FMenuItem.Caption = '-' then IsLine := true else IsLine := false;

  FMenu := FMenuItem.Parent.GetParentMenu;

  if FMenu is TMainMenu then
    for i := 0 to FMenuItem.GetParentMenu.Items.Count - 1 do
      if FMenuItem.GetParentMenu.Items[i] = FMenuItem then
      begin
        FTopMenu := True;
        break;
      end;


  FCanvas.Font.Assign(FFont);

  Inc(FRect.Bottom, 1);
  TextRect := FRect;
  txt := ' ' + FMenuItem.Caption;

  B := TBitmap.Create;
  HasBitmap := false;
  HasImgLstBitmap := false;


  if (FMenuItem.Parent.GetParentMenu.Images <> nil)
  or (FMenuItem.Parent.SubMenuImages <> nil)
  then
  begin
    if FMenuItem.ImageIndex <> -1 then
      HasImgLstBitmap := true
    else
      HasImgLstBitmap := false;
  end;

  if FMenuItem.Bitmap.Width  > 0 then
    HasBitmap := true;

  if HasBitmap then
    begin
      B.Width := FMenuItem.Bitmap.Width;
      B.Height := FMenuItem.Bitmap.Height;

      B.Canvas.CopyRect (Rect(0, 0, B.Width, B.Height), FMenuItem.Bitmap.Canvas,
                         Rect(0, 0, B.Width, B.Height));
    end;


  if HasImgLstBitmap then
  begin
    if FMenuItem.Parent.SubMenuImages <> nil then
    begin
      ImgListHandle := FMenuItem.Parent.SubMenuImages.Handle;
      ImgIndex := FMenuItem.ImageIndex;

      B.Width := FMenuItem.Parent.SubMenuImages.Width;
      B.Height := FMenuItem.Parent.SubMenuImages.Height;
      B.Canvas.Brush.Color := FCanvas.Brush.Color;
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(ImgListHandle, ImgIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

    end
    else
    if FMenuItem.Parent.GetParentMenu.Images <> nil then
    begin
      ImgListHandle := FMenuItem.Parent.GetParentMenu.Images.Handle;
      ImgIndex := FMenuItem.ImageIndex;

      B.Width := FMenuItem.Parent.GetParentMenu.Images.Width;
      B.Height := FMenuItem.Parent.GetParentMenu.Images.Height;
      B.Canvas.Brush.Color := FCanvas.Pixels[2,2];
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(ImgListHandle, ImgIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

    end;

  end;


  if FMenu.IsRightToLeft then
  begin
    X1 := FRect.Right - FIconWidth;
    X2 := FRect.Right;
  end
  else
  begin
    X1 := FRect.Left;
    X2 := FRect.Left + FIconWidth;
  end;
  IconRect := Rect(X1, FRect.Top, X2, FRect.Bottom);


  if HasImgLstBitmap or HasBitmap then
  begin
    CheckedRect := IconRect;
    Inc(CheckedRect.Left, 1);
    Inc(CheckedRect.Top, 2);
    Dec(CheckedRect.Right, 3);
    Dec(CheckedRect.Bottom, 2);
  end
  else
  begin
    CheckedRect.Left := IconRect.Left +
      (IConRect.Right - IconRect.Left - 10) div 2;
    CheckedRect.Top := IconRect.Top +
      (IConRect.Bottom - IconRect.Top - 10) div 2;
    CheckedRect.Right := CheckedRect.Left + 10;
    CheckedRect.Bottom := CheckedRect.Top + 10;
  end;

  if B.Width > FIconWidth then
    if FMenu.IsRightToLeft then
      CheckedRect.Left := CheckedRect.Right - B.Width
    else
      CheckedRect.Right := CheckedRect.Left + B.Width;

  if FTopMenu then Dec(CheckedRect.Top, 1);


  if FMenu.IsRightToLeft then
  begin
    X1 := FRect.Left;
    if not FTopMenu then
      Dec(X2, FIconWidth)
    else
      Dec(X2, 4);
    if (FRect.Right - B.Width) < X2 then
      X2 := FRect.Right - B.Width - 8;
  end
  else
  begin
    X1 := FRect.Left ;
    if not FTopMenu then
      Inc(X1, FIconWidth)
    else
      Inc(X1, 4);

    if (FRect.Left + B.Width) > X1 then
      X1 := FRect.Left + B.Width + 4;
    X2 := FRect.Right;
  end;

  TextRect := Rect(X1, FRect.Top, X2, FRect.Bottom);

  if FTopMenu then
  begin
    if not (HasImgLstBitmap or HasBitmap) then
    begin
      TextRect := FRect;
    end
    else
    begin
      if FMenu.IsRightToLeft then
        TextRect.Right := TextRect.Right + 5
      else
        TextRect.Left := TextRect.Left - 5;
    end

  end;

  if FTopMenu then
  begin
    FCanvas.brush.color := FFMenuBarColor;
    FCanvas.Pen.Color := FFMenuBarColor;

    FCanvas.FillRect(FRect);
  end
  else
  begin
    if (Is16Bit and FGradient) then
    begin
      inc(FRect.Right,2);  //needed for RightToLeft
      DrawGradient(FCanvas, FRect, FMenu.IsRightToLeft);
      Dec(FRect.Right,2);

    end
    else
    begin
      FCanvas.brush.color := FFColor;
      FCanvas.FillRect(FRect);

      FCanvas.brush.color := FFIconBackColor;
      FCanvas.FillRect(IconRect);
    end;


  end;


  if FMenuItem.Enabled then
    FCanvas.Font.Color := FFont.Color
  else
    FCanvas.Font.Color := FDisabledColor;

  if Selected and FDrawSelect then
  begin
    FCanvas.brush.Style := bsSolid;
    if FTopMenu then
    begin
      DrawTopMenuItem(FMenuItem, FCanvas, FRect, FMenuBarColor, FMenu.IsRightToLeft);
    end
    else
// HVW - 08/12/2003 - Jan Vandamme requirements
//      if FMenuItem.Enabled then
      begin

        Inc(FRect.Top, 1);
        Dec(FRect.Bottom, 1);
        FCanvas.brush.color := FFSelectColor;
        FCanvas.FillRect(FRect);
        FCanvas.Pen.color := FFSelectBorderColor;
        FCanvas.Brush.Style := bsClear;
        FCanvas.RoundRect(FRect.Left, FRect.top, FRect.Right, FRect.Bottom, 0, 0);
        Dec(FRect.Top, 1);
        Inc(FRect.Bottom, 1);
      end;
  end;

  DrawCheckedItem(FMenuItem, Selected, FMenuItem.Enabled, HasImgLstBitmap or HasBitmap,
                  FCanvas, CheckedRect);

  DrawIcon(FMenuItem, FCanvas, B, IconRect,
    Selected, False, FMenuItem.Enabled, FMenuItem.Checked,
    FTopMenu, FMenu.IsRightToLeft);



  if not IsLine then
  begin

    if FMenu.IsRightToLeft then
    begin
      TextFormat := DT_RIGHT + DT_RTLREADING;
      Dec(TextRect.Right, 3);
    end
    else
    begin
      TextFormat := 0;
      Inc(TextRect.Left, 3);
    end;

    DrawTheText(FMenuItem, txt, ShortCutToText(FMenuItem.ShortCut),
      FCanvas, TextRect,
      Selected, FMenuItem.Enabled, FMenuItem.Default,
      FTopMenu, FMenu.IsRightToLeft, FFont, TextFormat);

  end
  else
  begin
    if FMenu.IsRightToLeft then
    begin
      X1 := TextRect.Left;
      X2 := TextRect.Right - 7;
    end
    else
    begin
      X1 := TextRect.Left + 7;
      X2 := TextRect.Right;
    end;

    FCanvas.Pen.Color := FFSeparatorColor;
    FCanvas.MoveTo(X1,
      TextRect.Top +
      Round((TextRect.Bottom - TextRect.Top) / 2));
    FCanvas.LineTo(X2,
      TextRect.Top +
      Round((TextRect.Bottom - TextRect.Top) / 2))
  end;

  B.free;
  BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right - ARect.Left,
  ARect.Bottom - ARect.Top, ItemBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  ItemBitmap.Free;
  ActivateMenuItem(FMenuItem);
end;


procedure TPSCMenu.SetGlobalColor(ACanvas: TCanvas);
begin
  if GetDeviceCaps(ACanvas.Handle, BITSPIXEL) < 16 then
    Is16Bit := false
  else
    Is16Bit := true;


  FFColor := FColor;
  FFIconBackColor := FIconBackColor;

  if Is16Bit then
  begin
    FFSelectColor := NewColor(ACanvas, FSelectColor, 68);
    FCheckedAreaColor := NewColor(ACanvas, FSelectColor, 80);
    FCheckedAreaSelectColor := NewColor(ACanvas, FSelectColor, 50);

    FMenuBorderColor := GetShadeColor(ACanvas, clBtnFace, 90);
    FMenuShadowColor := GetShadeColor(ACanvas, clBtnFace, 76);
  end
  else
  begin
    FFSelectColor := FSelectColor;
    FCheckedAreaColor := clWhite;
    FCheckedAreaSelectColor := clSilver;
    FMenuBorderColor := clBtnShadow;
    FMenuShadowColor := clBtnShadow;
  end;

  FFSelectBorderColor := FSelectBorderColor;
  FFSelectFontColor := FSelectFontColor;
  FFMenuBarColor := FMenuBarColor;
  FFDisabledColor := FDisabledColor;
  FFCheckedColor := FCheckedColor;
  FFSeparatorColor := FSeparatorColor;



  if FUseSystemColors then
  begin
    GetSystemMenuFont(FFont);
    FFSelectFontColor := FFont.Color;
    if not Is16Bit then
    begin
      FFColor := clWhite;
      FFIconBackColor := clBtnFace;
      FFSelectColor := clWhite;
      FFSelectBorderColor := clHighlight;
      FFMenuBarColor := FFIconBackColor;
      FFDisabledColor := clBtnShadow;
      FFCheckedColor := clHighlight;
      FFSeparatorColor := clBtnShadow;
      FCheckedAreaColor := clWhite;
      FCheckedAreaSelectColor := clWhite;

    end
    else
    begin
      FFColor := NewColor(ACanvas, clBtnFace, 86);
      FFIconBackColor := NewColor(ACanvas, clBtnFace, 16);
      FFSelectColor := NewColor(ACanvas, clHighlight, 68);
      FFSelectBorderColor := clHighlight;
      FFMenuBarColor := clBtnFace;

      FFDisabledColor := NewColor(ACanvas, clBtnShadow, 10);
      FFSeparatorColor := NewColor(ACanvas, clBtnShadow, 25);
      FFCheckedColor := clHighlight;
      FCheckedAreaColor := NewColor(ACanvas, clHighlight, 80);
      FCheckedAreaSelectColor := NewColor(ACanvas, clHighlight, 50);

    end;
  end;

end;

procedure TPSCMenu.DrawTopMenuItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; BckColor:Tcolor; IsRightToLeft: boolean);
var
  X1, X2: integer;
  DefColor, HoldColor: TColor;
begin
  X1 := ARect.Left;
  X2 := ARect.Right;


  ACanvas.brush.Style := bsSolid;
  ACanvas.brush.color :=  FFSelectColor;

  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color := FFSelectBorderColor;

  if (not IsRightToLeft) and (Is16Bit) and (Sender is TMenuItem) then
  begin
    ACanvas.MoveTo(X1, ARect.Bottom - 1);
    ACanvas.LineTo(X1, ARect.Top);
    ACanvas.LineTo(X2 - 8, ARect.Top);
    ACanvas.LineTo(X2 - 8, ARect.Bottom);

    DefColor := FFMenuBarColor;


    HoldColor := GetShadeColor(ACanvas, DefColor, 10);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;

    ACanvas.FillRect(Rect(X2 - 7, ARect.Top, X2, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 30);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 3, X2 - 2, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 40 + 20);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 5, X2 - 3, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 60 + 40);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 6, X2 - 5, ARect.Bottom));


    ACanvas.Pen.Color := DefColor;
    ACanvas.MoveTo(X2 - 5, ARect.Top + 1);
    ACanvas.LineTo(X2 - 1, ARect.Top + 1);
    ACanvas.LineTo(X2 - 1, ARect.Top + 6);

    ACanvas.MoveTo(X2 - 3, ARect.Top + 2);
    ACanvas.LineTo(X2 - 2, ARect.Top + 2);
    ACanvas.LineTo(X2 - 2, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 3);



    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 10);
    ACanvas.MoveTo(X2 - 6, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 6);
    ACanvas.LineTo(X2 - 4, ARect.Top + 6);
    ACanvas.LineTo(X2 - 4, ARect.Top + 3);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 30);
    ACanvas.MoveTo(X2 - 5, ARect.Top + 5);
    ACanvas.LineTo(X2 - 4, ARect.Top + 5);
    ACanvas.LineTo(X2 - 4, ARect.Top + 9);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 40);
    ACanvas.MoveTo(X2 - 6, ARect.Top + 5);
    ACanvas.LineTo(X2 - 6, ARect.Top + 7);

  end
  else
  begin

    ACanvas.Pen.Color := FFSelectBorderColor;
    ACanvas.Brush.Color := GetShadeColor(ACanvas, BckColor, 70);

    ACanvas.MoveTo(X1, ARect.Bottom - 1);
    ACanvas.LineTo(X1, ARect.Top);
    ACanvas.LineTo(X2 - 3, ARect.Top);
    ACanvas.LineTo(X2 - 3, ARect.Bottom);


    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.FillRect(Rect(X2 - 2, ARect.Top + 2, X2, ARect.Bottom));

    ACanvas.Brush.Color := BckColor;
    ACanvas.FillRect(Rect(X2 - 2, ARect.Top , X2, ARect.Top + 2));


  end;

end;


procedure TPSCMenu.DrawCheckedItem(FMenuItem: TMenuItem; Selected, Enabled,
 HasImgLstBitmap: boolean; ACanvas: TCanvas; CheckedRect: TRect);
var
  X1, X2: integer;
begin
  if FMenuItem.RadioItem then
  begin
    if FMenuItem.Checked then
    begin
      if Enabled then
      begin
        ACanvas.Pen.color := FFSelectBorderColor;
        if selected then
          ACanvas.Brush.Color := FCheckedAreaSelectColor
        else
          ACanvas.Brush.Color := FCheckedAreaColor;
      end
      else
        ACanvas.Pen.color := FFDisabledColor;

      ACanvas.Brush.Style := bsSolid;
      if HasImgLstBitmap then
      begin
        ACanvas.RoundRect(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom,
          6, 6);
      end
      else
      begin
        ACanvas.Ellipse(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
      end;
    end;
  end
  else
  begin
    if (FMenuItem.Checked) then
      if (not HasImgLstBitmap) then
      begin
        if Enabled then
        begin
          ACanvas.Pen.color := FFCheckedColor;
          if selected then
            ACanvas.Brush.Color := FCheckedAreaSelectColor
          else
            ACanvas.Brush.Color := FCheckedAreaColor; ;
        end
        else
          ACanvas.Pen.color := FFDisabledColor;

        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
        if Enabled then
          ACanvas.Pen.color := clBlack
        else
          ACanvas.Pen.color := FFDisabledColor;
        x1 := CheckedRect.Left + 1;
        x2 := CheckedRect.Top + 5;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 2;
        ACanvas.LineTo(x1, x2);
        x1 := CheckedRect.Left + 2;
        x2 := CheckedRect.Top + 5;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.LineTo(x1, x2);
        x1 := CheckedRect.Left + 2;
        x2 := CheckedRect.Top + 4;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 5;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.LineTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Right + 2;
        x2 := CheckedRect.Top - 1;
        ACanvas.LineTo(x1, x2);
        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 2;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Right - 2;
        x2 := CheckedRect.Top + 3;
        ACanvas.LineTo(x1, x2);

      end
      else
      begin


        if Enabled then
        begin
          ACanvas.Pen.color := FFSelectBorderColor;
          if selected then
            ACanvas.Brush.Color := FCheckedAreaSelectColor
          else
            ACanvas.Brush.Color := FCheckedAreaColor; ;
        end
        else
          ACanvas.Pen.color := FFDisabledColor;

        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
      end;
  end;

end;

procedure TPSCMenu.DrawTheText(Sender: TObject; txt, ShortCuttext: string;
    ACanvas: TCanvas; TextRect: TRect;
    Selected, Enabled, Default, TopMenu, IsRightToLeft: boolean;
    var TxtFont: TFont; TextFormat: integer);
var
  DefColor: TColor;
  B: TBitmap;
  BRect: TRect;
begin

  DefColor := TxtFont.Color;

  ACanvas.Font.Assign (TxtFont);

  if Selected then
    DefColor := FFSelectFontColor;

  If not Enabled then
  begin
    DefColor := FFDisabledColor;

    if (Sender is TToolButton) then
    begin
      TextRect.Top := TextRect.Top +
        ((TextRect.Bottom - TextRect.Top) - ACanvas.TextHeight('W')) div 2;
      B := TBitmap.Create;

      B.Width := TextRect.Right - TextRect.Left;
      B.Height := TextRect.Bottom - TextRect.Top;
      BRect := Rect(0,0,B.Width, B.Height);


      B.Canvas.Brush.Color := ACanvas.Brush.Color;
      B.Canvas.FillRect (BRect);
      B.Canvas.Font.color := DefColor;

      DrawtextEx(B.Canvas.Handle,
        PChar(txt),
        Length(txt),
        BRect, TextFormat + DT_VCENTER, nil);
      ACanvas.CopyRect(TextRect, B.Canvas, BRect);
      B.Free;
      exit;
    end;

  end;

  if (TopMenu and Selected) then
    if FUseSystemColors then
    DefColor := TopMenuFontColor(ACanvas, FFIconBackColor);

  ACanvas.Font.color := DefColor;


  TextRect.Top := TextRect.Top +
    ((TextRect.Bottom - TextRect.Top) - ACanvas.TextHeight('W')) div 2;

  SetBkMode(ACanvas.Handle, TRANSPARENT);


  if Default and Enabled then
  begin

    Inc(TextRect.Left, 1);
    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 30);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);
    Dec(TextRect.Left, 1);


    Inc(TextRect.Top, 2);
    Inc(TextRect.Left, 1);
    Inc(TextRect.Right, 1);


    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 30);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);


    Dec(TextRect.Top, 1);
    Dec(TextRect.Left, 1);
    Dec(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 40);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);


    Inc(TextRect.Left, 1);
    Inc(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 60);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);

    Dec(TextRect.Left, 1);
    Dec(TextRect.Right, 1);
    Dec(TextRect.Top, 1);

    ACanvas.Font.color := DefColor;
  end;

  DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);


  txt := ShortCutText + ' ';

  if not Is16Bit then
    ACanvas.Font.color := DefColor
  else
    ACanvas.Font.color := GetShadeColor(ACanvas, DefColor, -40);



  if IsRightToLeft then
  begin
    Inc(TextRect.Left, 10);
    TextFormat := DT_LEFT
  end
  else
  begin
    Dec(TextRect.Right, 10);
    TextFormat := DT_RIGHT;
  end;


  DrawtextEx(ACanvas.Handle,
    PChar(txt),
    Length(txt),
    TextRect, TextFormat, nil);

end;

procedure TPSCMenu.DrawIcon(Sender: TObject; ACanvas: TCanvas; B: TBitmap;
 IconRect: Trect; Hot, Selected, Enabled, Checked, FTopMenu,
 IsRightToLeft: boolean);
var
  DefColor: TColor;
  X, Y: integer;
begin

  if (B <> nil) and (B.Width > 0) then
  begin
    X := IconRect.Left;
    Y := IconRect.Top + 1;

    if (Sender is TMenuItem) then
    begin
      inc(Y, 2);
      if FIconWidth > B.Width then
        X := X + ((FIconWidth - B.Width) div 2) - 1
      else
      begin
        if IsRightToLeft then
          X := IconRect.Right - b.Width - 2
        else
          X := IconRect.Left + 2;
      end;
    end;

    if FTopMenu then
    begin
      if IsRightToLeft then
        X := IconRect.Right - b.Width - 5
      else
        X := IconRect.Left + 1;
    end;


    if (Hot) and (not FTopMenu) and (Enabled) and (not Checked) then
      if not Selected then
      begin
        dec(X, 1);
        dec(Y, 1);
      end;

    if (not Hot) and (Enabled) and (not Checked) then
      if Is16Bit then
        DimBitmap(B, FDimLevel{30});


    if not Enabled then
    begin
      GrayBitmap(B, FGrayLevel );
      DimBitmap(B, 40);
    end;

    if (Hot) and (Enabled) and (not Checked) then
    begin
      if (Is16Bit) and (not UseSystemColors) and (Sender is TToolButton) then
        DefColor := NewColor(ACanvas, FSelectColor, 68)
      else
        DefColor := FFSelectColor;

      DefColor := GetShadeColor(ACanvas, DefColor, 50);
      DrawBitmapShadow(B, ACanvas, X + 2, Y + 2, DefColor);
    end;

    B.Transparent := true;
    ACanvas.Draw(X, Y, B);
  end;

end;


function TPSCMenu.TopMenuFontColor(ACanvas: TCanvas; Color: TColor): TColor;
var
  r, g, b, avg: integer;
begin

  Color := ColorToRGB(Color);
  r := Color and $000000FF;
  g := (Color and $0000FF00) shr 8;
  b := (Color and $00FF0000) shr 16;

  Avg := (r + b) div 2;

  if (Avg > 150) or (g > 200) then
    Result := FFont.Color
  else
    Result := NewColor(ACanvas, Color, 90);

end;


procedure TPSCMenu.SetActive(const Value: boolean);
begin
  FForm := Owner as TScrollingWinControl;

  FActive := Value;
  if FActive then
    InitItems(FForm, true, true)
  else
    InitItems(FForm, false, true);

    Windows.DrawMenuBar(FForm.Handle);
end;


procedure TPSCMenu.SetForm(const Value: TScrollingWinControl);
var
  Hold: boolean;
begin
  if Value <> FForm then
  begin
    Hold := Active;
    Active := false;
    FForm := Value;
    if Hold then
      Active := True;
  end;
end;

procedure TPSCMenu.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Windows.DrawMenuBar(FForm.Handle);

end;

procedure TPSCMenu.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TPSCMenu.SetIconBackColor(const Value: TColor);
begin
  FIconBackColor := Value;
end;

procedure TPSCMenu.SetMenuBarColor(const Value: TColor);
begin
  FMenuBarColor := Value;
  Windows.DrawMenuBar(FForm.Handle);
end;

procedure TPSCMenu.SetCheckedColor(const Value: TColor);
begin
  FCheckedColor := Value;
end;

procedure TPSCMenu.SetSeparatorColor(const Value: TColor);
begin
  FSeparatorColor := Value;
end;

procedure TPSCMenu.SetSelectBorderColor(const Value: TColor);
begin
  FSelectBorderColor := Value;
end;

procedure TPSCMenu.SetSelectColor(const Value: TColor);
begin
  FSelectColor := Value;
end;

procedure TPSCMenu.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
end;

procedure TPSCMenu.SetSelectFontColor(const Value: TColor);
begin
  FSelectFontColor := Value;
end;

procedure TPSCMenu.SetIconWidth(const Value: integer);
begin
  FIconWidth := Value;
end;

procedure TPSCMenu.SetDrawSelect(const Value: boolean);
begin
  FDrawSelect := Value;
end;



procedure TPSCMenu.SetOverrideOwnerDraw(const Value: boolean);
begin
  FOverrideOwnerDraw := Value;
  if FActive then
    Active := True;
end;


procedure TPSCMenu.SetUseSystemColors(const Value: boolean);
begin
  FUseSystemColors := Value;
  Windows.DrawMenuBar(FForm.Handle);
end;

procedure TPSCMenu.SetGradient(const Value: boolean);
begin
  if FGradient <> Value then
   begin
     FGradient := Value;
   end;
end;



procedure TPSCMenu.DrawGradient(ACanvas: TCanvas; ARect: TRect;
 IsRightToLeft: boolean);
var
  i: integer;
  v: integer;
  FRect: TRect;
begin

  fRect := ARect;
  V := 0;
  if IsRightToLeft then
  begin
    fRect.Left := fRect.Right - 1;
    for i := ARect.Right Downto ARect.Left do
    begin
      if (fRect.Left < ARect.Right)
        and (fRect.Left > ARect.Right - FIconWidth + 5) then
        inc(v, 3)
      else
        inc(v, 1);

      if v > 96 then v := 96;
      ACanvas.Brush.Color := NewColor(ACanvas, FFIconBackColor, v);
      ACanvas.FillRect(fRect);

      fRect.Left := fRect.Left - 1;
      fRect.Right := fRect.Left - 1;
    end;
  end
  else
  begin
    fRect.Right := fRect.Left + 1;
    for i := ARect.Left to ARect.Right do
    begin
      if (fRect.Left > ARect.Left)
        and (fRect.Left < ARect.Left + FIconWidth + 5) then
        inc(v, 3)
      else
        inc(v, 1);

      if v > 96 then v := 96;
      ACanvas.Brush.Color := NewColor(ACanvas, FFIconBackColor, v);
      ACanvas.FillRect(fRect);

      fRect.Left := fRect.Left + 1;
      fRect.Right := fRect.Left + 1;
    end;
  end;
end;


procedure TPSCMenu.DrawWindowBorder(hWnd: HWND; IsRightToLeft: boolean);
var
  WRect: TRect;
  dCanvas: TCanvas;
begin

  if hWnd <= 0 then
  begin
   exit;
  end;
  dCanvas := nil;
  try
    dCanvas := TCanvas.Create;
    dCanvas.Handle := GetWindowDC(GetDesktopWindow);

    GetWindowRect(hWnd, WRect);

    dCanvas.Brush.Style := bsClear;


    Dec(WRect.Right, 2);
    Dec(WRect.Bottom, 2);

    dCanvas.Pen.Color := FMenuBorderColor;
    dCanvas.Rectangle(WRect.Left, WRect.Top, WRect.Right, WRect.Bottom);

    if IsRightToLeft then
    begin
      dCanvas.Pen.Color := FFColor;
      dCanvas.Rectangle(WRect.Left + 1, WRect.Top + 1, WRect.Right - 2,
                        WRect.Top + 3);

      dCanvas.MoveTo(WRect.Left + 2, WRect.Top + 2);
      dCanvas.LineTo(WRect.Left + 2, WRect.Bottom - 2);


      dCanvas.Pen.Color := FFIconBackColor;
      dCanvas.MoveTo(WRect.Right - 2, WRect.Top + 2);
      dCanvas.LineTo(WRect.Right - 2, WRect.Bottom - 2);

      dCanvas.MoveTo(WRect.Right - 2, WRect.Top + 2);
      dCanvas.LineTo(WRect.Right - 1 - FIconWidth, WRect.Top + 2);
    end
    else
    begin
      if not FGradient then
      begin
        dCanvas.Pen.Color := FFColor;
        dCanvas.Rectangle(WRect.Left + 1, WRect.Top + 1, WRect.Right - 1,
                          WRect.Top + 3);

        dCanvas.Pen.Color := FFIconBackColor;
        dCanvas.MoveTo(WRect.Left + 1, WRect.Top + 2);
        dCanvas.LineTo(WRect.Left + 3 + FIconWidth, WRect.Top + 2);
      end
      else
        DrawGradient(dCanvas, Rect(WRect.Left + 1, WRect.Top + 1,
                                   WRect.Right - 3, WRect.Top + 3), IsRightToLeft);

      dCanvas.Pen.Color := FFIconBackColor;
      dCanvas.Rectangle(WRect.Left + 1, WRect.Top + 2,
                        WRect.Left + 3, WRect.Bottom - 1)

    end;

    Inc(WRect.Right, 2);
    Inc(WRect.Bottom, 2);

    dCanvas.Pen.Color := FMenuShadowColor;
    dCanvas.Rectangle(WRect.Left + 2, WRect.Bottom, WRect.Right, WRect.Bottom - 2);
    dCanvas.Rectangle(WRect.Right - 2, WRect.Bottom, WRect.Right, WRect.Top + 2);


    dCanvas.Pen.Color := clBtnFace ;
    dCanvas.Rectangle(WRect.Left, WRect.Bottom - 2, WRect.Left + 2, WRect.Bottom);
    dCanvas.Rectangle(WRect.Right - 2, WRect.Top, WRect.Right, WRect.Top + 2);
  finally
    ReleaseDC(GetDesktopWindow, dCanvas.Handle);
    dCanvas.Free;
  end;


end;


procedure TPSCMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not FActive then exit;
  if (Operation = opInsert) and
  ( (AComponent is TMenuItem) or (AComponent is TToolButton)) then
  begin
    if not (csDesigning in ComponentState) then
      Active := true;
  end;
end;




end.


