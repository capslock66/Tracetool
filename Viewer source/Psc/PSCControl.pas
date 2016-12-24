{*****************************************************************************
  Name           : PSCControl
  Author         : POL OFFICE COMMON
  Description    : Base control for PSC library
  History        :

  Date         By                   Description
  ----         --                   -----------
  13-09-2005   POL OFFICE COMMON     Initial creation of the Unit.
  28-10-2005   POL OFFICE COMMON     Added support for Windows XP themes
 *****************************************************************************}
{$I PSCControls.inc}

unit PSCControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  Forms,
  Graphics,
  ImgList,
  PSCCommon,
  {$IFDEF VERSION7}
  UxTheme,
  {$ELSE}
  PSCTheme,
  {$ENDIF}
  PSCXP,
  PSCHintInfo;


type

  IPSCControl = interface(IUnknown)
  ['{FDB9BE1D-2480-41B0-B076-DF1078D63744}']
  end;

  TPSCControl = class(TCustomControl)
  private
    FTheme: HTheme;
    FThemeActive: Boolean;
    FAboutInfo: TPSCAboutInfo;
    FFlickerFree: boolean;
    {$IFDEF VERSION5ONLY}
    FParentBackground : boolean;
    FNeedsBorderPaint : boolean;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    {$ENDIF}
    procedure WMThemeChanged( var Msg: TMessage ); message WM_THEMECHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure SetFlickerFree(const Value: boolean);
  protected
    function FindParentForm(Control: TControl): TCustomForm; virtual;
    procedure CreateWindowHandle( const Params: TCreateParams ); override;
    procedure DestroyWindowHandle; override;
    procedure PaintBorder(Control: TWinControl; EraseLRCorner: Boolean);
    {$IFDEF VERSION5ONLY}
    function GetNeedsBorderpaint: boolean; virtual;
    procedure SetNeedsBorderPaint(const Value: boolean); virtual;
    function GetParentBackground : boolean; virtual;
    procedure SetParentBackground(const Value : boolean); virtual;
    {$ENDIF}
    procedure PaintToCanvas(ACanvas : TCanvas; ARect : TRect); virtual;
    procedure BufferedPaint; virtual;
    {$IFDEF VERSION5ONLY}
    property ParentBackground : boolean read GetParentBackground write SetParentBackground;
    property NeedsBorderPaint : boolean read GetNeedsBorderpaint write SetNeedsBorderPaint;
    {$ENDIF}
    property ThemeActive : boolean read FThemeActive;
    property FlickerFree : boolean read FFlickerFree write SetFlickerFree default false;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property About: TPSCAboutInfo read FAboutInfo write FAboutInfo stored false;
  end;

  TPSCComponent = class(TComponent)
  private
    FAboutInfo : TPSCAboutInfo;
  published
    property About : TPSCAboutInfo read FAboutInfo write FAboutInfo stored false;
  end;

  TPSCButtonInfo = class(TPersistent)
  private
    FVisible: boolean;
    FImageIndex: TImageIndex;
    FOwner : TComponent;
    FHint: string;
    FHintInfo: TPSCHintInfo;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetVisible(const Value: boolean);
    procedure SetHintInfo(const Value: TPSCHintInfo);
  protected
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AnOwner : TComponent); virtual;
    destructor Destroy; override;
  published
    property Visible : boolean read FVisible write SetVisible default true;
    property ImageIndex : TImageIndex read FImageIndex write SetImageIndex default -1;
    property Hint : string read FHint write FHint;
    property HintInfo : TPSCHintInfo read FHintInfo write SetHintInfo;
  end;

implementation


{ TPSCControl }

procedure TPSCControl.BufferedPaint;
var
 Buffer : TBitmap;
 R : TRect;
begin
  R := GetClientRect;
  Buffer := TBitmap.Create;
  Buffer.Width := R.Right - R.Left;
  Buffer.Height := R.Bottom - R.Top;
  try
   PaintToCanvas(Buffer.Canvas, R);
   BitBlt(Self.Canvas.handle, 0, 0, ClientWidth, ClientHeight, Buffer.Canvas.handle, 0, 0, SRCCOPY);
  finally
    Buffer.Free;
  end;
end;

constructor TPSCControl.Create(AOwner: TComponent);
begin
  inherited;
  FFlickerFree := false;
  if isWindowsXP then
   begin
     FThemeActive := UseThemes;
   end;
end;

procedure TPSCControl.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  if ThemeActive then
    begin
      FTheme := OpenThemeData(Handle, 'EDIT');
    end;
end;

destructor TPSCControl.Destroy;
begin
  inherited;

end;

procedure TPSCControl.DestroyWindowHandle;
begin
  if ThemeActive then
   begin
     CloseThemeData(FTheme);
   end;

  inherited;

end;

function TPSCControl.FindParentForm(Control: TControl): TCustomForm;
begin
 if Control.Parent is TCustomForm then
  Result := TCustomForm(Control.parent) else
    begin
     while Control.Parent <> nil do
      begin
        Control := Control.Parent;
        if Control is TCustomForm then
         break;
      end;
     if Control is TCustomForm then
       Result := TCustomForm(Control) else
         Result := nil;
     end;
end;


{$IFDEF VERSION5ONLY}
function TPSCControl.GetNeedsBorderpaint: boolean;
begin
  Result := FNeedsBorderPaint;
end;

function TPSCControl.GetParentBackground: boolean;
begin
  Result := FParentBackground;
end;
{$ENDIF}

procedure TPSCControl.Paint;
begin
  if FlickerFree then
   BufferedPaint
    else
      PaintToCanvas(Canvas, GetClientRect);
end;

procedure TPSCControl.PaintBorder(Control: TWinControl;
  EraseLRCorner: Boolean);
var
  EmptyRect,
  DrawRect: TRect;
  DC: HDC;
  H, W: Integer;
  AStyle,
  ExStyle: Integer;
begin
  with Control do
  begin
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      DC := GetWindowDC(Handle);
      try
        EmptyRect := DrawRect;
        if EraseLRCorner then
        begin
          AStyle := GetWindowLong(Handle, GWL_STYLE);
          if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
          begin
            W := GetSystemMetrics(SM_CXVSCROLL);
            H := GetSystemMetrics(SM_CYHSCROLL);
            InflateRect(EmptyRect, -2, -2);
            with EmptyRect do
              EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
            FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
          end;
        end;
        with DrawRect do
          ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
        DrawThemeBackground(FTheme, DC, EP_EDITTEXT, 0, DrawRect, nil);
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  end;
end;

procedure TPSCControl.PaintToCanvas(ACanvas: TCanvas; ARect: TRect);
begin

end;

procedure TPSCControl.SetFlickerFree(const Value: boolean);
begin
  if FFlickerFree <> Value then
   begin
     FFlickerFree := Value;
     Invalidate;
   end;
end;


{$IFDEF VERSION5ONLY}

procedure TPSCControl.SetNeedsBorderPaint(const Value: boolean);
begin
  if FNeedsBorderPaint <> Value then
    begin
      FNeedsBorderPaint := Value;
      Invalidate;
    end;
end;

procedure TPSCControl.SetParentBackground(const Value: boolean);
begin
  if FParentBackground <> Value then
   begin
     FParentBackground := Value;
     Invalidate;
   end;
end;

procedure TPSCControl.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if ThemeActive and FNeedsBorderPaint then
   PaintBorder(Self, true);
end;

{$ENDIF}

procedure TPSCControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if FThemeActive and ParentBackground then
   begin
     DrawThemeParentBackground(Handle, Message.DC, nil);
     Message.Result := 1;
   end
     else
      inherited;
end;


procedure TPSCControl.WMThemeChanged(var Msg: TMessage);
begin
  inherited;
  if isWindowsXP then
   begin
     FThemeActive := UseThemes;
   end;
  if FThemeActive then
  begin
    CloseThemeData( FTheme );
    FTheme := OpenThemeData( Handle, 'EDIT' );
  end;
end;


{ TPSCButtonInfo }

procedure TPSCButtonInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TPSCButtonInfo then
    with TPSCButtonInfo(Dest) do
    begin
      ImageIndex := Self.ImageIndex;
      Visible := Self.Visible;
    end
  else inherited AssignTo(Dest);
end;

constructor TPSCButtonInfo.Create(AnOwner: TComponent);
begin
  inherited Create;
  FOwner := AnOwner;
  FVisible := true;
  FImageIndex := -1;
  FHintInfo := TPSCHintInfo.Create(Self);
end;

destructor TPSCButtonInfo.Destroy;
begin
  FHintInfo.Free;
  inherited;
end;

function TPSCButtonInfo.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TPSCButtonInfo.SetHintInfo(const Value: TPSCHintInfo);
begin
  FHintInfo.Assign(Value);
end;

procedure TPSCButtonInfo.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
   begin
     FImageIndex := Value;
     TPSCControl(FOwner).Invalidate;
   end;
end;

procedure TPSCButtonInfo.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
   begin
     FVisible := Value;
     TPSCControl(FOwner).Invalidate;
   end;
end;

end.
