{----------------------------------------------------------}
{           Simple Editors for VirtualStringTree           }
{               by Constantine Yannakopoulos               }
{----------------------------------------------------------}
{         This software is distributed "AS IS",            }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. }
{----------------------------------------------------------}

unit VTEditors;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, Buttons, ExtCtrls, ComCtrls, Spin;//, unit1;

{$I Compilers.inc}

const
  WM_STARTEDITING_TRACE  = WM_USER + 778;
  WM_STARTEDITING_MEMBER = WM_USER + 779;

type
  TCustomEditLinkClass = class of TCustomEditLink;

  //----------------------------------------------------------------------------

  TIntfOwnedPersistent = class(TPersistent, IUnknown)
  private
    FOwner: TPersistent;
    FOwnerInterface: IUnknown;
    FRefCount: Integer;
    FManaged: Boolean;
  protected
    function GetOwner: TPersistent; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TPersistent); overload; virtual;
    constructor Create(AManaged: Boolean = False); overload;    // if AManaged = True it is a ref-counted object
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    property Owner: TPersistent read FOwner;
    property RefCount: Integer read FRefCount;
    property Managed: Boolean read FManaged;
  end;

  //----------------------------------------------------------------------------

  TCustomEditLink = class(TIntfOwnedPersistent, IVTEditLink)
  private
    FEdit: TWinControl;
    FTree: TBaseVirtualTree;
    FNode: PVirtualNode;
    FColumn: Integer;
    FOldEditText: string;
    FStopping: Boolean;
    FOldWndProc: TWndMethod;
    function GetLink: IVTEditLink;
  protected
    function CreateEditControl: TWinControl; virtual; abstract;
    function GetEditText: WideString; virtual;
    procedure SetEditText(const Value: WideString); virtual;
    procedure PrepareEditControl; virtual;
    procedure EditWndProc(var Message: TMessage); virtual;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoExit(Sender: TObject); virtual;
    function Modified: Boolean; virtual;
    procedure StopEdit; virtual;
    function GetTreeText: WideString; virtual;
    procedure SetTreeText(const Value: WideString); virtual;
    procedure UpdateTree;
    property Tree: TBaseVirtualTree read FTree;
    property EditControl: TWinControl read FEdit;
    property Node: PVirtualNode read FNode;
    property Column: Integer read FColumn;
    property Stopping: Boolean read FStopping;
  public
    destructor Destroy; override;
//    class function GetName: string; virtual;

    property Link: IVTEditLink read GetLink;
  protected
    { IVTEditLink }
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(ATree: TBaseVirtualTree; ANode: PVirtualNode; AColumn: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(Rect: TRect); virtual; stdcall;
  end;

  //----------------------------------------------------------------------------
  TMoveMemoEditLink = class(TCustomEditLink)
  private
    FReadOnly: Boolean;
    FExtent: TPoint;
    fDisplayedLines : Longint ;
    fcharCount : integer ;
  protected
    function  CreateEditControl: TWinControl; override;
    procedure PrepareEditControl; override;
    function  GetEditText: WideString; override;
    procedure SetEditText(const Value: WideString); override;
  public
    constructor Create(AReadOnly: Boolean = False);  overload;
    procedure   SetBounds(R: TRect); override; stdcall;
    procedure   SelectAll() ;
  published
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ExtentX: Integer read FExtent.X write FExtent.X default 0;
    property ExtentY: Integer read FExtent.Y write FExtent.Y default 0;
  end;

  //----------------------------------------------------------------------------


//procedure RegisterEditLink(ALinkClass: TCustomEditLinkClass);
//procedure RevokeEditLink(ALinkClass: TCustomEditLinkClass);
//function  GetEditLinkClass(const Name: string): TCustomEditLinkClass;
//procedure GetEditLinkClasses(Strings: TStrings);

implementation

uses TypInfo, unt_tool;

{$IFNDEF DELPHI_5_UP}

//------------------------------------------------------------------------------

function AnsiSameText(const S1, S2: string): Boolean;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) = 2;
end;

//------------------------------------------------------------------------------

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;
  P.Free;
end;

{$ENDIF}

//------------------------------------------------------------------------------

//var
//  LinkClasses: TStringList = nil;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TIntfOwnedPersistent }

constructor TIntfOwnedPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FManaged := True;
end;

//------------------------------------------------------------------------------

constructor TIntfOwnedPersistent.Create(AManaged: Boolean = False);
begin
  Create(nil);
  FManaged := AManaged;
end;

//------------------------------------------------------------------------------

procedure TIntfOwnedPersistent.AfterConstruction;
begin
  inherited;
  if GetOwner <> nil then
    GetOwner.GetInterface(IUnknown, FOwnerInterface);
  if not FManaged then
    InterlockedDecrement(FRefCount);
end;

//------------------------------------------------------------------------------

procedure TIntfOwnedPersistent.BeforeDestruction; 
begin
  Assert(FManaged or (FRefCount = 0),
    Format('%s instance has RefCount <> 0 during destruction.', [ClassName]));
end;

//------------------------------------------------------------------------------

class function TIntfOwnedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TIntfOwnedPersistent(Result).FRefCount := 1;
end;

//------------------------------------------------------------------------------

function TIntfOwnedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

//------------------------------------------------------------------------------

function TIntfOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TIntfOwnedPersistent._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else if not FManaged then
    Result := InterlockedIncrement(FRefCount)
  else
    Result := -1;
end;

//------------------------------------------------------------------------------

function TIntfOwnedPersistent._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else if not FManaged then
  begin
    Result := InterlockedDecrement(FRefCount);
    if Result <= 0 then Destroy;
  end
  else
    Result := -1;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TCustomEditLink }

type
  TWinControlAccess = class(TWinControl);

//------------------------------------------------------------------------------

destructor TCustomEditLink.Destroy;
begin
  FreeAndNil(FEdit);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

//class function TCustomEditLink.GetName: string;
//var
//  I: Integer;
//begin
//  Result := ClassName;
//  if Result[1] = 'T' then Delete(Result, 1, 1);
//  I := Pos('EditLink', Result);
//  if I = Length(Result) - 7 then SetLength(Result, Length(Result) - 8);
//end;

//------------------------------------------------------------------------------

function TCustomEditLink.BeginEdit: Boolean;
var
  F: TCustomForm;
begin
  Result := not FStopping;
  if Result then
  begin
    FEdit.Show;
    F := GetParentForm(FTree);
    if Assigned(F) and F.Active and FTree.Focused then
      FEdit.SetFocus;
    FOldWndProc := FEdit.WindowProc;
    FEdit.WindowProc := EditWndProc;
    FOldEditText := GetEditText;
  end;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    TWinControlAccess(EditControl).OnExit := nil;
    FEdit.Hide;
    FTree.CancelEditNode;
    StopEdit;
  finally
    FStopping := False;
  end;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.EndEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    FEdit.Perform(CM_EXIT, 0, 0);
    UpdateTree;
    FTree.EndEditNode;
    StopEdit;
  finally
    FStopping := False;
  end;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.GetBounds: TRect;
begin
  Result := FEdit.BoundsRect;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.PrepareEdit(ATree: TBaseVirtualTree;
  ANode: PVirtualNode; AColumn: TColumnIndex): Boolean;
begin
  Result := not FStopping;
  if Result then
  begin
    FTree := ATree;
    FNode := ANode;
    FColumn := AColumn;
    FreeAndNil(FEdit);
    FEdit := CreateEditControl;
    Result := Assigned(FEdit);
    if Result then
      with FEdit do
      begin
        Visible := False;
        Parent := Tree;
        TWinControlAccess(EditControl).OnKeyDown := KeyDown;
        TWinControlAccess(EditControl).OnExit := DoExit;
        FEdit.BoundsRect := FTree.GetDisplayRect(FNode, FColumn, False);
        PrepareEditControl;
        SetEditText(GetTreeText);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.SetBounds(Rect: TRect);
begin
  FEdit.BoundsRect := Rect;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.GetEditText: WideString;
var
  Len: Integer;
begin
  Len := GetWindowTextLengthW(FEdit.Handle);
  SetLength(Result, Len);
  if Len > 0 then
    GetWindowTextW(FEdit.Handle, @Result[1], Len);
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.SetEditText(const Value: WideString);
begin
  SetWindowTextW(FEdit.Handle, PWideChar(Value));
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.PrepareEditControl;
begin
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.EditWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CHAR:
      if not (TWMChar(Message).CharCode in [VK_ESCAPE, VK_TAB]) then
        FOldWndProc(Message);
    WM_GETDLGCODE:
      Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS;
  else
    FOldWndProc(Message);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        FTree.SetFocus;
      end;
    VK_RETURN:
      if Shift = [] then
      begin
        FTree.EndEditNode;
        FTree.SetFocus;
      end
      else
        Exit;
  else
    Exit;
  end;
  Key := 0;
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.DoExit(Sender: TObject);
begin
  if not FStopping then FTree.EndEditNode;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.Modified: Boolean;
begin
  Result := GetEditText <> FOldEditText;
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.StopEdit;
begin
  if Assigned(FEdit) then
  begin
    FEdit.Hide;
    FEdit.WindowProc := FOldWndProc;
    FEdit.Parent := nil;
  end;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.GetTreeText: WideString;
begin
  if FTree is TCustomVirtualStringTree then
    Result := TVirtualStringTree(FTree).Text[FNode, FColumn]
  else
    Result := '';
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.SetTreeText(const Value: WideString);
begin
  if FTree is TCustomVirtualStringTree then
    TVirtualStringTree(FTree).Text[FNode, FColumn] := GetEditText;
end;

//------------------------------------------------------------------------------

procedure TCustomEditLink.UpdateTree;
begin
  if Modified then
  begin
    SetTreeText(GetEditText);
    SetEditText(GetTreeText);
    FOldEditText := GetEditText;
  end;
end;

//------------------------------------------------------------------------------

function TCustomEditLink.GetLink: IVTEditLink;
begin
  GetInterface(IVTEditLink, Result);
end;

//------------------------------------------------------------------------------

type
  TSizeGrip = class(TCustomControl)
  private
    FCaptured: Boolean;
    FDragStartPos: TPoint;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//------------------------------------------------------------------------------

constructor TSizeGrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Anchors := [akRight, akBottom];
  Cursor := crSizeNWSE;
  Width := GetSystemMetrics(SM_CXVSCROLL);
  Height := GetSystemMetrics(SM_CYHSCROLL);
  ControlStyle := [csDoubleClicks];
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    ImeMode := imDisable;
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  FCaptured := True;
  SetCaptureControl(Self);
  FDragStartPos := SmallPointToPoint(Message.Pos);
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  FCaptured := False;
  SetCaptureControl(nil);
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.WMMouseMove(var Message: TWMMouseMove);
var
  NewWidth, NewHeight, newTop, NewLeft: integer;
  RealParent : TWinControl ;
begin
  inherited;
  if FCaptured then
  begin
    RealParent := Parent.Parent ;
    NewTop    := RealParent.Top ;
    NewLeft   := RealParent.Left ;
    NewWidth  := RealParent.Width + Message.XPos - FDragStartPos.x;
    NewHeight := RealParent.Height + Message.YPos - FDragStartPos.Y;

    if NewWidth < (width *2) then begin
       NewLeft := NewLeft + NewWidth - (width *2) ;
       NewWidth := (width *2);
    end ;

    if NewHeight < (Height*2) then begin
       NewTop := NewTop + NewHeight - (Height*2) ;
       NewHeight := (Height*2);
    end ;

    if NewWidth + NewLeft + Width + 4 > RealParent.Parent.Width then
       NewWidth := RealParent.Parent.Width - NewLeft - Width - 4 ;

    if NewHeight + NewTop + (Height*2) + 4 > RealParent.Parent.Height then
       NewHeight := RealParent.Parent.Height - NewTop - (Height*2) - 4;

    if newTop < 0 then
       newTop := 0 ;

    if newLeft < 0 then
       NewLeft := 0 ;

    MoveWindow(RealParent.Handle, NewLeft, NewTop, NewWidth, NewHeight, True);
    Application.ProcessMessages; // Give windows a chance to repaint.
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Tabstop := False;
  with Params do
  begin
    Style := WS_CHILD;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
  ControlStyle := ControlStyle - [csFramed];
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.CreateWnd;
var
  Rgn: HRGN;
  Triangle: array[0..2] of TPoint;
begin
  inherited;
  with ClientRect do
  begin
    Triangle[0].X := Right;
    Triangle[0].Y := Top;
    Triangle[1] := BottomRight;
    Triangle[2].X := Left;
    Triangle[2].Y := Bottom;
  end;
  Rgn := CreatePolygonRgn(Triangle, 3, ALTERNATE);
  SetWindowRgn(Handle, Rgn, True);
end;

//------------------------------------------------------------------------------

procedure TSizeGrip.Paint;
begin
  inherited;
  DrawFrameControl(Canvas.Handle, ClientRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
end;

//------------------------------------------------------------------------------
type
  TMoveGrip = class(TCustomControl)
  private
    FCaptured: TCursor;
    FRightOffset : integer ;
    FDragStartPos: TPoint;
    ScrollWidth  : integer ;
    ScrollHeight : integer ;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//------------------------------------------------------------------------------

constructor TMoveGrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient ;
  FCaptured := 0 ;
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Tabstop := False;
  with Params do
  begin
    Style := WS_CHILD;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
  ControlStyle := ControlStyle - [csFramed];
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.Paint;
var
   c,r,g,b : integer ;
   nbBlock : integer ;
   posBlock : integer ;
begin
  inherited;

  with ClientRect do begin
     Canvas.Pen.Style := psSolid ;
     r := 10 ;
     g := 60 ;
     b := 150 ;
     for c := Bottom downto top do begin
        Canvas.Pen.Color := r + (g*256) + (b*65536) ;
        Canvas.MoveTo(Left ,c);
        Canvas.LineTo(Right,c);
        inc(r,13);
        inc(g,12) ;
        inc(b,10) ;
     end ;

     // draw small circles on the bar
     nbBlock := 9 ;
     if Right < 60 then nbBlock := 8 ;
     if Right < 54 then nbBlock := 7 ;
     if Right < 48 then nbBlock := 6 ;
     if Right < 42 then nbBlock := 5 ;

     posBlock := (Right - (nbBlock * 6)) div 2 ;
     for c := 1 to nbBlock do begin
        // draw a small circle : 4 * 4 pixels with an empty point in the midel

        // blue on top,left
        Canvas.Pixels [posBlock+1,3] := clblue ;
        Canvas.Pixels [posBlock+2,3] := clblue ;
        Canvas.Pixels [posBlock+0,4] := clblue ;
        Canvas.Pixels [posBlock+0,5] := clblue ;

        Canvas.Pixels [posBlock+1,4] := clblue ;

        // white on bottom,right
        Canvas.Pixels [posBlock+3,4] := clWhite ;
        Canvas.Pixels [posBlock+2,5] := clWhite ;
        Canvas.Pixels [posBlock+3,5] := clWhite ;
        Canvas.Pixels [posBlock+1,6] := clWhite ;
        Canvas.Pixels [posBlock+2,6] := clWhite ;

        inc (posBlock,6) ;  // next block
     end ;

     //Canvas.RoundRect(Left,Top,Right,Bottom-3, 2, 2) ;
     //Canvas.Rectangle(Left,Top+5,Right,Bottom+1) ;
  end ;
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  ScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
  ScrollHeight := GetSystemMetrics(SM_CYHSCROLL);
  FRightOffset := Parent.Parent.Width - Message.xPos ;

  if Message.xPos < 10 then begin
     FCaptured := crSizeNWSE ;
  end else if FRightOffset <= 10 then begin
     FCaptured := crSizeNESW ;

  end else begin
     FCaptured := crSizeAll ;
  end ;

  SetCaptureControl(Self);
  FDragStartPos := SmallPointToPoint(Message.Pos);
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  FCaptured := 0;
  SetCaptureControl(nil);
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

//------------------------------------------------------------------------------

procedure TMoveGrip.WMMouseMove(var Message: TWMMouseMove);
var
   NewLeft, NewTop, newHeight, newWidth : integer;
   RealParent : TWinControl ;
begin
   inherited;
   RealParent := Parent.Parent ;

   if FCaptured = crSizeNWSE then begin          // -------left resize----------
      NewLeft := RealParent.left + Message.XPos - FDragStartPos.x;
      if NewLeft < 0 then
         NewLeft := 0 ;

      NewTop  := RealParent.top  + Message.YPos - FDragStartPos.Y;
      if NewTop  < 0 then
         NewTop  := 0 ;

      // force move window to right if too small (NewLeft don't change)
      newWidth := RealParent.Width  + RealParent.left - NewLeft ;
      if newWidth < ScrollWidth*2 then
         newWidth := ScrollWidth*2 ;

      // force move window to bottom id too small (NewTop don't change)
      newHeight := RealParent.Height + RealParent.Top - NewTop ;
      if newHeight < ScrollHeight*2 then
         newHeight := ScrollHeight*2 ;

      // check tree boundaries
      if NewLeft+RealParent.Width + ScrollWidth + 4 > RealParent.Parent.Width then
         NewLeft := RealParent.Parent.Width -RealParent.Width - ScrollWidth - 4 ;

      if NewTop+RealParent.Height + (ScrollHeight*2) + 4 > RealParent.Parent.Height then
         NewTop := RealParent.Parent.Height -RealParent.Height - (ScrollHeight*2) - 4;

      MoveWindow(RealParent.Handle, NewLeft, NewTop, newWidth, newHeight, True);
      Application.ProcessMessages; // Give windows a chance to repaint.

   end else if FCaptured = crSizeNESW then begin // -------right resize---------
      NewLeft := RealParent.left ;
      NewTop  := RealParent.top  + Message.YPos - FDragStartPos.Y;
      if NewTop < 0 then
         NewTop := 0 ;

      // force move window to left if too small (newLeft is changed)
      newWidth := Message.XPos + FRightOffset; // - FDragStartPos.x ;
      if newWidth < ScrollWidth*2 then begin
         newLeft  := newLeft + newWidth - (ScrollWidth*2) ;
         newWidth := ScrollWidth*2 ;
      end ;

      // force move window to bottom if too small (NewTop don't change)
      newHeight := RealParent.Height + RealParent.Top - NewTop ;
      if newHeight < ScrollHeight*2 then
         newHeight := ScrollHeight*2 ;

      // check tree boundaries
      if NewLeft < 0 then
         NewLeft := 0 ;

      if NewTop+RealParent.Height + (ScrollHeight*2) + 4 > RealParent.Parent.Height then
         NewTop := RealParent.Parent.Height -RealParent.Height - (ScrollHeight*2) - 4;

      if NewLeft + newWidth + ScrollWidth + 4 > RealParent.Parent.Width then
         newWidth := RealParent.Parent.Width - NewLeft - ScrollWidth - 4 ;

      MoveWindow(RealParent.Handle,NewLeft , NewTop, newWidth, newHeight, True);
      Application.ProcessMessages; // Give windows a chance to repaint.

   end else if FCaptured = crSizeAll then begin  // --------move window---------
      NewLeft := RealParent.left + Message.XPos - FDragStartPos.x;
      NewTop  := RealParent.top  + Message.YPos - FDragStartPos.Y;

      // check tree boundaries
      if NewLeft < 0 then
         NewLeft := 0 ;
      if NewTop < 0 then
         NewTop := 0 ;

      if NewLeft+RealParent.Width + ScrollWidth + 4 > RealParent.Parent.Width then
         NewLeft := RealParent.Parent.Width -RealParent.Width - ScrollWidth - 4 ;

      if NewTop+RealParent.Height + (ScrollHeight*2) + 4 > RealParent.Parent.Height then
         NewTop := RealParent.Parent.Height -RealParent.Height - (ScrollHeight*2) - 4;

      MoveWindow(RealParent.Handle, NewLeft, NewTop, RealParent.Width, RealParent.Height, True);
      Application.ProcessMessages; // Give windows a chance to repaint.

   end else begin
       if Message.xPos < 10 then
          cursor := crSizeNWSE
       else if Message.xPos > RealParent.Width - 10 then
          cursor := crSizeNESW
       else
          Cursor := crSizeAll ; //crArrow	;
   end ;
end;

//------------------------------------------------------------------------------

type
  TGripMemo = class(TMemo)
  private
    FGrip: TSizeGrip;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WndProc(var Message: TMessage); override;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//------------------------------------------------------------------------------

constructor TGripMemo.Create(AOwner: TComponent);
begin
  inherited;
  FGrip := TSizeGrip.Create(Self);
  with FGrip do
  begin
    Parent := Self;
    Left := Self.Width - Width - 1;
    Top := Self.Height - Height - 1;
    Anchors := [akRight, akBottom];
    Visible := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TGripMemo.CNCommand(var Message: TWMCommand);
begin
  inherited;
  with Message do
    if (NotifyCode = EN_CHANGE) or (NotifyCode = EN_UPDATE) or
       (NotifyCode = EN_HSCROLL) or (NotifyCode = EN_VSCROLL) then
    begin
      Invalidate;
      FGrip.Invalidate;
    end;
end;

//------------------------------------------------------------------------------

procedure TGripMemo.KeyDown(var Key: Word; Shift: TShiftState);
var
   MoveMemo : TForm ;
begin
  inherited;
  // detect escape key to cancel edit
  if key = VK_ESCAPE then begin
     MoveMemo := TForm(parent) ;
     MoveMemo.OnKeyDown (MoveMemo,Key,Shift) ; // let the form handle the key
  end ;
  FGrip.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TGripMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  FGrip.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TGripMemo.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_NOTIFY:
      if TWMNotify(Message).NMHdr.code = EN_UPDATE then
        FGrip.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

type
  //TMoveMemo = class(TPanel)
  TMoveMemo = class(TForm)
    procedure CreateParams(var Params: TCreateParams); override ;
  public
    TopPanel: TPanel;
    Memo: TGripMemo;
    MoveGrip : TMoveGrip ;
    procedure FormResize(Sender: TObject);
  end;

//------------------------------------------------------------------------------

procedure TMoveMemo.CreateParams(var Params: TCreateParams);
begin
  // border :
  // ---------
  // WS_THICKFRAME  : 3d gray border           , with sizing support
  // WS_DLGFRAME    : 3d gray border           , without sizing
  // WS_BORDER      : 1 pixel thin-line border , without sizing
  // none specified : no border at all         , without sizing

  inherited CreateParams(Params);
  // TPanel(self).BevelOuter := bvNone ;   // panel based
  // exit ;                                // panel based

  // remove caption
  Params.Style := Params.Style xor WS_CAPTION ;

  // remove form border
  Params.Style := Params.Style xor WS_THICKFRAME ;
  //Params.Style := Params.Style xor WS_BORDER ;
end;

//------------------------------------------------------------------------------

procedure TMoveMemo.FormResize(Sender: TObject);
begin
  inherited;
  MoveGrip.Invalidate ;
end;

//------------------------------------------------------------------------------

function TMoveMemoEditLink.CreateEditControl: TWinControl;
var
  frm : TMoveMemo ;
begin

  //frm := TMoveMemo.create(nil);          // panel based
  frm := TMoveMemo.CreateNew(nil,0);     // form based

  frm.OnResize := frm.FormResize ;

  // create a top panel
  frm.TopPanel := TPanel.create (frm) ;
  frm.TopPanel.Height := 10 ;
  frm.TopPanel.color := clGreen ;
  frm.TopPanel.Align := alTop ;
  frm.TopPanel.parent := frm ;
  frm.TopPanel.BevelOuter := bvNone ;

  // create a move grip inside the Panel
  frm.MoveGrip := TMoveGrip.Create(frm);
  with frm.MoveGrip do
  begin
    Parent := frm.TopPanel ;
    Visible := True;
  end;

  // create a memo with an attached TSizeGrip
  frm.Memo := TGripMemo.Create(frm);
  frm.Memo.parent := frm ;
  frm.Memo.Align := alclient ;

  Result := frm ;
end;

//------------------------------------------------------------------------------
{ TMoveMemoEditLink }

constructor TMoveMemoEditLink.Create(AReadOnly: Boolean);
begin
  Create(nil);
  FReadOnly := AReadOnly;
end;

//------------------------------------------------------------------------------

function TMoveMemoEditLink.GetEditText: WideString;
begin
  Result := TMoveMemo(FEdit).Memo.Text ; //  TMemo(FEdit).Text;
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.SelectAll;
begin
   TMoveMemo(FEdit).Memo.SetFocus ;
   TMoveMemo(FEdit).Memo.SelStart := 0;
   TMoveMemo(FEdit).Memo.SelLength := Length(TMoveMemo(FEdit).Memo.lines.Text);  
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.PrepareEditControl;
begin
  inherited;
  //TForm1(FEdit).Visible := true ;

  with TMoveMemo(FEdit).Memo do
  begin
    Ctl3d := False;
    ReadOnly := FReadOnly;
    WordWrap := True;
    ScrollBars := ssVertical ;
  end;
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.SetBounds(R: TRect);
var
   TreeRect: TRect;
   memo : TMemo ;
   Pt: TPoint;
   p : integer;
   oldRight : integer ;
   ScrollHeight : integer ;
begin
   ScrollHeight := GetSystemMetrics(SM_CYHSCROLL);

   oldRight := r.right ;
   TreeRect := FTree.ClientRect;

   // extend memo height and width to max visible tree area
   R.Right := TreeRect.Right ;
   R.Bottom := TreeRect.bottom ;
   if (r.Bottom-r.Top) < 25 then
     r.top := r.bottom - 25 ;

   // VST automatically move the Top node position to at least 0
   // but leave the left position as is and can then be negative
   if r.Left < 0 then
      r.Left := 0 ;

   // position the memo
   inherited SetBounds(R);

   // ask the memo the line number of the last char.
   memo :=  TMoveMemo(FEdit).Memo ;
   fDisplayedLines := Memo.Perform(EM_LINEFROMCHAR, fcharCount, 0) ;
   inc (fDisplayedLines) ;

   // if only one line , try to limit to 1 column
   if fDisplayedLines = 1 then begin
      p := Memo.Perform( messages.EM_POSFROMCHAR, memo.SelLength - 1, 0);    //   fcharCount
      if p <> -1 then begin
         pt := point(loWord(p),hiWord(p));
         r.Right := r.Left + pt.X + 45 ;
         // increase memo width to column if possible.
         if OldRight > r.right then
            r.right := oldRight ;
      end ;
   end ;

   //  calculate memo height to match the last line
   if fDisplayedLines < 2 then
      R.Bottom := R.Top + 4 + (ScrollHeight * 2) 
   else
      R.Bottom := R.Top + 4 + (ScrollHeight * fDisplayedLines) ;

   // Returns the intersection of two rectangles
   IntersectRect(R, TreeRect, R);

   // display at least 2 or 3 lines
   if (fDisplayedLines = 2) and ((r.Bottom-r.Top) < 4+(2*ScrollHeight)) then begin
      r.Top := r.Bottom - 4 - (2*ScrollHeight) ;
   end else if (fDisplayedLines >= 3) and ((r.Bottom-r.Top) < 4+(3*ScrollHeight)) then begin
      r.Top := r.Bottom - 4 - (3*ScrollHeight) ;
   end ;

   // add "move grip" header height
   if r.top > 10 then begin
      r.Top := r.Top - 10 ;
      r.Bottom := r.Bottom - 10 ;
   end ;

   // rechange memo height
   inherited SetBounds(R);
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.SetEditText(const Value: WideString);
var
   memo : TMemo ;
   copystr : string ;
   strings : TStringList ;

   function getStrings(source: pchar; count : integer): TStringList;
   var
      Ptr1,ptr2, ptr3 : pchar ;
   begin
      result := TStringList.create ;
      ptr2 := source ;
      ptr1 := ptr2 ;
      ptr3 := ptr1 + count ;

      // loop until all chars are processed
      while (ptr2 <= ptr3) do begin
         if (ptr2^ = #0) then begin
            if ptr1 <> ptr2 then begin
               ptr2^ := #0 ;
               result.Add(ptr1) ;
            end ;
            inc (ptr2,1) ;
            ptr1 := ptr2 ;
            //break ;
         end else if (ptr2^ = #13) and ((ptr2+1)^ = #10) then begin   // CR + LF
            ptr2^ := #0 ;
            result.Add(ptr1) ;
            inc (ptr2,2) ;
            ptr1 := ptr2 ;
         end else if (ptr2^ = #13) then begin                // CR
            ptr2^ := #0 ;
            result.Add(ptr1) ;
            inc (ptr2) ;
            ptr1 := ptr2 ;
         end else if (ptr2^ = #10) then begin                // LF
            ptr2^ := #0 ;
            result.Add(ptr1) ;
            inc (ptr2) ;
            ptr1 := ptr2 ;
         end else begin
            inc (ptr2);
         end ;
      end ;
   end ;

begin

   memo := TMoveMemo(FEdit).Memo ;
   // duplicate the string before cuting it
   copystr := Value ;
   strings := getStrings(pchar(copystr), length(copystr)); // : TStringList
   if strings.Count = 1 then
      memo.Text := Value
   else
      memo.Lines := strings ; 
   strings.Free ;
   memo.SelectAll;
   fcharCount := memo.SelLength ;
end;

//------------------------------------------------------------------------------

{ Utility functions }

//procedure RegisterEditLink(ALinkClass: TCustomEditLinkClass);
//begin
//  GlobalNameSpace.BeginWrite;
//  try
//    LinkClasses.Addobject(ALinkClass.GetName, TObject(ALinkClass));
//  finally
//    GlobalNameSpace.EndWrite;
//  end;
//end;
//
////------------------------------------------------------------------------------
//
//procedure RevokeEditLink(ALinkClass: TCustomEditLinkClass);
//var
//  I: Integer;
//begin
//  GlobalNameSpace.BeginWrite;
//  try
//    I := LinkClasses.IndexOf(ALinkClass.GetName);
//    if I >= 0 then
//      LinkClasses.Delete(I);
//  finally
//    GlobalNameSpace.EndWrite;
//  end;
//end;
//
////------------------------------------------------------------------------------
//
//function GetEditLinkClass(const Name: string): TCustomEditLinkClass;
//var
//  I: Integer;
//begin
//  GlobalNameSpace.BeginRead;
//  try
//    I := LinkClasses.IndexOf(Name);
//    if I >= 0 then
//      Result := TCustomEditLinkClass(LinkClasses.Objects[I])
//    else
//      Result := nil;
//  finally
//    GlobalNameSpace.EndRead;
//  end;
//end;
//
////------------------------------------------------------------------------------
//
//procedure GetEditLinkClasses(Strings: TStrings);
//begin
//  GlobalNameSpace.BeginRead;
//  try
//    Strings.AddStrings(LinkClasses);
//  finally
//    GlobalNameSpace.EndRead;
//  end;
//end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization

//  LinkClasses := TStringList.Create;
//  LinkClasses.Sorted := True;
//  LinkClasses.Duplicates := dupError;
//
//  RegisterEditLink(TMoveMemoEditLink);

finalization

//  FreeAndNil(LinkClasses);

end.
