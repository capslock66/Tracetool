{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_Editor;

interface

uses
  types,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, Buttons, ExtCtrls, ComCtrls, Spin;//, unit1;

{$I Compilers.inc}

const
  WM_STARTEDITING_TRACE  = WM_USER + 778;
  WM_STARTEDITING_MEMBER = WM_USER + 779;

type

  TSizeGrip = class ;
  TMoveMemo = class ;
  TMoveMemoEditLink = class ;
  TIntfOwnedPersistent = class ;
  TGripMemo = class ;
  TMoveGrip = class ;

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

  // the link to the editor (TMoveMemo)
  TMoveMemoEditLink = class(TInterfacedObject, IVTEditLink)    //  TIntfOwnedPersistent
  private
    fcharCount : integer ;
  private
    FNode: PVirtualNode;
    FColumn: Integer;
    FStopping: Boolean;
    FOldWndProc: TWndMethod;
  protected
    procedure EditWndProc(var Message: TMessage); virtual;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoExit(Sender: TObject); virtual;
    procedure StopEdit; virtual;
  public
    Tree: TVirtualStringTree;
    MoveMemo : TMoveMemo ;
    constructor create ;
    destructor  Destroy; override;
    procedure   SelectAll() ;
    function    IsVisible : boolean ;
  protected
    { IVTEditLink }
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
    function  PrepareEdit(ATree: TBaseVirtualTree; ANode: PVirtualNode; AColumn: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
  end;

  //----------------------------------------------------------------------------

  // the form editor. contains the memo and a move grip
  TMoveMemo = class(TForm)
    procedure CreateParams(var Params: TCreateParams); override ;
  public
    TopPanel: TPanel;
    Memo: TGripMemo;
    MoveGrip : TMoveGrip ;
    procedure FormResize(Sender: TObject);
  end;

  //------------------------------------------------------------------------------

  // the memo placed in the form editor
  TGripMemo = class(TMemo)
  private
    FGrip: TSizeGrip;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //------------------------------------------------------------------------------

  // Move his parent (TMoveMemo)
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

  //----------------------------------------------------------------------------

  // resize his parent : TGripMemo
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

  //----------------------------------------------------------------------------


implementation

uses TypInfo;

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


type
  TWinControlAccess = class(TWinControl);

//------------------------------------------------------------------------------

{ TMoveMemoEditLink }

constructor TMoveMemoEditLink.create;
begin
   inherited ;
   MoveMemo := nil ;
end;

//------------------------------------------------------------------------------

{ IVTEditLink }
function TMoveMemoEditLink.PrepareEdit(ATree: TBaseVirtualTree;
  ANode: PVirtualNode; AColumn: TColumnIndex): Boolean;
var
   copystr  : string ;
   strings  : TStringList ;
   TextType : TVSTTextType;
   Text     : UnicodeString;

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
   Result := not FStopping;
   if Result = false then
      exit ;

   if (MoveMemo <> nil) and (MoveMemo.Visible) and (tree <> nil) then
       Tree.CancelEditNode ;

   Tree := TVirtualStringTree(ATree) ;
   FNode := ANode;
   FColumn := AColumn;
   //FreeAndNil(MoveMemo);

   if MoveMemo = nil then begin
      MoveMemo := TMoveMemo.CreateNew(nil,0);     // form based
      MoveMemo.OnResize := MoveMemo.FormResize ;

      // create a top panel
      MoveMemo.TopPanel := TPanel.create (MoveMemo) ;
      MoveMemo.TopPanel.Height := 10 ;
      MoveMemo.TopPanel.color := clGreen ;
      MoveMemo.TopPanel.Align := alTop ;
      MoveMemo.TopPanel.parent := MoveMemo ;
      MoveMemo.TopPanel.BevelOuter := bvNone ;

      // create a move grip inside the Panel
      MoveMemo.MoveGrip := TMoveGrip.Create(MoveMemo);
      MoveMemo.MoveGrip.Parent := MoveMemo.TopPanel ;
      MoveMemo.MoveGrip.Visible := True;

      // create a memo with an attached TSizeGrip
      MoveMemo.Memo := TGripMemo.Create(MoveMemo);
      MoveMemo.Memo.parent := MoveMemo ;
      MoveMemo.Memo.Align := alclient ;
      MoveMemo.Memo.Ctl3d := False;
      MoveMemo.Memo.ReadOnly := true ; //FReadOnly;
      MoveMemo.Memo.WordWrap := True;
      MoveMemo.Memo.ScrollBars := ssVertical ;

      MoveMemo.OnKeyDown := KeyDown;
      MoveMemo.OnExit := DoExit;

      MoveMemo.Visible := False;
   end ;

   MoveMemo.Parent := Tree;
   MoveMemo.BoundsRect := Tree.GetDisplayRect(FNode, FColumn, False);

   // get the real text for the node. to do that, call the OnGetText with the ttStatic flag.
   TextType := ttStatic ;
   if Assigned(Tree.OnGetText) then
      Tree.OnGetText(Tree, fNode, fColumn, TextType, Text);

   // duplicate the string before cuting it
   copystr := Text ;
   strings := getStrings(pchar(copystr), length(copystr)); // : TStringList
   if strings.Count = 1 then
      MoveMemo.memo.Text := Text
   else
      MoveMemo.memo.Lines := strings ;
   strings.Free ;
   MoveMemo.memo.SelectAll;
   fcharCount := MoveMemo.memo.SelLength ;
end;

//------------------------------------------------------------------------------

{ IVTEditLink }
function TMoveMemoEditLink.BeginEdit: Boolean;
var
  F: TCustomForm;
begin
  Result := not FStopping;
  if Result then
  begin
    MoveMemo.Show;
    F := GetParentForm(Tree);
    if Assigned(F) and F.Active and Tree.Focused then
      MoveMemo.SetFocus;
    FOldWndProc := MoveMemo.WindowProc;
    MoveMemo.WindowProc := EditWndProc;
  end;
end;

//------------------------------------------------------------------------------

{ IVTEditLink }
function TMoveMemoEditLink.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    TWinControlAccess(MoveMemo).OnExit := nil;
    MoveMemo.Hide;
    Tree.CancelEditNode;
    StopEdit;
  finally
    FStopping := False;
  end;
end;

//------------------------------------------------------------------------------

{ IVTEditLink }
function TMoveMemoEditLink.EndEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    MoveMemo.Perform(CM_EXIT, 0, 0);
    Tree.EndEditNode;
    StopEdit;
  finally
    FStopping := False;
  end;
end;

//------------------------------------------------------------------------------

{ IVTEditLink }
function TMoveMemoEditLink.GetBounds: TRect;
begin
  Result := MoveMemo.BoundsRect;
end;

//------------------------------------------------------------------------------

{ IVTEditLink }
procedure TMoveMemoEditLink.SetBounds(R: TRect);
var
   DisplayedLines : Longint ;
   TreeRect: TRect;
   memo : TMemo ;
   Pt: TPoint;
   p : integer;
   oldRight : integer ;
   ScrollHeight : integer ;
begin
   ScrollHeight := GetSystemMetrics(SM_CYHSCROLL);

   oldRight := r.right ;
   TreeRect := Tree.ClientRect;

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
   MoveMemo.BoundsRect := R;

   // ask the memo the line number of the last char.
   memo :=  MoveMemo.Memo ;
   DisplayedLines := Memo.Perform(EM_LINEFROMCHAR, fcharCount, 0) ;
   inc (DisplayedLines) ;

   // if only one line , try to limit to 1 column
   if DisplayedLines = 1 then begin
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
   if DisplayedLines < 2 then
      R.Bottom := R.Top + 4 + (ScrollHeight * 2)
   else
      R.Bottom := R.Top + 4 + (ScrollHeight * DisplayedLines) ;

   // Returns the intersection of two rectangles
   IntersectRect(R, TreeRect, R);

   // display at least 2 or 3 lines
   if (DisplayedLines = 2) and ((r.Bottom-r.Top) < 4+(2*ScrollHeight)) then begin
      r.Top := r.Bottom - 4 - (2*ScrollHeight) ;
   end else if (DisplayedLines >= 3) and ((r.Bottom-r.Top) < 4+(3*ScrollHeight)) then begin
      r.Top := r.Bottom - 4 - (3*ScrollHeight) ;
   end ;

   // add "move grip" header height
   if r.top > 10 then begin
      r.Top := r.Top - 10 ;
      r.Bottom := r.Bottom - 10 ;
   end ;

   // rechange memo height
   MoveMemo.BoundsRect := R;
end;

//------------------------------------------------------------------------------


{ IVTEditLink }
procedure TMoveMemoEditLink.ProcessMessage(var Message: TMessage);
begin
  MoveMemo.WindowProc(Message);
end;

//------------------------------------------------------------------------------

// will be called when reference counting become 0 ;
destructor TMoveMemoEditLink.Destroy;
begin
  FreeAndNil(MoveMemo);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.SelectAll;
begin
   MoveMemo.Memo.SetFocus ;
   MoveMemo.Memo.SelStart := 0;
   MoveMemo.Memo.SelLength := Length(MoveMemo.Memo.lines.Text);
end;

//------------------------------------------------------------------------------

function TMoveMemoEditLink.IsVisible: boolean;
begin
   result := MoveMemo.Visible ;  //   Memo.
end;

//------------------------------------------------------------------------------

// MoveMemo.WindowProc
procedure TMoveMemoEditLink.EditWndProc(var Message: TMessage);
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

procedure TMoveMemoEditLink.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Tree.CancelEditNode;
        Tree.SetFocus;
      end;
    VK_RETURN:
      if Shift = [] then
      begin
        Tree.EndEditNode;
        Tree.SetFocus;
      end
      else
        Exit;
  else
    Exit;
  end;
  Key := 0;
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.DoExit(Sender: TObject);
begin
  if not FStopping then Tree.EndEditNode;
end;

//------------------------------------------------------------------------------

procedure TMoveMemoEditLink.StopEdit;
begin
  if Assigned(MoveMemo) then
  begin
    MoveMemo.Hide;
    MoveMemo.WindowProc := FOldWndProc;
    MoveMemo.Parent := nil;
  end;
end;

//------------------------------------------------------------------------------

{TMoveMemo : the form editor. contains the memo and a move grip}

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

{TGripMemo : the memo placed in the form editor }

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

{TSizeGrip}

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

{TMoveGrip : Move his parent (TMoveMemo) }

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




end.
