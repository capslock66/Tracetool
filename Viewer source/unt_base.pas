{
  Base windows for all trace type
  ===============================

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   

}

unit unt_base;

interface

uses
  system.Contnrs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, unt_PageContainer, VirtualTrees, unt_tool, Menus, unt_filter;

type
  // base class for all forms : ODS, Eventlog , FrmTail and Frm_Trace
  TFrmBase = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Offset : TPoint ;
  public
    { Public declarations }
    VST : TVirtualStringTree ;
    Filter : TFrmFilter ;
    bookmarks : TList ;
    IsDestroying : boolean ;
    OldPageControl : TDockingPagecontrol ;
    procedure DockToMainPanel;
    procedure UnDock;
    function  getTabSheet: TTabSheet;
    function  getPageControl: TDockingPagecontrol;
    function  getPageContainer : TFrmPageContainer ;
    procedure SetActivePage ;

    procedure PageControlChange (); virtual ; abstract ;
    //procedure PageControlChanging (); virtual ; abstract ;

    procedure ClearWin ;          virtual ; abstract ;
    procedure SaveWin ;           virtual ; abstract ;
    procedure PauseWin ;          virtual ; abstract ;
    procedure ViewTraceInfo ;     virtual ; abstract ;
    procedure CopySelected ;      virtual ; abstract ;
    procedure CopyCurrentCell ;   virtual ; abstract ;
    procedure DeleteSelected ;    virtual ; abstract ;
    procedure SelectAll ;         virtual ; abstract ;
    procedure CheckAutoClear ;    virtual ; abstract ;
    procedure TimerInfo ;         virtual ; abstract ;
    procedure CloseWin ;          virtual ; abstract ;
    procedure ResizeColumns ;     virtual ; abstract ;
    procedure ViewProperty ;      virtual ; abstract ;
    procedure RefreshView ;       virtual ; abstract ;
    procedure ShowFilter ;        virtual ; abstract ;
    procedure ApplyFont ;         virtual ; abstract ;
    procedure Print ;             virtual ; abstract ;
    function  getMembers (Node : PVirtualNode) : TMember ;  virtual ; abstract ;
    function  SearchNext (start:boolean) : boolean ;  virtual ; abstract ;
    function  SearchPrevious (start:boolean) : boolean ;  virtual ; abstract ;
   end;

var
  FrmBase: TFrmBase;

  ActiveTracePage : TFrmBase ;

implementation

uses
Types
,unt_TraceWin
,unt_TraceConfig
;

{$R *.dfm}


procedure TFrmBase.FormCreate(Sender: TObject);
begin
   bookmarks    := TList.create () ;
   IsDestroying := false ;
   BaseList.Add (self) ;
end;

//------------------------------------------------------------------------------

procedure TFrmBase.FormClose(Sender: TObject; var Action: TCloseAction);
var
   Tab : TTabsheet ;
   PageControl : TDockingPagecontrol ;
   NextPage : TTabsheet ;

   // special PageControl.FindNextPage function with no loop.
   function FindNextPage(CurPage: TTabSheet; GoForward : boolean): TTabSheet;
   var
     I, StartIndex: Integer;
   begin
     if PageControl.PageCount <> 0 then
     begin

       StartIndex := CurPage.PageIndex;
       if StartIndex = -1 then
         if GoForward then
            StartIndex := PageControl.PageCount - 1
         else
            StartIndex := 0;

       I := StartIndex;
       repeat
         if GoForward then
         begin
           Inc(I);
           if I = PageControl.PageCount then begin   // stop loop : no next
              result := nil ;
              exit ;
           end ;
         end else
         begin
           if I = 0 then begin                       // stop loop : no previous
              result := nil ;
              exit ;
           end ;
           Dec(I);
         end;
         Result := PageControl.Pages [I];
         if Result.TabVisible then
            Exit;
       until I = StartIndex;
     end;
     Result := nil;
   end;

begin
   if VstEditor <> nil then begin
      if VstEditor.Tree = vst then
         VstEditor.Tree := nil ;
   end ;

   FreeAndNil (bookmarks);
   IsDestroying := true ;
   BaseList.Remove (self) ;
   if parent = nil then
      exit ;

   tab := TTabsheet(parent) ;
   PageControl := TDockingPagecontrol(tab.PageControl) ;

   // self.ManualDock (nil,nil,alClient) ;  // undock

   // switch to another page before closing it
   NextPage := FindNextPage (tab,true) ;        // get next visible page
   if NextPage = nil then                       // no next page ?
      NextPage := FindNextPage (tab,false) ;    // get previous page

   PageControl.ActivePage := NextPage ;
   PageControl.MyPageControlChange(nil);        // force redesign toolbar and menu for the new active page
end;

//------------------------------------------------------------------------------

procedure TFrmBase.DockToMainPanel ;
begin
   try
      ManualDock(unt_tool.MainPageContainer.DockingPagecontrol, nil, alClient);
   except
   end ;
end ;

//------------------------------------------------------------------------------

procedure TFrmBase.UnDock ;
begin
   ManualDock(nil, nil, alClient);
end ;

//------------------------------------------------------------------------------
function TFrmBase.getPageContainer: TFrmPageContainer;
var
   Tab : TTabsheet ;
   PageControl : TPageControl ;
begin
   result := nil ;
   if parent = nil then
      exit ;

   tab := TTabsheet(parent) ;
   PageControl := tab.PageControl ;
   if not (PageControl is TDockingPagecontrol) then
      exit ;

   result := TDockingPagecontrol(PageControl).container ;
end;

//------------------------------------------------------------------------------

function  TFrmBase.getTabSheet: TTabSheet;
begin
   result := nil ;
   if parent = nil then
      exit ;

   result := TTabsheet(parent) ;
end ;

//------------------------------------------------------------------------------

function TFrmBase.getPageControl : TDockingPagecontrol ;
var
   Tab : TTabsheet ;
   PageControl : TPageControl ;
begin
   result := nil ;
   if parent = nil then
      exit ;

   tab := TTabsheet(parent) ;
   PageControl := tab.PageControl ;
   if not (PageControl is TDockingPagecontrol) then
      exit ;
   result := TDockingPagecontrol(PageControl) ;
end ;

//------------------------------------------------------------------------------

procedure TFrmBase.SetActivePage;
var
   Tab : TTabsheet ;
   PageControl : TDockingPagecontrol ;
begin
   if parent = nil then
      exit ;

   tab := TTabsheet(parent) ;
   PageControl := TDockingPagecontrol(tab.PageControl) ;
   PageControl.ActivePage := tab ;
   PageControl.MyPageControlChange(nil);
   PageControl.container.BringToFront() ;
end;

//------------------------------------------------------------------------------

procedure TFrmBase.FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
var
   //DragObjectstr : string ;
   //selfStr : string ;
   P: TPoint;
begin
   //selfStr := 'self:' + inttostr (integer(self))+'.' ;
   //
   //if DragObject = nil then
   //   DragObjectstr := 'nil'
   //else
   //   DragObjectstr := DragObject.classname ;
   //TFrm_Trace.InternalTrace (selfStr+'TFrmBase StartDock (Sender, var DragObject:' + DragObjectstr + ')'
   //                          + ' Parent PageControl : ' + inttostr(integer(getPageControl()))  ) ;


   GetCursorPos(P);
   P := getPageControl.ScreenToClient(P);
   Offset := p ;  // used by onEndDock

   OldPageControl := getPageControl() ;
end;

//------------------------------------------------------------------------------

// create container if undocking
procedure TFrmBase.FormEndDock(Sender, Target: TObject; X, Y: Integer);
var
   NewContainer : TFrmPageContainer ;
   //TargetName : string ;
   //selfStr : string ;
   Container : TFrmPageContainer ;
begin
   //selfStr := 'self:' + inttostr (integer(self))+'.' ;
   //
   //if Target = nil then
   //   TargetName := 'null'
   //else
   //   TargetName := Target.ClassName ;
   //
   //TFrm_Trace.InternalTrace (selfStr+'TFrmBase EndDock (Sender, Target:' + TargetName + ', x:' + inttostr(x) + ',y:' + inttostr(y)+')') ;

   if parent = nil then begin
      NewContainer := TFrmPageContainer.Create(Frm_Tool);
      NewContainer.Left   := x + 12 - offset.X ;             // offset is calculate in FormStartDock
      NewContainer.Top    := y ; // - offset.y;
      NewContainer.Show ;

      // set toolbar buttons size and caption
      NewContainer.ToolBar.ShowCaptions := not TraceConfig.AppDisplay_SmallBut;
      if TraceConfig.AppDisplay_SmallBut = true then
         NewContainer.ToolBar.ButtonHeight := 22
      else
         NewContainer.ToolBar.ButtonHeight := 36 ;

      ManualDock (NewContainer.DockingPagecontrol, nil, alClient);
   end ;

   if OldPageControl <> nil then begin
      // Call PageControlChange for source drag
      OldPageControl.MyPageControlChange(nil);
   end ;

   Container := getPageContainer() ;
   Container.PanelPageControl.Parent.BringToFront ;

   if getPageControl() <> OldPageControl then begin
      //Call PageControlChange for target drag
      getPageControl.MyPageControlChange (nil);   // add special action in the current container
      // change container caption if not the main container
      getPageControl.SetContainerCaption() ;
      if OldPageControl <> nil then
         OldPageControl.SetContainerCaption() ;
   end ;

   //TFrm_Trace.InternalTrace (selfStr+'TFrmBase EndDock done ') ;
end;

//------------------------------------------------------------------------------

end.
