{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_PageContainer;

interface

uses
  system.Contnrs, types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,VirtualTrees ,
  Dialogs, ComCtrls, ToolWin, ActnList, ImgList, ExtCtrls, Menus, CommCtrl,pscMenu,
  System.Actions;

type
  TDockingPagecontrol = class ;

  TFrmPageContainer = class(TForm)
    Actions: TActionList;
    actCopy: TAction;
    actClear: TAction;
    actSelectAll: TAction;
    actSaveToFile: TAction;
    actDelete: TAction;
    actCut: TAction;
    actClearFileContent: TAction;
    actViewProperty: TAction;
    PanelPageControl: TPanel;
    ToolBar: TToolBar;
    tbnClear: TToolButton;
    tbnCopy: TToolButton;
    tbnSave: TToolButton;
    SepStandard: TToolButton;
    tbnPause: TToolButton;
    tbnTraceInfo: TToolButton;
    actPause: TAction;
    actViewTraceInfo: TAction;
    actResizeCols: TAction;
    MainMenu: TMainMenu;
    MnuAction: TMenuItem;
    CutSelectedLines1: TMenuItem;
    CopySelectedLines1: TMenuItem;
    DeleteSelected1: TMenuItem;
    SelectAll1: TMenuItem;
    actResizeCols1: TMenuItem;
    N3: TMenuItem;
    ViewTraceInfo1: TMenuItem;
    Property1: TMenuItem;
    actClearFileContent1: TMenuItem;
    N4: TMenuItem;
    Pause1: TMenuItem;
    Save1: TMenuItem;
    ClearWindow1: TMenuItem;
    Close1: TMenuItem;
    actCloseWin: TAction;
    actSearch: TAction;
    actFindNext: TAction;
    actSearch1: TMenuItem;
    actFindNext1: TMenuItem;
    actClearHighlight: TAction;
    ClearHighlight1: TMenuItem;
    actFilter: TAction;
    actFilter1: TMenuItem;
    actClearBookmarks: TAction;
    actPreviousBookmark: TAction;
    actNextBookmark: TAction;
    N1: TMenuItem;
    N2: TMenuItem;
    N5: TMenuItem;
    actNextBookmark1: TMenuItem;
    actClearBookmarks1: TMenuItem;
    actToggleBookmark: TAction;
    oggleBookmark1: TMenuItem;
    actFindPrevious: TAction;
    actFindPrevious1: TMenuItem;
    PreviousBookmark1: TMenuItem;
    actCopyCurrentCell: TAction;
    actClearFilter: TAction;
    ClearFilter1: TMenuItem;
    SepSearch: TToolButton;
    tbnSearch: TToolButton;
    tbnSearchNext: TToolButton;
    tbnSearchPrevious: TToolButton;
    SepBookmark: TToolButton;
    tbnClearHighlight: TToolButton;
    tbnToggleBookmark: TToolButton;
    tbnBookmarkNext: TToolButton;
    tbnBookmarkPrevious: TToolButton;
    tbnClearBookmark: TToolButton;
    SepFilter: TToolButton;
    tbnFilter: TToolButton;
    tbnClearFilter: TToolButton;
    actPrint: TAction;
    actPrint1: TMenuItem;
    ToolButton1: TToolButton;

    procedure FormCreate(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSaveToFileExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actClearFileContentExecute(Sender: TObject);
    procedure actViewPropertyExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actViewTraceInfoExecute(Sender: TObject);
    procedure actResizeColsExecute(Sender: TObject);
    procedure actCloseWinExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSearchExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actClearHighlightExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actClearBookmarksExecute(Sender: TObject);
    procedure actPreviousBookmarkExecute(Sender: TObject);
    procedure actNextBookmarkExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actToggleBookmarkExecute(Sender: TObject);
    procedure actCopyCurrentCellExecute(Sender: TObject);
    procedure actClearFilterExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
  protected
    procedure CreateParams(var Params : TCreateParams) ; override ;
  private
    { Private declarations }
  public
     DockingPagecontrol : TDockingPagecontrol ;
     procedure configureToolbar ;
  end;

  TDockingPagecontrol = class (TPageControl)
  private
      DockMoveAllowed : boolean ;
      procedure CMDockClient(var Message: TCMDockClient);  message CM_DOCKCLIENT;
      procedure WMLButtonDblClk(var Message: TWMLButtonDblClk) ; message WM_LBUTTONDBLCLK;
      function GetDockClientFromMousePos(MousePos: TPoint): TControl;
      procedure resetToolbarAndMenu;
   public
      Toolbar : TToolBar ;
      container : TFrmPageContainer ;
      constructor Create(AOwner: TComponent); override;
      procedure SetContainerCaption ;
      function ComputeDockingRect(var DockRect: TRect;  MousePos: TPoint; var NewTabSheetPos : integer): TAlign;
      function GetVisibleClientCount() : integer;
      // event
      procedure MyDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
      procedure MyUndock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
      procedure MyPageControlChange(Sender: TObject);
      procedure MyGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
  end ;


  //----------------------------------------------------------------------------

//var
//  frmPageContainer: TFrmPageContainer;

implementation

uses unt_base , unt_tail , unt_tool , unt_TraceWin , unt_search
  ,unt_filter
  ,unt_TraceConfig;

{$R *.dfm}

{TFrmPageContainer}


procedure TFrmPageContainer.FormCreate(Sender: TObject);
begin
   with TPSCMenu.create (self) do begin
      DimLevel := 0 ;    // don't gray icon
      Active := true ;
   end ;
   ContainerList.add (self) ;
   DockingPagecontrol := TDockingPagecontrol.create (self);
   DockingPagecontrol.Toolbar := ToolBar ;
   DockingPagecontrol.container := self ;
   DockingPagecontrol.Parent := PanelPageControl ; // self ;
end;


//------------------------------------------------------------------------------

procedure TFrmPageContainer.FormClose(Sender: TObject;  var Action: TCloseAction);
var
   base : TFrmBase ;
begin
   if DockingPagecontrol.DockClientCount = 0 then begin
    ContainerList.Remove (self);
    Action := caFree;
  end else begin
    Action := caNone ; // (caNone, caHide, caFree, caMinimize);
    // move clients to main. DockingPagecontrol.OnUndock will check that no more clients are attached and will close the form.
    while DockingPagecontrol.DockClientCount <> 0 do begin
       base := TFrmBase(DockingPagecontrol.DockClients[0]) ;
       base.DockToMainPanel ;
    end ;
  end ;
end;

//------------------------------------------------------------------------------

// CTRL C : Copy selected

procedure TFrmPageContainer.actCopyExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.CopySelected ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actCopyCurrentCellExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.CopyCurrentCell ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actClearExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.ClearWin ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actPrintExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.Print ;
end;

//------------------------------------------------------------------------------

// CTRL A : Select all
procedure TFrmPageContainer.actSelectAllExecute(Sender: TObject);
var
   ctrl : TControl ;
   Base : TFrmBase ;
begin
   // a lot of error catching here : a strange and difficult to reproduce bug can appears her

   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := nil ;
   try
      ctrl := DockingPagecontrol.ActivePage.controls[0] ;
      if ctrl = nil then begin
         TFrm_Trace.InternalTrace('ActivePage control[0] is nil') ;
         exit ;
      end ;
      if ctrl is TFrmBase then
         Base := TFrmBase (ctrl)
      else begin
         TFrm_Trace.InternalTrace('ActivePage control[0] is not TFrmBase : ', ctrl.ClassName) ;
         exit ;
      end ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace('Error while getting the ActivePage', e.Message) ;
         ShowMessage(e.Message);
      end ;
   end ;

//   try
      Base.SelectAll ;
//   except
//      on e : exception do begin
//         TFrm_Trace.InternalTrace('Error while calling SelectAll on base form', e.Message) ;
//         ShowMessage(e.Message);
//      end ;
//   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actSaveToFileExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.SaveWin ;
end;

//------------------------------------------------------------------------------
// Delete key : deleted selected

procedure TFrmPageContainer.actDeleteExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.DeleteSelected ;
end;

//------------------------------------------------------------------------------
// CTRL X : Cut
procedure TFrmPageContainer.actCutExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.CopySelected ;
   Base.DeleteSelected ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actClearFileContentExecute(Sender: TObject);
var
   Tail : TFrmTail ;
begin
   Tail := TFrmTail (DockingPagecontrol.ActivePage.controls[0]) ;
   Tail.ClearFileContent ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actViewPropertyExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.ViewProperty ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actPauseExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   actPause.Checked := not actPause.Checked;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.PauseWin ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actViewTraceInfoExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   actViewTraceInfo.Checked := not actViewTraceInfo.Checked ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.ViewTraceInfo ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actResizeColsExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.ResizeColumns ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actCloseWinExecute(Sender: TObject);
var
   Base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   Base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   Base.CloseWin ;
end;

//------------------------------------------------------------------------------

// Filter

procedure TFrmPageContainer.actFilterExecute(Sender: TObject);
var
   base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   base.ShowFilter ;
end;


//------------------------------------------------------------------------------

procedure TFrmPageContainer.actClearFilterExecute(Sender: TObject);
var
   base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   if base.Filter = nil then
      exit ;
   base.Filter.ResetFilter ;
end;

//------------------------------------------------------------------------------

// Search : CTRL-F
procedure TFrmPageContainer.actSearchExecute(Sender: TObject);
var
   Base : TFrmBase ;
   c : integer ;
   oldModalResult : integer ;
   oldSearch : string ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   oldModalResult  := SearchKind ;
   oldSearch       := FrmSearch.EditSearch.Text ;
   FrmSearch.EditSearch.SelectAll ;
   FrmSearch.ShowModal ;

   if FrmSearch.ModalResult = mrCancel then begin
      FrmSearch.EditSearch.Text := oldSearch ;
      FrmSearch.ModalResult := oldModalResult ;
      exit ;
   end ;

   ActiveTracePage     := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   MatchCase           := FrmSearch.chkMatchCase.Checked ;
   MatchWholeWord      := FrmSearch.chkMatchWholeWord.Checked ;
   SearchInAllPages    := FrmSearch.rbAllDocs.Checked ;
   SearchText          := FrmSearch.EditSearch.Text ;
   LenSearchText       := length (SearchText) ;
   UpperCaseSearchText := AnsiUpperCase(SearchText) ;
   SearchKind := FrmSearch.ModalResult ;

   // search the next record
   if FrmSearch.ModalResult = mrYes then
      actFindNextExecute (sender) ;

   // reset highlight : refresh the differents gutters
   for c := 0 to BaseList.Count -1 do begin
      Base := TFrmBase (BaseList[c]) ;
      Base.RefreshView ;
   end ;
end;

//------------------------------------------------------------------------------

// F3
procedure TFrmPageContainer.actFindNextExecute(Sender: TObject);
var
   Base : TFrmBase ;
   found : boolean ;
   c : integer ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   ActiveTracePage := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;

   // search in the current page
   found := ActiveTracePage.SearchNext(false) ;

   if found then
      exit ;

   // if not found and flag "search in all documents" is set then search in the next page
   if unt_search.SearchInAllPages = false then
      exit ;

   // search the indice of the ActiveTracePage
   for c := 0 to BaseList.Count -1 do begin
      Base := TFrmBase (BaseList[c]) ;
      if base = ActiveTracePage then
         break ;
   end ;
   
   // get the next page
   if c = BaseList.Count -1 then
      c := 1
   else
      c := c + 1 ;

   // loop over all pages to find the text
   while true do begin
      Base := TFrmBase (BaseList[c]) ;
      found := Base.SearchNext (true);    // search in the entire document
      if found then
         exit ;
      inc (c) ;
      if c >= BaseList.Count then
         c := 0 ;
      Base := TFrmBase (BaseList[c]) ;
      // if the page is the same as ActiveTracePage then we have already checked all pages. Stop search
      if Base = ActiveTracePage then
         exit ;  // no more found
   end ;

   // still not found. Scan the current document from the first line
   ActiveTracePage.SearchNext(true) ;   // search in the entire document (current)
end;

//------------------------------------------------------------------------------
// Shift F3 : previous search (same page)
procedure TFrmPageContainer.actFindPreviousExecute(Sender: TObject);
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;
   ActiveTracePage := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;

   // search in the current page
   ActiveTracePage.SearchPrevious(false) ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actClearHighlightExecute(Sender: TObject);
var
   Base : TFrmBase ;
   c : integer ;
begin
   FrmSearch.ModalResult := mrYes ; // convert to simple search
   SearchKind := FrmSearch.ModalResult ;
   // reset highlight : refresh the differents gutters
   for c := 0 to BaseList.Count -1 do begin
      Base := TFrmBase (BaseList[c]) ;
      Base.RefreshView ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actClearBookmarksExecute(Sender: TObject);
var
   base : TFrmBase ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   base.bookmarks.Clear ;
   base.VST.Invalidate ;
end;


//------------------------------------------------------------------------------

procedure TFrmPageContainer.actToggleBookmarkExecute(Sender: TObject);
var
   SelectedNodes: TNodeArray;
   currentNode : PVirtualNode ;
   c , index : integer ;
   notBookmark : boolean ;
   base : TFrmBase ;
begin
   SelectedNodes := nil;
   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;

   notBookmark := false ;
   SelectedNodes := base.VST.GetSortedSelection(false);
   // fisr check if one node is not yet bookmarked
   for c := 0 to High(SelectedNodes) do begin
      index := base.bookmarks.IndexOf(SelectedNodes[c]) ;
      if index = -1 then begin
         notBookmark := true ;
         break ;
      end ;
   end;

   if notBookmark = true then begin
      // set all selected as bookmark
      for c := 0 to High(SelectedNodes) do begin
         currentNode := SelectedNodes[c] ;
         index := base.bookmarks.IndexOf(currentNode) ;
         if index = -1 then
            base.bookmarks.Add(currentNode) ;
      end;
   end else begin
      // remove all selected from bookmark
      for c := 0 to High(SelectedNodes) do begin
         currentNode := SelectedNodes[c] ;
         index := base.bookmarks.IndexOf(currentNode) ;
         if index <> -1 then
            base.bookmarks.Delete(index) ;
      end;
   end ;
   base.VST.Invalidate ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actPreviousBookmarkExecute(Sender: TObject);
var
   base : TFrmBase ;
   currentNode : PVirtualNode ;
   procedure CheckVisible () ;
   begin
      while (currentNode <> nil) and (base.VST.IsVisible[currentNode] = false) do begin
         currentNode := base.VST.GetPrevious(currentNode) ;
      end ;
   end ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   if base.Visible = false then
      exit ;

   currentNode := base.VST.GetFirstSelected ;
   if currentNode = nil then
      currentNode := base.VST.GetLastVisible()
   else
      currentNode := base.VST.GetPrevious(currentNode) ;   // skip the first selected

   CheckVisible() ;
   while currentNode <> nil do begin
      if base.bookmarks.IndexOf(currentNode) <> -1 then begin
         if ActiveTracePage <> base then
            base.SetActivePage() ;
         // fully visible ?
         base.VST.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         base.VST.ClearSelection;
         base.VST.Selected [currentNode] := true ;
         base.VST.SetFocus() ;
         exit ;
      end ;
      currentNode := base.VST.GetPrevious(currentNode) ;
      CheckVisible() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.actNextBookmarkExecute(Sender: TObject);
var
   base : TFrmBase ;
   currentNode : PVirtualNode ;

   procedure CheckVisible () ;
   begin
      while (currentNode <> nil) and (base.VST.IsVisible[currentNode] = false) do begin
         currentNode := base.VST.GetNext(currentNode) ;
      end ;
   end ;
begin
   if DockingPagecontrol.ActivePage = nil then
      exit ;

   base := TFrmBase (DockingPagecontrol.ActivePage.controls[0]) ;
   if base.Visible = false then
      exit ;

   currentNode := base.VST.GetFirstSelected ;
   if currentNode = nil then
      currentNode := base.VST.GetFirstVisible()
   else  // when start is false, we are searching in the current document
      currentNode := base.VST.GetNext(currentNode) ;   // skip the first selected

   CheckVisible() ;
   while currentNode <> nil do begin
      if base.bookmarks.IndexOf(currentNode) <> -1 then begin
         if ActiveTracePage <> base then
            base.SetActivePage() ;
         // fully visible ?
         base.VST.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         base.VST.ClearSelection;
         base.VST.Selected [currentNode] := true ;
         base.VST.SetFocus() ;
         exit ;
      end ;
      currentNode := base.VST.GetNext(currentNode) ;
      CheckVisible() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.CreateParams(var Params: TCreateParams);
begin
   inherited ;
   Params.style := Params.style or WS_CAPTION ;
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW ;
   Params.WndParent := 0 ;   // window is independant of the main window (for example minimize)
end;

//------------------------------------------------------------------------------

procedure TFrmPageContainer.configureToolbar;
var
   ToolbarStandard : boolean ;
   ToolbarSearch   : boolean ;
   ToolbarBookmark : boolean ;
   ToolbarFilter   : boolean ;
   IsFirst : boolean ;
begin

   ToolbarStandard := traceConfig.AppDisplay_ToolbarStandard ;
   ToolbarSearch   := traceConfig.AppDisplay_ToolbarSearch ;
   ToolbarBookmark := traceConfig.AppDisplay_ToolbarBookmark ;
   ToolbarFilter   := traceConfig.AppDisplay_ToolbarFilter ;

   if (ToolbarStandard = false) and (ToolbarSearch = false) and (ToolbarBookmark = false) and (ToolbarFilter = false) then begin
      Toolbar.Visible := false ;
      exit ;
   end ;

   Toolbar.Visible := true ;

   ToolBar.ShowCaptions := not TraceConfig.AppDisplay_SmallBut ;

   if TraceConfig.AppDisplay_SmallBut = true then
      ToolBar.ButtonHeight := 22
   else
      ToolBar.ButtonHeight := 36 ;

   IsFirst := true ;
   if ToolbarStandard then
      IsFirst := false ;

   tbnClear     .Visible := ToolbarStandard ;
   tbnCopy      .Visible := ToolbarStandard ;
   tbnSave      .Visible := ToolbarStandard ;
   SepStandard  .Visible := ToolbarStandard ;
   tbnPause     .Visible := ToolbarStandard ;
   tbnTraceInfo .Visible := ToolbarStandard ;

   if (Isfirst) and (ToolbarBookmark) then begin
      IsFirst := false ;
      SepBookmark.Visible := false ;
   end else if ToolbarBookmark then begin   // not the first toolbar
      SepBookmark.Visible := true ;
   end else                               // no ToolbarBookmark
      SepBookmark.Visible := false ;

   tbnToggleBookmark    .Visible := ToolbarBookmark ;
   tbnBookmarkNext      .Visible := ToolbarBookmark ;
   tbnBookmarkPrevious  .Visible := ToolbarBookmark ;
   tbnClearBookmark     .Visible := ToolbarBookmark ;

   if (Isfirst) and (ToolbarSearch) then begin
      IsFirst := false ;
      SepSearch.Visible := false ;
   end else if ToolbarSearch then begin   // not the first toolbar
      SepSearch.Visible := true ;
   end else                               // no ToolbarSearch
      SepSearch.Visible := false ;

   tbnSearch         .Visible := ToolbarSearch ;
   tbnSearchNext     .Visible := ToolbarSearch ;
   tbnSearchPrevious .Visible := ToolbarSearch ;
   tbnClearHighlight .Visible := ToolbarSearch ;

   if (Isfirst) and (ToolbarFilter) then begin
      SepFilter.Visible := false ;
   end else if ToolbarFilter then begin   // not the first toolbar
      SepFilter.Visible := true ;
   end else                               // no ToolbarFilter
      SepFilter.Visible := false ;

   tbnFilter      .Visible := ToolbarFilter ;
   tbnClearFilter .Visible := ToolbarFilter ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{TDockingPagecontrol}

constructor TDockingPagecontrol.Create(AOwner: TComponent);
begin
   inherited;
   Width := 10 ;
   align := alClient ;
   DockSite := true ;
   MultiLine := true ;
   DragMode :=  dmManual;
   HotTrack := true ;

   // events
   OnDockOver    := MyDockOver;
   OnUnDock      := MyUndock ;
   OnChange      := MyPageControlChange ;
   OnGetSiteInfo := MyGetSiteInfo ;
end;

//------------------------------------------------------------------------------

procedure TDockingPagecontrol.resetToolbarAndMenu ;
var
   MnuAction : TMenuItem ;
   c : integer ;
   itemMenu : TMenuItem ;
begin
   container.actClear           .Enabled := false ;
   container.actSaveToFile      .Enabled := false ;
   container.actViewTraceInfo   .Enabled := false ;
   container.actPause           .Enabled := false ;
   container.actCopy            .Enabled := false ;
   container.actDelete          .Enabled := false ;
   container.actCut             .Enabled := false ;
   container.actSelectAll       .Enabled := false ;
   container.actSearch          .Enabled := false ;
   container.actFindNext        .Enabled := false ;
   Container.actClearFileContent.Visible := false ;
   container.actViewTraceInfo   .checked := false ;
   container.actPause           .checked := false ;
   container.actCloseWin        .Enabled := true ;    // always enabled

   if container = MainPageContainer then
      MnuAction := Frm_Tool.MainMnuAction
   else
      MnuAction := Container.MnuAction ;

   c := 0 ;
   while c < MnuAction.Count do begin
      itemMenu := MnuAction.Items[c] ;
      if itemMenu.Tag <> 0 then  // plugin item
         itemMenu.Parent.Delete(itemMenu.Parent.IndexOf(itemMenu)) 
      else
         inc (c) ;
   end ;
end ;

//------------------------------------------------------------------------------

procedure TDockingPagecontrol.MyPageControlChange(Sender: TObject);
var
   Base : TFrmBase ;
//   ActivePageStr : string ;
//   selfStr : string ;
begin
//   selfStr := 'self:' + inttostr (integer(self))+'.' ;
//
//   if ActivePage = nil then
//      ActivePageStr := 'nil'
//   else
//      ActivePageStr := ActivePage.Caption ;
//
//   TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.PageControlChange() ActivePage : ' + ActivePageStr);

   resetToolbarAndMenu() ;    // ensure menu and toolbar are reset
   if ActivePage = nil then
      exit ;

   if ActivePage.ControlCount = 0 then
      exit ;

   //container.actCloseWin     .Enabled := true ;
   Base := TFrmBase (ActivePage.controls[0]) ;
   Base.PageControlChange ();
end;

//------------------------------------------------------------------------------

procedure TDockingPagecontrol.MyGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
//var
//   CanDockStr : string ;
//   selfStr : string ;
//   MousePosStr : string ;
begin
   //selfStr := 'self:' + inttostr (integer(self))+'.' ;
   //if CanDock then
   //   CanDockStr := 'true'
   //else
   //   CanDockStr := 'false' ;
   //MousePosStr := 'x:'+inttostr(MousePos.X) + ';y:'+inttostr(MousePos.y) ;
   //
   //
   //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.OnGetSiteInfo (Sender:'+Sender.className +
   //                          ',DockClient:' + DockClient.className +
   //                          ',var InfluenceRect'+ inttostr (InfluenceRect.Top)+ ','+inttostr (InfluenceRect.Left)+ ','+ inttostr (InfluenceRect.Right)+ ','+ inttostr (InfluenceRect.Bottom)+
   //                          ',MousePos:' + MousePosStr +
   //                          ',var CanDock:'+CanDockStr+')');

   CanDock := true ;
end ;

//------------------------------------------------------------------------------

// occur before the undocking occur
procedure TDockingPagecontrol.MyUndock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
var
   //TargetName : string ;
   //AllowStr : string ;
   selfStr : string ;
   base : TFrmBase ;
begin
   selfStr := 'self:' + inttostr (integer(self))+'.' ;
   
   //if NewTarget = nil then
   //   TargetName := 'null'
   //else
   //   TargetName := NewTarget.className + ' ' + inttostr (integer(NewTarget)) ;
   //
   //if Allow then
   //   AllowStr := 'true'
   //else
   //   Allowstr := 'false' ;
   //
   //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.Undock (Sender:' + inttostr(integer(self)) +
   //                          ',Client:' + Client.Name +
   //                          ',NewTarget:' + TargetName +
   //                          ',Allow:'+Allowstr+') DockClientCount=' + inttostr (DockClientCount) ) ;

   base := TFrmBase (Client) ;
   if base.IsDestroying then begin
      Allow := true ;
      base.OldPageControl := nil ;
      if (DockClientCount <= 1) and (container <> MainPageContainer) then begin
         PostMessage(container.Handle, WM_CLOSE, 0, 0);
      end ;
      exit ;
   end ;

   if NewTarget = self then begin
      Allow := DockMoveAllowed ;  // DockMoveAllowed is set by the self.onDockOver method
      if Allow = false then begin
         //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.Undock Not allowed by self.onDockOver') ;

         exit ;  // Undock Not allowed by self.onDockOver method
      end ;
   end ;

   // OnUnDock is called before the undocking actually occurs
   if DockClientCount <= 1 then begin
      if container = MainPageContainer then begin
         // undock main container is always allowed
         allow := true
      end else if NewTarget = nil then begin
         // Undock to nil target Not allowed : DockClientCount=1
         allow := false ;
      end else begin
         // last undock, close container
         base.OldPageControl := nil ;
         PostMessage(container.Handle, WM_CLOSE, 0, 0);
      end ;
   end ;

end ;

//------------------------------------------------------------------------------

procedure TDockingPagecontrol.MyDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
         State: TDragState; var Accept: Boolean);
var
   ARect: TRect;
   OldTabSheetPos,NewTabSheetPos : integer ;
   selfStr : string ;
begin
   Source.DockRect := ARect ; // set DockRect to zero if accept is false


   selfStr := 'self:' + inttostr (integer(self))+'.' ;


   //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.DockOver (Sender:'+ inttostr(integer(self)) +
   //                         ',Source:'+Source.control.name+
   //                         ',x:' + inttostr(x) +
   //                         ',y:' + inttostr(y) +
   //                         ',state'+
   //                         ',accept)') ;

   Accept := (Source.Control is TFrmBase);
   DockMoveAllowed := Accept ;
   if Accept = false then
      exit ;  // Source.Control is not TFrmBase
                         

   //Draw dock preview depending on where the cursor is relative to our client area
   if ComputeDockingRect(ARect, Point(X, Y),NewTabSheetPos) = alNone then begin
      //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.DockOver alnone') ;
      Accept := false ;  // dock to the same position
      exit ;
   end ;

   // Source.control is TFrmBase (not nil). FrmBase Parent must be a tabsheet
   // is the pagecontrol is the same as the sender , only tab position is accepted
   if TFrmBase(Source.control).getPageControl = sender then begin
      OldTabSheetPos := ActivePage.TabIndex ;
      // dock to the same position
      if ((NewTabSheetPos - OldTabSheetPos  ) = 0) or ((NewTabSheetPos - OldTabSheetPos ) = 1 ) then begin
         //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.DockOver same pos') ;
         Accept := false ;
         DockMoveAllowed := false ;
         exit ;
      end ;

      if NewTabSheetPos = -1 then begin
         //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.DockOver self.center') ;
         Accept := false ;
         DockMoveAllowed := false ;
         exit ;
      end ;
   end ;
   //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.DockOver accept true') ;
   Source.DockRect := ARect;
end;

//------------------------------------------------------------------------------

//procedure TDockingPagecontrol.CMUnDockClient(var Message: TCMUnDockClient);
//var
//  Page: TTabSheet;
//begin
//  Message.Result := 0;
//  Page := GetPageFromDockClient(Message.Client);
//  if Page <> nil then
//  begin
//    FUndockingPage := Page;
//    Message.Client.Align := alNone;
//  end;
//
//end;

//------------------------------------------------------------------------------

procedure TDockingPagecontrol.CMDockClient(var Message: TCMDockClient);
var
   //IsVisible: Boolean;
   DockCtl: TControl;
   ARect: TRect;
   Pt: TPoint;
   NewDockSheet: TTabSheet;
   NewTabSheetPos : integer ;
   //selfStr : string ;
   base : TFrmBase ;
begin
   //selfStr := 'self:' + inttostr (integer(self))+'.' ;
   //TFrm_Trace.InternalTrace (selfStr+'TDockingPagecontrol.CMDockClient (Message:' + Message.DockSource.Control.Name+ ') self= ' + inttostr(integer(self))) ;

   NewTabSheetPos := 0 ;

   if self.DockClientCount <> 0 then begin
      Pt.x := Message.MousePos.x;
      Pt.y := Message.MousePos.y;
      ComputeDockingRect(ARect, pt,NewTabSheetPos) ;
   end ;

   Message.Result := 0;
   DockCtl := Message.DockSource.Control;
   if not (DockCtl is TFrmBase) then
      exit ;

   base := TFrmBase (DockCtl) ;

   NewDockSheet := TTabSheet.Create(Self);
   try

     NewDockSheet.Caption := base.Caption;
     NewDockSheet.PageControl := Self;
     if NewTabSheetPos <> -1 then
        NewDockSheet.PageIndex := NewTabSheetPos ;

     // force undock if already in other tabsheet.
     if DockCtl.Parent is TTabSheet then begin
        base.Hide ;                              // hide to prevent flickering
        base.ManualDock (nil,nil,alClient) ;     // undock
     end ;

     base.Parent := NewDockSheet ;
     base.Dock(Self, Message.DockSource.DockRect);  // dock to self (change HostDockSite and add to docked list)
     NewDockSheet.TabVisible := true ;
     ActivePage := NewDockSheet;
     base.Align := alClient;
     base.Show ;
     // change container caption if not the main container
     SetContainerCaption()
   except
     NewDockSheet.Free;
     raise;
   end;
end;

//------------------------------------------------------------------------------

//procedure TDockingPagecontrol.GetTabSheetBoudaries (var DockRect: TRect; TabSheetPos : integer) ;
//begin
//end ;

//------------------------------------------------------------------------------
function TDockingPagecontrol.ComputeDockingRect(var DockRect: TRect; MousePos: TPoint; var NewTabSheetPos : integer): TAlign;
var
   DockTabSheet, LeftDockTabSheet, RightDockTabsheet , DockCenterRect: TRect;
   tabSheetPos : integer ;
   WidthDiv5 : integer ;
   HeightDiv5 : integer ;

begin
   Result := alNone;
   NewTabSheetPos := -1 ;

   //divide form up into docking "Zones"

   tabSheetPos := IndexOfTabAt (MousePos.X, MousePos.Y) ;

   if tabSheetPos <> -1 then begin
      // MousePos is relative to the control

      DockTabSheet.TopLeft := MousePos ;
      DockTabSheet.BottomRight := MousePos ;
      // detect boundaries
      while IndexOfTabAt (MousePos.X, DockTabSheet.Top) = tabSheetPos do
         dec(DockTabSheet.Top) ;
      dec(DockTabSheet.Top,2) ;
      while IndexOfTabAt (MousePos.X, DockTabSheet.Bottom) = tabSheetPos do
         inc(DockTabSheet.bottom) ;
      inc(DockTabSheet.bottom,4) ;
      while IndexOfTabAt (DockTabSheet.left, MousePos.Y) = tabSheetPos do
         dec(DockTabSheet.left) ;
      while IndexOfTabAt (DockTabSheet.Right, MousePos.Y) = tabSheetPos do
         inc(DockTabSheet.right) ;

      LeftDockTabSheet := DockTabSheet ;
      LeftDockTabSheet.Left     := DockTabSheet.left - 10 ;
      LeftDockTabSheet.Right    := DockTabSheet.left + 10 ;
      RightDockTabsheet := DockTabSheet ;
      RightDockTabsheet.Left    := DockTabSheet.Right - 10 ;
      RightDockTabsheet.Right   := DockTabSheet.Right + 10 ;

      if PtInRect(LeftDockTabSheet, MousePos) then begin
         Result := alClient;
         DockRect := LeftDockTabSheet;
         NewTabSheetPos := tabSheetPos ;
      end else if PtInRect(RightDockTabsheet, MousePos) then begin
         Result := alClient;
         DockRect := RightDockTabSheet;
         NewTabSheetPos := tabSheetPos + 1 ;
      end else begin; // else result = alNone
         DockRect := DockTabSheet ;
         NewTabSheetPos := -1 ;
      end ;
   end ;


   if Result = alNone then begin
      NewTabSheetPos := -1 ; // self.PageCount ;

      DockTabSheet.TopLeft := Point(0, 0);
      DockTabSheet.BottomRight := Point(ClientWidth, ClientHeight);

      WidthDiv5  := ClientWidth div 5 ;
      HeightDiv5 := ClientHeight div 5 ;
      DockCenterRect.TopLeft := Point(WidthDiv5, HeightDiv5);
      DockCenterRect.BottomRight := Point(WidthDiv5 * 4, HeightDiv5 * 4);
      if PtInRect(DockTabSheet, MousePos) then begin
         Result := alClient;
         DockRect := DockCenterRect;
      end;
   end ;


   //if Result = alNone then
   //   Exit;

   //DockRect is in screen coordinates.
   DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
   DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
end;

//------------------------------------------------------------------------------

// don't let the double click goes to original page control to disable undock
procedure TDockingPagecontrol.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  DockCtl: TControl;
begin
  DockCtl := GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
  if DockCtl <> nil then begin
     //TFrm_Trace.InternalTrace ('double click undock disabled');
     exit ;  // do nothing
  end ;
  inherited;
end;

//------------------------------------------------------------------------------

// called by WMLButtonDblClk to get Docking client from mose position.
// Original code from TPageControl (but private)
function TDockingPagecontrol.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  i, HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
  Page: TTabSheet;
begin
  Result := nil;
  if DockSite then
  begin
    HitTestInfo.pt := MousePos;
    HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));
    if HitIndex >= 0 then
    begin
      Page := nil;
      for i := 0 to HitIndex do
        Page := FindNextPage(Page, True, True);
      if (Page <> nil) and (Page.ControlCount > 0) then
      begin
        Result := Page.Controls[0];
        if Result.HostDockSite <> Self then Result := nil;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDockingPagecontrol.SetContainerCaption;
var
   c : integer ;
   str : string ;
begin
   if container = MainPageContainer then
      exit ;

   str := '' ;
   for c := 0 to PageCount-1 do begin
      if str <> '' then
         str := str + ', ' ;
      str := str + Pages[c].Caption ;
   end ;

   container.Caption := str ;
end;

//------------------------------------------------------------------------------

function TDockingPagecontrol.GetVisibleClientCount: integer;
var
   base : TFrmBase ;
   c : integer ;
begin
   result := 0 ;
   for c := 0 to DockClientCount -1 do begin
       base := TFrmBase(DockClients[c]) ;
       if base.Visible then
          inc (result);
   end ;
end;

//------------------------------------------------------------------------------


end.
