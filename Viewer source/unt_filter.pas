{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_filter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms ,unt_utility,
  Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, ToolWin, Contnrs, VirtualTrees,pscMenu;

type
  TFrmFilter = class(TForm)
    PanelBottom: TPanel;
    PanelChildren: TPanel;
    chkCheckChildren: TCheckBox;
    Panel2: TPanel;
    rbShow: TRadioButton;
    rbHide: TRadioButton;
    PanelButtons: TPanel;
    butCancel: TButton;
    butFilter: TButton;
    PopupMenuAndOr: TPopupMenu;
    mnuAnd: TMenuItem;
    mnuOr: TMenuItem;
    PanelFilterList: TPanel;
    ToolBarAdd: TToolBar;
    ButAdd: TToolButton;
    procedure mnuAndClick(Sender: TObject);
    procedure mnuOrClick(Sender: TObject);
    procedure ButAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure ComboColumnChange(Sender: TObject);
    procedure butFilterClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    nbFilters : integer ;
    ComboColumnList  : tObjectList ;  // not owner
    ComboCompareList : tObjectList ;  // not owner
    ComboEditList    : tObjectList ;  // not owner
    EditList         : TObjectList ;  // not owner
    ColumnNameList   : TStringList ;
    vst : TVirtualStringTree ;
    base : TForm ;
    CompareAnd : boolean ;
    SelectFirst : boolean ;
    procedure ApplyFilter(TempInt : integer);
    procedure FillColumns() ;
    function  CheckNode(node: PVirtualNode): boolean;
    procedure ResetFilter();
    function AddEmptyRow() : integer;
    function AddRow(ColId: integer; Compare: integer ; Text: string): integer;
  end;

var
  FrmFilter: TFrmFilter;

implementation

uses unt_tool , unt_base ;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.FormCreate(Sender: TObject);
begin
   //with TFSSMenu.create (self) do begin        TPSCMenu
   //   DimLevel := 0 ;    // don't gray icon
   //   Active := true ;
   //end ;
   nbFilters := 0 ;
   ComboColumnList  := tObjectList.create(false) ;  // not owner
   ComboCompareList := tObjectList.create(false) ;  // not owner
   ComboEditList    := tObjectList.create(false) ;  // not owner
   EditList         := tObjectList.create(false) ;  // not owner
   ColumnNameList   := TStringList.create() ;
   AddEmptyRow() ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.FormDestroy(Sender: TObject);
begin
   ComboColumnList  .free ;
   ComboCompareList .free ;
   ComboEditList    .free ;
   EditList         .free ;
   ColumnNameList   .free ;
end;

//----------------------------------------------------------------------------------------------------------------------

// fill combobox in each filter row with column names
procedure TFrmFilter.FillColumns;
var
   ComboColumn  : TCombobox ;
   obj : tObject ;
   ColName : string ;
   c : integer ;
   d : integer ;
   title : string ;
begin
   for c := 0 to nbFilters-1 do begin
      ComboColumn := TComboBox (ComboColumnList.Items[c]) ;

      // first keep column value
      if ComboColumn.ItemIndex = -1 then begin
         ColName := '' ;
      end else begin
         ColName := ComboColumn.Items.Strings[ComboColumn.ItemIndex] ;
      end ;

      ComboColumn.Clear ;
      for d := 0 to ColumnNameList.Count-1 do begin
         title := ColumnNameList.Strings[d] ;
         obj := ColumnNameList.Objects[d] ;
         ComboColumn.AddItem(title,obj);
         if ColName = title then
            ComboColumn.ItemIndex := d ;
      end ;
   end ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.mnuAndClick(Sender: TObject);
begin
   butAdd.Caption := 'And' ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.mnuOrClick(Sender: TObject);
begin
   butAdd.Caption := 'Or' ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.ButAddClick(Sender: TObject);
begin
   AddEmptyRow() ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.ApplyFilter(TempInt : integer) ; // ConditionAnd, ShowMatch,IncludeChildren)
var
   TempInt2 : integer ;

begin
   // ConditionAnd<<2+ShowMatch<<1+IncludeChildren

   // ConditionAnd
   TempInt2 := TempInt and 4 ;
   if TempInt2 > 0 then
      ButAdd.Caption :=  ' And ...'
   else
      ButAdd.Caption :=  ' Or  ...' ;

   // ShowMatch
   TempInt2 := TempInt and 2 ;
   if TempInt2 > 0 then
      rbShow.Checked := true
   else
      rbHide.Checked := true ;

   // IncludeChildren
   TempInt2 := TempInt and 1 ;
   chkCheckChildren.Checked := (TempInt2 > 0) ;

   butFilterClick(nil) ;
end;

//----------------------------------------------------------------------------------------------------------------------

// compare and text are separated by a Tab
function TFrmFilter.AddRow(ColId : integer ; Compare: integer; Text : string) : integer ;
var
   c : integer ;
   ComboColumn  : TComboBox ;
   ComboCompare : TComboBox ;
   ComboEdit    : TComboBox ;
   Edit         : TEdit ;
   icoIndex : integer ;
begin
   result := AddEmptyRow() ;

   // get row components
   ComboColumn  := TComboBox (ComboColumnList.Items[result]) ;
   ComboCompare := TComboBox (ComboCompareList.Items[result]) ;
   ComboEdit    := TComboBox (ComboEditList.Items[result]) ;
   Edit         := TEdit     (EditList.Items[result]) ;

   for c := 0 to ComboColumn.items.Count - 1 do
      if integer(ComboColumn.Items.Objects[c]) = ColId then begin
         ComboColumn.ItemIndex := c ;
         ComboColumn.Text := ComboColumn.Items.Strings[c] ;
      end;

   if compare = -1 then
      compare := 4 ;

   if compare <= ComboCompare.Items.Count then begin
      ComboCompare.ItemIndex := compare ;
      ComboCompare.Text := ComboCompare.Items[compare] ;
   end;

   // comboedit and edit are displayed on the same place
   if ColId = 999 then begin  // 999 is the tracekind column
      ComboEdit.Items.Clear ;
      ComboEdit.Items.AddObject('24 : Debug/Info',TObject(24)) ;
      ComboEdit.Items.AddObject('22 : Warning',TObject(22)) ;
      ComboEdit.Items.AddObject('23 : Error',TObject(23)) ;
      ComboEdit.Visible := true ;
      ComboEdit.Text := text ;
      icoIndex := StrToIntDef(text,-1) ;
      if icoIndex <> -1 then begin
         for c := 0 to ComboEdit.Items.Count - 1 do
            if integer(ComboEdit.Items.Objects[c]) = icoIndex then
               ComboEdit.ItemIndex := c ;

         if ComboEdit.ItemIndex = -1 then begin
            ComboEdit.AddItem(text, TObject(icoIndex));
            ComboEdit.ItemIndex := ComboEdit.Items.Count-1 ;
         end;
      end;
      edit.Visible := false ;
   end else begin
      ComboEdit.Visible := false ;
      edit.Visible := true ;
      edit.Text := text ;
   end ;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrmFilter.AddEmptyRow() : integer ;
var
   newComboColumn  : TCombobox ;
   newComboCompare : TCombobox ;
   newComboEdit    : TCombobox ;
   newEdit         : TEdit ;
   title : string ;
   obj : TObject ;
   c : integer ;
begin

   // ensure the last filter is valid
   result := nbFilters - 1 ;
   if nbFilters <> 0 then begin
      newComboColumn := TComboBox (ComboColumnList.Items[nbFilters-1]) ;
      if newComboColumn.ItemIndex = -1 then // not a valid filter
         exit ;
   end;

   newComboColumn := TCombobox.Create(self);
   newComboColumn.Top      := 8 + (24 * nbFilters) ;
   newComboColumn.Left     := 8 ;
   newComboColumn.Width    := 145 ;
   newComboColumn.parent   := PanelFilterList ;
   newComboColumn.OnChange := ComboColumnChange ;
   newComboColumn.tag      := nbFilters ;
   newComboColumn.Style    := csDropDownList ;
   for c := 0 to ColumnNameList.Count-1 do begin
      title := ColumnNameList.Strings[c] ;
      obj := ColumnNameList.Objects[c] ;
      newComboColumn.AddItem(title,obj);
   end ;
   newComboColumn.ItemIndex := -1 ;         // not a valid filter
   ComboColumnList.Add(newComboColumn) ;

   newComboCompare := TCombobox.Create(self);
   newComboCompare.Top    := 8 + (24 * nbFilters) ;
   newComboCompare.Left   := 168 ;
   newComboCompare.Width  := 145 ;
   newComboCompare.parent := PanelFilterList ;
   newComboCompare.tag    := nbFilters ;
   newComboCompare.Style  := csDropDownList ;
   newComboCompare.Items.AddObject('Equal'    , TObject (0) ) ;
   newComboCompare.Items.AddObject('Not equal', TObject (1) ) ;
   newComboCompare.Items.AddObject('Contains', TObject (2) ) ;
   newComboCompare.Items.AddObject('Don''t contains', TObject (3) ) ;
   newComboCompare.Items.AddObject('(Ignore this filter)' , TObject (-1) ) ;
   newComboCompare.ItemIndex := 2 ; // take 'Containts' as default
   ComboCompareList.Add (newComboCompare) ;

   newComboEdit := TCombobox.Create(self);
   newComboEdit.Top     := 8 + (24 * nbFilters) ;
   newComboEdit.Left    := 328 ;
   newComboEdit.Width   := PanelFilterList.Width - newComboEdit.Left - 8 ;
   newComboEdit.Anchors := [akLeft,akTop,AkRight] ;
   newComboEdit.Visible := false ;
   newComboEdit.parent  := PanelFilterList ;
   newComboEdit.tag     := nbFilters ;
   ComboEditList.Add (newComboEdit) ;

   newEdit := TEdit.create (self) ;
   newEdit.Top    := 8 + (24 * nbFilters) ;
   newEdit.Left   := 328 ;
   newEdit.Width  := PanelFilterList.Width - newComboEdit.Left - 8 ;
   newEdit.Anchors := [akLeft,akTop,AkRight] ;
   newEdit.parent := PanelFilterList ;
   newEdit.tag    := nbFilters ;
   EditList.Add (newEdit) ;

   height := ((nbFilters+1) * 24) + 170 ;

   inc (nbFilters);
   if self.Visible then
      newComboColumn.SetFocus ;

   result := nbFilters -1 ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.ComboColumnChange(Sender: TObject);
var
   ComboColumn : TCombobox ;
   ComboEdit : TCombobox ;
   ColId : integer ;
   FilterId : integer ;
begin
   ComboColumn := TCombobox(sender) ;

   if ComboColumn.ItemIndex = -1 then   // if no column selected : not a valid filter
      exit ;

   FilterId  := ComboColumn.Tag ;
   ColId := integer(ComboColumn.Items.Objects[ComboColumn.ItemIndex]) ;

   ComboEdit := TCombobox (ComboEditList.Items[FilterId]) ;
   if ColId = 999 then begin  // 999 is the tracekind column
      ComboEdit.Items.Clear ;
      ComboEdit.Items.Add('24 : Debug/Info') ;
      ComboEdit.Items.Add('22 : Warning') ;
      ComboEdit.Items.Add('23 : Error') ;
      ComboEdit.Visible := true ;
      TEdit (EditList.Items[FilterId]).Visible := false ;
   end else begin
      ComboEdit.Visible := false ;
      TEdit (EditList.Items[FilterId]).Visible := true ;
   end ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.ResetFilter();
var
   ComboColumn  : TCombobox ;
   ComboCompare : TCombobox ;
   ComboEdit    : TCombobox ;
   Edit         : TEdit ;
   c : integer ;
begin
   for c := 0 to nbFilters-1 do begin
      ComboColumn := TComboBox (ComboColumnList.Items[c]) ;
      //ComboColumn.parent := nil ;
      ComboColumn.free ;

      ComboCompare := TComboBox (ComboCompareList.Items[c]) ;
      //ComboCompare.parent := nil ;
      ComboCompare.free ;

      ComboEdit := TComboBox (ComboEditList.Items[c]) ;
      //ComboEdit.parent := nil ;
      ComboEdit.free ;

      Edit := TEdit (EditList.Items[c]) ;
      //ComboEdit.parent := nil ;
      Edit.free ;
   end ;

   ComboColumnList.Clear ;
   ComboCompareList.Clear ;
   ComboEditList.Clear ;
   EditList.Clear ;

   nbFilters := 0 ;
   butFilterClick(nil) ;   // apply search with no criteria
   AddEmptyRow() ;
end ;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.butCancelClick(Sender: TObject);
begin
   ResetFilter() ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmFilter.butFilterClick(Sender: TObject);
var
   currentNode : PVirtualNode ;
   focusNode : PVirtualNode ;
begin
   vst.BeginUpdate ;
   try
      CompareAnd := (ButAdd.Caption =  ' And ...') ;
      currentNode := vst.GetFirst() ;
      SelectFirst := false ;
      // process root
      while currentNode <> nil do begin
         CheckNode (currentNode) ;
         currentNode := currentNode.NextSibling ;
      end ;
      // ensure that the info panel display correct node information
      if SelectFirst then begin
         focusNode := vst.GetFirstVisible() ;
         Vst.FocusedNode := focusNode ;
         vst.ClearSelection ;
         vst.Selected [focusNode] := true ;
         vst.OnChange (vst,focusNode) ;
      end ;
   finally
      vst.EndUpdate ;
   end ;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrmFilter.CheckNode (node : PVirtualNode) : boolean ;
var
   TextType     : TVSTTextType;
   TreeText     : String ;
   ComboColumn  : TCombobox ;
   ComboCompare : TCombobox ;
   ComboEdit    : TCombobox ;
   Edit         : TEdit ;
   ColId        : integer ;
   FilterOk     : boolean ;
   c            : integer ;
   ChildNode    : PVirtualNode ;
   ValidFilterCount : integer ;
   CompareMode  : integer ;
   FilterEdit   : string ;
   Members : TMember ;

   function CheckMember (Member : TMember) : boolean;     // recursive
   var
      c : integer ;
      SubMember : TMember ;
    begin
       if (CompareMode = 0) or (CompareMode = 2) then
          // if 0 : equal : one members must be equal to the search text
          // if 2 : contains : one members must contains the search text
          result := false  // if no members, result is false
       else
          // if 1 : not equal : all members must be differents from the search text
          // if 3 : don't contains : all members must have the search text
          result := true ;  // if no members, result is true

      for c := 0 to Member.SubMembers.Count -1 do begin
         SubMember := TMember (Member.SubMembers.Items[c]) ;

         if CompareMode = 0 then begin              // equal
            // if 0 : equal : one members must be equal to the search text

            result := (StrIComp(PChar(FilterEdit), PChar(SubMember.col1)) = 0) ;
            if result = true then
               exit ;
            result := (StrIComp(PChar(FilterEdit), PChar(SubMember.col2)) = 0) ;
            if result = true then
               exit ;
            result := (StrIComp(PChar(FilterEdit), PChar(SubMember.col3)) = 0) ;
            if result = true then
               exit ;
            result := CheckMember(SubMember);  // recursive check submembers
            if result = true then
               exit ;

         end else if CompareMode = 1 then begin     // not equal
            // if 1 : not equal : all members must be differents from the search text

            result := (StrIComp(PChar(FilterEdit), PChar(SubMember.col1)) <> 0) ;
            if result = false then
               exit ;
            result := (StrIComp(PChar(FilterEdit), PChar(SubMember.col2)) <> 0) ;
            if result = false then
               exit ;
            result := (StrIComp(PChar(FilterEdit), PChar(SubMember.col3)) <> 0) ;
            if result = false then
               exit ;
            result := CheckMember(SubMember);  // recursive check submembers
            if result = false then
               exit ;

         end else if CompareMode = 2 then begin     // contains
            // if 2 : contains : one members must contains the search text

            result := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(SubMember.col1)) > 0) ;
            if result = true then
               exit ;
            result := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(SubMember.col2)) > 0) ;
            if result = true then
               exit ;
            result := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(SubMember.col2)) > 0) ;
            if result = true then
               exit ;
            result := CheckMember(SubMember);  // recursive check submembers
            if result = true then
               exit ;

         end else if CompareMode = 3 then begin     // don't contains
            // if 3 : don't contains : all members must have the search text

            result := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(SubMember.col1)) = 0) ;
            if result = false then
               exit ;
            result := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(SubMember.col2)) = 0) ;
            if result = false then
               exit ;
            result := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(SubMember.col2)) = 0) ;
            if result = false then
               exit ;
            result := CheckMember(SubMember);  // recursive check submembers
            if result = false then
               exit ;
         end ;

      end ;
   end ;

   procedure  SetChildrenAsVisible (node : PVirtualNode) ;
   var
      ChildNode : PVirtualNode ;
   begin
      ChildNode := node.FirstChild ;
      // process children
      while ChildNode <> nil do begin
         vst.IsVisible [ChildNode] := true ;
         SetChildrenAsVisible (ChildNode) ;
         ChildNode := ChildNode.NextSibling ;
      end ;
   end ;

begin
   result := true ;

   ValidFilterCount := nbFilters ;
   for c := 0 to nbFilters-1 do begin
      ComboColumn  := TComboBox (ComboColumnList.Items[c]) ;
      ComboCompare := TComboBox (ComboCompareList.Items[c]) ;
      ComboEdit    := TComboBox (ComboEditList.Items[c]) ;
      Edit         := TEdit     (EditList.Items[c]) ;

      // if no column selected : not a valid filter
      if ComboColumn.ItemIndex = -1 then begin
         dec (ValidFilterCount) ;
         continue ;
      end ;

      ColId       := integer(ComboColumn.Items.Objects[ComboColumn.ItemIndex]) ;
      CompareMode := integer(ComboCompare.Items.Objects[ComboCompare.ItemIndex]) ;   // 0,1,2,3

      if CompareMode = -1 then
         continue  ;

      if ColId = 999 then begin  // framework : trace kind
         FilterEdit  := trim(ComboEdit.Text) ;
         if (FilterEdit = '24 : Debug/Info') or
            (FilterEdit = 'Debug/Info') or
            (FilterEdit = 'Debug') or
            (FilterEdit = 'Info') or
            (FilterEdit = '24') then
            FilterEdit := '24 : Debug/Info'
         else if (FilterEdit = '22 : Warning') or (FilterEdit = 'Warning') or (FilterEdit = '22') then
            FilterEdit := '22 : Warning'
         else if (FilterEdit = '23 : Error') or (FilterEdit = 'Error') or (FilterEdit = '23') then
            FilterEdit := '23 : Error'
         else
            FilterEdit := IntToStr(StrToIntDef (FilterEdit,24)) ;
         ColId := 0 ;
      end else begin
         FilterEdit := trim(Edit.Text) ;   
      end ;

      FilterOk := true ;

      if ColId = 998 then begin  // framework or Event log : trace information
         members := TFrmBase(base).getMembers (node) ;
         if (CompareMode = 0) or (CompareMode = 2) then
            // if 0 : equal : one members must be equal to the search text
            // if 2 : contains : one members must contains the search text
            FilterOk := false  // if no members, result is false
         else
            // if 1 : not equal : all members must be differents from the search text
            // if 3 : don't contains : all members must have the search text
            FilterOk := true ;  // if no members, result is true

         if (Members <> nil) and (Members.SubMembers.Count <> 0) then
            FilterOk := CheckMember (Members);

      end else begin
         // get the real text for the node. to do that, call the OnGetText with the ttStatic flag.
         TextType := ttStatic ;
         TreeText := '' ;
         vst.OnGetText (vst, Node, ColId, TextType, TreeText);

         if CompareMode = 0 then begin              // equal
            FilterOk := (StrIComp(PChar(FilterEdit), PChar(TreeText)) = 0) ;
         end else if CompareMode = 1 then begin     // not equal
            FilterOk := (StrIComp(PChar(FilterEdit), PChar(TreeText)) <> 0) ;
         end else if CompareMode = 2 then begin     // contains
            FilterOk := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(TreeText)) > 0) ;
         end else if CompareMode = 3 then begin     // don't contains
            FilterOk := (Pos(AnsiUpperCase(FilterEdit), AnsiUpperCase(TreeText)) = 0) ;
         end ;
      end ;

      if CompareAnd = true then begin    // AND filters
         if FilterOk = false then begin
            result := false  ;
            break ; // don't check other filters
         end ;  // else result stay to true
      end else begin // OR filters
         if FilterOk = true then begin
            result := true ;
            break ; // don't check other filters
         end else
            result := false ;   // maybee another filter can set it back to true
      end ;
   end ;

   if ValidFilterCount = 0 then  // reset filter
      result := true
   else if rbHide.Checked then
      result := not result ;

   if (result = false) and (vst.Selected[node]) then
      SelectFirst := true ;
   vst.IsVisible [node] := result ;

   // return if the node is not visible
   if result = false then
      exit ;

   // recursive check children if flag 'children' or when the filter is empty
   if (chkCheckChildren.Checked) and (ValidFilterCount > 0) then begin
      ChildNode := node.FirstChild ;
      // process children
      while ChildNode <> nil do begin
         CheckNode (ChildNode) ;
         ChildNode := ChildNode.NextSibling ;
      end ;
   end else begin
      // set all chldren and sub children as visible
      SetChildrenAsVisible (node) ;
   end ;
end ;

end.
