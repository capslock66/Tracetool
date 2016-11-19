{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit VstSort;

interface

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, contnrs, VirtualTrees;

type
  TSortColumn = class
     ColumnIndex : TColumnIndex ; // TVirtualTreeColumn
     SortDirection: TSortDirection ;
  end ;

  TVstSort = class(TComponent)
    procedure AfterConstruction ; Override ;
    procedure BeforeDestruction ; override ;
  private
    IsMultiColSort : boolean ;
    function  DoCompare(Node1, Node2: PVirtualNode): Integer;
    procedure DoNextColumnOrder (Column: TColumnIndex);
    function  GetSortColumn(Column: TColumnIndex): TSortColumn;
    procedure ClearSortExcept(SortCol: TSortColumn);
  public
    SortColumns : TObjectList ; // array of TSortColumn. SortColumns is the owner
    Tree : TVirtualStringTree ;
    UtilityImages : TImageList ;
    CanUnsort : boolean ;

    procedure Sort (Node: PVirtualNode; DoInit: Boolean = True);
    procedure Unsort (Node: PVirtualNode) ; // unsort node if the application provide an OnCompare methode with sort on the -1 column

  public  // events
    procedure onHeaderDrawQueryElements (Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure onAdvancedHeaderDraw (Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    //procedure onHeaderClick (Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure onHeaderClick (Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure onKeyUp (Sender: TObject; var Key: Word; Shift: TShiftState);

  published
    { Published declarations }
  end;

function TreeNodeCount (vt : TVirtualStringTree) : integer ;
function TreeHeight (vt : TVirtualStringTree) : integer ;

procedure Register;

implementation

//----------------------------------------------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TVstSort]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVstSort.Afterconstruction;
begin
   CanUnsort := false ;
   IsMultiColSort := false ;
   SortColumns := TObjectList.create (true) ; // array of TSortColumn. SortColumns is the owner
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVstSort.BeforeDestruction;
begin
   inherited;
   SortColumns.Clear ;
   SortColumns.free ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVstSort.onKeyUp (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   // CTRL : 17
   if (Key = 17) and (IsMultiColSort) then begin
      IsMultiColSort := false ;
      Sort (nil, False) ;
   end ;
end ;

//----------------------------------------------------------------------------------------------------------------------

procedure TVstSort.onHeaderDrawQueryElements (Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
   Include(Elements, hpeSortGlyph);
end ;

//----------------------------------------------------------------------------------------------------------------------

procedure TVstSort.onAdvancedHeaderDraw (Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  ColumnIndex : Integer ;
  SortIndex: Integer;

  GlyphPosX, GlyphPosY : integer ;
  SortCol : TSortColumn ;
begin

   // this procedure handle only the hpeSortGlyph event
   if not (hpeSortGlyph in Elements) then
      exit ;

   // no sort. Nothing to draw
   if SortColumns.Count = 0 then
      exit ;

   // check if the column is in the SortColumns
   ColumnIndex := PaintInfo.Column.Index ;
   SortCol := nil ;
   for SortIndex := 0 to SortColumns.Count-1 do begin
      SortCol := TSortColumn (SortColumns[SortIndex]) ;
      if SortCol.ColumnIndex = ColumnIndex then
         break ;
   end ;

   // Column not found : no sort on that column then nothing to draw
   if SortIndex >= SortColumns.Count then
      exit ;

   GlyphPosX := PaintInfo.TargetCanvas.ClipRect.Right - 21 ;
   GlyphPosY := (PaintInfo.PaintRectangle.Bottom - PaintInfo.PaintRectangle.Top) div 2 ;

   // draw the up or down bitmap
   if SortCol.SortDirection = sdAscending then
      UtilityImages.Draw(PaintInfo.TargetCanvas, GlyphPosX, GlyphPosY, 10)
   else
      UtilityImages.Draw(PaintInfo.TargetCanvas, GlyphPosX, GlyphPosY, 11);

   // if only one column : no need to draw the '1' number
   if SortColumns.Count = 1 then
      exit ;

   // Bug somewhere : the font name and size cannot be changed each time
   //TargetCanvas.font.name := 'MS Serif' ;
   //TargetCanvas.font.Height := -9 ;
   //TargetCanvas.TextOut (GlyphPosX+13, GlyphPosY, inttostr (PaintInfo.Column.Index)) ;

   // It's easier to draw number using a bitmap than using Canvas.TextOut.
   UtilityImages.Draw(PaintInfo.TargetCanvas, GlyphPosX+13, GlyphPosY, SortIndex+1);  // draw number

end ;

//----------------------------------------------------------------------------------------------------------------------

// 386 :  TVTHeaderClickEvent = procedure(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo) of object;
//  TVTHeaderHitInfo = record
//    X,
//    Y: Integer;
//    Button: TMouseButton;
//    Shift: TShiftState;
//    Column: TColumnIndex;
//    HitPosition: TVTHeaderHitPositions;
//  end;

procedure TVstSort.onHeaderClick (Sender: TVTHeader; HitInfo: TVTHeaderHitInfo) ;
//procedure TVstSort.onHeaderClick (Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) ;
begin
  if HitInfo.Button <> mbLeft then
     exit ;

  If tree.focused = false then
     tree.SetFocus ;

  IsMultiColSort := ssCtrl in HitInfo.shift ;

  DoNextColumnOrder (HitInfo.Column) ;
  Tree.Header.Invalidate (nil) ;

  if IsMultiColSort = false then
     Sort (nil, False);
end ;

//----------------------------------------------------------------------------------------------------------------------

// change the sort column order. no sort -> ascending -> descending -> no sort -> ...
procedure TVstSort.DoNextColumnOrder (Column: TColumnIndex) ;
var
   SortCol : TSortColumn ;
begin
   SortCol := GetSortColumn (Column) ;

   // If CTRL key is not pressed, clear all sort except for the current col 
   if IsMultiColSort = false then
      ClearSortExcept (SortCol) ;

   if SortCol = nil then begin                                  // no sort : use ascending
      SortCol := TSortColumn.create ;
      SortCol.ColumnIndex := Column ;
      SortCol.SortDirection := sdAscending ;
      SortColumns.Add (SortCol) ;
   end else if SortCol.SortDirection = sdAscending then begin   // Ascending : use Descending
      SortCol.SortDirection := sdDescending ;
   end else begin                                               
      if SortColumns.Count > 1 then                             // Descending + many column : remove sort
         SortColumns.Delete (SortColumns.IndexOf (SortCol))
      else // only one column
         if canUnsort then
            SortColumns.Delete (SortColumns.IndexOf (SortCol))  // Descending + only one column + unsort possibilities : remove sort
         else
            SortCol.SortDirection := sdAscending ;              // Descending + only one column and no unsort : use ascending

   end ;
end ;

//----------------------------------------------------------------------------------------------------------------------

// clear all sort except one column
procedure TVstSort.ClearSortExcept (SortCol : TSortColumn) ;
var
   c : integer ;
   Current : TSortColumn ;
begin
   c := 0 ;
   while c < SortColumns.Count do begin
      Current := TSortColumn (SortColumns[c]) ;
      if Current = SortCol then
         inc(c)
      else
         SortColumns.Delete (c) ;
   end ;
end ;

//----------------------------------------------------------------------------------------------------------------------

function TVstSort.GetSortColumn (Column: TColumnIndex) : TSortColumn ;
var
   c : integer ;
begin
   for c := 0 to SortColumns.Count-1 do begin
      result := TSortColumn (SortColumns[c]) ;
      if result.ColumnIndex = Column then
         exit ;
   end ;
   result := nil ;
end ;

//----------------------------------------------------------------------------------------------------------------------

//procedure TVstSort.Sort(Node: PVirtualNode);
//begin
//   tree.Sort (Node, tree.Header.SortColumn, tree.Header.SortDirection, False);
//end;
//
//----------------------------------------------------------------------------------------------------------------------


procedure TVstSort.Sort(Node: PVirtualNode; DoInit: Boolean = True);

// Sorts the given node. The application is queried about how to sort via the OnCompareNodes event.
// Column is simply passed to the the compare function so the application can also sort in a particular column.
// In order to free the application from taking care about the sort direction the parameter Direction is used.
// This way the application can always sort in increasing order, while this method reorders nodes according to this flag.

  //--------------- local functions -------------------------------------------

  function Merge(A, B: PVirtualNode): PVirtualNode;

  // Merges A and B (which both must be sorted via Compare) into one list.

  var
    Dummy: TVirtualNode;

  begin
    // This avoids checking for Result = nil in the loops.
    Result := @Dummy;
    while Assigned(A) and Assigned(B) do
    begin
      if DoCompare(A, B) <= 0 then  // ascending. For descending use >= 0
      begin
        Result.NextSibling := A;
        Result := A;
        A := A.NextSibling;
      end
      else
      begin
        Result.NextSibling := B;
        Result := B;
        B := B.NextSibling;
      end;
    end;

    // Just append the list which is not nil (or set end of result list to nil if both lists are nil).
    if Assigned(A) then
      Result.NextSibling := A
    else
      Result.NextSibling := B;
    // return start of the new merged list
    Result := Dummy.NextSibling;
  end;

  //---------------------------------------------------------------------------

  function MergeSort(var Node: PVirtualNode; N: Cardinal): PVirtualNode;

  // Sorts the list of nodes given by Node (which must not be nil).

  var
    A, B: PVirtualNode;

  begin
    if N > 1 then
    begin
      A := MergeSort(Node, N div 2);
      B := MergeSort(Node, (N + 1) div 2);
      Result := Merge(A, B);
    end
    else
    begin
      Result := Node;
      Node := Node.NextSibling;
      Result.NextSibling := nil;
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Run: PVirtualNode;
  Index: Cardinal;
  ChildCount: Cardinal ;

begin
   //  Tree.InterruptValidation;

   // no assigned tree : do nothing
   if not assigned (Tree) then
      exit ;

   // no sort procedure : stop sorting
   //if not Assigned(Tree.OnCompareNodes) then
   //   exit ;

   if tsEditPending in Tree.TreeStates then
   begin
      KillTimer(Tree.Handle, EditTimer);                     // StopTimer(EditTimer)
      tree.TreeStates := tree.TreeStates - [tsEditPending];  // DoStateChange([], [tsEditPending])
   end;

   if tsEditing in Tree.TreeStates then
      if Tree.EndEditNode = false then
         exit ;

   if Node = nil then
     Node := tree.RootNode;

   if vsHasChildren in Node.States then
   begin
     if (Node.ChildCount = 0) and DoInit then
        if Assigned(tree.OnInitChildren) then
           Tree.OnInitChildren(tree, Node, ChildCount);

     // Make sure the children are valid, so they can be sorted at all.
     if DoInit and (Node.ChildCount > 0) then
       Tree.ValidateChildren(Node, False);
     // Child count might have changed.
     if Node.ChildCount > 1 then
     begin
       // Sort the linked list
       Node.FirstChild := MergeSort (Node.FirstChild, Node.ChildCount);

       // Consolidate the child list finally.
       Run := Node.FirstChild;
       Run.PrevSibling := nil;
       Index := 0;
       repeat
         Run.Index := Index;
         Inc(Index);
         if Run.NextSibling = nil then
           Break;
         Run.NextSibling.PrevSibling := Run;
         Run := Run.NextSibling;
       until False;
       Node.LastChild := Run;

       //InvalidateCache;
       tree.TreeStates := tree.TreeStates + [tsValidationNeeded] - [tsUseCache];  // DoStateChange([tsValidationNeeded], [tsUseCache]);

     end;
     //if UpdateCount = 0 then
     //begin
     //  Tree.ValidateCache;
       Tree.Invalidate;
     //end;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

// unsort node if the application provide an OnCompare methode with sort on the -1 column
procedure TVstSort.Unsort (Node: PVirtualNode) ;
var
   SavedColumns : TObjectList ;
   TemporySort  : TObjectList ;
begin
   TemporySort := TObjectList.create ;   // create an empty sort order
   SavedColumns := SortColumns ;         // save the sort order
   SortColumns := TemporySort ;
   Sort (Node,false);                    // sort with an empty sort order
   SortColumns := SavedColumns ;         // restore old sort order
   TemporySort.Free ;
end ;

//----------------------------------------------------------------------------------------------------------------------

function TVstSort.DoCompare(Node1, Node2: PVirtualNode): Integer;
var
   c : integer ;
   Current : TSortColumn ;

   procedure doCompare2 (ColIndex : TColumnIndex) ;
   var
      cellText1, cellText2 : String ;
   begin
      if assigned (Tree.OnCompareNodes) then
         Tree.OnCompareNodes(tree, Node1, Node2, ColIndex, Result)
      else begin
         if ColIndex = -1 then  // no compare procedure, then it's not possible to unsort
            exit ;
         tree.OnGetText (tree,Node1,ColIndex,ttNormal,CellText1) ;
         tree.OnGetText (tree,Node2,ColIndex,ttNormal,CellText2) ;
         Result := CompareText (CellText1,CellText2) ;
      end ;
   end ;
begin
   Result := 0;

   // no sort specified : unsort (if the application saved original position)
   if SortColumns.Count = 0 then begin
      if canUnsort then
         doCompare2(-1) ;
      exit ;
   end ;

   for c := 0 to SortColumns.Count -1 do begin
      Current := TSortColumn (SortColumns[c]) ;
      doCompare2(Current.ColumnIndex) ;
      if result < 0 then begin
         if Current.SortDirection = sdDescending then
            result := 1 ; // inverse sort
         exit ;  // no need to sort on next column
      end else if result > 0 then begin
         if Current.SortDirection = sdDescending then
            result := -1 ; // inverse sort
         exit ;  // no need to sort on next column
      end ;
      // else nodes are identical for the current column, continue to check other sort column
   end ;

   // if the records are identicals from the selected columns, use the -1 column
   if (result = 0) and (assigned (Tree.OnCompareNodes)) then
      doCompare2(-1) ; 
end;

//----------------------------------------------------------------------------------------------------------------------

// print treeview
// note : not really fast but work

// HLine    : Height of line
// WCol     : Width  of col
// treeview : what you want to print

//procedure TForm1.But_PrintClick(Sender: TObject);
//begin
//   Printer.BeginDoc ;
//   Printer.canvas.Font.size := 8 ;
//   HLine := 50 ;
//   WCol  := 50 ;
//   PrintTreeView (treeview1, Printer.Canvas);
//   Printer.EndDoc;
//end ;
//
//procedure TForm1.But_ImgClick(Sender: TObject);
//begin
//   HLine := 16 ;
//   WCol  := 20 ;
//   PrintTreeView (treeview1, Image1.Canvas);
//end;

//----------------------------------------------------------------------------------------------------------------------

function TreeNodeCount (vt : TVirtualStringTree) : integer ;
var
   Node: PVirtualNode ;
begin
   result := 0 ;
   Node := vt.GetFirst;
   while Node <> nil do begin
      inc(result) ;
      Node := vt.GetNext(Node);
   end ;
end ;

//----------------------------------------------------------------------------------------------------------------------

function TreeHeight (vt : TVirtualStringTree) : integer ;
var
   Node: PVirtualNode ;
   NodeHeight : integer ;
begin
   result := 0 ;
   Node := vt.GetFirst;
   while Node <> nil do begin
      NodeHeight := vt.NodeHeight[Node] ;
      result := result + NodeHeight ;
      Node := vt.GetNext(Node);
   end ;
end ;

//----------------------------------------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------


end.

