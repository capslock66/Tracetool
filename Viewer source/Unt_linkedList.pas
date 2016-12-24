{
  virtual tree view linked list node cache

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   
}

unit Unt_linkedList;

interface

uses VirtualTrees, sysutils, classes, windows ;


type

  //---------------------------------------------------------------------------------

  TScoreNode = class
  public
     score : integer ;
     Node : PVirtualNode ;
     Next : TScoreNode ;
     previous : TScoreNode ;
  end ;

  //---------------------------------------------------------------------------------

  TNodeLinkedList = class
  private
  public
     name : string ;  // for debug purpose
     First : TScoreNode ;
     Last  : TScoreNode ;
     Count : integer ;
     Max   : integer ;
     constructor Create (MaxNode : integer) ;
     procedure BeforeDestruction () ; override ;

     function AddFirst  (node : PVirtualNode; ScoreNode : TScoreNode) : TScoreNode ;
     function AddLast   (node : PVirtualNode; ScoreNode : TScoreNode) : TScoreNode ;  // just add to last, no check
     function AddLastUsingFirst (node : PVirtualNode) : TScoreNode ;  // add to last and remove first if needed
     function AddToList (node : PVirtualNode) : TScoreNode ;
     procedure RemoveFromList(node: PVirtualNode);
     procedure RemoveAll;
     procedure RemoveFromSelected(vst: TVirtualStringTree);
     procedure Remove (ScoreNode : TScoreNode) ;
     function RemoveFirst() : TScoreNode ;
     function RemoveLast() : TScoreNode ;
     function SearchNode (vst : TVirtualStringTree ; ID : AnsiString ; increment : boolean) : PVirtualNode ;
     procedure Print (vst : TVirtualStringTree);
     procedure Dump ();
     procedure DumpToList (ListOut : tlist);
     procedure checkIntegrity() ;
  end ;

  //---------------------------------------------------------------------------------

var
   CurrentNodeLinkedList : TNodeLinkedList ; // for internal trace
   BeforeAddList : tList ;                   // for internal trace
   CurrentNodeToAdd : PVirtualNode ;         // for internal trace

implementation

uses Unt_TraceWin , unt_utility, unt_tool;


{ TNodeLinkedList }

//------------------------------------------------------------------------------

constructor TNodeLinkedList.Create(MaxNode: integer);
begin
   max := MaxNode ;
   count := 0 ;
   First := nil ;
   Last := nil ;
   inherited create ;
end;

//------------------------------------------------------------------------------

procedure TNodeLinkedList.BeforeDestruction;
begin
   RemoveAll() ;
   inherited ;
end;

//------------------------------------------------------------------------------

function TNodeLinkedList.AddFirst(node: PVirtualNode; ScoreNode : TScoreNode): TScoreNode;
begin
   if ScoreNode = nil then
      result := TScoreNode.create
   else
      result := ScoreNode ;

   if First <> nil then
      First.previous := result ;

   result.Next := First ;  // old first
   result.previous := nil ;
   result.score := node.ChildCount ;
   result.Node := node ;

   First := result ;
   if last = nil then
      last := first ;
   inc (Count) ;
end;

//------------------------------------------------------------------------------

function TNodeLinkedList.AddLast(node: PVirtualNode; ScoreNode : TScoreNode): TScoreNode;
begin
   if first = nil then begin
      result := AddFirst (node , ScoreNode) ;  // inc count
      exit ;
   end ;

   if ScoreNode = nil then
      result := TScoreNode.create
   else
      result := ScoreNode ;

   last.Next := result ;      // old last

   result.previous := last ;  // old last
   result.Next     := nil ;   // it's the new last
   result.Node     := node ;
   result.score    := node.ChildCount ;

   last := result ;
   inc (Count) ;
end;

//------------------------------------------------------------------------------
// add to last and remove first if needed

function TNodeLinkedList.AddLastUsingFirst(node: PVirtualNode): TScoreNode;
begin
   if first = nil then begin               // empty list
      //LowTrace (name + #9 + 'AddLastUsingFirst : no first. AddFirst()') ;
      result := AddFirst (node , nil) ;       // inc count
   end else if Count >= max then begin     // list full
      //LowTrace (name + #9 + 'AddLastUsingFirst : Count >= max. RemoveFirst and AddLast()') ;
      Result := RemoveFirst ;                 // dec count
      AddLast (node , Result) ;               // inc count
   end else begin                          // list not yet full
      //LowTrace (name + #9 + 'AddLastUsingFirst : Count < max : AddLast()') ;
      result := AddLast (node , nil) ;        // inc count
   end ;
end;

//------------------------------------------------------------------------------

// conditional add,  depending of the number of children
function TNodeLinkedList.AddToList(node: PVirtualNode): TScoreNode;
var
   NodeCount : integer ;
   current : TScoreNode ;
//   p : integer ;

   procedure AddTrace (msg1 : string; msg2 : string = '') ;
   begin
      BeforeAddList.Add(TMember.create(msg1,msg2));
   end ;

begin
   CurrentNodeLinkedList := self ; // for internal trace
   CurrentNodeToAdd := node ;      // for internal trace
   BeforeAddList.Clear ;           // for internal trace

//   AddTrace ('CurrentNodeToAdd : ' + '$' + inttohex (integer(CurrentNodeToAdd),8));
//   AddTrace('---');
//   AddTrace ('List before adding node :') ;
//   DumpToList (BeforeAddList) ;          // for internal trace

   NodeCount := node.ChildCount ;
   result := nil ;

   if Count >= max then begin  // list is full
      // last score is better than the node to insert.
      // do nothing
      if NodeCount <= Last.score then begin
//         AddTrace ('Count >= max and NodeCount (' + inttostr(NodeCount) + ') <= Last.score (' + inttostr (Last.score) + ') : exit') ;
         CurrentNodeLinkedList := nil ;
         exit ;
      end ;

      // remove last element, but keep his reference for reuse
//      AddTrace('NodeCount (' + inttostr(NodeCount) + ') <= Last.score (' + inttostr (Last.score) + ') : RemoveLast. Keep it for reuse') ;
//      LowTrace ('NodeCount (' + inttostr(NodeCount) + ') <= Last.score (' + inttostr (Last.score) + ') : RemoveLast. Keep it for reuse') ;

      result := RemoveLast() ;                     // dec count
//      AddTrace('---');
//      AddTrace ('List after deleting last :') ;
//      DumpToList(BeforeAddList) ;
//      LowTrace ('List after deleting last :');
//      dump() ;
   end else begin
//      AddTrace('Create TScoreNode') ;
      result := TScoreNode.Create ;
   end ;

   result.score := NodeCount ;
   result.Node := node ;

   current := First ;
//   p := 0 ;
   while current <> nil do begin
      if current.score < NodeCount then begin
         if current = First then begin           // add first
//            AddTrace('insert first') ;
//            LowTrace('insert first') ;
            AddFirst(node, result) ;             // inc count
         end else begin                          // add between 2 existing node
//            LowTrace('insert between 2 scores') ;
//            AddTrace ( 'insert between [' + inttostr(p)   + '] $' + inttohex (integer(current),8)) ;
//            AddTrace ( '   . previous = ' + '$' + inttohex (integer(current.previous),8)) ;
//            AddTrace ( '   . Next = ' + '$' + inttohex (integer(current.Next),8)) ;
//            AddTrace ( '   . score = ' + inttostr(current.score)) ;
//            AddTrace ( 'and [' + inttostr(p-1) + '] $' + inttohex (integer(current.previous),8)) ;
//            if current.previous = nil then
//               AddTrace ('current.previous is null !!!')
//            else begin
//               AddTrace ('   . previous = ' + '$' + inttohex (integer(current.previous.previous),8)) ;
//               AddTrace ('   . Next = ' + '$' + inttohex (integer(current.previous.Next),8)) ;
//               AddTrace ('   . score = ' + inttostr(current.previous.score)) ;
//            end ;
            result.previous := current.previous ;          // 1
            current.previous := result ;                   // 2
            result.Next :=  current ;                      // 3
            result.previous.Next := result ;               // 4
            inc (Count) ;
         end ;
//         Dump() ;
//         checkIntegrity() ;
         CurrentNodeLinkedList := nil ;
         exit ;   // we find it : stop search
      end ;
      current := current.Next ;
//      inc (p) ;
   end ;

   // not find. Add to end
//   AddTrace('not found , add at end') ;
   AddLast(node, result) ;                       // inc count
//   checkIntegrity() ;
//   Dump() ;
   CurrentNodeLinkedList := nil ;
end;

//------------------------------------------------------------------------------

function TNodeLinkedList.SearchNode(vst : TVirtualStringTree ; ID: Ansistring; increment: boolean): PVirtualNode;
var
   current : TScoreNode ;
   TreeRec : PTreeRec ;
   previous : TScoreNode ;

   SwapScore : integer ;
   SwapNode  : PVirtualNode ;
begin
   result := nil ;
   current := First ;
   while current <> nil do begin
      TreeRec := vst.GetNodeData(current.Node) ;
      if TreeRec.TraceID = ID then begin
         result := current.node ;
         if increment = true then begin
            current.score := current.node.ChildCount + 1 ;
            // verify if the new score is not better than previous score ...
            while current <> nil do begin
               // if current is better than previous : swap with previous
               if (current.previous <> nil) and (current.score > current.previous.score) then begin

                  // swap score and node in place of swapping link
                  previous := current.previous ;
                  SwapScore := previous.score ;
                  SwapNode  := previous.Node ;
                  previous.score := current.score ;
                  previous.Node  := current.Node ;
                  current.score := SwapScore ;
                  current.Node  := SwapNode  ;
               end else
                  break ;  // quit the loop : no previous having better score (or no previous at all)
             end ;
         end ;
         exit ; // found : stop searching.
      end ;
      current := current.Next ;
   end ;
end;

//------------------------------------------------------------------------------

// remove the corresponding node and his children from list then free it

procedure TNodeLinkedList.RemoveFromList(node: PVirtualNode) ;
var
   current : TScoreNode ;
   child : PVirtualNode ;
begin
   // delete child reference
   child := node.FirstChild ;
   while child <> nil do begin
      RemoveFromList(child) ;
      child := child.NextSibling ;
   end ;

   current := First ;
   while current <> nil do begin
      if current.Node = node then begin
         // remove from list then free it
         Remove(current);  // dec count
         current.Free ;
         exit ;   // we find it : stop search
      end ;

      current := current.Next ;
   end ;
end ;

//------------------------------------------------------------------------------

// remove and free all elements

procedure TNodeLinkedList.RemoveAll() ;
var
   current,nodeToFree : TScoreNode ;
begin
   current := First ;
   while current <> nil do begin
      nodeToFree := current ;
      current := current.Next ;
      try
         nodeToFree.Free ;
      except
      end ;
   end ;
   count := 0 ;
   first := nil ;
   Last  := nil ;
end ;

//------------------------------------------------------------------------------

// remove all ScoreNode in list where node is selected

procedure TNodeLinkedList.RemoveFromSelected (vst : TVirtualStringTree) ;
var
   current,nodeToFree : TScoreNode ;
   Node: PVirtualNode ;
begin
   current := First ;
   while current <> nil do begin
      node := current.Node ;

      nodeToFree := nil ;
      while node <> nil do begin
         if vst.Selected[Node] then begin
            nodeToFree := current ;
            current := current.Next ;    // point to the next
            // remove from list then free it
            Remove(nodeToFree);  // dec count
            nodeToFree.Free ;
            break ;  // break while <> nil
         end else begin
            node := node.Parent ;  // check if the parent is not selected
         end ;
      end ;

      // f node was not selected (for delete) then get next
      if nodeToFree = nil then
         current := current.Next ;
   end ;
end ;

//------------------------------------------------------------------------------

// remove element, but don't free it.

procedure TNodeLinkedList.Remove (ScoreNode: TScoreNode);
begin
   if first = nil then   // should not happens.
      exit ;

   if ScoreNode = first then
      RemoveFirst ()  // dec count
   else if ScoreNode = last then
      RemoveLast ()   // dec count
   else begin
      // not first and not last
      ScoreNode.Next.previous := ScoreNode.previous ;
      ScoreNode.previous.Next := ScoreNode.Next ;
      // ScoreNode.previous and ScoreNode.Next are not modified
      dec (count) ;
   end ;
end;


//------------------------------------------------------------------------------

function TNodeLinkedList.RemoveFirst: TScoreNode;
begin
   if first = nil then begin
      result := nil ;
      exit ;
   end ;
   dec (count) ;
   result := first ;
   first := First.Next ;
   if first = nil then
      Last := nil
   else
      first.previous := nil ;
end;

//------------------------------------------------------------------------------

function TNodeLinkedList.RemoveLast: TScoreNode;
begin
   if first = nil then begin
      result := nil ;
      exit ;
   end ;
   dec (count) ;
   result := last ;
   last := last.previous ;
   if last = nil then
      first := nil
   else
      last.Next := nil ;
end;

//------------------------------------------------------------------------------

// used internaly to display the linked list.
// Normaly not called in the final version of tracetool
procedure TNodeLinkedList.Dump ();
var
   current : TScoreNode ;
   line : string ;
   c : integer ;
begin

   LowTrace (inttostr(getCurrentThreadID()) + ' First ' + '$' + inttohex (integer(First),8) +
              ' ,Last '  + '$' + inttohex (integer(Last),8) +
              ' ,Count ' + inttostr(Count) +
              ' ,Max '   + inttostr(Max  )) ;

   current := First ;
   c := 0 ;
   while current <> nil do begin
      LowTrace (inttostr (c) + ' $' + inttohex (integer(current),8) + ':' + inttostr(current.score)
         + ' previous= ' + '$' + inttohex (integer(current.previous),8)
         + ' next= ' + '$' + inttohex (integer(current.Next),8));
      current := current.Next ;
      inc(c) ;
   end ;
   LowTrace (line) ;
end;

//------------------------------------------------------------------------------

procedure TNodeLinkedList.checkIntegrity() ;
var
   current : TScoreNode ;
begin
   current := First ;
   while current <> nil do begin
      if (current = first) and (current = last) then begin                      // single
         if (current.previous <> nil) then
            raise exception.create ('checkIntegrity : single.previous not nil') ;
         if (current.Next <> nil) then
            raise exception.create ('checkIntegrity : single.next not nil') ;

      end else if (current = first) and (count=2) then begin                    // 2 elem. current = first
        if (current.previous <> nil) then
            raise exception.create ('checkIntegrity : first.previous not nil') ;
        if (current.next = nil) then
            raise exception.create ('checkIntegrity : not last.next is nil') ;

      end  else if (current = last) and (count=2) then begin                    // 2 elem. current = last
        if (current.Next <> nil) then
            raise exception.create ('checkIntegrity : last.next not nil') ;
        if (current.previous = nil) then
            raise exception.create ('checkIntegrity : not first.previous is nil') ;

      end  else if (current = first) then begin                        // first , not last
        if (current.previous <> nil) then
            raise exception.create ('checkIntegrity : first.previous not nil') ;
        if (current.next = nil) then
            raise exception.create ('checkIntegrity : not last.next is nil') ;

      end  else if (current = last)  then begin                        // last , not first
        if (current.previous = nil) then
            raise exception.create ('checkIntegrity : not first.previous is nil') ;
        if (current.Next <> nil) then
            raise exception.create ('checkIntegrity : last.next not nil') ;

      end else begin                                                   // not first, not last
        if (current.previous = nil) then
            raise exception.create ('checkIntegrity : not first.previous is nil') ;
        if (current.next = nil) then
            raise exception.create ('checkIntegrity : not last.next is nil') ;

      end  ;

      current := current.Next ;
   end ;
end ;

//------------------------------------------------------------------------------


// used internaly to display the linked list.
// Normaly not called in the final version of tracetool
procedure TNodeLinkedList.DumpToList (ListOut : tlist);
var
   current : TScoreNode ;
   procedure AddTrace (msg1 : string; msg2 : string = '') ;
   begin
      ListOut.Add(TMember.create(msg1,msg2));
   end ;
begin
   AddTrace('Name' , name) ;
   AddTrace('First ' , '$' + inttohex (integer(First),8)) ;
   AddTrace('Last '  , '$' + inttohex (integer(Last),8));
   AddTrace('Count ' , inttostr(Count));
   AddTrace('Max '   , inttostr(Max  )) ;

   current := First ;
   while current <> nil do begin
      AddTrace('$' + inttohex (integer(current),8)  +
               ' score : '    + inttostr(current.score)+
               ' Node : '     + '$' + inttohex (integer(current.Node),8) +
               ' Previous : ' + '$' + inttohex (integer(current.previous),8) +
               ' Next : '     + '$' + inttohex (integer(current.Next),8)) ;

      current := current.Next ;
   end ;
end;


//------------------------------------------------------------------------------

// used internaly to display the linked list.
// Normaly not called in the final version of tracetool
procedure TNodeLinkedList.Print (vst : TVirtualStringTree);
var
   current : TScoreNode ;
   Node: PVirtualNode ;
   relation : string ;
   TreeRec : PTreeRec ;
begin

   LowTrace (name + #9 + 'First ' + '$' + inttohex (integer(First),8) +
                    ' ,Last '  + '$' + inttohex (integer(Last),8) +
                    ' ,Count ' + inttostr(Count) +
                    ' ,Max '   + inttostr(Max  )) ;

   current := First ;
   while current <> nil do begin

      LowTrace (#9 + 'TScoreNode ' + '$' + inttohex (integer(current),8) + ':') ;
      LowTrace (#9 + 'score    ' + inttostr(current.score)) ;
      
      LowTrace (#9 + 'Node     ' + '$' + inttohex (integer(current.Node),8)) ;
      LowTrace (#9 + 'Next     ' + '$' + inttohex (integer(current.Next),8)) ;
      LowTrace (#9 + 'previous ' + '$' + inttohex (integer(current.previous),8)) ;

      node := current.Node ;
      relation := 'Node ' ;
      while (node <> nil) and (Node <> vst.RootNode) do begin
         LowTrace (#9 + #9 + relation + '$' + inttohex (integer(node),8)) ;
      
         TreeRec := vst.GetNodeData(Node) ;
         if TreeRec <> nil then begin
            LowTrace (#9 + #9 + relation + 'TraceID ' + string(TreeRec.TraceID));
            LowTrace (#9 + #9 + relation + 'LeftMsg ' + TreeRec.LeftMsg);
         end ;
      
         relation := 'Parent ' ;
         node := node.Parent ;
      end ;
      LowTrace ('') ;
      current := current.Next ;
   end ;

end;

initialization
   BeforeAddList := tList.create() ;

finalization
   BeforeAddList.Free ;


end.
 