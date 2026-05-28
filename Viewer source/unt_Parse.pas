{
  parse method that receive message from the clients
  Author : Thierry Parent
  HomePage :  https://github.com/capslock66/Tracetool
}

unit unt_Parse;

interface

uses
  Classes,  Messages, Controls, SysUtils, Contnrs, Forms, windows, unt_search, unt_base, unt_filter ;

{$Include TraceTool.Inc}


// called only by the timer (then main thread) for each messages in the stack object list
procedure ParseTraceMsg (MsgList: TstringList);

var

   CurrentParseMsgList : TstringList ;
   CurrentParseCommandIndex : integer ;

implementation

uses
   unt_traceWin
   , unt_tool
   , unt_SelectTail
   , unt_saveDlg
   , unt_utility
   , DebugOptions
   , unt_TraceConfig
   , VirtualTrees.BaseTree, VirtualTrees, VirtualTrees.Types ;


// called only by the timer (then main thread) for each messages in the stack object list

procedure ParseTraceMsg (MsgList: TstringList) ;
var
   SingleMsg : String ;
   command : integer ;
   CommandIndex : integer ;
   THID : String ;
   ClientIP : String ;
   ProcessName : String ;
   ActiveNode, WatchNode : PVirtualNode ;
   ActiveNodeChanged : boolean ;  // used for filter
   ParentNodeVisible : boolean ;  // used for filter
   WatchExpanded : boolean ;
   ParentCompoNode : PVirtualNode ;
   CurrentMember, MemberToAdd : TMember ;
   stack : TStack ; //TObjectStack ;  // list of hierarchical TMember
   TreeRec : PTreeRec ;
   MessageTime : String ;
   NewOrder : cardinal ;
   TempStr : String ;
   TempInt,TempInt2,TempInt3 : integer ;
   IsBold, IsItalic : boolean ;
   TraceForm : TFrm_Trace ;
   WatchForm : TFrm_Trace ;
   IsNewNode : boolean ;
   IsWatch   : boolean ;
   IsFirstMember : boolean ;
   CurrentLevel, newLevel : TLevel ;
   Base : TFrmBase ;
   //p : integer ;
   //NodeCache: TNodeArray;                  // hold temporarily a bunch of node refs.
   //StartNode, LastNode : PVirtualNode;

   //------------------------------------
   function lowGetInt(strVal:string; context:string): integer;
   begin
      var E: Integer;
      Val(strVal, Result, E);
      if E <> 0 then begin
          Result := -1;
          Frm_Tool.ShowParsedMessage(context + '\ Bad string: <' + strVal + '>') ;
      end;

   end;
   //------------------------------------
   function getStr() : String ;
   begin
      result := copy (SingleMsg,6,length (SingleMsg)-5);
      CorrectString(result) ;
   end ;

   //------------------------------------
   function getIntAndStr (var intval : integer; context:string) : String ;
   var
      First : String ;
   begin
      First := copy (SingleMsg,6,11) ;
      intval := lowGetInt (First, context + '\I1');
      result := copy (SingleMsg,17,length (SingleMsg)-16);
      CorrectString(result) ;
   end ;

   //------------------------------------
   function get2IntAndStr (var intval1, intval2 : integer; context:string) : String ;
   var
      strVal : String ;
   begin
      strVal := copy (SingleMsg,6,11) ;
      intval1 := lowGetInt (strVal,context + '\I1');

      strVal := copy (SingleMsg,17,11) ;
      intval2 := lowGetInt (strVal,context + '\I2');

      result := copy (SingleMsg,28,length (SingleMsg)-27);
      CorrectString(result) ;
   end ;

   //------------------------------------
   function get3IntAndStr (var intval1, intval2, intval3 : integer;context:string) : String ;
   var
      strVal : String ;
   begin
      strVal := copy (SingleMsg,6,11) ;
      intval1 := lowGetInt (strVal,context + '\I1');

      strVal := copy (SingleMsg,17,11) ;
      intval2 := lowGetInt (strVal,context + '\I2');

      strVal := copy (SingleMsg,28,11) ;
      intval3 := lowGetInt (strVal,context + '\I3');

      result := copy (SingleMsg,39,length (SingleMsg)-38);
      CorrectString(result) ;
   end ;

   //------------------------------------

   function getInt (context:string) : integer ;
   var
      strVal : String ;
   begin
      strVal := copy (SingleMsg,6,11) ;
      result := lowGetInt (strVal, context + '\I1');
   end ;

   //------------------------------------

   function getDefaultInt (def : integer): integer ;
   var
      strVal : String ;
   begin
      strVal := copy (SingleMsg,6,11) ;
      result := StrToIntDef (strVal,def);
   end ;

   //------------------------------------
   function getBool : boolean ;
   begin
      if copy (SingleMsg,6,1) = '1' then
         result := true
      else
         result := false ;
   end ;

   //------------------------------------
   // set threadid, time,...  Called only by CST_NEW_NODE command
   procedure InitTreeRec  ;
   begin
      TreeRec.ThreadID       := THID  ;
      TreeRec.ip             := ClientIP ;
      TreeRec.Time           := MessageTime ;
      TreeRec.ProcessName    := ProcessName ;
      TreeRec.Members        := TMember.create ;
      TreeRec.TreeIcon       := -1 ;
      TreeRec.OriginalOrder  := NewOrder ;
      TreeRec.LastChildOrder := 0 ;
      CurrentMember          := TreeRec.Members ;
   end ;

   //------------------------------------
   procedure ParseBackgroundFont (var FontDetails : TFontDetailArray);
   var
      FontDetail : TFontDetail ;
      c,ArrayLen : integer ;
      found : boolean ;
      color : integer ;
      ColId : integer ;
   begin
      var colIdStr : string := getIntAndStr(color,'CST_BACKGROUND_COLOR(Color:I1,ColId:I2)');
      ColId := lowGetInt(colIdStr,'CST_BACKGROUND_COLOR(Color:I1,ColId:I2)\I2') ;
      found := false ;
      ArrayLen := length(FontDetails) ;
      for c := 0 to ArrayLen -1 do begin
         FontDetail := FontDetails[c] ;
         if FontDetail.ColId = ColId then begin
            found := true ;
            FontDetail.Bold   := false ;
            FontDetail.Italic := false ;
            FontDetail.Color  := -1 ;
            FontDetail.Size   := 0 ;
            FontDetail.Name   := '' ;
            FontDetail.BackGroundColor := color ;
         end ;
      end ;
      if found = false then begin
         FontDetail := TFontDetail.create(ColId,false ,false ,-1 ,0 ,'') ;
         FontDetail.BackGroundColor := color ;
         SetLength (FontDetails,ArrayLen+1);
         FontDetails [ArrayLen] := FontDetail ;
      end ;
   end ;

   //------------------------------------
   procedure ParseFontDetail (var FontDetails : TFontDetailArray);
   var
      FontDetail : TFontDetail ;
      c,ArrayLen : integer ;
      found : boolean ;
   begin
      // Cmd + ColId Bold Italic Color size  Fontname
      // %6    %3d   %c   %c     %11d  %11d  %s
      // 0     6     9    10     11    22    33

      // 1) get first int : the colum number
      TempStr := String(copy (SingleMsg,6,3)) ;
      TempInt := lowGetInt (TempStr, 'CST_FONT_DETAIL(6p)\colId:I1');

      // 2) get bold
      TempStr := String(copy (SingleMsg,9,1)) ;
      if tempStr = '1' then
         IsBold := true
      else
         IsBold := false ;

      // 3) get italic
      TempStr := String(copy (SingleMsg,10,1)) ;
      if tempStr = '1' then
         IsItalic := true
      else
         IsItalic := false ;

      // 4) get color
      TempStr := String(copy (SingleMsg,11,11)) ;
      TempInt2 := lowGetInt (TempStr,'CST_FONT_DETAIL(6p)\color:I2');

      // 5) get font size
      TempStr := String(copy (SingleMsg,22,11)) ;
      TempInt3 := lowGetInt (TempStr,'CST_FONT_DETAIL(6p)\FontSize:I3');

      // 6) get fontname
      TempStr := String(copy (SingleMsg,33, length (SingleMsg)-32)) ;

      found := false ;
      ArrayLen := length(FontDetails) ;
      for c := 0 to ArrayLen -1 do begin
         FontDetail := FontDetails[c] ;
         if FontDetail.ColId = TempInt then begin
            found := true ;
            FontDetail.Bold   := IsBold ;
            FontDetail.Italic := IsItalic ;
            FontDetail.Color  := TempInt2 ;
            FontDetail.Size   := TempInt3 ;
            FontDetail.Name   := TempStr ;
         end ;
      end ;
      if found = false then begin
         FontDetail := TFontDetail.create(TempInt,IsBold ,IsItalic ,TempInt2 ,TempInt3 ,TempStr) ;
         SetLength (FontDetails,ArrayLen+1);
         FontDetails [ArrayLen] := FontDetail ;
      end ;

   end ;

   //------------------------------------

   function getTraceForm (createMultiCol : boolean) : TFrm_Trace ;
   var
      c : integer ;
      TraceWinID : AnsiString ;
   begin
      TraceWinID := AnsiString(getStr()) ;
      if (TraceWinID = '') or (TraceWinID = '_') then begin
         result := Frm_Trace ;
         exit ;
      end ;

      for c := 0 to FormTraceList.Count-1 do begin
         result := TFrm_Trace (FormTraceList.Items[c]) ;
         if result.ID = TraceWinID then begin
            TraceForm := result ;
            exit ;
         end ;
      end ;

      // if the trace window don't exist, create it
      result := TFrm_Trace.Create(nil);   // auto add to FormTraceList

      result.ID := TraceWinID ;
      if createMultiCol then begin
         result.IsMultiColTree := true ;
         result.VstMain.Header.Columns.Clear ;
      end ;

      result.Caption := String(TraceWinID) ;
      result.DockToMainPanel() ;
      result.ViewTraceInfo ;
      result.MainCol := -1 ;

      if result.getPageControl().GetVisibleClientCount = 1 then
         result.getPageControl.OnChange (nil) ;

   end ;

   //------------------------------------

   function getWatchForm : TFrm_Trace ;
   var
      c : integer ;
      WatchWinID : AnsiString ;
   begin
      WatchWinID := AnsiString(getStr()) ;
      if (WatchWinID = '') or (WatchWinID = '_') then begin
         result := Frm_Watches ;
         exit ;
      end ;

      for c := 0 to FormTraceList.Count-1 do begin
         result := TFrm_Trace (FormTraceList.Items[c]) ;
         if result.ID = WatchWinID then begin
            TraceForm := result ;
            exit ;
         end ;
      end ;

      // if the trace window don't exist, create it
      result := CreateWatchForm(WatchWinID,String(WatchWinID)) ;   // auto add to FormTraceList
   end ;

   //------------------------------------

begin
   CurrentParseMsgList := MsgList ;
   CurrentParseCommandIndex := -1 ;
   stack := TObjectStack.create() ;

   try //try
      if TraceConfig.DebugMode then
         Frm_Tool.ShowParsedMessage('') ;   // no context, no need to save

      inc(Received);  // nb of messages received

      TreeRec           := nil ;
      ProcessName       := '' ;
      THID              := '' ;
      ActiveNode        := nil ;        // no active node
      IsNewNode         := false ;      // check if message is a new node
      ActiveNodeChanged := false ;
      WatchForm         := Frm_Watches ;
      WatchNode         := nil ;
      CurrentLevel      := nil ;
      TraceForm         := Frm_Trace ;  // traces are send to the master trace form by default
      IsWatch           := false ;      // if watch, CreateMember and AddMember command will create sub node
      WatchExpanded     := false ;
      IsFirstMember     := true ;
      ParentCompoNode   := nil ;

      // to be valid, CST_USE_TREE or CST_USE_MULTICOL_TREE or CST_WINWATCH_ID must be the first command
      if MsgList.Count > 0 then begin
         SingleMsg := MsgList[0] ;
         command := StrToIntDef (copy (SingleMsg, 1,5),-1);
         if command = CST_USE_TREE then begin
            TraceForm := getTraceForm (false)
         //end else if command = CST_USE_MULTICOL_TREE then begin
         //   TraceForm := getTraceForm (true) ;
         end else if command = CST_WINWATCH_ID then begin
            WatchForm := getWatchForm () ;
            TraceForm := WatchForm ;
         end ;
         if TraceConfig.DebugMode then
            Frm_Tool.ShowParsedForm (TraceForm) ;
      end ;

      TraceForm.LastModified := now ;


      for CommandIndex := 0 to MsgList.Count-1 do begin
         CurrentParseCommandIndex := CommandIndex  ; // for bug trapping
         SingleMsg := MsgList[CommandIndex] ;

         command := StrToIntDef (copy (SingleMsg, 1,5),-1);

         case command of
         //CST_USE_TREE :           getTraceForm() ;
         //CST_WINWATCH_ID :        getWatchForm() ;

         CST_TREE_MULTI_COLUMN :  begin  // change the tree to display multiple column
                                     if Frm_Trace <> TraceForm then begin
                                        TraceForm.VstMain.Header.Columns.Clear ;
                                        TraceForm.VstMain.FocusedColumn := NoColumn ;
                                        TraceForm.IsMultiColTree := true ;
                                        TraceForm.MainCol := getDefaultInt(-1) ;
                                     end ;
                                  end ;

         CST_TREE_COLUMNTITLE :   begin  // change the columns titles
                                     TempStr := getStr() ;
                                     if Frm_Trace <> TraceForm then
                                        TraceForm.generateCols (TempStr) ;
                                  end ;

         CST_TREE_COLUMNWIDTH :   begin  // change the columns widths
                                     TempStr := getStr() ;
                                     if Frm_Trace <> TraceForm then
                                        TraceForm.ChangeColWidths (TempStr) ;
                                  end ;


         CST_WINWATCH_NAME :      begin
                                     // CST_WINWATCH_NAME change the newly created form
                                     TempStr := getStr() ;
                                     if TempStr = '_' then
                                        WatchForm.Caption := 'Watches'
                                     else
                                        WatchForm.Caption := TempStr ;
                                  end ;

         CST_TREE_NAME :          begin
                                     // CST_TREE_NAME change the newly created form
                                     // (if CST_USE_TREE is sent just before)
                                     // or to replace the Master tree name
                                     TempStr := getStr() ;
                                     if TempStr = '_' then
                                        TraceForm.Caption := TraceConfig.Framework_MainTraceTitle  // 'Trace'
                                     else
                                        TraceForm.Caption := TempStr ;
                                  end ;

         CST_DISPLAY_TREE :       begin
                                     if ((TraceForm = Frm_Trace) or (TraceForm = Frm_Watches)) and (TraceForm.Visible = false) then begin
                                        //if Frm_Trace.Parent = nil then
                                        TraceForm.UnDock() ;
                                        TraceForm.DockToMainPanel() ;
                                        TraceForm.Visible := true ;
                                     end ;

                                     // PageControl.ActivePage := TabTraceForm ;
                                     TraceForm.SetActivePage ;
                                  end ;

         CST_SHOW :               if getint('CST_SHOW(ShowOrhide:I1)') = 1 then
                                     Frm_Tool.actShowExecute (nil)
                                  else
                                     Frm_Tool.actHideExecute (nil)  ;

         CST_ENTER_DEBUG_MODE :   TraceConfig.DebugMode := true ;    // TFrm_Trace.InternalTrace('CST_ENTER_DEBUG_MODE');

         CST_LEAVE_DEBUG_MODE :   TraceConfig.DebugMode := false ;   // TFrm_Trace.InternalTrace('CST_LEAVE_DEBUG_MODE');
         
         CST_OPEN_TAIL :          FrmSelectTail.OpenFile(getStr());  // TFrm_Trace.InternalTrace('CST_OPEN_TAIL',getStr());

         CST_OPEN_XML :           begin
                                     // create the trace form
                                     TempStr := getStr() ;
                                     Application.CreateForm(TFrm_Trace, TraceForm);
                                     TraceForm.Caption := 'Trace::' + ExtractFileName (TempStr) ;

                                     TraceForm.DockToMainPanel() ;
                                     TraceForm.SetActivePage() ;
                                     TraceForm.getPageControl.OnChange (nil) ;

                                     application.ProcessMessages ;
                                     SetCursor(Screen.Cursors[crHourGlass]);

                                     try
                                        TraceForm.LoadXML(TempStr);
                                     finally
                                        SetCursor(Screen.Cursors[crDefault]);
                                     end ;

                                  end ;

         CST_MESSAGE_TIME :       MessageTime := getStr() ;  // precede CST_NEW_NODE

         CST_PROCESS_NAME :       ProcessName := getStr() ;  // precede CST_NEW_NODE, not send by Java application

         CST_THREAD_ID :          THID := '0x' + IntToHex (getInt('CST_THREAD_ID(THID:I1)'),3) ;  // precede CST_NEW_NODE, not send by Java application

         CST_THREAD_NAME :        begin  // precede CST_NEW_NODE, send only by Java application in place of CST_THREAD_ID
                                     //if (Frm_Tool.StatusBar.Panels[STATUS_Thid].Width <= 45) then
                                     //   Frm_Tool.StatusBar.Panels[STATUS_Thid].Width := 200 ;
                                     THID := getStr() ;
                                  end ;

         CST_IP :                 ClientIP := getStr() ;

         CST_USE_NODE :           begin    // must be used before some other commands
                                     if (TraceForm = Frm_Trace) and (TraceForm.Visible = false) then begin
                                        //TraceForm.Visible := true ;
                                        Frm_Tool.actViewMainTracesExecute(nil) ;
                                     end ;

                                     // convert CLSID to node ID
                                     ActiveNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     // if the node is delete, the ActiveNode can then be nil
                                     if ActiveNode <> nil then begin
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        CurrentMember := TreeRec.Members ;
                                     end ;

                                  end ;

         CST_NEW_NODE :           begin
                                     if TraceForm.IsPaused then   // when paused, new lines are discarded
                                        Exit;

                                     // param1 : Parent Node
                                     if (TraceForm = Frm_Trace) and (TraceForm.Visible = false) then begin
                                        //TraceForm.Visible := true ;
                                        Frm_Tool.actViewMainTracesExecute(nil) ;
                                     end ;

                                     // convert CLSID to node ID
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr()), true) ;

                                     // put a number to all node for unsort
                                     if ParentCompoNode = nil then begin
                                        // no parent. Use the LastChildOrder in the form
                                        NewOrder := TraceForm.LastChildOrder ;
                                        inc (TraceForm.LastChildOrder) ;
                                     end else begin
                                        TreeRec := TraceForm.VstMain.GetNodeData(ParentCompoNode) ;
                                        NewOrder := TreeRec.LastChildOrder ;
                                        inc (TreeRec.LastChildOrder) ;
                                     end ;

                                     // create the node and ensure node is initialized.
                                     // (Needed when the node is free to call onFreeNode)
                                     ActiveNode := TraceForm.VstMain.AddChild (ParentCompoNode) ;
                                     ActiveNodeChanged := true ;  // for filtering
                                     TraceForm.VstMain.ReinitNode(ActiveNode,false);

                                     // fill the record
                                     TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                     InitTreeRec() ;  // set thread id, time,...

                                     // if date is gived, double the size of the time column, if not already done.
                                     if (TraceForm.IsDateTimeResized = false) and (Length(TreeRec.Time) > 12) then begin
                                        TraceForm.IsDateTimeResized := true ;
                                        TraceForm.VstMain.Header.Columns[1].Width := TraceForm.VstMain.Header.Columns[1].Width * 2 ;
                                     end ;

                                     // autosort parent if at least one column in sort
                                     if (ParentCompoNode = nil) and (TraceForm.Sorter.SortColumns.Count <> 0) then
                                        TraceForm.Sorter.sort (nil) ;

                                     // add to recent added nodes
                                     TraceForm.LastUsedList.AddLastUsingFirst(ActiveNode) ;

                                     // check if option 'Focus to latest added node' is true
                                     if TraceConfig.AppDisplay_FocusToReceivedMessage then
                                        TraceForm.NodeToFocus := ActiveNode ;

                                     if TraceConfig.AppDisplay_ShowOnMessageReceived then
                                        Frm_Tool.actShowExecute(nil) ;

                                     IsNewNode := true ;
                                  end ;

         CST_ICO_INDEX :          begin
                                      if ActiveNode <> nil then begin
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        TreeRec.TreeIcon := getInt('CST_ICO_INDEX(ColId:I1)') ;
                                      end ;
                                  end ;

         CST_TRACE_ID :           begin
                                     if ActiveNode <> nil then begin
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        TreeRec.TraceID := AnsiString(getStr()) ;
                                        CurrentMember := TreeRec.Members ;
                                     end ;
                                  end ;

         CST_LEFT_MSG :           begin   // param : msg
                                     TempStr := getStr() ;
                                     if ActiveNode <> nil then begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;

                                        if TraceForm.IsMultiColTree then begin
                                           if TreeRec.Columns <> nil then
                                              TreeRec.Columns.Free ;  // auto clear strings

                                           TreeRec.Columns := getTabStrings(pchar(TempStr)) ;
                                           // check if TreeRec.Columns contain more columns than the tree
                                           TraceForm.checkColumns (TreeRec.Columns) ;
                                        end else begin
                                           TreeRec.LeftMsg := TempStr ;
                                        end ;
                                        CurrentMember := TreeRec.Members ;
                                        TraceForm.VstMain.InvalidateNode(ActiveNode);
                                     end ;
                                  end ;

         CST_RIGHT_MSG :          begin   // param : msg
                                     TempStr := getStr() ;
                                     if ActiveNode <> nil then begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        TreeRec.RightMsg := TempStr ;
                                        CurrentMember := TreeRec.Members ;
                                        TraceForm.VstMain.InvalidateNode(ActiveNode);
                                     end ;
                                  end ;
         CST_APPEND_LEFT_MSG :    begin   // param : msg
                                     TempStr := getStr() ;
                                     if ActiveNode <> nil then begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;

                                        if TraceForm.IsMultiColTree then begin
                                           if TreeRec.Columns = nil then  // line is empty
                                              TreeRec.Columns := getTabStrings(pchar(TempStr))
                                           else begin // append columns to existing columns
                                              TempStr := StringsToTab (TreeRec.Columns) + TempStr ;
                                              TreeRec.Columns.Free ;
                                              TreeRec.Columns := getTabStrings(pchar(TempStr)) ;
                                           end ;
                                           // check if TreeRec.Columns contain more columns than the tree
                                           TraceForm.checkColumns (TreeRec.Columns) ;

                                        end else begin
                                           TreeRec.LeftMsg := TreeRec.LeftMsg + TempStr ;
                                        end ;
                                        CurrentMember := TreeRec.Members ;
                                        TraceForm.VstMain.InvalidateNode(ActiveNode);
                                     end ;
                                  end ;

         CST_APPEND_RIGHT_MSG :   begin   // param : msg
                                     TempStr := getStr() ;
                                     if ActiveNode <> nil then begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        TreeRec.RightMsg := TreeRec.RightMsg + TempStr ;
                                        CurrentMember := TreeRec.Members ;
                                        TraceForm.VstMain.InvalidateNode(ActiveNode);
                                     end ;
                                  end ;

         CST_BACKGROUND_COLOR   : begin   // param : background color, col
                                     if ActiveNode <> nil then begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        CurrentMember := TreeRec.Members ;
                                        ParseBackgroundFont(TreeRec.FontDetails) ;
                                        TraceForm.VstMain.InvalidateNode(ActiveNode);
                                     end ;
                                  end ;

         CST_FONT_DETAIL :        begin
                                     if ActiveNode <> nil then begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        TreeRec := TraceForm.VstMain.GetNodeData(ActiveNode) ;
                                        CurrentMember := TreeRec.Members ;
                                        ParseFontDetail(TreeRec.FontDetails) ;
                                        TraceForm.VstMain.InvalidateNode(ActiveNode);
                                     end ;
                                  end ;

         CST_SET_BOOKMARK :       begin
                                     if ActiveNode <> nil then begin   // SetBookmark (bool enabled)  param : int
                                        if getInt('CST_SET_BOOKMARK(Enabled:I1)') = 1 then begin   // add
                                           if TraceForm.bookmarks.IndexOf(ActiveNode) = -1 then begin
                                              TraceForm.bookmarks.Add(ActiveNode) ;
                                              TraceForm.VstMain.InvalidateNode(ActiveNode) ;
                                           end ;
                                        end else begin              // remove
                                           TempInt := TraceForm.bookmarks.IndexOf(ActiveNode) ;
                                           if TempInt <> -1 then
                                              TraceForm.bookmarks.Delete(TempInt) ;
                                        end ;
                                     end ;
                                  end ;

         CST_VISIBLE_NODE :       begin
                                     if ActiveNode <> nil then begin   // SetVisible  (bool visible)  param : int
                                        TraceForm.vst.IsVisible [ActiveNode] := (getInt('CST_VISIBLE_NODE(Visible:I1)') = 1) ;
                                     end ;
                                  end ;


         CST_WATCH_NAME :         begin  // the watch name is the key to find the node.
                                     // pause don't have effect.
                                     CurrentLevel := TLevel.create() ;
                                     CurrentLevel.Col2Empty := true ;
                                     CurrentLevel.Col3Empty := true ;

                                     IsWatch := true ;
                                     IsFirstMember := true ;  // first member must be merged with watch
                                     TempStr := getStr() ;
                                     WatchNode := WatchForm.CheckWatch (WatchForm.VstMain.RootNode, TempStr) ;
                                     if WatchNode <> nil then
                                        WatchExpanded := WatchForm.VstMain.Expanded [WatchNode] ;

                                     ActiveNode := WatchNode ;
                                     if ActiveNode = nil then begin
                                        ActiveNode := WatchForm.VstMain.AddChild (nil) ;
                                        // ensure node is initialized. Needed when the node is free to call onFreeNode
                                        WatchForm.VstMain.ReinitNode(ActiveNode,false);
                                        TreeRec := WatchForm.VstMain.GetNodeData(ActiveNode) ;
                                        TreeRec.ProcessName := ProcessName ;
                                        TreeRec.Columns := TStringList.create() ;
                                        TreeRec.Columns.Add (MessageTime) ;     // 0 MessageTime
                                        TreeRec.Columns.Add (THID) ;            // 1 THID
                                        TreeRec.Columns.Add (TempStr) ;         // 2 co1 : watch name
                                        TreeRec.Columns.Add ('') ;              // 3 col2
                                        TreeRec.Columns.Add ('') ;              // 4 col3
                                     end else begin // watch already exist
                                        //WatchForm.VstMain.DeleteChildren(ActiveNode);
                                        TreeRec := WatchForm.VstMain.GetNodeData(ActiveNode) ;
                                        TreeRec.Columns[0] := MessageTime ;     // 0 MessageTime
                                        TreeRec.Columns[1] := THID ;            // 1 THID
                                     end ;
                                     ActiveNodeChanged := true ;    // for filtering
                                     CurrentLevel.Node := ActiveNode ;
                                     CurrentLevel.CurrentChild := ActiveNode.FirstChild ;
                                     CurrentMember       := nil ;
                                  end ;

         CST_CREATE_MEMBER :      begin                           // create a member. CST_ADD_MEMBER will add it to the node
                                     TempStr := getStr() ;
                                     if IsWatch then begin
                                        if IsFirstMember then begin
                                           // use the ActiveNode for the first member

                                           if tempStr <> '' then begin
                                              TreeRec.Columns[3] := tempStr ;    // to do : change color if <>
                                              CurrentLevel.Col2Empty := false ;
                                           end else
                                              CurrentLevel.Col2Empty := true ;

                                           IsFirstMember := false ;
                                        end else begin
                                           newLevel := TLevel.Create() ;

                                           if CurrentLevel.CurrentChild = nil then begin
                                              ActiveNode := WatchForm.VstMain.AddChild (CurrentLevel.Node) ;
                                              newLevel.Node := ActiveNode ;
                                              newLevel.CurrentChild := nil ;

                                              // ensure node is initialized. Needed when the node is free to call onFreeNode
                                              WatchForm.VstMain.ReinitNode(newLevel.Node,false);
                                              TreeRec := WatchForm.VstMain.GetNodeData(newLevel.Node) ;
                                              TreeRec.Columns := TStringList.create() ;
                                              TreeRec.Columns.Add ('') ;       // 0 MessageTime
                                              TreeRec.Columns.Add ('') ;       // 1 THID
                                              TreeRec.Columns.add (tempStr) ;  // 2 col1
                                              TreeRec.Columns.Add ('') ;       // 3 col2
                                              TreeRec.Columns.Add ('') ;       // 4 col3
                                              CurrentLevel.Col2Empty := true ;
                                           end else begin  // previous added node exist
                                              ActiveNode := CurrentLevel.CurrentChild ;
                                              CurrentLevel.CurrentChild := CurrentLevel.CurrentChild.NextSibling ;
                                              newLevel.Node := ActiveNode ;
                                              newLevel.CurrentChild := ActiveNode.FirstChild ;
                                              // ensure node is initialized. Needed when the node is free to call onFreeNode
                                              WatchForm.VstMain.ReinitNode(newLevel.Node,false);
                                              TreeRec := WatchForm.VstMain.GetNodeData(newLevel.Node) ;

                                              if tempStr <> '' then begin
                                                 TreeRec.Columns[2] := tempStr ;    // to do : change color if <>
                                                 CurrentLevel.Col2Empty := false ;
                                              end else
                                                 CurrentLevel.Col2Empty := true ;
                                           end ;
                                           stack.Push (CurrentLevel) ;
                                           CurrentLevel := newLevel ;
                                        end ;

                                        CurrentLevel.Col3Empty := true ;
                                     end else begin   // End Watch
                                        stack.Push (CurrentMember) ;
                                        CurrentMember := TMember.create (TempStr);
                                     end ;
                                  end ;

         CST_MEMBER_COL2 :        begin
                                     TempStr := getStr() ;
                                     if IsWatch then begin
                                        TreeRec.Columns[3] := tempStr ;    // to do : change color if <>
                                        CurrentLevel.Col2Empty := false ;
                                     end else begin
                                        CurrentMember.Col2 := TempStr ;
                                     end ;
                                  end ;

         CST_MEMBER_COL3 :        begin
                                     TempStr := getStr() ;
                                     if IsWatch then begin
                                        TreeRec.Columns[4] := TempStr ;    // to do : change color if <>
                                        CurrentLevel.Col3Empty := false ;
                                     end else begin
                                        CurrentMember.Col3 := TempStr ;
                                     end ;
                                  end ;

         CST_MEMBER_VIEWER_KIND : begin
                                    if IsWatch = false then
                                        CurrentMember.ViewerKind := getInt('CST_MEMBER_VIEWER_KIND(ViewKind:I1)') ;
                                  end ;

         CST_MEMBER_FONT_DETAIL : begin
                                     ParseFontDetail(CurrentMember.FontDetails) ;
                                  end ;

         CST_ADD_MEMBER :         begin     // CST_ADD_MEMBER is called after CST_CREATE_MEMBER, CST_MEMBER_COL2 and CST_MEMBER_COL3
                                     if IsWatch then begin

                                        if CurrentLevel.Col2Empty = true then
                                           TreeRec.Columns[3] := '' ;          // to do : change color if <>
                                        if CurrentLevel.Col3Empty = true then
                                           TreeRec.Columns[4] := '' ;          // to do : change color if <>

                                        // delete all unused nodes on CurrentLevel
                                        while CurrentLevel.CurrentChild <> nil do begin
                                           ActiveNode := CurrentLevel.CurrentChild ;
                                           CurrentLevel.CurrentChild := CurrentLevel.CurrentChild.NextSibling ;
                                           WatchForm.VstMain.DeleteNode(ActiveNode);
                                        end ;

                                        if stack.Count <> 0 then begin
                                           // free currentlevel
                                           CurrentLevel.Free ;
                                           // pop currentlevel
                                           CurrentLevel := TLevel (stack.pop() )
                                        end ;

                                     end else begin
                                        ActiveNodeChanged := true ;    // for filtering
                                        MemberToAdd := CurrentMember ;
                                        if stack.Count <> 0 then begin   // stack should have at least one element
                                           CurrentMember := TMember (stack.pop() ) ;
                                           CurrentMember.SubMembers.Add(MemberToAdd) ;
                                        end ;
                                     end ;
                                  end ;

         CST_FOCUS_NODE :         begin
                                     if (TraceForm = Frm_Trace) and (TraceForm.Visible = false) then begin
                                        //TraceForm.Visible := true ;
                                        Frm_Tool.actViewMainTracesExecute(nil) ;
                                     end ;

                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     if (ParentCompoNode <> nil) and (TraceForm.VstMain.IsVisible[ParentCompoNode]) then begin
                                        TraceForm.VstMain.ScrollIntoView (ParentCompoNode,true);
                                     end ;
                                  end ;

         CST_SELECT_NODE :        begin
                                     if (TraceForm = Frm_Trace) and (TraceForm.Visible = false) then begin
                                        //TraceForm.Visible := true ;
                                        Frm_Tool.actViewMainTracesExecute(nil) ;
                                     end ;

                                     TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                  end ;

         CST_CLEAR_NODE :         begin   // param : the node to clear
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     TraceForm.VstMain.DeleteNode(ParentCompoNode);
                                  end ;

         CST_CLEAR_SUBNODES :     begin   // param : the parent node
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     // clear children : get first and last child
                                     if ParentCompoNode <> nil then
                                        TraceForm.VstMain.DeleteChildren(ParentCompoNode,true);

                                  end ;

         CST_CLEAR_ALL :          begin
                                     // if traceform is the main trace windows and not visible : show it
                                     if (TraceForm = Frm_Trace) and (TraceForm.Visible = false) then begin
                                        //TraceForm.Visible := true ;
                                        Frm_Tool.actViewMainTracesExecute(nil) ;
                                     end ;
                                     TraceForm.ClearWin() ;
                                  end ;
         CST_FIND_TEXT :          begin
                                     // Find (text, bool Sensitive, bool WholeWord , bool highlight, bool SearchInAllPages)
                                     // Search criteria are independant of the window. You must call CST_FIND_NEXT to position to next matching node
                                     // param : int (Sensitive<<3+WholeWord<<2+highlight<<1+SearchInAllPages) , string
                                     TempStr := getIntAndStr (TempInt,'CST_FIND_TEXT(Flags:I1)') ;
                                     unt_search.SearchText := TempStr ;
                                     unt_search.UpperCaseSearchText := AnsiUpperCase(unt_search.SearchText) ;
                                     unt_search.LenSearchText       := length (unt_search.SearchText) ;

                                     // Sensitive
                                     TempInt2 := TempInt and 8 ;
                                     //TFrm_Trace.InternalTrace ('Sensitive : ' + inttostr(TempInt2));

                                     unt_search.MatchCase := (TempInt2 > 0) ;

                                     // WholeWord
                                     TempInt2 := TempInt and 4 ;
                                     //TFrm_Trace.InternalTrace ('WholeWord : ' + inttostr(TempInt2));
                                     unt_search.MatchWholeWord := (TempInt2 > 0) ;

                                     // highlight
                                     TempInt2 := TempInt and 2 ;
                                     //TFrm_Trace.InternalTrace ('highlight : ' + inttostr(TempInt2));
                                     if TempInt2 > 0 then
                                        unt_search.SearchKind := mrYesToAll
                                     else
                                        unt_search.SearchKind := mrYes ;

                                     // SearchInAllPages
                                     TempInt2 := TempInt and 1 ;
                                     //TFrm_Trace.InternalTrace ('SearchInAllPages : ' + inttostr(TempInt2));
                                     unt_search.SearchInAllPages := (TempInt2 > 0) ;

                                     // reset highlight : refresh the differents gutters
                                     for TempInt := 0 to BaseList.Count -1 do begin
                                        Base := TFrmBase (BaseList[TempInt]) ;
                                        Base.RefreshView ;
                                     end ;
                                  end ;
         CST_FIND_NEXT :          // WinTrace.FindNext(bool forward)  param : int
                                  if getInt('CST_FIND_NEXT(NextOrPrevious:I1)') > 0 then  // search the next record
                                     TraceForm.getPageContainer().actFindNext.Execute()          // TraceForm.SearchNext(false)
                                  else
                                     TraceForm.getPageContainer().actFindPrevious.Execute() ;    // TraceForm.SearchPrevious(false)

         CST_GOTO_NEXTSIBLING :   begin // ITraceNode.GotoNextSibling ()                                               param : node
                                     TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     ParentCompoNode := ParentCompoNode.NextSibling ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                  end ;

         CST_GOTO_PREVSIBLING :   begin // ITraceNode.GotoPrevSibling ()                                               param : node
                                      TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     ParentCompoNode := ParentCompoNode.PrevSibling ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                 end ;

         CST_GOTO_FIRST_CHILD :   begin // ITraceNode.GotoFirstChild  ()                                               param : node
                                     TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     ParentCompoNode := ParentCompoNode.FirstChild ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                  end ;

         CST_GOTO_LAST_CHILD  :   begin // ITraceNode.GotoLastChild   ()                                               param : node
                                     TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.CheckNode (TraceForm.VstMain.RootNode, AnsiString(getStr())) ;
                                     ParentCompoNode := ParentCompoNode.LastChild ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                  end ;

         CST_GOTO_FIRST_NODE :    begin  // WinTrace.GotoFirstNode()  // param : none
                                     TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.vst.GetFirst() ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                  end ;

         CST_GOTO_LAST_NODE  :    begin   // WinTrace.GotoLastNode() ;                                                   param : node
                                     TraceForm.SetActivePage() ;
                                     ParentCompoNode := TraceForm.vst.GetLast() ;
                                     TraceForm.VstMain.ClearSelection();
                                     if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                        TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                        TraceForm.NodeToFocus := ParentCompoNode ;
                                        // in case of the node was already selected, force display of properties
                                        TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                     end ;
                                  end ;

         CST_GOTO_BOOKMARK   :    begin   // WinTrace.GotoBookmark(pos : integer);
                                     TraceForm.SetActivePage() ;
                                     TempInt := getInt('CST_GOTO_BOOKMARK(pos:I1)') ;
                                     if TempInt < TraceForm.bookmarks.Count then begin
                                        ParentCompoNode := TraceForm.bookmarks[TempInt] ;
                                        TraceForm.VstMain.ClearSelection();
                                        if TraceForm.VstMain.IsVisible[ParentCompoNode] then begin
                                           TraceForm.VstMain.Selected [ParentCompoNode] := true ;   // ParentCompoNode can be nil
                                           TraceForm.NodeToFocus := ParentCompoNode ;
                                           // in case of the node was already selected, force display of properties
                                           TraceForm.VstMainChange(TraceForm.VstMain,  ParentCompoNode);
                                        end ;
                                     end ;
                                  end ;

         CST_CLEAR_BOOKMARK  :    begin   // WinTrace.ClearBookmark();
                                     TraceForm.bookmarks.Clear() ;
                                  end ;

         CST_CLEAR_FILTER    :    begin   // WinTrace.ClearFilter() ;                                                    param : node
                                     if TraceForm.filter = nil then
                                        TraceForm.filter := TFrmFilter.Create(TraceForm);     // create the filter and add it a empty filter row
                                     TraceForm.InitColumns() ;            // fill filter.columnsNameList with column names
                                     TraceForm.filter.FillColumns();      // fill combobox in each filter row with column names
                                     TraceForm.Filter.ResetFilter() ;
                                  end ;

         CST_ADD_FILTER      :    begin   // WinTrace.AddFilter(column,compare,text : string) ;
                                     if TraceForm.filter = nil then
                                        TraceForm.filter := TFrmFilter.Create(TraceForm);     // create the filter and add it a empty filter row
                                     TraceForm.InitColumns() ;            // fill filter.columnsNameList with column names
                                     TraceForm.filter.FillColumns();      // fill combobox in each filter row with column names
                                     // compare and text are separated by a Tab
                                     TempStr := get2IntAndStr(TempInt,TempInt2,'CST_ADD_FILTER(ColId:I1,Compare:I2)') ;
                                     TraceForm.filter.AddRow(tempint,tempint2,tempStr) ;
                                  end ;

         CST_APPLY_FILTER    :    begin   // WinTrace.ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren) ;        param : integer (3 bool)
                                     TempInt := getInt('CST_APPLY_FILTER(ConditionalAnd:I1)') ;
                                     TraceForm.filter.ApplyFilter(TempInt) ; // ConditionAnd, ShowMatch,IncludeChildren)
                                  end ;


         CST_CLOSE_WIN :            // no param. Close winwatch or wintrace
                                  begin
                                     TraceForm.CloseWin() ;
                                  end ;
         CST_CLOSE_VIEWER :         // no param : quit tracetool
                                  begin
                                     Frm_Tool.actShutdown.Execute ;
                                  end ;

         CST_SAVETOTEXT :         TraceForm.SaveToTextFile (getStr(), DefaultSaveTofileOptions); // save to text file, parameter : filename

         CST_SAVETOXML  :         TraceForm.SaveToXML (getStr());                                // save to  XML file, parameter : filename and styleSheet

         CST_LOADXML    :         begin
                                     // if traceform is the main trace windows and not visible : show it
                                     if (TraceForm = Frm_Trace) and (TraceForm.Visible = false) then begin
                                        //TraceForm.Visible := true ;
                                        Frm_Tool.actViewMainTracesExecute(nil) ;
                                     end ;
                                     // clear the windows before loading the XML file.
                                     TraceForm.ClearWin() ;
                                     try
                                        TempStr := getStr() ;
                                        if fileExists(TempStr) then
                                           TraceForm.LoadXML(TempStr)
                                        else
                                           TFrm_Trace.InternalTrace ('Error when loading ' + TempStr + ' : File not found');
                                     except
                                        on e : exception do
                                           TFrm_Trace.InternalTrace ('Error when loading ' + TempStr + ' : ' + e.message);
                                     end ;
                                  end ;

         CST_LOGFILE :            begin
                                     // param1 : Mode
                                     // param2 : filename
                                     TempStr := get2IntAndStr (TempInt,TempInt2,'CST_LOGFILE(LogFileType:I1,MaxLines:I2)') ;
                                     TraceForm.LogFileName := TempStr ;

                                     //// TempStr : filename and styleSheet
                                     //p := pos ('|' , TempStr) ;
                                     //if p <> 0 then begin
                                     //   TraceForm.LogFileName := trim(copy (TempStr, 1 , p-1)) ;
                                     //   TraceForm.LogStyleSheet := Trim(copy (TempStr, p+1 , 1000)) ;
                                     //end else begin
                                     //   TraceForm.LogFileName := TempStr ;
                                     //   TraceForm.LogStyleSheet := '' ;
                                     //end ;

                                     TraceForm.LogFileType := TempInt ;
                                     TraceForm.MaxLines := TempInt2 ;
                                     TraceForm.ShowLog ;  // change LabelLogFile caption
                                  end ;

         CST_LINKTOPLUGIN :       begin
                                     // param 1 : flags (int)
                                     // param 2 : plugin name (string)
                                     TempStr := getIntAndStr (TempInt,'CST_LINKTOPLUGIN(Flags:I1)') ;
                                     TraceForm.AddPlugin (AnsiString(TempStr) , TempInt) ;
                                  end ;

         CST_CREATE_RESOURCE:     begin
                                     // param 1 : resource id    (int)
                                     // param 2 : resource type  (int)
                                     // param 3 : resource width (int)
                                     // param 4 : resource text  (string)
                                     TempStr := get3IntAndStr (TempInt,TempInt2,TempInt3,'CST_CREATE_RESOURCE(resId:I1,resType:I2,resWidth:I3)') ;
                                     TraceForm.CreateResource (TempInt,TempInt2,TempInt3,TempStr);
                                  end ;

         CST_SET_TEXT_RESOURCE:   begin
                                     // param 1 : resource id (int)
                                     // param 2 : resource text (string)
                                     TempStr := getIntAndStr (TempInt,'CST_SET_TEXT_RESOURCE(resId:I1)') ;
                                     TraceForm.SetTextResource (TempInt,TempStr);
                                  end ;
         CST_DISABLE_RESOURCE :   begin
                                     // param 1 : resource id (int)
                                     TraceForm.DisableResource(getInt('CST_DISABLE_RESOURCE(resId:I1)'));
                                  end ;


         // no else : unknow command do nothing
         end ;
      end ; // next line to interpret
      
      CurrentParseCommandIndex := -1 ;
      if CurrentLevel <> nil then
         CurrentLevel.free ;

      if WatchNode <> nil then
         WatchForm.VstMain.Expanded [WatchNode] := WatchExpanded ;

      // if new node then save to log file
      if IsNewNode then begin
         TraceForm.AddToLog (ActiveNode , ParentCompoNode ) ;
      end ;

      if ActiveNodeChanged and TraceForm.VstMain.IsVisible[ActiveNode] and TraceForm.VstMain.Selected [ActiveNode] then begin
         // refresh if needed
         TraceForm.VstMainChange(TraceForm.VstMain,  ActiveNode);
      end ;

      // filtering
      if (TraceForm.filter <> nil) and (ActiveNodeChanged) then begin
         if WatchNode <> nil then begin
            ActiveNode := WatchNode ;
            TraceForm := WatchForm ;
         end ;
         if ActiveNode.Parent = TraceForm.VstMain.RootNode then begin      // parent is root
            TraceForm.filter.CheckNode(ActiveNode)
         end else if TraceForm.filter.chkCheckChildren.checked then begin
            ParentCompoNode := ActiveNode.Parent ;
            ParentNodeVisible := true ;
            while ParentCompoNode <> TraceForm.VstMain.RootNode do begin   // check if all parent are visibles
               if TraceForm.VstMain.IsVisible[ParentCompoNode] = false then begin
                  ParentNodeVisible := false ;  // don't wast time to check for filter if the parent node is not visible
                  break ;
               end ;
               ParentCompoNode := ParentCompoNode.Parent ;
            end ;
            if ParentNodeVisible then
                TraceForm.filter.CheckNode(ActiveNode)
         end ;
      end ;
      CurrentParseMsgList := nil ;  // reset reference
   //except
   //   on E:Exception do begin
   //      ParentCompoNode := TFrm_Trace.InternalTrace('Exception in TraceTool. Line (' + inttostr (CommandIndex) + ')'
   //                         + ' : ' + e.Message) ;
   //      for CommandIndex := 0 to MsgList.Count-1 do begin
   //         SingleMsg := MsgList[CommandIndex] ;
   //         TFrm_Trace.InternalTrace (ParentCompoNode,'[' + inttostr(CommandIndex) + '] : ' + SingleMsg) ;
   //      end ;
   //
   //   end ;
   //end
   finally
      stack.Free ;
   end ;
end ;


end.
