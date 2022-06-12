{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_SelectTail;

interface

uses
  system.Contnrs, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, CheckLst, unt_tail,
  pscMenu;

type
  TFrmSelectTail = class(TForm)
    Label1: TLabel;
    ComboColumnsStyle: TComboBox;
    butSelect: TBitBtn;
    EditFileName: TEdit;
    butOpen: TButton;
    butCancel: TButton;
    rbSingleFile: TRadioButton;
    rbFavorite: TRadioButton;
    FavoriteFiles: TCheckListBox;
    Panel4: TPanel;
    ButAddfile: TButton;
    butDeletefile: TButton;
    Panel5: TPanel;
    rbFromFirstLine: TRadioButton;
    chkTitle: TCheckBox;
    rbEachLines: TRadioButton;
    rbFixedcol: TRadioButton;
    EditColNumber: TEdit;
    Panel6: TPanel;
    LabelTxtQualifier: TLabel;
    LabelSeparator1: TLabel;
    ComboQualifier: TComboBox;
    EditSeparator: TEdit;
    Label2: TLabel;
    procedure butSelectClick(Sender: TObject);
    procedure cbColumnsClick(Sender: TObject);
    procedure butOpenClick(Sender: TObject);
    procedure ButAddfileClick(Sender: TObject);
    procedure butDeletefileClick(Sender: TObject);
    procedure rbSingleFileClick(Sender: TObject);
    procedure EditFileNameKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FavoriteFilesClickCheck(Sender: TObject);
    procedure FavoriteFilesDblClick(Sender: TObject);
  private
  public
     procedure initOptions ;
     procedure SaveOptions ;
     function LoadFile(fileName: string) : TFrmTail;
     procedure OpenFile(FileName: string);
    { Private declarations }
  end;

var
  FrmSelectTail: TFrmSelectTail;

   TailOption : integer ;

implementation

uses
   Unt_Tool
   , DebugOptions
   , unt_TraceConfig
   , unt_base
   , XMLIntf
   , Unt_TailProgress;

{$R *.dfm}


//------------------------------------------------------------------------------

procedure TFrmSelectTail.cbColumnsClick(Sender: TObject);
begin
   // 0 : Show Approximative time and lines
   // 1 : Show lines only
   // 2 : Multi columns

   rbFromFirstLine  .Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 
   rbEachLines      .Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 
   rbFixedcol       .Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 
   EditSeparator    .Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 
   LabelSeparator1  .Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 
   LabelTxtQualifier.Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 
   ComboQualifier   .Enabled := (ComboColumnsStyle.ItemIndex = 2) ; 

   if (rbFromFirstLine.Enabled) and (rbFromFirstLine.Checked) then
      chkTitle.Enabled := true
   else
      chkTitle.Enabled := false ;

   if (rbFixedcol.Enabled) and (rbFixedcol.Checked) then
      EditColNumber.Enabled := true
   else
      EditColNumber.Enabled := false ;
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.butSelectClick(Sender: TObject);
begin
   if EditFileName.Text <> '' then
      Frm_Tool.OpenDialog1.FileName := EditFileName.Text
   else begin
      Frm_Tool.OpenDialog1.FileName := '' ;
      Frm_Tool.OpenDialog1.InitialDir := TraceConfig.tail_LastPath ;
   end ;

   if Frm_Tool.OpenDialog1.Execute = false then
      exit ;
   EditFileName.Text := Frm_Tool.OpenDialog1.FileName ;
   rbSingleFileClick (nil) ; // force validation
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.ButAddfileClick(Sender: TObject);
begin
   if Frm_Tool.OpenDialog1.Execute = false then
      exit ;
   FavoriteFiles.AddItem (Frm_Tool.OpenDialog1.FileName,nil) ;
   TraceConfig.FavoriteTailList.Add(Frm_Tool.OpenDialog1.FileName) ;
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.butDeletefileClick(Sender: TObject);
var
   fname : string ;
   c : integer ;
begin
   fname := FavoriteFiles.items.Strings [FavoriteFiles.ItemIndex] ;
   FavoriteFiles.items.Delete (FavoriteFiles.ItemIndex) ;
   for c := 0 to TraceConfig.FavoriteTailList.Count-1 do begin
      if TraceConfig.FavoriteTailList[c] = fname then begin
        TraceConfig.FavoriteTailList.Delete(c);
        break ;
      end ;
   end ;
   rbSingleFileClick (nil) ; // force validation
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.FavoriteFilesClickCheck(Sender: TObject);
begin
   rbSingleFileClick (nil) ; // force validation
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.FavoriteFilesDblClick(Sender: TObject);
begin
  FavoriteFiles.Checked [FavoriteFiles.ItemIndex] := not FavoriteFiles.Checked [FavoriteFiles.ItemIndex] ;
  rbSingleFileClick (nil) ; // force validation
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.EditFileNameKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   rbSingleFileClick (nil) ; // force validation
end;

//------------------------------------------------------------------------------

// force validation
procedure TFrmSelectTail.rbSingleFileClick(Sender: TObject);
var
   c : integer ;
   flagEnable : boolean ;
begin

   if rbSingleFile.Checked then begin       // single file
      editFileName .Enabled := true ;
      butSelect    .Enabled := true ;

      FavoriteFiles.Enabled := false ;
      ButAddfile   .Enabled := false ;
      butDeletefile.Enabled := false ;

      if trim(EditFileName.Text) = '' then
         butOpen.Enabled := false
      else
        butOpen.Enabled := true ;
   end else begin                           // multiple files
      editFileName .Enabled := false ;
      butSelect    .Enabled := false ;

      FavoriteFiles.Enabled := true ;
      ButAddfile   .Enabled := true ;
      butDeletefile.Enabled := true ;

      flagEnable := false ;
      for c := 0 to FavoriteFiles.Count-1 do begin
         if FavoriteFiles.Checked[c] then begin
            flagEnable := true ;
            break ;
         end ;
      end ;
      butOpen.Enabled := flagEnable ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.initOptions;
var
   c : integer ;
begin
   // reload all favorites
   FavoriteFiles.Clear ;
   for c := 0 to TraceConfig.FavoriteTailList.Count-1 do
      FavoriteFiles.AddItem(TraceConfig.FavoriteTailList[c],nil);

   Frm_Tool.OpenDialog1.Filter := 'Text file (*.txt)|*.txt|Log file (*.log)|*.log' ;
   Frm_Tool.OpenDialog1.InitialDir := TraceConfig.Tail_LastPath;

   if TraceConfig.tail_ColumnStyle = 'Classic' then ComboColumnsStyle.ItemIndex := 0 ;
   if TraceConfig.tail_ColumnStyle = 'Lines'   then ComboColumnsStyle.ItemIndex := 1 ;
   if TraceConfig.tail_ColumnStyle = 'Multi'   then ComboColumnsStyle.ItemIndex := 2 ;

   if TraceConfig.tail_AutoCreateColStyle = 'First'  then rbFromFirstLine.Checked := true ;
   if TraceConfig.tail_AutoCreateColStyle = 'Each'   then rbEachLines.Checked := true ;
   if TraceConfig.tail_AutoCreateColStyle = 'Fixed'  then rbFixedcol.Checked := true ;

   if TraceConfig.tail_TextQualifier = 'None'   then ComboQualifier.ItemIndex := 0 ;
   if TraceConfig.tail_TextQualifier = 'Single' then ComboQualifier.ItemIndex := 1 ;
   if TraceConfig.tail_TextQualifier = 'Double' then ComboQualifier.ItemIndex := 2 ;

   EditSeparator.Text := TraceConfig.tail_Separator ;
   chkTitle.Checked   := TraceConfig.tail_FirstcolIsTitle ;
   EditColNumber.Text := inttostr(TraceConfig.tail_FixedColCount) ;

   if TraceConfig.tail_OpenFromFavorites = false then rbSingleFile.Checked := true ;
   if TraceConfig.tail_OpenFromFavorites = true then rbFavorite.Checked := true ;

   cbColumnsClick (nil) ;    // force enable/disable sub controls
   rbSingleFileClick(nil) ;  // force enable/disable sub controls

end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.SaveOptions;
begin
   TraceConfig.Tail_LastPath := ExtractFilePath(Frm_Tool.OpenDialog1.FileName) ;

   if ComboColumnsStyle.ItemIndex = 0 then TraceConfig.Tail_ColumnStyle := 'Classic' ;
   if ComboColumnsStyle.ItemIndex = 1 then TraceConfig.Tail_ColumnStyle := 'Lines' ;
   if ComboColumnsStyle.ItemIndex = 2 then TraceConfig.Tail_ColumnStyle := 'Multi' ;

   if rbFromFirstLine.Checked then TraceConfig.Tail_AutoCreateColStyle := 'First' ;
   if rbEachLines.Checked     then TraceConfig.Tail_AutoCreateColStyle := 'Each'  ;
   if rbFixedcol.Checked      then TraceConfig.Tail_AutoCreateColStyle := 'Fixed' ;

   TraceConfig.Tail_FirstcolIsTitle  := chkTitle.Checked  ;
   TraceConfig.Tail_FixedColCount    := StrToIntDef(editColNumber.Text,1) ;

   TraceConfig.Tail_TextQualifier := ComboQualifier.Text ;

   if strtointDef(EditSeparator.text,-1) = -1 then
      TraceConfig.Tail_Separator := EditSeparator.text
   else
      TraceConfig.Tail_Separator := EditSeparator.text ;

   if rbSingleFile.Checked = true then TraceConfig.Tail_OpenFromFavorites := false ;
   if rbFavorite.Checked   = true then TraceConfig.Tail_OpenFromFavorites := true  ;


   Frm_Tool.SaveSettings() ;
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.OpenFile(FileName: string);
var
   tail : TFrmTail ;
begin
   SetCursor(Screen.Cursors[crHourGlass]);
   try
      tail := LoadFile (FileName) ;
      tail.SetActivePage ;  // switch to the file
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end;
end ;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.butOpenClick(Sender: TObject);
var
   c : integer ;
   tail : TFrmTail ;
begin
   Frm_Tool.OpenDialog1.FileName := EditFileName.Text ;
   SaveOptions ;
   hide ;

   SetCursor(Screen.Cursors[crHourGlass]);
   try
      tail := nil ;
      if rbSingleFile.checked then begin
         if Frm_Tool.OpenDialog1.FileName <> '' then
            tail := LoadFile (Frm_Tool.OpenDialog1.FileName) ;
      end else begin
         for c := 0 to FavoriteFiles.Count-1 do begin
            if FavoriteFiles.Checked[c] then
               tail := LoadFile (FavoriteFiles.Items[c]) ;
         end ;
      end ;
      tail.SetActivePage ;  // switch to the file
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end;
end;

//------------------------------------------------------------------------------

function TFrmSelectTail.LoadFile (fileName : string) : TFrmTail;
var
   c : integer ;
begin
   // detect if file already opened
   for c := 0 to TailList.Count -1 do begin
      result := TFrmTail (TailList[c]) ;
      if result.TailFile = fileName then begin
         exit ;
      end ;
   end ;

   // create the tail form
   result := TFrmTail.Create(Application) ; // Application.CreateForm(TFrmTail, result);

   result.caption := 'Tail::' + ExtractFileName (fileName) ;
   result.DragKind := dkDock ;
   result.DockToMainPanel();

   // since all tail form can handle columns in different ways, we must recopy options on each tail form
   result.ShowTimeAndLines := (ComboColumnsStyle.ItemIndex = 0) ;
   result.ShowOnlyLines    := (ComboColumnsStyle.ItemIndex = 1) ;
   result.ShowManycolumns  := (ComboColumnsStyle.ItemIndex = 2) ;

   result.DetectSeparatorOnfirstLine := rbFromFirstLine.Checked and result.ShowManycolumns ;
   result.DetectSeparatorOnEachLine  := rbEachLines.Checked and result.ShowManycolumns ;
   result.DetectTitleOnFirstLine     := chkTitle.Checked and result.ShowManycolumns ;
   result.IsFixedColumns             := rbFixedcol.Checked and result.ShowManycolumns ;
   result.FixedColCount              := StrToIntDef(editColNumber.Text,1) ;

   if ComboQualifier.ItemIndex = 0 then
      result.TextQualifier := AnsiChar(chr(0))
   else if ComboQualifier.ItemIndex = 1 then
      result.TextQualifier := ''''
   else if ComboQualifier.ItemIndex = 2 then
      result.TextQualifier := '"' ;

   if trim(EditSeparator.text) = '' then
      result.ColSeparator := AnsiChar(chr(0))   // no separator
   else if strtointDef(EditSeparator.text,-1) = -1 then
      result.ColSeparator := AnsiChar(EditSeparator.text[1])
   else
      result.ColSeparator := AnsiChar(chr (StrToIntDef(EditSeparator.text, 9))) ;

   result.TailFile := fileName ;
   result.DirMon.Path := ExtractFilePath (result.TailFile) ;
   if not FileExists(result.TailFile) then
      result.FileStatus :=  'No file' ;

   result.show ;
   try
      result.DirMon.Active := true ;   // can raise EDirMonError
      if result.DirMon.error <> '' then
         result.FileStatus := result.DirMon.error ;
   except
      on e : exception do begin
         result.FileStatus := e.Message ;
      end ;
   end ;
   result.InitColumns ;    // call loadFile() after reading header
   result.Display ;        // display some line from the end of file
end ;


end.
