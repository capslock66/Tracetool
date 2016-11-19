{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_SelectTail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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

uses Unt_Tool, DebugOptions, Config, unt_base , XMLIntf,
  Unt_TailProgress;

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
      Frm_Tool.OpenDialog1.InitialDir := XMLConfig.tail.LastPath.Value ;
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
   XMLConfig.tail.Favorites.Add(Frm_Tool.OpenDialog1.FileName) ;
end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.butDeletefileClick(Sender: TObject);
var
   fname : string ;
   c : integer ;
begin
   fname := FavoriteFiles.items.Strings [FavoriteFiles.ItemIndex] ;
   FavoriteFiles.items.Delete (FavoriteFiles.ItemIndex) ;
   for c := 0 to XMLConfig.tail.Favorites.Count-1 do begin
      if XMLConfig.tail.Favorites.FileName[c] = fname then begin
        XMLConfig.tail.Favorites.Delete(c);
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
   for c := 0 to XMLConfig.tail.Favorites.Count-1 do
      FavoriteFiles.AddItem(XMLConfig.tail.Favorites.FileName[c],nil);

   Frm_Tool.OpenDialog1.Filter := 'Text file (*.txt)|*.txt|Log file (*.log)|*.log' ;
   Frm_Tool.OpenDialog1.InitialDir := XMLConfig.Tail.LastPath.Value ;

   if XMLConfig.tail.ColumnStyle.value = 'Classic' then ComboColumnsStyle.ItemIndex := 0 ; 
   if XMLConfig.tail.ColumnStyle.value = 'Lines'   then ComboColumnsStyle.ItemIndex := 1 ; 
   if XMLConfig.tail.ColumnStyle.value = 'Multi'   then ComboColumnsStyle.ItemIndex := 2 ;

   if XMLConfig.tail.AutoCreateColStyle.value = 'First'  then rbFromFirstLine.Checked := true ;
   if XMLConfig.tail.AutoCreateColStyle.value = 'Each'   then rbEachLines.Checked := true ;
   if XMLConfig.tail.AutoCreateColStyle.value = 'Fixed'  then rbFixedcol.Checked := true ;

   if XMLConfig.tail.TextQualifier.value = 'None'   then ComboQualifier.ItemIndex := 0 ;
   if XMLConfig.tail.TextQualifier.value = 'Single' then ComboQualifier.ItemIndex := 1 ;
   if XMLConfig.tail.TextQualifier.value = 'Double' then ComboQualifier.ItemIndex := 2 ;

   EditSeparator.Text := XMLConfig.tail.Separator.value ;
   chkTitle.Checked   := XMLConfig.tail.FirstcolIsTitle.value ;
   EditColNumber.Text := inttostr(XMLConfig.tail.FixedColCount.value) ;

   if XMLConfig.tail.OpenFromFavorites.value = false then rbSingleFile.Checked := true ;
   if XMLConfig.tail.OpenFromFavorites.value = true then rbFavorite.Checked := true ;

   cbColumnsClick (nil) ;    // force enable/disable sub controls
   rbSingleFileClick(nil) ;  // force enable/disable sub controls

end;

//------------------------------------------------------------------------------

procedure TFrmSelectTail.SaveOptions;
begin
   XMLConfig.Tail.LastPath.Value := ExtractFilePath(Frm_Tool.OpenDialog1.FileName) ;

   if ComboColumnsStyle.ItemIndex = 0 then XMLConfig.tail.ColumnStyle.value := 'Classic' ;
   if ComboColumnsStyle.ItemIndex = 1 then XMLConfig.tail.ColumnStyle.value := 'Lines' ;
   if ComboColumnsStyle.ItemIndex = 2 then XMLConfig.tail.ColumnStyle.value := 'Multi' ;

   if rbFromFirstLine.Checked then XMLConfig.tail.AutoCreateColStyle.value := 'First' ;
   if rbEachLines.Checked     then XMLConfig.tail.AutoCreateColStyle.value := 'Each'  ;
   if rbFixedcol.Checked      then XMLConfig.tail.AutoCreateColStyle.value := 'Fixed' ;

   XMLConfig.tail.FirstcolIsTitle.value  := chkTitle.Checked  ;
   XMLConfig.tail.FixedColCount.value    := StrToIntDef(editColNumber.Text,1) ;

   XMLConfig.tail.TextQualifier.value := ComboQualifier.Text ;

   if strtointDef(EditSeparator.text,-1) = -1 then
      XMLConfig.tail.Separator.value := EditSeparator.text
   else
      XMLConfig.tail.Separator.value := EditSeparator.text ; 

   if rbSingleFile.Checked = true then XMLConfig.tail.OpenFromFavorites.value := false ;
   if rbFavorite.Checked   = true then XMLConfig.tail.OpenFromFavorites.value := true  ;


   frmDebugOptions.SaveSettings ;
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
