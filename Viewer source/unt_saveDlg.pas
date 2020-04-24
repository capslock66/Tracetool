{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_saveDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, CheckLst, Buttons, DebugOptions;

type
  TFrmSave = class(TForm)
    rbXML: TRadioButton;
    rbTextFile: TRadioButton;
    butSave: TButton;
    butCancel: TButton;
    Label1: TLabel;
    EditXml: TEdit;
    EditText: TEdit;
    chkIncludeTitle: TCheckBox;
    CheckOptionsList: TCheckListBox;
    butxml: TBitBtn;
    butText: TBitBtn;
    Label2: TLabel;
    EditStyleSheet: TEdit;
    procedure rbTextFileClick(Sender: TObject);
    procedure rbXMLClick(Sender: TObject);
    procedure ButXMLClick(Sender: TObject);
    procedure butTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure butSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IsMultiColTree : boolean ;
  end;

var
  FrmSave: TFrmSave;
  SaveTofileOptions : TSaveTofileOptions ;
  DefaultSaveTofileOptions : TSaveTofileOptions ;

implementation

uses
  Unt_Tool
  , unt_TraceConfig
  ;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrmSave.FormCreate(Sender: TObject);
begin
   // default save to file options (using the framework) : save all

   DefaultSaveTofileOptions := TSaveTofileOptions.Create;
   DefaultSaveTofileOptions.Copy_ProcessName      := true ;   // save process name in clipboard export
   DefaultSaveTofileOptions.Copy_ThreadID         := true ;   // save thread id
   DefaultSaveTofileOptions.Copy_Time             := true ;   // save time
   DefaultSaveTofileOptions.Copy_Col1             := true ;   // save tree column
   DefaultSaveTofileOptions.Copy_Col2             := true ;   // save comment
   DefaultSaveTofileOptions.Copy_ColumnTitle      := true ;   // include column title in clipboard export

   // user defined save to file options (based on Clipboard options)

   CheckOptionsList.Items.Clear ;
   SaveTofileOptions := TSaveTofileOptions.Create;

   SaveTofileOptions.Copy_ProcessName := TraceConfig.TextExport_ProcessName  ;   // save process name in clipboard export
   CheckOptionsList.Items.Add('Process Name');
   CheckOptionsList.Checked[0] := TraceConfig.TextExport_ProcessName ;

   SaveTofileOptions.Copy_ThreadID    := TraceConfig.TextExport_ThreadId     ;   // save thread id
   CheckOptionsList.Items.Add('Thread ID');
   CheckOptionsList.Checked[1] := TraceConfig.TextExport_ThreadId ;

   SaveTofileOptions.Copy_Time        := TraceConfig.TextExport_Time         ;   // save time
   CheckOptionsList.Items.Add('Time');
   CheckOptionsList.Checked[2] := TraceConfig.TextExport_Time ;

   SaveTofileOptions.Copy_col1        := TraceConfig.TextExport_Col1         ;   // save tree column
   CheckOptionsList.Items.Add('Tree mesage');
   CheckOptionsList.Checked[3] := TraceConfig.TextExport_Col1 ;

   SaveTofileOptions.Copy_Col2     := TraceConfig.TextExport_Col2      ;   // save comment
   CheckOptionsList.Items.Add('Comment message');
   CheckOptionsList.Checked[4] := TraceConfig.TextExport_Col2 ;

   SaveTofileOptions.Copy_ColumnTitle := TraceConfig.TextExport_GenerateColumnHeader  ;   // include column title in clipboard export
   chkIncludeTitle.Checked := TraceConfig.TextExport_GenerateColumnHeader  ;

   EditStyleSheet.Text := TraceConfig.General_LastStyleSheet ;

   rbXMLClick (nil) ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.FormDestroy(Sender: TObject);
begin
   DefaultSaveTofileOptions.free ;
   SaveTofileOptions.Free ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.butSaveClick(Sender: TObject);
begin
   if rbXML.Checked then begin
      if EditXml.Text = '' then
         ModalResult := mrNone ;

      if EditStyleSheet.Text <> TraceConfig.General_LastStyleSheet then begin
         TraceConfig.General_LastStyleSheet := EditStyleSheet.Text ;
         // frmDebugOptions.SaveSettings() will be called at exit
      end ;

   end else begin  // rbTextFile
      if EditText.Text = '' then begin
         ModalResult := mrNone ;
         exit ;
      end ;

      SaveTofileOptions.Copy_ProcessName := CheckOptionsList.Checked[0] ;
      SaveTofileOptions.Copy_ThreadID    := CheckOptionsList.Checked[1] ;
      SaveTofileOptions.Copy_Time        := CheckOptionsList.Checked[2] ;
      SaveTofileOptions.Copy_col1        := CheckOptionsList.Checked[3] ;
      SaveTofileOptions.Copy_Col2        := CheckOptionsList.Checked[4] ;
      SaveTofileOptions.Copy_ColumnTitle := chkIncludeTitle.Checked ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.rbTextFileClick(Sender: TObject);
begin
   EditXml.Enabled := false ;
   butxml.Enabled := false ;

   EditText.Enabled := true ;
   butText.Enabled := true ;
   chkIncludeTitle.Enabled := true ;
   if not IsMultiColTree then
      CheckOptionsList.Enabled := true ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.rbXMLClick(Sender: TObject);
begin
   EditXml.Enabled := true ;
   butxml.Enabled := true ;

   EditText.Enabled := false ;
   butText.Enabled := false ;
   chkIncludeTitle.Enabled := false ;
   CheckOptionsList.Enabled := false ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.ButXMLClick(Sender: TObject);
begin
   if EditXml.Text <> '' then
      Frm_Tool.SaveDialog1.FileName := EditXml.Text
   else begin
      Frm_Tool.SaveDialog1.FileName := '' ;
      Frm_Tool.SaveDialog1.InitialDir := TraceConfig.General_LastSavedPath ;
   end ;

   Frm_Tool.SaveDialog1.Filter := 'Xml file (*.xml)|*.xml' ;
   if Frm_Tool.SaveDialog1.Execute = false then
      exit ;
   TraceConfig.General_LastSavedPath := ExtractFilePath(Frm_Tool.SaveDialog1.FileName) ;
   if ExtractFileExt (Frm_Tool.SaveDialog1.FileName) = '' then
      Frm_Tool.SaveDialog1.FileName := Frm_Tool.SaveDialog1.FileName + '.xml' ;
   EditXml.Text := Frm_Tool.SaveDialog1.FileName ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.butTextClick(Sender: TObject);
begin
   if EditText.Text <> '' then
      Frm_Tool.SaveDialog1.FileName := EditText.Text
   else begin
      Frm_Tool.SaveDialog1.FileName := '' ;
      Frm_Tool.SaveDialog1.InitialDir := TraceConfig.General_LastSavedPath ;
   end ;

   Frm_Tool.SaveDialog1.Filter := 'Text file |*.*' ;
   if Frm_Tool.SaveDialog1.Execute = false then
      exit ;
   TraceConfig.General_LastSavedPath := ExtractFilePath(Frm_Tool.SaveDialog1.FileName) ;
   EditText.Text := Frm_Tool.SaveDialog1.FileName ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------




end.
