unit unt_Details_bitmap;

interface

uses
  system.Contnrs, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, ExtCtrls, StdCtrls, Dialogs, clipbrd, Menus ,
  unt_Details_base,
  unt_TraceWin,
  unt_tool;

type
  Tframe_BitmapDetails = class(Tframe_BaseDetails)
    ScrollBox1: TScrollBox;
    ImageViewer: TImage;
    PopupDetail: TPopupMenu;
    copyMenu: TMenuItem;
    procedure copyMenuClick(Sender: TObject);
  public
    { Public declarations }
    Procedure AddDetails(TreeRec: PTreeRec; RootMember : TMember); override;
    function HasFocus : boolean ; override;
    procedure SelectAll() ; override ;
    procedure copySelected() ; override;
  end;

var
  frame_BitmapDetails: Tframe_BitmapDetails;

implementation

{$R *.dfm}
Uses
  unt_Decode;

{ Tframe_BitmapDetails }

procedure Tframe_BitmapDetails.AddDetails(TreeRec: PTreeRec; RootMember : TMember);
var
   MemStream: TMemoryStream;
   S: AnsiString;
begin
   inherited;
   MemStream := TMemoryStream.Create;
   Try
      S := Decode(AnsiString(RootMember.col1));
      MemStream.Write(S[1], Length(S));
      MemStream.Position := 0;

      ImageViewer.Picture.Bitmap.LoadFromStream(MemStream);
      ImageViewer.Width  := ImageViewer.Picture.Bitmap.Width;
      ImageViewer.Height := ImageViewer.Picture.Bitmap.Height;

   Finally
      MemStream.Free;
   End;
   TFrm_Trace(Owner).CurrentViewers.add(self) ;
   //TFrm_Trace(Owner).BitmapVisible := true ;   // viewer will be visible
   //inc (TFrm_Trace(Owner).ViewerCount) ;       // need to know the number of viewer to display
   TFrm_Trace(Owner).AddOneLineDetail('Image dimension',IntToStr(ImageViewer.Width)+'x'+IntToStr(ImageViewer.Height),'');
end;

//------------------------------------------------------------------------------

procedure Tframe_BitmapDetails.copySelected;
var
   wFormat:word;
   wHandle:THandle;
   wPalette:HPalette;
begin
   ImageViewer.Picture.SaveToClipboardFormat (wformat,whandle,wpalette);
   ClipBoard.SetAsHandle(wFormat,wHandle);
end;

//------------------------------------------------------------------------------

function Tframe_BitmapDetails.HasFocus: boolean;
begin
  result := false ;
end;

//------------------------------------------------------------------------------

// CTRL-A : select all
procedure Tframe_BitmapDetails.SelectAll;
begin
   // no selection
end;

//------------------------------------------------------------------------------

procedure Tframe_BitmapDetails.copyMenuClick(Sender: TObject);
begin
   copySelected();
end;

end.
