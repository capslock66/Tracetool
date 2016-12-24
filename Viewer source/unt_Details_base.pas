unit unt_Details_base;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls ,
  VirtualTrees,
  unt_TraceWin,
  unt_tool ;

{$Include TraceTool.Inc}

type
  TFrameBaseClass = class of Tframe_BaseDetails ;
  Tframe_BaseDetails = class(TFrame)
  public
    Splitter : TSplitter ;  // will be create at runtime, but not inside the frame
    constructor Create(AOwner: TComponent); override;
    Procedure AddDetails(TreeRec: PTreeRec; RootMember : TMember); virtual; abstract;
    procedure SelectAll() ; virtual; abstract;
    function HasFocus : boolean ; virtual; abstract;
    procedure copySelected() ; virtual; abstract;
  end;

  procedure CopyDetail (Vst : TVirtualStringTree; CopyStrings: TStrings ; TestNode : PVirtualNode);

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure CopyDetail (Vst : TVirtualStringTree; CopyStrings: TStrings ; TestNode : PVirtualNode);
var
   node : PVirtualNode ;
   DetailRec : PDetailRec ;
   NewLine: string;
begin
   if Vst.Selected [TestNode] then begin

      DetailRec := Vst.GetNodeData(TestNode) ;
      NewLine := TraceConfig.TextExport_TextQualifier + DetailRec.Col1 + TraceConfig.TextExport_TextQualifier  +
                 TraceConfig.TextExport_Separator +
                 TraceConfig.TextExport_TextQualifier + DetailRec.Col2 + TraceConfig.TextExport_TextQualifier +
                 TraceConfig.TextExport_Separator +
                 TraceConfig.TextExport_TextQualifier + DetailRec.Col3 + TraceConfig.TextExport_TextQualifier  ;

      CopyStrings.Add(NewLine);
   end ;

   // multi select
   node := TestNode.FirstChild ;
   while Node <> nil do begin
      CopyDetail (vst,CopyStrings,node) ;
      node := node.NextSibling ;
   end ;
end ;

//------------------------------------------------------------------------------

{ Tframe_BaseDetails }

constructor Tframe_BaseDetails.Create(AOwner: TComponent);
begin
   inherited;
   Splitter := TSplitter.Create(AOwner);
   Splitter.Parent := TFrm_Trace(AOwner).PanelRight;
   Splitter.Align := alTop;
end;

end.
