{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_FrmPlugin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls , VirtualTrees, unt_plugin , unt_tool;

type
  TfrmPlugin = class(TFrame)
    LabelPlugName: TLabel;
    LabelStatus: TLabel;
    LabelTitleFileName: TLabel;
    Label2: TLabel;
    LabelFileName: TLabel;
    LabelClassName: TLabel;
    chkLoadAtStartup: TCheckBox;
    butLoadAndStart: TButton;
    butUnload: TButton;
    butStart: TButton;
    butStop: TButton;
    butStopAndUnload: TButton;
    butRemove: TButton;
    procedure butLoadAndStartClick(Sender: TObject);
    procedure butUnloadClick(Sender: TObject);
    procedure butStopAndUnloadClick(Sender: TObject);
    procedure butStartClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure chkLoadAtStartupClick(Sender: TObject);
    procedure butRemoveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    plugin : tplugin ;
    node : PVirtualNode ;  // pont to the tree node
    procedure Display ;
  end;

implementation

uses DebugOptions;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TfrmPlugin.butLoadAndStartClick(Sender: TObject);
begin
   plugin.DoLoad() ;
   plugin.DoStart() ;
   Display() ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.butUnloadClick(Sender: TObject);
begin
   plugin.DoUnload ;
   Display() ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.butStopAndUnloadClick(Sender: TObject);
begin
   plugin.DoStop ;
   plugin.DoUnload ;
   Display() ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.butStartClick(Sender: TObject);
begin
   plugin.DoStart ;
   Display() ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.butStopClick(Sender: TObject);
begin
   plugin.DoStop ;
   Display() ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.Display;
begin
   if (plugin.PlugName <> '') and (plugin.PlugName <> '_') then
      LabelPlugName.Caption := String(plugin.PlugName)
   else
      LabelPlugName.Caption := String(plugin.FileName) ;

   LabelPlugName.Caption := LabelPlugName.Caption + ' (' + plugin.plugKind + ')'  ;

   if plugin.plugKind = 'Java' then begin
      LabelFileName.Visible := false ;
      LabelTitleFileName.Visible := false ;
   end else begin
      LabelFileName.Visible := true ;
      LabelTitleFileName.Visible := true ;
      LabelFileName.Caption := String(plugin.FileName) ;
   end ;

   LabelClassName.Caption := plugin.className ;
   chkLoadAtStartup.Checked := plugin.startup ;

   case plugin.Status of
      psUnloaded :
         begin
            LabelStatus.caption := 'Unloaded' ;
            butLoadAndStart .Visible := true ;
            butLoadAndStart .top  := 160 ;
            butLoadAndStart .left := 16 ;
            butUnload       .Visible := false ;
            butStopAndUnload.Visible := false ;
            butStop         .Visible := false ;
            butStart        .Visible := false ;
            butRemove       .Visible := true ;

         end ;
      psLoaded   :
         begin
            LabelStatus.caption := 'Loaded' ;
            butLoadAndStart .Visible := false ;
            butUnload       .Visible := true ;
            butUnload       .top  := 160 ;
            butUnload       .left := 16 ;
            butStopAndUnload.Visible := false ;
            butStop         .Visible := false ;
            butStart        .Visible := true ;
            butStart        .top  := 160 ;
            butStart        .left := 112 ;
            butRemove       .Visible := false ;
         end ;
      psStarted  :
         begin
            LabelStatus.caption := 'Started' ;
            butLoadAndStart .Visible := false ;
            butUnload       .Visible := false ;
            butStop         .Visible := true ;
            butStop         .top  := 160 ;
            butStop         .left := 16 ;
            butStart        .Visible := false ;
            butStopAndUnload.Visible := false ; // true ;
            butStopAndUnload.top  := 160 ;
            butStopAndUnload.left := 112 ;
            butRemove       .Visible := false ;
        end ;
   end ;
   frmDebugOptions.VSTOptions.Refresh ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.chkLoadAtStartupClick(Sender: TObject);
begin
   plugin.startup := chkLoadAtStartup.Checked ;
end;

//------------------------------------------------------------------------------

procedure TfrmPlugin.butRemoveClick(Sender: TObject);
begin
   // save to xml
   XMLConfig.Plugins.Plugin.Remove(plugin.xmlPlugin) ;
   XMLConfig.OwnerDocument.SaveToFile(strConfigFile);
   plugin.xmlPlugin := nil ;

   // remove from list, screen and tree 
   self.parent := nil ;
   //PluginList.Remove(plugin) ;    // remove call plugin destructor (wich free frmPlugin)
   frmDebugOptions.VSTOptions.DeleteNode(node);

   frmDebugOptions.VSTOptions.Expanded [frmDebugOptions.VstPlugNode] := true ;
   frmDebugOptions.VSTOptionsChange (frmDebugOptions.VSTOptions, frmDebugOptions.VstPlugNode) ;

   //Application.MessageBox ('You must restart TraceTool if you want to load another plugin with the same name ','Plugin', MB_OK);

end;

end.
