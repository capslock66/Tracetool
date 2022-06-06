{
  Author : Thierry Parent
  Part of that project is based on the GDebug program, see http://www.gexperts.org
  The TreeViews are virtual tree view 4.0.17 components, see http://www.delphi-gems.com
  Indy components must be installed.
  See License.txt for license information 
}

program TraceTool;


uses
  fastmm5,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  windows,
  Forms,
  unt_TraceWin in 'unt_TraceWin.pas' {Frm_Trace},
  Unt_receiver in 'Unt_receiver.pas' {FormReceiver},
  DebugOptions in 'DebugOptions.pas' {frmDebugOptions},
  unt_about in 'unt_about.pas' {FrmAbout},
  DirMon in 'DirMon.pas',
  unt_Tail in 'unt_Tail.pas' {FrmTail},
  Unt_linkedList in 'Unt_linkedList.pas',
  unt_ODS in 'unt_ODS.pas' {Frm_ODS},
  unt_base in 'unt_base.pas' {FrmBase},
  Unt_Tool in 'Unt_Tool.pas' {Frm_Tool},
  unt_utility in 'unt_utility.pas',
  unt_selectEvent in 'unt_selectEvent.pas' {FrmSelectEvent},
  unt_eventLog in 'unt_eventLog.pas' {FrmEventLog},
  Eventlog in 'Eventlog.pas',
  unt_saveDlg in 'unt_saveDlg.pas' {FrmSave},
  unt_SelectTail in 'unt_SelectTail.pas' {FrmSelectTail},
  unt_traceWinProperty in 'unt_traceWinProperty.pas' {FrmTraceWinProp},
  unt_plugin in 'unt_plugin.pas',
  unt_FrmPlugin in 'unt_FrmPlugin.pas' {frmPlugin: TFrame},
  Unt_TailProgress in 'Unt_TailProgress.pas' {FrmTailProgress},
  unt_PageContainer in 'unt_PageContainer.pas' {FrmPageContainer},
  unt_search in 'unt_search.pas' {FrmSearch},
  VstSort in 'VstSort.pas',
  unt_Parse in 'unt_Parse.pas',
  unt_filter in 'unt_filter.pas' {FrmFilter},
  unt_editor in 'unt_editor.pas',
  unt_Details_base in 'unt_Details_base.pas' {frame_BaseDetails: TFrame},
  unt_Details_bitmap in 'unt_Details_bitmap.pas' {frame_BitmapDetails: TFrame},
  unt_Details_xml in 'unt_Details_xml.pas' {Frame_XML: TFrame},
  unt_Details_Classic in 'unt_Details_Classic.pas' {frame_Classic: TFrame},
  unt_Details_table in 'unt_Details_table.pas' {frame_Table: TFrame},
  untPrintPreview in 'untPrintPreview.pas' {FrmPrintPreview},
  Application6 in 'Application6.pas',
  Preview in 'Preview\Preview.pas',
  //FileViewer in 'dependencyviewer\FileViewer.pas',
  Config in 'Config.pas',
  unt_TraceConfig in 'unt_TraceConfig.pas';

{$R *.res}

begin
   //ResetLowTrace() ;  // delete internal lowtrace file

  //ReportMemoryLeaksOnShutdown := false;  // when true, force include mmetUnexpectedMemoryLeakSummary in FastMM_MessageBoxEvents in

  FastMM_SetEventLogFilename('c:\temp\tracetool.log');
  FastMM_DeleteEventLogFile();
  FastMM_LogToFileEvents := FastMM_LogToFileEvents + [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
  FastMM_MessageBoxEvents := [];

   //LowTrace ('traceTool started') ;
   SetLastError(NO_ERROR);
   CreateMutex (nil,false, 'TraceTool') ;
   if getlasterror = ERROR_ALREADY_EXISTS then begin
      manyInstances() ;
      exit ;
   end ;

   Application.Initialize;
   Application.Title := 'TraceTool';
   Application.CreateForm(TFrm_Tool, Frm_Tool);
   Application.CreateForm(TfrmDebugOptions, frmDebugOptions);
   Application.CreateForm(TFormReceiver, FormReceiver);
   Application.CreateForm(TFrmAbout, FrmAbout);
   Application.CreateForm(TFrmSelectEvent, FrmSelectEvent);
   Application.CreateForm(TFrmSave, FrmSave);
   Application.CreateForm(TFrmTraceWinProp, FrmTraceWinProp);
   Application.CreateForm(TFrmSearch, FrmSearch);
   Application.CreateForm(TFrmPrintPreview, FrmPrintPreview);

   if Frm_Tool.InitError <> '' then
      exit ;
   //LowTrace ('Application.Run') ;
   Application.Run;
end.
