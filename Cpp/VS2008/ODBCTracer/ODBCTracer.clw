; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
LastClass=CODBCTracerDlg
LastTemplate=CDialog
NewFileInclude1=#include "stdafx.h"
NewFileInclude2=#include "odbctracer.h"
LastPage=0

ClassCount=1

ResourceCount=4
Class1=CODBCTracerDlg
Resource1=IDD_DIALOG_OPTIONS
Resource2=IDD_ODBCTRACER_DIALOG
Resource3=IDR_MENU1
Resource4=IDR_POPUP_MENU

[DLG:IDD_ODBCTRACER_DIALOG]
Type=1
Class=CODBCTracerDlg
ControlCount=2
Control1=IDC_EDIT_OUT,edit,1352734916
Control2=IDC_BUT_CLEAR,button,1342242816

[CLS:CODBCTracerDlg]
Type=0
HeaderFile=ODBCTracerDlg.h
ImplementationFile=ODBCTracerDlg.cpp
BaseClass=CDialog
Filter=D

[MNU:IDR_MENU1]
Type=1
Class=?
Command1=ID_POPUP_SHOWODBCTRACERDIALOG
Command2=ID_POPUP_OPTIONS
CommandCount=2

[DLG:IDD_DIALOG_OPTIONS]
Type=1
Class=?
ControlCount=11
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_STATIC,button,1342177287
Control4=IDC_STATIC,button,1342177287
Control5=IDC_STATIC_LOGFILE,static,1342312448
Control6=IDC_CHECK_LOGFILE,button,1342242819
Control7=IDC_STATIC,static,1342308352
Control8=IDC_STATIC,static,1342177294
Control9=IDC_STATIC,button,1342177287
Control10=IDC_STATIC_W3L_DE,static,1342308352
Control11=IDC_STATIC,static,1342308364

[MNU:IDR_POPUP_MENU]
Type=1
Class=?
Command1=ID_POPUP_SHOWODBCTRACERDIALOG
Command2=ID_POPUP_OPTIONS
CommandCount=2

