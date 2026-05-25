// ***************************************************************
//  madIWSupport.cpp          version:  1.0   ·  date: 2007-05-07
//  -------------------------------------------------------------
//  exception trapping support for IntraWeb 5 - 9
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2007 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2007-05-07 initial version

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "madIWSupport.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompButton"
#pragma link "IWCompLabel"
#pragma link "IWCompMemo"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TIWExceptForm::TIWExceptForm(TComponent* Owner) : TIWAppForm(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TIWExceptForm::ContinueBtnClick(TObject *Sender)
{
    Release();    
}
//---------------------------------------------------------------------------
void __fastcall TIWExceptForm::ShowBtnClick(TObject *Sender)
{
  ShowBtn->Visible = false;
  BugReportMemo->Lines->Text = BugRepText;
  BugReportMemo->Visible = true;
}
//---------------------------------------------------------------------------
void __fastcall ShowExceptionForm(TIWApplication *application, String exceptClass, String exceptMsg, String bugReport, IMESettings *settings)
{
  TIWExceptForm *form_;
  TSize p1;
  Graphics::TBitmap *bmp;
  String s1;
  int i1;

  form_ = new TIWExceptForm(application);
  s1 = settings->ExceptMsg;
  ExpandVars(settings->Module(), s1, exceptClass, exceptMsg, bugReport);
  form_->MsgLabel->Caption = s1;
  form_->MsgLabel->AutoSize = true;

  bmp = new Graphics::TBitmap();
  bmp->Canvas->Font->Name = "MS Sans Serif";
  bmp->Canvas->Font->Size = 10;

  form_->ContinueBtn->Caption = settings->ContinueBtnCaption;
  p1 = bmp->Canvas->TextExtent(form_->ContinueBtn->Caption);
  form_->ContinueBtn->SetBounds(form_->ContinueBtn->Left, form_->MsgLabel->Top + form_->MsgLabel->Height + 16, p1.cx + 28, p1.cy + 10);
  form_->ShowBtn->Caption = settings->ShowBtnCaption;
  p1 = bmp->Canvas->TextExtent(form_->ShowBtn->Caption);
  form_->ShowBtn->SetBounds(form_->ContinueBtn->Left + form_->ContinueBtn->Width + 14, form_->ContinueBtn->Top, p1.cx + 28, form_->ContinueBtn->Height);
  form_->ShowBtn->Visible = ( (!settings->AutoShowBugReport) && settings->ShowBtnVisible );

  delete bmp;
  bmp = NULL;

  if (settings->ShowBtnVisible || settings->AutoShowBugReport)
  {
    i1 = form_->ContinueBtn->Top + form_->ContinueBtn->Height + 16 - form_->BugReportMemo->Top;
    form_->BugReportMemo->Top = form_->BugReportMemo->Top + i1;
    form_->BugReportMemo->Height = form_->BugReportMemo->Height - i1;
    if (settings->AutoShowBugReport)
      form_->BugReportMemo->Lines->Text = bugReport;
    else
      form_->BugRepText = bugReport;
    if ( GetPropInfo(form_->BugReportMemo, "ReadOnly") != NULL )
      SetEnumProp(form_->BugReportMemo, "ReadOnly", "true");
    else
      form_->BugReportMemo->Editable = false;
  }
  form_->BugReportMemo->Visible = settings->AutoShowBugReport;
  form_->Show();
}
//---------------------------------------------------------------------------
bool __fastcall TIWServerControllerBaseDoExceptionCallbackOld(TIWServerControllerBase *Self, TIWApplication *AApplication, Exception *AException)
{
  String exceptClass, exceptMsg, bugReport;
  bugReport = "";
  if (AException != NULL)
  {
    exceptClass = AException->ClassName;
    exceptMsg = AException->Message;
  }
  else
  {
    exceptClass = "Unknown";
    exceptMsg = "Unknown.";
  }
  HandleException(etNormal, AException, NULL, true, Esp(), Ebp(), NULL, esIntraweb, AApplication, 0, &bugReport);
  if (bugReport != "")
    ShowExceptionForm(AApplication, exceptClass, exceptMsg, bugReport, MESettings());
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TIWServerControllerBaseDoExceptionCallbackNew(TIWServerControllerBase *Self, TIWApplication *AApplication, Exception *AException, bool *handled)
{
  String exceptClass, exceptMsg, bugReport;
  bugReport = "";
  if (AException != NULL)
  {
    exceptClass = AException->ClassName;
    exceptMsg = AException->Message;
  }
  else
  {
    exceptClass = "Unknown";
    exceptMsg = "Unknown.";
  }
  HandleException(etNormal, AException, NULL, true, Esp(), Ebp(), NULL, esIntraweb, AApplication, 0, &bugReport);
  if (bugReport != "")
    ShowExceptionForm(AApplication, exceptClass, exceptMsg, bugReport, MESettings());
  *handled = true;
  return true;
}
//---------------------------------------------------------------------------
void InitMadIWSupport()
{
  AmHttpServer = true;

  TFunctionInfo fi;
  bool (_fastcall TIWServerControllerBase::*mthd)(TIWApplication *,Exception *,bool &) = TIWServerControllerBase::DoException;
  LPVOID prc;
  memcpy(&prc, &mthd, sizeof(prc));
  int iwver = BcbHelper_GetIntraWebVersion(prc);
  if (iwver == 2)
    // IntraWeb 7.2.33
    PatchJmp(prc, &TIWServerControllerBaseDoExceptionCallbackNew);
  else
    if (iwver == 1) 
      // older IntraWeb versions
      PatchJmp(fi.EntryPoint, &TIWServerControllerBaseDoExceptionCallbackOld);
}
