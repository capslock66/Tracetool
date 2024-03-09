// ***************************************************************
//  madIWSupport.h            version:  1.0   ·  date: 2007-05-07
//  -------------------------------------------------------------
//  exception trapping support for IntraWeb 5 - 9
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2007 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2007-05-07 initial version

//---------------------------------------------------------------------------
#ifndef madIWSupportH
#define madIWSupportH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompButton.hpp"
#include "IWCompLabel.hpp"
#include "IWCompMemo.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "madExcept.hpp"
//---------------------------------------------------------------------------
class TIWExceptForm: public TIWAppForm
{
__published:
  TIWLabel *MsgLabel;
  TIWButton *ContinueBtn;
  TIWButton *ShowBtn;
  TIWMemo *BugReportMemo;
  void __fastcall ContinueBtnClick(TObject *Sender);
  void __fastcall ShowBtnClick(TObject *Sender);
public:
  String BugRepText;
  __fastcall TIWExceptForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern void __fastcall ShowExceptionForm(TIWApplication *application,
  String exceptClass, String exceptMsg, String bugReport, IMESettings *settings);
extern bool __fastcall TIWServerControllerBaseDoExceptionCallbackOld(
  TIWServerControllerBase *Self, TIWApplication *AApplication,
  Exception *AException);
extern bool __fastcall TIWServerControllerBaseDoExceptionCallbackNew(
  TIWServerControllerBase *Self, TIWApplication *AApplication,
  Exception *AException, bool *handled);
//---------------------------------------------------------------------------

// call this during project initialization
extern void InitMadIWSupport();

//---------------------------------------------------------------------------
#endif
