// ***************************************************************
//  madIWSupport.pas          version: 1.1.1  ·  date: 2018-05-29
//  -------------------------------------------------------------
//  exception trapping support for IntraWeb 5 - 10
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2018 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2018-05-29 1.1.1 added support for official IW exception callback
// 2012-04-05 1.1.0 added x64 and unicode support
// 2009-07-21 1.0e  added support for Delphi/BCB 2009
// 2006-09-12 1.0d  special handling when AAplication is not initialized yet
// 2006-01-05 1.0c  Memo visibility was not always correct
// 2005-06-04 1.0b  (1) necessary changes for madExcept 3.0
//                  (2) bug report is only transported to client when needed
//                  (3) IW 7.2.33 has changed parameters of some internal methods
// 2004-05-19 1.0a  (1) "HandleException" parameters changed
//                  (2) ServerController removed from uses clause
// 2004-03-08 1.0   initial version

unit madIWSupport;

{$I mad.inc}

{$define useIWExceptionCallback}

interface

uses IWAppForm, IWCompLabel, IWCompButton, IWCompMemo, madTypes;

// ***************************************************************

type
  TIWExceptForm = class(TIWAppForm)
    MsgLabel      : TIWLabel;
    ContinueBtn   : TIWButton;
    ShowBtn       : TIWButton;
    BugReportMemo : TIWMemo;
    procedure ContinueBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
  private
    BugRepText : UnicodeString;
  end;

// ***************************************************************

implementation

{$R *.dfm}

uses Windows, SysUtils, Graphics, TypInfo, IWServerControllerBase, 
     IWApplication, madExcept, madDisAsm, madStrings;

// ***************************************************************

procedure TIWExceptForm.ContinueBtnClick(Sender: TObject);
begin
  Release;
end;

procedure TIWExceptForm.ShowBtnClick(Sender: TObject);
begin
  ShowBtn.Visible := false;
  BugReportMemo.Lines.Text := BugRepText;
  BugReportMemo.Visible := true;
end;

procedure ShowExceptionForm(application: TIWApplication; const exceptClass, exceptMsg, bugReport: UnicodeString; const settings: IMESettings);
var form_ : TIWExceptForm;
    p1    : TSize;
    bmp   : TBitmap;
    s1    : UnicodeString;
    i1    : integer;
begin
  form_ := TIWExceptForm.Create(application);
  with form_ do begin
    with MsgLabel do begin
      s1 := settings.ExceptMsg;
      ExpandVars(settings.Module, s1, exceptClass, exceptMsg, bugReport, '');
      Caption := s1;
      Width := 1000;
      AutoSize := true;
    end;
    bmp := TBitmap.Create;
    bmp.Canvas.Font.Name := 'MS Sans Serif';
    bmp.Canvas.Font.Size := 10;
    with ContinueBtn do begin
      Caption := settings.ContinueBtnCaption;
      p1 := bmp.Canvas.TextExtent(Caption);
      SetBounds(Left, MsgLabel.Top + MsgLabel.Height + 16, p1.cx + 28, p1.cy + 10);
    end;
    with ShowBtn do begin
      Caption := settings.ShowBtnCaption;
      p1 := bmp.Canvas.TextExtent(Caption);
      SetBounds(ContinueBtn.Left + ContinueBtn.Width + 14, ContinueBtn.Top, p1.cx + 28, ContinueBtn.Height);
      Visible := (not settings.AutoShowBugReport) and settings.ShowBtnVisible;
    end;
    bmp.Free;
    if settings.ShowBtnVisible or settings.AutoShowBugReport then
      with BugReportMemo do begin
        i1 := ContinueBtn.Top + ContinueBtn.Height + 16 - Top;
        Top := Top + i1;
        Height := Height - i1;
        if settings.AutoShowBugReport then
             Lines.Text := bugReport
        else BugRepText := bugReport;
        if GetPropInfo(BugReportMemo, 'ReadOnly') <> nil then
          SetEnumProp(BugReportMemo, 'ReadOnly', 'true')
        else
          Editable := false;
      end;
    BugReportMemo.Visible := settings.AutoShowBugReport;
    Show;
  end;
end;

// ***************************************************************

{$ifdef useIWExceptionCallback}

  type
    TIWExceptionHandler = class
      class procedure TIWServerControllerBaseDoExceptionCallback14(AExceptionInfo: TIWExceptionCallbackInfo; out Handled: boolean);
    end;

  class procedure TIWExceptionHandler.TIWServerControllerBaseDoExceptionCallback14(AExceptionInfo: TIWExceptionCallbackInfo; out Handled: boolean);
  var exceptClass, exceptMsg, bugReport : UnicodeString;
      AException : Exception;
      AApplication : TIWApplication;
  begin
    bugReport := '';
    AException := AExceptionInfo.SourceException;
    if AException <> nil then begin
      exceptClass := UnicodeString(AException.ClassName);
      exceptMsg := UnicodeString(AException.Message);
      if (AException is EIWException) and (EIWException(AException).Details <> '') then
        AException.Message := AException.Message + ', Details: ' + EIWException(AException).Details;
    end else begin
      exceptClass := 'Unknown';
      exceptMsg := 'Unknown.';
    end;
    AApplication := nil;
    if Assigned(GGetWebApplicationThreadVar) then
      AApplication := GGetWebApplicationThreadVar;
    HandleException(etNormal, AException, nil, true, Esp, Ebp, nil, esIntraweb, AApplication, 0, @bugReport);
    if (bugReport <> '') and (AApplication <> nil) then
      ShowExceptionForm(AApplication, exceptClass, exceptMsg, bugReport, MESettings);
    handled := AApplication <> nil;
  end;

  procedure Init;
  begin
    TIWServerControllerBase.RegisterExceptionCallback(TIWExceptionHandler.TIWServerControllerBaseDoExceptionCallback14);
  end;

{$else}

  function TIWServerControllerBaseDoExceptionCallbackOld(Self: TIWServerControllerBase; AApplication: TIWApplication; AException: Exception) : boolean;
  var exceptClass, exceptMsg, bugReport : UnicodeString;
  begin
    bugReport := '';
    if AException <> nil then begin
      exceptClass := UnicodeString(AException.ClassName);
      exceptMsg := UnicodeString(AException.Message);
    end else begin
      exceptClass := 'Unknown';
      exceptMsg := 'Unknown.';
    end;
    HandleException(etNormal, AException, nil, true, Esp, Ebp, nil, esIntraweb, AApplication, 0, @bugReport);
    if bugReport <> '' then
      ShowExceptionForm(AApplication, exceptClass, exceptMsg, bugReport, MESettings);
    result := true;
  end;

  function TIWServerControllerBaseDoExceptionCallbackNew(Self: TIWServerControllerBase; AApplication: TIWApplication; AException: Exception; var handled: boolean) : boolean;
  var exceptClass, exceptMsg, bugReport : UnicodeString;
  begin
    bugReport := '';
    if AException <> nil then begin
      exceptClass := UnicodeString(AException.ClassName);
      exceptMsg := UnicodeString(AException.Message);
    end else begin
      exceptClass := 'Unknown';
      exceptMsg := 'Unknown.';
    end;
    HandleException(etNormal, AException, nil, true, Esp, Ebp, nil, esIntraweb, AApplication, 0, @bugReport);
    if (bugReport <> '') and (AApplication <> nil) then
      ShowExceptionForm(AApplication, exceptClass, exceptMsg, bugReport, MESettings);
    result := true;
  //  in newer IW versions the last parameter is actually a THttpRequest object, instead of a "var handled: boolean"
  //  handled := AApplication <> nil;
  end;

  procedure Init;
  var fi : TFunctionInfo;
  begin
    fi := ParseFunction(@TIWServerControllerBase.DoException);
    if fi.IsValid then
      if dword(pointer(NativeUInt(fi.CodeBegin) + NativeUInt(fi.CodeLen) - 4)^) and $ffffff00 = $0004c200 then begin
        // IntraWeb 7.2.33
        PatchJmp(fi.EntryPoint, @TIWServerControllerBaseDoExceptionCallbackNew);
      end else
        if byte(pointer(NativeUInt(fi.CodeBegin) + NativeUInt(fi.CodeLen) - 1)^) = $c3 then
          // older IntraWeb versions
          PatchJmp(fi.EntryPoint, @TIWServerControllerBaseDoExceptionCallbackOld);
  end;

{$endif}

// ***************************************************************

initialization
  AmHttpServer := true;
  Init;
end.
