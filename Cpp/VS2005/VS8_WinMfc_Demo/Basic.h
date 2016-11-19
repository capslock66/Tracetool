#pragma once

#include "Resource.h"

// Basic dialog

class Basic : public CPropertyPage
{
	DECLARE_DYNAMIC(Basic)

public:
	Basic();
	virtual ~Basic();

// Dialog Data
	enum { IDD = IDD_BASIC };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
   afx_msg void OnBnClickedIndent();
   afx_msg void OnBnClickedSample();
   afx_msg void OnBnClickedMainsavetext();
   afx_msg void OnBnClickedMainsavetoxml();
   afx_msg void OnBnClickedMainloadxml();
   afx_msg void OnBnClickedShowviewer();
   afx_msg void OnBnClickedMainclear();
   afx_msg void OnBnClickedLocalfile();
   afx_msg void OnBnClickedWindow();
   afx_msg void OnBnClickedSocket();
   afx_msg void OnBnClickedNone();
};
