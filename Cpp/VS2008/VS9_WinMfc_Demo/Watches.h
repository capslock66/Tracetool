#pragma once

#include "resource.h"		// main symbols
#include "..\..\Source\tracetool.h"

// Watches dialog

class Watches : public CPropertyPage
{
	DECLARE_DYNAMIC(Watches)

public:
	Watches();
	virtual ~Watches();

   WinWatch * MyWinWatch ;

   // Dialog Data
	enum { IDD = IDD_WATCHES };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
   afx_msg void OnBnClickedbutwatch();
   afx_msg void OnBnClickedbutclearwatchwindow();
   afx_msg void OnBnClickedbutcreatewinwatch();
   afx_msg void OnBnClickedbutwinwatchsend();
   afx_msg void OnBnClickedbutwinwatchclear();
   afx_msg void OnBnClickedbutwinwatchdisplay();
   afx_msg void OnBnClickedbutdisplaywatchwindow();
};
