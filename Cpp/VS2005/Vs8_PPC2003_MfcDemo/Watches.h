#pragma once

#include "..\..\Source\tracetool.h"

// Watches dialog

class Watches : public CPropertyPage
{
	DECLARE_DYNAMIC(Watches)

public:
	Watches();   // standard constructor
	virtual ~Watches();

// Dialog Data
	enum { IDD = IDD_WATCHES };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
    afx_msg void OnBnClickedbutwatch();
public:
    afx_msg void OnBnClickedbutclearwatchwindow();
public:
    afx_msg void OnBnClickedbutdisplaywatchwindow();
public:
    afx_msg void OnBnClickedbutcreatewinwatch();
public:
    afx_msg void OnBnClickedbutwinwatchsend();
public:
    afx_msg void OnBnClickedbutwinwatchclear();
public:
    afx_msg void OnBnClickedbutwinwatchdisplay();
};
