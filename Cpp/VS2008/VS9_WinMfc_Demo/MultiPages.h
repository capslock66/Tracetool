#pragma once

#include "..\..\Source\tracetool.h"

// MultiPages dialog

class MultiPages : public CPropertyPage
{
	DECLARE_DYNAMIC(MultiPages)

public:
	MultiPages();
	virtual ~MultiPages();

   WinTrace * MyWinTrace ;
   WinTrace * MulticolWintrace ;

   // Dialog Data
	//enum { IDD = IDD_MULTIPAGES };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
   afx_msg void OnBnClickedNewwin();
   afx_msg void OnBnClickedDisplaywin();
   afx_msg void OnBnClickedSayhello();
   afx_msg void OnBnClickedSavetext();
   afx_msg void OnBnClickedSavetoxml();
   afx_msg void OnBnClickedLoadxml();
   afx_msg void OnBnClickedMulticol();
   afx_msg void OnBnClickedClosewin();
};
