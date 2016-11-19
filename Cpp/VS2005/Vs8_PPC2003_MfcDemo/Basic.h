#pragma once


// Basic dialog

class Basic : public CPropertyPage
{
	DECLARE_DYNAMIC(Basic)

public:
	Basic();
	virtual ~Basic();
    void CheckHost (wchar_t * Host) ;

// Dialog Data
	enum { IDD = IDD_BASIC };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

public:
    afx_msg void OnBnClickedSample();
public:
    afx_msg void OnBnClickedIndent();
public:
    afx_msg void OnBnClickedMainsavetext();
public:
    afx_msg void OnBnClickedMainsavetoxml();
public:
    afx_msg void OnBnClickedMainloadxml();
public:
    afx_msg void OnBnClickedShowviewer();
public:
    afx_msg void OnBnClickedMainclear();
public:
    afx_msg void OnBnClickedPartner();
public:
    afx_msg void OnBnClickedHosts();
};
