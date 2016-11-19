#pragma once

#include "Basic.h"
#include "MultiPages.h"
#include "NodeOperations.h"
#include "Watches.h"


// Sheet

class Sheet : public CPropertySheet
{
	DECLARE_DYNAMIC(Sheet)

public:
	Sheet(UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	Sheet(LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	virtual ~Sheet();

	virtual BOOL OnInitDialog();
	void AddControlPages(void);

	Basic           m_Basic;
	MultiPages      m_MultiPages;
	NodeOperations  m_NodeOperations;
    Watches         m_Watches ;

	HICON m_hIcon;

protected:
	DECLARE_MESSAGE_MAP()
};


