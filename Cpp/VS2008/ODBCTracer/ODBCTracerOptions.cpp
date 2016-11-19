/**
 * Copyright (C) 2002-2005
 * W3L GmbH
 * Technologiezentrum Ruhr
 * Universit‰tsstraﬂe 142
 * D-44799 Bochum
 * 
 * Author: Dipl.Ing. Doga Arinir
 * E-Mail: doga.arinir@w3l.de
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author or the company be held liable 
 * for any damages arising from the use of this software. EXPECT BUGS!
 * 
 * You may use this software in compiled form in any way you desire PROVIDING it is
 * not sold for profit without the authors written permission, and providing that this
 * notice and the authors name is included. If the source code in this file is used in 
 * any commercial application then acknowledgement must be made to the author of this file.
 */
#include "stdafx.h"
#include "odbctracer.h"
#include "resource.h"
#include "sql.h"
#include "sqlext.h"

#define TraceFuncOption(s, id) init(#s, id)

void ODBCTraceOptions::init(const char *fn, int fid)
{
	//First fill the functionname 2 functionid hashtable...
	fn2id.insert(CHAR2FuncSwitchMAP::value_type(fn, fid));
	//vector containing all functionnames
	keys.insert(keys.end(), fn);
	//initialize all functions for logging
	setLogFunction(fn, true); 
}

ODBCTraceOptions::ODBCTraceOptions() : fileloggingactivated(false)
{
	TraceFuncOption(TraceSQLDataSources, SQL_API_SQLDATASOURCES);
	TraceFuncOption(TraceSQLAllocHandleStd, SQL_API_SQLALLOCHANDLESTD);
	TraceFuncOption(TraceSQLTables, SQL_API_SQLTABLES);
	TraceFuncOption(TraceSQLColumns, SQL_API_SQLCOLUMNS);
	TraceFuncOption(TraceSQLStatistics, SQL_API_SQLSTATISTICS);
	TraceFuncOption(TraceSQLTablePrivileges, SQL_API_SQLTABLEPRIVILEGES);
	TraceFuncOption(TraceSQLColumnPrivileges, SQL_API_SQLCOLUMNPRIVILEGES);
	TraceFuncOption(TraceSQLSpecialColumns, SQL_API_SQLSPECIALCOLUMNS);
	TraceFuncOption(TraceSQLPrimaryKeys, SQL_API_SQLPRIMARYKEYS);
	TraceFuncOption(TraceSQLForeignKeys, SQL_API_SQLFOREIGNKEYS);
	TraceFuncOption(TraceSQLProcedures, SQL_API_SQLPROCEDURES);
	TraceFuncOption(TraceSQLProcedureColumns, SQL_API_SQLPROCEDURECOLUMNS);
	TraceFuncOption(TraceSQLConnect, SQL_API_SQLCONNECT);
	TraceFuncOption(TraceSQLDriverConnect, SQL_API_SQLDRIVERCONNECT);
	TraceFuncOption(TraceSQLBrowseConnect, SQL_API_SQLBROWSECONNECT);
	TraceFuncOption(TraceSQLDisconnect, SQL_API_SQLDISCONNECT);
	TraceFuncOption(TraceSQLEndTran, SQL_API_SQLENDTRAN);
	TraceFuncOption(TraceSQLTransact, SQL_API_SQLTRANSACT);
	TraceFuncOption(TraceSQLNumResultCols, SQL_API_SQLNUMRESULTCOLS);
	TraceFuncOption(TraceSQLDescribeCol, SQL_API_SQLDESCRIBECOL);
	TraceFuncOption(TraceSQLColAttribute, SQL_API_SQLCOLATTRIBUTE);
	TraceFuncOption(TraceSQLColAttributes, SQL_API_SQLCOLATTRIBUTES);
	TraceFuncOption(TraceSQLBindCol, SQL_API_SQLBINDCOL);
	TraceFuncOption(TraceSQLFetch, SQL_API_SQLFETCH);
	TraceFuncOption(TraceSQLGetData, SQL_API_SQLGETDATA);
	TraceFuncOption(TraceSQLMoreResults, SQL_API_SQLMORERESULTS);
	TraceFuncOption(TraceSQLRowCount, SQL_API_SQLROWCOUNT);
	TraceFuncOption(TraceSQLExtendedFetch, SQL_API_SQLEXTENDEDFETCH);
	TraceFuncOption(TraceSQLFetchScroll, SQL_API_SQLFETCHSCROLL);
	TraceFuncOption(TraceSQLSetConnectOption, SQL_API_SQLSETCONNECTOPTION);
	TraceFuncOption(TraceSQLGetConnectOption, SQL_API_SQLGETCONNECTOPTION);
	TraceFuncOption(TraceSQLSetStmtOption, SQL_API_SQLSETSTMTOPTION);
	TraceFuncOption(TraceSQLGetStmtOption, SQL_API_SQLGETSTMTOPTION);
	TraceFuncOption(TraceSQLSetEnvAttr, SQL_API_SQLSETENVATTR);
	TraceFuncOption(TraceSQLGetEnvAttr, SQL_API_SQLGETENVATTR);
	TraceFuncOption(TraceSQLSetConnectAttr, SQL_API_SQLSETCONNECTATTR);
	TraceFuncOption(TraceSQLGetConnectAttr, SQL_API_SQLGETCONNECTATTR);
	TraceFuncOption(TraceSQLSetStmtAttr, SQL_API_SQLSETSTMTATTR);
	TraceFuncOption(TraceSQLGetStmtAttr, SQL_API_SQLGETSTMTATTR);
	TraceFuncOption(TraceSQLPrepare, SQL_API_SQLPREPARE);
	TraceFuncOption(TraceSQLBindParameter, SQL_API_SQLBINDPARAMETER);
	TraceFuncOption(TraceSQLDescribeParam, SQL_API_SQLDESCRIBEPARAM);
	TraceFuncOption(TraceSQLParamOptions, SQL_API_SQLPARAMOPTIONS);
	TraceFuncOption(TraceSQLNumParams, SQL_API_SQLNUMPARAMS);
	TraceFuncOption(TraceSQLSetScrollOptions, SQL_API_SQLSETSCROLLOPTIONS);
	TraceFuncOption(TraceSQLExecute, SQL_API_SQLEXECUTE);
	TraceFuncOption(TraceSQLExecDirect, SQL_API_SQLEXECDIRECT);
	TraceFuncOption(TraceSQLNativeSql, SQL_API_SQLNATIVESQL);
	TraceFuncOption(TraceSQLParamData, SQL_API_SQLPARAMDATA);
	TraceFuncOption(TraceSQLPutData, SQL_API_SQLPUTDATA);
	TraceFuncOption(TraceSQLCancel, SQL_API_SQLCANCEL);
	TraceFuncOption(TraceSQLAllocEnv, SQL_API_SQLALLOCENV);
	TraceFuncOption(TraceSQLFreeEnv, SQL_API_SQLFREEENV);
	TraceFuncOption(TraceSQLAllocConnect, SQL_API_SQLALLOCCONNECT);
	TraceFuncOption(TraceSQLFreeConnect, SQL_API_SQLFREECONNECT);
	TraceFuncOption(TraceSQLAllocStmt, SQL_API_SQLALLOCSTMT);
	TraceFuncOption(TraceSQLFreeStmt, SQL_API_SQLFREESTMT);
	TraceFuncOption(TraceSQLAllocHandle, SQL_API_SQLALLOCHANDLE);
	TraceFuncOption(TraceSQLFreeHandle, SQL_API_SQLFREEHANDLE);
	TraceFuncOption(TraceSQLSetCursorName, SQL_API_SQLSETCURSORNAME);
	TraceFuncOption(TraceSQLGetCursorName, SQL_API_SQLGETCURSORNAME);
	TraceFuncOption(TraceSQLSetPos, SQL_API_SQLSETPOS);
	TraceFuncOption(TraceSQLBulkOperations, SQL_API_SQLBULKOPERATIONS);
	TraceFuncOption(TraceSQLCloseCursor, SQL_API_SQLCLOSECURSOR);
	TraceFuncOption(TraceSQLGetInfo, SQL_API_SQLGETINFO);
	TraceFuncOption(TraceSQLGetTypeInfo, SQL_API_SQLGETTYPEINFO);
	TraceFuncOption(TraceSQLGetFunctions, SQL_API_SQLGETFUNCTIONS);
	TraceFuncOption(TraceSQLGetDiagField, SQL_API_SQLGETDIAGFIELD);
	TraceFuncOption(TraceSQLGetDiagRec, SQL_API_SQLGETDIAGREC);
	TraceFuncOption(TraceSQLError, SQL_API_SQLERROR);
	TraceFuncOption(TraceSQLCopyDesc, SQL_API_SQLCOPYDESC);

}

bool ODBCTraceOptions::logFunction(int functionid)
{
	INT2FuncSwitchMAP::iterator it = options.find(functionid);
	if (it != options.end())
		return (*it).second;

	return false;
}

bool ODBCTraceOptions::logFunction(const char *function)
{
	return logFunction(ODBCTraceOptions::FN2ID(function));
}

CHAR2FuncSwitchMAP ODBCTraceOptions::fn2id;
void ODBCTraceOptions::setLogFunction(const char *function, bool log)
{
	std::pair<INT2FuncSwitchMAP::iterator, bool> ret = options.insert(INT2FuncSwitchMAP::value_type(ODBCTraceOptions::FN2ID(function), log));
	if (!ret.second)
		(*ret.first).second = log;

}

int ODBCTraceOptions::FN2ID(const char *n)
{
	CHAR2FuncSwitchMAP::iterator it = fn2id.find(n);
	if (it != fn2id.end())
		return (*it).second;
	return -1;
}

int ODBCTraceOptionDlg::doModal(HWND hwnd)
{
	this->hwndParent = hwnd;
	return DialogBoxParam(ODBCTraceOptionDlg::getHINSTANCE()	,
							MAKEINTRESOURCE(IDD_DIALOG_OPTIONS), 
							hwnd,
							(DLGPROC) ODBCTraceOptionDlg::ConfigDlgProc,
							(LONG)(LPSTR)this);
}

#define GETBUTTONID(i) (10000 + i)

void ODBCTraceOptionDlg::createCheckBoxes()
{
	int x = 0;
	int y = 0;
	for (unsigned i = 0; i < new_options.keys.size(); i++)
	{
		y = 20 +  20 * (i % 25);
		x = 10 + 220 * (i / 25);
		::CreateWindow( "BUTTON", new_options.keys[i].c_str(), WS_CHILD | WS_VISIBLE | BS_CHECKBOX, x, y, 210, 20, wnd, (HMENU)GETBUTTONID(i), getHINSTANCE(), NULL);
		::CheckDlgButton(wnd, GETBUTTONID(i), new_options.logFunction(new_options.keys[i].c_str()) ? BST_CHECKED : BST_UNCHECKED);
	}
}

int CALLBACK ODBCTraceOptionDlg::ConfigDlgProc(HWND hdlg,WORD wMsg,WPARAM wParam,LPARAM lParam)
{
	switch (wMsg) 
	{
		case WM_INITDIALOG:
		{
			ODBCTraceOptionDlg *config = (ODBCTraceOptionDlg*)lParam;
			SetWindowLong(hdlg, DWL_USER, lParam);
			config->wnd = hdlg;
			config->createCheckBoxes();

			::SetWindowText(GetDlgItem(hdlg, IDC_STATIC_LOGFILE), config->new_options.logfile.c_str());
			::CheckDlgButton(hdlg, IDC_CHECK_LOGFILE, config->new_options.fileloggingactivated ? BST_CHECKED : BST_UNCHECKED);

			//hyperlink
			config->hyperlink.create(IDC_STATIC_W3L_DE, hdlg, ODBCTraceOptionDlg::getHINSTANCE());
			return TRUE;
		}
		case WM_COMMAND:
			if (GET_WM_COMMAND_CMD(wParam, lParam) == BN_CLICKED)
			{
				DWORD id = GET_WM_COMMAND_ID(wParam, lParam);
				if (id >= GETBUTTONID(0))
				{
					char buffer[256];
					::GetWindowText(GetDlgItem(hdlg, id), buffer, sizeof(buffer));

					ODBCTraceOptionDlg *config = (ODBCTraceOptionDlg*)GetWindowLong(hdlg, DWL_USER);
					bool state = !config->new_options.logFunction(buffer);
					config->new_options.setLogFunction(buffer, state);

					::CheckDlgButton(hdlg, id, state ? BST_CHECKED : BST_UNCHECKED);
				}
				else if (id == IDC_CHECK_LOGFILE)
				{
					ODBCTraceOptionDlg *config = (ODBCTraceOptionDlg*)GetWindowLong(hdlg, DWL_USER);
					bool state = !config->new_options.fileloggingactivated;
					config->new_options.fileloggingactivated = state;
					::CheckDlgButton(hdlg, id, state ? BST_CHECKED : BST_UNCHECKED);				
				}
			}
			switch (GET_WM_COMMAND_ID(wParam, lParam)) 
			{
			    case IDC_EDIT_DSN:
					if (GET_WM_COMMAND_CMD(wParam, lParam) == EN_CHANGE)
					{
						char  szItem[1024];    
						EnableWindow(GetDlgItem(hdlg, IDOK), GetDlgItemText(hdlg, IDC_EDIT_DSN, szItem, sizeof(szItem)));
						return TRUE;
					}
				break;

				case IDOK:
				case IDCANCEL:
				  EndDialog(hdlg, wParam);
				  return TRUE;
			}
		break;
  }
  return FALSE;
}


ODBCTraceOptionDlg::ODBCTraceOptionDlg(ODBCTraceOptions *options) : old_options(options) , new_options(*options), maxcheckbuttonid(0)
{
	maxcheckbuttonid = GETBUTTONID(new_options.keys.size());
}

void ODBCTraceOptionDlg::commit()
{
	*old_options = new_options;
}