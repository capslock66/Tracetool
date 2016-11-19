/**
 * Copyright (C) 2002-2005
 * W3L GmbH
 * Technologiezentrum Ruhr
 * Universitätsstraße 142
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

// ODBCTracer.cpp : Definiert den Einsprungpunkt für die DLL-Anwendung.
//

#include "stdafx.h"

#include "sql.h"
#include "sqlext.h"
#include "ODBCTracer.h"
#include "resource.h"
#include <assert.h>

#define	WM_ICON_NOTIFY WM_APP+10

ODBCTraceOptions* ODBCTraceOptions::unique_instance;
ODBCTraceOptions* ODBCTraceOptions::getUniqueInstance() 
{
	if (unique_instance == NULL)
		unique_instance = new ODBCTraceOptions();
	return unique_instance;
}

ODBCTraceStack stack;
HINSTANCE instance = NULL;
HINSTANCE ODBCTraceOptionDlg::getHINSTANCE() {return instance;}


//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------


int CALLBACK DlgProc(HWND hwnd,WORD message,WPARAM wParam,LPARAM lParam)
{
	switch(message)
	{
	case WM_TIMER:
		ODBCTraceDialog::getUniqueInstance()->flush();
		break;
	case WM_DESTROY:
		ODBCTraceDialog::getUniqueInstance()->stopFlush();
		break;
	case WM_ICON_NOTIFY:
		return ODBCTraceDialog::getUniqueInstance()->getSystemTray().OnTrayNotification(wParam, lParam);
	case WM_COMMAND:
		switch (GET_WM_COMMAND_ID(wParam, lParam)) 
		{ 
		case ID_POPUP_SHOWODBCTRACERDIALOG:
			::ShowWindow(hwnd, SW_SHOW);
			break;
		case ID_POPUP_OPTIONS:
		{
			ODBCTraceOptionDlg optionsdlg(ODBCTraceOptions::getUniqueInstance());
			if (optionsdlg.doModal(hwnd) == IDOK)
				optionsdlg.commit();
			break;
		}
		case IDC_BUT_CLEAR:
			::SetWindowText(::GetDlgItem(hwnd, IDC_EDIT_OUT), "");
			break;
		}
		break; 
	case WM_SIZE:
	{ 
		RECT rect; memset(&rect, 0, sizeof(rect));
		GetClientRect(hwnd, &rect);
		::MoveWindow(::GetDlgItem(hwnd, IDC_EDIT_OUT), 0, 0, rect.right, rect.bottom - 16, TRUE);
		::MoveWindow(::GetDlgItem(hwnd, IDC_BUT_CLEAR), 0, rect.bottom - 14, rect.right, 14, TRUE);
		break;
	}
	case WM_CLOSE:
		::ShowWindow(hwnd, SW_HIDE);
		break;
	}

	return 0;
}

//------------------------------------------------------------------------------------------

//	
BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	instance = (HINSTANCE)hModule;
   ODBCTraceDialog* dialog = ODBCTraceDialog::getUniqueInstance() ;
   WinTrace * Trace = dialog->ODBCWinTrace ;


   string reason = ""; 

	switch (ul_reason_for_call) 
   {
   case DLL_PROCESS_ATTACH:  // 1
      reason = "DLL_PROCESS_ATTACH" ;
      break;
   case DLL_THREAD_ATTACH:   // 2
      reason = "DLL_THREAD_ATTACH";
      break;
   case DLL_PROCESS_DETACH:  // 0
      reason = "DLL_PROCESS_DETACH";
      break;
   case DLL_THREAD_DETACH:   // 3
      reason = "DLL_THREAD_DETACH";
      break;
   default:
      break;
   }

   char	szProcName[MAX_PATH];
   _splitpath_s(GetCommandLine(), NULL,  0, NULL, 0, szProcName, MAX_PATH, NULL,0);  // fullpath, drive, drivesize, dir, dirsize, file, filesize, ext, extsize

   DWORD ProcId = GetCurrentProcessId();
   DWORD ThId = GetCurrentThreadId();

   SYSTEMTIME Time;
   char buffer [MAX_PATH] ;
   GetLocalTime(&Time);

   sprintf_s(buffer, MAX_PATH,"%02d:%02d:%02d:%03d %s (%d-%d)\t%s", 
      Time.wHour, Time.wMinute, Time.wSecond, Time.wMilliseconds,
      szProcName, ProcId,ThId,reason.c_str());

   Trace->Debug()->Send (buffer) ; 
   return TRUE; 
}

//------------------------------------------------------------------------------------------

Mutex::Mutex()
{
	InitializeCriticalSection(&CriticalSection); 

}
Mutex::~Mutex()
{
    DeleteCriticalSection(&CriticalSection);
}
void Mutex::enter()
{
    EnterCriticalSection(&CriticalSection); 
}
void Mutex::leave()
{
    LeaveCriticalSection(&CriticalSection);
}
MutexGuard::MutexGuard(Mutex *mutex) : mutex(mutex)
{
	mutex->enter();
}
MutexGuard::~MutexGuard()
{
	mutex->leave();
}
//------------------------------------------------------------------------------------------

ODBCTraceCall::ODBCTraceCall() : arguments_count(0), retcode(0), unicode(false)
{

}
//------------------------------------------------------------------------------------------

void ODBCTraceCall::insertArgument(const char *name, ODBCTracer_ArgumentTypes type, void *value)
{
	arguments[arguments_count].name = name;
	arguments[arguments_count].type = type;
	arguments[arguments_count].value = value;
	arguments_count++;
	assert(arguments_count < MAX_ARGUMENTS);
}

//------------------------------------------------------------------------------------------

ODBCTraceStack::ODBCTraceStack()
{
	for (int i = 0; i < ODBCTRACE_STACKSIZE; i++)
		stack[i] = NULL;
}

int ODBCTraceStack::push(ODBCTraceCall *call)
{
	//We have to take care about concurrent access...
	MutexGuard guard(&lock);
	for (int i = 0; i < ODBCTRACE_STACKSIZE; i++)
		if (stack[i] == NULL)
		{
			stack[i] = call;
			return i;
		}
	return -1;
}
ODBCTraceCall* ODBCTraceStack::pop(int index)
{
	//We have to take care about concurrent access...
	MutexGuard guard(&lock);
	ODBCTraceCall *call = stack[index];
	stack[index] = NULL;
	return call;
}

//------------------------------------------------------------------------------------------

void ODBCTraceDialog::flush()
{
	MutexGuard guard(&lock);

	if (buffer)
	{
		buffer->flush();

		HWND output = ::GetDlgItem(tracer_dialog, IDC_EDIT_OUT);		
		long length = ::SendMessage(output, WM_GETTEXTLENGTH, NULL, NULL);
		::SendMessage(output, EM_SETSEL, LOWORD(length), LOWORD(length));
		::SendMessage(output, EM_REPLACESEL, (WPARAM)0, (LPARAM)buffer->str().c_str());

		delete buffer;
		buffer = NULL;
	}
}
//------------------------------------------------------------------------------------------

void ODBCTraceDialog::appendText(const char *text)
{	
	MutexGuard guard(&lock);

	if (buffer == NULL)
		buffer = new std::ostringstream;
	*buffer << text;
}

//------------------------------------------------------------------------------------------

void ODBCTraceDialog::startFlush()
{
	timerID = ::SetTimer(tracer_dialog, timerID, 1000, NULL);
}
//------------------------------------------------------------------------------------------

void ODBCTraceDialog::stopFlush()
{
    if (timerID)
	    ::KillTimer(tracer_dialog, timerID);
}
//------------------------------------------------------------------------------------------

ODBCTraceDialog::ODBCTraceDialog() : tracer_dialog(NULL), buffer(NULL)
{
	tracer_dialog = ::CreateDialog(instance, MAKEINTRESOURCE(IDD_ODBCTRACER_DIALOG), NULL, (DLGPROC)DlgProc);
	::ShowWindow(tracer_dialog, SW_HIDE);
	system_tray.Create(instance, tracer_dialog, WM_ICON_NOTIFY, "ODBC Tracer", ::LoadIcon(instance, (LPCTSTR)IDI_ICON_TASKBAR), IDR_POPUP_MENU);	

	//Set Textlimitation
	::SendMessage(::GetDlgItem(tracer_dialog, IDC_EDIT_OUT), EM_LIMITTEXT, 0x7ffffffe, 0);

   ODBCWinTrace = new WinTrace("ODBCWinTrace" , "Odbc Traces") ;
   ODBCWinTrace->SetMultiColumn (1) ;  // must be called before calling setColumnsTitle
   ODBCWinTrace->SetColumnsTitle("Time and process \t Functions and params \t Type \t Value");
   ODBCWinTrace->SetColumnsWidth("180 \t 150 \t 200 \t 200");
   ODBCWinTrace->Debug()->Send("OdbTracer start") ;

	startFlush();
}
ODBCTraceDialog::~ODBCTraceDialog()
{
	destroyWindow();
}
//------------------------------------------------------------------------------------------


ODBCTraceDialog* ODBCTraceDialog::unique_instance = NULL;
ODBCTraceDialog* ODBCTraceDialog::getUniqueInstance()
{
	if (unique_instance == NULL) 
		unique_instance = new ODBCTraceDialog();



	return unique_instance;
}
CSystemTray& ODBCTraceDialog::getSystemTray() {return system_tray;}
//------------------------------------------------------------------------------------------

HWND ODBCTraceDialog::getWND()
{
	return tracer_dialog;
}
//------------------------------------------------------------------------------------------
void ODBCTraceDialog::destroyWindow()
{
	if (tracer_dialog != NULL)
	{
		::DestroyWindow(tracer_dialog);
		tracer_dialog = NULL;
	}
}

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

// http://msdn.microsoft.com/en-us/library/ms714562%28VS.85%29.aspx

RETCODE SQL_API TraceSQLTables(SQLHSTMT hstmt, SQLCHAR FAR *CatalogName, SQLSMALLINT NameLength1,
											   SQLCHAR FAR *SchemaName,SQLSMALLINT NameLength2,
											   SQLCHAR FAR *TableName,SQLSMALLINT NameLength3,
											   SQLCHAR FAR *TableType,SQLSMALLINT NameLength4)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("CatalogName", TYP_SQLCHAR_PTR, CatalogName);
	call->insertArgument("NameLength1", TYP_SQLSMALLINT, (void*)NameLength1);
	call->insertArgument("SchemaName", TYP_SQLCHAR_PTR, SchemaName);
	call->insertArgument("NameLength2", TYP_SQLSMALLINT, (void*)NameLength2);
	call->insertArgument("TableName", TYP_SQLCHAR_PTR, TableName);
	call->insertArgument("NameLength3", TYP_SQLSMALLINT, (void*)NameLength3);
	call->insertArgument("TableType", TYP_SQLCHAR_PTR, TableType);
	call->insertArgument("NameLength4", TYP_SQLSMALLINT, (void*)NameLength4);

	call->function_id = SQL_API_SQLTABLES;
	call->function_name = "SQLTables";

   ODBCTraceEnter(call);
   return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLTablesW(SQLHSTMT hstmt, SQLWCHAR FAR *CatalogName, SQLSMALLINT NameLength1,
											   SQLWCHAR FAR *SchemaName,SQLSMALLINT NameLength2,
											   SQLWCHAR FAR *TableName,SQLSMALLINT NameLength3,
											   SQLWCHAR FAR *TableType,SQLSMALLINT NameLength4)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("CatalogName", TYP_SQLWCHAR_PTR, CatalogName);
	call->insertArgument("NameLength1", TYP_SQLSMALLINT, (void*)NameLength1);
	call->insertArgument("SchemaName", TYP_SQLWCHAR_PTR, SchemaName);
	call->insertArgument("NameLength2", TYP_SQLSMALLINT, (void*)NameLength2);
	call->insertArgument("TableName", TYP_SQLWCHAR_PTR, TableName);
	call->insertArgument("NameLength3", TYP_SQLSMALLINT, (void*)NameLength3);
	call->insertArgument("TableType", TYP_SQLWCHAR_PTR, TableType);
	call->insertArgument("NameLength4", TYP_SQLSMALLINT, (void*)NameLength4);

	call->unicode = true;
	call->function_id = SQL_API_SQLTABLES;
	call->function_name = "SQLTablesW";

	ODBCTraceEnter(call);
   return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLColumns(SQLHSTMT hstmt,	SQLCHAR FAR *CatalogName,SQLSMALLINT CatLength,
												SQLCHAR FAR *SchemaName,SQLSMALLINT SchLength,
												SQLCHAR FAR *TableName, SQLSMALLINT TabLength,
												SQLCHAR FAR *ColumnName,SQLSMALLINT ColLength)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("CatalogName", TYP_SQLCHAR_PTR, CatalogName);
	call->insertArgument("CatLength", TYP_SQLSMALLINT, (void*)CatLength);
	call->insertArgument("SchemaName", TYP_SQLCHAR_PTR, SchemaName);
	call->insertArgument("SchLength", TYP_SQLSMALLINT, (void*)SchLength);
	call->insertArgument("TableName", TYP_SQLCHAR_PTR, TableName);
	call->insertArgument("TabLength", TYP_SQLSMALLINT, (void*)TabLength);
	call->insertArgument("ColumnName", TYP_SQLCHAR_PTR, ColumnName);
	call->insertArgument("ColLength", TYP_SQLSMALLINT, (void*)ColLength);

	call->function_name = "SQLColumns";
	call->function_id = SQL_API_SQLCOLUMNS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLStatistics(SQLHSTMT hstmt,SQLCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
												  SQLCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
												  SQLCHAR FAR *szTableName,SQLSMALLINT cbTableName,
												  SQLUSMALLINT fUnique,SQLUSMALLINT fAccuracy)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);
	call->insertArgument("fUnique", TYP_SQLUSMALLINT, (void*)fUnique);
	call->insertArgument("fAccuracy", TYP_SQLUSMALLINT, (void*)fAccuracy);

	call->function_name = "SQLStatistics";
	call->function_id = SQL_API_SQLSTATISTICS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLStatisticsW(SQLHSTMT hstmt,SQLWCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
												  SQLWCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
												  SQLWCHAR FAR *szTableName,SQLSMALLINT cbTableName,
												  SQLUSMALLINT fUnique,SQLUSMALLINT fAccuracy)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLWCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLWCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLWCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);
	call->insertArgument("fUnique", TYP_SQLUSMALLINT, (void*)fUnique);
	call->insertArgument("fAccuracy", TYP_SQLUSMALLINT, (void*)fAccuracy);

	call->unicode = true;
	call->function_name = "SQLStatisticsW";
	call->function_id = SQL_API_SQLSTATISTICS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLTablePrivileges(SQLHSTMT hstmt,	SQLCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
														SQLCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
														SQLCHAR FAR *szTableName,SQLSMALLINT cbTableName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);

	call->function_name = "SQLTablePrivileges";
	call->function_id = SQL_API_SQLTABLEPRIVILEGES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLTablePrivilegesW(SQLHSTMT hstmt,	SQLWCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
														SQLWCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
														SQLWCHAR FAR *szTableName,SQLSMALLINT cbTableName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLWCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLWCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLWCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);

	call->unicode = true;
	call->function_name = "SQLTablePrivilegesW";
	call->function_id = SQL_API_SQLTABLEPRIVILEGES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLColumnPrivileges(SQLHSTMT hstmt,SQLCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
														SQLCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
														SQLCHAR FAR *szTableName,SQLSMALLINT cbTableName,
														SQLCHAR FAR *szColumnName,SQLSMALLINT cbColumnName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);
	call->insertArgument("szColumnName", TYP_SQLCHAR_PTR, szColumnName);
	call->insertArgument("cbColumnName", TYP_SQLSMALLINT, (void*)cbColumnName);

	call->function_name = "SQLColumnPrivileges";
	call->function_id = SQL_API_SQLCOLUMNPRIVILEGES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSpecialColumns(SQLHSTMT hstmt,SQLUSMALLINT fColType,
									   SQLCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
									   SQLCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
									   SQLCHAR FAR *szTableName,SQLSMALLINT cbTableName,
									   SQLUSMALLINT fScope,SQLUSMALLINT fNullable)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fColType", TYP_SQLUSMALLINT, (void*)fColType);
	call->insertArgument("szTableQualifier", TYP_SQLCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);
	call->insertArgument("fScope", TYP_SQLUSMALLINT, (void*)fScope);
	call->insertArgument("fNullable", TYP_SQLUSMALLINT, (void*)fNullable);

	call->function_name = "SQLSpecialColumns";
	call->function_id = SQL_API_SQLSPECIALCOLUMNS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSpecialColumnsW(SQLHSTMT hstmt,SQLUSMALLINT fColType,
									   SQLWCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
									   SQLWCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
									   SQLWCHAR FAR *szTableName,SQLSMALLINT cbTableName,
									   SQLUSMALLINT fScope,SQLUSMALLINT fNullable)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fColType", TYP_SQLUSMALLINT, (void*)fColType);
	call->insertArgument("szTableQualifier", TYP_SQLWCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLWCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLWCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);
	call->insertArgument("fScope", TYP_SQLUSMALLINT, (void*)fScope);
	call->insertArgument("fNullable", TYP_SQLUSMALLINT, (void*)fNullable);

	call->unicode = true;
	call->function_name = "SQLSpecialColumnsW";
	call->function_id = SQL_API_SQLSPECIALCOLUMNS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLPrimaryKeys(SQLHSTMT hstmt,SQLCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
									SQLCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
									SQLCHAR FAR *szTableName,SQLSMALLINT cbTableName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);

	call->function_name = "SQLPrimaryKeys";
	call->function_id = SQL_API_SQLPRIMARYKEYS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLPrimaryKeysW(SQLHSTMT hstmt,SQLWCHAR FAR *szTableQualifier,SQLSMALLINT cbTableQualifier,
									SQLWCHAR FAR *szTableOwner,SQLSMALLINT cbTableOwner,
									SQLWCHAR FAR *szTableName,SQLSMALLINT cbTableName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szTableQualifier", TYP_SQLWCHAR_PTR, szTableQualifier);
	call->insertArgument("cbTableQualifier", TYP_SQLSMALLINT, (void*)cbTableQualifier);
	call->insertArgument("szTableOwner", TYP_SQLWCHAR_PTR, szTableOwner);
	call->insertArgument("cbTableOwner", TYP_SQLSMALLINT, (void*)cbTableOwner);
	call->insertArgument("szTableName", TYP_SQLWCHAR_PTR, szTableName);
	call->insertArgument("cbTableName", TYP_SQLSMALLINT, (void*)cbTableName);

	call->unicode = true;
	call->function_name = "SQLPrimaryKeysW";
	call->function_id = SQL_API_SQLPRIMARYKEYS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLForeignKeys(SQLHSTMT hstmt,	SQLCHAR FAR *szPkTableQualifier,SQLSMALLINT cbPkTableQualifier,
													SQLCHAR FAR *szPkTableOwner,SQLSMALLINT cbPkTableOwner,
													SQLCHAR FAR *szPkTableName,SQLSMALLINT cbPkTableName,
													SQLCHAR FAR *szFkTableQualifier,SQLSMALLINT cbFkTableQualifier,
													SQLCHAR FAR *szFkTableOwner,SQLSMALLINT cbFkTableOwner,
													SQLCHAR FAR *szFkTableName,SQLSMALLINT cbFkTableName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szPkTableQualifier", TYP_SQLCHAR_PTR, szPkTableQualifier);
	call->insertArgument("cbPkTableQualifier", TYP_SQLSMALLINT, (void*)cbPkTableQualifier);
	call->insertArgument("szPkTableOwner", TYP_SQLCHAR_PTR, szPkTableOwner);
	call->insertArgument("cbPkTableOwner", TYP_SQLSMALLINT, (void*)cbPkTableOwner);
	call->insertArgument("szPkTableName", TYP_SQLCHAR_PTR, szPkTableName);
	call->insertArgument("cbPkTableName", TYP_SQLSMALLINT, (void*)cbPkTableName);
	call->insertArgument("szFkTableQualifier", TYP_SQLCHAR_PTR, szFkTableQualifier);
	call->insertArgument("cbFkTableQualifier", TYP_SQLSMALLINT, (void*)cbFkTableQualifier);
	call->insertArgument("szFkTableOwner", TYP_SQLCHAR_PTR, szFkTableOwner);
	call->insertArgument("cbFkTableOwner", TYP_SQLSMALLINT, (void*)cbFkTableOwner);
	call->insertArgument("szFkTableName", TYP_SQLCHAR_PTR, szFkTableName);
	call->insertArgument("cbFkTableName", TYP_SQLSMALLINT, (void*)cbFkTableName);

	call->function_name = "SQLForeignKeys";
	call->function_id = SQL_API_SQLFOREIGNKEYS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLProcedures(SQLHSTMT    hstmt,	SQLCHAR FAR *szProcQualifier,SQLSMALLINT cbProcQualifier,
														SQLCHAR FAR *szProcOwner,SQLSMALLINT cbProcOwner,
														SQLCHAR FAR *szProcName,SQLSMALLINT cbProcName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szProcQualifier", TYP_SQLCHAR_PTR, szProcQualifier);
	call->insertArgument("cbProcQualifier", TYP_SQLSMALLINT, (void*)cbProcQualifier);
	call->insertArgument("szProcOwner", TYP_SQLCHAR_PTR, szProcOwner);
	call->insertArgument("cbProcOwner", TYP_SQLSMALLINT, (void*)cbProcOwner);
	call->insertArgument("szProcName", TYP_SQLCHAR_PTR, szProcName);
	call->insertArgument("cbProcName", TYP_SQLSMALLINT, (void*)cbProcName);

	call->function_name = "SQLProcedures";
	call->function_id = SQL_API_SQLPROCEDURES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLProceduresW(SQLHSTMT    hstmt,	SQLWCHAR FAR *szProcQualifier,SQLSMALLINT cbProcQualifier,
														SQLWCHAR FAR *szProcOwner,SQLSMALLINT cbProcOwner,
														SQLWCHAR FAR *szProcName,SQLSMALLINT cbProcName)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szProcQualifier", TYP_SQLWCHAR_PTR, szProcQualifier);
	call->insertArgument("cbProcQualifier", TYP_SQLSMALLINT, (void*)cbProcQualifier);
	call->insertArgument("szProcOwner", TYP_SQLWCHAR_PTR, szProcOwner);
	call->insertArgument("cbProcOwner", TYP_SQLSMALLINT, (void*)cbProcOwner);
	call->insertArgument("szProcName", TYP_SQLWCHAR_PTR, szProcName);
	call->insertArgument("cbProcName", TYP_SQLSMALLINT, (void*)cbProcName);

	call->unicode = true;
	call->function_name = "SQLProceduresW";
	call->function_id = SQL_API_SQLPROCEDURES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLProcedureColumns(SQLHSTMT hstmt,SQLCHAR FAR*szProcQualifier ,SQLSMALLINT cbProcQualifier ,
														SQLCHAR FAR*szProcOwner ,SQLSMALLINT cbProcOwner ,
														SQLCHAR FAR*szProcName ,SQLSMALLINT cbProcName ,
														SQLCHAR FAR*szColumnName ,SQLSMALLINT cbColumnName )
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szProcQualifier", TYP_SQLCHAR_PTR, szProcQualifier);
	call->insertArgument("cbProcQualifier", TYP_SQLSMALLINT, (void*)cbProcQualifier);
	call->insertArgument("szProcOwner", TYP_SQLCHAR_PTR, szProcOwner);
	call->insertArgument("cbProcOwner", TYP_SQLSMALLINT, (void*)cbProcOwner);
	call->insertArgument("szProcName", TYP_SQLCHAR_PTR, szProcName);
	call->insertArgument("cbProcName", TYP_SQLSMALLINT, (void*)cbProcName);
	call->insertArgument("szColumnName", TYP_SQLCHAR_PTR, szColumnName);
	call->insertArgument("cbColumnName", TYP_SQLSMALLINT, (void*)cbColumnName);

	call->function_name = "SQLProcedureColumns";
	call->function_id = SQL_API_SQLPROCEDURECOLUMNS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLConnect(SQLHDBC hdbc,	SQLCHAR FAR *szDSN,SQLSMALLINT	 cbDSN,
												SQLCHAR FAR *szUID, SQLSMALLINT cbUID,
												SQLCHAR FAR *szAuthStr, SQLSMALLINT cbAuthStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("szDSN", TYP_SQLCHAR_PTR, szDSN);
	call->insertArgument("cbDSN", TYP_SQLSMALLINT, (void*)cbDSN);
	call->insertArgument("szUID", TYP_SQLCHAR_PTR, szUID);
	call->insertArgument("cbUID", TYP_SQLSMALLINT, (void*)cbUID);
	call->insertArgument("szAuthStr", TYP_SQLCHAR_PTR, szAuthStr);
	call->insertArgument("cbAuthStr", TYP_SQLSMALLINT, (void*)cbAuthStr);

	call->function_name = "SQLConnect";
	call->function_id = SQL_API_SQLCONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLDriverConnect(SQLHDBC hdbc,SQLHWND hwnd,
												SQLCHAR FAR *szConnStrIn,SQLSMALLINT cbConnStrIn,
												SQLCHAR FAR *szConnStrOut,SQLSMALLINT cbConnStrOutMax,
												SQLSMALLINT FAR *pcbConnStrOut,
												SQLUSMALLINT fDriverCompletion)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("hwnd", TYP_SQLHWND, hwnd);
	call->insertArgument("szConnStrIn", TYP_SQLCHAR_PTR, szConnStrIn);
	call->insertArgument("cbConnStrIn", TYP_SQLSMALLINT, (void*)cbConnStrIn);
	call->insertArgument("szConnStrOut", TYP_SQLCHAR_PTR, szConnStrOut);
	call->insertArgument("cbConnStrOutMax", TYP_SQLSMALLINT, (void*)cbConnStrOutMax);
	call->insertArgument("pcbConnStrOut", TYP_SQLSMALLINT_PTR, pcbConnStrOut);
	call->insertArgument("fDriverCompletion", TYP_SQLSMALLINT, (void*)fDriverCompletion);

	call->function_name = "SQLDriverConnect";
	call->function_id = SQL_API_SQLDRIVERCONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLDriverConnectW(SQLHDBC hdbc,SQLHWND hwnd,
												SQLWCHAR FAR *szConnStrIn,SQLSMALLINT cbConnStrIn,
												SQLWCHAR FAR *szConnStrOut,SQLSMALLINT cbConnStrOutMax,
												SQLSMALLINT FAR *pcbConnStrOut,
												SQLUSMALLINT fDriverCompletion)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("hwnd", TYP_SQLHWND, hwnd);
	call->insertArgument("szConnStrIn", TYP_SQLWCHAR_PTR, szConnStrIn);
	call->insertArgument("cbConnStrIn", TYP_SQLSMALLINT, (void*)cbConnStrIn);
	call->insertArgument("szConnStrOut", TYP_SQLWCHAR_PTR, szConnStrOut);
	call->insertArgument("cbConnStrOutMax", TYP_SQLSMALLINT, (void*)cbConnStrOutMax);
	call->insertArgument("pcbConnStrOut", TYP_SQLSMALLINT_PTR, pcbConnStrOut);
	call->insertArgument("fDriverCompletion", TYP_SQLSMALLINT, (void*)fDriverCompletion);

	call->unicode = true;
	call->function_name = "SQLDriverConnectW";
	call->function_id = SQL_API_SQLDRIVERCONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLBrowseConnect(SQLHDBC hdbc,	SQLCHAR FAR *szConnStrIn,SQLSMALLINT cbConnStrIn,
													SQLCHAR FAR *szConnStrOut,SQLSMALLINT cbConnStrOutMax,
													SQLSMALLINT FAR *pcbConnStrOut)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("szConnStrIn", TYP_SQLCHAR_PTR, szConnStrIn);
	call->insertArgument("cbConnStrIn", TYP_SQLSMALLINT, (void*)cbConnStrIn);
	call->insertArgument("szConnStrOut", TYP_SQLCHAR_PTR, szConnStrOut);
	call->insertArgument("cbConnStrOutMax", TYP_SQLSMALLINT, (void*)cbConnStrOutMax);
	call->insertArgument("pcbConnStrOut", TYP_SQLSMALLINT_PTR, pcbConnStrOut);

	call->function_name = "SQLBrowseConnect";
	call->function_id = SQL_API_SQLBROWSECONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLBrowseConnectW(SQLHDBC hdbc,SQLWCHAR FAR *szConnStrIn,SQLSMALLINT cbConnStrIn,
													SQLWCHAR FAR *szConnStrOut,SQLSMALLINT cbConnStrOutMax,
													SQLSMALLINT FAR *pcbConnStrOut)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("szConnStrIn", TYP_SQLWCHAR_PTR, szConnStrIn);
	call->insertArgument("cbConnStrIn", TYP_SQLSMALLINT, (void*)cbConnStrIn);
	call->insertArgument("szConnStrOut", TYP_SQLWCHAR_PTR, szConnStrOut);
	call->insertArgument("cbConnStrOutMax", TYP_SQLSMALLINT, (void*)cbConnStrOutMax);
	call->insertArgument("pcbConnStrOut", TYP_SQLSMALLINT_PTR, pcbConnStrOut);

	call->unicode = true;
	call->function_name = "SQLBrowseConnectW";
	call->function_id = SQL_API_SQLBROWSECONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}


RETCODE SQL_API TraceSQLDisconnect(SQLHDBC hdbc)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);

	call->function_name = "SQLDisconnect";
	call->function_id = SQL_API_SQLDISCONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLEndTran(SQLSMALLINT HandleType,SQLHANDLE   Handle,SQLSMALLINT CompletionType)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("Handle", TYP_SQLHANDLE, Handle);
	call->insertArgument("CompletionType", TYP_SQLSMALLINT, (void*)CompletionType);

	call->function_name = "SQLEndTran";
	call->function_id = SQL_API_SQLENDTRAN;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLTransact(SQLHENV henv,SQLHDBC hdbc,SQLUSMALLINT fType)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);
	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fType", TYP_SQLUSMALLINT, (void*)fType);

	call->function_name = "SQLTransact";
	call->function_id = SQL_API_SQLTRANSACT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLNumResultCols(SQLHSTMT  hstmt, SQLSMALLINT FAR *pccol)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("pccol", TYP_SQLSMALLINT_PTR, pccol);

	call->function_name = "SQLNumResultCols";
	call->function_id = SQL_API_SQLNUMRESULTCOLS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLDescribeCol(SQLHSTMT hstmt, SQLUSMALLINT ColumnNumber,
										SQLCHAR FAR *ColumnName,SQLSMALLINT BufferLength,
										SQLSMALLINT FAR *NameLengthPtr,
										SQLSMALLINT FAR *DataTypePtr,
										SQLUINTEGER FAR *ColumnSizePtr,
										SQLSMALLINT FAR *DecimalDigitsPtr,
										SQLSMALLINT FAR *NullablePtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("ColumnNumber", TYP_SQLUSMALLINT, (void*)ColumnNumber);
	call->insertArgument("ColumnName", TYP_SQLCHAR_PTR, ColumnName);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("NameLengthPtr", TYP_SQLSMALLINT_PTR, NameLengthPtr);
	call->insertArgument("DataTypePtr", TYP_SQLSMALLINT_PTR, DataTypePtr);
	call->insertArgument("ColumnSizePtr", TYP_SQLUINTEGER_PTR, ColumnSizePtr);
	call->insertArgument("DecimalDigitsPtr", TYP_SQLSMALLINT_PTR, DecimalDigitsPtr);
	call->insertArgument("NullablePtr", TYP_SQLSMALLINT_PTR, NullablePtr);

	call->function_name = "SQLDescribeCol";
	call->function_id = SQL_API_SQLDESCRIBECOL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLDescribeColW(SQLHSTMT hstmt, SQLUSMALLINT ColumnNumber,
										SQLWCHAR FAR *ColumnName,SQLSMALLINT BufferLength,
										SQLSMALLINT FAR *NameLengthPtr,
										SQLSMALLINT FAR *DataTypePtr,
										SQLUINTEGER FAR *ColumnSizePtr,
										SQLSMALLINT FAR *DecimalDigitsPtr,
										SQLSMALLINT FAR *NullablePtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("ColumnNumber", TYP_SQLUSMALLINT, (void*)ColumnNumber);
	call->insertArgument("ColumnName", TYP_SQLWCHAR_PTR, ColumnName);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("NameLengthPtr", TYP_SQLSMALLINT_PTR, NameLengthPtr);
	call->insertArgument("DataTypePtr", TYP_SQLSMALLINT_PTR, DataTypePtr);
	call->insertArgument("ColumnSizePtr", TYP_SQLUINTEGER_PTR, ColumnSizePtr);
	call->insertArgument("DecimalDigitsPtr", TYP_SQLSMALLINT_PTR, DecimalDigitsPtr);
	call->insertArgument("NullablePtr", TYP_SQLSMALLINT_PTR, NullablePtr);

	call->unicode = true;
	call->function_name = "SQLDescribeColW";
	call->function_id = SQL_API_SQLDESCRIBECOL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}



RETCODE SQL_API TraceSQLColAttribute(SQLHSTMT  StatementHandle,SQLUSMALLINT ColumnNumber,
									 SQLUSMALLINT FieldIdentifier,
									 SQLPOINTER  CharacterAttributePtr,
									 SQLSMALLINT BufferLength,
									 SQLSMALLINT *StringLengthPtr,
									 SQLPOINTER  NumericAttributePtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("StatementHandle", TYP_SQLHSTMT, StatementHandle);
	call->insertArgument("ColumnNumber", TYP_SQLUSMALLINT, (void*)ColumnNumber);
	call->insertArgument("FieldIdentifier", TYP_SQLUSMALLINT, (void*)FieldIdentifier);
	call->insertArgument("CharacterAttributePtr", TYP_SQLPOINTER, CharacterAttributePtr);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLSMALLINT_PTR, StringLengthPtr);
	call->insertArgument("NumericAttributePtr", TYP_SQLPOINTER, NumericAttributePtr);

	call->function_name = "SQLColAttribute";
	call->function_id = SQL_API_SQLCOLATTRIBUTE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLColAttributes(SQLHSTMT hstmt,SQLUSMALLINT icol,
									  SQLUSMALLINT fDescType,
									  SQLPOINTER rgbDesc, 
									  SQLSMALLINT cbDescMax,
									  SQLSMALLINT FAR *pcbDesc,
									  SQLINTEGER FAR *pfDesc)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("icol", TYP_SQLUSMALLINT, (void*)icol);
	call->insertArgument("fDescType", TYP_SQLUSMALLINT, (void*)fDescType);
	call->insertArgument("rgbDesc", TYP_SQLPOINTER, rgbDesc);
	call->insertArgument("cbDescMax", TYP_SQLSMALLINT, (void*)cbDescMax);
	call->insertArgument("pcbDesc", TYP_SQLSMALLINT_PTR, pcbDesc);
	call->insertArgument("pfDesc", TYP_SQLINTEGER_PTR, pfDesc);

	call->function_name = "SQLColAttributes";
	call->function_id = SQL_API_SQLCOLATTRIBUTES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLColAttributeW(SQLHSTMT  StatementHandle,SQLUSMALLINT ColumnNumber,
									 SQLUSMALLINT FieldIdentifier,
									 SQLPOINTER  CharacterAttributePtr,
									 SQLSMALLINT BufferLength,
									 SQLSMALLINT *StringLengthPtr,
									 SQLPOINTER  NumericAttributePtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("StatementHandle", TYP_SQLHSTMT, StatementHandle);
	call->insertArgument("ColumnNumber", TYP_SQLUSMALLINT, (void*)ColumnNumber);
	call->insertArgument("FieldIdentifier", TYP_SQLUSMALLINT, (void*)FieldIdentifier);
	call->insertArgument("CharacterAttributePtr", TYP_SQLPOINTER, CharacterAttributePtr);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLSMALLINT_PTR, StringLengthPtr);
	call->insertArgument("NumericAttributePtr", TYP_SQLPOINTER, NumericAttributePtr);

	call->unicode = true;
	call->function_name = "SQLColAttributeW";
	call->function_id = SQL_API_SQLCOLATTRIBUTE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLColAttributesW(SQLHSTMT hstmt,SQLUSMALLINT icol,
									  SQLUSMALLINT fDescType,
									  SQLPOINTER rgbDesc, 
									  SQLSMALLINT cbDescMax,
									  SQLSMALLINT FAR *pcbDesc,
									  SQLINTEGER FAR *pfDesc)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("icol", TYP_SQLUSMALLINT, (void*)icol);
	call->insertArgument("fDescType", TYP_SQLUSMALLINT, (void*)fDescType);
	call->insertArgument("rgbDesc", TYP_SQLPOINTER, rgbDesc);
	call->insertArgument("cbDescMax", TYP_SQLSMALLINT, (void*)cbDescMax);
	call->insertArgument("pcbDesc", TYP_SQLSMALLINT_PTR, pcbDesc);
	call->insertArgument("pfDesc", TYP_SQLINTEGER_PTR, pfDesc);

	call->unicode = true;
	call->function_name = "SQLColAttributesW";
	call->function_id = SQL_API_SQLCOLATTRIBUTES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}




RETCODE SQL_API TraceSQLBindCol(SQLHSTMT hstmt,	SQLUSMALLINT ColumnNumber,
								SQLSMALLINT TargetType, 
								SQLPOINTER TargetValuePtr,
								SQLINTEGER BufferLength, 
								SQLINTEGER FAR *StrLen_or_Ind)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("ColumnNumber", TYP_SQLUSMALLINT, (void*)ColumnNumber);
	call->insertArgument("TargetType", TYP_SQLSMALLINT, (void*)TargetType);
	call->insertArgument("TargetValuePtr", TYP_SQLPOINTER, TargetValuePtr);
	call->insertArgument("BufferLength", TYP_SQLINTEGER, (void*)BufferLength);
	call->insertArgument("StrLen_or_Ind", TYP_SQLINTEGER_PTR, StrLen_or_Ind);

	call->function_name = "SQLBindCol";
	call->function_id = SQL_API_SQLBINDCOL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLFetch(SQLHSTMT hstmt)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);

	call->function_name = "SQLFetch";
	call->function_id = SQL_API_SQLFETCH;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetData(SQLHSTMT hstmt,SQLUSMALLINT icol,
								SQLSMALLINT fCType,
								SQLPOINTER rgbValue,
								SQLINTEGER cbValueMax, 
								SQLINTEGER FAR *pcbValue)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("icol", TYP_SQLUSMALLINT, (void*)icol);
	call->insertArgument("fCType", TYP_SQLSMALLINT, (void*)fCType);
	call->insertArgument("rgbValue", TYP_SQLPOINTER, rgbValue);
	call->insertArgument("cbValueMax", TYP_SQLINTEGER, (void*)cbValueMax);
	call->insertArgument("pcbValue", TYP_SQLINTEGER_PTR, pcbValue);

	call->function_name = "SQLGetData";
	call->function_id = SQL_API_SQLGETDATA;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLMoreResults(SQLHSTMT  hstmt)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);

	call->function_name = "SQLMoreResults";
	call->function_id = SQL_API_SQLMORERESULTS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLRowCount(SQLHSTMT hstmt, SQLINTEGER FAR *pcrow)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("pcrow", TYP_SQLINTEGER_PTR, pcrow);

	call->function_name = "SQLRowCount";
	call->function_id = SQL_API_SQLROWCOUNT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLExtendedFetch(SQLHSTMT hstmt,
									  SQLUSMALLINT fFetchType,
									  SQLINTEGER irow,
									  SQLUINTEGER FAR *pcrow,
									  SQLUSMALLINT FAR *rgfRowStatus)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fFetchType", TYP_SQLUSMALLINT, (void*)fFetchType);
	call->insertArgument("irow", TYP_SQLINTEGER, (void*)irow);
	call->insertArgument("pcrow", TYP_SQLUINTEGER_PTR, pcrow);
	call->insertArgument("rgfRowStatus", TYP_SQLUSMALLINT_PTR, rgfRowStatus);

	call->function_name = "SQLExtendedFetch";
	call->function_id = SQL_API_SQLEXTENDEDFETCH;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLFetchScroll(SQLHSTMT    StatementHandle,
									SQLSMALLINT FetchOrientation, 
									SQLINTEGER  FetchOffset)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("StatementHandle", TYP_SQLHSTMT, StatementHandle);
	call->insertArgument("FetchOrientation", TYP_SQLSMALLINT, (void*)FetchOrientation);
	call->insertArgument("FetchOffset", TYP_SQLINTEGER, (void*)FetchOffset);

	call->function_name = "SQLFetchScroll";
	call->function_id = SQL_API_SQLFETCHSCROLL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetConnectOption(SQLHDBC hdbc, SQLUSMALLINT fOption,SQLUINTEGER  vParam)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("vParam", TYP_SQLUINTEGER, (void*)vParam);

	call->function_name = "SQLSetConnectOption";
	call->function_id = SQL_API_SQLSETCONNECTOPTION;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetConnectOptionW(SQLHDBC hdbc, SQLUSMALLINT fOption,SQLUINTEGER  vParam)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("vParam", TYP_SQLUINTEGER, (void*)vParam);

	call->unicode = true;
	call->function_name = "SQLSetConnectOptionW";
	call->function_id = SQL_API_SQLSETCONNECTOPTION;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetConnectOption(SQLHDBC hdbc,SQLUSMALLINT fOption,SQLPOINTER vParam)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("vParam", TYP_SQLPOINTER, vParam);

	call->function_name = "SQLGetConnectOption";
	call->function_id = SQL_API_SQLGETCONNECTOPTION;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetConnectOptionW(SQLHDBC hdbc,SQLUSMALLINT fOption,SQLPOINTER vParam)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("vParam", TYP_SQLPOINTER, vParam);

	call->unicode = true;
	call->function_name = "SQLGetConnectOptionW";
	call->function_id = SQL_API_SQLGETCONNECTOPTION;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetStmtOption(SQLHSTMT hstmt,SQLUSMALLINT fOption,SQLUINTEGER vParam)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("vParam", TYP_SQLUINTEGER, (void*)vParam);

	call->function_name = "SQLSetStmtOption";
	call->function_id = SQL_API_SQLSETSTMTOPTION;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetStmtOption(SQLHSTMT hstmt,SQLUSMALLINT fOption,SQLPOINTER vParam)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("vParam", TYP_SQLPOINTER, (void*)vParam);

	call->function_name = "SQLGetStmtOption";
	call->function_id = SQL_API_SQLGETSTMTOPTION;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetEnvAttr(SQLHENV    henv,
								   SQLINTEGER Attribute,
								   SQLPOINTER ValuePtr,
								   SQLINTEGER StringLength)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("StringLength", TYP_SQLINTEGER, (void*)StringLength);

	call->function_name = "SQLSetEnvAttr";
	call->function_id = SQL_API_SQLSETENVATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetEnvAttr(SQLHENV    henv,
								   SQLINTEGER Attribute,
								   SQLPOINTER ValuePtr,
								   SQLINTEGER BufferLength,
								   SQLINTEGER *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("BufferLength", TYP_SQLINTEGER, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLINTEGER_PTR, StringLengthPtr);

	call->function_name = "SQLGetEnvAttr";
	call->function_id = SQL_API_SQLGETENVATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetConnectAttr(SQLHDBC hdbc,
									   SQLINTEGER Attribute,
									   SQLPOINTER ValuePtr,
									   SQLINTEGER StringLength)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("StringLength", TYP_SQLINTEGER, (void*)StringLength);

	call->function_name = "SQLSetConnectAttr";
	call->function_id = SQL_API_SQLSETCONNECTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetConnectAttrW(SQLHDBC hdbc,
									   SQLINTEGER Attribute,
									   SQLPOINTER ValuePtr,
									   SQLINTEGER StringLength)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("StringLength", TYP_SQLINTEGER, (void*)StringLength);

	call->unicode = true;
	call->function_name = "SQLSetConnectAttrW";
	call->function_id = SQL_API_SQLSETCONNECTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetConnectAttr(SQLHDBC hdbc,
									   SQLINTEGER Attribute,
									   SQLPOINTER ValuePtr,
									   SQLINTEGER BufferLength,
									   SQLINTEGER *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("BufferLength", TYP_SQLINTEGER, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLINTEGER_PTR, (void*)StringLengthPtr);

	call->function_name = "SQLGetConnectAttr";
	call->function_id = SQL_API_SQLGETCONNECTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLGetConnectAttrW(SQLHDBC hdbc,
									   SQLINTEGER Attribute,
									   SQLPOINTER ValuePtr,
									   SQLINTEGER BufferLength,
									   SQLINTEGER *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("BufferLength", TYP_SQLINTEGER, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLINTEGER_PTR, (void*)StringLengthPtr);

	call->unicode = true;
	call->function_name = "SQLGetConnectAttrW";
	call->function_id = SQL_API_SQLGETCONNECTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}


RETCODE SQL_API TraceSQLSetStmtAttr(SQLHSTMT   hstmt,
									SQLINTEGER Attribute,
									SQLPOINTER ValuePtr,
									SQLINTEGER StringLength)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("StringLength", TYP_SQLINTEGER, (void*)StringLength);

	call->function_name = "SQLSetStmtAttr";
	call->function_id = SQL_API_SQLSETSTMTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetStmtAttrW(SQLHSTMT   hstmt,
									SQLINTEGER Attribute,
									SQLPOINTER ValuePtr,
									SQLINTEGER StringLength)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("StringLength", TYP_SQLINTEGER, (void*)StringLength);

	call->unicode = true;
	call->function_name = "SQLSetStmtAttrW";
	call->function_id = SQL_API_SQLSETSTMTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetStmtAttr(SQLHSTMT   hstmt,
									SQLINTEGER Attribute,
									SQLPOINTER ValuePtr,
									SQLINTEGER BufferLength,
									SQLINTEGER *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("BufferLength", TYP_SQLINTEGER, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLINTEGER_PTR, (void*)StringLengthPtr);


	call->function_name = "SQLGetStmtAttr";
	call->function_id = SQL_API_SQLGETSTMTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

} 
RETCODE SQL_API TraceSQLGetStmtAttrW(SQLHSTMT   hstmt,
									SQLINTEGER Attribute,
									SQLPOINTER ValuePtr,
									SQLINTEGER BufferLength,
									SQLINTEGER *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("Attribute", TYP_SQLINTEGER, (void*)Attribute);
	call->insertArgument("ValuePtr", TYP_SQLPOINTER, ValuePtr);
	call->insertArgument("BufferLength", TYP_SQLINTEGER, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLINTEGER_PTR, (void*)StringLengthPtr);


	call->unicode = true;
	call->function_name = "SQLGetStmtAttrW";
	call->function_id = SQL_API_SQLGETSTMTATTR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

} 
RETCODE SQL_API TraceSQLPrepare(SQLHSTMT hstmt,SQLCHAR FAR *szSqlStr,SQLINTEGER cbSqlStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szSqlStr", TYP_SQLCHAR_PTR, szSqlStr);
	call->insertArgument("cbSqlStr", TYP_SQLINTEGER, (void*)cbSqlStr);


	call->function_name = "SQLPrepare";
	call->function_id = SQL_API_SQLPREPARE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLPrepareW(SQLHSTMT hstmt,SQLWCHAR FAR *szSqlStr,SQLINTEGER cbSqlStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szSqlStr", TYP_SQLWCHAR_PTR, szSqlStr);
	call->insertArgument("cbSqlStr", TYP_SQLINTEGER, (void*)cbSqlStr);

	call->unicode = true;
	call->function_name = "SQLPrepareW";
	call->function_id = SQL_API_SQLPREPARE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLBindParameter(SQLHSTMT hstmt,SQLUSMALLINT ipar, 
									  SQLSMALLINT fParamType,
									  SQLSMALLINT fCType, 
									  SQLSMALLINT fSqlType,
									  SQLUINTEGER cbColDef, 
									  SQLSMALLINT ibScale,
									  SQLPOINTER rgbValue, 
									  SQLINTEGER cbValueMax,
									  SQLINTEGER FAR *pcbValue)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("ipar", TYP_SQLUSMALLINT, (void*)ipar);
	call->insertArgument("fParamType", TYP_SQLSMALLINT, (void*)fParamType);
	call->insertArgument("fCType", TYP_SQLSMALLINT, (void*)fCType);
	call->insertArgument("fSqlType", TYP_SQLSMALLINT, (void*)fSqlType);
	call->insertArgument("cbColDef", TYP_SQLUINTEGER, (void*)cbColDef);
	call->insertArgument("ibScale", TYP_SQLSMALLINT, (void*)ibScale);
	call->insertArgument("rgbValue", TYP_SQLPOINTER, rgbValue);
	call->insertArgument("cbValueMax", TYP_SQLINTEGER, (void*)cbValueMax);
	call->insertArgument("pcbValue", TYP_SQLINTEGER_PTR, pcbValue);

	call->function_name = "SQLBindParameter";
	call->function_id = SQL_API_SQLBINDPARAMETER;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLDescribeParam(SQLHSTMT hstmt, 
									  SQLUSMALLINT ipar,
									  SQLSMALLINT FAR *pfSqlType,
									  SQLUINTEGER FAR *pcbColDef,
									  SQLSMALLINT FAR *pibScale,
									  SQLSMALLINT FAR *pfNullable)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("ipar", TYP_SQLUSMALLINT, (void*)ipar);
	call->insertArgument("pfSqlType", TYP_SQLSMALLINT_PTR, (void*)pfSqlType);
	call->insertArgument("pcbColDef", TYP_SQLUINTEGER_PTR, pcbColDef);
	call->insertArgument("pibScale", TYP_SQLSMALLINT_PTR, pibScale);
	call->insertArgument("pfNullable", TYP_SQLSMALLINT_PTR, pfNullable);

	call->function_name = "SQLDescribeParam";
	call->function_id = SQL_API_SQLDESCRIBEPARAM;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLParamOptions(SQLHSTMT hstmt, SQLUINTEGER crow,SQLUINTEGER FAR *pirow)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("crow", TYP_SQLUINTEGER, (void*)crow);
	call->insertArgument("pirow", TYP_SQLUINTEGER_PTR, pirow);

	call->function_name = "SQLParamOptions";
	call->function_id = SQL_API_SQLPARAMOPTIONS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLNumParams(SQLHSTMT hstmt, SQLSMALLINT FAR *pcpar)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("pcpar", TYP_SQLSMALLINT_PTR, pcpar);

	call->function_name = "SQLNumParams";
	call->function_id = SQL_API_SQLNUMPARAMS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetScrollOptions(SQLHSTMT hstmt,
										 SQLUSMALLINT fConcurrency,
										 SQLINTEGER crowKeyset,
										 SQLUSMALLINT crowRowset)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fConcurrency", TYP_SQLUSMALLINT, (void*)fConcurrency);
	call->insertArgument("crowKeyset", TYP_SQLINTEGER, (void*)crowKeyset);
	call->insertArgument("crowRowset", TYP_SQLUSMALLINT, (void*)crowRowset);

	call->function_name = "SQLSetScrollOptions";
	call->function_id = SQL_API_SQLSETSCROLLOPTIONS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLExecute(SQLHSTMT hstmt)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);

	call->function_name = "SQLExecute";
	call->function_id = SQL_API_SQLEXECUTE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLExecDirect(SQLHSTMT hstmt, SQLCHAR FAR *szSqlStr, SQLINTEGER cbSqlStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szSqlStr", TYP_SQLCHAR_PTR, szSqlStr);
	call->insertArgument("cbSqlStr", TYP_SQLINTEGER, (void*)cbSqlStr);

	call->function_name = "SQLExecDirect";
	call->function_id = SQL_API_SQLEXECDIRECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLExecDirectW(SQLHSTMT hstmt, SQLWCHAR FAR *szSqlStr, SQLINTEGER cbSqlStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szSqlStr", TYP_SQLWCHAR_PTR, szSqlStr);
	call->insertArgument("cbSqlStr", TYP_SQLINTEGER, (void*)cbSqlStr);

	call->unicode = true;
	call->function_name = "SQLExecDirectW";
	call->function_id = SQL_API_SQLEXECDIRECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}



RETCODE SQL_API TraceSQLNativeSql(SQLHDBC hdbc,
								  SQLCHAR FAR *szSqlStrIn, 
								  SQLINTEGER cbSqlStrIn,
								  SQLCHAR FAR *szSqlStr,
								  SQLINTEGER cbSqlStrMax,
								  SQLINTEGER FAR *pcbSqlStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("szSqlStrIn", TYP_SQLCHAR_PTR, szSqlStrIn);
	call->insertArgument("cbSqlStrIn", TYP_SQLINTEGER, (void*)cbSqlStrIn);
	call->insertArgument("szSqlStr", TYP_SQLCHAR_PTR, szSqlStr);
	call->insertArgument("cbSqlStrMax", TYP_SQLINTEGER, (void*)cbSqlStrMax);
	call->insertArgument("pcbSqlStr", TYP_SQLINTEGER_PTR, pcbSqlStr);

	call->function_name = "SQLNativeSql";
	call->function_id = SQL_API_SQLNATIVESQL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLNativeSqlW(SQLHDBC hdbc,
								  SQLWCHAR FAR *szSqlStrIn, 
								  SQLINTEGER cbSqlStrIn,
								  SQLWCHAR FAR *szSqlStr,
								  SQLINTEGER cbSqlStrMax,
								  SQLINTEGER FAR *pcbSqlStr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("szSqlStrIn", TYP_SQLWCHAR_PTR, szSqlStrIn);
	call->insertArgument("cbSqlStrIn", TYP_SQLINTEGER, (void*)cbSqlStrIn);
	call->insertArgument("szSqlStr", TYP_SQLWCHAR_PTR, szSqlStr);
	call->insertArgument("cbSqlStrMax", TYP_SQLINTEGER, (void*)cbSqlStrMax);
	call->insertArgument("pcbSqlStr", TYP_SQLINTEGER_PTR, pcbSqlStr);

	call->unicode = true;
	call->function_name = "SQLNativeSqlW";
	call->function_id = SQL_API_SQLNATIVESQL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLParamData(SQLHSTMT hstmt, SQLPOINTER FAR *prbgValue)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("prbgValue", TYP_SQLPOINTER_PTR, prbgValue);

	call->function_name = "SQLParamData";
	call->function_id = SQL_API_SQLPARAMDATA;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLPutData(SQLHSTMT hstmt, SQLPOINTER rgbValue,SQLINTEGER cbValue)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("rgbValue", TYP_SQLPOINTER, rgbValue);
	call->insertArgument("cbValue", TYP_SQLINTEGER, (void*)cbValue);

	call->function_name = "SQLPutData";
	call->function_id = SQL_API_SQLPUTDATA;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLCancel(SQLHSTMT hstmt)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);

	call->function_name = "SQLCancel";
	call->function_id = SQL_API_SQLCANCEL;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLAllocEnv(SQLHENV FAR * phenv)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("phenv", TYP_SQLHENV_PTR, phenv);

	call->function_name = "SQLAllocEnv";
	call->function_id = SQL_API_SQLALLOCENV;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);
}

RETCODE  SQL_API TraceSQLDataSources(SQLHENV EnvironmentHandle,
           SQLUSMALLINT Direction, 
		   SQLCHAR *ServerName, SQLSMALLINT BufferLength1, SQLSMALLINT *NameLength1,
           SQLCHAR *Description, SQLSMALLINT BufferLength2, SQLSMALLINT *NameLength2)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("EnvironmentHandle", TYP_SQLHENV, EnvironmentHandle);
	call->insertArgument("Direction", TYP_SQLUSMALLINT, (void*)Direction);
	call->insertArgument("ServerName", TYP_SQLCHAR_PTR, ServerName);
	call->insertArgument("BufferLength1", TYP_SQLSMALLINT, (void*)BufferLength1);
	call->insertArgument("NameLength1", TYP_SQLSMALLINT_PTR, NameLength1);
	call->insertArgument("Description", TYP_SQLCHAR_PTR, Description);
	call->insertArgument("BufferLength2", TYP_SQLSMALLINT, (void*)BufferLength2);
	call->insertArgument("NameLength2", TYP_SQLSMALLINT_PTR, NameLength2);

	call->function_name = "TraceSQLDataSources";
	call->function_id = SQL_API_SQLDATASOURCES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);
}


RETCODE SQL_API TraceSQLDataSourcesW( SQLHENV            EnvironmentHandle,
		SQLUSMALLINT       Direction,
		SQLWCHAR *ServerName, SQLSMALLINT BufferLength1, SQLSMALLINT    *NameLength1,
		SQLWCHAR *Description, SQLSMALLINT BufferLength2, SQLSMALLINT *NameLength2)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("EnvironmentHandle", TYP_SQLHENV, EnvironmentHandle);
	call->insertArgument("Direction", TYP_SQLUSMALLINT, (void*)Direction);
	call->insertArgument("ServerName", TYP_SQLWCHAR_PTR, ServerName);
	call->insertArgument("BufferLength1", TYP_SQLSMALLINT, (void*)BufferLength1);
	call->insertArgument("NameLength1", TYP_SQLSMALLINT_PTR, NameLength1);
	call->insertArgument("Description", TYP_SQLWCHAR_PTR, Description);
	call->insertArgument("BufferLength2", TYP_SQLSMALLINT, (void*)BufferLength2);
	call->insertArgument("NameLength2", TYP_SQLSMALLINT_PTR, NameLength2);

	call->unicode = true;
	call->function_name = "TraceSQLDataSourcesW";
	call->function_id = SQL_API_SQLDATASOURCES;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);
}



RETCODE SQL_API TraceSQLAllocHandleStd  (SQLSMALLINT type,
		 SQLHANDLE handle,
		 SQLHANDLE * handleptr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("type", TYP_SQLSMALLINT, (void*)type);
	call->insertArgument("handle", TYP_SQLHANDLE, handle);
	call->insertArgument("handleptr", TYP_SQLHANDLE_PTR, handleptr);

	call->function_name = "SQLAllocHandleStd";
	call->function_id = SQL_API_SQLALLOCHANDLESTD;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);
}
RETCODE SQL_API TraceSQLAllocHandleStdW (SQLSMALLINT type,
		 SQLHANDLE handle,
		 SQLHANDLE * handleptr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->unicode = true;
	call->insertArgument("type", TYP_SQLSMALLINT, (void*)type);
	call->insertArgument("handle", TYP_SQLHANDLE, handle);
	call->insertArgument("handleptr", TYP_SQLHANDLE_PTR, handleptr);

	call->function_name = "SQLAllocHandleStdW";
	call->function_id = SQL_API_SQLALLOCHANDLESTD;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);
}




RETCODE SQL_API TraceSQLFreeEnv(SQLHENV henv)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);

	call->function_name = "SQLFreeEnv";
	call->function_id = SQL_API_SQLFREEENV;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLAllocConnect(SQLHENV henv, SQLHDBC FAR *phdbc)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);
	call->insertArgument("phdbc", TYP_SQLHDBC_PTR, phdbc);

	call->function_name = "SQLAllocConnect";
	call->function_id = SQL_API_SQLALLOCCONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLFreeConnect(SQLHDBC hdbc)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);

	call->function_name = "SQLFreeConnect";
	call->function_id = SQL_API_SQLFREECONNECT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLAllocStmt(SQLHDBC hdbc,SQLHSTMT FAR *phstmt)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("phstmt", TYP_SQLHSTMT_PTR, phstmt);

	call->function_name = "SQLAllocStmt";
	call->function_id = SQL_API_SQLALLOCSTMT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLFreeStmt(SQLHSTMT hstmt,SQLUSMALLINT fOption)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);

	call->function_name = "SQLFreeStmt";
	call->function_id = SQL_API_SQLFREESTMT;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLAllocHandle(SQLSMALLINT HandleType,
									SQLHANDLE   InputHandle,
									SQLHANDLE   *OutputHandlePtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("InputHandle", TYP_SQLHANDLE, InputHandle);
	call->insertArgument("OutputHandlePtr", TYP_SQLHANDLE_PTR, OutputHandlePtr);

	call->function_name = "SQLAllocHandle";
	call->function_id = SQL_API_SQLALLOCHANDLE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLFreeHandle(SQLSMALLINT HandleType,SQLHANDLE   Handle)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("Handle", TYP_SQLHANDLE, Handle);

	call->function_name = "SQLFreeHandle";
	call->function_id = SQL_API_SQLFREEHANDLE;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetCursorName(SQLHSTMT hstmt, SQLCHAR *szCursor, SQLSMALLINT cbCursor)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szCursor", TYP_SQLCHAR_PTR, szCursor);
	call->insertArgument("cbCursor", TYP_SQLSMALLINT, (void*)cbCursor);

	call->function_name = "SQLSetCursorName";
	call->function_id = SQL_API_SQLSETCURSORNAME;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetCursorNameW(SQLHSTMT hstmt, SQLWCHAR *szCursor, SQLSMALLINT cbCursor)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szCursor", TYP_SQLWCHAR_PTR, szCursor);
	call->insertArgument("cbCursor", TYP_SQLSMALLINT, (void*)cbCursor);

	call->unicode = true;
	call->function_name = "SQLSetCursorNameW";
	call->function_id = SQL_API_SQLSETCURSORNAME;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetCursorName(SQLHSTMT hstmt, 
									  SQLCHAR FAR *szCursor,
									  SQLSMALLINT cbCursorMax, 
									  SQLSMALLINT FAR *pcbCursor)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szCursor", TYP_SQLCHAR_PTR, szCursor);
	call->insertArgument("cbCursorMax", TYP_SQLSMALLINT, (void*)cbCursorMax);
	call->insertArgument("pcbCursor", TYP_SQLSMALLINT_PTR, pcbCursor);

	call->function_name = "SQLGetCursorName";
	call->function_id = SQL_API_SQLGETCURSORNAME;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetCursorNameW(SQLHSTMT hstmt, 
									  SQLWCHAR FAR *szCursor,
									  SQLSMALLINT cbCursorMax, 
									  SQLSMALLINT FAR *pcbCursor)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szCursor", TYP_SQLWCHAR_PTR, szCursor);
	call->insertArgument("cbCursorMax", TYP_SQLSMALLINT, (void*)cbCursorMax);
	call->insertArgument("pcbCursor", TYP_SQLSMALLINT_PTR, pcbCursor);

	call->unicode = true;
	call->function_name = "SQLGetCursorNameW";
	call->function_id = SQL_API_SQLGETCURSORNAME;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLSetPos( SQLHSTMT hstmt, 
							   SQLUSMALLINT irow,
							   SQLUSMALLINT fOption, 
							   SQLUSMALLINT fLock)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("irow", TYP_SQLUSMALLINT, (void*)irow);
	call->insertArgument("fOption", TYP_SQLUSMALLINT, (void*)fOption);
	call->insertArgument("fLock", TYP_SQLUSMALLINT, (void*)fLock);

	call->function_name = "SQLSetPos";
	call->function_id = SQL_API_SQLSETPOS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLBulkOperations(SQLHSTMT  Handle, SQLSMALLINT Operation)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("Handle", TYP_SQLHSTMT, Handle);
	call->insertArgument("Operation", TYP_SQLSMALLINT, (void*)Operation);

	call->function_name = "SQLBulkOperations";
	call->function_id = SQL_API_SQLBULKOPERATIONS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLCloseCursor(SQLHSTMT Handle)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("Handle", TYP_SQLHSTMT, Handle);

	call->function_name = "SQLCloseCursor";
	call->function_id = SQL_API_SQLCLOSECURSOR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetInfo(SQLHDBC hdbc, 
								SQLUSMALLINT fInfoType,  
								SQLPOINTER rgbInfoValue,
								SQLSMALLINT cbInfoValueMax,
								SQLSMALLINT FAR *pcbInfoValue)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fInfoType", TYP_SQLUSMALLINT, (void*)fInfoType);
	call->insertArgument("rgbInfoValue", TYP_SQLPOINTER, rgbInfoValue);
	call->insertArgument("cbInfoValueMax", TYP_SQLSMALLINT, (void*)cbInfoValueMax);
	call->insertArgument("pcbInfoValue", TYP_SQLSMALLINT_PTR, pcbInfoValue);

	call->function_name = "SQLGetInfo";
	call->function_id = SQL_API_SQLGETINFO;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetInfoW(SQLHDBC hdbc, 
								SQLUSMALLINT fInfoType,  
								SQLPOINTER rgbInfoValue,
								SQLSMALLINT cbInfoValueMax,
								SQLSMALLINT FAR *pcbInfoValue)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fInfoType", TYP_SQLUSMALLINT, (void*)fInfoType);
	call->insertArgument("rgbInfoValue", TYP_SQLPOINTER, rgbInfoValue);
	call->insertArgument("cbInfoValueMax", TYP_SQLSMALLINT, (void*)cbInfoValueMax);
	call->insertArgument("pcbInfoValue", TYP_SQLSMALLINT_PTR, pcbInfoValue);

	call->unicode = true;
	call->function_name = "SQLGetInfoW";
	call->function_id = SQL_API_SQLGETINFO;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetTypeInfo(SQLHSTMT hstmt, SQLSMALLINT fSqlType)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("fSqlType", TYP_SQLSMALLINT, (void*)fSqlType);

	call->function_name = "SQLGetTypeInfo";
	call->function_id = SQL_API_SQLGETTYPEINFO;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetFunctions(SQLHDBC hdbc,SQLUSMALLINT fFunction, SQLUSMALLINT FAR *pfExists)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("fFunction", TYP_SQLUSMALLINT, (void*)fFunction);
	call->insertArgument("pfExists", TYP_SQLUSMALLINT, (void*)pfExists);

	call->function_name = "SQLGetFunctions";
	call->function_id = SQL_API_SQLGETFUNCTIONS;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetDiagField(SQLSMALLINT HandleType,
									 SQLHANDLE   Handle,
									 SQLSMALLINT RecNumber,
									 SQLSMALLINT DiagIdentifier,
									 SQLPOINTER  DiagInfoPtr,
									 SQLSMALLINT BufferLength,
									 SQLSMALLINT *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("Handle", TYP_SQLHANDLE, Handle);
	call->insertArgument("RecNumber", TYP_SQLSMALLINT, (void*)RecNumber);
	call->insertArgument("DiagIdentifier", TYP_SQLSMALLINT, (void*)DiagIdentifier);
	call->insertArgument("DiagInfoPtr", TYP_SQLPOINTER, DiagInfoPtr);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLSMALLINT_PTR, StringLengthPtr);

	call->function_name = "SQLGetDiagField";
	call->function_id = SQL_API_SQLGETDIAGFIELD;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetDiagFieldW(SQLSMALLINT HandleType,
									 SQLHANDLE   Handle,
									 SQLSMALLINT RecNumber,
									 SQLSMALLINT DiagIdentifier,
									 SQLPOINTER  DiagInfoPtr,
									 SQLSMALLINT BufferLength,
									 SQLSMALLINT *StringLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("Handle", TYP_SQLHANDLE, Handle);
	call->insertArgument("RecNumber", TYP_SQLSMALLINT, (void*)RecNumber);
	call->insertArgument("DiagIdentifier", TYP_SQLSMALLINT, (void*)DiagIdentifier);
	call->insertArgument("DiagInfoPtr", TYP_SQLPOINTER, DiagInfoPtr);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("StringLengthPtr", TYP_SQLSMALLINT_PTR, StringLengthPtr);

	call->unicode = true;
	call->function_name = "SQLGetDiagFieldW";
	call->function_id = SQL_API_SQLGETDIAGFIELD;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}
RETCODE SQL_API TraceSQLGetDiagRec(SQLSMALLINT HandleType,
								   SQLHANDLE   Handle,
								   SQLSMALLINT RecNumber,
								   SQLCHAR     *Sqlstate,
								   SQLINTEGER  *NativeErrorPtr,
								   SQLCHAR     *MessageText,
								   SQLSMALLINT BufferLength,
								   SQLSMALLINT *TextLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("Handle", TYP_SQLHANDLE, Handle);
	call->insertArgument("RecNumber", TYP_SQLSMALLINT, (void*)RecNumber);
	call->insertArgument("Sqlstate", TYP_SQLCHAR_PTR, Sqlstate);
	call->insertArgument("NativeErrorPtr", TYP_SQLINTEGER_PTR, NativeErrorPtr);
	call->insertArgument("MessageText", TYP_SQLCHAR_PTR, MessageText);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("TextLengthPtr", TYP_SQLSMALLINT_PTR, TextLengthPtr);

	call->function_name = "SQLGetDiagRec";
	call->function_id = SQL_API_SQLGETDIAGREC;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLGetDiagRecW(SQLSMALLINT HandleType,
								   SQLHANDLE   Handle,
								   SQLSMALLINT RecNumber,
								   SQLWCHAR     *Sqlstate,
								   SQLINTEGER  *NativeErrorPtr,
								   SQLWCHAR     *MessageText,
								   SQLSMALLINT BufferLength,
								   SQLSMALLINT *TextLengthPtr)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("HandleType", TYP_SQLSMALLINT, (void*)HandleType);
	call->insertArgument("Handle", TYP_SQLHANDLE, Handle);
	call->insertArgument("RecNumber", TYP_SQLSMALLINT, (void*)RecNumber);
	call->insertArgument("Sqlstate", TYP_SQLWCHAR_PTR, Sqlstate);
	call->insertArgument("NativeErrorPtr", TYP_SQLINTEGER_PTR, NativeErrorPtr);
	call->insertArgument("MessageText", TYP_SQLWCHAR_PTR, MessageText);
	call->insertArgument("BufferLength", TYP_SQLSMALLINT, (void*)BufferLength);
	call->insertArgument("TextLengthPtr", TYP_SQLSMALLINT_PTR, TextLengthPtr);

	call->unicode = true;
	call->function_name = "SQLGetDiagRecW";
	call->function_id = SQL_API_SQLGETDIAGREC;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}


RETCODE SQL_API TraceSQLError(SQLHENV henv, 
							  SQLHDBC hdbc, 
							  SQLHSTMT hstmt,
							  SQLCHAR FAR	  *szSqlState,
							  SQLINTEGER FAR *pfNativeError,
							  SQLCHAR FAR	  *szErrorMsg,
							  SQLSMALLINT	  cbErrorMsgMax,
							  SQLSMALLINT FAR *pcbErrorMsg)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);
	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szSqlState", TYP_SQLCHAR_PTR, szSqlState);
	call->insertArgument("pfNativeError", TYP_SQLINTEGER_PTR, pfNativeError);
	call->insertArgument("szErrorMsg", TYP_SQLCHAR_PTR, szErrorMsg);
	call->insertArgument("cbErrorMsgMax", TYP_SQLSMALLINT, (void*)cbErrorMsgMax);
	call->insertArgument("pcbErrorMsg", TYP_SQLSMALLINT_PTR, pcbErrorMsg);


	call->function_name = "SQLError";
	call->function_id = SQL_API_SQLERROR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

RETCODE SQL_API TraceSQLErrorW(SQLHENV henv, 
							  SQLHDBC hdbc, 
							  SQLHSTMT hstmt,
							  SQLWCHAR FAR	  *szSqlState,
							  SQLINTEGER FAR *pfNativeError,
							  SQLWCHAR FAR	  *szErrorMsg,
							  SQLSMALLINT	  cbErrorMsgMax,
							  SQLSMALLINT FAR *pcbErrorMsg)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("henv", TYP_SQLHENV, henv);
	call->insertArgument("hdbc", TYP_SQLHDBC, hdbc);
	call->insertArgument("hstmt", TYP_SQLHSTMT, hstmt);
	call->insertArgument("szSqlState", TYP_SQLWCHAR_PTR, szSqlState);
	call->insertArgument("pfNativeError", TYP_SQLINTEGER_PTR, pfNativeError);
	call->insertArgument("szErrorMsg", TYP_SQLWCHAR_PTR, szErrorMsg);
	call->insertArgument("cbErrorMsgMax", TYP_SQLSMALLINT, (void*)cbErrorMsgMax);
	call->insertArgument("pcbErrorMsg", TYP_SQLSMALLINT_PTR, pcbErrorMsg);

	call->unicode = true;
	call->function_name = "SQLErrorW";
	call->function_id = SQL_API_SQLERROR;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

//------------------------------------------------------------------------------------------

RETCODE SQL_API TraceSQLCopyDesc(SQLHDESC SourceDescHandle,SQLHDESC TargetDescHandle)
{
	ODBCTraceCall *call = new ODBCTraceCall();

	call->insertArgument("SoureDescHandle", TYP_SQLHDESC, SourceDescHandle);
	call->insertArgument("TargetDescHandle", TYP_SQLHDESC, TargetDescHandle);

	call->function_name = "SQLCopyDesc";
	call->function_id = SQL_API_SQLCOPYDESC;

	ODBCTraceEnter(call);
	return (RETCODE)stack.push(call);

}

//------------------------------------------------------------------------------------------
 
RETCODE	SQL_API TraceOpenLogFile(LPWSTR s,LPWSTR t,DWORD w)// open a trace log file
{
   char buffer[256];
   size_t nbChar ;
   wcstombs_s(&nbChar, buffer, 256, s, sizeof(buffer) );

	ODBCTraceOptions::getUniqueInstance()->logfile = buffer;
	return 0;
}
//------------------------------------------------------------------------------------------
RETCODE	SQL_API TraceCloseLogFile()			// Request to close a trace log
{
	return 0;
}
//------------------------------------------------------------------------------------------
VOID SQL_API TraceReturn(RETCODE rethandle,RETCODE retcode)	// Processes trace after FN is called
{
	ODBCTraceCall *call = stack.pop(rethandle);
	if (call != NULL)
	{
		call->retcode = retcode;
		ODBCTraceExit(call);
		delete call;
	}
} 
//------------------------------------------------------------------------------------------
DWORD SQL_API TraceVersion()					// Returns trace API version
{
	return TRACE_VERSION; 
}  
 