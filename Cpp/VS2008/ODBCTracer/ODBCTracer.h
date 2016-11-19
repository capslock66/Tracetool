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

#if !defined(ODBCDRIVERDELEGATOR_13_06_2005_ARINIR_H)
#define ODBCDRIVERDELEGATOR_13_06_2005_ARINIR_H

#include <sstream>
#include <sqltypes.h>
#include "Hyperlink.h"
#include "SystemTraySDK.h"
//#include <string>
#include "..\..\Source\tracetool.h"

#define DeclareString(s) static const char * str ## s = #s
typedef std::map<std::string, int> CHAR2FuncSwitchMAP;
typedef std::map<int, bool> INT2FuncSwitchMAP;

//------------------------------------------------------------------------------------------
/**
 * Mutual exclusion
 */
class Mutex
{
public:
	Mutex();
	~Mutex();
	void enter();
	void leave();
private:
	CRITICAL_SECTION CriticalSection;
};
//------------------------------------------------------------------------------------------

class MutexGuard
{
public:
	MutexGuard(Mutex *mutex);
	~MutexGuard();
protected:
	MutexGuard();
	MutexGuard(const MutexGuard &);
	MutexGuard & operator = (const MutexGuard &);
	Mutex *mutex;
};

//------------------------------------------------------------------------------------------

/**
 * General options. 
 */
class ODBCTraceOptions
{
private:
	ODBCTraceOptions();
	static ODBCTraceOptions* unique_instance;
	static CHAR2FuncSwitchMAP fn2id;
	INT2FuncSwitchMAP options;
	void init(const char *fn, int fid);
public:
	static ODBCTraceOptions* getUniqueInstance();
	//Functionname to function id...
	static int FN2ID(const char *n);

	bool logFunction(int functionid);
	bool logFunction(const char *function);
	void setLogFunction(const char *function, bool log);

	bool fileloggingactivated;
	std::string logfile;
	std::vector<std::string> keys;
};
//------------------------------------------------------------------------------------------
/**
 * Dialog for configuring trace options...
 */
class ODBCTraceOptionDlg
{
public:
	ODBCTraceOptionDlg(ODBCTraceOptions *options);
	void commit();
	int doModal(HWND hwnd);
	static int CALLBACK ConfigDlgProc (HWND hdlg,WORD wMsg,WPARAM wParam,LPARAM lParam);
	static HINSTANCE getHINSTANCE();
protected:
	HWND wnd;
	HWND hwndParent;
	int maxcheckbuttonid;
	ODBCTraceOptions *old_options;
	ODBCTraceOptions new_options;
	CHyperlink hyperlink;
	void createCheckBoxes();
};

//------------------------------------------------------------------------------------------
/**
 * Dialog for printing trace informations
 */
class ODBCTraceDialog
{
private:
	CSystemTray system_tray;
	HWND tracer_dialog;
	ODBCTraceDialog();
	~ODBCTraceDialog();
	static ODBCTraceDialog* unique_instance;
	Mutex lock;
	std::ostringstream *buffer;
	int timerID;
public:
	static ODBCTraceDialog* getUniqueInstance();
	void startFlush();
	void stopFlush();
	void appendText(const char *text);
	void flush();
   WinTrace * ODBCWinTrace ;
	HWND getWND();
	CSystemTray& getSystemTray();
	void destroyWindow();
};
//------------------------------------------------------------------------------------------
#define ODBCTRACE_STACKSIZE 256
#define MAX_ARGUMENTS 20
/**
 * SQL_TYPES
 */
enum ODBCTracer_ArgumentTypes
{
	TYP_UNKNOWN = 0,

	TYP_SQLPOINTER,
	TYP_SQLSMALLINT,
	TYP_SQLUSMALLINT,
	TYP_SQLINTEGER,
	TYP_SQLUINTEGER,

	TYP_SQLPOINTER_PTR,
	TYP_SQLCHAR_PTR,
	TYP_SQLWCHAR_PTR,
	TYP_SQLSMALLINT_PTR,
	TYP_SQLUSMALLINT_PTR,
	TYP_SQLINTEGER_PTR,
	TYP_SQLUINTEGER_PTR,

	TYP_SQLHDESC,
	TYP_SQLHSTMT,
	TYP_SQLHDBC,
	TYP_SQLHWND,
	TYP_SQLHENV,
	TYP_SQLHANDLE,

	TYP_SQLHDESC_PTR,
	TYP_SQLHSTMT_PTR,
	TYP_SQLHDBC_PTR,
	TYP_SQLHWND_PTR,
	TYP_SQLHENV_PTR,
	TYP_SQLHANDLE_PTR
};
//------------------------------------------------------------------------------------------

/**
 * Used by each TraceXXX function to fill debug information about ODBC function call...
 */
struct ODBCTraceArgument
{
	std::string name;
	ODBCTracer_ArgumentTypes type;
	void *value;
};
//------------------------------------------------------------------------------------------

struct ODBCTraceCall
{
	ODBCTraceCall();
	void insertArgument(const char *name, ODBCTracer_ArgumentTypes type, void *value); 

	std::string function_name;
	int function_id;
	int arguments_count;
	int retcode;
	bool unicode;
	ODBCTraceArgument arguments[MAX_ARGUMENTS];
   TraceNodeEx * nodeEx ;
};
//------------------------------------------------------------------------------------------

/**
 * Little different semantic to known stack. Each ODBC-API invocation gets one position from this
 * stack. By pushing trace calls to this stack one can determine the calling function in 
 * TraceReturn(...).
 */
struct ODBCTraceStack
{
	ODBCTraceStack();

	int push(ODBCTraceCall *call);
	ODBCTraceCall* pop(int index);

	Mutex lock;
	ODBCTraceCall* stack[ODBCTRACE_STACKSIZE];
};
//------------------------------------------------------------------------------------------

/**
 * dumps invocation information in either way: calling or returning
 */
void ODBCTraceEnter(ODBCTraceCall *call);
void ODBCTraceExit (ODBCTraceCall *call);

#endif //#if !defined(ODBCDRIVERDELEGATOR_13_06_2005_ARINIR_H)