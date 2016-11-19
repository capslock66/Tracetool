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
#include <sys/stat.h>
#include <direct.h>
#include <time.h>
#include "sql.h"
#include "sqlext.h"


#define MAX_BUFFER_CONVERT 1024


#define ID2STRINGMAP_BEGIN(funcname) \
char * funcname(int code) \
{ \
switch(code) {

#define ID2STRINGMAP_ELEMENT(el) case el: return #el;
#define ID2STRINGMAP_ELEMENT2(eln, elv) case eln: return elv;

#define ID2STRINGMAP_END() \
	default:\
		return "unknown code"; }}

ID2STRINGMAP_BEGIN(GetInfoType2Char)
	ID2STRINGMAP_ELEMENT( SQL_ODBC_VER)
	ID2STRINGMAP_ELEMENT( SQL_ACTIVE_ENVIRONMENTS)
	ID2STRINGMAP_ELEMENT( SQL_ACCESSIBLE_TABLES)	
	ID2STRINGMAP_ELEMENT( SQL_ALTER_DOMAIN)
	ID2STRINGMAP_ELEMENT( SQL_ALTER_TABLE)
	ID2STRINGMAP_ELEMENT( SQL_ASYNC_MODE)
	ID2STRINGMAP_ELEMENT( SQL_BATCH_ROW_COUNT)
	ID2STRINGMAP_ELEMENT( SQL_CATALOG_LOCATION)
	ID2STRINGMAP_ELEMENT( SQL_CATALOG_NAME_SEPARATOR)
	ID2STRINGMAP_ELEMENT( SQL_CATALOG_TERM)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_BIGINT)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_BIT)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_CHAR)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_DATE)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_DECIMAL)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_DOUBLE)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_FLOAT)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_INTEGER)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_LONGVARCHAR)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_NUMERIC)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_REAL)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_SMALLINT)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_TIME)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_TIMESTAMP)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_TINYINT)
	ID2STRINGMAP_ELEMENT( SQL_CONVERT_VARCHAR)
	ID2STRINGMAP_ELEMENT( SQL_DATETIME_LITERALS)
	ID2STRINGMAP_ELEMENT( SQL_CURSOR_COMMIT_BEHAVIOR)
	ID2STRINGMAP_ELEMENT( SQL_CURSOR_ROLLBACK_BEHAVIOR)
	ID2STRINGMAP_ELEMENT( SQL_TXN_ISOLATION_OPTION)
	ID2STRINGMAP_ELEMENT( SQL_MAX_INDEX_SIZE)
	ID2STRINGMAP_ELEMENT( SQL_MAX_PROCEDURE_NAME_LEN)
	ID2STRINGMAP_ELEMENT( SQL_MAX_ROW_SIZE)
	ID2STRINGMAP_ELEMENT( SQL_MAX_TABLES_IN_SELECT)
	ID2STRINGMAP_ELEMENT( SQL_MAX_COLUMNS_IN_GROUP_BY)
	ID2STRINGMAP_ELEMENT( SQL_MAX_COLUMNS_IN_INDEX)
	ID2STRINGMAP_ELEMENT( SQL_MAX_COLUMNS_IN_ORDER_BY)
	ID2STRINGMAP_ELEMENT( SQL_MAX_COLUMNS_IN_SELECT)
	ID2STRINGMAP_ELEMENT( SQL_MAX_COLUMNS_IN_TABLE)
	ID2STRINGMAP_ELEMENT( SQL_MAX_CATALOG_NAME_LEN)
	ID2STRINGMAP_ELEMENT( SQL_MAX_SCHEMA_NAME_LEN)
	ID2STRINGMAP_ELEMENT( SQL_MAX_TABLE_NAME_LEN)
	ID2STRINGMAP_ELEMENT( SQL_NULL_COLLATION)
	ID2STRINGMAP_ELEMENT( SQL_ORDER_BY_COLUMNS_IN_SELECT)
	ID2STRINGMAP_ELEMENT( SQL_INTEGRITY)
	ID2STRINGMAP_ELEMENT( SQL_ACCESSIBLE_PROCEDURES)
	ID2STRINGMAP_ELEMENT( SQL_SPECIAL_CHARACTERS)
	ID2STRINGMAP_ELEMENT( SQL_IDENTIFIER_QUOTE_CHAR)
	ID2STRINGMAP_ELEMENT( SQL_DYNAMIC_CURSOR_ATTRIBUTES1)
	ID2STRINGMAP_ELEMENT( SQL_KEYSET_CURSOR_ATTRIBUTES1)
	ID2STRINGMAP_ELEMENT( SQL_KEYSET_CURSOR_ATTRIBUTES2)
	ID2STRINGMAP_ELEMENT( SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1)
	ID2STRINGMAP_ELEMENT( SQL_STATIC_CURSOR_ATTRIBUTES1)
	ID2STRINGMAP_ELEMENT( SQL_STATIC_CURSOR_ATTRIBUTES2)
	ID2STRINGMAP_ELEMENT( SQL_NUMERIC_FUNCTIONS)
	ID2STRINGMAP_ELEMENT( SQL_STATIC_SENSITIVITY)
	ID2STRINGMAP_ELEMENT( SQL_LOCK_TYPES)
	ID2STRINGMAP_ELEMENT( SQL_SCROLL_OPTIONS)
	ID2STRINGMAP_ELEMENT( SQL_POSITIONED_STATEMENTS)
	ID2STRINGMAP_ELEMENT( SQL_NEED_LONG_DATA_LEN)
	ID2STRINGMAP_ELEMENT( SQL_QUALIFIER_USAGE)
	ID2STRINGMAP_ELEMENT( SQL_NON_NULLABLE_COLUMNS)
	ID2STRINGMAP_ELEMENT( SQL_CONCAT_NULL_BEHAVIOR)
	ID2STRINGMAP_ELEMENT( SQL_GROUP_BY)
	ID2STRINGMAP_ELEMENT( SQL_MAX_ROW_SIZE_INCLUDES_LONG)
	ID2STRINGMAP_ELEMENT( SQL_PROCEDURE_TERM)
	ID2STRINGMAP_ELEMENT( SQL_QUOTED_IDENTIFIER_CASE)
	ID2STRINGMAP_ELEMENT( SQL_SUBQUERIES)
	ID2STRINGMAP_ELEMENT( SQL_TABLE_TERM)
	ID2STRINGMAP_ELEMENT( SQL_MULT_RESULT_SETS)
	ID2STRINGMAP_ELEMENT( SQL_OJ_CAPABILITIES)
	ID2STRINGMAP_ELEMENT( SQL_PROCEDURES)
	ID2STRINGMAP_ELEMENT( SQL_LIKE_ESCAPE_CLAUSE)
	ID2STRINGMAP_ELEMENT( SQL_OUTER_JOINS)
	ID2STRINGMAP_ELEMENT( SQL_STRING_FUNCTIONS)
	ID2STRINGMAP_ELEMENT( SQL_SYSTEM_FUNCTIONS)
	ID2STRINGMAP_ELEMENT( SQL_TIMEDATE_FUNCTIONS)
	ID2STRINGMAP_ELEMENT( SQL_FILE_USAGE)
	ID2STRINGMAP_ELEMENT( SQL_DRIVER_ODBC_VER)
	ID2STRINGMAP_ELEMENT( SQL_TXN_CAPABLE)
	ID2STRINGMAP_ELEMENT( SQL_GETDATA_EXTENSIONS)
	ID2STRINGMAP_ELEMENT( SQL_SCROLL_CONCURRENCY)
	ID2STRINGMAP_ELEMENT( SQL_MAX_CONCURRENT_ACTIVITIES)
	ID2STRINGMAP_ELEMENT( SQL_MAX_DRIVER_CONNECTIONS)
	ID2STRINGMAP_ELEMENT( SQL_ODBC_API_CONFORMANCE)
	ID2STRINGMAP_ELEMENT( SQL_ODBC_SQL_CONFORMANCE)
	ID2STRINGMAP_ELEMENT( SQL_ODBC_INTERFACE_CONFORMANCE)
	ID2STRINGMAP_ELEMENT( SQL_KEYWORDS) 
	ID2STRINGMAP_ELEMENT( SQL_DATA_SOURCE_READ_ONLY)
	ID2STRINGMAP_ELEMENT( SQL_BOOKMARK_PERSISTENCE)
	ID2STRINGMAP_ELEMENT( SQL_FETCH_DIRECTION)
	ID2STRINGMAP_ELEMENT( SQL_SCHEMA_TERM)
	ID2STRINGMAP_ELEMENT( SQL_IDENTIFIER_CASE)
	ID2STRINGMAP_ELEMENT( SQL_DATABASE_NAME)
	ID2STRINGMAP_ELEMENT( SQL_DRIVER_NAME)
	ID2STRINGMAP_ELEMENT( SQL_DRIVER_VER)
	ID2STRINGMAP_ELEMENT( SQL_DBMS_NAME)
	ID2STRINGMAP_ELEMENT( SQL_DBMS_VER)
	ID2STRINGMAP_ELEMENT( SQL_DATA_SOURCE_NAME)
	ID2STRINGMAP_ELEMENT( SQL_USER_NAME)
	ID2STRINGMAP_ELEMENT( SQL_SERVER_NAME)
ID2STRINGMAP_END()

ID2STRINGMAP_BEGIN(Returncode2Char)
	ID2STRINGMAP_ELEMENT(SQL_SUCCESS)
	ID2STRINGMAP_ELEMENT(SQL_NO_DATA_FOUND)
	ID2STRINGMAP_ELEMENT(SQL_NEED_DATA)
	ID2STRINGMAP_ELEMENT(SQL_INVALID_HANDLE)
	ID2STRINGMAP_ELEMENT(SQL_SUCCESS_WITH_INFO)
	ID2STRINGMAP_ELEMENT(SQL_ERROR)
	ID2STRINGMAP_ELEMENT(SQL_STILL_EXECUTING)
ID2STRINGMAP_END()

ID2STRINGMAP_BEGIN(TracerType2SQLType)
	ID2STRINGMAP_ELEMENT2(TYP_UNKNOWN, "unknown type")
	ID2STRINGMAP_ELEMENT2(TYP_SQLPOINTER, "SQLPOINTER")
	ID2STRINGMAP_ELEMENT2(TYP_SQLSMALLINT, "SQLSMALLINT")
	ID2STRINGMAP_ELEMENT2(TYP_SQLUSMALLINT, "SQLUSMALLINT")
	ID2STRINGMAP_ELEMENT2(TYP_SQLINTEGER, "SQLINTEGER")
	ID2STRINGMAP_ELEMENT2(TYP_SQLUINTEGER, "SQLUINTEGER")
	ID2STRINGMAP_ELEMENT2(TYP_SQLPOINTER_PTR, "SQLPOINTER*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLWCHAR_PTR, "SQLWCHAR*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLCHAR_PTR, "SQLCHAR*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLSMALLINT_PTR, "SQLSMALLINT*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLUSMALLINT_PTR, "SQLUSMALLINT*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLINTEGER_PTR, "SQLINTEGER*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLUINTEGER_PTR, "SQLUINTEGER*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHDESC, "SQLHDESC")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHSTMT, "SQLHSTMT")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHDBC, "SQLHDBC")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHWND, "SQLHWND")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHENV, "SQLHENV")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHANDLE, "SQLHANDLE")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHDESC_PTR, "SQLHDESC*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHSTMT_PTR, "SQLHSTMT*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHDBC_PTR, "SQLHDBC*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHWND_PTR, "SQLHWND*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHENV_PTR, "SQLHENV*")
	ID2STRINGMAP_ELEMENT2(TYP_SQLHANDLE_PTR, "SQLHANDLE*")
ID2STRINGMAP_END()

//We better dump the errors ourselves in case of applications not handling them...
bool disable_all_tracing = false;

//------------------------------------------------------------------------------------------

WinTrace * traces (void)
{
   return ODBCTraceDialog::getUniqueInstance()->ODBCWinTrace ;
}

//------------------------------------------------------------------------------------------

void ODBCTraceErrors(ODBCTraceCall *call)
{
	SQLSMALLINT handle_type = 0;

	switch(call->function_id)
	{
	case SQL_API_SQLALLOCCONNECT:
	case SQL_API_SQLFREEENV:
	case SQL_API_SQLGETENVATTR:
	case SQL_API_SQLSETENVATTR:
		handle_type = SQL_HANDLE_ENV;
		break;

	case SQL_API_SQLGETDESCFIELD:
	case SQL_API_SQLGETDESCREC:
	case SQL_API_SQLCOPYDESC:
	case SQL_API_SQLSETDESCFIELD:
	case SQL_API_SQLSETDESCREC:
		handle_type = SQL_HANDLE_DESC;
		break;

	case SQL_API_SQLALLOCSTMT:
	case SQL_API_SQLDRIVERS:
	case SQL_API_SQLDATASOURCES:
	case SQL_API_SQLBROWSECONNECT:
	case SQL_API_SQLDRIVERCONNECT:
	case SQL_API_SQLCONNECT:
	case SQL_API_SQLDISCONNECT:
	case SQL_API_SQLFREECONNECT:
	case SQL_API_SQLGETCONNECTATTR:
	case SQL_API_SQLGETCONNECTOPTION:
	case SQL_API_SQLSETCONNECTATTR:
	case SQL_API_SQLSETCONNECTOPTION:
	case SQL_API_SQLNATIVESQL:
	case SQL_API_SQLGETINFO:
		handle_type = SQL_HANDLE_DBC;
		break;

	case SQL_API_SQLBINDCOL              :
	case SQL_API_SQLBINDPARAM         :
	case SQL_API_SQLCANCEL               :
	case SQL_API_SQLCLOSECURSOR      :
	case SQL_API_SQLCOLATTRIBUTE         :
	case SQL_API_SQLCOLUMNS         :
	case SQL_API_SQLDESCRIBECOL          :
	case SQL_API_SQLENDTRAN        :
	case SQL_API_SQLEXECDIRECT    :
	case SQL_API_SQLEXECUTE       :
	case SQL_API_SQLFETCH        :
	case SQL_API_SQLFETCHSCROLL :
	case SQL_API_SQLFREESTMT        :
	case SQL_API_SQLGETCURSORNAME    :
	case SQL_API_SQLGETDATA         :
	case SQL_API_SQLGETFUNCTIONS  :
	case SQL_API_SQLGETSTMTATTR :
	case SQL_API_SQLGETSTMTOPTION     :
	case SQL_API_SQLGETTYPEINFO      :
	case SQL_API_SQLNUMRESULTCOLS   :
	case SQL_API_SQLPARAMDATA      :
	case SQL_API_SQLPREPARE       :
	case SQL_API_SQLPUTDATA      :
	case SQL_API_SQLROWCOUNT    :
	case SQL_API_SQLSETCURSORNAME    :
	case SQL_API_SQLSETPARAM      :
	case SQL_API_SQLSETSTMTATTR     :
	case SQL_API_SQLSETSTMTOPTION   :
	case SQL_API_SQLSPECIALCOLUMNS      :
	case SQL_API_SQLSTATISTICS          :
	case SQL_API_SQLTABLES              :
	case SQL_API_SQLBULKOPERATIONS	:
	case SQL_API_SQLBINDPARAMETER    :
	case SQL_API_SQLCOLUMNPRIVILEGES :
	case SQL_API_SQLDESCRIBEPARAM    :
	case SQL_API_SQLEXTENDEDFETCH    :
	case SQL_API_SQLFOREIGNKEYS      :
	case SQL_API_SQLMORERESULTS      :
	case SQL_API_SQLNUMPARAMS        :
	case SQL_API_SQLPARAMOPTIONS     :
	case SQL_API_SQLPRIMARYKEYS      :
	case SQL_API_SQLPROCEDURECOLUMNS :
	case SQL_API_SQLPROCEDURES       :
	case SQL_API_SQLSETPOS           :
	case SQL_API_SQLSETSCROLLOPTIONS :
	case SQL_API_SQLTABLEPRIVILEGES  :
		handle_type = SQL_HANDLE_STMT;
		break;
	}

	if (handle_type != 0)
	{
		RETCODE ret;

		SQLSMALLINT RecNumber = 0;
		SQLCHAR MessageText[2048];
		SQLCHAR Sqlstate[128];
		SQLINTEGER	NativeError;
		SQLSMALLINT TextLength;

		do
		{
			//dont want to end up in recursion
			disable_all_tracing = true;

			ret = ::SQLGetDiagRec(handle_type, call->arguments[0].value, 
								++RecNumber,
								Sqlstate,
								&NativeError,
								MessageText,
								sizeof(MessageText), &TextLength);

			disable_all_tracing = false;

			if (ret == SQL_SUCCESS)
         {
            char line[20];
            sprintf_s(line, 20,"\t%d ", NativeError);

            string msg = "\tSQLGetDiagRect" ;
            msg.append ("\t").append ((char *)Sqlstate) ;
            msg.append ("\t").append ((char *)MessageText) ;
            msg.append(line) ;
            call->nodeEx->traceNode->Send (msg.c_str()) ;
         }
		}
		while(ret == SQL_SUCCESS);
	}
}

//------------------------------------------------------------------------------------------

bool IsValidAdr (void * adr)
{
   __try  {
      // check if adress is valid
      SQLCHAR noErr = *(SQLCHAR*)adr ;
      return true ;
   }
   __except (EXCEPTION_EXECUTE_HANDLER ) {
      return false ;
   }
}

//------------------------------------------------------------------------------------------

void ODBCTraceArg(ODBCTraceCall *call, int argnumber)
{
   ODBCTraceArgument *arg = &call->arguments[argnumber];
   char line[20];

   string strValue = "";
   string msg = "\t" ;
   msg.append(arg->name.c_str()) ;

   sprintf_s(line, 20,"0x%08x ", arg->value);
   strValue.append(line) ;

	switch(arg->type)
	{
	case TYP_SQLPOINTER_PTR:
	case TYP_SQLHDESC:
	case TYP_SQLHSTMT:
	case TYP_SQLHDBC:
	case TYP_SQLHWND:
	case TYP_SQLHENV:
	case TYP_SQLHANDLE:

   case TYP_SQLSMALLINT:
	case TYP_SQLUSMALLINT:
	case TYP_SQLINTEGER:
	case TYP_SQLUINTEGER:
      if (call->function_id == SQL_API_SQLGETINFO && argnumber == 1) 
         strValue.append("(").append(GetInfoType2Char((int)arg->value)).append(")");
      break;

	case TYP_SQLWCHAR_PTR:
		if (arg->value)
		{
			const size_t lnbuf = wcslen((SQLWCHAR*)arg->value);
			char *buffer = new char[ lnbuf + 1 ];
         size_t nbChar ;
         wcstombs_s(&nbChar, buffer, lnbuf+1, (SQLWCHAR*)arg->value, lnbuf );
			buffer[ lnbuf ] = '\0';
	      strValue.append("(").append(buffer).append(")") ;
			delete [] buffer;
      } else {
	      strValue.append("(Nullpointer)") ;
      }
		break;

	case TYP_SQLCHAR_PTR:
		if (arg->value)
	      strValue.append("(").append((char *)arg->value).append(")") ;
		else
	      strValue.append("(Nullpointer)") ;
		break;

	case TYP_SQLSMALLINT_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLSMALLINT*)arg->value));
         strValue.append(line) ;
      } else
	      strValue.append("(Nullpointer)") ;
		break;

	case TYP_SQLUSMALLINT_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLUSMALLINT*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
      break;

	case TYP_SQLINTEGER_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLINTEGER*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
      break;

   case TYP_SQLUINTEGER_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLUINTEGER*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
      break;

	case TYP_SQLPOINTER:
		//let's see what's inside...
      if (arg->value && IsValidAdr (arg->value))
      {
         sprintf_s(line, 20,"0x%02x", *((SQLCHAR*)arg->value));
         strValue.append("(*SQLCHAR* = ").append (line) ;

         sprintf_s(line, 20,"0x%04x", *((SQLUSMALLINT*)arg->value));
         strValue.append(",*SQLUSMALLINT* = ").append (line) ;

         sprintf_s(line, 20,"0x%08x", *((SQLUINTEGER*)arg->value));
         strValue.append(",*SQLUINTEGER* = ").append (line) ;

         if (call->unicode)
         {
            char buffer[256];
            size_t nbChar ;
            wcstombs_s(&nbChar, buffer, 256, (SQLWCHAR*)arg->value, sizeof(buffer) );

            strValue.append(",WString = ").append (buffer) ;
         }
         else
            strValue.append(",String = ").append ((char*)arg->value);
         strValue.append(")") ;
      } else
         strValue.append("(Nullpointer)") ;
		break;

   case TYP_SQLHDESC_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLHDESC*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
      break;

      
	case TYP_SQLHSTMT_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLHSTMT*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
		break;

   case TYP_SQLHDBC_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLHDBC*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
		break;

	case TYP_SQLHWND_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLHWND*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
		break;

	case TYP_SQLHENV_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLHENV*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
		break;

	case TYP_SQLHANDLE_PTR:
      if (arg->value) {
         sprintf_s(line, 20,"(0x%08x)", *((SQLHANDLE*)arg->value));
         strValue.append(line) ;
      } else
         strValue.append("(Nullpointer)") ;
		break;

	case TYP_UNKNOWN:
	default:
      strValue.append("Unknown argument type") ;
	}

   msg.append ("\t").append(TracerType2SQLType(arg->type)) ; 
   msg.append ("\t").append(strValue.c_str());

   call->nodeEx->traceNode->Send (msg.c_str()) ;
}

//------------------------------------------------------------------------------------------

void ODBCTraceEnter(ODBCTraceCall *call)
{
	if (disable_all_tracing) return;

	ODBCTraceOptions *option = ODBCTraceOptions::getUniqueInstance();

	//log the functions of interest...
	if (option->logFunction(call->function_id))
	{
		char	szProcName[MAX_PATH];
		_splitpath_s(GetCommandLine(), NULL,  0, NULL, 0, szProcName, MAX_PATH, NULL,0);  // fullpath, drive, drivesize, dir, dirsize, file, filesize, ext, extsize

      DWORD ProcId = GetCurrentProcessId();
      DWORD ThId = GetCurrentThreadId();

      SYSTEMTIME Time;
      char buffer [MAX_PATH] ;
      GetLocalTime(&Time);

      sprintf_s(buffer, MAX_PATH,"%02d:%02d:%02d:%03d %s (%d-%d)\t%s", 
         Time.wHour, Time.wMinute, Time.wSecond, Time.wMilliseconds,
         szProcName, ProcId,ThId,call->function_name.c_str());

      call->nodeEx = traces()->Debug()->CreateChildEx(buffer) ; // str.c_str()
      call->nodeEx->Send() ;
      
		//arguments
		for (int i = 0; i < call->arguments_count; i++)
			ODBCTraceArg(call, i);

	}
}

//------------------------------------------------------------------------------------------

void ODBCTraceExit(ODBCTraceCall *call)
{
	if (disable_all_tracing) return;

	ODBCTraceOptions *option = ODBCTraceOptions::getUniqueInstance();

	//log the functions of interest...
	if (option->logFunction(call->function_id))
	{
      SYSTEMTIME Time;
      char buffer [MAX_PATH] ;
      GetLocalTime(&Time);

      sprintf_s(buffer, MAX_PATH,"%02d:%02d:%02d:%03d\tExit with return code %d (%s)",
         Time.wHour, Time.wMinute, Time.wSecond, Time.wMilliseconds,
         call->retcode,Returncode2Char(call->retcode)) ;

      //call->nodeEx->traceNode->Append (buffer);
      call->nodeEx->traceNode->Send (buffer) ;

      //arguments
		for (int i = 0; i < call->arguments_count; i++)
			ODBCTraceArg(call, i);

      //Are there any errors?
      if (call->retcode == SQL_ERROR || call->retcode == SQL_SUCCESS_WITH_INFO)
         ODBCTraceErrors(call);

      delete call->nodeEx ;        
	}
}

