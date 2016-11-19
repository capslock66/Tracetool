// ClipBrdHistory.cpp : Defines the initialization routines for the DLL.
//

// General : use of MFC : Use MFC in a Static Library
// C++ : Additional Include directories : tracetool folder : ../Source
// C++ : Basic Runtime check : both
// C++ : runtime library : Multi-threaded Debug (/MTd)

#include "stdafx.h"
#include "winuser.h"
#include "ClipBrdHistory.h"
#include "tracetool.h"
#include ".\ClipBrdHistory.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#endif

// CClipBrdHistoryApp

BEGIN_MESSAGE_MAP(CClipBrdHistoryApp, CWinApp)
END_MESSAGE_MAP()


// CClipBrdHistoryApp construction

CClipBrdHistoryApp::CClipBrdHistoryApp()
{
}


//-------------------------------------------------------------------------

// Application variables

CClipBrdHistoryApp theApp;
char PlugName[] = "ClipBoardHistory" ;
char ClassName [] = "CLIPBOARDCLS" ;
WinTrace * PlugTraces  ;
HWND hwndNextViewer; 
HWND hClipboardWnd ;
bool IsPaused = false ;

//-------------------------------------------------------------------------

struct CClipFormatData { UINT uFormat; LPCTSTR szFormatName; };

CClipFormatData g_aClipData[] = {
   { CF_TEXT,            _T("CF_TEXT") },
   { CF_BITMAP,          _T("CF_BITMAP") },
   { CF_METAFILEPICT,    _T("CF_METAFILEPICT") },
   { CF_SYLK,            _T("CF_SYLK") },
   { CF_DIF,             _T("CF_DIF") },
   { CF_TIFF,            _T("CF_TIFF") },
   { CF_OEMTEXT,         _T("CF_OEMTEXT") },
   { CF_DIB,             _T("CF_DIB") },
   { CF_DIBV5,           _T("CF_DIBV5") },
   { CF_PALETTE,         _T("CF_PALETTE") },
   { CF_PENDATA,         _T("CF_PENDATA") },
   { CF_RIFF,            _T("CF_RIFF") },
   { CF_WAVE,            _T("CF_WAVE") },
   { CF_UNICODETEXT,     _T("CF_UNICODETEXT") },
   { CF_ENHMETAFILE,     _T("CF_ENHMETAFILE") },
   { CF_HDROP,           _T("CF_HDROP") },
   { CF_LOCALE,          _T("CF_LOCALE") },
   { CF_OWNERDISPLAY,    _T("CF_OWNERDISPLAY") },
   { CF_DSPTEXT,         _T("CF_DSPTEXT") },
   { CF_DSPBITMAP,       _T("CF_DSPBITMAP") },
   { CF_DSPMETAFILEPICT, _T("CF_DSPMETAFILEPICT") },
   { CF_DSPENHMETAFILE,  _T("CF_DSPENHMETAFILE") },
   { 0, _T("") } 
};

//-------------------------------------------------------------------------

void AddDataBlock (TraceNodeEx * clipTrace , char * format , HGLOBAL hgData, const SIZE_T uSize )
{
   LPVOID pvData;
   unsigned MaxByte ;
   TCHAR temp [100] ; 

   MaxByte = (unsigned) uSize ;
   if (MaxByte > 1000)
      MaxByte = 1000 ;

   // Get a real pointer to the data.
   pvData = GlobalLock ( hgData );

   // Use the given trace node to add dump
   sprintf (temp, "%s . %d byte(s)", format , (unsigned) uSize) ;
   clipTrace->AddDump (temp, (char *) pvData , 0 , MaxByte) ;

   // Unlock the HGLOBAL.
   GlobalUnlock ( hgData );
}

//-------------------------------------------------------------------------

void WINAPI ShowClipboardContent(void)  // HWND hwnd) 
{ 
   LPSTR lpstr; 
   HGLOBAL hglb; 
   UINT uFormat = (UINT)(-1); 
   TraceNodeEx * clipTrace ;

   static UINT auPriorityList[] = 
   { 
      CF_TEXT ,
      CF_UNICODETEXT,
      CF_OEMTEXT,
      49307,   // Rich Text Format 
      CF_DIB ,
      CF_DIBV5 ,
      CF_METAFILEPICT,
      CF_ENHMETAFILE,
      CF_HDROP
   }; 

   uFormat = GetPriorityClipboardFormat(auPriorityList, 9); 

   //create an empty node, setting the left part to the text .
   // using a loop display the different formats in the clipboard as a dump

   clipTrace = new TraceNodeEx (PlugTraces->Debug()) ;

   switch (uFormat) 
   { 
   case CF_UNICODETEXT :
   case CF_TEXT: 
   case CF_OEMTEXT :
   case CF_HDROP:
      if (OpenClipboard(NULL))   // hwnd
      { 
         hglb = GetClipboardData(uFormat); 
         lpstr = (char*)GlobalLock(hglb); 

         //GetClientRect(hwnd, &rc); 

         if (uFormat == CF_UNICODETEXT) {
            size_t len = wcslen( (wchar_t *)lpstr) ;
            char  *pmb = (char *)malloc(len+1);

            size_t s = wcstombs  (pmb,(wchar_t *)lpstr,  len+1) ;
            clipTrace->leftMsg = pmb ;
            free (pmb) ;
         } else if (uFormat == CF_HDROP) {
            int count, i, bufferSize ;
            clipTrace->leftMsg = "<File list>" ;

            //First call to know number of files
            count = DragQueryFile((HDROP) hglb, 0xFFFFFFFF, NULL, 0);

            TMemberNode * members ;
            members = clipTrace->Members()->Add ("File list") ;
            for (i=0; i < count ; i++)
            {
               // First call to know buffer size to allocate
               bufferSize = DragQueryFile((HDROP)hglb, i, NULL, 0);
               char  *Filename = (char *)malloc(bufferSize+1);
            
               // get file name
               DragQueryFile((HDROP)hglb, i, Filename, bufferSize + 1);
               members->Add (Filename) ;
               free (Filename) ;

            }
         } else {
            clipTrace->leftMsg = lpstr ;
         }

         GlobalUnlock(hglb); 
         CloseClipboard(); 
      } 
      break; 

   case CF_DIB :
   case CF_DIBV5 :
   case CF_METAFILEPICT:
   case CF_ENHMETAFILE:
      clipTrace->leftMsg = "<Bitmap>" ;
      break;

   case 0: 
      clipTrace->leftMsg = "<Empty>" ;
      break; 

   default: 
      clipTrace->leftMsg = "<not text format>" ;
      break;

   } 

   COleDataObject xDataObj;

   if ( !xDataObj.AttachClipboard() )
   {
      clipTrace->Send() ;
      return;
   }

   FORMATETC        etc;
   CClipFormatData* pClip;
   TCHAR            szFormat[256];     // format name
   //TCHAR            FormatCode [20] ;

   // Get all the data and pass it to the doc for storage.

   xDataObj.BeginEnumFormats() ;

   while ( xDataObj.GetNextFormat ( &etc ))
   {
      //sprintf (FormatCode, " (%d)", etc.cfFormat) ; 

      // See if this is a built-in clipboard format.  If so, we already have
      // a description string for it - we just have to find it in the 
      // g_aClipData array.
      for ( pClip = &g_aClipData[0]; 0 != pClip->uFormat; pClip++ )
      {
         if ( etc.cfFormat == pClip->uFormat )
         {
            lstrcpy ( szFormat, pClip->szFormatName );
            //lstrcat ( szFormat, FormatCode) ;
            break;
         }
      }

      // If we didn't find the format in g_aClipData, then it's a custom
      // format, and we need to get the name from Windows.
      if (pClip->uFormat == 0)
      {
         GetClipboardFormatName ( etc.cfFormat, szFormat, 256 );
         //lstrcat ( szFormat, FormatCode) ;
      }

      HGLOBAL hgData;
      SIZE_T  uDataSize;

      // Get an HGLOBAL of the data.
      hgData = xDataObj.GetGlobalData ( etc.cfFormat );

      if ( NULL != hgData )
      {
         uDataSize = GlobalSize ( hgData );
         AddDataBlock (clipTrace,szFormat, hgData, uDataSize ); 
         // Free the memory that GetGlobalData() allocated for us.
         GlobalFree ( hgData );
      }
      else
      {
         // The data isn't in global memory, so try getting an IStream 
         // interface to it.
         STGMEDIUM stg;

         if ( xDataObj.GetData ( etc.cfFormat, &stg ) )
         {
            switch ( stg.tymed )
            {
            case TYMED_HGLOBAL:
               {
                  uDataSize = GlobalSize ( stg.hGlobal );
                  AddDataBlock (clipTrace, szFormat, stg.hGlobal, uDataSize );
               }
               break;

            case TYMED_ISTREAM:
               {
                  LARGE_INTEGER li;
                  ULARGE_INTEGER uli;

                  li.HighPart = li.LowPart = 0;

                  if ( SUCCEEDED( stg.pstm->Seek ( li, STREAM_SEEK_END, &uli )))
                  {
                     HGLOBAL hg = GlobalAlloc ( GMEM_MOVEABLE | GMEM_SHARE, uli.LowPart );
                     void* pv = GlobalLock ( hg );

                     stg.pstm->Seek ( li, STREAM_SEEK_SET, NULL );

                     if ( SUCCEEDED( stg.pstm->Read ( pv, uli.LowPart, (PULONG) &uDataSize )))
                     {
                        GlobalUnlock ( hg );

                        AddDataBlock (clipTrace, szFormat, hg, uDataSize );
                        // Free the memory we just allocated.
                        GlobalFree ( hg );
                     }
                     else
                     {
                        GlobalUnlock ( hg );
                     }
                  }
               }
               break;  // case TYMED_ISTREAM
            }
            ReleaseStgMedium ( &stg );
         }
      }
   }   // end while
   clipTrace->Send() ;
}

//-------------------------------------------------------------------------

LRESULT APIENTRY ClipBoardWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) 
{ 
   switch (uMsg) 
   { 
   case WM_DESTROY:       
      ChangeClipboardChain(hwnd, hwndNextViewer); 
      return (LRESULT) NULL ; 
   case WM_CHANGECBCHAIN: 
      // If the next window is closing, repair the chain. 

      if ((HWND) wParam == hwndNextViewer) 
         hwndNextViewer = (HWND) lParam; 

      // Otherwise, pass the message to the next link. 

      else if (hwndNextViewer != NULL) 
         SendMessage(hwndNextViewer, uMsg, wParam, lParam); 

      return (LRESULT) NULL ; 

   case WM_DRAWCLIPBOARD: 
      // Update the window by using Auto clipboard format. 
      if (IsPaused == false)
         ShowClipboardContent () ; // (hwnd); 

      // Pass the message to the next window in clipboard 
      // viewer chain. 

      if (hwndNextViewer != 0)
         SendMessage(hwndNextViewer, uMsg, wParam, lParam); 
      break ; // clipboard contents changed. 

   }
   return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

//-------------------------------------------------------------------------


void PrintLastError (char * title)
{
   LPVOID lpMsgBuf;
   if (!FormatMessage( 
      FORMAT_MESSAGE_ALLOCATE_BUFFER | 
      FORMAT_MESSAGE_FROM_SYSTEM | 
      FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
      (LPTSTR) &lpMsgBuf,
      0,
      NULL ))
   {
      // Handle the error.
      return;
   }
   // Display the string.
   if (PlugTraces != NULL)
      PlugTraces->Error()->Send (title,(char *)lpMsgBuf) ;
   else
      TTrace::Error()->Send (title,(char *)lpMsgBuf) ;

   // Free the buffer.
   LocalFree( lpMsgBuf );
}

//-------------------------------------------------------------------------

// CClipBrdHistoryApp initialization

BOOL CClipBrdHistoryApp::InitInstance()
{
   CWinApp::InitInstance();
   return TRUE;
}

//-------------------------------------------------------------------------

int CClipBrdHistoryApp::ExitInstance()
{
   return CWinApp::ExitInstance();
}

//-------------------------------------------------------------------------


extern "C"
{

   //-------------------------------------------------------------------------

   // Get the plugin name 
   __declspec(dllexport) void  __cdecl  GetPlugName (char * lpPlugName)  
   {
      strcpy (lpPlugName, PlugName) ;
   }

   //-------------------------------------------------------------------------

   //Initialise the plugin. Called after TraceTool load the plugin
   __declspec(dllexport) void __cdecl Start() 
   {

      PlugTraces = new WinTrace("CLIPBH" , "Clipboard History") ;
      PlugTraces->DisplayWin() ;

      // attach the window to the plugin (itself)
      PlugTraces->LinkToPlugin (PlugName, CST_PLUG_ONACTION) ; 

      // add a button on right (100 pixels)
      PlugTraces->CreateResource (103,CST_RES_BUT_RIGHT,100,"Show last");

      //----

      WNDCLASSEX wcex;
      wcex.cbSize          = sizeof(WNDCLASSEX); 
      wcex.style			   = WS_OVERLAPPED  ; 
      wcex.lpfnWndProc	   = (WNDPROC)ClipBoardWndProc;
      wcex.cbClsExtra	   = 0;
      wcex.cbWndExtra	   = 0;
      wcex.hInstance		   = theApp.m_hInstance;
      wcex.hIcon			   = 0;
      wcex.hCursor		   = ::LoadCursor(NULL, IDC_ARROW);
      wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
      wcex.lpszMenuName	   = ClassName;
      wcex.lpszClassName	= ClassName;
      wcex.hIconSm		   = 0 ;

      SetLastError(S_OK);
      ATOM classEx = RegisterClassEx(&wcex);
      if (!classEx)
         PrintLastError("RegisterClassEx failed") ;

      SetLastError(S_OK);
      hClipboardWnd = CreateWindow(
         ClassName,                 //   LPCTSTR lpClassName,
         "",                        //   LPCTSTR lpWindowName,
         WS_OVERLAPPEDWINDOW,       //   DWORD dwStyle,
         0 ,                        //   int x,
         0 ,                        //   int y,
         0 ,                        //   int nWidth,
         0,                         //   int nHeight,
         ((HWND)-3),                //   HWND hWndParent : HWND_MESSAGE    
         NULL,                      //   HMENU hMenu,
         theApp.m_hInstance,               //   HINSTANCE hInstance,
         NULL);                     //   LPVOID lpParam

      if (!hClipboardWnd)
         PrintLastError("CreateWindow failed") ;

      hwndNextViewer = SetClipboardViewer(hClipboardWnd); 

   }

   //-------------------------------------------------------------------------

   //Stop the plugin (free any resources before unloading)
   __declspec(dllexport) void __cdecl Stop() 
   {
      ChangeClipboardChain(hClipboardWnd, hwndNextViewer); 

      if (DestroyWindow (hClipboardWnd)  == 0)
         PrintLastError("DestroyWindow failed") ;

      if (UnregisterClass (ClassName, theApp.m_hInstance) == 0)
         PrintLastError("UnregisterClass failed") ;

      if (PlugTraces == NULL)  // plugin stopped
         return ;

      delete PlugTraces ;
      PlugTraces = NULL;
   }

   //-------------------------------------------------------------------------

   // Called when the user click on a button, label or menu on a WinTrace. 
   // The plugin must call WinTrace.LinkToPlugin with CST_PLUG_ONACTION  in order to receive this event.
   // Parameters
   //    WinId  is the Wintrace Id
   //    ResourceId is the User created resource or a tracetool resource.See the Tracetool resources table above
   //    NodeId is the node id of the current selected trace (can be empty)
   // Return value : 
   //    when true : tracetool perform the default action 
   //    when false : tracetool don't perform any action

   __declspec(dllexport) BOOL __cdecl OnAction (char * WinId , int ResourceId ,char *  NodeId) 
   {
      if (PlugTraces == NULL)  // plugin stopped
         return true ;
      switch ( ResourceId )
      {
      case CST_ACTION_PAUSE       :   // Pause on
         IsPaused = true ;
         break ;     

      case CST_ACTION_RESUME      :   // resume from Pause
         IsPaused = false ;
         break ;      

      case 103                    :   // user defined action : "Show last"
         ShowClipboardContent() ;

      case CST_ACTION_CLOSE_WIN   :   // Close win
         return false ;                    // disable close button. Stop the plugin before closing
         break ;     
      }

      return true ;
   }

   //-------------------------------------------------------------------------

   //Called when a node is to be deleted on a WinTrace 
   //The plugin must call WinTrace.LinkToPlugin with CST_PLUG_ONBEFOREDELETE in order to receive this event.
   //WinId  is the WinTrace Id
   //NodeId is the node id of the current selected trace
   //Return value : 
   //when true : TraceTool delete the node 
   //when false : TraceTool don't delete the node
   __declspec(dllexport) BOOL __cdecl  OnBeforeDelete (char * WinId , char *  NodeId) 
   {
      return true ;
   }

   //-------------------------------------------------------------------------

   //Called every 500 ms. Can be used for example to refresh labels 
   //The plugin must call LinkToPlugin with CST_PLUG_ONTIMER in order to receive this event 
   __declspec(dllexport) void __cdecl OnTimer() 
   {
   }

   //-------------------------------------------------------------------------



}

