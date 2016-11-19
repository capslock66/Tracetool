// Basic.cpp : implementation file
//

#include "stdafx.h"
#include "VS8_PPC2003Mfc_demo.h"
#include "Basic.h"
#include "..\..\Source\tracetool.h"
#include "winsock.h"

// Basic dialog

IMPLEMENT_DYNAMIC(Basic, CPropertyPage)

Basic::Basic()
	: CPropertyPage(Basic::IDD)
{

}

Basic::~Basic()
{
}

void Basic::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(Basic, CPropertyPage)
    ON_BN_CLICKED(IDC_SAMPLE, &Basic::OnBnClickedSample)
    ON_BN_CLICKED(IDC_INDENT, &Basic::OnBnClickedIndent)
    ON_BN_CLICKED(IDC_MAINSAVETEXT, &Basic::OnBnClickedMainsavetext)
    ON_BN_CLICKED(IDC_MAINSAVETOXML, &Basic::OnBnClickedMainsavetoxml)
    ON_BN_CLICKED(IDC_MAINLOADXML, &Basic::OnBnClickedMainloadxml)
    ON_BN_CLICKED(IDC_SHOWVIEWER, &Basic::OnBnClickedShowviewer)
    ON_BN_CLICKED(IDC_MAINCLEAR, &Basic::OnBnClickedMainclear)
    ON_BN_CLICKED(IDC_PARTNER, &Basic::OnBnClickedPartner)
    ON_BN_CLICKED(IDC_HOSTS, &Basic::OnBnClickedHosts)
END_MESSAGE_MAP()


// Basic message handlers


// send samples traces
void Basic::OnBnClickedSample()
{
    // Tips : ensure you "craddle" your emulator or give a correct viewer adress.
    char buffer [100] ;
    DWORD thid ; 

    // send the first trace (process name is not send)
    TTrace::Debug()->Send ("Hello");   // single byte string
    TTrace::Debug()->Send (L"C++");    // wide string

    WCHAR wFileName [MAX_PATH+1] ;      
    wFileName[GetModuleFileNameW (0 /* hInstance */ ,wFileName,MAX_PATH)] = 0; 
    TTrace::Debug()->Send (L"Process name", wFileName);    // don't mix char * and wchar_t * in the send() functions

    // force the process name to be send for further traces
    TTrace::Options()->SendProcessName = true ;

    // send some traces : current thread Id, Process ID 
    thid = GetCurrentThreadId();
    sprintf (buffer, "Thread id : %u" , thid) ;
    TTrace::Debug()->Send (buffer);                      // 1 column trace

    thid = GetCurrentProcessId() ;
    sprintf (buffer,"%u" , thid) ;
    TTrace::Warning()->Send ("process id", buffer);      // 2 columns traces

   // single separator
   TTrace::Debug()->Send("---");

   // extended Node with members
   TraceNodeEx * node = TTrace::Debug()->CreateChildEx("MyNode") ;
   node->AddFontDetail (3,true,false,255) ;                       // change the "MyNode" trace caption to bold and red
   node->AddBackgroundColor(12639424,-1) ;                        // background color : light green
   node->Members()
      ->Add ("a1" , "b1" , "c1")                        // add member              //   a1        | b1   |  c1
      ->SetFontDetail (0,true,false,255)                // a1 bold, red
      ->SetFontDetail (1,false,true)                    // b1 Italic 
      ->SetFontDetail (2,false,false,-1,15) ;           // c1 Size 15
   node->Members()
      ->Add (NULL , "b2" )                              // add member              //             | b2   |
      ->SetFontDetail (1,false,false,-1,15,"Symbol") ;  // b2 font Symbol
   node->Members()
      ->Add ("a3") ;                                    // add sub member          //   a3        |      |
   node->Members()
      ->Add ("a4" , "b4" ) ;                            // add sub member          //   a4        | b4   |
   node->Members()         
      ->Add ("a5")                                      // add member              //   a5        |      |
         ->Add ("a6")                                   // add sub member          //      a6     |      |
            ->Add ("a7", "b7") ;                        // add sub sub member      //         a7  | b7   |
   // finally send the node
   node->Send() ;
   delete node ;

   // Dump test
   TTrace::Options()->SendDate = true ;
   TTrace::Debug()->SendDump ("Dump test", NULL , "Dump" , buffer , 30) ; 	
   TTrace::Options()->SendDate = false ;

}

//--------------------------------------------------------------------------------------

// Indent and Unindent demo
void Basic::OnBnClickedIndent()
{
   // Indent and UnIndent 
   TTrace::Debug()->Indent ("Before", "some work");
   TTrace::Debug()->Indent ("Level1") ;
   TTrace::Debug()->Send ("Level2") ;
   TTrace::Debug()->Send ("More level2") ;
   TTrace::Debug()->UnIndent ("Done level 1") ;
   TTrace::Debug()->UnIndent ("Work is done") ;	
}

//--------------------------------------------------------------------------------------

// save traces to text file
void Basic::OnBnClickedMainsavetext()
{
   TTrace::WindowTrace()->SaveToTextfile("c:\\log.txt");
}

//--------------------------------------------------------------------------------------

// save traces to xml file
void Basic::OnBnClickedMainsavetoxml()
{
   TTrace::WindowTrace()->SaveToXml("c:\\log.xml");
}

//--------------------------------------------------------------------------------------

// load the xml trace into the viewer
void Basic::OnBnClickedMainloadxml()
{
   TTrace::WindowTrace()->LoadXml("c:\\log.xml");
}

//--------------------------------------------------------------------------------------

// show the viewer
void Basic::OnBnClickedShowviewer()
{
   TTrace::Show(true) ;
}

//--------------------------------------------------------------------------------------

// clear the main trace window
void Basic::OnBnClickedMainclear()
{
   TTrace::ClearAll() ;
}

//--------------------------------------------------------------------------------------

// Check partner 
void Basic::OnBnClickedPartner()
{
    HKEY hKey;
    char buffer[100];
    wchar_t key[100] ;
    DWORD Partner_id ;
    DWORD dwBufLen=100;
    LONG lRet;

    WSADATA wsaData;
    int err = WSAStartup( MAKEWORD(1,1), &wsaData );

    wsprintf (key, L"Software\\Microsoft\\Windows CE Services\\Partners") ;
    lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE, key, 0, KEY_QUERY_VALUE, &hKey );
    if( lRet != ERROR_SUCCESS )
        return ;

    // get the PCur value
    dwBufLen = 100 ;
    lRet = RegQueryValueEx( hKey, L"PCur", /*Reserved*/ NULL, /*rights*/ NULL, (LPBYTE)buffer, &dwBufLen);

    RegCloseKey( hKey );

    if( (lRet != ERROR_SUCCESS) || (dwBufLen > 100) )
        return ;

    Partner_id = * PDWORD (buffer) ;

    // open key for the current partner
    wsprintf (key, L"Software\\Microsoft\\Windows CE Services\\Partners\\P%d", Partner_id) ;
    lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE, key, 0, KEY_QUERY_VALUE, &hKey );
    if( lRet != ERROR_SUCCESS )
        return ;

    // get the PName value
    dwBufLen = 100 ;
    lRet = RegQueryValueEx( hKey, L"PName", /*Reserved*/ NULL, /*rights*/ NULL, (LPBYTE)buffer, &dwBufLen);

    RegCloseKey( hKey );

    //MessageBox (LPCWSTR (buffer), L"Active Sync Partner name") ;

    // it's your computer name used for synchronisation. The Resolved adress is the computer IP
    CheckHost ((wchar_t *) (buffer)) ; 
    WSACleanup();
}

//--------------------------------------------------------------------------------------

// Check registry to see if a network adapter is available
void Basic::OnBnClickedHosts()
{
    HKEY hKey;
    wchar_t key[100] ;
    DWORD dwBufLen=100;
    LONG lRet;

    WSADATA wsaData;
    int err = WSAStartup( MAKEWORD(1,1), &wsaData );


    // get host list
    wsprintf (key, L"Comm\\Tcpip\\Hosts") ;
    lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE, key, 0, KEY_QUERY_VALUE, &hKey );
    if( lRet != ERROR_SUCCESS )
        return ;

    DWORD index = 0; 
    wchar_t SPKey[256]; 
    DWORD SPKeyLen = 256; 
    
    // the PPP_PEER host is on that list
    while(RegEnumKeyEx(
               hKey,
               index,
               SPKey,
               &SPKeyLen, NULL, NULL, NULL, NULL) != ERROR_NO_MORE_ITEMS)
    {
        CheckHost (SPKey) ;
        index++ ;
    }

    RegCloseKey( hKey );
    WSACleanup();
}

//--------------------------------------------------------------------------------------

void Basic::CheckHost (wchar_t * Host)
{
    char socketHost [MAX_PATH] ;
    wchar_t Host_adr [MAX_PATH] ;

    wcstombs(socketHost , Host, MAX_PATH );       

    struct sockaddr_in serverSockAddr;    // Socket adress
    
    memset(&serverSockAddr,0,sizeof(serverSockAddr));

    serverSockAddr.sin_family = AF_INET; 
    // Get the address of the host... 

    // Try to convert the string as an IP address (e.g., "128.1.0.3") 
    serverSockAddr.sin_addr.S_un.S_addr = inet_addr(socketHost); 

    // If not in IP format, get the address via DSN... 
    if (serverSockAddr.sin_addr.s_addr == INADDR_NONE) 
    { 
        hostent* lphost;  

        // ...request the host address... 
        lphost = gethostbyname(socketHost); // allocated by Windows Sockets

        // ...if able to resolve the name... 
        if (lphost != NULL) 
        {
            serverSockAddr.sin_addr.S_un.S_addr = ((LPIN_ADDR)lphost->h_addr_list[0])->s_addr; 
            wsprintf (Host_adr, L"%s (%d.%d.%d.%d)", Host,
                serverSockAddr.sin_addr.S_un.S_un_b.s_b1,
                serverSockAddr.sin_addr.S_un.S_un_b.s_b2,
                serverSockAddr.sin_addr.S_un.S_un_b.s_b3,
                serverSockAddr.sin_addr.S_un.S_un_b.s_b4) ;
        } else {         // ...else name was invalid (or couldn't be resolved)...
            MessageBox(Host,L"Unable to resolve host");		
            return; 
        }
    } else {
       wcscpy (Host_adr, Host) ;
    }


    serverSockAddr.sin_port = htons(TTrace::Options()->socketPort);     // port to network port  

    // Socket creation 
    int socketHandle ;
    if ( (socketHandle = socket(AF_INET,SOCK_STREAM,0)) < 0) 
    {
        MessageBox(Host_adr,L"Couldn't create socket");		
        shutdown(socketHandle,2);   // SD_BOTH
        return;
    }

    // Open connection 
    if(connect(socketHandle,(struct sockaddr *)&serverSockAddr,	sizeof(serverSockAddr))<0)	{
        MessageBox(Host_adr,L"Couldn't connect");		
        shutdown(socketHandle,2);   // SD_BOTH
        return;
    }

    MessageBox(Host_adr,L"Connection succeed");		

    // close connection 
    shutdown(socketHandle,2);   // SD_BOTH
    closesocket(socketHandle);
}

//--------------------------------------------------------------------------------------

