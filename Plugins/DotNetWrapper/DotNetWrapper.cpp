// Dot net wrapper.cpp 
// This dll must be installed at the same place as the viewer.
//
// Author : Thierry Parent
// Version : 12.3
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information 

//#include "stdafx.h"
#include <string.h>
#include <fstream>
#include "DotNetWrapper.h" 
//#include <windows.h>

//------------------------------------------------------------------------------

/// <summary>
/// Utility function : concat a Managed String to a C string buffer 
/// </summary>
/// <param name="dest">target buffer. The size is limited to 1024 chars </param>
/// <param name="source">source </param>

void strcat(char* dest, String^ source)
{
    unsigned SourceLen = source->Length;
    unsigned DestLen = strlen(dest);

    if ((DestLen + SourceLen) > 1024)   // limit all strings to 1024 chars max
        SourceLen = 1024 - DestLen;

    array<Char>^ SourceChars = source->ToCharArray();

    unsigned int i;
    for (i = 0; i < SourceLen; i++)
        dest[DestLen + i] = (char)SourceChars[i];

    dest[DestLen + i] = '\0';
}

//------------------------------------------------------------------------------

extern "C"
{
    //AppDomain^ LocalAppDomain;



    __declspec(dllexport) void __cdecl test(unsigned PlugId, char* strException)
    {
        Singleton::trace("CppTest()\n");
        //strcat (strException ,  "Inside CppTest") ;
    }

    //---------------------------------------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl CheckPlugInFile(unsigned PlugId, char* FileName, char* PlugName, char* strException)
    {
        System::Object^ key = PlugId;
        String^ strFileName = gcnew String(FileName);
        AppDomain^ domain;

        CppPluginLoader^ Loader;
        String^ Path;

        //Singleton::trace ("wrapper : CheckPlugInFile(PLugin:<" + key + ">,FileName:<" + strFileName + ">\n");

        if (Singleton::PlugList->ContainsKey(key) == true)
        {
            Singleton::trace("wrapper : CheckPlugInFile : FileName : " + strFileName + "\n");
            Singleton::trace("wrapper : CheckPlugInFile : PLugin " + key + " is already loaded.\n");
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : CheckPlugInFile : PLugin ")->Append(key)->Append(" is already loaded.");
            strcat(strException, sb->ToString());
            return;
        }

        // create a domain for the plugin
        //-------------------------------

        try {
            domain = AppDomain::CreateDomain("PlugDomain"); // , nullptr, domainSetup);
            //Singleton::trace("wrapper : CheckPlugInFile : domain created " + "\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : CheckPlugInFile : FileName : " + strFileName + "\n");
            Singleton::trace("wrapper : CheckPlugInFile : CreateDomain exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return;
        }

        // load the loader in the created domain
        // https://docs.microsoft.com/en-us/dotnet/api/system.marshalbyrefobject?view=netframework-4.8
        // https://weblog.west-wind.com/posts/2009/Jan/19/Assembly-Loading-across-AppDomains
        //----------------------------------------

        try {
            Path = Assembly::GetAssembly(CppPluginLoader::typeid)->Location;
            //Singleton::trace("wrapper : CheckPlugInFile : Path : " + Path + "\n");

            Loader = (CppPluginLoader^)(domain->CreateInstanceFromAndUnwrap(Path, CppPluginLoader::typeid->FullName));  // "CppPluginLoader"
            //Singleton::trace("wrapper : CheckPlugInFile : Loader created " + "\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : CheckPlugInFile : FileName : " + strFileName + "\n");
            Singleton::trace("wrapper : CheckPlugInFile : CreateInstanceFromAndUnwrap exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return;
        }

        Loader->SetDomain(domain);

        // call Loader.CheckPlugInFile
        //----------------------------
        try {
            Loader->CheckPlugInFile(strFileName);
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : CheckPlugInFile : FileName : " + strFileName + "\n");
            Singleton::trace("wrapper : Loader->CheckPlugInFile exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            Loader = nullptr;
            AppDomain::Unload(domain);
            return;
        }

        try {
            strcat(PlugName, Loader->GetPlugName());
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : get loader name exception : " + ex->Message + "\n");
            strcat(strException, "wrapper : CheckPlugInFile : ");
            strcat(strException, strFileName);
            strcat(strException, ex->Message);
            return;
        }
              
        Loader->SetStatus(Loaded); //PluginStatus::Loaded ;

        // add to plugin list
        Singleton::PlugList->Add(key, Loader);

        //Singleton::trace ("wrapper : CheckPlugInFile : done\n" ) ;
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl Start(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : Start : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->GetStatus() == Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : Start : PLugin ")->Append(key)->Append(" is already started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        if (Loader->GetStatus() != Loaded) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper.Start : PLugin ")->Append(key)->Append(" is not loaded.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        try {
            //Singleton::trace("wrapper : Start : begin\n");
            Loader->StartPlugin(); 
            //Singleton::trace("wrapper : Start : end\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : Start : Loader->StartPlugin() exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return;
        }

        Loader->SetStatus(Started);
        strcat(strException, "OK");
    }

    //------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl Stop(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : Stop : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->GetStatus() != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : Stop : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        try {
            //Singleton::trace("wrapper : Stop : before stop plugin\n");
            Loader->StopPlugin();
            //Singleton::trace("wrapper : Stop : after stop plugin, update status\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : Stop() : delegate_Stop exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return;
        }

        Loader->SetStatus(Loaded);
        strcat(strException, "OK");
    }

    //------------------------------------------------------------------------------

    __declspec(dllexport) int __cdecl OnAction(unsigned PlugId, char* WinId, unsigned ResourceId, char* NodeId, char* strException)
    {
        System::Object^ key = PlugId;
        String^ strWinId = gcnew String(WinId);
        String^ strNodeId = gcnew String(NodeId);
        
        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : OnAction : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->GetStatus() != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Dot net wrapper.OnAction : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        int result = true;
        try {
            //Singleton::trace("wrapper : OnAction : before call action\n");
            result = Loader->OnAction(strWinId, ResourceId, strNodeId);
            //Singleton::trace("wrapper : OnAction : after call action\n");
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("wrapper : OnAction : delegate_OnAction exception : " + ex->Message + "\n");
            return true;
        }
        strcat(strException, "OK");
        return result;
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) int __cdecl OnBeforeDelete(unsigned PlugId, char* WinId, char* NodeId, char* strException)
    {
        System::Object^ key = PlugId;
        String^ strWinId = gcnew String(WinId);
        String^ strNodeId = gcnew String(NodeId);

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : OnBeforeDelete : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->GetStatus() != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper.OnBeforeDelete : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        try {
            //Singleton::trace("wrapper : OnBeforeDelete : before call delete\n");
            return Loader->OnBeforeDelete(strWinId, strNodeId);
            //Singleton::trace("wrapper : OnBeforeDelete : after call delete\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("wrapper : OnBeforeDelete : delegate_OnBeforeDelete exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return true;
        }
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) void __cdecl OnTimer(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : OnTimer : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->GetStatus() != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : OnTimer : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        try {
            //Singleton::trace("wrapper : OnTimer : before call OnTimer\n");
            Loader->OnTimer();
            //Singleton::trace("wrapper : OnTimer : after call OnTimer\n");
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("wrapper : OnTimer : delegate_OnTimer exception : " + ex->Message + "\n");
            return;
        }
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) void __cdecl Unload(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : Unload : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        // stop before unload
        if (Loader->GetStatus() == Started) {
            try {
                //Singleton::trace("wrapper : Unload : before call StopPlugin\n");
                Loader->StopPlugin();
                //Singleton::trace("wrapper : Unload : after call StopPlugin\n");
            }
            catch (Exception ^ ex) {
                strcat(strException, ex->Message);
                Singleton::trace("wrapper : Unload : delegate_Stop exception : " + ex->Message + "\n");
                return;
            }
        }

        if (Loader->GetStatus() != Loaded) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("wrapper : Unload : PLugin ")->Append(key)->Append(" is not loaded'.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // unload domain
        try {
            AppDomain^ domain = Loader->GetDomain();
            AppDomain::Unload(domain);
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);

            Singleton::trace("wrapper : Unload : Unload domain exception : " + ex->Message + "\n");
            return;
        }
        //Singleton::trace("wrapper : Unload : before call Unload\n");
        Loader->UnloadPlugin();
        //Singleton::trace("wrapper : Unload : after call Unload\n");
        Loader->SetDomain(nullptr);
        Loader->SetStatus (Unloaded);
        Singleton::PlugList->Remove(key);

        Loader = nullptr;
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------



}  // extern "C"
