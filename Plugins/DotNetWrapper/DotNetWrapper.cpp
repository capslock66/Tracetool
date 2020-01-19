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
        AppDomainSetup^ domainSetup;
        AppDomain^ domain;

        CppPluginLoader^ Loader;
        String^ Path;

        Singleton::trace ("Cpp wrapper : CheckPlugInFile()\n") ;
        Singleton::trace (" ->PlugId : " + key->ToString() + "\n") ;
        Singleton::trace (" ->FileName : " + strFileName + "\n") ;
        Singleton::trace (" ->PlugName ...\n") ;
        Singleton::trace (" ->strException ...\n") ;

        if (Singleton::PlugList->ContainsKey(key) == true)
        {
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : FileName : " + strFileName + "\n");
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : PLugin " + key + " is already loaded.\n");
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : CheckPlugInFile() : PLugin ")->Append(key)->Append(" is already loaded.");
            strcat(strException, sb->ToString());
            return;
        }

        // create a domain for the plugin
        //-------------------------------

        try {
            StringBuilder^ sb = gcnew StringBuilder();
            Guid  guid;
            CoCreateGuid(&guid);

            sb->Append("PlugDomain")->Append(Guid::NewGuid().ToString().GetHashCode().ToString("x"));
            String^ appDomain = sb->ToString();

            AppDomainSetup ^domainSetup = new AppDomainSetup();
            domainSetup.ApplicationName = appDomain;

            // *** Point at current directory
            domainSetup.ApplicationBase = Environment.CurrentDirectory;   // AppDomain.CurrentDomain.BaseDirectory;                 


            domain = AppDomain::CreateDomain("PlugDomain");
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : domain created " + "\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : FileName : " + strFileName + "\n");
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : CreateDomain exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return;
        }

        // load the loader in the created domain
        // https://docs.microsoft.com/en-us/dotnet/api/system.marshalbyrefobject?view=netframework-4.8
        // https://weblog.west-wind.com/posts/2009/Jan/19/Assembly-Loading-across-AppDomains
        //----------------------------------------

        try {
            Path = Assembly::GetAssembly(CppPluginLoader::typeid)->Location;
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : Path : " + Path + "\n");

            //Loader = safe_cast<CppPluginLoader^>(domain->CreateInstanceFromAndUnwrap(Path, CppPluginLoader::typeid->FullName));  // "CppPluginLoader"
            
            Loader = (CppPluginLoader^)(domain->CreateInstanceFromAndUnwrap(Path, CppPluginLoader::typeid->FullName));  // "CppPluginLoader"
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : Loader created " + "\n");
        }
        catch (Exception ^ ex) {
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : FileName : " + strFileName + "\n");
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : CreateInstanceFromAndUnwrap exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            return;
        }

        //Singleton::trace ("Cpp wrapper : CheckPlugInFile : domain->CreateInstanceFromAndUnwrap ok\n") ;
        Loader->domain = domain;

        // call Loader.CheckPlugInFile
        //----------------------------
        try {
            Loader->CheckPlugInFile(strFileName);
        }
        catch (Exception ^ ex) {
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : FileName : " + strFileName + "\n");
            Singleton::trace("Cpp wrapper : Loader->CheckPlugInFile exception : " + ex->Message + "\n");
            strcat(strException, ex->Message);
            Loader = nullptr;
            AppDomain::Unload(domain);
            return;
        }

        if (Loader->Plugin == nullptr) {
            Singleton::trace("Cpp wrapper : CheckPlugInFile() : FileName : " + strFileName);
            Singleton::trace(" don't contain class implementing TraceTool.ITracePLugin interface\n");
            strcat(strException, "Cpp wrapper : CheckPlugInFile : ");
            strcat(strException, strFileName);
            strcat(strException, " don't contain class implementing TraceTool.ITracePLugin interface");
            Loader = nullptr;
            AppDomain::Unload(domain);
            return;
        }

        Loader->status = Loaded; //PluginStatus::Loaded ;
        strcat(PlugName, Loader->name);

        // add to list
        // --------------
        Singleton::PlugList->Add(key, Loader);

        //Singleton::trace ("Cpp wrapper : CheckPlugInFile : done\n" ) ;
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl Start(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        //Singleton::trace ("Cpp wrapper : Start()\n") ;
        //Singleton::trace (" ->PlugId " + key->ToString() + "\n") ;
        //Singleton::trace (" ->strException\n") ;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : Start() : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->status == Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : Start() : PLugin ")->Append(key)->Append(" is already started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        if (Loader->status != Loaded) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper.Start() : PLugin ")->Append(key)->Append(" is not loaded.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        try {
            Loader->delegate_Start();
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("Cpp wrapper : Start() : Loader->delegate_Start() exception : " + ex->Message + "\n");
            return;
        }

        Loader->status = Started;
        strcat(strException, "OK");
        //Singleton::trace ("Cpp wrapper : Start() ended with no error\n") ;
    }

    //------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl Stop(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        //Singleton::trace ("Cpp wrapper : Stop()\n") ;
        //Singleton::trace (" ->PlugId " + key->ToString() + "\n") ;
        //Singleton::trace (" ->strException\n") ;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : Stop() : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->status != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : Stop() : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        try {
            Loader->delegate_Stop();
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("Cpp wrapper : Stop() : delegate_Stop exception : " + ex->Message + "\n");
            return;
        }

        Loader->status = Loaded;
        strcat(strException, "OK");
    }

    //------------------------------------------------------------------------------

    __declspec(dllexport) int __cdecl OnAction(unsigned PlugId, char* WinId, unsigned ResourceId, char* NodeId, char* strException)
    {
        System::Object^ key = PlugId;
        String^ strWinId = gcnew String(WinId);
        String^ strNodeId = gcnew String(NodeId);
        System::Object^ ResId = ResourceId;

        //Singleton::trace ("Cpp wrapper : OnAction()\n") ;
        //Singleton::trace (" ->PlugId : " + key->ToString() + "\n") ;
        //Singleton::trace (" ->WinId : " + strWinId + "\n") ;
        //Singleton::trace (" ->ResourceId : " + ResId->ToString() + "\n") ;
        //Singleton::trace (" ->NodeId : " + strNodeId + "\n") ;
        //Singleton::trace (" ->strException\n") ;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : OnAction : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->status != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Dot net wrapper.OnAction() : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        int result = true;
        try {
            result = Loader->delegate_OnAction(strWinId, ResourceId, strNodeId);
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("Cpp wrapper : OnAction() : delegate_OnAction exception : " + ex->Message + "\n");
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

        //Singleton::trace ("Cpp wrapper : OnBeforeDelete()\n") ;
        //Singleton::trace (" ->PlugId : " + key->ToString() + "\n") ;
        //Singleton::trace (" ->WinId : "  + strWinId + "\n") ;
        //Singleton::trace (" ->NodeId : " + strNodeId + "\n") ;
        //Singleton::trace (" ->strException\n") //;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : OnBeforeDelete () : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->status != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper.OnBeforeDelete() : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return true;
        }

        try {
            return Loader->delegate_OnBeforeDelete(strWinId, strNodeId);
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("Cpp wrapper : OnBeforeDelete() : delegate_OnBeforeDelete exception : " + ex->Message + "\n");
            return true;
        }
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) void __cdecl OnTimer(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        //Singleton::trace ("Cpp wrapper : OnTimer\n") ;
        //Singleton::trace (" ->PlugId : " + key->ToString() + "\n") ;
        //Singleton::trace (" ->strException\n") ;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : OnTimer () : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        if (Loader->status != Started) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : OnTimer() : PLugin ")->Append(key)->Append(" is not started.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        try {
            Loader->delegate_OnTimer();
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);
            Singleton::trace("Cpp wrapper : OnTimer() : delegate_OnTimer exception : " + ex->Message + "\n");
            return;
        }
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) void __cdecl Unload(unsigned PlugId, char* strException)
    {
        System::Object^ key = PlugId;

        //Singleton::trace ("Cpp wrapper : Unload\n") ;
        //Singleton::trace (" ->PlugId : " + key->ToString() + "\n") ;
        //Singleton::trace (" ->strException\n") ;

        // check if plugin is know
        if (Singleton::PlugList->ContainsKey(key) == false)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : Unload () : PLugin ")->Append(key)->Append(" is unknow.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // get plugin 
        CppPluginLoader^ Loader = safe_cast<CppPluginLoader^> (Singleton::PlugList->default[key]); //  get_Item(key)) ;

        // stop before unload
        if (Loader->status == Started) {
            try {
                Loader->delegate_Stop();
            }
            catch (Exception ^ ex) {
                strcat(strException, ex->Message);
                Singleton::trace("Cpp wrapper : Unload() : delegate_Stop exception : " + ex->Message + "\n");
                return;
            }
        }

        if (Loader->status != Loaded) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb->Append("Cpp wrapper : Unload() : PLugin ")->Append(key)->Append(" is not loaded'.");
            strcat(strException, sb->ToString());
            Singleton::trace(sb->ToString() + "\n");
            return;
        }

        // unload domain
        try {
            AppDomain::Unload(Loader->domain);
        }
        catch (Exception ^ ex) {
            strcat(strException, ex->Message);

            Singleton::trace("Cpp wrapper : Unload() : Unload domain exception : " + ex->Message + "\n");
            return;
        }
        Loader->Plugin = nullptr;
        Loader->domain = nullptr;
        Loader->status = Unloaded;
        Singleton::PlugList->Remove(key);

        Loader = nullptr;
        strcat(strException, "OK");
    }

    //---------------------------------------------------------------------------------------------------------------



}  // extern "C"
