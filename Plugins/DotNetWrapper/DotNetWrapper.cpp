// Dot net wrapper.cpp 
// This dll must be installed at the same place as the viewer.
//
// Author : Thierry Parent
// Version : 13.0
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information 

#include "pch.h"
#include <string.h>
#include <fstream>
#include "DotNetWrapper.h" 

//------------------------------------------------------------------------------

/// <summary>
/// Utility function : concat a Managed String to a C string buffer 
/// </summary>
/// <param name="dest">target buffer. The size is limited to 1999 chars </param>
/// <param name="source">source </param>

void strcat(char* dest, String^ source)
{
    unsigned SourceLen = source->Length;
    unsigned DestLen = strlen(dest);

    if ((DestLen + SourceLen) >= 1998)   // limit all strings to 1999 chars max
        SourceLen = 1998 - DestLen;

    array<Char>^ SourceChars = source->ToCharArray();

    unsigned int i;
    for (i = 0; i < SourceLen; i++)
        dest[DestLen + i] = (char)SourceChars[i];

    dest[DestLen + i] = '\0';
}

//------------------------------------------------------------------------------

extern "C"
{
    // test purpose. Not Called
    __declspec(dllexport) void __cdecl test(unsigned PlugId, char* strException)
    {
        trace("CppTest()\n");
    }

    //---------------------------------------------------------------------------------------------------------------
    bool GetPlugName(PluginWrapper^ wrapper, char* PlugName, char* strException)
    {
        try {
            strcat(PlugName, wrapper->DomainCaller->GetPlugName());
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("GetPlugName exception \n")
                ->Append("FileName : " + wrapper->FileName + "\n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return false;
        }
        return true;
    }

    //---------------------------------------------------------------------------------------------------------------

    bool GetDelegates(PluginWrapper^ wrapper, char* strException)
    {
        try {
            wrapper->DomainCaller->CheckPlugInFile(wrapper->FileName);
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("DomainCaller->CheckPlugInFile exception \n")
                ->Append("FileName : " + wrapper->FileName + "\n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");

            wrapper->DomainCaller = nullptr;
            AppDomain::Unload(wrapper->Domain);
            return false;
        }
        return true;
    }

    //---------------------------------------------------------------------------------------------------------------

    // load the plugin in the created domain
    bool LoadPlugin(PluginWrapper^ wrapper, char* strException)
    {
        // https://docs.microsoft.com/en-us/dotnet/api/system.marshalbyrefobject?view=netframework-4.8
        // https://weblog.west-wind.com/posts/2009/Jan/19/Assembly-Loading-across-AppDomains
        try {
            String^ Path;
            Path = Assembly::GetAssembly(DomainPluginCaller::typeid)->Location;
            //trace("wrapper : CheckPlugInFile : Path : " + Path + "\n");

            wrapper->DomainCaller = (DomainPluginCaller^)(wrapper->Domain->CreateInstanceFromAndUnwrap(Path, DomainPluginCaller::typeid->FullName));
            //trace("wrapper : CheckPlugInFile : DomainCaller created " + "\n");

            wrapper->Status = Loaded;
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("CreateInstanceFromAndUnwrap exception \n")
                ->Append("FileName : " + wrapper->FileName + "\n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());

            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");

            return false;
        }
        return true;
    }

    //---------------------------------------------------------------------------------------------------------------

    // create a domain for the plugin

    bool CreateDomain(PluginWrapper^ wrapper, char* strException)
    {
        try {
            wrapper->Domain = AppDomain::CreateDomain("PlugDomain");
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("CreateDomain exception \n")
                ->Append("FileName : " + wrapper->FileName + "\n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return false;
        }
        return true;
    }
    
    //---------------------------------------------------------------------------------------------------------------

    bool StartPlugin(PluginWrapper^ wrapper, char* strException)
    {
        try {
            wrapper->DomainCaller->StartPlugin(wrapper->Parameters);
            wrapper->Status = Started;
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("StartPlugin exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return false;
        }
        return true;
    }

    //---------------------------------------------------------------------------------------------------------------

    // called by onAction, OnDelete, OnTimer when the plugin is no more accessible (server disconnected, unloaded,...)
    bool ReLoadPlugin(PluginWrapper^ wrapper, char* strException)
    {
        trace("Reload plugin \n");
        
        wrapper->DomainCaller = nullptr;
        wrapper->Domain = nullptr;
        wrapper->Status = Unloaded;

        if (CreateDomain(wrapper, strException) == false) return false;
        if (LoadPlugin(wrapper, strException) == false) return false;
        if (GetDelegates(wrapper, strException) == false) return false;
        if (StartPlugin(wrapper, strException) == false) return false;

        return true;
    }

    //---------------------------------------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl CheckPlugInFile(unsigned PlugId, char* FileName, char* PlugName, char* strException)
    {
        try
        {
            //trace ("wrapper : CheckPlugInFile(PLugin:<" + key + ">,FileName:<" + strFileName + ">\n");
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId))
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : CheckPlugInFile : PLugin ")->Append(PlugId)->Append(" is already loaded.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            // wrapper contains all informations about the pluggin
            PluginWrapper^ wrapper = gcnew PluginWrapper();
            wrapper->FileName = gcnew String(FileName);
            wrapper->Status = Unloaded;

            // create a domain for the plugin
            if (CreateDomain(wrapper, strException) == false) return;

            // load the DomainCaller in the created domain. Set status to Loaded;
            if (LoadPlugin(wrapper, strException) == false) return;

            // get delegate functions
            if (GetDelegates(wrapper, strException) == false) return;

            if (GetPlugName(wrapper, PlugName, strException) == false) return;

            // add to plugin list
            ManagedGlobals::WrapperDic->Add(PlugId, wrapper);

            //trace("wrapper : CheckPlugInFile : done\n");
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("CheckPlugInFile exception \n")
                ->Append("FileName : ")->Append(FileName)->Append("\n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return;
        }
    }

    //---------------------------------------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl Start(unsigned PlugId, char* Parameter, char* strException)
    {
        try
        {
            trace("wrapper : Start(PLugin:" + PlugId + ")\n");

            // check if plugin is know
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId) == false)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : Start : PLugin ")->Append(PlugId)->Append(" is unknow.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            // get plugin 
            PluginWrapper^ wrapper;
            ManagedGlobals::WrapperDic->TryGetValue(PlugId, wrapper);

            if (wrapper->Status == Started) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : Start : PLugin ")->Append(PlugId)->Append(" is already started.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            if (wrapper->Status != Loaded) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("Cpp wrapper.Start : PLugin ")->Append(PlugId)->Append(" is not loaded.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }
            wrapper->Parameters = gcnew String(Parameter);

            if (StartPlugin(wrapper, strException) == false)
                return;
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("Start exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return;
        }
    }

    //------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl Stop(unsigned PlugId, char* strException)
    {
        try
        {
            // check if plugin is know
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId) == false)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : Stop : PLugin ")->Append(PlugId)->Append(" is unknow.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            // get plugin 
            PluginWrapper^ wrapper;
            ManagedGlobals::WrapperDic->TryGetValue(PlugId, wrapper);

            if (wrapper->Status != Started) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : Stop : PLugin ")->Append(PlugId)->Append(" is not started.");
                strcat(strException, sb->ToString());
                //trace(sb->ToString() + "\n");
                return;
            }

            try {
                wrapper->DomainCaller->StopPlugin();
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("Stop exception \n")
                    ->Append(ex->Message + "\n")
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            wrapper->Status = Loaded;
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("Stop exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return;
        }
    }

    //------------------------------------------------------------------------------

    __declspec(dllexport) int __cdecl OnAction(unsigned PlugId, char* WinId, unsigned ResourceId, char* NodeId, char* strException)
    {
        int result = true;
        try
        {
            // check if plugin is know
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId) == false)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : OnAction : PLugin ")->Append(PlugId)->Append(" is unknow.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return true;
            }

            // get plugin 
            PluginWrapper^ wrapper;
            ManagedGlobals::WrapperDic->TryGetValue(PlugId, wrapper);

            if (wrapper->Status != Started)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("Dot net wrapper.OnAction : PLugin ")->Append(PlugId)->Append(" is not started.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return true;
            }

            String^ strWinId = gcnew String(WinId);
            String^ strNodeId = gcnew String(NodeId);
            try {
                result = wrapper->DomainCaller->OnAction(strWinId, ResourceId, strNodeId);
            }
            catch (Exception^ ex) 
            {
                if (ReLoadPlugin(wrapper, strException) == true)
                {
                    // plugin reloaded. Try again OnAction
                    result = wrapper->DomainCaller->OnAction(strWinId, ResourceId, strNodeId);
                }  else {
                    StringBuilder^ sb = gcnew StringBuilder();
                    sb
                        ->Append("OnAction exception \n")
                        ->Append(ex->Message + "\n")                     // Object '/xxx' has been disconnected or does not exist at the server
                        ->Append(ex->StackTrace->ToString());
                    trace(sb->ToString() + "\n");
                    strcat(strException, sb->ToString());
                    return true;
                }
            }
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("OnAction exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return true;
        }
        return result;
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) int __cdecl OnBeforeDelete(unsigned PlugId, char* WinId, char* NodeId, char* strException)
    {
        try
        {
            String^ strWinId = gcnew String(WinId);
            String^ strNodeId = gcnew String(NodeId);

            // check if plugin is know
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId) == false)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : OnBeforeDelete : PLugin ")->Append(PlugId)->Append(" is unknow.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return true;
            }

            // get plugin 
            PluginWrapper^ wrapper;
            ManagedGlobals::WrapperDic->TryGetValue(PlugId, wrapper);

            if (wrapper->Status != Started) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("Cpp wrapper.OnBeforeDelete : PLugin ")->Append(PlugId)->Append(" is not started.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return true;
            }

            try {
                return wrapper->DomainCaller->OnBeforeDelete(strWinId, strNodeId);
            }
            catch (Exception^ ex) 
            {
                if (ReLoadPlugin(wrapper, strException) == true)
                {
                    // plugin reloaded. Try again OnBeforeDelete
                    return wrapper->DomainCaller->OnBeforeDelete(strWinId, strNodeId);
                } else {
                    StringBuilder^ sb = gcnew StringBuilder();
                    sb
                        ->Append("OnBeforeDelete exception \n")
                        ->Append(ex->Message + "\n")
                        ->Append(ex->StackTrace->ToString());
                    trace(sb->ToString() + "\n");
                    strcat(strException, sb->ToString());
                    return true;
                }
            }
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("OnBeforeDelete exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return true;
        }
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) void __cdecl OnTimer(unsigned PlugId, char* strException)
    {
        try
        {
            // check if plugin is know
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId) == false)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : OnTimer : PLugin ")->Append(PlugId)->Append(" is unknow.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            // get plugin 
            PluginWrapper^ wrapper;
            ManagedGlobals::WrapperDic->TryGetValue(PlugId, wrapper);

            if (wrapper->Status != Started) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : OnTimer : PLugin ")->Append(PlugId)->Append(" is not started.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            try {
                wrapper->DomainCaller->OnTimer();
            }
            catch (Exception^ ex) 
            {
                if (ReLoadPlugin(wrapper, strException) == true)
                {
                    // plugin reloaded. Try again OnTimer
                    wrapper->DomainCaller->OnTimer();
                } else {
                    StringBuilder^ sb = gcnew StringBuilder();
                    sb
                        ->Append("OnTimer exception \n")
                        ->Append(ex->Message + "\n")
                        ->Append(ex->StackTrace->ToString());
                    trace(sb->ToString() + "\n");
                    strcat(strException, sb->ToString());
                    return;
                }
            }
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("OnTimer exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return;
        }
    }

    //---------------------------------------------------------------------------------------------------------------
    __declspec(dllexport) void __cdecl Unload(unsigned PlugId, char* strException)
    {
        try
        {
            System::Object^ key = PlugId;

            // check if plugin is know
            if (ManagedGlobals::WrapperDic->ContainsKey(PlugId) == false)
            {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : Unload : PLugin ")->Append(key)->Append(" is unknow.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            // get plugin 
            PluginWrapper^ wrapper;
            ManagedGlobals::WrapperDic->TryGetValue(PlugId, wrapper);

            // stop before unload
            if (wrapper->Status == Started) {
                try {
                    wrapper->DomainCaller->StopPlugin();
                }
                catch (Exception^ ex) {
                    StringBuilder^ sb = gcnew StringBuilder();
                    sb
                        ->Append("StopPlugin exception \n")
                        ->Append(ex->Message + "\n")
                        ->Append(ex->StackTrace->ToString());
                    strcat(strException, sb->ToString());
                    trace(sb->ToString() + "\n");
                    return;
                }
            }

            if (wrapper->Status != Loaded) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb->Append("wrapper : Unload : PLugin ")->Append(key)->Append(" is not loaded'.");
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            try {
                wrapper->DomainCaller->UnloadPlugin();  // release reference to plugin
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("unload domain exception \n")
                    ->Append(ex->Message + "\n")
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                // no need to reload the plugin
                return;
            }
            wrapper->DomainCaller = nullptr;
            wrapper->Status = Unloaded;
            ManagedGlobals::WrapperDic->Remove(PlugId);

            // unload domain
            //trace("wrapper : Unload : Unload domain \n");
            try {
                AppDomain^ domain = wrapper->Domain;
                AppDomain::Unload(domain);
                wrapper->Domain = nullptr;
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("unload domain exception")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("Unload exception \n")
                ->Append(ex->Message + "\n")
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return;
        }
    }

}  // extern "C"
