// Dot net wrapper.cpp 
// This dll must be installed at the same place as the viewer.
//
// Author : Thierry Parent
// Version : 12.3
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information 

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
    // test purpose. Not Called
    __declspec(dllexport) void __cdecl test(unsigned PlugId, char* strException)
    {
        trace("CppTest()\n");
    }

    //---------------------------------------------------------------------------------------------------------------

    __declspec(dllexport) void __cdecl CheckPlugInFile(unsigned PlugId, char* FileName, char* PlugName, char* strException)
    {
        String^ strFileName = gcnew String(FileName);
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

            // create a domain for the plugin
            //-------------------------------

            try {
                wrapper->Domain = AppDomain::CreateDomain("PlugDomain");
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("CreateDomain exception")
                    ->Append(",FileName : " + strFileName + ",")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            // load the loader in the created domain
            // https://docs.microsoft.com/en-us/dotnet/api/system.marshalbyrefobject?view=netframework-4.8
            // https://weblog.west-wind.com/posts/2009/Jan/19/Assembly-Loading-across-AppDomains
            //----------------------------------------

            try {
                String^ Path;
                Path = Assembly::GetAssembly(DomainPluginCaller::typeid)->Location;
                //trace("wrapper : CheckPlugInFile : Path : " + Path + "\n");

                wrapper->Loader = (DomainPluginCaller^)(wrapper->Domain->CreateInstanceFromAndUnwrap(Path, DomainPluginCaller::typeid->FullName));
                //trace("wrapper : CheckPlugInFile : Loader created " + "\n");
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("CreateInstanceFromAndUnwrap exception")
                    ->Append(",FileName : " + strFileName + ",")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());

                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");

                return;
            }

            // call Loader.CheckPlugInFile
            //----------------------------
            try {
                wrapper->Loader->CheckPlugInFile(strFileName);
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("Loader->CheckPlugInFile exception")
                    ->Append(",FileName : " + strFileName + ",")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");

                wrapper->Loader = nullptr;
                AppDomain::Unload(wrapper->Domain);
                return;
            }

            wrapper->Status = Loaded;
            try {
                strcat(PlugName, wrapper->Loader->GetPlugName());
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("GetPlugName exception")
                    ->Append(",FileName : " + strFileName + ",")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            wrapper->Status = Loaded;

            // add to plugin list
            ManagedGlobals::WrapperDic->Add(PlugId, wrapper);

            //trace("wrapper : CheckPlugInFile : done\n");
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("CheckPlugInFile exception")
                ->Append(",FileName : " + strFileName + ",")
                ->Append(ex->Message)
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
            //trace("wrapper : Start(PLugin:" + PlugId + ")\n");

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

            try {
                wrapper->Loader->StartPlugin(wrapper->Parameters);
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("StartPlugin exception")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return;
            }

            wrapper->Status = Started;
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("Start exception")
                ->Append(ex->Message)
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
                trace(sb->ToString() + "\n");
                return;
            }

            try {
                wrapper->Loader->StopPlugin();
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("Stop exception")
                    ->Append(ex->Message)
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
                ->Append("Stop exception")
                ->Append(ex->Message)
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

            try {
                String^ strWinId = gcnew String(WinId);
                String^ strNodeId = gcnew String(NodeId);
                result = wrapper->Loader->OnAction(strWinId, ResourceId, strNodeId);
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("OnAction exception")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return true;
            }
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("OnAction exception")
                ->Append(ex->Message)
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
                return wrapper->Loader->OnBeforeDelete(strWinId, strNodeId);
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("OnBeforeDelete exception")
                    ->Append(ex->Message)
                    ->Append(ex->StackTrace->ToString());
                strcat(strException, sb->ToString());
                trace(sb->ToString() + "\n");
                return true;
            }
            strcat(strException, "OK");
        }
        catch (Exception^ ex)
        {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("OnBeforeDelete exception")
                ->Append(ex->Message)
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
                wrapper->Loader->OnTimer();
            }
            catch (Exception^ ex) {
                StringBuilder^ sb = gcnew StringBuilder();
                sb
                    ->Append("OnTimer exception")
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
                ->Append("OnTimer exception")
                ->Append(ex->Message)
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
                    wrapper->Loader->StopPlugin();
                }
                catch (Exception^ ex) {
                    StringBuilder^ sb = gcnew StringBuilder();
                    sb
                        ->Append("StopPlugin exception")
                        ->Append(ex->Message)
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

            wrapper->Loader->UnloadPlugin();
            wrapper->Loader = nullptr;
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
                ->Append("Unload exception")
                ->Append(ex->Message)
                ->Append(ex->StackTrace->ToString());
            strcat(strException, sb->ToString());
            trace(sb->ToString() + "\n");
            return;
        }
    }

}  // extern "C"
