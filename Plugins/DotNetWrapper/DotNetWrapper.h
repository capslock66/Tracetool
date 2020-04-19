// DotNetWrapper.h
// This dll must be installed at the same place as the viewer.
//
// Author : Thierry Parent
// Version : 11.0
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information 

#pragma once

using namespace System;
using namespace System::Text;
using namespace System::Reflection;   // Assembly
using namespace System::Collections;
using namespace System::IO;           // Stream
using namespace System::Collections::Generic;

//---------------------------------------------------------------------------------------------------------------

extern "C"
{
    static void trace(String^ source)
    {
        FileStream^ f;
        String^ FileToWrite = gcnew String("c:\\temp\\DotNetWrapperLog.txt");

        // check if exist

        if (File::Exists(FileToWrite) == false)
        {
            f = gcnew FileStream(FileToWrite, FileMode::Create);
        }
        else {  // append only the node
            f = File::Open(FileToWrite, FileMode::Open, FileAccess::ReadWrite, FileShare::ReadWrite);
            f->Seek(0, SeekOrigin::End);
        }
        array<Byte>^ info = (gcnew UTF8Encoding(true))->GetBytes((DateTime::Now.ToString("yyyyMMdd HH:mm:ss:fff ") + source)->ToString());
        f->Write(info, 0, info->Length);
        f->Close();
        f = nullptr;
    }
}

//---------------------------------------------------------------------------------------------------------------

enum PluginStatus
{
    /// <summary>
    /// Unloaded
    /// </summary>
    Unloaded,
    /// <summary>
    /// Loaded, not started
    /// </summary>
    Loaded,
    /// <summary>
    /// Loaded and started
    /// </summary>
    Started
};

//---------------------------------------------------------------------------------------------------------------

public delegate String^ Delegate_GetPlugName();
public delegate void Delegate_Start(String^ strParameter);
public delegate void Delegate_Stop();
public delegate void Delegate_OnTimer();
public delegate bool Delegate_OnBeforeDelete(String^ WinId, String^ NodeId);
public delegate bool Delegate_OnAction(String^ WinId, int ResourceId, String^ NodeId);

// Each managed plugin are loaded in separated domain.
// a PLugin loader load only one plugin. 
// DomainPluginCaller is created from separated AppDomain
[Serializable]
public ref class DomainPluginCaller : MarshalByRefObject
{
private:

    // delegate to the differents ITracePlugin functions
    Delegate_GetPlugName^ _delegate_GetPlugName;
    Delegate_Start^ _delegate_Start;
    Delegate_Stop^ _delegate_Stop;
    Delegate_OnAction^ _delegate_OnAction;
    Delegate_OnBeforeDelete^ _delegate_OnBeforeDelete;
    Delegate_OnTimer^ _delegate_OnTimer;

    // Point to the plugin (type ITracePlugin). Needed to unloaded the plugin
    Object^ _plugin;

public:

    String^ GetPlugName()
    {
        //trace("    DomainPluginCaller : GetPlugName\n");
        String^ name;
        name = gcnew String(_delegate_GetPlugName());
        //trace("    DomainPluginCaller : GetPlugName : " + name + "\n");

        return name; //  gcnew String("my plugin"); // _delegate_GetPlugName();
    }

    void UnloadPlugin()
    {
        //trace("    DomainPluginCaller : UnloadPlugin\n");
        _plugin = nullptr;  // Garbage collected
    }

    void StartPlugin(String^ strParameter)
    {
        //trace("    DomainPluginCaller : StartPlugin begin\n");
        _delegate_Start(strParameter);
        //trace("    DomainPluginCaller : StartPlugin end\n");
    }

    void StopPlugin()
    {
        //trace("    DomainPluginCaller : StopPlugin begin\n");
        _delegate_Stop();
        //trace("    DomainPluginCaller : StopPlugin end\n");
    }

    bool OnAction(String^ strWinId, int ResourceId, String^ strNodeId)
    {
        //trace("    DomainPluginCaller : OnAction start\n");
        bool result = _delegate_OnAction(strWinId, ResourceId, strNodeId);
        //trace("    DomainPluginCaller : OnAction end \n");
        return result;
    }

    bool OnBeforeDelete(String^ strWinId, String^ strNodeId)
    {
        //trace("    DomainPluginCaller : OnBeforeDelete start\n");
        bool result = _delegate_OnBeforeDelete(strWinId, strNodeId);
        //trace("    DomainPluginCaller : OnBeforeDelete end \n");
        return result;
    }

    void OnTimer()
    {
        //trace("    DomainPluginCaller : OnTimer start\n");
        _delegate_OnTimer();
        //trace("    DomainPluginCaller : OnTimer end\n");
    }

    //---------------------------------------------------------------------------------------------------------------

    // check if the assembly containt a class that implement the Tracetool.ITracePlugin interface
    // throw exception on error
    void CheckPlugInFile(String^ FileName)
    {
        //trace("    DomainPluginCaller : CheckPlugInFile : " + FileName + "\n");
        //name = nullptr;
        _plugin = nullptr;

        // Load the assembly in the current domain (the one created with AppDomain.CreateDomain() )
        //------------------------------------------------------------------------------------------

        // exception can occur on assembly load or GetTypes
        Assembly^ assem;
        try {
            AssemblyName^ name = AssemblyName::GetAssemblyName(FileName);
            assem = AppDomain::CurrentDomain->Load(name);
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("DomainPluginCaller : CheckPlugInFile load assembly exception")
                ->Append(",FileName : " + FileName + ",")
                ->Append(ex->Message)
                ->Append(ex->StackTrace->ToString());
            trace(sb->ToString() + "\n");
            throw gcnew Exception(sb->ToString());
        }

        // get all types for the assembly, check if one of the type implement the TraceTool.ITracePlugin
        //-----------------------------------------
        array<Type^>^ types;
        try {
            // exception can be throw if the assembly make reference to an unavailable assembly (like tracetool)
            types = assem->GetTypes();
        }
        catch (ReflectionTypeLoadException^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("DomainPluginCaller : CheckPlugInFile assem->GetTypes() exception")
                ->Append(",FileName : " + FileName + ",")
                ->Append(ex->Message)
                ->Append(ex->StackTrace->ToString())
                ->Append("\n");
            array<Exception^>^ err = ex->LoaderExceptions;
            for (int c = 0; c < err->Length; c++)
            {
                Exception^ e = err[c];
                sb->Append(e->Message);
            }
            trace(sb->ToString() + "\n");
            throw gcnew Exception(sb->ToString());   // throw original exception concatened with LoaderExceptions messages
        }
        catch (Exception^ ex) {
            StringBuilder^ sb = gcnew StringBuilder();
            sb
                ->Append("DomainPluginCaller : CheckPlugInFile assem->GetTypes() exception")
                ->Append(",FileName : " + FileName + ",")
                ->Append(ex->Message)
                ->Append(ex->StackTrace->ToString());
            trace(sb->ToString() + "\n");
            throw gcnew Exception(sb->ToString());

        }

        if (types->Length == 0) {
            trace("    DomainPluginCaller : CheckPlugInFile : " + FileName + "\n");
            trace("    DomainPluginCaller : CheckPlugInFile : The Assembly don't contain any classes. Try to load it as a Win32 plugin\n");
            throw gcnew Exception("The Assembly " + FileName + " don't contain any classes. Try to load it as a Win32 plugin");
        }

        //trace ("    DomainPluginCaller : CheckPlugInFile : Assembly contains " + types->Length.ToString() + " classes or interfaces\n") ;
        for (int c = 0; c < types->Length; c++)
        {
            Type^ OneType = types[c];

            // check if the type implement the Tracetool.ITracePLugin interface
            array<Type^>^ interfaces = OneType->GetInterfaces();

            for (int d = 0; d < interfaces->Length; d++)
            {
                Type^ OneInterface = interfaces[d];
                //trace("    DomainPluginCaller : CheckPlugInFile : interface <" + OneInterface->FullName + ">\n");  // TraceTool.ITracePlugin
                if (OneInterface->FullName->Equals("TraceTool.ITracePlugin"))
                {
                    // create an instance of the type and save it in the PluginLoader
                    // Error can occur if the type require parameters.

                    //trace("    DomainPluginCaller : CheckPlugInFile : TraceTool.ITracePlugin found. Create instance\n");
                    try {
                        _plugin = Activator::CreateInstance(OneType);
                    }
                    catch (Exception^ ex) {
                        StringBuilder^ sb = gcnew StringBuilder();
                        sb
                            ->Append("DomainPluginCaller : CheckPlugInFile CreateInstance() exception")
                            ->Append(",FileName : " + FileName + ",")
                            ->Append(ex->Message)
                            ->Append(ex->StackTrace->ToString());
                        trace(sb->ToString() + "\n");
                        throw gcnew Exception(sb->ToString());
                    }

                    // create delegates
                    //--------------------
                    try {
                        //trace("    DomainPluginCaller : CheckPlugInFile : getting delegates\n");
                        _delegate_GetPlugName = (Delegate_GetPlugName^)Delegate::CreateDelegate(Delegate_GetPlugName   ::typeid, _plugin, "GetPlugName");
                        _delegate_Start = (Delegate_Start^)Delegate::CreateDelegate(Delegate_Start         ::typeid, _plugin, "Start");
                        _delegate_Stop = (Delegate_Stop^)Delegate::CreateDelegate(Delegate_Stop          ::typeid, _plugin, "Stop");
                        _delegate_OnTimer = (Delegate_OnTimer^)Delegate::CreateDelegate(Delegate_OnTimer       ::typeid, _plugin, "OnTimer");
                        _delegate_OnBeforeDelete = (Delegate_OnBeforeDelete^)Delegate::CreateDelegate(Delegate_OnBeforeDelete::typeid, _plugin, "OnBeforeDelete");
                        _delegate_OnAction = (Delegate_OnAction^)Delegate::CreateDelegate(Delegate_OnAction      ::typeid, _plugin, "OnAction");
                    }
                    catch (Exception^ ex) {
                        StringBuilder^ sb = gcnew StringBuilder();
                        sb
                            ->Append("DomainPluginCaller : CheckPlugInFile CreateDelegate exception")
                            ->Append(",FileName : " + FileName + ",")
                            ->Append(ex->Message)
                            ->Append(ex->StackTrace->ToString());
                        trace(sb->ToString() + "\n");
                        throw gcnew Exception(sb->ToString());
                    }
                    //trace("    DomainPluginCaller : CheckPlugInFile : DONE\n");
                    return;
                }
            }  // next interface
        }     // next type

        // no TraceTool.ITracePlugin found.
        // generate exception

        StringBuilder^ sb = gcnew StringBuilder();
        sb
            ->Append("DomainPluginCaller : Assembly <")
            ->Append(FileName)
            ->Append("> contains ")
            ->Append(types->Length)
            ->Append(" classe(s), \n but none implements TraceTool.ITracePlugin interface. It's perhaps a Win32 plugin");
        trace(sb->ToString() + "\n");
        throw gcnew Exception(sb->ToString());
    }  // CheckPlugInFile

};    // DomainPluginCaller class

//---------------------------------------------------------------------------------------------------------------

// wrapper for DomainPluginCaller (loaded in another domain)
public ref class PluginWrapper
{
public:
    // Status of the plugin
    PluginStatus Status;

    String^ Parameters;

    // Domain that host the plugin. Needed to unloaded the domain
    AppDomain^ Domain;

    // wrapper to the loader
    DomainPluginCaller^ Loader;

    //String^ SatusToString(PluginStatus status)
    //{
    //    String^ result;
    //    switch (status)
    //    {
    //    case Unloaded: result = gcnew String("Unloaded"); break;
    //    case Loaded:   result = gcnew String("Loaded");   break;
    //    case Started:  result = gcnew String("Started");  break;
    //    }
    //    return result;
    //}
};

//---------------------------------------------------------------------------------------------------------------

public ref class ManagedGlobals
{
public:
    //static System::Collections::Hashtable^ PlugList = gcnew Hashtable();
    static Dictionary<unsigned, PluginWrapper^>^ WrapperDic = gcnew Dictionary<unsigned, PluginWrapper^>();

};   // Singleton


//---------------------------------------------------------------------------------------------------------------
//}   // namespace TraceTool


