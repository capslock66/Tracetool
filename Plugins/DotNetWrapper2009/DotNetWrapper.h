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

using namespace System ;
using namespace System::Text ;
using namespace System::Reflection ;   // Assembly
using namespace System::Collections ; 
using namespace System::IO ;           // Stream

enum PluginStatus 
{
   /// <summary>
   /// Unloaded
   /// </summary>
   Unloaded, 
   /// <summary>
   /// Loaded, not started
   /// </summary>
   Loaded , 
   /// <summary>
   /// Loaded and started
   /// </summary>
   Started 
} ;


// the Singleton and the CppPluginLoader classes are not put on a namespace

//---------------------------------------------------------------------------------------------------------------
public ref class Singleton
{
public : 
   static System::Collections::Hashtable ^ PlugList  = gcnew Hashtable() ; 

   static void trace (String ^ source)
   {
      FileStream ^ f ;
      String ^ FileToWrite = gcnew String("c:\\TracetoolInternalLog.txt") ;

      // check if exist

      if (File::Exists(FileToWrite) == false)
      {
         f = gcnew FileStream(FileToWrite, FileMode::Create) ;
      } else {  // append only the node
         f = File::Open(FileToWrite, FileMode::Open, FileAccess::ReadWrite, FileShare::ReadWrite);
         f->Seek(0, SeekOrigin::End); 
      }
      array<Byte>^info = (gcnew UTF8Encoding(true))->GetBytes((DateTime::Now.ToString("yyyyMMdd HH:mm:ss:fff ") + source)->ToString());
      f->Write(info, 0, info->Length);
      f->Close();
      f = nullptr ;
   }  // trace()
} ;   // Singleton

//---------------------------------------------------------------------------------------------------------------


public delegate String ^ Delegate_GetPlugName() ;
public delegate void Delegate_Start() ;
public delegate void Delegate_Stop() ;
public delegate void Delegate_OnTimer() ;
public delegate bool Delegate_OnBeforeDelete(String ^  WinId, String ^  NodeId) ;
public delegate bool Delegate_OnAction(String ^ WinId, int ResourceId, String ^  NodeId) ;

// Each managed plugin are loaded in separated domain.
// a PLugin loader load only one plugin. 
// PluginLoader is created from separated AppDomain
[Serializable]  
public ref class CppPluginLoader 
{
public :
   /// <summary>
   /// Point to the plugin (type ITracePLugin)
   /// </summary>
   Object ^ Plugin ; 

   /// <summary>
   /// delegate to the differents ITracePLugin functions
   /// </summary>
   Delegate_GetPlugName ^ delegate_GetPlugName ;
   Delegate_Start ^ delegate_Start ;      
   Delegate_Stop ^ delegate_Stop ;
   Delegate_OnAction ^ delegate_OnAction ;
   Delegate_OnBeforeDelete ^ delegate_OnBeforeDelete ;
   Delegate_OnTimer ^ delegate_OnTimer ;

   /// <summary>
   /// Domain that host the plugin
   /// </summary>
   AppDomain ^ domain ;

   /// <summary>
   /// Plugin name
   /// </summary>
   String ^ name ; 

   /// <summary>
   /// Status of the plugin
   /// </summary>
   PluginStatus status ;

   //---------------------------------------------------------------------------------------------------------------

   // check if the assembly containt a class that implement the Tracetool.ITracePLugin interface
   // throw exception on error
   void CheckPlugInFile (String ^ FileName)   
   {
      name = gcnew String("") ;
      Plugin = nullptr ;

      // Load the assembly in the current domain (the one created with AppDomain.CreateDomain() )
      //------------------------------------------------------------------------------------------

      // exception can occur on assembly load or GetTypes
      Assembly ^ assem ;
      try {
         AssemblyName ^ name = AssemblyName::GetAssemblyName(FileName) ;
         assem = AppDomain::CurrentDomain->Load(name) ;
      } catch ( Exception  ^ ex ) {
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n") ;
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : AppDomain::CurrentDomain->Load() exception : " + ex->Message + "\n") ;
         throw gcnew Exception(ex->Message) ;
      }

      // get all types for the assembly, check if one of the type implement the TraceTool.ITracePLugin
      //-----------------------------------------
      array<Type^>^ types ;
      try {
         // exception can be throw if the assembly make reference to an unavailable assembly (like tracetool)
         types = assem->GetTypes() ;
      } catch ( ReflectionTypeLoadException  ^ ex ) {
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n") ;
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : assem->GetTypes() exception : " + ex->Message + "\n") ;

         StringBuilder ^ errMsg = gcnew StringBuilder (ex->Message) ;
         errMsg->Append("\n");
         array<Exception^>^ err = ex->LoaderExceptions ;
         for (int c = 0; c < err->Length ; c++)
         {
            Exception ^ e = err[c] ;
            Singleton::trace (e->Message + "\n") ;
            errMsg->Append(e->Message) ;
         }
         throw gcnew Exception(errMsg->ToString()) ;   // throw original exception concatened with LoaderExceptions messages
      } catch ( Exception  ^ ex ) {
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n") ;
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : assem->GetTypes() exception : " + ex->Message + "\n") ;
         throw gcnew Exception(ex->Message) ;
      }

      if (types->Length == 0) {
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n") ;
         Singleton::trace ("CppPluginLoader : CheckPlugInFile : The Assembly don't contain any classes. Try to load it as a Win32 plugin\n") ;
         throw gcnew Exception("The Assembly don't contain any classes. Try to load it as a Win32 plugin") ;
      }
      //Singleton::trace ("CppPluginLoader : CheckPlugInFile : Assembly contains " + types->Length.ToString() + " classes or interfaces ") ;
      for (int c = 0; c < types->Length ; c++)
      {
         Type ^ OneType = types[c] ;

         //Singleton::trace ("   ->" + OneType->Name) ;

         // check if the type implement the Tracetool.ITracePLugin interface
         array<Type^>^ interfaces = OneType->GetInterfaces() ;

         for (int d = 0 ; d < interfaces->Length ; d++) 
         {
            Type ^ OneInterface = interfaces[d] ;
            if (OneInterface->FullName->Equals("TraceTool.ITracePLugin"))
            {
               // create an instance of the type and save it in the PluginLoader
               // Error can occur if the type require parameters.
               Plugin = Activator::CreateInstance(OneType) ;

               // create delegates
               //--------------------
               try {
                  delegate_GetPlugName    = (Delegate_GetPlugName    ^) Delegate::CreateDelegate (Delegate_GetPlugName   ::typeid , Plugin,"GetPlugName"   );
                  delegate_Start          = (Delegate_Start          ^) Delegate::CreateDelegate (Delegate_Start         ::typeid , Plugin,"Start"         );
                  delegate_Stop           = (Delegate_Stop           ^) Delegate::CreateDelegate (Delegate_Stop          ::typeid , Plugin,"Stop"          );
                  delegate_OnTimer        = (Delegate_OnTimer        ^) Delegate::CreateDelegate (Delegate_OnTimer       ::typeid , Plugin,"OnTimer"       );
                  delegate_OnBeforeDelete = (Delegate_OnBeforeDelete ^) Delegate::CreateDelegate (Delegate_OnBeforeDelete::typeid , Plugin,"OnBeforeDelete");
                  delegate_OnAction       = (Delegate_OnAction       ^) Delegate::CreateDelegate (Delegate_OnAction      ::typeid , Plugin,"OnAction"      );
               } catch ( Exception  ^ ex ) {
                  Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n") ;
                  Singleton::trace ("CppPluginLoader : CheckPlugInFile : CreateDelegate() exception : " + ex->Message + "\n") ;
                  throw gcnew Exception(ex->Message) ;
               }

               // Get the plugin name
               // -------------------
               try {
                  name = delegate_GetPlugName () ; 
               } catch ( Exception  ^ ex ) {
                  Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n") ;
                  Singleton::trace ("CppPluginLoader : CheckPlugInFile : delegate_GetPlugName exception : " + ex->Message + "\n") ;
                  throw gcnew Exception(ex->Message) ;
               }
               if (name->Trim()->Length == 0)
                  name = FileName ;

               // this plugin will be added to Singleton::PlugList
               return ; 
            }
         }  // next interface
      }     // next type

      StringBuilder ^ sb = gcnew StringBuilder ("PluginLoader : Assembly <") ;
      sb->Append (FileName)->Append ("> contains ")->Append (types->Length)
         ->Append (" classe(s), \n but none implements TraceTool.IPplugin interface. It's perhaps a Win32 plugin") ;

      Singleton::trace ("CppPluginLoader : CheckPlugInFile : " + FileName + "\n" + sb->ToString() + "\n" ) ;
      throw gcnew Exception(sb->ToString()) ;
   }  // CheckPlugInFile
};    // CppPluginLoader class

//}        // namespace TraceTool
