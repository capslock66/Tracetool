//==========================================================================================
//
//		OpenNETCF.Win32.Registry
//		Copyright (C) 2003-2005, OpenNETCF.org
//
//		This library is free software; you can redistribute it and/or modify it under 
//		the terms of the OpenNETCF.org Shared Source License.
//
//		This library is distributed in the hope that it will be useful, but 
//		WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
//		FITNESS FOR A PARTICULAR PURPOSE. See the OpenNETCF.org Shared Source License 
//		for more details.
//
//		You should have received a copy of the OpenNETCF.org Shared Source License 
//		along with this library; if not, email licensing@opennetcf.org to request a copy.
//
//		If you wish to contact the OpenNETCF Advisory Board to discuss licensing, please 
//		email licensing@opennetcf.org.
//
//		For general enquiries, email enquiries@opennetcf.org or visit our website at:
//		http://www.opennetcf.org
//
//==========================================================================================

using System;
using System.Collections;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace OpenNETCF.Win32
{
	#region Registry
	/// <summary>
	/// Supplies the base <b>RegistryKeys</b> that access values and subkeys in the registry.
	/// </summary>
	/// <remarks>This class provides the set of standard root keys found in the registry on machines running Windows CE.
	/// The registry is a storage facility for information about applications and default system settings.
	/// For example, applications can use the registry for storing information that needs to be preserved once the application is closed, and access that same information when the application is reloaded.
	/// For instance, you can store color preferences, screen locations, or the size of the window.
	/// The keys exposed by Registry are: 
	/// <list type="bullet">
	/// <item><term>CurrentUser</term><description>Stores information about user preferences.</description></item>
	/// <item><term>LocalMachine</term><description>Stores configuration information for the local machine.</description></item>
	/// <item><term>ClassesRoot</term><description>Stores information about types (and classes) and their properties.</description></item>  
	/// <item><term>Users</term><description>Stores information about the default user configuration.</description></item></list> 
	/// Once you have identified the root key under which you want to store/retrieve information from the registry, you can use the RegistryKey class to add or remove subkeys, and manipulate the values for a given key.</remarks>
	public sealed class Registry
	{
		/// <summary>
		/// Contains the configuration data for the local machine. This field reads the Windows registry base key HKEY_LOCAL_MACHINE.
		/// </summary>
		public static readonly RegistryKey LocalMachine = new RegistryKey((uint)RootKeys.LocalMachine, "HKEY_LOCAL_MACHINE", true, true);
		
		/// <summary>
		/// Contains information about the current user preferences. This field reads the Windows registry base key HKEY_CURRENT_USER.
		/// </summary>
		public static readonly RegistryKey CurrentUser = new RegistryKey((uint)RootKeys.CurrentUser, "HKEY_CURRENT_USER", true, true);
		
		/// <summary>
		///  Defines the types (or classes) of documents and the properties associated with those types. This field reads the Windows registry base key HKEY_CLASSES_ROOT.
		/// </summary>
		public static readonly RegistryKey ClassesRoot = new RegistryKey((uint)RootKeys.ClassesRoot, "HKEY_CLASSES_ROOT", true, true);
		
		/// <summary>
		/// Contains information about the default user configuration. This field reads the Windows registry base key HKEY_USERS.
		/// </summary>
		public static readonly RegistryKey Users = new RegistryKey((uint)RootKeys.Users, "HKEY_USERS", true, true);
		
	}
	//hKeys for the root keys
	internal enum RootKeys : uint 
	{
		ClassesRoot = 0x80000000, 
		CurrentUser = 0x80000001, 
		LocalMachine = 0x80000002,
		Users = 0x80000003 
	} 
	#endregion

	#region Registry Key
	/// <summary>
	/// Represents a key level node in the Windows registry. This class is a registry encapsulation.
	/// </summary>
	public sealed class RegistryKey : MarshalByRefObject, IDisposable
	{
		//hkey - handle to a registry key
		private uint m_handle;
		//full name of key
		private string m_name;
		//was key opened as writable
		private bool m_writable;
		//is root key
		private bool m_isroot;

		//error code when all items have been enumerated
		private const int ERROR_NO_MORE_ITEMS = 259;

		#region Constructor
		internal RegistryKey(uint handle, string name, bool writable, bool isroot)
		{
			m_handle = handle;
			m_name = name;
			m_writable = writable;
			m_isroot = isroot;
		}
		#endregion


		#region Name
		/// <summary>
		/// Retrieves the name of the key.
		/// </summary>
		public string Name
		{
			get
			{
				return m_name;
			}
		}
		#endregion

		#region To String
		/// <summary>
		/// Retrieves a string representation of this key.
		/// </summary>
		/// <returns>A string representing the key. If the specified key is invalid (cannot be found) then a null value is returned.</returns>
		/// <exception cref="System.ObjectDisposedException"> The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public override string ToString()
		{
			if(CheckHKey())
			{
				return m_name + " [0x" + m_handle.ToString("X") + "]";
			}
			else
			{
				throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
			}
		}
		#endregion


		#region Flush
		/// <summary>
		/// Writes all the attributes of the specified open registry key into the registry.
		/// </summary>
		/// <remarks>The Flush method may also write out parts of or all of the other keys.
		/// Calling this function excessively can have a negative effect on an application’s performance.</remarks>
		public void Flush()
		{
			//flush changes to memory
			int result = RegFlushKey(m_handle);
		}
		#endregion

		#region Close
		/// <summary>
		/// Closes the key and flushes it to storage if the contents have been modified.
		/// </summary>
		/// <remarks>Calling this method on system keys will have no effect, since system keys should never be closed.
		/// This method does nothing if you call it on an instance of <b>RegistryKey</b> that is already closed.</remarks>
		public void Close()
		{
			if(m_isroot)
			{
				//we do not close root keys - because they can not be reopened
				//close will fail silently - no exception is raised
			}
			else
			{
				//ignore if already closed
				if(CheckHKey())
				{
					//close the key
					int result = RegCloseKey(m_handle);

					if(result==0)
					{
						//set handle to invalid value
						m_handle = 0;
					}
					else
					{
						//error occured
						throw new ExternalException("Error closing RegistryKey");
					}
				}
			}
		}
		#endregion


		#region Create SubKey
		/// <summary>
		///  Creates a new subkey or opens an existing subkey.
		///  The string subKey is not case-sensitive.
		/// </summary>
		/// <param name="subkey">Name or path of subkey to create or open.</param>
		/// <returns>Returns the subkey, or null if the operation failed.</returns>
		/// <exception cref="System.ArgumentNullException">The specified subkey is null.</exception>
		/// <exception cref="System.ArgumentException">The length of the specified subkey is longer than the maximum length allowed (255 characters).</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey on which this method is being invoked is closed (closed keys cannot be accessed).</exception>
		public RegistryKey CreateSubKey(string subkey)
		{
			return CreateSubKey(subkey, false);
		}
		/// <summary>
		///  Creates a new subkey or opens an existing subkey.
		///  The string subKey is not case-sensitive.
		///  <para><b>New in v1.3</b></para>
		/// </summary>
		/// <param name="subkey">Name or path of subkey to create or open.</param>
		/// <param name="createVolatile">If true creates a volatile key (Requires Windows CE 5.0).</param>
		/// <returns>Returns the subkey, or null if the operation failed.</returns>
		/// <exception cref="System.ArgumentNullException">The specified subkey is null.</exception>
		/// <exception cref="System.ArgumentException">The length of the specified subkey is longer than the maximum length allowed (255 characters).</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey on which this method is being invoked is closed (closed keys cannot be accessed).</exception>
		public RegistryKey CreateSubKey(string subkey, bool createVolatile)
		{
			//check handle is valid
			if(CheckHKey())
			{
				//check subkey is not null
				if(subkey!=null)
				{
					//check subkey length
					if(subkey.Length < 256)
					{
						//handle to new registry key
						uint newhandle = 0;

						//key disposition - did this create a new key or open an existing key
						KeyDisposition kdisp = 0;

						//options
						RegOptions options = 0;
						if(createVolatile)
						{
							options = RegOptions.Volatile;
						}

						//create new key
						int result = RegCreateKeyEx(m_handle, subkey, 0, null, options, 0, IntPtr.Zero, ref newhandle, ref kdisp);

						if(result==0)
						{
							//success return the new key
							return new RegistryKey(newhandle, m_name + "\\" + subkey, true, false);
						}
						else
						{
							throw new ExternalException("An error occured creating the registry key.");
						}
					}
					else
					{
						//name is more than 255 chars
						throw new ArgumentException("The length of the specified subkey is longer than the maximum length allowed (255 characters).");
					}
				}
				else
				{
					throw new ArgumentNullException("The specified subkey is null.");
				}
			}
			else
			{
				//registry key is closed
				throw new ObjectDisposedException("The RegistryKey on which this method is being invoked is closed (closed keys cannot be accessed).");
			}
		}
		#endregion

		#region Open SubKey
		/// <summary>
		/// Retrieves a subkey as read-only.
		/// </summary>
		/// <param name="name">Name or path of subkey to open.</param>
		/// <returns>The subkey requested, or null if the operation failed.</returns>
		public RegistryKey OpenSubKey(string name)
		{
			return OpenSubKey(name, false);
		}
		/// <summary>
		/// Retrieves a specified subkey.
		/// </summary>
		/// <param name="name">Name or path of subkey to open.</param>
		/// <param name="writable">Set to true if you need write access to the key.</param>
		/// <returns>The subkey requested, or null if the operation failed.</returns>
		/// <exception cref="System.ArgumentNullException">name is null.</exception>
		/// <exception cref="System.ArgumentException">The length of the specified subkey is longer than the maximum length allowed (255 characters).</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public RegistryKey OpenSubKey(string name, bool writable)
		{
			//check handle is valid
			if(CheckHKey())
			{
				//check name is not null
				if(name!=null)
				{
					//check length
					if(name.Length < 256)
					{
						//handle to receive new key
						uint newhandle = 0;

						int result = RegOpenKeyEx(m_handle, name, 0, 0, ref newhandle);

						if(result==0)
						{
							return new RegistryKey(newhandle, m_name + "\\" + name, writable, false);
						}
						else
						{
							//desktop model returns null of key not found
							return null;
							//throw new ExternalException("An error occured retrieving the registry key");
						}
					}
					else
					{
						throw new ArgumentException("The length of the specified subkey is longer than the maximum length allowed (255 characters).");
					}
				}
				else
				{
					throw new ArgumentNullException("name is null.");
				}
			}
			else
			{
				throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
			}
		}
		#endregion

		#region Delete SubKey
		/// <summary>
		/// Deletes the specified subkey. The string subkey is not case-sensitive.
		/// </summary>
		/// <param name="subkey">Name of the subkey to delete.</param>
		/// <exception cref="System.ArgumentException">The specified subkey is not a valid reference to a registry key.</exception>
		/// <exception cref="System.ArgumentNullException">The subkey is null.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey on which this method is being invoked is closed (closed keys cannot be accessed).</exception>
		public void DeleteSubKey(string subkey)
		{
			DeleteSubKey(subkey, true);
		}
		/// <summary>
		/// Deletes the specified subkey. The string subkey is not case-sensitive.
		/// </summary>
		/// <param name="subkey">Name of the subkey to delete.</param>
		/// <param name="throwOnMissingSubKey">Indicates whether an exception should be raised if the specified subkey cannot be found.
		/// If this argument is true and the specified subkey does not exist then an exception is raised.
		/// If this argument is false and the specified subkey does not exist, then no action is taken</param>
		/// <exception cref="System.ArgumentException">The specified subkey is not a valid reference to a registry key (and throwOnMissingSubKey is true).</exception>
		/// <exception cref="System.ArgumentNullException">The subkey is null.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey on which this method is being invoked is closed (closed keys cannot be accessed).</exception>
		public void DeleteSubKey(string subkey, bool throwOnMissingSubKey)
		{
			if(subkey==null || subkey=="")
			{
				throw new ArgumentNullException("The subkey is null");
			}
			else
			{
				if(CheckHKey())
				{
					//delete the subkey
					int result = RegDeleteKey(m_handle, subkey);

					//if operation failed
					if(result != 0)
					{
						//only throw if flag was set
						if(throwOnMissingSubKey)
						{
							throw new ArgumentException("The specified subkey is not a valid reference to a registry key");
						}
					}
				}
				else
				{
					//key is closed
					throw new ObjectDisposedException("The RegistryKey on which this method is being invoked is closed (closed keys cannot be accessed).");
				}
			}
		}
		#endregion

		#region Delete SubKey Tree
		/// <summary>
		///  Deletes a subkey and any child subkeys recursively.
		///  The string subKey is not case-sensitive.
		/// </summary>
		/// <param name="subkey">Subkey to delete.</param>
		/// <exception cref="System.ArgumentNullException">The subkey parameter is null.</exception>
		/// <exception cref="System.ArgumentException">Deletion of a root hive is attempted. 
		/// The subkey parameter does not match a valid registry subkey.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public void DeleteSubKeyTree(string subkey)
		{
			//call delete subkey - this will delete all sub keys autmoatically
			DeleteSubKey(subkey, true);
		}
		#endregion

		#region Get SubKey Names
		/// <summary>
		/// Retrieves an array of strings that contains all the subkey names.
		/// </summary>
		/// <returns>An array of strings that contains the names of the subkeys for the current key.</returns>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public string[] GetSubKeyNames()
		{
			if(CheckHKey())
			{
				//error/success returned by RegKeyEnumEx
				int result = 0;
				//store the names
				System.Collections.ArrayList subkeynames = new System.Collections.ArrayList();
				int index = 0;

				//buffer to store the name
				char[] buffer = new char[256];
				int keynamelen = buffer.Length;

				//enumerate sub keys
				result = RegEnumKeyEx(m_handle, index, buffer, ref keynamelen, 0, null, 0, 0);
				
				//while there are more key names available
				while(result != ERROR_NO_MORE_ITEMS)
				{
					//add the name to the arraylist
					subkeynames.Add(new string(buffer, 0, keynamelen));

					//increment index
					index++;

					//reset length available to max
					keynamelen = buffer.Length;

					//retrieve next key name
					result = RegEnumKeyEx(m_handle, index, buffer, ref keynamelen, 0, null, 0, 0);
				}

				//sort the results
				subkeynames.Sort();
				
				//return a fixed size string array
				return (string[])subkeynames.ToArray(typeof(string));
			}
			else
			{
				throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
			}
		}
		#endregion

		#region SubKey Count
		/// <summary>
		/// Retrieves the count of subkeys at the base level, for the current key.
		/// </summary>
		/// <exception cref="System.ObjectDisposedException"> The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public int SubKeyCount
		{
			get
			{
				//check handle
				if(CheckHKey())
				{
					int subkeycount;
					int valuescount;
					int maxsubkeylen;
					int maxsubkeyclasslen;
					int maxvalnamelen;
					int maxvallen;
					char[] name = new char[256];
					int namelen = name.Length;

					if(RegQueryInfoKey(m_handle, name, ref namelen, 0, out subkeycount, out maxsubkeylen, out maxsubkeyclasslen, out valuescount, out maxvalnamelen, out maxvallen, 0, 0)==0)
					{
						return subkeycount;
					}
					else
					{
						throw new ExternalException("Error retrieving registry properties");
					}
				}
				else
				{
					throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
				}
			}
		}
		#endregion


		#region Get Value
		/// <summary>
		/// Retrieves the data associated with the specified value, or null if the value does not exist.
		/// </summary>
		/// <param name="name">Name of the value to retrieve.</param>
		/// <returns>The data associated with name , or null if the value does not exist.</returns>
		/// <exception cref="System.ArgumentException">The RegistryKey being manipulated does not exist.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public object GetValue(string name)
		{
			return GetValue(name, null);
		}
		/// <summary>
		/// Retrieves the specified value, or the default value you provide if the specified value is not found. 
		/// </summary>
		/// <param name="name">Name of the value to retrieve.</param>
		/// <param name="defaultValue">Value to return if name does not exist.</param>
		/// <returns>The data associated with name, or defaultValue if name is not found.</returns>
		/// <exception cref="System.ArgumentException">The RegistryKey being manipulated does not exist.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public object GetValue(string name, object defaultValue)
		{
			if(CheckHKey())
			{
				RegistryValueKind kt = 0;
				//support up to 256 characters (512 bytes)
				byte[] buffer;

				//pass in buffer size
				int size = 0;

				//determine validity and get required buffer size
				int result = RegQueryValueEx(m_handle, name, 0, ref kt, null, ref size);

				//catch value name not valid
				if(result==87)
				{
					return defaultValue;
				}

				//call api again with valid buffer size
				buffer = new byte[size];
				result = RegQueryValueEx(m_handle, name, 0, ref kt, buffer, ref size);

				//return appropriate type of value
				switch(kt)
				{
					case RegistryValueKind.Binary:
						//return binary data (byte[])
						return buffer;

					case RegistryValueKind.DWord:
						//updated return value as Int32
						return System.BitConverter.ToInt32(buffer, 0);

					case RegistryValueKind.ExpandString:
					case RegistryValueKind.String:
						//return value as a string (trailing null removed)
						string val = System.Text.Encoding.Unicode.GetString(buffer, 0, size);
						return val.Substring(0, val.IndexOf('\0'));

					case RegistryValueKind.MultiString:
						//get string of value
						string raw = System.Text.Encoding.Unicode.GetString(buffer, 0, size);
						//multistring ends with double nulls
						raw = raw.Substring(0, raw.IndexOf("\0\0"));
						//return array of substrings between single nulls
						return raw.Split('\0');
					default:
						return defaultValue;
				}
	
			}
			else
			{
				throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
			}
		}
		#endregion

		#region Get Value Kind
		/// <summary>
		/// Retrieves the registry data type of the value associated with the specified name.
		/// <para><b>New in v1.3</b></para>
		/// </summary>
		/// <param name="name">The name of the value whose registry data type is to be retrieved.</param>
		/// <returns>A <see cref="RegistryValueKind"/> value representing the registry data type of the value associated with name.</returns>
		public RegistryValueKind GetValueKind(string name)
		{
			if(CheckHKey())
			{

				RegistryValueKind kt = RegistryValueKind.Unknown;

				//pass in buffer size
				int size = 0;

				//determine validity and get required buffer size
				int result = RegQueryValueEx(m_handle, name, 0, ref kt, null, ref size);

				//catch value name not valid
				if(result != 0)
				{
					throw new Win32Exception(Marshal.GetLastWin32Error(), "Error retrieving value type");
				}
				else
				{
					return kt;
				}
			}
			else
			{
				throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
			}
		}
		#endregion

		#region Set Value
		/// <summary>
		/// Sets the specified value.
		/// </summary>
		/// <param name="name">Name of value to store data in.</param>
		/// <param name="value">Data to store.</param>
		/// <exception cref="System.ArgumentException">The length of the specified value is longer than the maximum length allowed (255 characters).</exception>
		/// <exception cref="System.ArgumentNullException">value is null.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being set is closed (closed keys cannot be accessed).</exception>
		/// <exception cref="System.UnauthorizedAccessException">The RegistryKey being set is readonly, and cannot be written to (for example, it is a root-level node, or the key has not been opened with write-access).</exception>
		public void SetValue(string name, object value)
		{
			if(m_writable)
			{
				if(CheckHKey())
				{
					RegistryValueKind type = 0;
					byte[] data;

					switch(value.GetType().ToString())
					{
						case "System.String":
							type = RegistryValueKind.String;
							data = System.Text.Encoding.Unicode.GetBytes((string)value + '\0');
							break;
						case "System.String[]":
							System.Text.StringBuilder sb = new System.Text.StringBuilder();
							foreach (string str in (string[])value)
								sb.Append(str + '\0');
							sb.Append('\0'); // terminated by two null characters
							type = RegistryValueKind.MultiString;
							data = System.Text.Encoding.Unicode.GetBytes(sb.ToString());
							break;
						case "System.Byte[]":
							type = RegistryValueKind.Binary;
							data = (byte[])value;
							break;
						case "System.Int32":
							type = RegistryValueKind.DWord;
							data = BitConverter.GetBytes((int)value);
							break;
						case "System.UInt32":
							type = RegistryValueKind.DWord;
							data = BitConverter.GetBytes((uint)value);
							break;
						default:
							throw new ArgumentException("value is not a supported type");
					}

					int size = data.Length;
					int result = RegSetValueEx(m_handle, name, 0, type, data, size);


					if(result!=0)
					{
						throw new ExternalException("Error writing to the RegistryKey");
					}
				}
				else
				{
					throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
				}
			}
			else
			{
				throw new UnauthorizedAccessException("Cannot set value on RegistryKey which was opened as ReadOnly");
			}
		}

		/// <summary>
		/// Sets the value of a name/value pair in the registry key, using the specified registry data type.
		/// <para><b>New in v1.3</b></para>
		/// </summary>
		/// <param name="name">Name of value to store data in.</param>
		/// <param name="value">Data to store.</param>
		/// <param name="valueKind">The registry data type to use when storing the data.</param>
		/// <exception cref="System.ArgumentException">The length of the specified value is longer than the maximum length allowed (255 characters).</exception>
		/// <exception cref="System.ArgumentNullException">value is null.</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being set is closed (closed keys cannot be accessed).</exception>
		/// <exception cref="System.UnauthorizedAccessException">The RegistryKey being set is readonly, and cannot be written to (for example, it is a root-level node, or the key has not been opened with write-access).</exception>
		public void SetValue(string name, object value, RegistryValueKind valueKind)
		{
			if(m_writable)
			{
				if(CheckHKey())
				{
					byte[] data;

					switch(valueKind)
					{
						case RegistryValueKind.String:
							data = System.Text.Encoding.Unicode.GetBytes((string)value + '\0');
							break;
						case RegistryValueKind.MultiString:
							System.Text.StringBuilder sb = new System.Text.StringBuilder();
							
							foreach (string str in (string[])value)
								sb.Append(str + '\0');
							sb.Append('\0'); // terminated by two null characters
							data = System.Text.Encoding.Unicode.GetBytes(sb.ToString());
							break;
						case RegistryValueKind.Binary:
							data = (byte[])value;
							break;
						case RegistryValueKind.DWord:
							if(value is UInt32)
							{
								data = BitConverter.GetBytes((uint)value);
							}
							else
							{
								data = BitConverter.GetBytes(Convert.ToInt32(value));
							}
							break;

						default:
							SetValue(name, value);
							return;
					}

					int size = data.Length;
					int result = RegSetValueEx(m_handle, name, 0, valueKind, data, size);


					if(result!=0)
					{
						throw new Win32Exception(Marshal.GetLastWin32Error(), "Error writing to the RegistryKey");
					}
				}
				else
				{
					throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
				}
			}
			else
			{
				throw new UnauthorizedAccessException("Cannot set value on RegistryKey which was opened as ReadOnly");
			}
		}
		#endregion

		#region Delete Value
		/// <summary>
		/// Deletes the specified value from this key.
		/// </summary>
		/// <param name="name">Name of the value to delete.</param>
		/// <exception cref="System.ArgumentException">name is not a valid reference to a value (and throwOnMissingValue is true) or name is null</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		/// <exception cref="System.UnauthorizedAccessException">The RegistryKey being manipulated is readonly.</exception>
		public void DeleteValue(string name)
		{
			DeleteValue(name, true);
		}
		/// <summary>
		/// Deletes the specified value from this key.
		/// </summary>
		/// <param name="name">Name of the value to delete.</param>
		/// <param name="throwOnMissingValue">Indicates whether an exception should be raised if the specified value cannot be found.
		/// If this argument is true and the specified value does not exist then an exception is raised.
		/// If this argument is false and the specified value does not exist, then no action is taken</param>
		/// <exception cref="System.ArgumentException">name is not a valid reference to a value (and throwOnMissingValue is true) or name is null</exception>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		/// <exception cref="System.UnauthorizedAccessException">The RegistryKey being manipulated is readonly.</exception>
		public void DeleteValue(string name, bool throwOnMissingValue)
		{
			if(m_writable)
			{
				if(CheckHKey())
				{
					if(name!=null)
					{
						//call api function to delete value
						int result = RegDeleteValue(m_handle, name);

						//check for error in supplied name
						if(result==87)
						{
							//only throw exception if flag is set
							if(throwOnMissingValue)
							{
								//name doesnt exist
								throw new ArgumentException("name is not a valid reference to a value (and throwOnMissingValue is true) or name is null");
							}
						}
					}
					else
					{
						//name is null
						throw new ArgumentException("name is null");
					}
				}
				else
				{
					//handle is closed throw exception
					throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
				}
			}
			else
			{
				//key is readonly throw exception
				throw new UnauthorizedAccessException("Cannot delete a value from a RegistryKey opened as ReadOnly.");
			}

		}
		#endregion

		#region Get Value Names
		/// <summary>
		/// Retrieves an array of strings that contains all the value names associated with this key.
		/// </summary>
		/// <returns>An array of strings that contains the value names for the current key.</returns>
		/// <remarks>If no value names for the key are found, an empty array is returned.
		/// <para>All RegistryKeys are assigned a default value.
		/// This is not counted as a value name, and is not returned as part of the result set.</para></remarks>
		/// <exception cref="System.ObjectDisposedException">The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public string[] GetValueNames()
		{
			if(CheckHKey())
			{
				int result = 0;
				//store the names
				ArrayList valuenames = new ArrayList();
				int index = 0;
				//buffer to store the name
				char[] buffer = new char[256];
				int valuenamelen = buffer.Length;

				//get first value name
				result = RegEnumValue(m_handle, index, buffer, ref valuenamelen, 0, 0, null, 0);
				
				//enumerate sub keys
				while(result != ERROR_NO_MORE_ITEMS)
				{
					//add the name to the arraylist
					valuenames.Add(new string(buffer, 0, valuenamelen));
					//increment index
					index++;
					//reset length available to max
					valuenamelen = buffer.Length;

					//get next value name
					result = RegEnumValue(m_handle, index, buffer, ref valuenamelen, 0, 0, null, 0);
				}

				//sort the results
				valuenames.Sort();
				
				//return a fixed size string array
				return (string[])valuenames.ToArray(typeof(string));
			}
			else
			{
				//key is closed
				throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
			}
		}
		#endregion

		#region Value Count
		/// <summary>
		/// Retrieves the count of values in the key.
		/// </summary>
		/// <exception cref="System.ObjectDisposedException"> The RegistryKey being manipulated is closed (closed keys cannot be accessed).</exception>
		public int ValueCount
		{
			get
			{
				//check handle
				if(CheckHKey())
				{
					int subkeycount;
					int valuescount;
					int maxsubkeylen;
					int maxsubkeyclasslen;
					int maxvalnamelen;
					int maxvallen;
					char[] name = new char[256];
					int namelen = name.Length;

					if(RegQueryInfoKey(m_handle, name, ref namelen, 0, out subkeycount, out maxsubkeylen, out maxsubkeyclasslen, out valuescount, out maxvalnamelen, out maxvallen, 0, 0)==0)
					{
						return valuescount;
					}
					else
					{
						throw new ExternalException("Error retrieving registry properties");
					}
				}
				else
				{
					throw new ObjectDisposedException("The RegistryKey being manipulated is closed (closed keys cannot be accessed).");
				}
			}
		}
		#endregion


		#region Check HKey
		//used to check that the handle is a valid open hkey
		private bool CheckHKey()
		{
			if(m_handle==0)
			{
				return false;
			}
			else
			{
				return true;
			}
		}
		#endregion

		#region IDisposable Members
		/// <summary>
		/// Free up resources used by the RegistryKey
		/// </summary>
		public void Dispose()
		{
			//close and save out data
			this.Close();
		}
		#endregion


		#region KeyDisposition Enumeration
		// <summary>
		// Key disposition for RegCreateKey(Ex)
		// </summary>
		private enum KeyDisposition : int 
		{
			CreatedNewKey = 1, 
			OpenedExistingKey = 2 
		}
		#endregion	

		#region Reg Options
		[Flags()]
		private enum RegOptions
		{
			NonVolatile = 0,
			Volatile = 1,
			//CreateLink = 2,
			//BackupRestore = 4,
			//OpenLink = 8,
		}
		#endregion

		#region Registry P/Invokes

		//open key
		[DllImport("coredll.dll", EntryPoint="RegOpenKeyEx", SetLastError=true)] 
		private static extern int RegOpenKeyEx(
			uint hKey,
			string lpSubKey,
			int ulOptions,
			int samDesired,
			ref uint phkResult); 

		//create key
		[DllImport("coredll.dll", EntryPoint="RegCreateKeyEx", SetLastError=true)] 
		private static extern int RegCreateKeyEx(
			uint hKey,
			string lpSubKey,
			int lpReserved,
			string lpClass,
			RegOptions dwOptions,
			int samDesired,
			IntPtr lpSecurityAttributes,
			ref uint phkResult, 
			ref KeyDisposition lpdwDisposition); 

		//enumerate keys
		[DllImport("coredll.dll", EntryPoint="RegEnumKeyEx", SetLastError=true)]
		private static extern int RegEnumKeyEx(
			uint hKey,
			int iIndex, 
			char[] sKeyName,
			ref int iKeyNameLen, 
			int iReservedZero,
			byte[] sClassName,
			int iClassNameLenZero, 
			int iFiletimeZero);

		//enumerate values
		[DllImport("coredll.dll", EntryPoint="RegEnumValue", SetLastError=true)]
		private static extern int RegEnumValue(
			uint hKey,
			int iIndex,
			char[] sValueName, 
			ref int iValueNameLen,
			int iReservedZero,
			int iTypeZero, /*should take ref KeyType but we never want to restrict type when enumerating values*/
			byte[] byData,
			int iDataLenZero /*takes ref int but we dont need the value when enumerating the names*/);

		//query key info
		[DllImport("coredll.dll", EntryPoint="RegQueryInfoKey", SetLastError=true)]
		private static extern int RegQueryInfoKey(
			uint hKey,
			char[] lpClass,
			ref int lpcbClass, 
			int reservedZero,
			out int cSubkey, 
			out int iMaxSubkeyLen,
			out int lpcbMaxSubkeyClassLen,
			out int cValueNames,
			out int iMaxValueNameLen, 
			out int iMaxValueLen,
			int securityDescriptorZero,
			int lastWriteTimeZero);

		//get value
		[DllImport("coredll.dll", EntryPoint="RegQueryValueEx", SetLastError=true)] 
		private static extern int RegQueryValueEx(
			uint hKey,
			string lpValueName,
			int lpReserved, 
			ref RegistryValueKind lpType,
			byte[] lpData,
			ref int lpcbData); 

		//set value
		[DllImport("coredll.dll", EntryPoint="RegSetValueExW", SetLastError=true)] 
		private static extern int RegSetValueEx(
			uint hKey,
			string lpValueName,
			int lpReserved, 
			RegistryValueKind lpType,
			byte[] lpData,
			int lpcbData); 

		//close key
		[DllImport("coredll.dll", EntryPoint="RegCloseKey", SetLastError=true)] 
		private static extern int RegCloseKey(
			uint hKey);

		//delete key
		[DllImport("coredll.dll", EntryPoint="RegDeleteKey", SetLastError=true)]
		private static extern int RegDeleteKey(
			uint hKey,
			string keyName);

		//delete value
		[DllImport("coredll.dll", EntryPoint="RegDeleteValue", SetLastError=true)]
		private static extern int RegDeleteValue(
			uint hKey,
			string valueName);

		//flush key
		[DllImport("coredll.dll", EntryPoint="RegFlushKey", SetLastError=true)]
		private static extern int RegFlushKey(
			uint hKey );


		#endregion
	}
	#endregion

	#region Registry Value Kind Enumeration
	/// <summary>
	/// Specifies the data types to use when storing values in the registry, or identifies the data type of a value in the registry.
	/// <para><b>New in v1.3</b></para>
	/// </summary>
	/// <remarks>The RegistryValueKind enumeration defines the set of supported registry data types and the value that is used for unsupported types (Unknown).
	/// <para>Use the <see cref="RegistryKey.GetValueKind"/> method to determine the data type of a registry key value before retrieving the value.
	/// When you set a registry key value, use the <see cref="RegistryKey.SetValue"/> method to specify the registry data type explicitly.</para></remarks>
	public enum RegistryValueKind : int
	{
		/// <summary>
		/// Indicates an unsupported registry data type.
		/// Use this value to specify that SetValue should determine the appropriate registry data type when storing a name/value pair.
		/// </summary>
		Unknown = 0,
		/// <summary>
		/// Specifies a null-terminated string.
		/// This value is equivalent to the Win32 API registry data type REG_SZ.
		/// </summary>
		String = 1,
		/// <summary>
		/// Specifies a null-terminated string that contains unexpanded references to environment variables, such as %PATH%, that are expanded when the value is retrieved.
		/// This value is equivalent to the Win32 API registry data type REG_EXPAND_SZ.
		/// </summary>
		ExpandString = 2,
		/// <summary>
		/// Specifies binary data in any form.
		/// This value is equivalent to the Win32 API registry data type REG_BINARY.
		/// </summary>
		Binary = 3,
		/// <summary>
		/// Specifies a 32-bit binary number.
		/// This value is equivalent to the Win32 API registry data type REG_DWORD.
		/// </summary>
		DWord = 4,
		/// <summary>
		/// Specifies an array of null-terminated strings, terminated by two null characters.
		/// This value is equivalent to the Win32 API registry data type REG_MULTI_SZ.
		/// </summary>
		MultiString = 7,
		// <summary>
		// Specifies a 64-bit binary number.
		// This value is equivalent to the Win32 API registry data type REG_QWORD.
		// </summary>
		//QWord = 11,
	}
	#endregion
}

