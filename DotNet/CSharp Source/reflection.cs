// reflection.cs
//
// Helper for the system reflection
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

using System;
using System.IO;
using System.Reflection;
using System.Text;
using System.Collections.Generic;

#if !NETSTANDARD1_6  
using System.Runtime.InteropServices;
#endif

// ReSharper disable ClassNeverInstantiated.Global
// ReSharper disable ConvertIfStatementToNullCoalescingExpression
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable IntroduceOptionalParameters.Global
// ReSharper disable FieldCanBeMadeReadOnly.Global
// ReSharper disable UnusedMethodReturnValue.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable InlineOutVariableDeclaration
// ReSharper disable UseStringInterpolation
// ReSharper disable UseObjectOrCollectionInitializer
// ReSharper disable UseNullPropagation
// ReSharper disable MergeCastWithTypeCheck
// ReSharper disable UsePatternMatching
// ReSharper disable ArrangeAccessorOwnerBody

namespace TraceTool
{
    /// <summary>
    /// Helper for reflection. You can use it your project without TraceTool
    /// </summary>
    // ReSharper disable once ClassNeverInstantiated.Global
    public class ReflectionHelper
    {
        private static readonly Dictionary<Type, string> TypeNames = new Dictionary<Type, string>();    // list all short name for know types
        private static readonly Dictionary<string, string> OperatorsNames = new Dictionary<string, string>();  // list all operators

        static ReflectionHelper()
        {
            // store short name for know types.
            TypeNames.Add(typeof(void), "void");
            TypeNames.Add(typeof(object), "object");
            TypeNames.Add(typeof(string), "string");
            TypeNames.Add(typeof(sbyte), "sbyte");
            TypeNames.Add(typeof(byte), "byte");
            TypeNames.Add(typeof(short), "short");
            TypeNames.Add(typeof(ushort), "ushort");
            TypeNames.Add(typeof(int), "int");
            TypeNames.Add(typeof(uint), "uint");
            TypeNames.Add(typeof(long), "long");
            TypeNames.Add(typeof(ulong), "ulong");
            TypeNames.Add(typeof(Char), "char");
            TypeNames.Add(typeof(bool), "bool");
            TypeNames.Add(typeof(float), "float");
            TypeNames.Add(typeof(double), "double");
            TypeNames.Add(typeof(decimal), "decimal");

            OperatorsNames.Add("op_Addition", "operator +");
            OperatorsNames.Add("op_BitwiseAnd", "operator &");
            OperatorsNames.Add("op_BitwiseOr", "operator |");
            OperatorsNames.Add("op_Decrement", "operator --");
            OperatorsNames.Add("op_Division", "operator /");
            OperatorsNames.Add("op_Equality", "operator ==");
            OperatorsNames.Add("op_ExclusiveOr", "operator ^");
            OperatorsNames.Add("op_False", "operator false");
            OperatorsNames.Add("op_GreaterThan", "operator >");
            OperatorsNames.Add("op_GreaterThanOrEqual", "operator >=");
            OperatorsNames.Add("op_Increment", "operator ++");
            OperatorsNames.Add("op_Inequality", "operator !=");
            OperatorsNames.Add("op_LessThan", "operator <");
            OperatorsNames.Add("op_LessThanOrEqual", "operator <=");
            OperatorsNames.Add("op_LogicalNot", "operator !");
            OperatorsNames.Add("op_Modulus", "operator %");
            OperatorsNames.Add("op_Multiply", "operator *");
            OperatorsNames.Add("op_OnesComplement", "operator ~");
            OperatorsNames.Add("op_Subtraction", "operator -");
            OperatorsNames.Add("op_True", "operator true");
            OperatorsNames.Add("op_UnaryNegation", "operator -");
            OperatorsNames.Add("op_UnaryPlus", "operator +");
            OperatorsNames.Add("op_Implicit", "operator Implicit");
            OperatorsNames.Add("op_Explicit", "operator Explicit");
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// return the name of the type or element type if reference
        /// </summary>
        public static string Type2ShortString(Type type)
        {
            try
            {
                if (type == null)
                    return "Null";

                if (IsArray(type))
                {
                    string result = Type2ShortString(type.GetElementType());    // recursive
                    result += '[';
                    for (int i = type.GetArrayRank(); i > 1; i--)
                        result += ',';
                    result += ']';
                    return result;
                    // see the ArrayBounds function below to get System.Array dimensions :
                }

                if (type.IsByRef)
                    return " ref " + Type2ShortString(type.GetElementType());   // recursive

                if (type.IsPointer)
                    return Type2ShortString(type.GetElementType()) + "*";       // recursive

                string name;
                if (TypeNames.ContainsKey(type))
                    name = TypeNames[type];
                else if (type.FullName != null)
                    name = type.FullName;
                else
                    name = type.Name;

                // remove unneeded assembly information [version,culture,mscorlib,...]
                int pos = name.IndexOf('[');
                if (pos != -1)
                    name = name.Substring(0, pos); // get the string before the first '[' char

                string strParam = "";
#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsGenericType)
#else
                if (type.IsGenericType)
#endif
                {
                    name = name + "<";
                    // type.IsGenericTypeDefinition 
                    // if true : Type is open (not gived) : typeof(templateTest<,>)
                    // if false : Generics parametersare gived : MyClass<int,int>
                    //            you can call GetGenericTypeDefinition to get the open type

                    int genpos = 0;
#if NETSTANDARD1_6
                  foreach (Type t in type.GetTypeInfo().GetGenericArguments())
#else
                    foreach (Type t in type.GetGenericArguments())
#endif
                    {
                        if (genpos == 0)
                            name += t.Name;
                        else
                            name += "," + t.Name;
                        genpos++;
                        if (t.IsGenericParameter)
                        {
                            // t.GenericParameterPosition is the parameter (constraint) position

                            string strParamPos = "";
                            string ch = "";

#if NETSTANDARD1_6
                          GenericParameterAttributes attributes = t.GetTypeInfo().GenericParameterAttributes;
#else
                            GenericParameterAttributes attributes = t.GenericParameterAttributes;
#endif
                            if ((attributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0)
                            {
                                strParamPos += ch + "class";
                                ch = ",";
                            }
                            if ((attributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0)
                            {
                                strParamPos += ch + "struct";
                                ch = ",";
                            }
                            // base types or interfaces that the parameter mut implement
#if NETSTANDARD1_6
                          Type[] derivationConstraints = t.GetTypeInfo().GetGenericParameterConstraints();
#else
                            Type[] derivationConstraints = t.GetGenericParameterConstraints();
#endif
                            foreach (Type derivationConstraint in derivationConstraints)
                            {
                                strParamPos += ch + derivationConstraint.Name;
                                ch = ",";
                            }
                            if ((attributes & GenericParameterAttributes.DefaultConstructorConstraint) != 0)
                            {
                                strParamPos += ch + "new()";
                                //ch = ",";
                            }

                            if (strParamPos != "")
                                strParam += " where " + t.Name + ":" + strParamPos;
                        }
                    }

                    name += ">" + strParam;
                }
                return name; //+  + results.ToString() + ">";
            }
            catch (Exception e)
            {
                return "ReflectionHelper.Type2ShortString threw an exception of type " + e.GetType();
            }
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the list of bounds : 1..2, 0..3 , 1..1
        /// </summary>
        // ReSharper disable once UnusedMember.Global
        public string ArrayBounds(Array array)
        {
            StringBuilder arrTitle = new StringBuilder();
            int dimCount = array.Rank;
            int[] boundsArray = new int[dimCount];

            arrTitle.Append(array.GetLowerBound(0)).Append("..").Append(array.GetUpperBound(0));
            boundsArray[0] = array.GetLowerBound(0);
            for (int i = 1; i < dimCount; i++)
            {
                arrTitle.Append(",").Append(array.GetLowerBound(i)).Append("..").Append(array.GetUpperBound(i));
                boundsArray[i] = array.GetLowerBound(i);   // set bounds array to low index
            }
            return arrTitle.ToString();
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the modifiers and the name of a type
        /// </summary>
        public static void Type2String(Type type, ref string strModifier, ref string strName)
        {
            try
            {
                if (type == null)
                    return;
                strName += Type2ShortString(type);

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsPublic || type.GetTypeInfo().IsNestedPublic)
#else
                if (type.IsPublic || type.IsNestedPublic)
#endif
                {
                    strModifier += "public ";
                }
#if NETSTANDARD1_6
              else if (type.GetTypeInfo().IsNestedPrivate)
#else
                else if (type.IsNestedPrivate)
#endif
                {
                    strModifier += "private ";
                }
#if NETSTANDARD1_6
              else if (type.GetTypeInfo().IsNestedAssembly)
#else
                else if (type.IsNestedAssembly)
#endif
                {
                    strModifier += "internal ";
                }
#if NETSTANDARD1_6
              else if (type.GetTypeInfo().IsNestedFamily)
#else
                else if (type.IsNestedFamily)
#endif
                {
                    strModifier += "protected ";
                }
#if NETSTANDARD1_6
              else if (type.GetTypeInfo().IsNestedFamORAssem)
#else
                else if (type.IsNestedFamORAssem)
#endif
                {
                    strModifier += "protected internal ";
                }
#if NETSTANDARD1_6
              else if (type.GetTypeInfo().IsNestedFamANDAssem)
#else
                else if (type.IsNestedFamANDAssem)
#endif
                {
                    strModifier += "protected internal ";
                }

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsSubclassOf(typeof(Delegate)) && type != typeof(MulticastDelegate))
#else
                if (type.IsSubclassOf(typeof(Delegate)) && type != typeof(MulticastDelegate))
#endif
                {
#if NETSTANDARD1_6
                  MethodInfo method = type.GetTypeInfo().GetMethod("Invoke");
#else
                    MethodInfo method = type.GetMethod("Invoke");
#endif
                    if (method == null)
                        return;
                    strModifier += "delegate " + Type2ShortString(method.ReturnType);
                    strName += '(' + MethodParams2String(method) + ")";
                    return;
                }

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsEnum)
#else
                if (type.IsEnum)
#endif
                {
                    strModifier += "enum ";
                    return;
                }

                // don't add the "struct" keyword for primitive
#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsValueType && type.GetTypeInfo().IsPrimitive == false)
#else
                if (type.IsValueType && type.IsPrimitive == false)
#endif
                {
                    strModifier += "struct ";
                    return;
                }

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsInterface)
#else
                if (type.IsInterface)
#endif
                {
                    strModifier += "interface ";
                    return;
                }

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsAbstract) strModifier += "abstract ";
#else
                if (type.IsAbstract) strModifier += "abstract ";
#endif

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsSealed)   strModifier += "sealed ";
#else
                if (type.IsSealed) strModifier += "sealed ";
#endif

#if NETSTANDARD1_6
              if (type.GetTypeInfo().IsClass)    strModifier += "class " ;
#else
                if (type.IsClass) strModifier += "class ";
#endif

            }
            catch (Exception)
            {
                // ignored
            }
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the modifiers and the name of a constructor (similar to a method)
        /// </summary>

        // ReSharper disable RedundantAssignment
        public static void Constructor2String(ConstructorInfo ctor, ref string strModifier, ref string strName)
        // ReSharper restore RedundantAssignment
        {
            //if (strModifier == null) throw new ArgumentNullException(nameof(strModifier));

            strModifier = "?";
            strName = "?";
            try
            {
                strModifier = Method2StringModifier(ctor);
                //  ctor.Name can be 'ctor' or 'cctor' Beter is to use the class name
                if (ctor.DeclaringType != null)
                    strName = ctor.DeclaringType.Name;
                strName += " (" + MethodParams2String(ctor) + ")";
            }
            catch (Exception)
            {
                // ignored
            }
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// return the modifiers of a field
        /// </summary>
        public static string GetFieldModifier(FieldInfo field)
        {
            string result = "";
            try
            {
                if (field.IsPrivate) result += "private ";
                if (field.IsPublic) result += "public ";
                if (field.IsFamily) result += "protected ";
                if (field.IsAssembly) result += "internal ";
                if (field.IsFamilyOrAssembly) result += "protected internal ";
                if (field.IsFamilyAndAssembly) result += "protected and internal ";
                if (field.IsStatic && field.IsLiteral)
                {
                    result += "const ";
                }
                else
                {
                    if (field.IsStatic) result += "static ";
                    if (field.IsInitOnly) result += "readonly ";
                }
            }
            catch (Exception)
            {
                result += "???";
            }
            return result;
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// return the modifiers and the name of a field
        /// </summary>
        public static void Field2String(FieldInfo field, ref string strModifier, ref string strName)
        {
            try
            {
                strModifier = GetFieldModifier(field);
                strModifier += Type2ShortString(field.FieldType);
                // add the declaring type if not the actual type
#if !NETSTANDARD1_6
                if (field.DeclaringType != field.ReflectedType)
#endif
                    strName += Type2ShortString(field.DeclaringType) + "::";
                strName += field.Name;
            }
            catch (Exception)
            {
                // ignored
            }
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the modifiers and the name of a property
        /// </summary>
        public static void Property2String(PropertyInfo field, ref string strModifier, ref string strType, ref string strName, ref bool hasPublic)
        {
            try
            {
                // get the "get" method of the property, if don't exist, take the "set" one
                MethodInfo getMethod = field.GetGetMethod(true);
                MethodInfo setMethod = field.GetSetMethod(true);
                MethodInfo oneMethod;

                // if the get or set method is public, HasPublic become true
                if (getMethod != null)
                    hasPublic = getMethod.IsPublic;
                if (setMethod != null)
                    if (setMethod.IsPublic)
                        hasPublic = true;

                if (getMethod != null)
                    oneMethod = getMethod;
                else
                    oneMethod = setMethod;

                // retreive private, public, abstract, ... of that method
                if (oneMethod != null)
                    strModifier += Method2StringModifier(oneMethod);

                // add it the return type of the field (same as for the get method)
                strType = Type2ShortString(field.PropertyType);

                // add the declaring type if not the actual type
#if !NETSTANDARD1_6
                if (field.DeclaringType != field.ReflectedType)
#endif
                    strName += Type2ShortString(field.DeclaringType) + "::";

                // get the name of the property or the parameters in case of indexed parameters
                ParameterInfo[] parameters = field.GetIndexParameters();
                if (parameters.Length == 0)
                    strName += field.Name + " ";
                else
                    strName += "this [" + MethodParams2String(oneMethod) + "] ";

                // indicate the read/write accessor
                if (field.CanRead)
                    strName += "{get} ";
                if (field.CanWrite)
                    strName += "{set} ";
            }
            catch (Exception)
            {
                // ignored
            }
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the modifiers and the name of an event
        /// </summary>
        public static void Event2String(EventInfo field, ref string strModifier, ref string strName)
        {
            try
            {
                // get the "add" method of the event
                MethodInfo oneMethod = field.GetAddMethod(true);

                // retreive private, public, abstract, ... of that method
                if (oneMethod != null)
                    strModifier += Method2StringModifier(oneMethod);

                if (field.IsMulticast)
                    strModifier += "[multi] ";

                strModifier += "event ";

                strModifier += Type2ShortString(field.EventHandlerType);

                // add the declaring type if not the actual type
#if !NETSTANDARD1_6
                if (field.DeclaringType != field.ReflectedType)
#endif
                    strName += Type2ShortString(field.DeclaringType) + "::";

                strName += field.Name + " ";
            }
            catch (Exception)
            {
                // ignored
            }
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Give the modifiers and the name of a method
        /// if the method is an operator, the method return true
        /// </summary>
        public static bool Method2String(MethodInfo method, ref string strModifier, ref string strName)
        {
            bool result = false;
            try
            {
                // get the modifier (public, ...)
                strModifier += Method2StringModifier(method);
                // add the return type
                strModifier += Type2ShortString(method.ReturnType);
                // add the declaring type if not the actual type
#if !NETSTANDARD1_6
                if (method.DeclaringType != method.ReflectedType)
#endif
                    strName = Type2ShortString(method.DeclaringType) + "::";

                // get the method name

                string methodName;
                if (OperatorsNames.ContainsKey(method.Name))
                {
                    methodName = OperatorsNames[method.Name];
                    result = true;
                }
                else
                {
                    methodName = method.Name;
                    //result = false;
                }

                //Console.WriteLine("\tIs this a generic method definition? {0}", method.IsGenericMethodDefinition);
                //Console.WriteLine("\tIs it a generic method? {0}", method.IsGenericMethod);
                //Console.WriteLine("\tDoes it have unassigned generic parameters? {0}", method.ContainsGenericParameters);

                //// If this is a generic method, display its type arguments.

                string strParam = "";
                if (method.IsGenericMethod)
                {
                    methodName += "<";
                    int genpos = 0;
                    foreach (Type t in method.GetGenericArguments())
                    {
                        if (genpos == 0)
                            methodName += t.Name;
                        else
                            methodName += "," + t.Name;
                        genpos++;
                        if (t.IsGenericParameter)
                        {
                            // t.DeclaringMethod : MethodBase
                            // t.GenericParameterPosition is the parameter (constraint) position

                            string strParamPos = "";
                            string ch = "";

#if NETSTANDARD1_6
                          GenericParameterAttributes attributes = t.GetTypeInfo(). GenericParameterAttributes;
#else
                            GenericParameterAttributes attributes = t.GenericParameterAttributes;
#endif
                            if ((attributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0)
                            {
                                strParamPos += ch + "class";
                                ch = ",";
                            }
                            if ((attributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0)
                            {
                                strParamPos += ch + "struct";
                                ch = ",";
                            }
                            // base types or interfaces that the parameter mut implement
#if NETSTANDARD1_6
                          Type[] derivationConstraints = t.GetTypeInfo().GetGenericParameterConstraints();
#else
                            Type[] derivationConstraints = t.GetGenericParameterConstraints();
#endif
                            foreach (Type derivationConstraint in derivationConstraints)
                            {
                                strParamPos += ch + derivationConstraint.Name;
                                ch = ",";
                            }

                            if ((attributes & GenericParameterAttributes.DefaultConstructorConstraint) != 0)
                            {
                                strParamPos += ch + "new()";
                                //ch = ",";
                            }
                            if (strParamPos != "")
                                strParam += " where " + t.Name + ":" + strParamPos;
                        }  // if (t.IsGenericParameter)
                    } // foreach (Type t in method.GetGenericArguments())
                    methodName += ">";
                }

                // add parameters
                strName += methodName + " (" + MethodParams2String(method) + ")" + strParam;
            }
            catch (Exception)
            {
                // ignored
            }
            return result;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the modifier (private,...) of a method (without the method type and name)
        /// called by Property2String,Constructor2String and Method2String
        /// </summary>
        // ReSharper disable once MemberCanBePrivate.Global
        public static string Method2StringModifier(MethodBase oneMethod)
        {
            string strModifier = "";

            try
            {
                MethodAttributes attrs = oneMethod.Attributes;
                if (oneMethod.IsPrivate) strModifier += "private ";
                if (oneMethod.IsPublic) strModifier += "public ";
                if (oneMethod.IsFamily) strModifier += "protected ";
                if (oneMethod.IsStatic) strModifier += "static ";
                if (oneMethod.IsAssembly) strModifier += "internal ";
                if (oneMethod.IsFamilyOrAssembly) strModifier += "protected internal ";
                if (oneMethod.IsFamilyAndAssembly) strModifier += "protected and internal ";

                // special case : if Virtual + final + NewSlot then it's an interface implementation
                // it's not possible to create (then NewSlot) a fct virtual and final : it's an interface fct
                if (oneMethod.IsVirtual && oneMethod.IsFinal && ((attrs & MethodAttributes.NewSlot) != 0))
                {
                    strModifier += "[interface] ";
                    return strModifier;
                }

                if (oneMethod.IsFinal)
                    strModifier += "final ";

                // virtual is a complicated case.
                // 'NewSlot means not redefined since the base class where the fct is defined
                // 'virtual' and not 'newslot' is then an 'override' over that fct
                if (oneMethod.IsVirtual)
                {
                    if ((attrs & MethodAttributes.NewSlot) != 0)   // not redefined
                        if (oneMethod.IsAbstract)
                            strModifier += "abstract ";              // only for type, no instance possible
                        else
                            strModifier += "virtual ";               // virtual fct not redefined
                    else                                           // redefined in sub class
                        if (oneMethod.IsAbstract)
                        strModifier += "abstract override";      // abtract fct redefined. Not tested
                    else
                        strModifier += "override ";              // virtual fct redefined

                }
            }
            catch (Exception)
            {
                // ignored
            }
            return strModifier;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the parameters name and type of a method
        /// </summary>
        // ReSharper disable once MemberCanBePrivate.Global
        public static string MethodParams2String(MethodBase method) // (ParameterInfo[] parameters)
        {
            string result = "";

            try
            {
                ParameterInfo[] parameters = method.GetParameters();
                for (int i = 0; i < parameters.Length; i++)
                {
                    ParameterInfo param = parameters[i];
                    if (i != 0)
                        result += ", ";
                    Type type = param.ParameterType;
                    // "params" modifier permit to convert a list of arguments to an array

#if NETSTANDARD1_6
                if (IsArray(type) && type.GetTypeInfo().IsDefined(typeof(ParamArrayAttribute), true))
#else
                    if (IsArray(type) && Attribute.IsDefined(param, typeof(ParamArrayAttribute), true))
#endif
                        result += "params ";

                    if (param.IsIn) result += "[In()] ";
                    if (param.IsOut) result += "[Out()] ";      // param.ParameterType will add "ref"
                    if (param.IsOptional) result += "[Optional()] ";

#if !NETSTANDARD1_6
                    if (param.IsLcid) result += "[Lcid()] ";
#endif
                    if (param.IsRetval) result += "[Retval()] ";

                    result += Type2ShortString(type) + " " + param.Name;    // note that Type2ShortString can add "ref" modifier
                }

                // var arg ...
                if (method.CallingConvention == CallingConventions.VarArgs)
                    result += ", __arglist";
            }
            catch (Exception e)
            {
                return "ReflectionHelper.MethodParams2String threw an exception of type " + e.GetType();
            }

            return result;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the parameters type of a method , usefull to retreive XML documentation for a method
        /// parenthesis are included only if parameters exists
        /// </summary>
        public static string MethodParamsType2String(MethodBase method) // (ParameterInfo[] parameters)
        {
            string result;
            try
            {
                ParameterInfo[] parameters = method.GetParameters();
                if (parameters.Length == 0)
                    return "";

                result = "(";
                for (int i = 0; i < parameters.Length; i++)
                {
                    ParameterInfo param = parameters[i];
                    if (i != 0)
                        result += ",";
                    Type type = param.ParameterType;
                    result += type.ToString();
                }
                result += ")";
            }
            catch (Exception e)
            {
                return "ReflectionHelper.MethodParamsType2String threw an exception of type " + e.GetType();
            }
            return result;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// return the parameters type of a property , usefull to retreive XML documentation for a property
        /// parenthesis are included only if parameters exists
        /// </summary>
        public static string PropertyParamsType2String(PropertyInfo prop)
        {
            try
            {
                ParameterInfo[] parameters = prop.GetIndexParameters();
                if (parameters.Length == 0)
                    return "";

                var result = "(";
                for (int i = 0; i < parameters.Length; i++)
                {
                    ParameterInfo param = parameters[i];
                    if (i != 0)
                        result += ",";
                    Type type = param.ParameterType;
                    result += type.ToString();
                }
                if (parameters.Length != 0)
                    result += ")";

                return result;
            }
            catch (Exception e)
            {
                return "ReflectionHelper.PropertyParamsType2String threw an exception of type " + e.GetType();
            }
        }
        //----------------------------------------------------------------------

        /// <summary>
        /// indicate if the member is one of the "default" member.
        /// I don't know how to specify 2 "default", but ...
        /// </summary>
        public static bool IsDefaultMember(Type oType, MemberInfo member)
        {
#if NETSTANDARD1_6
          MemberInfo[] memberInfoArray = oType.GetTypeInfo().GetDefaultMembers();
#else
            MemberInfo[] memberInfoArray = oType.GetDefaultMembers();
#endif

            if (memberInfoArray.Length > 0)
                foreach (MemberInfo memberInfoObj in memberInfoArray)
                    if (ReferenceEquals(memberInfoObj, member))
                        return true;
            return false;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// indicate if the type is an aray.
        /// </summary>
        // ReSharper disable once MemberCanBePrivate.Global
        public static bool IsArray(Type type)
        {
            return type.IsArray && type != typeof(Array);
        }

        //----------------------------------------------------------------------

        ///// <summary>
        ///// indicate if the type is a delegate.
        ///// </summary>
        //public static bool IsDelegate (Type type)
        //{
        //   return type.IsSubclassOf(typeof(Delegate)) && type != typeof(MulticastDelegate);
        //}

        //----------------------------------------------------------------------
        /// <summary>
        /// return the name of the XML assembly documentation file for a type.Empty string if not found
        /// </summary>
        // ReSharper disable once UnusedMember.Global
        public static string AssemblyDocumentationFileName(Type oType)
        {
            try
            {
#if NETSTANDARD1_6
              string assemblyFileName = oType.GetTypeInfo().Assembly.Location ;
#else
                string assemblyFileName = oType.Assembly.Location;
#endif
                string xmlDocFileName = Path.ChangeExtension(assemblyFileName, ".xml");
                if (!File.Exists(xmlDocFileName))
                {
#if !NETSTANDARD1_6
                    xmlDocFileName = RuntimeEnvironment.GetRuntimeDirectory() + Path.GetFileName(xmlDocFileName);
                    if (!File.Exists(xmlDocFileName))
#endif
                        xmlDocFileName = "";
                }
                return xmlDocFileName;
            }
            catch (Exception e)  // 2015 10 06 : this method can return exception when type/assembly is generated dynamically 
            {
                return "ReflectionHelper.AssemblyDocumentationFileName threw an exception of type " + e.GetType();
            }
        }
    }  // class
}      // namespace
