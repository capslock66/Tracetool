namespace Cake.Tracetool
{
    using System;

    using Cake.Core;
    using Cake.Core.Annotations;

    using TraceTool ;

    /// <summary>
    /// Contains functionality for working with Tracetool.
    /// </summary>
    [CakeAliasCategory("Tracetool")]
    public static class TracetoolAlias
    {
        //[CakeMethodAlias] 
        //public static void Debug_SendObject(this ICakeContext context,string message,Object obj)
        //{
        //    TTrace.Debug.SendObject(message,obj) ;
        //}

        [CakeMethodAlias]  
        public static void DebugSend(this ICakeContext context, string message)
        {
            //TTrace.Debug.Send(message);
            //return 0;
        }

        [CakePropertyAlias]
        public static int TheAnswerToLife(this ICakeContext context)
        {
            return 5;
        }
    }
}
