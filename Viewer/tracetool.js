//------------------------------------------------------------------------------
//  TraceTool JavaScript API.
//  Author : Thierry Parent
//  Version : 13.2.6
//
//  sample use for NodeJs:    
//     var ttrace = require('tracetool') ;
//     ttrace.clearAll();
//     ttrace.debug.send("Hello world");
//
//  sample use for typescript using SystemJs:
//     systemjs.config.js : (assuming here tracetool.js is installed via nodeJs/Npm). Ensure format is set to 'global'
//     paths   : {..., 'npm:': 'node_modules/'                           , ...},
//     map     : {..., 'tracetool': 'npm:tracetool/lib/tracetool.js'     , ...},
//     packages: {..., tracetool: {format: 'global', exports: 'ttrace' } , ...}   
//
//     sample.ts :
//     import 'tracetool';  
//     var ttrace:any ;
//     ttrace = window["ttrace"] ;
//     ttrace.host = "127.0.0.1:85"; 
//     ttrace.debug.send("Hello world");
//
//
//   See http://www.codeproject.com/Articles/5498/TraceTool-The-Swiss-Army-Knife-of-Trace for full sample use
//------------------------------------------------------------------------------

// NodeJs v6.x, v7.x use Chrome V8 JavaScript engine (ES5), but support some ES6 features (ECMAScript 2015)
// https://kangax.github.io/compat-table/es6/
// http://node.green/
// https://nodejs.org/dist/latest-v6.x/docs/api/
// https://nodejs.org/dist/latest-v7.x/docs/api/

// Loader competitive (I will never finish with all of them)
// http://benmccormick.org/2015/05/28/moving-past-requirejs
// https://github.com/systemjs/systemjs/blob/master/docs/module-formats.md

// https://nodejs.org/docs/latest/api/modules.html   (CommonJS = NodeJs)
// http://requirejs.org                              (Asynchronous Module Definition = AMD)
// https://github.com/systemjs/systemjs              (AMD, CommonJS, ES6 , using promise) 
// http://browserify.org                             (CommonJS on browser)
// http://webpack.github.io                          (AMD, CommonJS)
// ...

"use strict";

// ReSharper disable once InconsistentNaming
var define;        // in case RequireJs is not used. Remove warning for use strict

(function (global)
{

// private tracetool vars 

//--------------------------------------------------------------------------------------------------------

var ttrace = null ;                        /** the tracetool api instance                               */ 

var ttraceScript = null;                   /** current trace script. Used by sendToClientUsingScript()  */
var headId = null;                         /** Shortcut to head. Used by sendToClientUsingScript()      */

var request ;                              /** nodejs library                                           */
var stackTrace;                            /** nodejs library                                           */
var uuid ;                                 /** nodejs library                                           */


var requestId = 0;                         /** number of request                                        */
var toSend = [];                           /** array of script to run.                                  */
var nbDone = 0;                            /** number of message send                                   */ 
var winTraceSingeton = null;               /** main WinTrace                                            */
var watchesSingeton = null;                /** main WinWatch                                            */
var clientId = "";                         /** Communication ID with the viewer                         */  
var host = "127.0.0.1:81";                 /** Full Url to TraceTool viewer (localhost:81 for example)  */
var traceClasses = {};                     /** Contains all tracetool classes                           */

var isChromeExtension ;                    /** library run under chrome as an extension                 */
var isBrowser;                             /** library run in a browser                                 */
var isNodeJs;                              /** library run in Node Js                                   */
var isRequireJs;                           /** library load by require.js                               */
var isCommonJS;                            /** library load by CommonJS                                 */
var isSystemJS;                            /** library load by SystemJS                                 */

//--------------------------------------------------------------------------------------------------------
    
detectEnvironment() ;

if (isRequireJs) 
{
  stackTrace = require('stack-trace');
  uuid       = require('uuid');
  clientId   = uuid().replace(/-/g, '');   // replace all(using g) '-' by empty string
}

if (isNodeJs)    
{
    request = require('request');
} else if (isBrowser) {
    ttraceScript = null;                                       
    headId = global.document.getElementsByTagName("head")[0];
}

//--------------------------------------------------------------------------------------------------------
// Private helpers : extend, getFormattedTime, ...
//--------------------------------------------------------------------------------------------------------

function detectEnvironment() 
{
    // Note : Trying to detect SystemJS to call register is not possible
    // because SystemJs transpile tracetool.js to detect the call to register as the FIRST statement (comments on top are ignored)
    // This will break the single file solution
    // The only solution is to configure systemJS to load tracetool.js as a 'global' format (ttrace is saved in global window object)

    isChromeExtension = false;
    isBrowser         = false;
    isNodeJs          = false;
    isRequireJs       = false;
    isCommonJS        = false;
    isSystemJS        = false;

    //console.log("chrome         (Chrome)    " , typeof chrome);
    //console.log("require        (AMD,NodeJs)" , typeof require);
    //console.log("define         (AMD)       " , typeof define);
    //console.log("process        (NodeJs)    " , typeof process);
    //console.log("module         (NodeJs)    " , typeof module);
    //console.log("System         (System JS) " , typeof System);
    //if (typeof module === "object") 
    //    console.log("module.exports (CommonJs) " , typeof module.exports);

    try {

        // ReSharper disable UndeclaredGlobalVariableUsing

        if (typeof require === "function") 
            isRequireJs = true;  // AMD module

        if ((typeof module === "object") && (typeof module.exports === "object"))
            isCommonJS = true ;  

        if ((typeof chrome === "object") && (typeof chrome.extension === "object"))
            isChromeExtension = true;
        else if ((typeof require === "function")       
            &&(typeof process === "object") 
            &&(typeof process.release === "object") 
            &&(typeof process.release.name === "string") 
            &&(process.release.name.search(/node|io.js/) !== -1) // process.release.name = 'node'
            )
            isNodeJs = true;  
        else
            isBrowser = true;
        // ReSharper restore UndeclaredGlobalVariableUsing
    }
    catch (e) {
        console.log("detectEnvironment exception", e);
    }
}

//--------------------------------------------------------------------------------------------------------

/** extend object with another 
* @param {Object} target Object that receive properties
* @param {Object} source Object that give properties
* @returns {Object} target
*/  
function extend(target,source)
{
    // ReSharper disable once MissingHasOwnPropertyInForeach
    for (var property in source)
        target[property] = source[property];
    return target;
} ;

//--------------------------------------------------------------------------------------------------------
/** Create a time and an optional date string 
* @returns {string} Date and time
*/
function getFormattedTime()
{
    var currentDate = new Date() ;
    var date = "" ;

    if (ttrace.options.sendDate === true)
        date = "" + currentDate.getFullYear() + intToStr(currentDate.getMonth(),2,'0') + intToStr(currentDate.getDay(),2,'0') + " " ;

    var h = currentDate.getHours() ;
    var m = currentDate.getMinutes() ;
    var s = currentDate.getSeconds() ;
    var n = currentDate.getMilliseconds() ;
    return date + intToStr(h,2,'0') + ':' + intToStr(m,2,'0') + ':' + intToStr(s,2,'0') + ':' + intToStr(n,3,'0') ;
} ;

//--------------------------------------------------------------------------------------------------------
/** Send the wintrace ArrayList to the viewer  
* @param {Array} commandList Messages list to send
* @param {string} winTraceId Wintrace ID
* @param {string} dateTime Date and time
* @returns {void}
*/
function sendToWinTraceClient (commandList, winTraceId , dateTime)
{
    // add current time.
    if (typeof(dateTime) == "undefined" || dateTime === null || dateTime === '')
        commandList.unshift (intToStr5(/*CST_MESSAGE_TIME*/ 304) + getFormattedTime()) ; // "HH:mm:ss:fff"
    else
        commandList.unshift (intToStr5(/*CST_MESSAGE_TIME*/ 304) + dateTime );

    // CST_USE_TREE MUST be inserted at the first position
    if (winTraceId != null && winTraceId !== "")
        commandList.unshift (intToStr5(/*CST_USE_TREE*/ 99) +  winTraceId);

    sendToClient (commandList);
} ;

//--------------------------------------------------------------------------------------------------------
/** send the winwatch ArrayList to the viewer 
* @param {Array} commandList Messages list to send
* @param {string} winWatchId Wintrace ID
* @param {string} dateTime Date and time
* @returns {void}
*/
function sendToWinWatchClient(commandList, winWatchId , dateTime)
{
    // add current time.
    if (typeof(dateTime) == "undefined" || dateTime === null || dateTime === '')
        commandList.unshift (intToStr5(/*CST_MESSAGE_TIME*/ 304) + getFormattedTime()) ; // "HH:mm:ss:fff"
    else
        commandList.unshift (intToStr5(/*CST_MESSAGE_TIME*/ 304) + dateTime );

    // CST_WINWATCH_ID MUST be inserted at the first position
    //if (winWatchId != null && winWatchId != "")
    commandList.unshift (intToStr5(/*CST_WINWATCH_ID*/ 111) +  winWatchId);

    sendToClient (commandList);
} ;


//--------------------------------------------------------------------------------------------------------
/** Convert the command list array to one or more string messages and add to queue
* @param {Array} commandList Messages list to send
* @returns {void}
*/
function sendToClient (commandList)
{
   var msgId = newGuid() ;
   var msg = commandList.join("\0") ;
   var msgLenth = msg.length ;
   if (msgLenth > 1000)
   {
      var part ;
      var partNum = 1 ;
      var partLen ;
      while (msgLenth > 0)
      {
         part = msg.substring(0, 1000) ;  // 0..999
         msg = msg.substring(1000) ;      // 1000..end

         msgLenth -= 1000 ;
         partLen = part.length ;
         if (partLen >= 1000)
            addMessage ({msgId:msgId, msg:part, partNum:partNum}) ;
         else
            addMessage ({msgId:msgId, msg:part, partNum:'Last'} ) ;
         partNum++;
      }
   } else {
      addMessage ({msgId:msgId, msg:msg, partNum:''}) ;
   }
} ;
   
//--------------------------------------------------------------------------------------------------------
/** Add a message to the waiting queue list. Run it if no other scripts are waiting 
* @param {Object} objMessage Object message : {msgId , msg, partNum}
* @returns {void}
*/
function addMessage(objMessage)
{
   objMessage.command = objMessage.command || "WMD" ;
   toSend.push(objMessage) ;          // add to end
   if (toSend.length === 1) 
      setTimeout(worker, 0);
} ;

//--------------------------------------------------------------------------------------------------------
/** Callback Timer function 
* @returns {void}
*/
function worker()
{
    //console.log("tracetool:worker " + toSend.length) ;
    var objMessage;
    if (toSend.length !== 0)
    {
        // no script is running.
        objMessage = toSend.shift(); // get first
        var hostUrl = "http://" + host + "/" + objMessage.command + "?msgId=" + objMessage.msgId + "&msg=" + encodeURIComponent(objMessage.msg);  // escape is deprecated. Generate bad encoding.
        if (objMessage.partNum !== "")
            hostUrl = hostUrl + "&partNum=" + objMessage.partNum;

        nbDone++;
        if (isNodeJs)
            sendToClientUsingRequest(hostUrl);
        else if (isBrowser)
            sendToClientUsingScript(hostUrl);
        else // if (IsChromeExtension)
            sendToClientUsingXmlHttpRequest(hostUrl);
    }
}

//--------------------------------------------------------------------------------------------------------
/** Create a script and execute it. This script send message to the viewer 
 * @param {string} hostUrl message 
 * @returns {void}
 */
function sendToClientUsingScript(hostUrl)
{
    // script is executed only when added to head
    var script = document.createElement("script");
    script.type = "text/javascript";    // ttraceScript.setAttribute("type",'text/javascript')
    script.setAttribute("id", "ttraceScript");
    script.setAttribute("name", "ttraceScript");
    script.src = hostUrl;
    script.timeSend = new Date();
    ttraceScript = script;        // set script as current script
    headId.appendChild(script);           // run the script

    /*
    generated element in head :
    
    <script type="text/javascript" id="ttraceScript" name="ttraceScript" 
       src="http://localhost:85/WMD?msgId=3_2&amp;msg= encoded Trace ...">
    </script>

    tracetool server will return this kind of script that will be run :
    ttrace._done("1_2","");
    ttrace.setClientID("123");
    */

    // check every 20 seconds if msg is send
    setTimeout(worker, 20000);
}

//--------------------------------------------------------------------------------------------------------
/** Called by ttrace._done() when script is send by the viewer   
 * @returns {void}
 */
function afterRun() 
{
    if (ttraceScript === null)
        return;
    // remove the script once loaded and executed.
    headId.removeChild(ttraceScript);
    ttraceScript = null;
    setTimeout(worker, 0);    // send next
};

//--------------------------------------------------------------------------------------------------------
/** send message to the viewer using XMLHttpRequest (Chrome extension solution)
* @param {string} hostUrl message
* @returns {void}
*/
function sendToClientUsingXmlHttpRequest(hostUrl)
{

    var xhr;
    try {
        xhr = new XMLHttpRequest();
    } catch (e1) {
        try {
            xhr = new ActiveXObject("Microsoft.XMLHTTP");
        } catch (e2) {
            xhr = new ActiveXObject("Msxml2.XMLHTTP");
        }
    }

    //xhr.addEventListener("load", function(e) {
    //  console.log("tracetool:load callback");
    //  }, false);

    xhr.addEventListener("error", function ( /*errorEvent*/) {
        //console.log("tracetool:error callback " + toSend.length);
        setTimeout(worker, 0);    // send next
    }, false);

    xhr.onload = function (onloadEvent) {
        // e : ProgressEvent
        // e.currentTarget : XMLHttpRequest
        var onloadRequest = onloadEvent.currentTarget;

        // With the js tracetool API for browser, the response for "UniqueClientId" command is a single line script 
        // Sample script for "UniqueClientId" : ttrace.setclientId("123");
        // Sample script for other messages   : ttrace._done("_1",""); 
        // On the browser, this script is executed.
        // For compatibility, on NodeJs , the Id is extracted from this script

        var script = onloadRequest.responseText;
        if (script.startsWith("ttrace.setClientID("))
            clientId = script.match(/\d+/)[0];  // extract first number anywhere in the string. Result is an array of string. first : 123
        //console.log("tracetool:onload " + toSend.length);
        setTimeout(worker, 0);    // send next
    }
    xhr.open("GET", hostUrl, true);     // xhrReq.open(method, url, async, user, password); 
    //xhr.setRequestHeader("Content-Type", "text/javascript");

    //xhr.setRequestHeader('Access-Control-Allow-Headers', '*');
    //xhr.setRequestHeader('Content-type', 'application/ecmascript');
    //xhr.setRequestHeader('Content-type', 'text/plain');
    //xhr.setRequestHeader('Access-Control-Allow-Origin', '*');

    xhr.send();                     // fire onload

    // check every 20 seconds if msg is send
    //setTimeout(worker, 20000);
}

//--------------------------------------------------------------------------------------------------------

/** send message to the viewer using nodeJs request
* @param {string} hostUrl message
* @returns {void}
*/  
function sendToClientUsingRequest (hostUrl)
{

   request(hostUrl, function (error, response) //, body)
   {
       if (!error && response.statusCode === 200) 
       {
           // With the js tracetool API for browser, the response for "UniqueClientId" command is a single line script 
           // Sample script for "UniqueClientId" : ttrace.setClientID("123");
           // Sample script for other messages   : ttrace._done("_1",""); 
           // On browser, this script is executed.
           // For compatibility, on NodeJs , the Id is extracted from this script

           var script = response.body ;
           if (script.startsWith("ttrace.setClientID("))
               clientId = script.match(/\d+/)[0];  // extract first number anywhere in the string. Result is an array of string. first : 123
               
           setTimeout(worker, 0);
       }
   });

   // check every 20 seconds if msg is send
   setTimeout(worker, 20000);
}

//--------------------------------------------------------------------------------------------------------
/** Remove extra left spaces 
* @param {string} str String to trim
* @returns {string} Trimmed string
*/
function lTrim(str)
{
    var k = 0 ;
    while( k<str.length && str.charAt(k)<=" ") k++ ;
    return str.substring(k, str.length);
}

//--------------------------------------------------------------------------------------------------------
/** Remove extra right spaces 
* @param {Object} str String to trim
* @returns {string} Trimmed string
*/
function rTrim(str)
{
    var k = str.length-1 ;
    while (k>=0 && str.charAt(k)<=" ") k-- ;
    return str.substring(0, k+1);
}

//--------------------------------------------------------------------------------------------------------
/** return the function of a code fragment 
* @param {Object} fctName code fragment
* @returns {string} function name
*/
function getFunctionName(fctName)
{
    fctName = rTrim(lTrim(fctName)) ;
    // [ecmascript code] or function MyClass()...
    if (fctName.indexOf('[ecmascript code]') === 0)
        return '?' ;

    if (fctName.indexOf('function ') === 0)// 0..8 (include space)
    {
        var p = 9 ;
        var endClassName = 1000 ;
        while (fctName.charAt(p) === ' ')
        p++;
        // search '(' char
        for(var c = p ; c < fctName.length; c++)
        {
        var ch = fctName.charAt(c) ;
        if (ch==='(') {
            endClassName = c;
            break;
        }
        }
        if (endClassName === p)  // nothing before ()
        return '?';

        return fctName.substring(9,endClassName);
    }
    var pos = fctName.indexOf("{") ;   // search for function body
    if (pos >= 0)
    {
        if (pos ===0)
        return "<unnamed function>" ;

        // remove function body
        fctName = fctName.substr(0,pos) ;

        pos = fctName.indexOf("(") ;   // remove function parameters
        if (pos !== -1)
        fctName = fctName.substr(0,pos) ;

        if (fctName.substr(0,8) === "function")
        fctName = fctName.substr(8,fctName.length) ;

        return fctName ;
    }

    return fctName ;
}

//--------------------------------------------------------------------------------------------------------
/** Convert a color coded as a string to a RGB integer. the color parameter can be #RRGGBB or RGB(r,g,b). 
* Browser colors like "yellow" or "red" are not suported (return black (0,0,0))
* @param {string} color Color to convert
* @returns {integer} RGB
*/   
function rgbToBgr(color)
{
    var r ;
    var g ;
    var b ;

    var hexString = rTrim(lTrim(color)).toUpperCase() ;
    if (hexString.charAt(0) === '#')
    {
        // decompose #RRGGBB
        //           0123456
        r = parseInt(hexString.substring (1,3),16) ;   //  1,2 (char 3 is not included)
        g = parseInt(hexString.substring (3,5),16) ;   //  3,4 (char 5 is not included)
        b = parseInt(hexString.substring (5,7),16) ;   //  5,6 (char 7 is not included)
    } else if (hexString.indexOf('RGB') === 0) {
        // decompose RGB(r,g,b) or RGB (r,g,b)
        //           0123          01234

        // start after the RGB word and skip spaces
        var p = 3 ;
        while (hexString.charAt(p) === ' ')
        p++;

        // check the '(' and the ')'
        if (hexString.charAt(p) !== '(' &&
            hexString.charAt(hexString.length-1) !== ')')
        return 0 ;  // error : use black

        // remove '(' and ')'
        var block = hexString.substring(p+1,hexString.length-1) ; // r,g,b
        var rgb = block.split(',');
        if (rgb.length !== 3)
        return 0 ;  // error : use black (0,0,0)
        r = parseInt(rgb[0]) ;
        g = parseInt(rgb[1]) ;
        b = parseInt(rgb[2]) ;
    } else {
        return 0 ;  // unknow color , use black (0,0,0)
    }
    return (b << 16) + (g  << 8) + r ;
}

//--------------------------------------------------------------------------------------------------------
/** Generate a new unique indentifier 
* @returns {string} An unique identifier
*/
function newGuid ()
{
    requestId++ ;
    return clientId + '_' + requestId ;
} ;

//--------------------------------------------------------------------------------------------------------

/** convert int to hexa string
* @param {integer} param An integer to convert
* @param {integer} len result width
* @returns {string} String representation
*/
function intToHex(param,len)
{
    var str = (param).toString(16) ;
    while (str.length < len) 
        str = '0' + str ;
    return str ;
}

//--------------------------------------------------------------------------------------------------------
/** Convert an integer to chars 
* @param {integer} param An integer to convert
* @param {integer} len result width
* @param padding caracter padding
* @returns {string} String representation
*/
function intToStr (param,len,padding)
{
    var str = '' + param ;
    padding = padding || ' ' ;
    while (str.length < len) 
        str = padding + str ;
    return str ;
} ;

//--------------------------------------------------------------------------------------------------------
/** convert an integer to 5 chars
* @param {integer} param An integer to convert
* @returns {string} a String of 5 chars
*/
function intToStr3 (param)
{
    var str = '' + param ;
    while (str.length < 3) 
        str = ' ' + str ;
    return str ;
} ;

//--------------------------------------------------------------------------------------------------------
/** convert an integer to 5 chars
* @param {integer} param An integer to convert
* @returns {string} a String of 5 chars
*/
function intToStr5 (param)
{
    var str = '' + param ;
    while (str.length < 5) 
        str = ' ' + str ;
    return str ;
} ;

//--------------------------------------------------------------------------------------------------------
/** convert an integer to 11 chars
* @param {integer} param An integer to convert
* @returns {string} a String of 11 chars
*/
function intToStr11 (param)
{
    var str = '' + param ;
    while (str.length < 11) 
        str = ' ' + str ;
    return str ;
} ;

//--------------------------------------------------------------------------------------------------------
/** Get the class name of an object
* @param {Object} obj Object to get class name
* @returns {string} class name
*/
function getClassName(obj)
{
    if (obj === null)
        return "null" ;

    var type = typeof (obj) ;

    // if not vague, return typeof(obj)
    if (type !== "object")
        return type ;

    // if a property "classname" or "className" of type string exist, return it ;
    var objClassname = obj.classname || obj.className;
    if (typeof(objClassname) == "string") 
        return objClassname ;

    try {
        // HTML DOM nodeName Property : http://www.w3schools.com/jsref/prop_node_nodename.asp
        if (obj.nodeName)
        {
            switch(obj.nodeType)
            {
                case  0 : return 'NODE_INVALID' ;
                case  1 : return 'NODE_ELEMENT' ;
                case  2 : return 'NODE_ATTRIBUTE' ;
                case  3 : return 'NODE_TEXT' ;
                case  4 : return 'NODE_CDATA_SECTION' ;
                case  5 : return 'NODE_ENTITY_REFERENCE' ;
                case  6 : return 'NODE_ENTITY' ;
                case  7 : return 'NODE_PROCESSING_INSTRUCTION' ;
                case  8 : return 'NODE_COMMENT' ;
                case  9 : return 'NODE_DOCUMENT' ;
                case 10 : return 'NODE_DOCUMENT_TYPE' ;
                case 11 : return 'NODE_DOCUMENT_FRAGMENT' ;
                case 12 : return 'NODE_NOTATION' ;
            }
        }

        if (typeof obj.length == 'number') 
        {
            if (obj.item)   return 'collection';
            if (obj.callee) return 'arguments';
        }
    } catch (e) {
        return e.message;
    }

    var protoString = Object.prototype.toString.apply(obj) ;
    if (protoString.substr(0,7) === "[object" ||
        protoString.substr(0,7) === "[Object")
    {
        protoString = protoString.substring(7,protoString.length-1);  // remove "[object" and "]"
        protoString = protoString.replace(/^\s*|\s*$/g, "");  // // Strip leading and trailing white-space

        if (protoString === "")
        protoString = "Object" ;

        // if not vague, return protoString
        if (protoString.toLowerCase() !== "object")
        return protoString ;
    }

    try   // getting "constructor" property can generate exception under firefox
    {
        if ("constructor" in obj)
        {
            // if constructor don't have prototype : unknow type
            if (typeof(obj.constructor) == "undefined")
                return protoString ;

            switch(obj.constructor){
                case Array:  return 'Array';
                case RegExp: return 'Regexp';
                //case Class:  return 'Class';
            }
            // constructor sould be a function. if constructor is an object, return protoString
            if (obj.constructor === Object)
                return protoString ;

            // if constructor don't have prototype : unknow type
            if (typeof(obj.constructor.prototype) == "undefined")
                return protoString ;

            var propType = obj.constructor.toString() ;
            if (propType.length === 0)
                return protoString ;

            var pos = propType.indexOf("{") ;   // search for function body
            if (pos ===-1)
                return propType ;
            if (pos ===0)
                return "<unnamed constructor>" + protoString ;

            propType = propType.substr(0,pos) ;

            pos = propType.indexOf("(") ;   // remove function parameters
            if (pos !== -1)
                propType = propType.substr(0,pos) ;

            if (propType.substr(0,8) === "function")
                propType = propType.substr(8,propType.length) ;

            return propType ;
        }
    } catch (e) {
        return protoString ;
    }
    return protoString ;
}

//--------------------------------------------------------------------------------------------------------
/** Prepare the commandList. Common to all SendXXX function 
* @param {TraceToSend} parentNode Parent node
* @param {string} leftMsg Left message to send
* @param {string} newId Id of the message

* @returns {Array} Array of messages
*/
function prepareNewNode(parentNode, leftMsg, newId)
{
    var parentContext = parentNode.context.getLast() ;  // {Context}
    var parentContextId = parentNode.id ;
    if (parentContext !== '')
        parentContextId = parentContext ;
    var commandList = new Array();
    commandList.push( intToStr5(/*CST_NEW_NODE*/ 550)+ parentContextId);               // param : parent Node id (string)
    commandList.push( intToStr5(/*CST_TRACE_ID*/ 101)+ newId);                         // param : guid(string)
    commandList.push( intToStr5(/*CST_LEFT_MSG*/ 551)+ leftMsg);                       // param : left string
    if (parentNode.iconIndex !== -1)
        commandList.push( intToStr5(/*CST_ICO_INDEX*/ 103) + intToStr11(parentNode.iconIndex));
    return commandList;
}

//--------------------------------------------------------------------------------------------------------
/**
* ttrace give 3 'TraceNode' doors : warning , error and debug.
* Theses 3 doors are displayed with a special icon (all of them have the 'enabled' property set to true).
* @class ttrace is the entry point for all traces.
*/

ttrace =
{
   /** 
   * ask an unique client id to the viewer.
   * Use this function only in Browser mode and if you don't load the javascript Api from the viewer.
   * For NodeJs, an unique id is generated
   * Alternatively, you can give yourself an unique id : ttrace.clientId = "NodeJsServer1" ;  
   * NOTE : result is Asynchrone !!! You must ensure the viewer has returned an Id before sending traces.
   * @function
   * @returns {void}
   */
   queryClientId : function()
   {
       addMessage ({msgId:"", msg:"", partNum:"",command:"UniqueClientId" }) ;
       // the viewer will return a script that is executed by the browser.
       // sample script : ttrace.setClientID(newid) ;
       // On NodeJs, the newid will be extracted.
   } ,

   //--------------------------------------------------------------------------------------------------------
   /** Show or hide the viewer
   * @function
   * @param {boolean} [isVisible] Show if true or undefined, hide if false
   * @returns {void}
   */
   show : function (isVisible)
   {
       var commandList = new Array();

       if (typeof(isVisible) == "undefined")
          isVisible = true ;
       if (isVisible)
          commandList.push( intToStr5(/*CST_SHOW*/ 102)+ '1');
       else
          commandList.push( intToStr5(/*CST_SHOW*/ 102)+ '0');

       sendToClient(commandList);
   } ,

   //--------------------------------------------------------------------------------------------------
   /** Close the viewer
   * @function
   * @returns {void}
   */
   closeViewer : function ()
   {
       var commandList = new Array();
       commandList.push( intToStr5(/*CST_CLOSE_VIEWER*/ 106));
       sendToClient(commandList);
   } ,

   //--------------------------------------------------------------------------------------------------
   /** Clear all traces in the main trace window
   * @function
   * @returns {void}
   */
   clearAll : function()
   {
       ttrace.winTrace.clearAll() ;
   } ,
   
   //--------------------------------------------------------------------------------------------------
   /** Set the global search criteria. You must call TTrace.Wintrace.FindNext to position to the next or previous matching node
   * @function
   * @param {string}  text Text to search</param>
   * @param {boolean} sensitive Search is case sensitive</param>
   * @param {boolean} wholeWord Match only whole word</param>
   * @param {boolean} highlight Highlight results</param>
   * @param {boolean} searchInAllPages Call to FindNext will search also in other traces windows if true</param>
   * @returns {void}
   */
   find : function (text, sensitive, wholeWord , highlight, searchInAllPages) 
   {
       var commandList = new Array();
       var flags = 0 ;
       // Sensitive<<3+WholeWord<<2+highlight<<1+SearchInAllPages

       sensitive         = sensitive || false ;
       wholeWord         = wholeWord || false ;
       highlight         = highlight || false ;
       searchInAllPages  = searchInAllPages || false ;

       if (sensitive)
           flags += 8;
       if (wholeWord)
           flags += 4;
       if (highlight)
           flags += 2;
       if (searchInAllPages)
           flags += 1;

       commandList.push( intToStr5(/*CST_FIND_TEXT*/ 100)  + intToStr11(flags) + text );
       sendToClient(commandList);
   } ,

   /** Callback function called by the server 
   * @function
   * @param {string} msgId Message id
   * @param {string} partNum part of the message
   * @returns {void}
   */
   _done : function () // msgId, partNum
   {
       nbDone++;
       //afterRun(msgId, partNum); // call protected function
       afterRun(); // call protected function
   }
}; // ttrace

Object.defineProperties(ttrace, 
{
    "classname" : {
        value: "ttrace",
        enumerable : true,
        configurable : false
    },
    
    /** 
    * Full Url to TraceTool viewer (localhost:81 for example) 
    */
    "host" : {
        get: function () { return host; },
        set : function (value) {
            host = value;
            //ttrace.queryClientId();        
        },
        enumerable : true,
        configurable : false
    },

    /** 
    *  How tracetool is loaded
    */
    "environment" : {
        get: function () { 
            var result = '' ;
            result += 'isBrowser:'         ; if (isBrowser        ) result += 'true'; else result += 'false' ;                             
            result += ',isNodeJs:'         ; if (isNodeJs         ) result += 'true'; else result += 'false' ;                             
            result += ',isRequireJs:'      ; if (isRequireJs      ) result += 'true'; else result += 'false' ;                            
            result += ',isCommonJS:'       ; if (isCommonJS       ) result += 'true'; else result += 'false' ;                           
            result += ',isSystemJS:'       ; if (isSystemJS       ) result += 'true'; else result += 'false' ; 
            result += ',isChromeExtension:'; if (isChromeExtension) result += 'true'; else result += 'false' ;                           
            return result; 
        },
        enumerable : true,
        configurable : false
    },
    
    /** 
    *  messages already send to the viewer
    */
    "waitingMessageCount" : {
        get: function () { return nbDone; },
        enumerable : true,
        configurable : false
    },
    
    /** 
    *  messages still to send to the viewer
    */
    "sendMessageCount": {
        get: function () { return toSend.length; },
        enumerable: true,
        configurable: false
    },
    /**
    * Communication handle. Unique client id send by the viewer.
    */
    "clientId" : {
        get: function () { return clientId; },
        set : function (value) { 
            // User should not force the ClientId
            clientId = value; 
        },
        enumerable : true,
        configurable : false
    },

    /**
     * The main "WinTrace" instance where traces are send.
     * Warning, error, debug and clearAll functions use this WinTrace. 
     */
    "winTrace" : {
        get: function () {
            if (winTraceSingeton === null)
                winTraceSingeton = new traceClasses.WinTrace();
            return winTraceSingeton;
        },
        enumerable : true,
        configurable : false
    },

    /**
     * The main "WinWatch" instance where watches are send.
     */
    "watches" : {
        get: function () {
            if (watchesSingeton === null)
                watchesSingeton = new traceClasses.WinWatch();
            return watchesSingeton;
        },
        enumerable : true,
        configurable : false
    },

    /** 
    * debug output
    */
    "debug" : {
        get: function () {return this.winTrace.debug;},
        enumerable : true,
        configurable : false
    } ,
    
    /** 
    * warning output
    */
    "warning" : {
        get: function () {return this.winTrace.warning;},
        enumerable : true,
        configurable : false
    } ,

    /** 
    * error output
    */
    "error" : {
        get: function () {return this.winTrace.error;},
        enumerable : true,
        configurable : false
    } ,

    /** 
    * internal classes
    */
    "classes" : {
        get: function () {
            return traceClasses;
        },
        enumerable : true,
        configurable : false
    } ,

    /** 
    * Options
    */

    "options" : {
        value: {
            /** {boolean} Indicate if the reflection should display functions and constructor. Default is false */
            sendFunctions : true ,
            
            /** {boolean} Indicate if the date must be send with the time. Default is false */
            sendDate : false ,
            
            /** {integer} Max Object tree depth for sendValue and Watches */
            objectTreeDepth : 3
        },
        enumerable : true,
        configurable : false
    } 

});


//=============================================================================================================

/**
* @class Define a specific font for a cell or for a whole trace line .
* @constructor
*/
traceClasses.FontDetail = function ()
{
   /** {integer} column id. -1 for the whole line */
   this.colId = 0 ;

   /** {boolean} bold */
   this.bold = false ;

   /** {boolean} italic*/
   this.italic = false;

   /** {string} font color */
   this.color = "" ;

   /** {integer} font size */
   this.size = 0 ;

   /** {string} font name */
   this.fontName = "" ;

   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,      
     value: 'TraceClasses.FontDetail'
   });
}


// add prototype to FontDetail
Object.defineProperty(traceClasses.FontDetail.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,        
  value: 'TraceClasses.FontDetail.prototype'
});



//=============================================================================================================

/**
* @class Used internaly to handle indent() and unindent() functions .
* @description You should not create instance of this class
* @constructor
* @returns {void}
*/
traceClasses.Context = function()
{
   /** {Array} context queue */
   this.contextList = [] ;

   /** {Context} context for Wintrace */
   this.winTraceContext = null ;

   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.Context'
   });

}

// add prototype to Context

traceClasses.Context.prototype =

   /** @lends TraceClasses.Context.prototype */
   {
      //------------------------------------------------------------------------------
      /**
      * Get the last context.
      * @function
      * @returns {Context} last context
      */
      getLast : function ()
      {
         var cList ;
         if (this.winTraceContext != null)
            cList = this.winTraceContext;
         else
            cList = this.contextList;

         if (cList.length === 0)
            return "" ;
         return cList[0] ; // get first
      } ,

      //------------------------------------------------------------------------------
      /**
      * Save the context
      * @name push
      * @function
      * @param {Context} newContext A context to push
      * @returns {void}
      */
      push : function (newContext)
      {
         var cList ;
         if (this.winTraceContext != null)
            cList = this.winTraceContext;
         else
            cList = this.contextList;

         cList.unshift(newContext);
      } ,

      //------------------------------------------------------------------------------
      /**
      * Current indent level.
      * @name level
      * @function
      * @See Indent()
      * @returns {integer} current indent level
      */
      level : function ()
      {
         var cList ;
         if (this.winTraceContext != null)
            cList = this.winTraceContext;
         else
            cList = this.contextList;

         return cList.length;
      } ,

      //------------------------------------------------------------------------------
      /**
      * Delete the last context for the thread
      * @name deleteLast
      * @function
      * @returns {Context} deleted context
      */
      deleteLast : function ()
      {
         var cList ;
         if (this.winTraceContext != null)
            cList = this.winTraceContext;
         else
            cList = this.contextList;

         if (cList.length === 0)
            return "" ;
         return cList.shift() ; // get first and remove it
      }
   }
;

Object.defineProperty(traceClasses.Context.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.Context.prototype'
});


//=============================================================================================================

/**
* @class Common base class for TraceNode and WinTrace. Don't create yourself an instance of this class
* @description TraceToSend methodes create new traces and send it to the viewer
* @constructor
*/
traceClasses.TraceToSend = function ()
{
   /** {string} Unique node id */
   this.id           = '' ;

   /** {integer} Icon index */
   this.iconIndex    = /*CST_ICO_DEFAULT*/ -1 ;

   /** {boolean} Enable methods on this node*/
   this.enabled      = true ;

   /** {string} Wintrace id */
   this.winTraceId   = '' ;

   /** {Context} Tell what is the current node for sub traces. Default is self */
   this.context      = new traceClasses.Context() ;
   this.context.list = {} ;

   Object.defineProperty(this, 'classname', {
     enumerable: true,      
     configurable: false,   
     writable: true, // descendant can override it      
     value: 'TraceClasses.TraceToSend'
   });


} ; // TraceToSend abstract class

//--------------------------------------------------------------------------------------------------------

// TraceToSend prototype
traceClasses.TraceToSend.prototype =
   /** @lends TraceClasses.TraceToSend.prototype */
   {
      //--------------------------------------------------------------------------------------------------------
      /**
       * The most useful trace function : send just one or two strings
       * @function
       * @param {string} leftMsg The message to display
       * @param {string} [rightMsg] The right message
       * @returns {TraceNode} A Trace node. Useful to add sub traces
       */
      send : function (leftMsg,rightMsg)
      {
         if (this.enabled === false)
            return new traceClasses.TraceNode(this);

         // create a node with same properties as "this" with new ID
         var result = new traceClasses.TraceNode(this, true);
         var commandList = prepareNewNode(this, leftMsg, result.id);
         if (typeof (rightMsg) != "undefined" && ("" + rightMsg) !== "")
            commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ rightMsg);                   // param : right string

         sendToWinTraceClient(commandList, this.winTraceId);
         return result;
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Send a message. further trace to the same node are indented under this one.
      * @function
      * @param {string} leftMsg Left message to send
      * @param {string} [rightMsg] Right Message to send (optional)
      * @param {string} [backGroundColor] BackGround Color
      * @param {boolean} [isEnter] if true , a special "enter" icon is added on the node
      * @returns {void}
      */

      indent : function (leftMsg, rightMsg, backGroundColor, isEnter)
      {
         if (!this.enabled)
            return;

         var newId = newGuid ();
         var commandList = prepareNewNode(this, leftMsg, newId);

         if (typeof (rightMsg) != "undefined" && ("" + rightMsg) !== "" && rightMsg != null)
            commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ rightMsg);                   // param : right string

         if (backGroundColor != null)
         {
            var colorValue = rgbToBgr(backGroundColor);
            commandList.push( intToStr5( /*CST_BACKGROUND_COLOR*/ 568) + intToStr11(colorValue) + "-1"); // param : color, colId
            }

         if (isEnter)   // undefined is false
         {
            var member = new traceClasses.MemberNode(); // create root member
            member.add("").viewerKind = /*CST_VIEWER_ENTER*/ 8; // then add an empty member with special viewer
            member.addToStringList(commandList); // convert all groups and nested items/group to strings
         }

         sendToWinTraceClient(commandList, this.winTraceId);

         this.context.push(newId);
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Delete indentation to the node added by indent()
      * @function
      * @param {string} [leftMsg] Left message to send to close indentation (optional)
      * @param {string} [rightMsg] Right message to send to close indentation (optional)
      * @param {string} [backGroundColor] background color (optional)
      * @param {boolean} [isExit] if true, viewer type 'exit' is used (optional)
      * @returns {void}
      */
      unIndent : function (leftMsg, rightMsg, backGroundColor, isExit)
      {
         if (!this.enabled)
            return;

         this.context.deleteLast();

         if (typeof (leftMsg) != "undefined"  || typeof (rightMsg) != "undefined")
         {
            var newId = newGuid ();
            var commandList = prepareNewNode(this, leftMsg, newId);

            if (typeof (rightMsg) != "undefined" && ("" + rightMsg) !== "" && rightMsg != null)
               commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ rightMsg);                   // param : right string

            if (backGroundColor != null) {
               var colorValue = rgbToBgr(backGroundColor);
               commandList.push( intToStr5( /*CST_BACKGROUND_COLOR*/ 568) + intToStr11(colorValue) + "-1"); // param : color, colId
            }

            if (isExit)  // undefined is false
            {
               var member = new traceClasses.MemberNode(); // create root member
               member.add("").viewerKind = /*CST_VIEWER_EXIT*/ 9; // then add an empty member with special viewer
               member.addToStringList(commandList); // convert all groups and nested items/group to strings
            }
            sendToWinTraceClient(commandList, this.winTraceId);
         }
      } ,
         
      //------------------------------------------------------------------------------

      /**
      * send trace with a specific background color
      * @function
      * @param {string} leftMsg Trace message
      * @param {string} color background color
      * @param {integer} colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
      * @return a Trace node
      */

      sendBackgroundColor : function (leftMsg, color, colId)
      {
         if (this.enabled === false)
            return new traceClasses.TraceNode(this);

         // create a node with same properties as "this" with new ID
         var result = new traceClasses.TraceNode(this, true);
         var commandList = prepareNewNode(this, leftMsg, result.id);
         var colorValue = rgbToBgr(color);
         commandList.push( intToStr5( /*CST_BACKGROUND_COLOR*/ 568) + intToStr11(colorValue) + intToStr5(colId)); // param : color, colId

         sendToWinTraceClient(commandList, this.winTraceId);
         return result;
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
       * Indent with "Enter " + left message + right message (optional) + background color (optional)
       * @function
       * @param {string} leftMsg Left message to send
       * @param {string} [rightMsg] Right message to send
       * @param {string} [backGroundColor] BackGround Color
       * @returns {void}
       */
      enterMethod : function (leftMsg, rightMsg, backGroundColor)
      {
         if (!this.enabled)
            return;
         this.indent("Enter " + leftMsg, rightMsg, backGroundColor, true);
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
      * @function
      * @param {string} [leftMsg] Left message to send
      * @param {string} [rightMsg] Right message to send
      * @param {string} [backGroundColor] BackGround Color
      * @returns {void}
      */
      exitMethod : function (leftMsg, rightMsg, backGroundColor)
      {
         if (!this.enabled)
            return ;
         this.unIndent("Exit " + leftMsg, rightMsg, backGroundColor, true);
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Send Private and public values of an object.
      * sendValue is quite different from sendObject : less verbal (no class info) but can show many level.
      * @function
      * @param {string} leftMsg The message text
      * @param {object} objToSend the object to examine
      * @param {integer} [maxLevel] The number of sub element to display. Default is 3
      * @param {string} [title] object title
      * @returns {TraceNode} A trace node
      */
      sendValue : function (leftMsg, objToSend, maxLevel, title)
      {
         if (!this.enabled)
            return new traceClasses.TraceNode(this);

         var result = new traceClasses.TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg || "Object" , result.id);
         result.addValue(objToSend, maxLevel, title);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Send a trace and an object (class info, fields, method)
      * @function
      * @param {string} leftMsg The left trace message to send
      * @param {Object} objToSend The object to inspect
      * @param {boolean} [displayFunctions] Let you specify what to send
      * @returns {TraceNode} A trace node
      */
      sendObject : function (leftMsg, objToSend, displayFunctions)
      {
         if (!this.enabled)
            return new traceClasses.TraceNode(this);

         var result = new traceClasses.TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg || "Object" , result.id);
         result.addObject(objToSend, displayFunctions);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Send the call stack
      * @function
      * @param {string} leftMsg Trace message
      * @param {integer} [level] Call to skip (default = 0 : don't skip)
      * @returns {TraceNode} a Trace node
      */
      sendStack: function (leftMsg, level)
      {
         if (!this.enabled)
            return new traceClasses.TraceNode(this);

         var result = new traceClasses.TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg || "Stack" , result.id);
         result.addStackTrace(level);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);

      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Send the caller function name.It's like the call stack, but display only 1 line
      * @function
      * @param {string} leftMsg Trace message
      * @param {integer} [level] Call to skip (default = 0 : don't skip)
      * @returns {TraceNode} a Trace node
      */
      sendCaller: function (leftMsg, level)
      {
         if (!this.enabled)
            return new traceClasses.TraceNode(this);

         var result = new traceClasses.TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg || "Caller" , result.id);
         result.addCaller(level);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * send dump
      * @function
      * @param {string} leftMsg Trace message
      * @param {string} shortTitle A short title displayed on top of the dump
      * @param {string} buffer The buffer to dump
      * @param {integer} [count] Number of byte to dump
      * @returns {TraceNode} a Trace node
      */
      sendDump : function (leftMsg, shortTitle, buffer, count)
      {
         if (!this.enabled)
             return new traceClasses.TraceNode(this);
         var result = new traceClasses.TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg || "Stack" , result.id);

         result.addDump(shortTitle, buffer, count);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Send xml text
      * @function
      * @param {string} leftMsg Trace message
      * @param {string} xml xml text to send
      * @returns {TraceNode} a Trace node
      */
      sendXml : function (leftMsg, xml)
      {
         if (!this.enabled)
             return new traceClasses.TraceNode(this);

         var result = new traceClasses.TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg || "Stack" , result.id);

         result.addXML(xml);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Add table to node
      * @function
      * @param {string} leftMsg  Trace message
      * @param {TraceTable/Array} table TraceTable or Array to send
      * @returns {TraceNode} a Trace node
      */
      sendTable : function (leftMsg,table)
      {
         if (!this.enabled)
             return new traceClasses.TraceNode(this);
         var result = new traceClasses.TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         var commandList = prepareNewNode(this, leftMsg, result.id);

         result.addTable(table);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId);
         return new traceClasses.TraceNode(result);
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * return current indent level. See Indent()
      * @function
      * @returns {integer} current indent level
      */
      indentLevel : function ()
      {
         return this.context.level() ;
      }
   }
;

Object.defineProperty(traceClasses.TraceToSend.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.TraceToSend.prototype'
});
//=============================================================================================================

/**
* @class TraceNode represent a node on the viewer.
* @extends TraceClasses.TraceToSend
* @description parentNode can be another TraceNode or TraceToSend instance
* @constructor
* @param {string} parentNode Parent node id
* @param {boolean} generateUniqueId If true, Generate the node id
*/
traceClasses.TraceNode = function (parentNode, generateUniqueId)
{
   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.TraceNode'
   });

   // fix default parameters
   parentNode = parentNode || null ;
   if (typeof (generateUniqueId) == "undefined")
       generateUniqueId = true;

   /** {string} Unique node id */
   this.id = '' ;
   if (generateUniqueId)
      this.id = newGuid();

   /** {integer} Icon index */
   this.iconIndex = /*CST_ICO_DEFAULT*/ -1 ;

   /** {boolean} Enable methods on this node*/
   this.enabled = true ;

   /** {string} Wintrace id */
   this.winTraceId = '' ;

   if (parentNode !== null)
   {
      this.iconIndex  = parentNode.iconIndex;
      this.enabled    = parentNode.enabled;
      this.winTraceId = parentNode.winTraceId;
   }
} ; // TraceNode class

//--------------------------------------------------------------------------------------------------------

// TraceNode prototype. Inherit from TraceToSend class
var traceNodePrototype = new traceClasses.TraceToSend() ;  // create a new prototype based on TraceToSend
traceClasses.TraceNode.prototype = traceNodePrototype;

extend(traceNodePrototype,
   /** @lends TraceClasses.TraceNode.prototype */
   {
      //--------------------------------------------------------------------------------------------------------
      /**
      * Override a previous send message (both column)
      * @function
      * @param {string} [newLeftMsg] The new Left message
      * @param {string} [newRightMsg] The new Right message
      * @returns {TraceNode} The trace node
      */
      resend : function( newLeftMsg,  newRightMsg)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5( /* TraceConst.CST_USE_NODE */ 555)+ this.id); // param : id (this)

         if (typeof (newLeftMsg) != "undefined" )
            commandList.push( intToStr5(/*CST_LEFT_MSG*/ 551) + newLeftMsg); // param : new left string
         if (typeof (newRightMsg) != "undefined" )
            commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ newRightMsg); // param : new right string

         // don't resend members and icon
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Override a previous send message
      * @function
      * @param {string} newLeftMsg The new Left message
      * @returns {TraceNode} The trace node
      */
      resendLeft : function(newLeftMsg)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_LEFT_MSG*/ 551) + newLeftMsg); // param : new left string
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Override a previous send message
      * @function
      * @param {string} newRightMsg The new Right message
      * @returns {TraceNode} The trace node
      */
      resendRight : function(newRightMsg)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ newRightMsg); // param : new right string
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Change the Icon index
      * @function
      * @param {number} index Index of the icon to use
      * @returns {TraceNode} The trace node
      */
      resendIconIndex : function(index)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555) + this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_ICO_INDEX*/ 103) + index);   // param : left string
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Change Background Color (whole line) of a node
      * @function
      * @param {string} color new background color of the node
      * @param {integer} colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
      * @returns {TraceNode} The trace node
      */
      setBackgroundColor : function(color, colId)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         var colorValue = rgbToBgr(color);
         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_BACKGROUND_COLOR*/ 568) + intToStr11(colorValue) + intToStr3(colId)); // param : color, colId
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Append text to a previous send message (both column)
      * @function
      * @param {string} [newLeftMsg] The new Left message to append
      * @param {string} [newRightMsg] The new Right message to append
      * @returns {TraceNode} The trace node
      */
      append : function(leftMsgtoAdd, rightMsgtoAdd)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         if (typeof (leftMsgtoAdd) != "undefined" )
            commandList.push( intToStr5(/*CST_APPEND_LEFT_MSG */ 556) + leftMsgtoAdd); // param : new left string
         if (typeof (rightMsgtoAdd) != "undefined" )
            commandList.push( intToStr5(/*CST_APPEND_RIGHT_MSG */ 557) + rightMsgtoAdd); // param : new right string
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Append text to a previous send message
      * @function
      * @param {string} newLeftMsg The new Left message to append
      * @returns {TraceNode} The trace node
      */
      appendLeft : function(leftMsgtoAdd)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "") //$NON-NLS-1$
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555) + this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_APPEND_LEFT_MSG */ 556)+ leftMsgtoAdd); // param : new left string
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Append text to a previous send message
      * @function
      * @param {string} newRightMsg The new Right message to append
      * @returns {TraceNode} The trace node
      */
      appendRight : function(rightMsgtoAdd)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "") //$NON-NLS-1$
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_APPEND_RIGHT_MSG */ 557) + rightMsgtoAdd); // param : new right string
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Force a node to be displayed. don't confuse with setSelected()
      * @function
      * @returns {TraceNode} The trace node
      */
      show : function()
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_FOCUS_NODE */ 558)+ this.id); // param : id (this)
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Set a node as selected in the viewer. Don't confuse with show() that force a node to be displayed
      * @function
      * @returns {TraceNode} The trace node
      */
      setSelected : function()
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_SELECT_NODE*/ 553)+ this.id); // param : id (this)
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Delete the node
      * @function
      * @returns {TraceNode} The trace node
      */
      deleteIt : function()
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_CLEAR_NODE*/ 300) + this.id); // param : id (this)
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //--------------------------------------------------------------------------------------------------------
      /**
      * Delete children node
      * @function
      * @returns {TraceNode} The trace node
      */
      deleteChildren : function()
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_CLEAR_SUBNODES*/ 301)+ this.id); // param : id (this)
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //------------------------------------------------------------------------------

      /**
      * Set or reset the bookmark for the node
      * @function
      * @param {boolean} bookmarked true/false
      * @returns {TraceNode} The trace node
      */
      setBookmark : function(bookmarked) 
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();

         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         if (bookmarked)
            commandList.push( intToStr5(/*CST_SET_BOOKMARK */ 122)+ intToStr11(1)); 
         else  
            commandList.push( intToStr5(/*CST_SET_BOOKMARK */ 122)+ intToStr11(0)); 
      
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //------------------------------------------------------------------------------

      /**
      * set a node visible or invisible
      * @function
      * @param {boolean} visible true/false
      * @returns {TraceNode} The trace node
      */
      setVisible : function(visible)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();

         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)
         if (visible)
            commandList.push( intToStr5(/*CST_VISIBLE_NODE */ 123)+ intToStr11(1)); 
         else  
            commandList.push( intToStr5(/*CST_VISIBLE_NODE */ 123)+ intToStr11(0)); 
    
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //------------------------------------------------------------------------------

      /**
      * Set focus to next sibling
      * @function
      * @returns {TraceNode} The trace node
      */
      gotoNextSibling : function()
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_GOTO_NEXTSIBLING */ 114) + this.id) ;
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,
         
      //------------------------------------------------------------------------------

      /**
      * Set focus to previous sibling
      * @function
      * @returns {TraceNode} The trace node
      */
      gotoPrevSibling : function()
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_GOTO_PREVSIBLING */ 115) + this.id) ;

         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //------------------------------------------------------------------------------

      /**
       * Set focus to first child
       * @function
       * @returns {TraceNode} The trace node
       */
      gotoFirstChild : function()              
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_GOTO_FIRST_CHILD */ 116) + this.id) ;

         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //------------------------------------------------------------------------------

      /**
      * Set focus to last child
      * @function
      * @returns {TraceNode} The trace node
      */
      gotoLastChild : function()  
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_GOTO_LAST_CHILD */ 117) + this.id) ;
         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } ,

      //--------------------------------------------------------------------------------------------------------
      /**
      * Change font detail for an item in the trace. You can give font details in the 6 parameters or a TraceClasses.FontDetail object
      * @function
      * @param {integer} [colId] Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
      * @param {bolean} [bold] Change font to bold
      * @param {boolean} [italic] Change font to Italic
      * @param {string} [color] Change Color
      * @param {integer} [size] Change font size, use zero to keep normal size
      * @param {string} [fontName] Change font name
      * @returns {TraceNode} The trace node
      */
      setFontDetail : function(colId, bold, italic, color, size, fontName)
      {
         if (!this.enabled)
            return this;

         // "Node Id is null, root node cannot be modified (for now)"
         if (this.id === "")
            return this;

         var commandList = new Array();
         commandList.push( intToStr5(/*CST_USE_NODE */ 555)+ this.id); // param : id (this)

         var tempStr = "" ;

         if (colId instanceof traceClasses.FontDetail)
         {
            var fontDetail = colId ;
            bold     = fontDetail.bold ;
            italic   = fontDetail.italic ;
            color    = fontDetail.color ;
            size     = fontDetail.size ;
            fontName = fontDetail.fontName ;
            colId    = fontDetail.colId ; // set as last to don't lose object

         } else {
            if (typeof(colId)    == "undefined") colId = -1 ;
            if (typeof(bold)     == "undefined") bold  = true ;
            if (typeof (italic)  == "undefined") italic = false;
            if (typeof(color)    == "undefined") color = null ;
            if (typeof(size)     == "undefined") size = 0 ;
            if (typeof(fontName) == "undefined") fontName = '' ;
         }
         tempStr += intToStr5(/*CST_FONT_DETAIL*/ 567) + intToStr3(colId);

         if (bold)
            tempStr += "1";
         else
            tempStr += "0";

         if (italic)
            tempStr += "1";
         else
            tempStr += "0";

         // Color is coded as RGB. convert to BGR
         var colorValue;
         if (color === null)
            colorValue = -1 ;
         else
            colorValue = rgbToBgr(color);

         tempStr += intToStr11(colorValue) + intToStr11(size) + fontName ;
         commandList.push(tempStr);


         sendToWinTraceClient(commandList, this.winTraceId);
         return this;
      } // setFontDetail
   }
) ;

Object.defineProperty(traceClasses.TraceNode.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.TraceNode.prototype'
});

//=============================================================================================================

/**
*
* @class WinTrace represent a windows tree where you put traces.
* @extends TraceClasses.TraceToSend
* @description The Window Trace is create on the viewer (if not already done)
* if no parameters are gived, the WinTrace object represent an already created a windows tree. In this case nothing is send to the viewer
* @constructor
* @param {string} [WinTraceID] Window trace Id. If empty, a guid will be generated
* @param {string} [WinTraceText] The Window Title on the viewer.If empty, a default name will be used
*/

traceClasses.WinTrace = function (winTraceId, winTraceText) // inherit from TraceToSend
{
   // private vars, accessible only by privileged method
   //-------------

   var that            = null ;        // used by createNodes 
   var debugInstance   = null ;        // TraceClasses.TraceToSend object
   var warningInstance = null ;        // TraceClasses.TraceToSend object
   var errorInstance   = null ;        // TraceClasses.TraceToSend object
   var contextInstance = new Array() ;

   // public vars
   //-------------

   /** {string} Unique node id */
   this.id = '' ;

   /** {integer} Icon index */
   this.iconIndex = /*CST_ICO_DEFAULT*/ -1 ;    // WinTrace don't have icon (for now)

   /** {boolean} Enable methods on this node*/
   this.enabled = true ;

   /** {string} Parent node id (sould always be an emty string) */
   this.parentNodeId = '' ;

   /** {string} Wintrace id */
   this.winTraceId = '' ;

   /** {string} members (always empty for Wintrace object)*/
   this.members = null ;

   that = this ;
   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.WinTrace'
   });

   if(arguments.length === 0)
   {
      // no arguments : Represent an existing wintrace on the viewer.
      // The user must assign itself the id
      // Nothing is send to the viewer.
      createNodes() ;
   } else {
      // create a new WinTrace window on the viewer
      if (typeof (winTraceId) == "undefined" || winTraceId === "")
         that.id = newGuid() ;
      else
         that.id = winTraceId ;

      createNodes() ;

      if (typeof (winTraceId) != "undefined" && winTraceId != null && winTraceId === "_")
         return ;  // don't create new window on the viewer

      if (typeof (winTraceText) == "undefined" || winTraceText === null || winTraceText === "")
         winTraceText = that.id ;

      // create the trace window
      var commandList = new Array();
      commandList.unshift (intToStr5(/*CST_TREE_NAME*/ 98)+winTraceText);
      sendToWinTraceClient (commandList, that.id);
   }  // arg len > 0

   Object.defineProperties(this,
   {
      /** 
      * Debug, Warning, Error are the 3 doors to send traces
      */
      "debug": {
         get: function() { 
             return debugInstance; 
         },
         enumerable: true,
         configurable: false
      },

      /** 
      * Debug, Warning, Error are the 3 doors to send traces
      */
      "warning": {
         get: function() { 
             return warningInstance; 
         },
         enumerable: true,
         configurable: false
      },

      /** 
      * Debug, Warning, Error are the 3 doors to send traces
      */
      "error": {
         get: function() { 
             return errorInstance; 
         },
         enumerable: true,
         configurable: false
      }
   });   

   //--------------------------------------------------------
   // create privates nodes (debug,warning, error)
   // visibility : internal
   function createNodes()
   {
      that.winTraceId = that.id ;    // winTraceId need to be the same as 'id' if we want to call sendXxx() directly on WinTrace object

      debugInstance = new traceClasses.TraceToSend() ; // (null,false,contextInstance) ;    // no parentNode, don't generate id, winTraceContext
      debugInstance.iconIndex = /*CST_ICO_INFO*/ 24 ;
      debugInstance.winTraceId = that.id ;
      debugInstance.enabled = true ;

      warningInstance = new traceClasses.TraceToSend() ; // (null,false,contextInstance) ;  // no parentNode, don't generate id, winTraceContext
      warningInstance.iconIndex = /*CST_ICO_WARNING*/ 22 ;
      warningInstance.winTraceId = that.id ;
      warningInstance.enabled = true ;

      errorInstance = new traceClasses.TraceToSend() ; // (null,false,contextInstance) ;    // no parentNode, don't generate id, winTraceContext
      errorInstance.iconIndex = /*CST_ICO_ERROR*/ 23 ;
      errorInstance.winTraceId = that.id ;
      errorInstance.enabled = true ;
   } ;
} ;   // WinTrace class

// WinTrace prototype. Inherit from TraceToSend class
var winTracePrototype = new traceClasses.TraceToSend() ;           // create a new prototype based on TraceToSend
traceClasses.WinTrace.prototype = winTracePrototype;

extend(winTracePrototype,
   /** @lends TraceClasses.WinTrace.prototype */
   {
      //--------------------------------------------------------
      /**
      * Save window content to text file
      * (Path is relative to the viewer)
      * @function
      * @param {string} fileName target filename
      * @returns {void}
      */
      saveToTextFile : function(fileName)
      {
         var commandList = new Array();
         commandList.unshift ( intToStr5(/*CST_SAVETOTEXT*/ 559) + fileName) ;
         sendToWinTraceClient (commandList,this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
      * Save window content to xml file <p> (Path is relative to the viewer)
      * @function
      * @param {string} fileName target filename
      * @param {string} [styleSheet] optional stylesheet file name
      * @returns {void}
      */
      saveToXml : function(fileName,styleSheet)
      {
         var commandList = new Array();
         if (typeof(styleSheet) == "undefined")
            commandList.unshift (intToStr5(/*CST_SAVETOXML*/ 560) + fileName) ;
         else
            commandList.unshift (intToStr5(/*CST_SAVETOXML*/ 560) + fileName + '|' + styleSheet);
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
       * Load an XML file to the window tree traces
       * @function
       * @param {string} fileName file to open
       * @returns {void}
       */
      loadXml : function(fileName)
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_LOADXML*/ 561) + fileName) ;
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
      * Set the Viewer log file. Local log file is not supported in javascript.
      * @function
      * @param {string} fileName target filename.(Path is relative to the viewer)
      * @param {integer} [mode] <p>When 0, Log is disabled. <p>When 1, Enabled.<p>
      * When 2, a new file is create each day (CCYYMMDD is appended to the filename)
      * @param {integer} [MaxLines] Number of lines before starting a new file (default : -1 = unlimited)
      * @returns {void}
      */
      setLogFile : function(fileName, mode, maxLines)
      {
         var commandList = new Array();

         mode = mode || 1 ;
         maxLines = maxLines || -1 ;
         commandList.unshift (intToStr5(/*CST_LOGFILE*/ 562) + intToStr11(mode) + intToStr11(maxLines) + fileName) ;
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
      * Switch viewer to this window
      * @function
      * @returns {void}
      */
      displayWin : function()
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_DISPLAY_TREE*/ 97)) ;
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
      * change the tree to display user defined multiple columns
      * @function
      * @param {integer} [mainColIndex] The Main column index (default is 0)
      * @returns {void}
      */
      setMultiColumn : function (mainColIndex)
      {
         mainColIndex = mainColIndex || 0 ;
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_TREE_MULTI_COLUMN*/ 95) +intToStr11(mainColIndex));
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
      * set columns title
      * @function
      * @param {string} titles tab separated columns titles
      * @returns {void}
      */
      setColumnsTitle : function(titles)
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_TREE_COLUMNTITLE*/ 96) + titles);
         sendToWinTraceClient(commandList, this.id);
      } ,
         
      //------------------------------------------------------------------------------
      /**
      * set columns widthsDeleteChildren
      * @function
      * @param {string} widths Tab separated columns width. <p>
      * The format for each column is width[:Min[:Max]] <p>
      * where Min and Max are optional minimum and maximum column width for resizing purpose.<p>
      * Example : 100:20:80 tab 200:50 tab 100
      * @returns {void}
      */
      setColumnsWidth : function(widths)
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_TREE_COLUMNWIDTH*/ 93)+ widths);
         sendToWinTraceClient(commandList, this.id);
      } ,


      //------------------------------------------------------------------------------

      /**
      * Set the focus to the first trace node
      * @function
      * @returns {void}
      */
      gotoFirstNode : function() 
      {

         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_GOTO_FIRST_NODE*/ 80));
         sendToWinTraceClient(commandList, this.id);
      } ,
    
      //------------------------------------------------------------------------------

      /**
      * Set the focus to the last trace node
      * @function
      * @returns {void}
      */
      gotoLastNode : function() 
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_GOTO_LAST_NODE*/ 81));
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------

      /**
      * Set the focus to the next matching node
      * @function
      * @param {boolean} searForward If true search down, else search up
      * @returns {void}
      */
      findNext : function( searForward) 
      {
         var commandList = new Array();
         if (searForward)
            commandList.unshift (intToStr5(/*CST_FIND_NEXT*/ 82) + intToStr11(1));
         else
            commandList.unshift (intToStr5(/*CST_FIND_NEXT*/ 82) + intToStr11(0));
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------

      /**
      * Set the focus to a bookmarked node identified by his position. Bookmarks are cheched by the user or with the node.SetBookmark() function
      * @function
      * @param {integer} pos Indice of the bookmark
      * @returns {void}
      */
      gotoBookmark : function( pos)
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_GOTO_BOOKMARK*/ 83) + intToStr11(pos));
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------

      /**
      * Clear all bookmarks
      * @function
      * @returns {void}
      */
      clearBookmark : function()
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_CLEAR_BOOKMARK*/ 84));
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------

      /**
      * Clear all filters
      * @function
      * @returns {void}
      */
      clearFilter : function() 
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_CLEAR_FILTER*/ 85));
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------

      /**
      * Add a filter to node. Multiple calls to this function can be done. Call ApplyFilter() to apply filtering
      * @function
      * @param {integer} column Column to apply filter.<p>
      * In multicolumn mode the first column start at 0 <p>
      * In normal mode : <p>
      * col icone   = 999    <p>
      * col time    = 1      <p>
      * col thread  = 2      <p>
      * col traces  = 3      <p>
      * col Comment = 4      <p>
      * col members = 998
      * @param {integer} compare There is 5 kinds of filters : <p>
      * Equal           = 0  <p>
      * Not equal       = 1  <p>
      * contains        = 2  <p>
      * Don't contains  = 3  <p>
      * (Ignore this filter) = 4 or -1
      * @param {string} text The text to search (insensitive)        
      * @returns {void}
      */

      addFilter : function(column , compare , text) 
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_ADD_FILTER*/ 86) + intToStr11(column) + intToStr11(compare) + text);
         sendToWinTraceClient(commandList, this.id);
      },
         
      //------------------------------------------------------------------------------

      /**
      * Apply filters after calls to AddFilter()
      * @function
      * @param {boolean} conditionAnd If true, use an 'AND' condition for each filters, else use a "OR"
      * @param {boolean} showMatch If true, show node that match filter and hide others. If false hide matching node and show others
      * @param {boolean} includeChildren If true, search in subnodes
      * @returns {void}
      */

      applyFilter : function( conditionAnd,  showMatch,  includeChildren) 
      {
         var flags = 0;
         // ConditionAnd<<2+ShowMatch<<1+IncludeChildren
         if (conditionAnd)
            flags += 4;
         if (showMatch)
            flags += 2;
         if (includeChildren)
            flags += 1;

         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_APPLY_FILTER*/ 87) + intToStr11(flags));
         sendToWinTraceClient(commandList, this.id);
      } ,
         
      //------------------------------------------------------------------------------
      /**
      * clear all trace in that window
      * @function
      * @returns {void}
      */
      clearAll : function()
      {
         var commandList = new Array();
         commandList.unshift (intToStr5(/*CST_CLEAR_ALL*/ 104)) ;
         sendToWinTraceClient(commandList, this.id);
      } ,

      //------------------------------------------------------------------------------
      /**
      * clear all trace in that window
      * @function
      * @returns {void}
      */
      close : function()
      {
         var commandList = new Array();
         commandList.unshift(intToStr5(/*CST_CLOSE_WIN*/ 105));
         sendToWinTraceClient(commandList, this.id);
      }
   }
) ;

Object.defineProperty(traceClasses.WinTrace.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.WinTrace.prototype'
});

//=============================================================================================================

/**
* @class Alternate way to send traces : prepare a TraceNode with all properties then send it
* @constructor
* @param {string} [parentNode] The parent node where to place that trace. The IconIndex and the enabled properties are also recopied Parameters can be null : the root tree become the parent node, enabled is true and the default icon is used
* @param {string} [generateUniqueId] if true, the id is generated automatically, else set the empty string
*/
traceClasses.TraceNodeEx = function (parentNode, generateUniqueId)
{
   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.TraceNodeEx'
   });

   // public vars
   //-------------

   /** {Array} fonts details applied to the member. Don't use it directly. Use the addFontDetail function in place */
   this.fontDetails = new Array();

   /** {string} Left message */
   this.leftMsg = '';

   /** {string} right message (comment) */
   this.rightMsg = '';

   /** {string} time */
   this.time = '';

   /** {string} thread name */
   this.threadName = '';

   /** {MemberNode} the root for the Member tree */
   this.members = new traceClasses.MemberNode();

   /** {string} Parent Node Id */
   this.parentNodeId = "";

   /** {integer} The index of the icon to use. You can then show an icon for Warning traces different for Error traces */
   this.iconIndex = /*CST_ICO_DEFAULT*/ -1 ;

   /** {boolean} When enabled is false, all traces are disabled. Default is true.
    All node have a enabled property, that lets you define group of enabled trace.
    For example set the TTrace.Debug.enabled to false but continue to accept Error and Warning traces */
   this.enabled = true;

   /** {string} The parent win tree Id*/
   this.winTraceId = null;

   /** {string} The unique ID. Normally it's a GUID, but can be replaced by something else for inter process traces. */
   this.id = '';

   if (typeof(generateUniqueId) == "undefined")
      generateUniqueId = true ;
   if (generateUniqueId)
      this.id = newGuid();

   if (parentNode !== null)
   {
      // ReSharper disable once QualifiedExpressionMaybeNull
      this.iconIndex    = parentNode.iconIndex;
      this.enabled      = parentNode.enabled;
      this.winTraceId   = parentNode.winTraceId;

      var parentContext = parentNode.context.getLast() ;
      if (parentContext !== '')
         this.parentNodeId = parentContext ;
   }
} ;

//--------------------------------------------------------

// TraceNodeEx prototype
traceClasses.TraceNodeEx.prototype =
   {
      //--------------------------------------------------------
      /**
      * Add xml text
      * @function
      * @param {string} xml xml text to send
      * @returns {void}
      */
      addXML : function (xml)
      {
         if (!this.enabled)
            return;

         if (typeof(xml) == "undefined") xml = "" ;
         var member = this.members.add(xml);
         member.viewerKind = /* TraceConst.CST_VIEWER_XML */ 2;
      } ,
         
      //--------------------------------------------------------
      /**
      * Add Dump
      * @function
      * @param {string} shortTitle A short title displayed on top of the dump
      * @param {string} buffer The buffer to dump
      * @param {integer} [count] Number of byte to dump
      * @returns {void}
      */
      addDump : function (shortTitle, buffer, count)
      {
         if (!this.enabled)
            return;

         count = count || buffer.length ;
         var dumpGroup = new traceClasses.MemberNode(shortTitle).setFontDetail(0, true);
         dumpGroup.viewerKind = /* TraceConst.CST_VIEWER_DUMP */ 1;
         this.members.add(dumpGroup);

         var byteDumped = 0;
         var c = 0;


         while (byteDumped < count && (c < buffer.length))
         {
            var d = 0; // inner loop. From 0 to 15 max
            var beginLine = c; // used to print the offset
            var hexaRepresentation = '';
            //var strRepresentation = '' ;

            // 8 integers per line
            while ((byteDumped < count) && (d < 16) && (c < buffer.length))
            {
               //var oneChar = buffer.charAt(c) ;
               var oneInt = buffer.charCodeAt(c) ;
               var str = intToHex(oneInt,2) ;  // minimum 2 but can be more
               hexaRepresentation += str + ' ';

               // only the zero cannot be copied to the stream
               //if (oneInt == 0)
               //   strRepresentation += '.' ;
               //else
               //   strRepresentation += oneChar ;

               byteDumped++;
               d++;
               c++;
            }
            var adr = intToHex (beginLine,6) ;

            dumpGroup.add(adr, hexaRepresentation) ; // , strRepresentation);
         }
         dumpGroup.col2 = '' + byteDumped + " char(s) dumped";

      } ,
         
      //--------------------------------------------------------
      /**
      * Add table to node
      * @function
      * @param {TraceTable/Array} table TraceTable or Array to send
      * @returns {void}
      */
      addTable : function (table)
      {            
         if (!this.enabled) 
            return;
         
         var objClassName = getClassName(table);
         var obj; // real object 
         if (objClassName === "string") {
            obj = eval(table);
            objClassName = getClassName(table);
         } else // already an object or array
             obj = table;
         
         if (table instanceof traceClasses.TraceTable) {     
            table.copyToNodeMembers(this.members); // copy member to node. Member viewer kind is already set
         } else if (objClassName === "Array") {
            // create table
            var tableMembers = new traceClasses.MemberNode('Index').setFontDetail(0, true); // Display the index on first column
            tableMembers.viewerKind = /* TraceConst.CST_VIEWER_TABLE */ 3;
            this.members.add(tableMembers);
            var isFirst = true; // indicate first line (for column detection)
            
            // for loop return the keys of the array
            // ReSharper disable once MissingHasOwnPropertyInForeach
            for (var key in obj) {
               var itemObject = obj[key];
               var fCurrentRow = tableMembers.add('[' + key + ']');
               //var itemClassName = getClassName(itemObject);
               
               // if itemObject primitive
               if (typeof(itemObject) != "object") {
               
                  if (isFirst === true) 
                     tableMembers.col1 += "\t" + "Value";
                  fCurrentRow.col1 += "\t" + itemObject; // .ToString()
               } else { // itemObject is an object
                  var propType;
                  var memberValue;
                  // ReSharper disable once MissingHasOwnPropertyInForeach
                  for (var memberName in itemObject) {
                     try {
                        if (memberName === "ELEMENT_NODE" ||
                        memberName === "ATTRIBUTE_NODE" ||
                        memberName === "TEXT_NODE" ||
                        memberName === "CDATA_SECTION_NODE" ||
                        memberName === "ENTITY_REFERENCE_NODE" ||
                        memberName === "ENTITY_NODE" ||
                        memberName === "PROCESSING_INSTRUCTION_NODE" ||
                        memberName === "COMMENT_NODE" ||
                        memberName === "DOCUMENT_NODE" ||
                        memberName === "DOCUMENT_TYPE_NODE" ||
                        memberName === "DOCUMENT_FRAGMENT_NODE" ||
                        memberName === "NOTATION_NODE" ||
                        memberName === "DOCUMENT_POSITION_DISCONNECTED" ||
                        memberName === "DOCUMENT_POSITION_PRECEDING" ||
                        memberName === "DOCUMENT_POSITION_FOLLOWING" ||
                        memberName === "DOCUMENT_POSITION_CONTAINS" ||
                        memberName === "DOCUMENT_POSITION_CONTAINED_BY" ||
                        memberName === "DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC") 
                           continue;
                        
                        memberValue = itemObject[memberName];
                        propType = getClassName(memberValue);
                        
                        if (propType === "function") 
                           continue;
                        
                        // Add Column Title if first line
                        if (isFirst === true) 
                           tableMembers.col1 += "\t" + memberName;
                        
                        // add data
                        fCurrentRow.col1 = fCurrentRow.col1 + "\t" + memberValue;  //.toString()
                     } 
                     catch (e) {
                        var node;
                        var msg = "" + e;
                        if (msg === e.message) 
                           node = new traceClasses.MemberNode(memberName, e);
                        else 
                           node = new traceClasses.MemberNode(memberName, e.message, e);
                        tableMembers.add(node);
                     }
                  } // for (var memberName in itemObject)
               } // item is an object
               isFirst = false;
            } // next item in array
         } else { // table is not an array : add it as value
             this.addValue(obj, 1, "");
         }
      } ,

      //--------------------------------------------------------
      /**
      * Re-send the trace to the viewer (only left and right message)
      * @function
      * @returns {void}
      */
      resend : function ()
      {
         if (!this.enabled)
            return;

         var commandList = new Array();
         commandList.push( intToStr5( /* TraceConst.CST_USE_NODE */ 555)+ this.id); // param : id (this)
         commandList.push( intToStr5(/*CST_LEFT_MSG*/ 551) + this.leftMsg); // param : new left string
         commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ this.rightMsg); // param : new right string

         // don't re-send members and icon
         sendToWinTraceClient(commandList, this.winTraceId);
      } ,
         
      //--------------------------------------------------------
      /**
      * Change background font color
      * @function
      * @param {string} color xml background color
      * @param {integer} colId Column id or -1 for whole line
      * @returns {void}
      */
      addBackgroundColor : function (color, colId)
      {
         if (!this.enabled)
            return;
         var fontDetail = new traceClasses.FontDetail();
         fontDetail.colId = colId;
         fontDetail.color = color;    // store the color and convert it to BGR when the node is send
         fontDetail.fontName = "BackgroundColor";  // special name. Indicate that color is for background, not font itself //$NON-NLS-1$
         if (typeof(this.fontDetails) == "undefined" || this.fontDetails === null)   // private var fontDetails
            this.fontDetails = new Array();
         this.fontDetails.push(fontDetail);
      } ,

      //--------------------------------------------------------
      /**
      * Change font detail for an item in the trace. You can give font details in the 6 parameters or a TraceClasses.FontDetail object
      * @function
      * @param {integer|FontDetail} [colId] Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
      * @param {bolean} [bold] Change font to bold
      * @param {boolean} [italic] Change font to Italic
      * @param {string} [color] Change Color
      * @param {integer} [size] Change font size, use zero to keep normal size
      * @param {string} [fontName] Change font name
      * @returns {TraceNodeEx} The trace node
      */
      addFontDetail : function (colId, bold, italic, color, size, fontName)
      {
         if (!this.enabled)
            return this ;
         var fontDetail ;
         if (colId instanceof traceClasses.FontDetail) {     
            fontDetail = colId ;
         } else {
            fontDetail = new traceClasses.FontDetail();
            if (typeof(colId)    == "undefined") colId = -1 ;
            if (typeof(bold)     == "undefined") bold  = true ;
            if (typeof(italic)   == "undefined") italic = false;
            if (typeof(color)    == "undefined") color = null ;
            if (typeof(size)     == "undefined") size = 0 ;
            if (typeof(fontName) == "undefined") fontName = '' ;

            fontDetail.colId    = colId;
            fontDetail.bold     = bold;
            fontDetail.italic   = italic;
            fontDetail.color    = color;        // store the color and convert it to BGR when the node is send
            fontDetail.size     = size;
            fontDetail.fontName = fontName;
         }

         if (typeof(this.fontDetails) == "undefined" || this.fontDetails === null)   // private var fontDetails
            this.fontDetails = new Array();

         this.fontDetails.push(fontDetail);
         return this;
      } ,
         
      //--------------------------------------------------------
      /**
      * Send Private and public values of an object.<p>
      * addValue is quite different from addObject : less verbal (no class info) but can show many level.
      * @function
      * @param {Object} objToSend the object to examine
      * @param {integer} [maxLevel] The number of sub element to display. Default is 3
      * @param {string} [title] Object title
      * @returns {void}
      */
      addValue : function (objToSend, maxLevel, title)
      {
         /** @ignore */
         function innerAddValue (objToSend, upperNode, maxLevel, alreadyParsedObject)
         {
            try
            {
               if (objToSend === null)
               {
                  upperNode.col2 = "Null";
                  return;
               }

               if (typeof (objToSend) === "undefined")
               {
                  upperNode.col2 = "undefined";
                  return;
               }

               var objClass = getClassName(objToSend);

               // display the modifier and type name in upper node (col 3). Old col3 content is keept
               upperNode.col3 = upperNode.col3 + objClass ;

               // display value in upper node (col2)

               // check primitive and well know type
               if (typeof (objToSend) != "object" )
               {
                  upperNode.col2 = '' + objToSend.toString();
                  return;
               }


               for(var i = 0; i < alreadyParsedObject.length; i++)
                  if (alreadyParsedObject[i] === objToSend)
                  {
                     upperNode.col2 = "..." ;
                     return ;
                  }

               // max level reached
               if (maxLevel <= 1)
                  return;

               // no more display this object content (array or fields)
               // this is the only place where object is added to alreadyParsedObject list
               alreadyParsedObject.push(objToSend);

               var memberValue ;
               var propType ;
               // ReSharper disable once MissingHasOwnPropertyInForeach
               for (var memberName in objToSend)
               {
                  try
                  {
                     if (memberName === "ELEMENT_NODE" ||
                         memberName === "ATTRIBUTE_NODE" ||
                         memberName === "TEXT_NODE" ||
                         memberName === "CDATA_SECTION_NODE" ||
                         memberName === "ENTITY_REFERENCE_NODE" ||
                         memberName === "ENTITY_NODE" ||
                         memberName === "PROCESSING_INSTRUCTION_NODE" ||
                         memberName === "COMMENT_NODE" ||
                         memberName === "DOCUMENT_NODE" ||
                         memberName === "DOCUMENT_TYPE_NODE" ||
                         memberName === "DOCUMENT_FRAGMENT_NODE" ||
                         memberName === "NOTATION_NODE" ||
                         memberName === "DOCUMENT_POSITION_DISCONNECTED" ||
                         memberName === "DOCUMENT_POSITION_PRECEDING" ||
                         memberName === "DOCUMENT_POSITION_FOLLOWING" ||
                         memberName === "DOCUMENT_POSITION_CONTAINS" ||
                         memberName === "DOCUMENT_POSITION_CONTAINED_BY" ||
                         memberName === "DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
                         continue ;


                     memberValue = objToSend[memberName];

                     if (objClass === "Array")
                        memberName = "[" + memberName + "]";

                     propType = getClassName(memberValue) ;

                     if (propType === "function")
                        continue ;


                     var node = new traceClasses.MemberNode(memberName, "", "");
                     upperNode.add(node);
                     innerAddValue(memberValue, node, maxLevel - 1, alreadyParsedObject);

                  } catch (e) {
                     var nodeEx ;
                     var msg = "" + e ;
                     if (msg === e.message)
                        nodeEx = new traceClasses.MemberNode(memberName, e);
                     else
                        nodeEx = new traceClasses.MemberNode(memberName, e.message, e);
                     upperNode.add(nodeEx);
                  }
               }

            } catch (e2) {
               var nodeEx2 = new traceClasses.MemberNode(e2.message);
               upperNode.add(nodeEx2);
            }
         } ; // innerAddValue()

         if (!this.enabled)
            return;

         if (objToSend === null)
         {
            this.members.add("undefined");
            return;
         }

         if (typeof(maxLevel) == "undefined") maxLevel = ttrace.options.objectTreeDepth ;
         if (typeof(title) == "undefined") title = "Object Value" ;

         var alreadyParsedObject = new Array() ;

         // create the top node using only title.
         // Value (col2) and Type (col3) will be added by inner_addValue
         var result = new traceClasses.MemberNode(title);
         result.viewerKind = /* TraceConst.CST_VIEWER_VALUE */ 7 ;

         // add top node to trace
         this.members.add(result);

         // recursive fill members
         innerAddValue(objToSend, result, maxLevel, alreadyParsedObject);
      } ,

      //--------------------------------------------------------
      /**
      * Call addObject to fill the "member" tree with the object value
      * @function
      * @param {Object} objToSend the object to send
      * @param {boolean} [displayFunctions] Display functions if true
      * @returns {void}
      */
      addObject : function (objName, displayFunctions)
      {
         if (!this.enabled)
            return;

         var oProp ;
         var propType ;
         var propCount = 0 ;
         var propertiesGroup = null ;
         var functionsGroup = null ;
         var classGroup = null ;

         classGroup = new traceClasses.MemberNode("Class information").setFontDetail(0, true);
         classGroup.viewerKind = /* CST_VIEWER_OBJECT */ 6 ;
         this.members.add(classGroup);

         propertiesGroup = new traceClasses.MemberNode("Properties").setFontDetail(0, true);
         propertiesGroup.viewerKind = /* CST_VIEWER_OBJECT */ 6;
         this.members.add(propertiesGroup);

         try
         {
            if (typeof(objName) == "undefined")
            {
               this.members.add("undefined");
               return;
            }

            if (typeof(displayFunctions) == "undefined")
               displayFunctions = ttrace.options.sendFunctions ;

            var obj ;
            if (typeof(objName) == "string")
               obj = eval(objName) ;
            else
               obj = objName ;

            classGroup.add('Class name',getClassName(obj)) ;

            // Loop through properties/functions of the object
            // ReSharper disable once MissingHasOwnPropertyInForeach
            for (var sProp in obj)
            {
               propCount++ ;
               // sProp is the name of the property (string)
               try
               {
                  if (sProp === "ELEMENT_NODE" ||
                      sProp === "ATTRIBUTE_NODE" ||
                      sProp === "TEXT_NODE" ||
                      sProp === "CDATA_SECTION_NODE" ||
                      sProp === "ENTITY_REFERENCE_NODE" ||
                      sProp === "ENTITY_NODE" ||
                      sProp === "PROCESSING_INSTRUCTION_NODE" ||
                      sProp === "COMMENT_NODE" ||
                      sProp === "DOCUMENT_NODE" ||
                      sProp === "DOCUMENT_TYPE_NODE" ||
                      sProp === "DOCUMENT_FRAGMENT_NODE" ||
                      sProp === "NOTATION_NODE" ||
                      sProp === "DOCUMENT_POSITION_DISCONNECTED" ||
                      sProp === "DOCUMENT_POSITION_PRECEDING" ||
                      sProp === "DOCUMENT_POSITION_FOLLOWING" ||
                      sProp === "DOCUMENT_POSITION_CONTAINS" ||
                      sProp === "DOCUMENT_POSITION_CONTAINED_BY" ||
                      sProp === "DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
                      continue ;

                  oProp = obj[sProp];
                  propType = typeof(oProp) ;  // get type as usual in case of the getClassName() generate exception

                  propType = getClassName(oProp) ;

                  if (propType === "function")
                  {
                     if (displayFunctions === true)
                     {
                        if (functionsGroup === null)
                        {
                           functionsGroup = new traceClasses.MemberNode("Functions").setFontDetail(0, true);
                           functionsGroup.viewerKind = /* CST_VIEWER_OBJECT */ 6;
                           this.members.add(functionsGroup);
                        }
                        functionsGroup.add(getFunctionName(sProp)) ;
                     }
                  } else {
                     propertiesGroup.add(sProp,oProp , propType) ;
                  }
               } catch (e) {
                  var msgEx = "" + e ;
                  if (msgEx === e.message)
                     propertiesGroup.add(sProp,e);
                  else
                     propertiesGroup.add(sProp,e.message,e);
               }
            }
            // if no functions or properties : add a warning message
            if (propCount === 0)
            {
               propertiesGroup.add("No properties !") ;
            }
         } catch (e2) {
            var msgExg = "" + e2 ;
            if (msgExg === e2.message)
               classGroup.add(e2);
            else
               classGroup.add(e2,e2.message);
         }
      } ,

      //--------------------------------------------------------
      /**
      * add caller to the Members
      * It's like the call stack, but display only 1 line
      * @function
      * @param {function} [level] Call to skip (default = 0 : don't skip)
      * @returns {void}
      */
      addCaller: function (level)
      {
         if (!this.enabled)
            return;

         var stack ;
         var stackList ;
         var stackLength ;
         var callObj ;
         var callName ;

         level = level || 0 ;

         var group = new traceClasses.MemberNode("Call stack").setFontDetail(0, true);
         group.viewerKind =  /* CST_VIEWER_STACK */ 4 ;
         this.members.add(group);

         if (isRequireJs)  // isNodeJs
         {
             stack = stackTrace.get(this.addCaller);
             stackLength = stack.length;
             for (let i = 0; i < stackLength; i++)
             {
                callObj = stack[i];
                if (callObj.getFileName().includes("tracetool.js") === false)
                {
                   if (level > 0)
                      level-- ;
                   else {
                      callName = callObj.toString();
                      group.add(callName);
                      return;
                   }
                }
             }
         } else {
           
            stack = new Error().stack ;
            stackList = stack.split('\n') ;
            stackLength = stackList.length;
            for (let i = 0; i < stackLength; i++)
            {
               callObj = stackList[i].trim();
               if (callObj === "Error")
                   continue ;
               
               if (callObj.includes("tracetool.js") === true)
                   continue ;

               if (callObj.startsWith("at ")) // other fancy lines are ignored
               {
                  if (level > 0)
                     level-- ;
                  else {
                     callName = callObj.substring(3);
                     group.add(callName);
                     return ;
                  }
               }  
            }
         }
      } ,

      //--------------------------------------------------------
      /**
      * add stack to the Members
      * @function
      * @param {integer} [level] Call to skip (default = 0 : don't skip)
      * @returns {void}
      */
      addStackTrace: function (level)
      {
         if (!this.enabled)
            return;
         level = level || 0 ;

         var group = new traceClasses.MemberNode("Call stack").setFontDetail(0, true);
         group.viewerKind =  /* CST_VIEWER_STACK */ 4 ;
         this.members.add(group);
         
         var stack ;
         var stackList ;
         var stackLength ;
         var callObj ;
         var callName ;
         

         if (isNodeJs)
         {
            stack = stackTrace.get(this.addStackTrace);
            stackLength = stack.length;
            for (let i = 0; i < stackLength; i++)
            {
               callObj = stack[i];
               if (callObj.getFileName().includes("tracetool.js") === false)
               {
                  if (level > 0)
                     level-- ;
                  else {
                     callName = callObj.toString();
                     group.add(callName);
                  }
               }
            }
         } else {
            stack = new Error().stack ;
            
            /*
            typeof stack : string 
            Error
                at traceClasses.TraceNodeEx.addStackTrace (file:///C:/Thierry/ChromeExtensions/Page%20checker/components/tracetool/tracetool.js:3232:25)
                at traceClasses.TraceToSend.sendStack (file:///C:/Thierry/ChromeExtensions/Page%20checker/components/tracetool/tracetool.js:1409:17)
                at butSample (file:///C:/Thierry/ChromeExtensions/Page%20checker/components/tracetool/sample.html:52:17)
                at HTMLInputElement.onclick (file:///C:/Thierry/ChromeExtensions/Page%20checker/components/tracetool/sample.html:302:94)
            */
            
            stackList = stack.split('\n') ;
            stackLength = stackList.length;
            for (let i = 0; i < stackLength; i++)
            {
               callObj = stackList[i].trim();
               if (callObj === "Error")
                   continue ;
               
               if (callObj.includes("tracetool.js") === true)
                   continue ;

               if (callObj.startsWith("at ")) // other fancy lines are ignored
               {
                  if (level > 0)
                     level-- ;
                  else {
                     callName = callObj.substring(3);
                     group.add(callName);
                  }
               }  
            }           
         }
      } ,

      //--------------------------------------------------------
      /**
      * Send the trace to the server (left + right + members)
      * @function
      * @returns {TraceNode} a traceNode
      */
      send : function ()
      {
         var result = new traceClasses.TraceNode(this);
         if (!this.enabled)
            return result;
         var commandList = new Array();

         if (this.threadName !== '')
            commandList.push(intToStr5(/*TraceConst.CST_THREAD_NAME*/305) + this.threadName);
         commandList.push( intToStr5(/*CST_NEW_NODE*/ 550)+ this.parentNodeId); // param : parent Node id (this)
         commandList.push( intToStr5(/*CST_TRACE_ID*/ 101)+ this.id);           // param : Node id
         if (typeof (this.leftMsg) != "undefined" && ("" + this.leftMsg) !== "")
            commandList.push( intToStr5(/*CST_LEFT_MSG*/ 551)+ this.leftMsg);   // param : left string
         if (typeof (this.rightMsg) != "undefined" && ("" + this.rightMsg) !== "")
            commandList.push( intToStr5(/*CST_RIGHT_MSG*/ 552)+ this.rightMsg); // param : right string

         commandList.push( intToStr5(/*CST_ICO_INDEX*/ 103) + intToStr11(this.iconIndex)); // param : Icon index

         // add font detail (private var)
         if (this.fontDetails != null)
         {
            for (var c = 0; c < this.fontDetails.length; c++)
            {
               var fontDetail = this.fontDetails[c];
               var colorValue ;
               if (typeof(fontDetail.color) == "undefined" || fontDetail.color === null)
                  colorValue = -1 ;
               else
                  colorValue = rgbToBgr(fontDetail.color);
               var tempStr = "" ;

               if (fontDetail.fontName === "BackgroundColor") //$NON-NLS-1$
               {
                  //special color : background color
                  commandList.push( intToStr5( /*CST_BACKGROUND_COLOR*/ 568) + intToStr11(colorValue) + intToStr(fontDetail.colId));      // param : color, colId
               } else {
                  tempStr += intToStr5(/*CST_FONT_DETAIL*/ 567) + intToStr3(fontDetail.colId);

                  if (fontDetail.bold)
                     tempStr += "1";
                  else
                     tempStr += "0";

                  if (fontDetail.italic)
                     tempStr += "1";
                  else
                     tempStr += "0";

                  tempStr += intToStr11(colorValue) + intToStr11(fontDetail.size) + fontDetail.fontName ;
                  commandList.push(tempStr);
               }
            }
            this.fontDetails = null; // once copied to commandlist, clear the array
         }

         this.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         sendToWinTraceClient(commandList, this.winTraceId , this.time);
         return result;
      }
   }
;

Object.defineProperty(traceClasses.TraceNodeEx.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.TraceNodeEx.prototype'
});

//=============================================================================================================

/**
* @class Construct a table of row to display in the viewer on a node.
* @description The table must be associated with a node. See TraceNodeEx.AddTable() and TraceSend.SendTable()
* @constructor
*/
traceClasses.TraceTable = function ()
{
   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.TraceTable'
   });

   /** {MemberNode} the root for the Member tree */
   this.members = new traceClasses.MemberNode();
   this.members.viewerKind = 3 ; /*CST_VIEWER_TABLE*/;

   /** {string} the current row */
   this.currentRow= null;
}

//--------------------------------------------------------
// TraceTable prototype
traceClasses.TraceTable.prototype =
   {
      //--------------------------------------------------------
      /**
      * Add columns title : one or more columns titles separated by tabs
      * @function
      * @param {string} colTitle one or more columns titles separated by tabs. Can also be called several times to add title
      * @returns {void}
      */
      addColumnTitle : function (colTitle)
      {
         if (this.members.col1 === "")
            this.members.col1 = colTitle;
         else
            this.members.col1 += "\t" + colTitle;
      } ,

      //--------------------------------------------------------
      /**
      * Add an empty row
      * @function
      * @returns {void}
      */
      addRow : function ()
      {
         this.currentRow = this.members.add("");
      } ,

      //--------------------------------------------------------
      /**
      * Add data to current row
      * @function
      * @param {string} cell one or more columns data separated by tabs. Can also be called several times to add cells
      * @returns {void}
      */
      addRowData : function (cell)
      {
         if (this.currentRow === null)
            addRow();

         if (this.currentRow.col1 === "")
            this.currentRow.col1 = cell;
         else
            this.currentRow.col1 += "\t" + cell;
      } ,

      //--------------------------------------------------------
      /**
      * convert to members
      * @function
      * @param {Array} nodeMembers target
      * @returns {void}
      */
      copyToNodeMembers : function (nodeMembers)
      {
         var tableMembers = nodeMembers.add(this.members.col1);
         tableMembers.viewerKind = /* TraceConst.CST_VIEWER_TABLE */ 3;
         for (var c = 0; c < this.members.members.length; c++)
            tableMembers.add(this.members.members[c].col1);
      }
   }
;

Object.defineProperty(traceClasses.TraceTable.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.TraceTable.prototype'
});

//=============================================================================================================

/**
 * @class WinWatch represent a windows tree where you put watches.
 * @description Windows watch. The Window watch is create on the viewer (if not already done). Sample code : TTrace.watches().send("test2", mySet);
 * @constructor
 * @param {string} [winWatchId] Required window trace Id. If empty, a guid will be generated
 * @param {string} [winWatchText] The Window Title on the viewer.If empty, a default name will be used
 */
traceClasses.WinWatch = function (winWatchId , winWatchText)
{
   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.WinWatch'
   });

   /** {boolean} When enabled is false, all traces are disabled. Default is true. */
   this.enabled = true ;

   /**
   * {string} The "Required" Id of the window tree, can be any string, or a guid.
   * The Main window watch Id is empty
   */
   this.id = "" ;  //$NON-NLS-1$

   // case of the main WinWatch
   if (typeof(winWatchId) == "undefined")
      return ;

   if (winWatchId === null || winWatchId === "")
      this.id = newGuid();
   else
      this.id = winWatchId ;

   // create the trace window
   var commandList = new Array();

   if (winWatchText === null || winWatchText === "") //$NON-NLS-1$
      commandList.unshift (intToStr5(/*CST_WINWATCH_NAME*/ 110) + "Watches " + this.id);
   else
      commandList.unshift (intToStr5(/*CST_WINWATCH_NAME*/ 110) + winWatchText);

   sendToWinWatchClient  (commandList, this.id);
}

//--------------------------------------------------------

// WinWatch prototype
traceClasses.WinWatch.prototype =
    {
       /**
       * Switch viewer to this window
       * @function
       * @returns {void}
       */
       displayWin : function ()
       {
          var commandList = new Array();
          commandList.unshift (intToStr5(/*CST_DISPLAY_TREE*/ 97)) ;
          sendToWinWatchClient(commandList, this.id);
       } ,

       //--------------------------------------------------------
       /**
       * clear all traces in that window
       * @function
       * @returns {void}
       */
       clearAll : function ()
       {
          var commandList = new Array();
          commandList.unshift (intToStr5(/*CST_CLEAR_ALL*/ 104)) ;
          sendToWinWatchClient(commandList, this.id);
       } ,

       //--------------------------------------------------------
       /**
       * clear all traces in that window
       * @function
       * @returns {void}
       */
       close : function ()
       {
          var commandList = new Array();
          commandList.unshift(intToStr5(/*CST_CLOSE_WIN*/ 105));
          sendToWinWatchClient(commandList, this.id);
       } ,

       //--------------------------------------------------------
       /**
       * Send a watch
       * @function
       * @param {string} watchName Watch name
       * @param {Object} watchValue Watch value
       * @returns {void}
       */
       send : function (watchName ,watchValue)
       {
          if (!this.enabled)
             return ;

          var commandList = new Array();
          commandList.push(intToStr5(/*CST_WATCH_NAME*/ 112) + watchName);

          // create a node to store members
          var node = new traceClasses.TraceNodeEx(null, false);  // no parent, don't generate node id

          node.addValue(watchValue  ,  ttrace.options.objectTreeDepth , "");    // no title
          node.members.addToStringList(commandList) ;   // convert all groups and nested items/group to strings

          sendToWinWatchClient(commandList, this.id);
       }
    }
;

Object.defineProperty(traceClasses.WinWatch.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.WinWatch.prototype'
});
//=============================================================================================================

/**
* @class Create a TMemberNode with text for the 3 columns
* @constructor
* @param {string} [col1] text of first col
* @param {string} [col2] text of second col
* @param {string} [col3] text of third col
*/
traceClasses.MemberNode = function (col1, col2, col3)
{
   Object.defineProperty(this, 'classname', {
     enumerable: true,     
     configurable: false,  
     writable: false,       
     value: 'TraceClasses.MemberNode'
   });

   /** {integer} Indicate the type of viewer to use do display the member */
   this.viewerKind = 0 ;

   /** {string} first column */
   this.col1 = "" + (col1 || "") ;

   /** {string} second column */
   this.col2 = "" + (col2 || "") ;

   /** {string} third column */
   this.col3 = "" + (col3 || "") ;

   /** {Array} sub members */
   this.members = new Array() ;

   /** {Array} fonts details applied to the member. Don't use it directly. Use the setFontDetail function in place */
   this.fontDetails = new Array() ;
} ;   // MemberNode class

//--------------------------------------------------------------------------------------------------------

// MemberNode prototype
traceClasses.MemberNode.prototype =
{
   //--------------------------------------------------------------------------------------------------------
   /**
   * @function
   * @param {string|MemberNode} [col1] text of first col
   * @param {string} [col2] text of second col
   * @param {string} [col3] text of third col
   * @returns {MemberNode} A new member
   */
   add : function (col1, col2, col3)
   {
      var member ;
      if (col1 instanceof traceClasses.MemberNode)    
      {
         member = col1 ; // strCol1 is already a MemberNode object. Add to array and return it
      } else {
         member = new traceClasses.MemberNode(col1, col2, col3) ;  // create a Member object
      }
      this.members.push(member);
      return member;
   } ,

   //--------------------------------------------------------------------------------------------------------
   /**
   * Change font detail for an item in the trace. You can give font details in the 6 parameters or a TraceClasses.FontDetail object
   * @function
   * @param {integer|FontDetail} [colId] Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
   * @param {bolean} [bold] Change font to bold
   * @param {boolean} [italic] Change font to Italic
   * @param {string} [color] Change Color
   * @param {integer} [size] Change font size, use zero to keep normal size
   * @param {string} [fontName] Change font name
   * @returns {MemberNode} the Member node
   */
   setFontDetail : function (colId, bold, italic, color, size, fontName)
   {
      var fontDetail ;

      if (getClassName(colId) === "TraceClasses.FontDetail") 
      {
         fontDetail = colId ;
      } else {
         fontDetail = new traceClasses.FontDetail();

         if (typeof(colId)    == "undefined") colId = -1 ;     // default : whole line
         if (typeof(bold)     == "undefined") bold  = false ;  // default : not bold
         if (typeof(italic)   == "undefined") italic = false;  // default : not italic
         if (typeof(color)    == "undefined") color = null ;   // store color string. Will be converted to "BGR" before sending. default : black
         if (typeof(size)     == "undefined") size = 0 ;       // default : use default viewer font size
         if (typeof(fontName) == "undefined") fontName = '' ;  // default : no font name

         fontDetail.colId    = colId    ;
         fontDetail.bold     = bold     ;
         fontDetail.italic   = italic   ;
         fontDetail.color    = color    ;
         fontDetail.size     = size     ;
         fontDetail.fontName = fontName ;
      }
      if (typeof(this.fontDetails) == "undefined" || this.fontDetails === null)
         this.fontDetails = new Array();

      this.fontDetails.push(fontDetail);
      return this;
   } ,

   //----------------------------------------------------------------------
   /**
   * recursively add members to the node commandList
   * @function
   * @param commandList Where to store members
   * @returns {void}
   */
   addToStringList : function(commandList)
   {
      /** @ignore */
      function internalAddToStringList(subnode)
      {
         var c;
         // create the member and set col1
         commandList.push( intToStr5( /*CST_CREATE_MEMBER*/ 500) + subnode.col1); // 3/11 ?
         // add command if col2 and/or col3 exist
         if (subnode.col2 !== "")
            commandList.push( intToStr5( /*CST_MEMBER_COL2*/ 502) + subnode.col2); // 3/11 ?

         if (subnode.col3 !== "")
            commandList.push( intToStr5( /*CST_MEMBER_COL3*/ 504) + subnode.col3); // 3/11 ?

         // add viewer kind
         if (subnode.viewerKind !== 0)
            commandList.push( intToStr5( /*CST_MEMBER_VIEWER_KIND*/ 503) + subnode.viewerKind);     // 3/11 ?

         // add font detail
         if (subnode.fontDetails != null)
         {
            for (c = 0; c < subnode.fontDetails.length; c++)
            {
               var fontDetail = subnode.fontDetails[c];

               var tempStr = "" ;

               tempStr += intToStr5(/*CST_MEMBER_FONT_DETAIL*/ 501) ;
               tempStr += intToStr3(fontDetail.colId) ;

               if (fontDetail.bold)
                  tempStr += "1";
               else
                  tempStr += "0";

               if (fontDetail.italic)
                  tempStr += "1";
               else
                  tempStr += "0";

               // Color is coded as RGB. convert to BGR
               var colorValue ;
            if (typeof(fontDetail.color) == "undefined" || fontDetail.color === null)
                  colorValue = -1 ;
               else
                  colorValue = rgbToBgr(fontDetail.color);

               tempStr += intToStr11(colorValue) + intToStr11(fontDetail.size) + fontDetail.fontName ;
               commandList.push(tempStr);
            }
            subnode.fontDetails = null ;   // once copied to commandlist, clear the array
         }

         // recursive add sub nodes, if any
         for (c = 0; c < subnode.members.length; c++)
         {
             var node2 = subnode.members[c];
             internalAddToStringList(node2);
         }

          // close the member group
         commandList.push( intToStr5( /*CST_ADD_MEMBER*/ 505));
      } ; // end of internalAddToStringList


      // the root node node itself is not send for now.
      // Later we can send the 3 columns text to specify the header, if specfied.
      // the text should be like that : "Myfirstcol:150" where 150 is the column with
      // sub nodes, if any
      for (var d = 0; d < this.members.length; d++)
      {
         var node = this.members[d];
         internalAddToStringList(node);
      }

      // once copied to Commandlist, clear the array
      this.members= new Array() ;
   }  // addToStringList
};  // TraceClasses.MemberNode.prototype

Object.defineProperty(traceClasses.MemberNode.prototype, 'classname', {
  enumerable: true,     
  configurable: false,  
  writable: false,       
  value: 'TraceClasses.MemberNode.prototype'
});

//--------------------------------------------------------------------------------------------------------

// http://benmccormick.org/2015/05/28/moving-past-requirejs/

// CommonJS syntax (synchronous nodejs)
if (isCommonJS)
{
   module.exports = ttrace;
} else if (isRequireJs) {

   // AMD modules syntaxe . RequireJS requires developers to use AMD modules (asynchronous)
   define({
      ttrace: ttrace
   });
}

// ES6 syntax :
//export default ttrace ;

//if (isSystemJS) {
//  System.register(["tracetool"], function (exports_1, context_1) {
//    "use strict";
//    console.log("exports_1", exports_1);
//    console.log("context_1", context_1);
//    var __moduleName = context_1 && context_1.id;
//    return {
//      setters: [
//        function (_1) {
//        }
//      ],
//      execute: function () {
//          console.log("System.register execute");
//          exports_1("default", ttrace);
//      }
//    };
//  });  
//}


// simple browser mode : put it global (window)
if (isBrowser)
{
   global.ttrace = ttrace;
}

// ReSharper disable once ThisInGlobalContext
})(typeof window !== "undefined" ? window : this);
