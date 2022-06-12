
﻿"use strict";var define;(function(global)
{var ttrace=null;var ttraceScript=null;var headId=null;var request;var stackTrace;var uuid;var requestId=0;var toSend=[];var nbDone=0;var winTraceSingeton=null;var watchesSingeton=null;var clientId="";var host="127.0.0.1:81";var traceClasses={};var isChromeExtension;var isBrowser;var isNodeJs;var isRequireJs;var isCommonJS;var isSystemJS;detectEnvironment();if(isRequireJs)
{stackTrace=require('stack-trace');uuid=require('uuid');clientId=uuid().replace(/-/g,'');}
if(isNodeJs)
{request=require('request');}else if(isBrowser){ttraceScript=null;headId=global.document.getElementsByTagName("head")[0];}
function detectEnvironment()
{isChromeExtension=false;isBrowser=false;isNodeJs=false;isRequireJs=false;isCommonJS=false;isSystemJS=false;try{if(typeof require==="function")
isRequireJs=true;if((typeof module==="object")&&(typeof module.exports==="object"))
isCommonJS=true;if((typeof chrome==="object")&&(typeof chrome.extension==="object"))
isChromeExtension=true;else if((typeof require==="function")&&(typeof process==="object")&&(typeof process.release==="object")&&(typeof process.release.name==="string")&&(process.release.name.search(/node|io.js/)!==-1))
isNodeJs=true;else
isBrowser=true;}
catch(e){console.log("detectEnvironment exception",e);}}
function extend(target,source)
{for(var property in source)
target[property]=source[property];return target;};function getFormattedTime()
{var currentDate=new Date();var date="";if(ttrace.options.sendDate===true)
date=""+currentDate.getFullYear()+intToStr(currentDate.getMonth(),2,'0')+intToStr(currentDate.getDay(),2,'0')+" ";var h=currentDate.getHours();var m=currentDate.getMinutes();var s=currentDate.getSeconds();var n=currentDate.getMilliseconds();return date+intToStr(h,2,'0')+':'+intToStr(m,2,'0')+':'+intToStr(s,2,'0')+':'+intToStr(n,3,'0');};function sendToWinTraceClient(commandList,winTraceId,dateTime)
{if(typeof(dateTime)=="undefined"||dateTime===null||dateTime==='')
commandList.unshift(intToStr5(304)+getFormattedTime());else
commandList.unshift(intToStr5(304)+dateTime);if(winTraceId!=null&&winTraceId!=="")
commandList.unshift(intToStr5(99)+winTraceId);sendToClient(commandList);};function sendToWinWatchClient(commandList,winWatchId,dateTime)
{if(typeof(dateTime)=="undefined"||dateTime===null||dateTime==='')
commandList.unshift(intToStr5(304)+getFormattedTime());else
commandList.unshift(intToStr5(304)+dateTime);commandList.unshift(intToStr5(111)+winWatchId);sendToClient(commandList);};function sendToClient(commandList)
{var msgId=newGuid();var msg=commandList.join("\0");var msgLenth=msg.length;if(msgLenth>1000)
{var part;var partNum=1;var partLen;while(msgLenth>0)
{part=msg.substring(0,1000);msg=msg.substring(1000);msgLenth-=1000;partLen=part.length;if(partLen>=1000)
addMessage({msgId:msgId,msg:part,partNum:partNum});else
addMessage({msgId:msgId,msg:part,partNum:'Last'});partNum++;}}else{addMessage({msgId:msgId,msg:msg,partNum:''});}};function addMessage(objMessage)
{objMessage.command=objMessage.command||"WMD";toSend.push(objMessage);if(toSend.length===1)
setTimeout(worker,0);};function worker()
{var objMessage;if(toSend.length!==0)
{objMessage=toSend.shift();var hostUrl="http://"+host+"/"+objMessage.command+"?msgId="+objMessage.msgId+"&msg="+encodeURIComponent(objMessage.msg);if(objMessage.partNum!=="")
hostUrl=hostUrl+"&partNum="+objMessage.partNum;nbDone++;if(isNodeJs)
sendToClientUsingRequest(hostUrl);else if(isBrowser)
sendToClientUsingScript(hostUrl);else
sendToClientUsingXmlHttpRequest(hostUrl);}}
function sendToClientUsingScript(hostUrl)
{var script=document.createElement("script");script.type="text/javascript";script.setAttribute("id","ttraceScript");script.setAttribute("name","ttraceScript");script.src=hostUrl;script.timeSend=new Date();ttraceScript=script;headId.appendChild(script);setTimeout(worker,20000);}
function afterRun()
{if(ttraceScript===null)
return;headId.removeChild(ttraceScript);ttraceScript=null;setTimeout(worker,0);};function sendToClientUsingXmlHttpRequest(hostUrl)
{var xhr;try{xhr=new XMLHttpRequest();}catch(e1){try{xhr=new ActiveXObject("Microsoft.XMLHTTP");}catch(e2){xhr=new ActiveXObject("Msxml2.XMLHTTP");}}
xhr.addEventListener("error",function(){setTimeout(worker,0);},false);xhr.onload=function(onloadEvent){var onloadRequest=onloadEvent.currentTarget;var script=onloadRequest.responseText;if(script.startsWith("ttrace.setClientID("))
clientId=script.match(/\d+/)[0];setTimeout(worker,0);}
xhr.open("GET",hostUrl,true);xhr.send();}
function sendToClientUsingRequest(hostUrl)
{request(hostUrl,function(error,response)
{if(!error&&response.statusCode===200)
{var script=response.body;if(script.startsWith("ttrace.setClientID("))
clientId=script.match(/\d+/)[0];setTimeout(worker,0);}});setTimeout(worker,20000);}
function lTrim(str)
{var k=0;while(k<str.length&&str.charAt(k)<=" ")k++;return str.substring(k,str.length);}
function rTrim(str)
{var k=str.length-1;while(k>=0&&str.charAt(k)<=" ")k--;return str.substring(0,k+1);}
function getFunctionName(fctName)
{fctName=rTrim(lTrim(fctName));if(fctName.indexOf('[ecmascript code]')===0)
return'?';if(fctName.indexOf('function ')===0)
{var p=9;var endClassName=1000;while(fctName.charAt(p)===' ')
p++;for(var c=p;c<fctName.length;c++)
{var ch=fctName.charAt(c);if(ch==='('){endClassName=c;break;}}
if(endClassName===p)
return'?';return fctName.substring(9,endClassName);}
var pos=fctName.indexOf("{");if(pos>=0)
{if(pos===0)
return"<unnamed function>";fctName=fctName.substr(0,pos);pos=fctName.indexOf("(");if(pos!==-1)
fctName=fctName.substr(0,pos);if(fctName.substr(0,8)==="function")
fctName=fctName.substr(8,fctName.length);return fctName;}
return fctName;}
function rgbToBgr(color)
{var r;var g;var b;var hexString=rTrim(lTrim(color)).toUpperCase();if(hexString.charAt(0)==='#')
{r=parseInt(hexString.substring(1,3),16);g=parseInt(hexString.substring(3,5),16);b=parseInt(hexString.substring(5,7),16);}else if(hexString.indexOf('RGB')===0){var p=3;while(hexString.charAt(p)===' ')
p++;if(hexString.charAt(p)!=='('&&hexString.charAt(hexString.length-1)!==')')
return 0;var block=hexString.substring(p+1,hexString.length-1);var rgb=block.split(',');if(rgb.length!==3)
return 0;r=parseInt(rgb[0]);g=parseInt(rgb[1]);b=parseInt(rgb[2]);}else{return 0;}
return(b<<16)+(g<<8)+r;}
function newGuid()
{requestId++;return clientId+'_'+requestId;};function intToHex(param,len)
{var str=(param).toString(16);while(str.length<len)
str='0'+str;return str;}
function intToStr(param,len,padding)
{var str=''+param;padding=padding||' ';while(str.length<len)
str=padding+str;return str;};function intToStr3(param)
{var str=''+param;while(str.length<3)
str=' '+str;return str;};function intToStr5(param)
{var str=''+param;while(str.length<5)
str=' '+str;return str;};function intToStr11(param)
{var str=''+param;while(str.length<11)
str=' '+str;return str;};function getClassName(obj)
{if(obj===null)
return"null";var type=typeof(obj);if(type!=="object")
return type;var objClassname=obj.classname||obj.className;if(typeof(objClassname)=="string")
return objClassname;try{if(obj.nodeName)
{switch(obj.nodeType)
{case 0:return'NODE_INVALID';case 1:return'NODE_ELEMENT';case 2:return'NODE_ATTRIBUTE';case 3:return'NODE_TEXT';case 4:return'NODE_CDATA_SECTION';case 5:return'NODE_ENTITY_REFERENCE';case 6:return'NODE_ENTITY';case 7:return'NODE_PROCESSING_INSTRUCTION';case 8:return'NODE_COMMENT';case 9:return'NODE_DOCUMENT';case 10:return'NODE_DOCUMENT_TYPE';case 11:return'NODE_DOCUMENT_FRAGMENT';case 12:return'NODE_NOTATION';}}
if(typeof obj.length=='number')
{if(obj.item)return'collection';if(obj.callee)return'arguments';}}catch(e){return e.message;}
var protoString=Object.prototype.toString.apply(obj);if(protoString.substr(0,7)==="[object"||protoString.substr(0,7)==="[Object")
{protoString=protoString.substring(7,protoString.length-1);protoString=protoString.replace(/^\s*|\s*$/g,"");if(protoString==="")
protoString="Object";if(protoString.toLowerCase()!=="object")
return protoString;}
try
{if("constructor"in obj)
{if(typeof(obj.constructor)=="undefined")
return protoString;switch(obj.constructor){case Array:return'Array';case RegExp:return'Regexp';}
if(obj.constructor===Object)
return protoString;if(typeof(obj.constructor.prototype)=="undefined")
return protoString;var propType=obj.constructor.toString();if(propType.length===0)
return protoString;var pos=propType.indexOf("{");if(pos===-1)
return propType;if(pos===0)
return"<unnamed constructor>"+protoString;propType=propType.substr(0,pos);pos=propType.indexOf("(");if(pos!==-1)
propType=propType.substr(0,pos);if(propType.substr(0,8)==="function")
propType=propType.substr(8,propType.length);return propType;}}catch(e){return protoString;}
return protoString;}
function prepareNewNode(parentNode,leftMsg,newId)
{var parentContext=parentNode.context.getLast();var parentContextId=parentNode.id;if(parentContext!=='')
parentContextId=parentContext;var commandList=new Array();commandList.push(intToStr5(550)+parentContextId);commandList.push(intToStr5(101)+newId);commandList.push(intToStr5(551)+leftMsg);if(parentNode.iconIndex!==-1)
commandList.push(intToStr5(103)+intToStr11(parentNode.iconIndex));return commandList;}
ttrace={queryClientId:function()
{addMessage({msgId:"",msg:"",partNum:"",command:"UniqueClientId"});},show:function(isVisible)
{var commandList=new Array();if(typeof(isVisible)=="undefined")
isVisible=true;if(isVisible)
commandList.push(intToStr5(102)+'1');else
commandList.push(intToStr5(102)+'0');sendToClient(commandList);},closeViewer:function()
{var commandList=new Array();commandList.push(intToStr5(106));sendToClient(commandList);},clearAll:function()
{ttrace.winTrace.clearAll();},find:function(text,sensitive,wholeWord,highlight,searchInAllPages)
{var commandList=new Array();var flags=0;sensitive=sensitive||false;wholeWord=wholeWord||false;highlight=highlight||false;searchInAllPages=searchInAllPages||false;if(sensitive)
flags+=8;if(wholeWord)
flags+=4;if(highlight)
flags+=2;if(searchInAllPages)
flags+=1;commandList.push(intToStr5(100)+intToStr11(flags)+text);sendToClient(commandList);},_done:function()
{nbDone++;afterRun();}};Object.defineProperties(ttrace,{"classname":{value:"ttrace",enumerable:true,configurable:false},"host":{get:function(){return host;},set:function(value){host=value;},enumerable:true,configurable:false},"environment":{get:function(){var result='';result+='isBrowser:';if(isBrowser)result+='true';else result+='false';result+=',isNodeJs:';if(isNodeJs)result+='true';else result+='false';result+=',isRequireJs:';if(isRequireJs)result+='true';else result+='false';result+=',isCommonJS:';if(isCommonJS)result+='true';else result+='false';result+=',isSystemJS:';if(isSystemJS)result+='true';else result+='false';result+=',isChromeExtension:';if(isChromeExtension)result+='true';else result+='false';return result;},enumerable:true,configurable:false},"waitingMessageCount":{get:function(){return nbDone;},enumerable:true,configurable:false},"sendMessageCount":{get:function(){return toSend.length;},enumerable:true,configurable:false},"clientId":{get:function(){return clientId;},set:function(value){clientId=value;},enumerable:true,configurable:false},"winTrace":{get:function(){if(winTraceSingeton===null)
winTraceSingeton=new traceClasses.WinTrace();return winTraceSingeton;},enumerable:true,configurable:false},"watches":{get:function(){if(watchesSingeton===null)
watchesSingeton=new traceClasses.WinWatch();return watchesSingeton;},enumerable:true,configurable:false},"debug":{get:function(){return this.winTrace.debug;},enumerable:true,configurable:false},"warning":{get:function(){return this.winTrace.warning;},enumerable:true,configurable:false},"error":{get:function(){return this.winTrace.error;},enumerable:true,configurable:false},"classes":{get:function(){return traceClasses;},enumerable:true,configurable:false},"options":{value:{sendFunctions:true,sendDate:false,objectTreeDepth:3},enumerable:true,configurable:false}});traceClasses.FontDetail=function()
{this.colId=0;this.bold=false;this.italic=false;this.color="";this.size=0;this.fontName="";Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.FontDetail'});}
Object.defineProperty(traceClasses.FontDetail.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.FontDetail.prototype'});traceClasses.Context=function()
{this.contextList=[];this.winTraceContext=null;Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.Context'});}
traceClasses.Context.prototype={getLast:function()
{var cList;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;if(cList.length===0)
return"";return cList[0];},push:function(newContext)
{var cList;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;cList.unshift(newContext);},level:function()
{var cList;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;return cList.length;},deleteLast:function()
{var cList;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;if(cList.length===0)
return"";return cList.shift();}};Object.defineProperty(traceClasses.Context.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.Context.prototype'});traceClasses.TraceToSend=function()
{this.id='';this.iconIndex=-1;this.enabled=true;this.winTraceId='';this.context=new traceClasses.Context();this.context.list={};Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:true,value:'TraceClasses.TraceToSend'});};traceClasses.TraceToSend.prototype={send:function(leftMsg,rightMsg)
{if(this.enabled===false)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNode(this,true);var commandList=prepareNewNode(this,leftMsg,result.id);if(typeof(rightMsg)!="undefined"&&(""+rightMsg)!=="")
commandList.push(intToStr5(552)+rightMsg);sendToWinTraceClient(commandList,this.winTraceId);return result;},indent:function(leftMsg,rightMsg,backGroundColor,isEnter)
{if(!this.enabled)
return;var newId=newGuid();var commandList=prepareNewNode(this,leftMsg,newId);if(typeof(rightMsg)!="undefined"&&(""+rightMsg)!==""&&rightMsg!=null)
commandList.push(intToStr5(552)+rightMsg);if(backGroundColor!=null)
{var colorValue=rgbToBgr(backGroundColor);commandList.push(intToStr5(568)+intToStr11(colorValue)+"-1");}
if(isEnter)
{var member=new traceClasses.MemberNode();member.add("").viewerKind=8;member.addToStringList(commandList);}
sendToWinTraceClient(commandList,this.winTraceId);this.context.push(newId);},unIndent:function(leftMsg,rightMsg,backGroundColor,isExit)
{if(!this.enabled)
return;this.context.deleteLast();if(typeof(leftMsg)!="undefined"||typeof(rightMsg)!="undefined")
{var newId=newGuid();var commandList=prepareNewNode(this,leftMsg,newId);if(typeof(rightMsg)!="undefined"&&(""+rightMsg)!==""&&rightMsg!=null)
commandList.push(intToStr5(552)+rightMsg);if(backGroundColor!=null){var colorValue=rgbToBgr(backGroundColor);commandList.push(intToStr5(568)+intToStr11(colorValue)+"-1");}
if(isExit)
{var member=new traceClasses.MemberNode();member.add("").viewerKind=9;member.addToStringList(commandList);}
sendToWinTraceClient(commandList,this.winTraceId);}},sendBackgroundColor:function(leftMsg,color,colId)
{if(this.enabled===false)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNode(this,true);var commandList=prepareNewNode(this,leftMsg,result.id);var colorValue=rgbToBgr(color);commandList.push(intToStr5(568)+intToStr11(colorValue)+intToStr5(colId));sendToWinTraceClient(commandList,this.winTraceId);return result;},enterMethod:function(leftMsg,rightMsg,backGroundColor)
{if(!this.enabled)
return;this.indent("Enter "+leftMsg,rightMsg,backGroundColor,true);},exitMethod:function(leftMsg,rightMsg,backGroundColor)
{if(!this.enabled)
return;this.unIndent("Exit "+leftMsg,rightMsg,backGroundColor,true);},sendValue:function(leftMsg,objToSend,maxLevel,title)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Object",result.id);result.addValue(objToSend,maxLevel,title);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},sendObject:function(leftMsg,objToSend,displayFunctions)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Object",result.id);result.addObject(objToSend,displayFunctions);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},sendStack:function(leftMsg,level)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Stack",result.id);result.addStackTrace(level);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},sendCaller:function(leftMsg,level)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Caller",result.id);result.addCaller(level);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},sendDump:function(leftMsg,shortTitle,buffer,count)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Stack",result.id);result.addDump(shortTitle,buffer,count);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},sendXml:function(leftMsg,xml)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Stack",result.id);result.addXML(xml);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},sendTable:function(leftMsg,table)
{if(!this.enabled)
return new traceClasses.TraceNode(this);var result=new traceClasses.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg,result.id);result.addTable(table);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new traceClasses.TraceNode(result);},indentLevel:function()
{return this.context.level();}};Object.defineProperty(traceClasses.TraceToSend.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceToSend.prototype'});traceClasses.TraceNode=function(parentNode,generateUniqueId)
{Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceNode'});parentNode=parentNode||null;if(typeof(generateUniqueId)=="undefined")
generateUniqueId=true;this.id='';if(generateUniqueId)
this.id=newGuid();this.iconIndex=-1;this.enabled=true;this.winTraceId='';if(parentNode!==null)
{this.iconIndex=parentNode.iconIndex;this.enabled=parentNode.enabled;this.winTraceId=parentNode.winTraceId;}};var traceNodePrototype=new traceClasses.TraceToSend();traceClasses.TraceNode.prototype=traceNodePrototype;extend(traceNodePrototype,{resend:function(newLeftMsg,newRightMsg)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(typeof(newLeftMsg)!="undefined")
commandList.push(intToStr5(551)+newLeftMsg);if(typeof(newRightMsg)!="undefined")
commandList.push(intToStr5(552)+newRightMsg);sendToWinTraceClient(commandList,this.winTraceId);return this;},resendLeft:function(newLeftMsg)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(551)+newLeftMsg);sendToWinTraceClient(commandList,this.winTraceId);return this;},resendRight:function(newRightMsg)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(552)+newRightMsg);sendToWinTraceClient(commandList,this.winTraceId);return this;},resendIconIndex:function(index)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(103)+index);sendToWinTraceClient(commandList,this.winTraceId);return this;},setBackgroundColor:function(color,colId)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();var colorValue=rgbToBgr(color);commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(568)+intToStr11(colorValue)+intToStr3(colId));sendToWinTraceClient(commandList,this.winTraceId);return this;},append:function(leftMsgtoAdd,rightMsgtoAdd)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(typeof(leftMsgtoAdd)!="undefined")
commandList.push(intToStr5(556)+leftMsgtoAdd);if(typeof(rightMsgtoAdd)!="undefined")
commandList.push(intToStr5(557)+rightMsgtoAdd);sendToWinTraceClient(commandList,this.winTraceId);return this;},appendLeft:function(leftMsgtoAdd)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(556)+leftMsgtoAdd);sendToWinTraceClient(commandList,this.winTraceId);return this;},appendRight:function(rightMsgtoAdd)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(557)+rightMsgtoAdd);sendToWinTraceClient(commandList,this.winTraceId);return this;},show:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(558)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},setSelected:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(553)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},deleteIt:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(300)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},deleteChildren:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(301)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},setBookmark:function(bookmarked)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(bookmarked)
commandList.push(intToStr5(122)+intToStr11(1));else
commandList.push(intToStr5(122)+intToStr11(0));sendToWinTraceClient(commandList,this.winTraceId);return this;},setVisible:function(visible)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(visible)
commandList.push(intToStr5(123)+intToStr11(1));else
commandList.push(intToStr5(123)+intToStr11(0));sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoNextSibling:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(114)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoPrevSibling:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(115)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoFirstChild:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(116)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoLastChild:function()
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(117)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},setFontDetail:function(colId,bold,italic,color,size,fontName)
{if(!this.enabled)
return this;if(this.id==="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);var tempStr="";if(colId instanceof traceClasses.FontDetail)
{var fontDetail=colId;bold=fontDetail.bold;italic=fontDetail.italic;color=fontDetail.color;size=fontDetail.size;fontName=fontDetail.fontName;colId=fontDetail.colId;}else{if(typeof(colId)=="undefined")colId=-1;if(typeof(bold)=="undefined")bold=true;if(typeof(italic)=="undefined")italic=false;if(typeof(color)=="undefined")color=null;if(typeof(size)=="undefined")size=0;if(typeof(fontName)=="undefined")fontName='';}
tempStr+=intToStr5(567)+intToStr3(colId);if(bold)
tempStr+="1";else
tempStr+="0";if(italic)
tempStr+="1";else
tempStr+="0";var colorValue;if(color===null)
colorValue=-1;else
colorValue=rgbToBgr(color);tempStr+=intToStr11(colorValue)+intToStr11(size)+fontName;commandList.push(tempStr);sendToWinTraceClient(commandList,this.winTraceId);return this;}});Object.defineProperty(traceClasses.TraceNode.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceNode.prototype'});traceClasses.WinTrace=function(winTraceId,winTraceText)
{var that=null;var debugInstance=null;var warningInstance=null;var errorInstance=null;var contextInstance=new Array();this.id='';this.iconIndex=-1;this.enabled=true;this.parentNodeId='';this.winTraceId='';this.members=null;that=this;Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.WinTrace'});if(arguments.length===0)
{createNodes();}else{if(typeof(winTraceId)=="undefined"||winTraceId==="")
that.id=newGuid();else
that.id=winTraceId;createNodes();if(typeof(winTraceId)!="undefined"&&winTraceId!=null&&winTraceId==="_")
return;if(typeof(winTraceText)=="undefined"||winTraceText===null||winTraceText==="")
winTraceText=that.id;var commandList=new Array();commandList.unshift(intToStr5(98)+winTraceText);sendToWinTraceClient(commandList,that.id);}
Object.defineProperties(this,{"debug":{get:function(){return debugInstance;},enumerable:true,configurable:false},"warning":{get:function(){return warningInstance;},enumerable:true,configurable:false},"error":{get:function(){return errorInstance;},enumerable:true,configurable:false}});function createNodes()
{that.winTraceId=that.id;debugInstance=new traceClasses.TraceToSend();debugInstance.iconIndex=24;debugInstance.winTraceId=that.id;debugInstance.enabled=true;warningInstance=new traceClasses.TraceToSend();warningInstance.iconIndex=22;warningInstance.winTraceId=that.id;warningInstance.enabled=true;errorInstance=new traceClasses.TraceToSend();errorInstance.iconIndex=23;errorInstance.winTraceId=that.id;errorInstance.enabled=true;};};var winTracePrototype=new traceClasses.TraceToSend();traceClasses.WinTrace.prototype=winTracePrototype;extend(winTracePrototype,{saveToTextFile:function(fileName)
{var commandList=new Array();commandList.unshift(intToStr5(559)+fileName);sendToWinTraceClient(commandList,this.id);},saveToXml:function(fileName,styleSheet)
{var commandList=new Array();if(typeof(styleSheet)=="undefined")
commandList.unshift(intToStr5(560)+fileName);else
commandList.unshift(intToStr5(560)+fileName+'|'+styleSheet);sendToWinTraceClient(commandList,this.id);},loadXml:function(fileName)
{var commandList=new Array();commandList.unshift(intToStr5(561)+fileName);sendToWinTraceClient(commandList,this.id);},setLogFile:function(fileName,mode,maxLines)
{var commandList=new Array();mode=mode||1;maxLines=maxLines||-1;commandList.unshift(intToStr5(562)+intToStr11(mode)+intToStr11(maxLines)+fileName);sendToWinTraceClient(commandList,this.id);},displayWin:function()
{var commandList=new Array();commandList.unshift(intToStr5(97));sendToWinTraceClient(commandList,this.id);},setMultiColumn:function(mainColIndex)
{mainColIndex=mainColIndex||0;var commandList=new Array();commandList.unshift(intToStr5(95)+intToStr11(mainColIndex));sendToWinTraceClient(commandList,this.id);},setColumnsTitle:function(titles)
{var commandList=new Array();commandList.unshift(intToStr5(96)+titles);sendToWinTraceClient(commandList,this.id);},setColumnsWidth:function(widths)
{var commandList=new Array();commandList.unshift(intToStr5(93)+widths);sendToWinTraceClient(commandList,this.id);},gotoFirstNode:function()
{var commandList=new Array();commandList.unshift(intToStr5(80));sendToWinTraceClient(commandList,this.id);},gotoLastNode:function()
{var commandList=new Array();commandList.unshift(intToStr5(81));sendToWinTraceClient(commandList,this.id);},findNext:function(searForward)
{var commandList=new Array();if(searForward)
commandList.unshift(intToStr5(82)+intToStr11(1));else
commandList.unshift(intToStr5(82)+intToStr11(0));sendToWinTraceClient(commandList,this.id);},gotoBookmark:function(pos)
{var commandList=new Array();commandList.unshift(intToStr5(83)+intToStr11(pos));sendToWinTraceClient(commandList,this.id);},clearBookmark:function()
{var commandList=new Array();commandList.unshift(intToStr5(84));sendToWinTraceClient(commandList,this.id);},clearFilter:function()
{var commandList=new Array();commandList.unshift(intToStr5(85));sendToWinTraceClient(commandList,this.id);},addFilter:function(column,compare,text)
{var commandList=new Array();commandList.unshift(intToStr5(86)+intToStr11(column)+intToStr11(compare)+text);sendToWinTraceClient(commandList,this.id);},applyFilter:function(conditionAnd,showMatch,includeChildren)
{var flags=0;if(conditionAnd)
flags+=4;if(showMatch)
flags+=2;if(includeChildren)
flags+=1;var commandList=new Array();commandList.unshift(intToStr5(87)+intToStr11(flags));sendToWinTraceClient(commandList,this.id);},clearAll:function()
{var commandList=new Array();commandList.unshift(intToStr5(104));sendToWinTraceClient(commandList,this.id);},close:function()
{var commandList=new Array();commandList.unshift(intToStr5(105));sendToWinTraceClient(commandList,this.id);}});Object.defineProperty(traceClasses.WinTrace.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.WinTrace.prototype'});traceClasses.TraceNodeEx=function(parentNode,generateUniqueId)
{Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceNodeEx'});this.fontDetails=new Array();this.leftMsg='';this.rightMsg='';this.time='';this.threadName='';this.members=new traceClasses.MemberNode();this.parentNodeId="";this.iconIndex=-1;this.enabled=true;this.winTraceId=null;this.id='';if(typeof(generateUniqueId)=="undefined")
generateUniqueId=true;if(generateUniqueId)
this.id=newGuid();if(parentNode!==null)
{this.iconIndex=parentNode.iconIndex;this.enabled=parentNode.enabled;this.winTraceId=parentNode.winTraceId;var parentContext=parentNode.context.getLast();if(parentContext!=='')
this.parentNodeId=parentContext;}};traceClasses.TraceNodeEx.prototype={addXML:function(xml)
{if(!this.enabled)
return;if(typeof(xml)=="undefined")xml="";var member=this.members.add(xml);member.viewerKind=2;},addDump:function(shortTitle,buffer,count)
{if(!this.enabled)
return;count=count||buffer.length;var dumpGroup=new traceClasses.MemberNode(shortTitle).setFontDetail(0,true);dumpGroup.viewerKind=1;this.members.add(dumpGroup);var byteDumped=0;var c=0;while(byteDumped<count&&(c<buffer.length))
{var d=0;var beginLine=c;var hexaRepresentation='';while((byteDumped<count)&&(d<16)&&(c<buffer.length))
{var oneInt=buffer.charCodeAt(c);var str=intToHex(oneInt,2);hexaRepresentation+=str+' ';byteDumped++;d++;c++;}
var adr=intToHex(beginLine,6);dumpGroup.add(adr,hexaRepresentation);}
dumpGroup.col2=''+byteDumped+" char(s) dumped";},addTable:function(table)
{if(!this.enabled)
return;var objClassName=getClassName(table);var obj;if(objClassName==="string"){obj=eval(table);objClassName=getClassName(table);}else
obj=table;if(table instanceof traceClasses.TraceTable){table.copyToNodeMembers(this.members);}else if(objClassName==="Array"){var tableMembers=new traceClasses.MemberNode('Index').setFontDetail(0,true);tableMembers.viewerKind=3;this.members.add(tableMembers);var isFirst=true;for(var key in obj){var itemObject=obj[key];var fCurrentRow=tableMembers.add('['+key+']');if(typeof(itemObject)!="object"){if(isFirst===true)
tableMembers.col1+="\t"+"Value";fCurrentRow.col1+="\t"+itemObject;}else{var propType;var memberValue;for(var memberName in itemObject){try{if(memberName==="ELEMENT_NODE"||memberName==="ATTRIBUTE_NODE"||memberName==="TEXT_NODE"||memberName==="CDATA_SECTION_NODE"||memberName==="ENTITY_REFERENCE_NODE"||memberName==="ENTITY_NODE"||memberName==="PROCESSING_INSTRUCTION_NODE"||memberName==="COMMENT_NODE"||memberName==="DOCUMENT_NODE"||memberName==="DOCUMENT_TYPE_NODE"||memberName==="DOCUMENT_FRAGMENT_NODE"||memberName==="NOTATION_NODE"||memberName==="DOCUMENT_POSITION_DISCONNECTED"||memberName==="DOCUMENT_POSITION_PRECEDING"||memberName==="DOCUMENT_POSITION_FOLLOWING"||memberName==="DOCUMENT_POSITION_CONTAINS"||memberName==="DOCUMENT_POSITION_CONTAINED_BY"||memberName==="DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
continue;memberValue=itemObject[memberName];propType=getClassName(memberValue);if(propType==="function")
continue;if(isFirst===true)
tableMembers.col1+="\t"+memberName;fCurrentRow.col1=fCurrentRow.col1+"\t"+memberValue;}
catch(e){var node;var msg=""+e;if(msg===e.message)
node=new traceClasses.MemberNode(memberName,e);else
node=new traceClasses.MemberNode(memberName,e.message,e);tableMembers.add(node);}}}
isFirst=false;}}else{this.addValue(obj,1,"");}},resend:function()
{if(!this.enabled)
return;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(551)+this.leftMsg);commandList.push(intToStr5(552)+this.rightMsg);sendToWinTraceClient(commandList,this.winTraceId);},addBackgroundColor:function(color,colId)
{if(!this.enabled)
return;var fontDetail=new traceClasses.FontDetail();fontDetail.colId=colId;fontDetail.color=color;fontDetail.fontName="BackgroundColor";if(typeof(this.fontDetails)=="undefined"||this.fontDetails===null)
this.fontDetails=new Array();this.fontDetails.push(fontDetail);},addFontDetail:function(colId,bold,italic,color,size,fontName)
{if(!this.enabled)
return this;var fontDetail;if(colId instanceof traceClasses.FontDetail){fontDetail=colId;}else{fontDetail=new traceClasses.FontDetail();if(typeof(colId)=="undefined")colId=-1;if(typeof(bold)=="undefined")bold=true;if(typeof(italic)=="undefined")italic=false;if(typeof(color)=="undefined")color=null;if(typeof(size)=="undefined")size=0;if(typeof(fontName)=="undefined")fontName='';fontDetail.colId=colId;fontDetail.bold=bold;fontDetail.italic=italic;fontDetail.color=color;fontDetail.size=size;fontDetail.fontName=fontName;}
if(typeof(this.fontDetails)=="undefined"||this.fontDetails===null)
this.fontDetails=new Array();this.fontDetails.push(fontDetail);return this;},addValue:function(objToSend,maxLevel,title)
{function innerAddValue(objToSend,upperNode,maxLevel,alreadyParsedObject)
{try
{if(objToSend===null)
{upperNode.col2="Null";return;}
if(typeof(objToSend)==="undefined")
{upperNode.col2="undefined";return;}
var objClass=getClassName(objToSend);upperNode.col3=upperNode.col3+objClass;if(typeof(objToSend)!="object")
{upperNode.col2=''+objToSend.toString();return;}
for(var i=0;i<alreadyParsedObject.length;i++)
if(alreadyParsedObject[i]===objToSend)
{upperNode.col2="...";return;}
if(maxLevel<=1)
return;alreadyParsedObject.push(objToSend);var memberValue;var propType;for(var memberName in objToSend)
{try
{if(memberName==="ELEMENT_NODE"||memberName==="ATTRIBUTE_NODE"||memberName==="TEXT_NODE"||memberName==="CDATA_SECTION_NODE"||memberName==="ENTITY_REFERENCE_NODE"||memberName==="ENTITY_NODE"||memberName==="PROCESSING_INSTRUCTION_NODE"||memberName==="COMMENT_NODE"||memberName==="DOCUMENT_NODE"||memberName==="DOCUMENT_TYPE_NODE"||memberName==="DOCUMENT_FRAGMENT_NODE"||memberName==="NOTATION_NODE"||memberName==="DOCUMENT_POSITION_DISCONNECTED"||memberName==="DOCUMENT_POSITION_PRECEDING"||memberName==="DOCUMENT_POSITION_FOLLOWING"||memberName==="DOCUMENT_POSITION_CONTAINS"||memberName==="DOCUMENT_POSITION_CONTAINED_BY"||memberName==="DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
continue;memberValue=objToSend[memberName];if(objClass==="Array")
memberName="["+memberName+"]";propType=getClassName(memberValue);if(propType==="function")
continue;var node=new traceClasses.MemberNode(memberName,"","");upperNode.add(node);innerAddValue(memberValue,node,maxLevel-1,alreadyParsedObject);}catch(e){var nodeEx;var msg=""+e;if(msg===e.message)
nodeEx=new traceClasses.MemberNode(memberName,e);else
nodeEx=new traceClasses.MemberNode(memberName,e.message,e);upperNode.add(nodeEx);}}}catch(e2){var nodeEx2=new traceClasses.MemberNode(e2.message);upperNode.add(nodeEx2);}};if(!this.enabled)
return;if(objToSend===null)
{this.members.add("undefined");return;}
if(typeof(maxLevel)=="undefined")maxLevel=ttrace.options.objectTreeDepth;if(typeof(title)=="undefined")title="Object Value";var alreadyParsedObject=new Array();var result=new traceClasses.MemberNode(title);result.viewerKind=7;this.members.add(result);innerAddValue(objToSend,result,maxLevel,alreadyParsedObject);},addObject:function(objName,displayFunctions)
{if(!this.enabled)
return;var oProp;var propType;var propCount=0;var propertiesGroup=null;var functionsGroup=null;var classGroup=null;classGroup=new traceClasses.MemberNode("Class information").setFontDetail(0,true);classGroup.viewerKind=6;this.members.add(classGroup);propertiesGroup=new traceClasses.MemberNode("Properties").setFontDetail(0,true);propertiesGroup.viewerKind=6;this.members.add(propertiesGroup);try
{if(typeof(objName)=="undefined")
{this.members.add("undefined");return;}
if(typeof(displayFunctions)=="undefined")
displayFunctions=ttrace.options.sendFunctions;var obj;if(typeof(objName)=="string")
obj=eval(objName);else
obj=objName;classGroup.add('Class name',getClassName(obj));for(var sProp in obj)
{propCount++;try
{if(sProp==="ELEMENT_NODE"||sProp==="ATTRIBUTE_NODE"||sProp==="TEXT_NODE"||sProp==="CDATA_SECTION_NODE"||sProp==="ENTITY_REFERENCE_NODE"||sProp==="ENTITY_NODE"||sProp==="PROCESSING_INSTRUCTION_NODE"||sProp==="COMMENT_NODE"||sProp==="DOCUMENT_NODE"||sProp==="DOCUMENT_TYPE_NODE"||sProp==="DOCUMENT_FRAGMENT_NODE"||sProp==="NOTATION_NODE"||sProp==="DOCUMENT_POSITION_DISCONNECTED"||sProp==="DOCUMENT_POSITION_PRECEDING"||sProp==="DOCUMENT_POSITION_FOLLOWING"||sProp==="DOCUMENT_POSITION_CONTAINS"||sProp==="DOCUMENT_POSITION_CONTAINED_BY"||sProp==="DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
continue;oProp=obj[sProp];propType=typeof(oProp);propType=getClassName(oProp);if(propType==="function")
{if(displayFunctions===true)
{if(functionsGroup===null)
{functionsGroup=new traceClasses.MemberNode("Functions").setFontDetail(0,true);functionsGroup.viewerKind=6;this.members.add(functionsGroup);}
functionsGroup.add(getFunctionName(sProp));}}else{propertiesGroup.add(sProp,oProp,propType);}}catch(e){var msgEx=""+e;if(msgEx===e.message)
propertiesGroup.add(sProp,e);else
propertiesGroup.add(sProp,e.message,e);}}
if(propCount===0)
{propertiesGroup.add("No properties !");}}catch(e2){var msgExg=""+e2;if(msgExg===e2.message)
classGroup.add(e2);else
classGroup.add(e2,e2.message);}},addCaller:function(level)
{if(!this.enabled)
return;var stack;var stackList;var stackLength;var callObj;var callName;level=level||0;var group=new traceClasses.MemberNode("Call stack").setFontDetail(0,true);group.viewerKind=4;this.members.add(group);if(isRequireJs)
{stack=stackTrace.get(this.addCaller);stackLength=stack.length;for(let i=0;i<stackLength;i++)
{callObj=stack[i];if(callObj.getFileName().includes("tracetool.js")===false)
{if(level>0)
level--;else{callName=callObj.toString();group.add(callName);return;}}}}else{stack=new Error().stack;stackList=stack.split('\n');stackLength=stackList.length;for(let i=0;i<stackLength;i++)
{callObj=stackList[i].trim();if(callObj==="Error")
continue;if(callObj.includes("tracetool.js")===true)
continue;if(callObj.startsWith("at "))
{if(level>0)
level--;else{callName=callObj.substring(3);group.add(callName);return;}}}}},addStackTrace:function(level)
{if(!this.enabled)
return;level=level||0;var group=new traceClasses.MemberNode("Call stack").setFontDetail(0,true);group.viewerKind=4;this.members.add(group);var stack;var stackList;var stackLength;var callObj;var callName;if(isNodeJs)
{stack=stackTrace.get(this.addStackTrace);stackLength=stack.length;for(let i=0;i<stackLength;i++)
{callObj=stack[i];if(callObj.getFileName().includes("tracetool.js")===false)
{if(level>0)
level--;else{callName=callObj.toString();group.add(callName);}}}}else{stack=new Error().stack;stackList=stack.split('\n');stackLength=stackList.length;for(let i=0;i<stackLength;i++)
{callObj=stackList[i].trim();if(callObj==="Error")
continue;if(callObj.includes("tracetool.js")===true)
continue;if(callObj.startsWith("at "))
{if(level>0)
level--;else{callName=callObj.substring(3);group.add(callName);}}}}},send:function()
{var result=new traceClasses.TraceNode(this);if(!this.enabled)
return result;var commandList=new Array();if(this.threadName!=='')
commandList.push(intToStr5(305)+this.threadName);commandList.push(intToStr5(550)+this.parentNodeId);commandList.push(intToStr5(101)+this.id);if(typeof(this.leftMsg)!="undefined"&&(""+this.leftMsg)!=="")
commandList.push(intToStr5(551)+this.leftMsg);if(typeof(this.rightMsg)!="undefined"&&(""+this.rightMsg)!=="")
commandList.push(intToStr5(552)+this.rightMsg);commandList.push(intToStr5(103)+intToStr11(this.iconIndex));if(this.fontDetails!=null)
{for(var c=0;c<this.fontDetails.length;c++)
{var fontDetail=this.fontDetails[c];var colorValue;if(typeof(fontDetail.color)=="undefined"||fontDetail.color===null)
colorValue=-1;else
colorValue=rgbToBgr(fontDetail.color);var tempStr="";if(fontDetail.fontName==="BackgroundColor")
{commandList.push(intToStr5(568)+intToStr11(colorValue)+intToStr(fontDetail.colId));}else{tempStr+=intToStr5(567)+intToStr3(fontDetail.colId);if(fontDetail.bold)
tempStr+="1";else
tempStr+="0";if(fontDetail.italic)
tempStr+="1";else
tempStr+="0";tempStr+=intToStr11(colorValue)+intToStr11(fontDetail.size)+fontDetail.fontName;commandList.push(tempStr);}}
this.fontDetails=null;}
this.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId,this.time);return result;}};Object.defineProperty(traceClasses.TraceNodeEx.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceNodeEx.prototype'});traceClasses.TraceTable=function()
{Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceTable'});this.members=new traceClasses.MemberNode();this.members.viewerKind=3;;this.currentRow=null;}
traceClasses.TraceTable.prototype={addColumnTitle:function(colTitle)
{if(this.members.col1==="")
this.members.col1=colTitle;else
this.members.col1+="\t"+colTitle;},addRow:function()
{this.currentRow=this.members.add("");},addRowData:function(cell)
{if(this.currentRow===null)
addRow();if(this.currentRow.col1==="")
this.currentRow.col1=cell;else
this.currentRow.col1+="\t"+cell;},copyToNodeMembers:function(nodeMembers)
{var tableMembers=nodeMembers.add(this.members.col1);tableMembers.viewerKind=3;for(var c=0;c<this.members.members.length;c++)
tableMembers.add(this.members.members[c].col1);}};Object.defineProperty(traceClasses.TraceTable.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.TraceTable.prototype'});traceClasses.WinWatch=function(winWatchId,winWatchText)
{Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.WinWatch'});this.enabled=true;this.id="";if(typeof(winWatchId)=="undefined")
return;if(winWatchId===null||winWatchId==="")
this.id=newGuid();else
this.id=winWatchId;var commandList=new Array();if(winWatchText===null||winWatchText==="")
commandList.unshift(intToStr5(110)+"Watches "+this.id);else
commandList.unshift(intToStr5(110)+winWatchText);sendToWinWatchClient(commandList,this.id);}
traceClasses.WinWatch.prototype={displayWin:function()
{var commandList=new Array();commandList.unshift(intToStr5(97));sendToWinWatchClient(commandList,this.id);},clearAll:function()
{var commandList=new Array();commandList.unshift(intToStr5(104));sendToWinWatchClient(commandList,this.id);},close:function()
{var commandList=new Array();commandList.unshift(intToStr5(105));sendToWinWatchClient(commandList,this.id);},send:function(watchName,watchValue)
{if(!this.enabled)
return;var commandList=new Array();commandList.push(intToStr5(112)+watchName);var node=new traceClasses.TraceNodeEx(null,false);node.addValue(watchValue,ttrace.options.objectTreeDepth,"");node.members.addToStringList(commandList);sendToWinWatchClient(commandList,this.id);}};Object.defineProperty(traceClasses.WinWatch.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.WinWatch.prototype'});traceClasses.MemberNode=function(col1,col2,col3)
{Object.defineProperty(this,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.MemberNode'});this.viewerKind=0;this.col1=""+(col1||"");this.col2=""+(col2||"");this.col3=""+(col3||"");this.members=new Array();this.fontDetails=new Array();};traceClasses.MemberNode.prototype={add:function(col1,col2,col3)
{var member;if(col1 instanceof traceClasses.MemberNode)
{member=col1;}else{member=new traceClasses.MemberNode(col1,col2,col3);}
this.members.push(member);return member;},setFontDetail:function(colId,bold,italic,color,size,fontName)
{var fontDetail;if(getClassName(colId)==="TraceClasses.FontDetail")
{fontDetail=colId;}else{fontDetail=new traceClasses.FontDetail();if(typeof(colId)=="undefined")colId=-1;if(typeof(bold)=="undefined")bold=false;if(typeof(italic)=="undefined")italic=false;if(typeof(color)=="undefined")color=null;if(typeof(size)=="undefined")size=0;if(typeof(fontName)=="undefined")fontName='';fontDetail.colId=colId;fontDetail.bold=bold;fontDetail.italic=italic;fontDetail.color=color;fontDetail.size=size;fontDetail.fontName=fontName;}
if(typeof(this.fontDetails)=="undefined"||this.fontDetails===null)
this.fontDetails=new Array();this.fontDetails.push(fontDetail);return this;},addToStringList:function(commandList)
{function internalAddToStringList(subnode)
{var c;commandList.push(intToStr5(500)+subnode.col1);if(subnode.col2!=="")
commandList.push(intToStr5(502)+subnode.col2);if(subnode.col3!=="")
commandList.push(intToStr5(504)+subnode.col3);if(subnode.viewerKind!==0)
commandList.push(intToStr5(503)+subnode.viewerKind);if(subnode.fontDetails!=null)
{for(c=0;c<subnode.fontDetails.length;c++)
{var fontDetail=subnode.fontDetails[c];var tempStr="";tempStr+=intToStr5(501);tempStr+=intToStr3(fontDetail.colId);if(fontDetail.bold)
tempStr+="1";else
tempStr+="0";if(fontDetail.italic)
tempStr+="1";else
tempStr+="0";var colorValue;if(typeof(fontDetail.color)=="undefined"||fontDetail.color===null)
colorValue=-1;else
colorValue=rgbToBgr(fontDetail.color);tempStr+=intToStr11(colorValue)+intToStr11(fontDetail.size)+fontDetail.fontName;commandList.push(tempStr);}
subnode.fontDetails=null;}
for(c=0;c<subnode.members.length;c++)
{var node2=subnode.members[c];internalAddToStringList(node2);}
commandList.push(intToStr5(505));};for(var d=0;d<this.members.length;d++)
{var node=this.members[d];internalAddToStringList(node);}
this.members=new Array();}};Object.defineProperty(traceClasses.MemberNode.prototype,'classname',{enumerable:true,configurable:false,writable:false,value:'TraceClasses.MemberNode.prototype'});if(isCommonJS)
{module.exports=ttrace;}else if(isRequireJs){define({ttrace:ttrace});}
if(isBrowser)
{global.ttrace=ttrace;}})(typeof window!=="undefined"?window:this);