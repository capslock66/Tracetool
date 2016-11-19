
if(!window.ttrace){var tracetool=function()
{var RequestId=0;var toSend=[];var nbDone=0;var ttraceScript=null;var _winTrace=null;var _watches=null;var clientID="";var host="127.0.0.1:81";var headID=document.getElementsByTagName("head")[0];function setPrototype(constructorFunction,objPrototype)
{constructorFunction.prototype=objPrototype;};function extend(target,source)
{for(var property in source)
target[property]=source[property];return target;};function getFormattedTime()
{var currentDate=new Date();var date="";if(ttrace.options.sendDate==true)
date=""+currentDate.getFullYear()+intToStr(currentDate.getMonth(),2,'0')+intToStr(currentDate.getDay(),2,'0')+" ";var h=currentDate.getHours();var m=currentDate.getMinutes();var s=currentDate.getSeconds();var n=currentDate.getMilliseconds();return date+intToStr(h,2,'0')+':'+intToStr(m,2,'0')+':'+intToStr(s,2,'0')+':'+intToStr(n,3,'0');};function sendToWinTraceClient(commandList,winTraceId,dateTime)
{if(typeof(dateTime)=="undefined"||dateTime===null||dateTime=='')
commandList.unshift(intToStr5(304)+getFormattedTime());else
commandList.unshift(intToStr5(304)+dateTime);if(winTraceId!=null&&winTraceId!="")
commandList.unshift(intToStr5(99)+winTraceId);sendToClient(commandList);};function sendToWinWatchClient(commandList,winWatchId,dateTime)
{if(typeof(dateTime)=="undefined"||dateTime===null||dateTime=='')
commandList.unshift(intToStr5(304)+getFormattedTime());else
commandList.unshift(intToStr5(304)+dateTime);commandList.unshift(intToStr5(111)+winWatchId);sendToClient(commandList);};function sendToClient(commandList)
{var msgId=newGuid();var msg=commandList.join("\0");var msgLenth=msg.length;if(msgLenth>1000)
{var part;var partNum=1;var partLen;while(msgLenth>0)
{part=msg.substring(0,1000);msg=msg.substring(1000);msgLenth-=1000;partLen=part.length;if(partLen>=1000)
addMessage({msgId:msgId,msg:part,partNum:partNum});else
addMessage({msgId:msgId,msg:part,partNum:'Last'});partNum++;}}else{addMessage({msgId:msgId,msg:msg,partNum:''});}};function addMessage(objMessage)
{objMessage.command=objMessage.command||"WMD";toSend.push(objMessage);if(toSend.length==1){setTimeout(Worker,0);}};function runScript(objMessage)
{var ScriptUrl='http://'+host+'/'+objMessage.command+'?msgId='+objMessage.msgId+'&msg='+escape(objMessage.msg);if(objMessage.partNum!='')
ScriptUrl=ScriptUrl+'&partNum='+objMessage.partNum;var script;script=document.createElement('script');script.type='text/javascript';script.setAttribute("id","ttraceScript");script.setAttribute("name","ttraceScript");script.src=ScriptUrl;script.timeSend=new Date();script.message=objMessage
ttraceScript=script;headId.appendChild(script);setTimeout(Worker,20000);}
function Worker()
{if(ttraceScript===null&&toSend.length!=0)
{var objMessage=toSend.shift();runScript(objMessage);return}
if(ttraceScript!==null&&new Date()-ttraceScript.timeSend>19900)
{var objMessage=ttraceScript.message;headID.removeChild(ttraceScript);ttraceScript=null;runScript(objMessage);}}
function afterRun(msgId,partNum)
{if(ttraceScript===null)
return;msgId=msgId||'';partNum=partNum||'';headID.removeChild(ttraceScript);ttraceScript=null;if(toSend.length!=0)
{var objMessage=toSend.shift();runScript(objMessage);}};function lTrim(str)
{var k=0;while(k<str.length&&str.charAt(k)<=" ")k++;return str.substring(k,str.length);}
function rTrim(str)
{var k=str.length-1;while(k>=0&&str.charAt(k)<=" ")k--;return str.substring(0,k+1);}
function getFunctionName(fctName)
{fctName=rTrim(lTrim(fctName));if(fctName.indexOf('[ecmascript code]')==0)
return'?';if(fctName.indexOf('function ')==0)
{var p=9;var endClassName=1000;while(fctName.charAt(p)==' ')
p++;for(var c=p;c<fctName.length;c++)
{var ch=fctName.charAt(c);if(ch=='('){endClassName=c;break;}}
if(c==p)
return'?';return fctName.substring(9,endClassName);}
var pos=fctName.indexOf("{");if(pos>=0)
{if(pos==0)
return"<unnamed function>";fctName=fctName.substr(0,pos);pos=fctName.indexOf("(");if(pos!=-1)
fctName=fctName.substr(0,pos);if(fctName.substr(0,8)=="function")
fctName=fctName.substr(8,fctName.length);return fctName;}
return fctName;}
function rgbToBgr(color)
{var r;var g;var b;var hexString=rTrim(lTrim(color)).toUpperCase();if(hexString.charAt(0)=='#')
{r=parseInt(hexString.substring(1,3),16);g=parseInt(hexString.substring(3,5),16);b=parseInt(hexString.substring(5,7),16);}else if(hexString.indexOf('RGB')==0){var p=3;while(hexString.charAt(p)==' ')
p++;if(hexString.charAt(p)!='('&&hexString.charAt(hexString.length-1)!=')')
return 0;var block=hexString.substring(p+1,hexString.length-1);var rgb=block.split(',')
if(rgb.length!=3)
return 0;r=parseInt(rgb[0]);g=parseInt(rgb[1]);b=parseInt(rgb[2]);}else{return 0;}
return(b<<16)+(g<<8)+r;}
function newGuid()
{RequestId++;return clientID+'_'+RequestId;};function hex2Dec(x)
{var y=0,z;for(var i=0;i<x.length;i++){z=x.toUpperCase().charCodeAt(i)
y=16*y+z-((z<58)?48:55)}
return y;}
function intToHex(param,len)
{var str=(param).toString(16);while(str.length<len){str='0'+str;}
return str;}
function intToStr(param,len,padding)
{var str=''+param;padding=padding||' ';while(str.length<len){str=padding+str;}
return str;};function intToStr3(param)
{var str=''+param;while(str.length<3){str=' '+str;}
return str;};function intToStr5(param)
{var str=''+param;while(str.length<5){str=' '+str;}
return str;};function intToStr11(param)
{var str=''+param;while(str.length<11){str=' '+str;}
return str;};function getClassName(obj)
{if(obj===null)
return"null";var type=typeof(obj);if(type!="object")
return type;var objClassname=obj.classname||obj.className;if(typeof(objClassname)=="string"){return objClassname;}
try
{if(obj.nodeName){switch(obj.nodeType){case 0:return'NODE_INVALID';case 1:return'NODE_ELEMENT';case 2:return'NODE_ATTRIBUTE';case 3:return'NODE_TEXT';case 4:return'NODE_CDATA_SECTION';case 5:return'NODE_ENTITY_REFERENCE';case 6:return'NODE_ENTITY';case 7:return'NODE_PROCESSING_INSTRUCTION';case 8:return'NODE_COMMENT';case 9:return'NODE_DOCUMENT';case 10:return'NODE_DOCUMENT_TYPE';case 11:return'NODE_DOCUMENT_FRAGMENT';case 12:return'NODE_NOTATION';}}
if(typeof obj.length=='number'){if(obj.item)return'collection';if(obj.callee)return'arguments';}}catch(e){return e.message;}
var protoString=Object.prototype.toString.apply(obj);if(protoString.substr(0,7)=="[object"||protoString.substr(0,7)=="[Object")
{protoString=protoString.substring(7,protoString.length-1);protoString=protoString.replace(/^\s*|\s*$/g,"");if(protoString=="")
protoString="Object";if(protoString.toLowerCase()!="object")
return protoString;}
try
{if("constructor"in obj)
{if(typeof(obj.constructor)=="undefined")
return protoString;switch(obj.constructor){case Array:return'Array';case RegExp:return'Regexp';}
if(obj.constructor==Object)
return protoString;if(typeof(obj.constructor.prototype)=="undefined")
return protoString;var propType=obj.constructor.toString();if(propType.length==0)
return protoString;var pos=propType.indexOf("{");if(pos==-1)
return propType;if(pos==0)
return"<unnamed constructor>"+protoString;propType=propType.substr(0,pos);pos=propType.indexOf("(");if(pos!=-1)
propType=propType.substr(0,pos);if(propType.substr(0,8)=="function")
propType=propType.substr(8,propType.length);return propType;}}catch(e){return protoString;}
return protoString;}
function prepareNewNode(parentNode,leftMsg,newId)
{var parentContext=parentNode.context.getLast();var parentContextId=parentNode.id;if(parentContext!='')
parentContextId=parentContext;var commandList=new Array();commandList.push(intToStr5(550)+parentContextId);commandList.push(intToStr5(101)+newId);commandList.push(intToStr5(551)+leftMsg);if(parentNode.iconIndex!=-1)
commandList.push(intToStr5(103)+intToStr11(parentNode.iconIndex));return commandList;}
ttrace={classname:"ttrace",classes:{},getWaitingMessageCount:function()
{return toSend.length;},getSendMessageCount:function()
{return nbDone;},setHost:function(newHost)
{host=newHost;},getHost:function()
{return host;},queryClientId:function()
{addMessage({msgId:"",msg:"",partNum:"",command:"UniqueClientId"});},setClientID:function(newId)
{clientID=newId;afterRun();},getClientID:function()
{return clientID;},show:function(isVisible)
{var commandList=new Array();if(typeof(isVisible)=="undefined")
isVisible=true;if(isVisible)
commandList.push(intToStr5(102)+'1');else
commandList.push(intToStr5(102)+'0');sendToClient(commandList);},closeViewer:function()
{var commandList=new Array();commandList.push(intToStr5(106));sendToClient(commandList);},clearAll:function()
{ttrace.winTrace().clearAll();},find:function(Text,Sensitive,WholeWord,Highlight,SearchInAllPages)
{var commandList=new Array();var flags=0;Sensitive=Sensitive||false;WholeWord=WholeWord||false;Highlight=Highlight||false;SearchInAllPages=SearchInAllPages||false;if(Sensitive)
flags+=8;if(WholeWord)
flags+=4;if(Highlight)
flags+=2;if(SearchInAllPages)
flags+=1;commandList.push(intToStr5(100)+intToStr11(flags)+Text);sendToClient(commandList);},winTrace:function()
{if(_winTrace===null)
_winTrace=new ttrace.classes.WinTrace();return _winTrace;},watches:function()
{if(_watches===null)
_watches=new ttrace.classes.WinWatch();return _watches;},debug:function()
{return this.winTrace().debug();},warning:function()
{return this.winTrace().warning();},error:function()
{return this.winTrace().error();},display:function(left,right)
{if(!window.jsTrace)
return;if(typeof(right)=="undefined")
jsTrace.send(left)
else
jsTrace.send(left+'   :   '+right);},_done:function(msgId,partNum)
{nbDone++;afterRun(msgId,partNum);}};ttrace.options={sendFunctions:true,sendDate:false,objectTreeDepth:3};ttrace.classes.FontDetail=function()
{this.classname='ttrace.classes.FontDetail';this.colId=0;this.bold=false;this.italic=false;this.color="";this.size=0;this.fontName="";}
setPrototype(ttrace.classes.FontDetail,{classname:'FontDetail prototype'});ttrace.classes.Context=function()
{this.classname='ttrace.classes.Context';this.contextList=[];this.winTraceContext=null;}
setPrototype(ttrace.classes.Context,{classname:'Context prototype',getLast:function()
{var cList=null;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;if(cList.length==0)
return"";return cList[0];},push:function(newContext)
{var cList=null;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;cList.unshift(newContext);},level:function()
{var cList=null;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;return cList.length;},deleteLast:function()
{var cList=null;if(this.winTraceContext!=null)
cList=this.winTraceContext;else
cList=this.contextList;if(cList.length==0)
return"";return cList.shift();}});ttrace.classes.TraceToSend=function()
{this.classname='ttrace.classes.TraceToSend';var _that=this;this.id='';this.iconIndex=-1;this.enabled=true;this.winTraceId='';this.context=new ttrace.classes.Context();this.context.list={};};setPrototype(ttrace.classes.TraceToSend,{classname:'ttrace.classes.TraceToSend prototype',send:function(leftMsg,rightMsg)
{if(this.enabled==false)
return new ttrace.classes.TraceNode(this);var result=new ttrace.classes.TraceNode(this,true);var commandList=prepareNewNode(this,leftMsg,result.id);if(typeof(rightMsg)!="undefined"&&(""+rightMsg)!="")
commandList.push(intToStr5(552)+rightMsg);sendToWinTraceClient(commandList,this.winTraceId);return result;},indent:function(leftMsg,rightMsg,backGroundColor,isEnter)
{if(!this.enabled)
return;var newId=newGuid();var commandList=prepareNewNode(this,leftMsg,newId);if(typeof(rightMsg)!="undefined"&&(""+rightMsg)!="")
commandList.push(intToStr5(552)+rightMsg);if(backGroundColor!=null)
{var colorValue=rgbToBgr(backGroundColor);commandList.push(intToStr5(568)+intToStr11(colorValue)+"-1");}
if(isEnter||true)
{member=new ttrace.classes.MemberNode();member.add("").viewerKind=8;member.addToStringList(commandList);}
sendToWinTraceClient(commandList,this.winTraceId);this.context.push(newId);},unIndent:function(leftMsg,rightMsg,backGroundColor,isExit)
{if(!this.enabled)
return;this.context.deleteLast();if((""+leftMsg)!=""||(""+rightMsg)!="")
{var newId=newGuid();var commandList=prepareNewNode(this,leftMsg,newId);if(typeof(rightMsg)!="undefined"&&(""+rightMsg)!="")
commandList.push(intToStr5(552)+rightMsg);if(backGroundColor!=null){var colorValue=rgbToBgr(backGroundColor);commandList.push(intToStr5(568)+intToStr11(colorValue)+"-1");}
if(isExit||true)
{member=new ttrace.classes.MemberNode();member.add("").viewerKind=9;member.addToStringList(commandList);}
sendToWinTraceClient(commandList,this.winTraceId);}},sendBackgroundColor:function(leftMsg,color,colId)
{if(this.enabled==false)
return new ttrace.classes.TraceNode(this);var result=new ttrace.classes.TraceNode(this,true);var commandList=prepareNewNode(this,leftMsg,result.id);var colorValue=rgbToBgr(color);commandList.push(intToStr5(568)+intToStr11(colorValue)+intToStr5(colId));sendToWinTraceClient(commandList,this.winTraceId);return result;},enterMethod:function(leftMsg,rightMsg,backGroundColor)
{if(!this.enabled)
return;this.indent("Enter "+leftMsg,rightMsg,backGroundColor,true);},exitMethod:function(leftMsg,rightMsg,backGroundColor)
{if(!this.enabled)
return;this.unIndent("Exit "+leftMsg,rightMsg,backGroundColor,true);},sendValue:function(leftMsg,objToSend,maxLevel,title)
{if(!this.enabled)
return new ttrace.classes.TraceNode(this);var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Object",result.id);result.addValue(objToSend,maxLevel,title);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},sendObject:function(leftMsg,objToSend,displayFunctions)
{if(!this.enabled)
return new ttrace.classes.TraceNode(this);var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Object",result.id);result.addObject(objToSend,displayFunctions);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},sendStack:function(leftMsg,level)
{if(!this.enabled)
return new ttrace.classes.TraceNode(this);level=level||1;var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Stack",result.id);result.addStackTrace(level+1);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},sendCaller:function(leftMsg,level)
{if(!this.enabled)
return new ttrace.classes.TraceNode(this);level=level||1;var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Caller",result.id);result.addCaller(level+1);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},sendDump:function(leftMsg,shortTitle,buffer,count)
{if(!this.enabled)
return;var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Stack",result.id);result.addDump(shortTitle,buffer,count);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},sendXml:function(leftMsg,xml)
{if(!this.enabled)
return;var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg||"Stack",result.id);result.addXML(xml);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},sendTable:function(leftMsg,table)
{if(!this.enabled)
return;var result=new ttrace.classes.TraceNodeEx(this,true);var commandList=prepareNewNode(this,leftMsg,result.id);result.addTable(table);result.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId);return new ttrace.classes.TraceNode(result);},indentLevel:function()
{return this.context.level();}});ttrace.classes.TraceNode=function(parentNode,generateUniqueId)
{this.classname='ttrace.classes.TraceNode';parentNode=parentNode||null;if(typeof(generateUniqueId)=="undefined")generateUniqueId=true;this.id='';if(generateUniqueId)
this.id=newGuid();this.iconIndex=-1;this.enabled=true;this.winTraceId='';if(parentNode!==null)
{this.iconIndex=parentNode.iconIndex;this.enabled=parentNode.enabled;this.winTraceId=parentNode.winTraceId;}};var traceNode_Prototype=new ttrace.classes.TraceToSend();setPrototype(ttrace.classes.TraceNode,traceNode_Prototype);extend(traceNode_Prototype,{classname:'ttrace.classes.TraceNode prototype',resend:function(newLeftMsg,newRightMsg)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(typeof(newLeftMsg)!="undefined")
commandList.push(intToStr5(551)+newLeftMsg);if(typeof(newRightMsg)!="undefined")
commandList.push(intToStr5(552)+newRightMsg);sendToWinTraceClient(commandList,this.winTraceId);return this;},resendLeft:function(newLeftMsg)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(551)+newLeftMsg);sendToWinTraceClient(commandList,this.winTraceId);return this;},resendRight:function(newRightMsg)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(552)+newRightMsg);sendToWinTraceClient(commandList,this.winTraceId);return this;},resendIconIndex:function(index)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(103)+index);sendToWinTraceClient(commandList,this.winTraceId);return this;},setBackgroundColor:function(color,colId)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();var colorValue=rgbToBgr(color);commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(568)+intToStr11(colorValue)+intToStr3(colId));sendToWinTraceClient(commandList,this.winTraceId);return this;},append:function(leftMsgtoAdd,rightMsgtoAdd)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(typeof(leftMsgtoAdd)!="undefined")
commandList.push(intToStr5(556)+leftMsgtoAdd);if(typeof(rightMsgtoAdd)!="undefined")
commandList.push(intToStr5(557)+rightMsgtoAdd);sendToWinTraceClient(commandList,this.winTraceId);return this;},appendLeft:function(leftMsgtoAdd)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(556)+leftMsgtoAdd);sendToWinTraceClient(commandList,this.winTraceId);return this;},appendRight:function(rightMsgtoAdd)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(557)+rightMsgtoAdd);sendToWinTraceClient(commandList,this.winTraceId);return this;},show:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(558)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},setSelected:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(553)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},deleteIt:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(300)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},deleteChildren:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(301)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},setBookmark:function(Bookmarked)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(Bookmarked)
commandList.push(intToStr5(122)+intToStr11(1));else
commandList.push(intToStr5(122)+intToStr11(0));sendToWinTraceClient(commandList,this.winTraceId);return this;},setVisible:function(Visible)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);if(Visible)
commandList.push(intToStr5(123)+intToStr11(1));else
commandList.push(intToStr5(123)+intToStr11(0));sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoNextSibling:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(114)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoPrevSibling:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(115)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoFirstChild:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(116)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},gotoLastChild:function()
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(117)+this.id);sendToWinTraceClient(commandList,this.winTraceId);return this;},setFontDetail:function(colId,bold,italic,color,size,fontName)
{if(!this.enabled)
return this;if(this.id=="")
return this;var commandList=new Array();commandList.push(intToStr5(555)+this.id);var tempStr="";if(getClassName(colId)=="ttrace.classes.FontDetail")
{var fontDetail=colId;bold=fontDetail.bold;italic=fontDetail.italic;color=fontDetail.color;size=fontDetail.size;fontName=fontDetail.fontName;colId=fontDetail.colId;}else{if(typeof(colId)=="undefined")colId=-1;if(typeof(bold)=="undefined")bold=true;if(typeof(italic)=="undefined")italic=false
if(typeof(color)=="undefined")color=null;if(typeof(size)=="undefined")size=0;if(typeof(fontName)=="undefined")fontName='';}
tempStr+=intToStr5(567)+intToStr3(colId);if(bold)
tempStr+="1";else
tempStr+="0";if(italic)
tempStr+="1";else
tempStr+="0";var colorValue;if(color===null)
colorValue=-1;else
colorValue=rgbToBgr(color);tempStr+=intToStr11(colorValue)+intToStr11(size)+fontName;commandList.push(tempStr);sendToWinTraceClient(commandList,this.winTraceId);return this;}});ttrace.classes.WinTrace=function(winTraceId,winTraceText)
{var _that=null;var _debug=null;var _warning=null;var _error=null;var _context=new Array();this.id='';this.iconIndex=-1;this.enabled=true;this.parentNodeId='';this.winTraceId='';this.members=null;_that=this;this.classname='ttrace.classes.WinTrace';if(arguments.length==0)
{createNodes();}else{if(typeof(winTraceId)=="undefined"||winTraceId=="")
_that.id=newGuid();else
_that.id=winTraceId;createNodes();if(typeof(winTraceId)!="undefined"&&winTraceId!=null&&winTraceId=="_")
return;if(typeof(winTraceText)=="undefined"||winTraceText===null||winTraceText=="")
winTraceText=_that.id;var commandList=new Array();commandList.unshift(intToStr5(98)+winTraceText);sendToWinTraceClient(commandList,_that.id);}
function createNodes()
{_that.winTraceId=_that.id;_debug=new ttrace.classes.TraceToSend(null,false,_context);_debug.iconIndex=24;_debug.winTraceId=_that.id;_debug.enabled=true;_warning=new ttrace.classes.TraceToSend(null,false,_context);_warning.iconIndex=22;_warning.winTraceId=_that.id;_warning.enabled=true;_error=new ttrace.classes.TraceToSend(null,false,_context);_error.iconIndex=23;_error.winTraceId=_that.id;_error.enabled=true;};this.debug=function()
{return _debug;};this.warning=function()
{return _warning;};this.error=function()
{return _error;};};var winTrace_Prototype=new ttrace.classes.TraceToSend();setPrototype(ttrace.classes.WinTrace,winTrace_Prototype);extend(winTrace_Prototype,{classname:'ttrace.classes.WinTrace prototype',saveToTextFile:function(fileName)
{var commandList=new Array();commandList.unshift(intToStr5(559)+fileName);sendToWinTraceClient(commandList,this.id);},saveToXml:function(fileName,StyleSheet)
{var commandList=new Array();if(typeof(StyleSheet)=="undefined")
commandList.unshift(intToStr5(560)+fileName);else
commandList.unshift(intToStr5(560)+fileName+'|'+StyleSheet);sendToWinTraceClient(commandList,this.id);},loadXml:function(fileName)
{var commandList=new Array();commandList.unshift(intToStr5(561)+fileName);sendToWinTraceClient(commandList,this.id);},setLogFile:function(fileName,mode,maxLines)
{var commandList=new Array();mode=mode||1;maxLines=maxLines||-1;commandList.unshift(intToStr5(562)+intToStr11(mode)+intToStr11(maxLines)+fileName);sendToWinTraceClient(commandList,this.id);},displayWin:function()
{var commandList=new Array();commandList.unshift(intToStr5(97));sendToWinTraceClient(commandList,this.id);},setMultiColumn:function(MainColIndex)
{MainColIndex=MainColIndex||0;var commandList=new Array();commandList.unshift(intToStr5(95)+intToStr11(MainColIndex));sendToWinTraceClient(commandList,this.id);},setColumnsTitle:function(titles)
{var commandList=new Array();commandList.unshift(intToStr5(96)+titles);sendToWinTraceClient(commandList,this.id);},setColumnsWidth:function(widths)
{var commandList=new Array();commandList.unshift(intToStr5(93)+widths);sendToWinTraceClient(commandList,this.id);},gotoFirstNode:function()
{var commandList=new Array();commandList.unshift(intToStr5(80));sendToWinTraceClient(commandList,this.id);},gotoLastNode:function()
{var commandList=new Array();commandList.unshift(intToStr5(81));sendToWinTraceClient(commandList,this.id);},findNext:function(SearForward)
{var commandList=new Array();if(SearForward)
commandList.unshift(intToStr5(82)+intToStr11(1));else
commandList.unshift(intToStr5(82)+intToStr11(0));sendToWinTraceClient(commandList,this.id);},gotoBookmark:function(Pos)
{var commandList=new Array();commandList.unshift(intToStr5(83)+intToStr11(Pos));sendToWinTraceClient(commandList,this.id);},clearBookmark:function()
{var commandList=new Array();commandList.unshift(intToStr5(84));sendToWinTraceClient(commandList,this.id);},clearFilter:function()
{var commandList=new Array();commandList.unshift(intToStr5(85));sendToWinTraceClient(commandList,this.id);},addFilter:function(Column,Compare,Text)
{var commandList=new Array();commandList.unshift(intToStr5(86)+intToStr11(Column)+intToStr11(Compare)+Text);sendToWinTraceClient(commandList,this.id);},applyFilter:function(ConditionAnd,ShowMatch,IncludeChildren)
{var flags=0;if(ConditionAnd)
flags+=4;if(ShowMatch)
flags+=2;if(IncludeChildren)
flags+=1;var commandList=new Array();commandList.unshift(intToStr5(87)+intToStr11(flags));sendToWinTraceClient(commandList,this.id);},clearAll:function()
{var commandList=new Array();commandList.unshift(intToStr5(104));sendToWinTraceClient(commandList,this.id);},close:function()
{var commandList=new Array();commandList.unshift(intToStr5(105));sendToWinTraceClient(commandList,this.id);}});ttrace.classes.TraceNodeEx=function(parentNode,generateUniqueId)
{this.classname='ttrace.classes.TraceNodeEx';var _that=this;this.fontDetails=new Array();this.leftMsg='';this.rightMsg='';this.time='';this.threadName='';this.members=new ttrace.classes.MemberNode();this.parentNodeId="";this.iconIndex=-1;this.enabled=true;this.winTraceId=null;this.id='';if(typeof(generateUniqueId)=="undefined")
generateUniqueId=true;if(generateUniqueId)
this.id=newGuid();if(parentNode!==null)
{this.iconIndex=parentNode.iconIndex;this.enabled=parentNode.enabled;this.winTraceId=parentNode.winTraceId;var parentContext=parentNode.context.getLast();if(parentContext!='')
this.parentNodeId=parentContext;}};setPrototype(ttrace.classes.TraceNodeEx,{classname:'ttrace.classes.TraceNodeEx prototype',addXML:function(xml)
{if(!this.enabled)
return;if(typeof(xml)=="undefined")xml="";var member=this.members.add(xml);member.viewerKind=2;},addDump:function(shortTitle,buffer,count)
{if(!this.enabled)
return;count=count||buffer.length;var dumpGroup=new ttrace.classes.MemberNode(shortTitle).setFontDetail(0,true);dumpGroup.viewerKind=1;this.members.add(dumpGroup);var byteDumped=0;var c=0;while(byteDumped<count&&(c<buffer.length))
{var d=0;var beginLine=c;var hexaRepresentation='';while((byteDumped<count)&&(d<16)&&(c<buffer.length))
{var oneChar=buffer.charAt(c);var oneInt=buffer.charCodeAt(c);var str=intToHex(oneInt,2);hexaRepresentation+=str+' ';byteDumped++;d++;c++;}
adr=intToHex(beginLine,6);dumpGroup.add(adr,hexaRepresentation);}
dumpGroup.col2=''+byteDumped+" char(s) dumped";},addTable:function(table)
{if(!this.enabled)
return;var objClassName=getClassName(table);var obj;if(objClassName=="string"){obj=eval(table);objClassName=getClassName(table);}else
obj=table;if(objClassName=="ttrace.classes.TraceTable"){table.copyToNodeMembers(this.members);}else if(objClassName=="Array"){var TableMembers=new ttrace.classes.MemberNode('Index').setFontDetail(0,true);TableMembers.viewerKind=3;this.members.add(TableMembers);var isFirst=true;for(var key in obj){var itemObject=obj[key];var fCurrentRow=TableMembers.add('['+key+']');var itemClassName=getClassName(itemObject);if(typeof(itemObject)!="object"){if(isFirst==true)
TableMembers.col1+="\t"+"Value";fCurrentRow.col1+="\t"+itemObject;}else{var propType;var memberValue;for(var memberName in itemObject){try{if(memberName=="ELEMENT_NODE"||memberName=="ATTRIBUTE_NODE"||memberName=="TEXT_NODE"||memberName=="CDATA_SECTION_NODE"||memberName=="ENTITY_REFERENCE_NODE"||memberName=="ENTITY_NODE"||memberName=="PROCESSING_INSTRUCTION_NODE"||memberName=="COMMENT_NODE"||memberName=="DOCUMENT_NODE"||memberName=="DOCUMENT_TYPE_NODE"||memberName=="DOCUMENT_FRAGMENT_NODE"||memberName=="NOTATION_NODE"||memberName=="DOCUMENT_POSITION_DISCONNECTED"||memberName=="DOCUMENT_POSITION_PRECEDING"||memberName=="DOCUMENT_POSITION_FOLLOWING"||memberName=="DOCUMENT_POSITION_CONTAINS"||memberName=="DOCUMENT_POSITION_CONTAINED_BY"||memberName=="DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
continue;memberValue=itemObject[memberName];propType=getClassName(memberValue);if(propType=="function")
continue;if(isFirst==true)
TableMembers.col1+="\t"+memberName;fCurrentRow.col1=fCurrentRow.col1+"\t"+memberValue;}
catch(e){var node;var msg=""+e;if(msg==e.message)
node=new ttrace.classes.MemberNode(memberName,e);else
node=new ttrace.classes.MemberNode(memberName,e.message,e);TableMembers.add(node);}}}
isFirst=false;}}else{this.addValue(obj,1,"")}},resend:function()
{if(!this.enabled)
return;var commandList=new Array();commandList.push(intToStr5(555)+this.id);commandList.push(intToStr5(551)+this.leftMsg);commandList.push(intToStr5(552)+this.rightMsg);sendToWinTraceClient(commandList,this.winTraceId);},addBackgroundColor:function(color,colId)
{if(!this.enabled)
return;var fontDetail=new ttrace.classes.FontDetail();fontDetail.colId=colId;fontDetail.color=color;fontDetail.fontName="BackgroundColor";if(typeof(this.fontDetails)=="undefined"||this.fontDetails===null)
this.fontDetails=new Array();this.fontDetails.push(fontDetail);},addFontDetail:function(colId,bold,italic,color,size,fontName)
{if(!this.enabled)
return this;var fontDetail;if(getClassName(colId)=="ttrace.classes.FontDetail")
{fontDetail=colId;}else{fontDetail=new ttrace.classes.FontDetail();if(typeof(colId)=="undefined")colId=-1;if(typeof(bold)=="undefined")bold=true;if(typeof(italic)=="undefined")italic=false
if(typeof(color)=="undefined")color=null;if(typeof(size)=="undefined")size=0;if(typeof(fontName)=="undefined")fontName='';fontDetail.colId=colId;fontDetail.bold=bold;fontDetail.italic=italic;fontDetail.color=color;fontDetail.size=size;fontDetail.fontName=fontName;}
if(typeof(this.fontDetails)=="undefined"||this.fontDetails===null)
this.fontDetails=new Array();this.fontDetails.push(fontDetail);return this;},addValue:function(objToSend,maxLevel,title)
{function innerAddValue(objToSend,upperNode,maxLevel,alreadyParsedObject)
{try
{if(objToSend===null)
{upperNode.col2="Null";return;}
var objClass=getClassName(objToSend);upperNode.col3=upperNode.col3+objClass;if(typeof(objToSend)!="object")
{upperNode.col2=''+objToSend.toString();return;}
for(var i=0;i<alreadyParsedObject.length;i++)
if(alreadyParsedObject[i]==objToSend)
{upperNode.col2="...";return;}
if(maxLevel<=1)
return;alreadyParsedObject.push(objToSend);var memberValue;var propType;for(var memberName in objToSend)
{try
{if(memberName=="ELEMENT_NODE"||memberName=="ATTRIBUTE_NODE"||memberName=="TEXT_NODE"||memberName=="CDATA_SECTION_NODE"||memberName=="ENTITY_REFERENCE_NODE"||memberName=="ENTITY_NODE"||memberName=="PROCESSING_INSTRUCTION_NODE"||memberName=="COMMENT_NODE"||memberName=="DOCUMENT_NODE"||memberName=="DOCUMENT_TYPE_NODE"||memberName=="DOCUMENT_FRAGMENT_NODE"||memberName=="NOTATION_NODE"||memberName=="DOCUMENT_POSITION_DISCONNECTED"||memberName=="DOCUMENT_POSITION_PRECEDING"||memberName=="DOCUMENT_POSITION_FOLLOWING"||memberName=="DOCUMENT_POSITION_CONTAINS"||memberName=="DOCUMENT_POSITION_CONTAINED_BY"||memberName=="DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
continue;memberValue=objToSend[memberName];if(objClass=="Array")
memberName="["+memberName+"]";propType=getClassName(memberValue);if(propType=="function")
continue;var node=new ttrace.classes.MemberNode(memberName,"","");upperNode.add(node);innerAddValue(memberValue,node,maxLevel-1,alreadyParsedObject);}catch(e){var node;var msg=""+e;if(msg==e.message)
node=new ttrace.classes.MemberNode(memberName,e);else
node=new ttrace.classes.MemberNode(memberName,e.message,e);upperNode.add(node);}}}catch(e){var node=new ttrace.classes.MemberNode(e.message);upperNode.add(node);}};if(!this.enabled)
return;if(objToSend===null)
{this.members.add("undefined");return;}
if(typeof(maxLevel)=="undefined")maxLevel=ttrace.options.objectTreeDepth;if(typeof(title)=="undefined")title="Object Value";var alreadyParsedObject=new Array();var result=new ttrace.classes.MemberNode(title);result.viewerKind=7;this.members.add(result);innerAddValue(objToSend,result,maxLevel,alreadyParsedObject);},addObject:function(objName,displayFunctions)
{if(!this.enabled)
return;var msg=""
var oProp;var propType;var propCount=0;var propertiesGroup=null;var functionsGroup=null;var classGroup=null;classGroup=new ttrace.classes.MemberNode("Class information").setFontDetail(0,true);classGroup.viewerKind=6;this.members.add(classGroup);propertiesGroup=new ttrace.classes.MemberNode("Properties").setFontDetail(0,true);propertiesGroup.viewerKind=6;this.members.add(propertiesGroup);try
{if(typeof(objName)=="undefined")
{this.members.add("undefined");return;}
if(typeof(displayFunctions)=="undefined")
displayFunctions=ttrace.options.sendFunctions;var obj;if(typeof(objName)=="string")
obj=eval(objName);else
obj=objName;classGroup.add('Class name',getClassName(obj));for(var sProp in obj)
{propCount++;try
{if(sProp=="ELEMENT_NODE"||sProp=="ATTRIBUTE_NODE"||sProp=="TEXT_NODE"||sProp=="CDATA_SECTION_NODE"||sProp=="ENTITY_REFERENCE_NODE"||sProp=="ENTITY_NODE"||sProp=="PROCESSING_INSTRUCTION_NODE"||sProp=="COMMENT_NODE"||sProp=="DOCUMENT_NODE"||sProp=="DOCUMENT_TYPE_NODE"||sProp=="DOCUMENT_FRAGMENT_NODE"||sProp=="NOTATION_NODE"||sProp=="DOCUMENT_POSITION_DISCONNECTED"||sProp=="DOCUMENT_POSITION_PRECEDING"||sProp=="DOCUMENT_POSITION_FOLLOWING"||sProp=="DOCUMENT_POSITION_CONTAINS"||sProp=="DOCUMENT_POSITION_CONTAINED_BY"||sProp=="DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC")
continue;oProp=obj[sProp];propType=typeof(oProp);propType=getClassName(oProp);if(propType=="function")
{if(displayFunctions==true)
{if(functionsGroup===null)
{functionsGroup=new ttrace.classes.MemberNode("Functions").setFontDetail(0,true);functionsGroup.viewerKind=6;this.members.add(functionsGroup);}
functionsGroup.add(getFunctionName(sProp));}}else{propertiesGroup.add(sProp,oProp,propType);}}catch(e){var msg=""+e;if(msg==e.message)
propertiesGroup.add(sProp,e);else
propertiesGroup.add(sProp,e.message,e);}}
if(propCount==0)
{propertiesGroup.add("No properties !");}}catch(e){var msg=""+e;if(msg==e.message)
classGroup.add(e);else
classGroup.add(e,e.message);}},addCaller:function(level)
{if(!this.enabled)
return;level=level||1;var group=new ttrace.classes.MemberNode("Call stack").setFontDetail(0,true);group.viewerKind=4;this.members.add(group);callObj=arguments.callee;while(callObj)
{if(level>0)
level--;else{callName=callObj.name?callObj.name:callObj.toString();callName=getFunctionName(callName);group.add(callName);return;}
callObj=callObj.caller;}},addStackTrace:function(level)
{if(!this.enabled)
return;level=level||1;var group=new ttrace.classes.MemberNode("Call stack").setFontDetail(0,true);group.viewerKind=4;this.members.add(group);callObj=arguments.callee;while(callObj)
{if(level>0)
level--;else{callName=callObj.name?callObj.name:callObj.toString();callName=getFunctionName(callName);group.add(callName);}
callObj=callObj.caller;}},send:function()
{var result=new ttrace.classes.TraceNode(this);if(!this.enabled)
return result;var commandList=new Array();if(this.threadName!='')
commandList.push(intToStr5(305)+this.threadName);commandList.push(intToStr5(550)+this.parentNodeId);commandList.push(intToStr5(101)+this.id);if(typeof(this.leftMsg)!="undefined"&&(""+this.leftMsg)!="")
commandList.push(intToStr5(551)+this.leftMsg);if(typeof(this.rightMsg)!="undefined"&&(""+this.rightMsg)!="")
commandList.push(intToStr5(552)+this.rightMsg);commandList.push(intToStr5(103)+intToStr11(this.iconIndex));if(this.fontDetails!=null)
{for(var c=0;c<this.fontDetails.length;c++)
{var fontDetail=this.fontDetails[c];var colorValue;if(typeof(fontDetail.color)=="undefined"||fontDetail.color===null)
colorValue=-1;else
colorValue=rgbToBgr(fontDetail.color);var tempStr="";if(fontDetail.fontName=="BackgroundColor")
{commandList.push(intToStr5(568)+intToStr11(colorValue)+intToStr(fontDetail.colId));}else{tempStr+=intToStr5(567)+intToStr3(fontDetail.colId);if(fontDetail.bold)
tempStr+="1";else
tempStr+="0";if(fontDetail.italic)
tempStr+="1";else
tempStr+="0";tempStr+=intToStr11(colorValue)+intToStr11(fontDetail.size)+fontDetail.fontName;commandList.push(tempStr);}}
this.fontDetails=null;}
this.members.addToStringList(commandList);sendToWinTraceClient(commandList,this.winTraceId,this.time);return result;}});ttrace.classes.TraceTable=function()
{this.classname='ttrace.classes.TraceTable';this.members=new ttrace.classes.MemberNode();this.members.viewerKind=3;;this.currentRow=null;}
setPrototype(ttrace.classes.TraceTable,{classname:'ttrace.classes.TraceTable prototype',addColumnTitle:function(colTitle)
{if(this.members.col1=="")
this.members.col1=colTitle;else
this.members.col1+="\t"+colTitle;},addRow:function()
{this.currentRow=this.members.add("");},addRowData:function(cell)
{if(this.currentRow===null)
addRow();if(this.currentRow.col1=="")
this.currentRow.col1=cell;else
this.currentRow.col1+="\t"+cell;},copyToNodeMembers:function(nodeMembers)
{var tableMembers=nodeMembers.add(this.members.col1);tableMembers.viewerKind=3;for(var c=0;c<this.members.members.length;c++)
tableMembers.add(this.members.members[c].col1);}});ttrace.classes.WinWatch=function(winWatchID,winWatchText)
{this.classname='ttrace.classes.WinWatch';this.enabled=true;this.id="";if(typeof(winWatchID)=="undefined")
return;if(winWatchID===null||winWatchID=="")
this.id=newGuid();else
this.id=winWatchID;var commandList=new Array();if(winWatchText===null||winWatchText=="")
commandList.unshift(intToStr5(110)+"Watches "+this.id);else
commandList.unshift(intToStr5(110)+winWatchText);sendToWinWatchClient(commandList,this.id);}
setPrototype(ttrace.classes.WinWatch,{classname:'ttrace.classes.WinWatch prototype',displayWin:function()
{var commandList=new Array();commandList.unshift(intToStr5(97));sendToWinWatchClient(commandList,this.id);},clearAll:function()
{var commandList=new Array();commandList.unshift(intToStr5(104));sendToWinWatchClient(commandList,this.id);},close:function()
{var commandList=new Array();commandList.unshift(intToStr5(105));sendToWinWatchClient(commandList,this.id);},send:function(watchName,watchValue)
{if(!this.enabled)
return;var commandList=new Array();commandList.push(intToStr5(112)+watchName);var node=new ttrace.classes.TraceNodeEx(null,false);node.addValue(watchValue,ttrace.options.objectTreeDepth,"");node.members.addToStringList(commandList);sendToWinWatchClient(commandList,this.id);}});ttrace.classes.MemberNode=function(col1,col2,col3)
{this.classname="ttrace.classes.MemberNode"
this.viewerKind=0;this.col1=""+(col1||"");this.col2=""+(col2||"");this.col3=""+(col3||"");this.members=new Array();this.fontDetails=new Array();};setPrototype(ttrace.classes.MemberNode,{classname:'ttrace.classes.MemberNode prototype',add:function(col1,col2,col3)
{var member;if(getClassName(col1)=="ttrace.classes.MemberNode")
{member=col1;}else{member=new ttrace.classes.MemberNode(col1,col2,col3);}
this.members.push(member);return member;},setFontDetail:function(colId,bold,italic,color,size,fontName)
{var fontDetail;if(getClassName(colId)=="ttrace.classes.FontDetail")
{fontDetail=colId;}else{fontDetail=new ttrace.classes.FontDetail();if(typeof(colId)=="undefined")colId=-1;if(typeof(bold)=="undefined")bold=false;if(typeof(italic)=="undefined")italic=false
if(typeof(color)=="undefined")color=null;if(typeof(size)=="undefined")size=0;if(typeof(fontName)=="undefined")fontName='';fontDetail.colId=colId;fontDetail.bold=bold;fontDetail.italic=italic;fontDetail.color=color;fontDetail.size=size;fontDetail.fontName=fontName;}
if(typeof(this.fontDetails)=="undefined"||this.fontDetails===null)
this.fontDetails=new Array();this.fontDetails.push(fontDetail);return this;},addToStringList:function(commandList)
{function internalAddToStringList(subnode)
{commandList.push(intToStr5(500)+subnode.col1);if(subnode.col2!="")
commandList.push(intToStr5(502)+subnode.col2);if(subnode.col3!="")
commandList.push(intToStr5(504)+subnode.col3);if(subnode.viewerKind!=0)
commandList.push(intToStr5(503)+subnode.viewerKind);if(subnode.fontDetails!=null)
{for(var c=0;c<subnode.fontDetails.length;c++)
{var fontDetail=subnode.fontDetails[c];var tempStr="";tempStr+=intToStr5(501);tempStr+=intToStr3(fontDetail.colId);if(fontDetail.bold)
tempStr+="1";else
tempStr+="0";if(fontDetail.italic)
tempStr+="1";else
tempStr+="0";var colorValue;if(typeof(fontDetail.color)=="undefined"||fontDetail.color===null)
colorValue=-1;else
colorValue=rgbToBgr(fontDetail.color);tempStr+=intToStr11(colorValue)+intToStr11(fontDetail.size)+fontDetail.fontName;commandList.push(tempStr);}
subnode.fontDetails=null;}
for(var c=0;c<subnode.members.length;c++)
{var node;node=subnode.members[c];internalAddToStringList(node);}
commandList.push(intToStr5(505));};for(var c=0;c<this.members.length;c++)
{var node;node=this.members[c];internalAddToStringList(node);}
this.members=new Array();}});};tracetool();}