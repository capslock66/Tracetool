
// ===============================================
// Tracetool node js samples.
// Don't forget to start the viewer first !!!
// ===============================================

"use strict";

const ttrace = require('tracetool');    // default host is 127.0.0.1:81
ttrace.host = "127.0.0.1:85";
ttrace.options.objectTreeDepth = 5;

// clear all traces on the viewer (main traces)
ttrace.clearAll();

// the more simple way to send traces
ttrace.debug.send  ("Simple debug trace");
ttrace.warning.send("Simple warning trace");
ttrace.error.send  ("Simple error trace");

// 2 columns traces
ttrace.debug.send("trace left","trace right");

// indent / unindent
ttrace.debug.indent("level1 using indent method", undefined, undefined, true);   
ttrace.debug.send("level2");
ttrace.debug.send("level2");
ttrace.debug.send("level2");
ttrace.debug.unIndent("end level1", undefined, undefined, true);    

// subnode 
var Node = ttrace.debug.send("level1 using sub node method");
Node.send("level2 as child from level1");
Node.send("level2 as child from level1");
Node.send("level2 as child from level1");

// 3 levels subnodes 
ttrace.debug
    .send("level1")
       .send("level2")
           .send("level3");

// enter method
//...

// call stack
ttrace.debug.sendCaller ("caller");
ttrace.debug.sendStack("stack");

// color, font size and type
ttrace.debug.send("bold").setFontDetail(-1, /*bold*/ true) ; // colId, bold, italic, color, size, fontName.

var FontDetail = new ttrace.classes.FontDetail() ;
FontDetail.colId = -1 ;
FontDetail.italic = true ;
ttrace.debug.send("Italic").setFontDetail(FontDetail) ; 

//ttrace.debug.send("environment", ttrace.environment);