# Tracetool


<img alt="Viewer" src="https://raw.githubusercontent.com/capslock66/Tracetool/master/Server1.jpg">


# Introduction
The problem when debugging a multi-tier system (with perhaps multiple languages) is that traces are placed in different log files, in different formats, and in different locations on the network.

# What is TraceTool
* A viewer (written in unmanaged code) that displays multiple kinds of sources (from the provided framework, log file, event log, or the OutputDebugString method)
* An easy and powerful client framework to send simple traces, grouped traces, class and object viewer, dump, and call stack.

# Installation

The viewer must be install on your pc (windows only).

First download the nuget in a **temp** folder using this command :
> cd c:\temp 

> nuget install Tracetool.DotNet.Api


inside your temps folder, open the tools folder and copy content to 

..... work in progress ....
..... TO continue .....


For Dot Net developers, start visual studio install from nuget console: 
>Install-Package Tracetool.DotNet.Api 

or via visual studio interface (Manage Nuget Packages for solution) and search for "Tracetool.DotNet.Api"

If you target Javascript (browser or server) or Typescript, install from Npm command line : npm install -s tracetool

You can download viewer (exe) and libraries sources (you need to compile) from github (always updated) or codeproject.

Both include the corresponding api and the viewer. 
See the [codeproject](http://www.codeproject.com/Articles/5498/TraceTool-The-Swiss-Army-Knife-of-Trace) site for all possibilities

Everything is opensource

If you target Dot Net, install from nuget console: Install-Package Tracetool.DotNet.Api or via visual studio interface (Manage Nuget Packages for solution) and search for "Tracetool.DotNet.Api"

If you target Javascript (browser or server) or Typescript, install from Npm command line : npm install -s tracetool
