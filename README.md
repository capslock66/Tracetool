# Tracetool


<img alt="Viewer" src="https://raw.githubusercontent.com/capslock66/Tracetool/master/Server1.jpg">


# Introduction
The problem when debugging a multi-tier system (with perhaps multiple languages) is that traces are placed in different log files, in different formats, and in different locations on the network.

# What is TraceTool
* A viewer (written in unmanaged code) that displays multiple kinds of sources (from the provided framework, log file, event log, or the OutputDebugString method)
* An easy and powerful client framework to send simple traces, grouped traces, class and object viewer, dump, and call stack.

# Installation

The viewer must be install on your pc (windows only).

Download the [Viewer](https://raw.githubusercontent.com/capslock66/Tracetool/master/GithubFiles/Viewer.zip "Viewer.zip") and unpack the file into a folder of your chose.

Download the [Viewer](GithubFiles/Viewer.zip "Viewer.zip") and unpack the file into a folder of your chose.


If you plan to use the "windows message" mode, you must start once the viewer to self register the location into the registry 

For Dot Net developers, in visual studio install the api using "manage nuget packages" or via the nuget console 
>Install-Package Tracetool.DotNet.Api 

or via visual studio interface (Manage Nuget Packages for solution) and search for "Tracetool.DotNet.Api"

If you target Javascript (browser or server) or Typescript, install from Npm command line : 
>npm install -s tracetool


..... work in progress ....
