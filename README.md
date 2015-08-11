RISE
====

RISE is an open source sharing and collaboration suite that works over a 
distributed network. All information transmitted over this network 
(including the meta data) remains private, and anonymous.

RISE offers.
* group messaging
* task/ project management
* relationship management
* financial accounting
* unlimited file sharing.

Why RISE?
---------

In a post Edward Snowden world, no major web service provider can guarantee the 
privacy of their users.

There could be no better time to consider what is at the root of any free and 
open society.

With this in mind, please read the following articles from the 
Universal Declaration Of Human Rights.

Article 12.

No one shall be subjected to arbitrary interference with his privacy, family, home or correspondence, nor to attacks upon his honour and reputation. Everyone has the right to the protection of the law against such interference or attacks.

Article 19.

Everyone has the right to freedom of opinion and expression; this right includes freedom to hold opinions without interference and to seek, receive and impart information and ideas through any media and regardless of frontiers.

Build Status
------------

Currently, we are running [build-bot](http://travis-ci.org)

[![Build Status](https://travis-ci.org/SovereignPrime/RISE.svg?branch=master)](https://travis-ci.org/SovereignPrime/RISE)

Requirements
-------------

To build and run the software you need:

* Erlang/OTP R16B1-1 or greater version
* GCC
* gnumake
* Git
* Qt


### For linux
* Qt build tools
* deblelper

### For Windows
* Qt
* MinGW
* MsysGit
* Inno Setup

### For MacOSX
* XCode

Building
--------

### Linux (Ubuntu)
* Install GCC and GNUMake
* Install git
* Install Qt build tools
* Install Erlang
* Install DebHelper to build deb

<code>
make deb
</code>

### Mac
* Install XCode
* Install git

<code>
make mac
</code>

### Windows
* Install  Qt
* Install MsysGit
* Install Erlang
* Don't forget to add Qt binaries to PATH (prepend)
* Install Inno Setup

<code>
make win
</code>

Project file structure
----------------------

After compilation compiled project and package for target OS will be situated under rel/Release folder.
During runtime all configs, DB files and logs are situated in
* $HOME/Library/RISE for MacOSX
* $HOME/.config/RISE for Linux
* Windows stores all data in program directory at the moment

The main application folder structure is inherited from [NitrogenProject](http://nitrogenproject.com).
* Most of Erlang dependences are situated in "lib" subdirectory.
* The "site" contains web part.
* In "bin" all Qt stuff and some shell helpers are situated.
* "etc" is for template configs before moving to OS specific place.
* All the rest seems to be standard for an erlang release.

External Libraries and Frameworks
--------------------------------

RISE uses
 
[NitrogenProject](http://nitrogenproject.com) as a WebFramework and inherits most of directory 
structure from it.

For BitTorrent handling [Etorrent](https://github.com/jlouis/etorrent_core).

Frontend part is based on Qt for Windows and Linux.
MacOSX frontend is native.

See [THANKS](THANKS.markdown) for details.

License
-------

This code is licensed under GPL v2.
See [LICENSE](LICENSE) for details.
