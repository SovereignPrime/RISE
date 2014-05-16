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

In a post Edward Snowdon world, no major web service provider can guarantee the 
privacy of their users.

There could be no better time to consider what is at the root of any free and 
open society.

With this in mind, please read the following articles from the 
Universal Declaration Of Human Rights.

Build Status
------------

Currently, we are running [build-bot](http://travis-ci.org)

[![Build Status](https://travis-ci.org/SovereignPrime/RISE.svg?branch=master)](https://travis-ci.org/SovereignPrime/RISE)

Requirements
-------------

To build and run the software you need:

* Erlang/OTP R16B1-1 or greater version
* GCC, xcodebuild, MinGW or compatiable
* gnumake
* Python 2.7 (NOT 3.x at the moment)
* GTK3 with GTKWebKit (for Linux and Windows)
* Developer Tools for MacOSX

Building
--------

First install all dependences.

### Linux (Ubunto)

<code>
make deb
</code>

### Mac

<code>
make mac
</code>

### Windows

<code>
make win
</code>

Project file structure
----------------------

After compilation compiled project and package for target OS will be situated under rel/Release folder.
Durin runtime all configs, DB files and logs are situated in
* $HOME/Library/RISE for MacOSX
* $HOME/.config/RISE for Linux
* $HOME/Application Data/RISE for Windows

License
-------

This code is licensed under GPL v2.
See LICENSE for details.
