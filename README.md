# My Emacs Config

1. Emacs taught me freedom for software
2. Emacs taught me how to read code
3. Emacs taught me power of Lisp
4. Emacs taught me how to implement a language core
5. Emacs taught me how to implement a garbage collector
6. Emacs helped me to code and debug
7. Emacs helped me to write and edit text/mails/documents
8. Emacs helped me to be a effective programmer
9. Emacs made me a hacker
10. Emacs has changed my life forever
_-Matz_

## Setup

A lovely set of emacs setting. It depends emacs 24.2 on the Mac OS 10.8.

To grab all the dependencies, either:

    git clone git://github.com/arcthur/.emacs.d.git
    cd .emacs.d
    git submodule init
    git submodule update

## Install emacs on mac

I use Cocoa Emacs, installed like this:

    brew install emacs --cocoa

You can also get the very latest Emacs:

    brew install emacs --cocoa --use-git-head --HEAD

## Yasnippet

I use own yasnippet snippet, update in .emacs.d:

    git submodule foreach git pull origin master

## Thanks

These emacsors helped me. You need learned either:

* [mbriggs](https://github.com/mbriggs/.emacs.d)
* [magnars](https://github.com/magnars/.emacs.d)
* [purcell](https://github.com/purcell/emacs.d)
