% Critical Code Studies Working Group 2012
% Eric Rochester, Scholar's Lab, University of Virginia
%

# Code Critique

## Overview

Name of Software

:   Bakers12

Programming Language

:   Haskell

Hardware Requirements

:   You'll need hardware. PC.

Software Requirements

:   Git, Haskell, command line. See [*Getting the Code*](#getting-the-code),
    [*Compiling*](#compiling), and [*Running*](#running). This should work on
    Mac, Windows, and Linux.

Year of Launch

:   2011

Author

:   [Eric Rochester](mailto:erochest@virginia.edu)

## About 

Baker's 12 started out as a 13-day (one day per week) bootcamp-style project.
The purpose was to learn Haskell and experiment with NLP. Each day was a sprint
in which I would attempt to add a meaningful feature and finish the day with a
working program.  Since then, I've given up on the intense workflow, but I'm
continuing to develop the project in a more relaxed manner. Recently, I've
started over. The original code is still in the `master` branch; the new code
that we're looking at is in the `dev` branch, in the file
[`lib/Text/Bakers12/Tokenizer.hs`](https://github.com/erochest/bakers12/blob/dev/lib/Text/Bakers12/Tokenizer.hs).

This chunk of code is the module to tokenize or lex text. It takes streams of
texts and produces streams of tokens. The underlying library that it uses
([enumerator][enumerator]) allows it to stream a lot of data with constant
memory usage. Because of the issues that I'm getting ready to outline, I've
pushed most of my discussion of the code's functionality into the comments.

I'm interested in how the abstractions that make code powerful and useful also
push against and prevent readers---all readers, but especially non-expert
readers---from interpreting these texts. With that in mind, I've selected this
section of code not only because it's an important, central part of this
program, but also because it's fairly dense. It contains abstractions that are
probably unfamiliar to most programmers (functional programming and monads),
and it uses a complicated library (enumerators). Both monads and enumerators
rely on a lot of hidden code, and as such they resist easy interpretation by
even many sophisticated readers.

To help make this more approachable, I've added a lot of comments---many more
than I normally would. But even here, looking at the
[docco](http://jashkenas.github.com/docco/)-generated documentation, it's
debatable whether those comments help or hurt readability.

I'm interested here in questions about how to make the code more approachable.
I'm undecided whether this is possible or, from the developer's point of view,
desirable. The interplay between powerful abstractions and accessibility,
between tackling complex tasks and communication, seem to create an inherent
tension in the text. Is there anyway to make this code more legible, either
through the code itself, through comments, or through external tools? Is this
legibility even desirable?

### Links

* <http://www.ericrochester.com/bakers12/docs/Text/Bakers12/Tokenizer.html>
* <https://github.com/erochest/bakers12/blob/dev/lib/Text/Bakers12/Tokenizer.hs>
* <http://www.ericrochester.com/bakers12/ccswg12.html>

### Deeper Links

If you want more background on some of the concepts involved, I've found these
links to be helpful:

* [Haskell Wikibook, Understanding Monads](http://en.wikibooks.org/wiki/Haskell/Monads)
* [A tutorial on the enumerator library](http://www.mew.org/~kazu/proj/enumerator/)
* [Yesod Book: Enumerator Package](http://www.yesodweb.com/book/enumerator)
* [Trying to work out iteratees](http://therning.org/magnus/archives/735)

## Installation

### Getting the Binary

If requested, I'll dig up some pre-compiled binaries for your platform. Email me.

### Getting the Code

To get the code, either use `git`:

    git clone --branch dev git://github.com/erochest/bakers12.git

Or download it from

* <https://github.com/erochest/bakers12/zipball/dev>
* <https://github.com/erochest/bakers12/tarball/dev>

### Compiling

If you don't already have Haskell installed, the easiest way to get running is
to download and install the [Haskell Platform][platform].

Now, open up a command line (Terminal, console, or whatever your platform calls
it). Obviously, you'll need to change the first line to change into whatever
directory you clones or unpacked the source into.

    cd bakers12
    cabal update
    cabal install

That's it. It will download any dependencies it needs and compile the
executable. (You can ignore any compiler warnings.) It will also install it
into a place of its own choosing. This varies by platform, and can be a little
hard to find (see [this chart][install-paths], which actually doesn't cover the
Mac locations).

Fortunately, you can also get to the executable in the source directory, inside
`dist/build/bakers12`. That file should contain everything, so you can just
copy it wherever is convenient.

### Running

Assuming that the you've successfully [compiled the program](#compiling) or
[downloaded it](#getting-the-binary) and that you've put it somewhere on your
path, you can open a console window and use it like this:

    $ bakers12 -\?
    bakers12 v0.0.0, (c) Eric Rochester 2011, 2012

    bakers12 [OPTIONS] [FILES/DIRS]

    Common flags:
      -f --filter=TOKEN FILTER   The filter to use on the output tokens. This can
                                 be one of 'null', 'minimal', 'penn'. The default
                                 is 'minimal'.
      -F --format=OUTPUT FORMAT  The output format to use. This can be either
                                 'csv' or 'json'. If there is no input, currently
                                 the JSON formatter outputs nothing. This should
                                 probably be an empty list.
      -? --help                  Display help message
      -V --version               Print version information

    This takes one or files and tokenizes them.

    $ bakers12 -f penn README.md | head
    "#","#",1,PunctuationToken,README.md,1
    baker,Baker,5,AlphaToken,README.md,3
    "'s","'s",2,PunctuationToken,README.md,8
    dozen,Dozen,5,AlphaToken,README.md,11
    this,This,4,AlphaToken,README.md,18
    is,is,2,AlphaToken,README.md,23
    a,a,1,AlphaToken,README.md,26
    boot,boot,4,AlphaToken,README.md,28
    "-","-",1,PunctuationToken,README.md,32

    $ bakers12 -f penn -F json README.md  | aeson-pretty | head -25
    [
        {
            "offset": 1,
            "source": "README.md",
            "text": "#",
            "raw": "#",
            "length": 1,
            "type": "PunctuationToken"
        },
        {
            "offset": 3,
            "source": "README.md",
            "text": "baker",
            "raw": "Baker",
            "length": 5,
            "type": "AlphaToken"
        },
        {
            "offset": 8,
            "source": "README.md",
            "text": "'s",
            "raw": "'s",
            "length": 2,
            "type": "PunctuationToken"
        },

(`aeson-pretty` is a utility for pretty-printing JSON.)

Originally, this had more features. But I decided that I wanted to go in a
slightly different direction, so I razed it to the ground and started over. The
original version is on the `master` branch, however, so you can check that you
if you want.

[enumerator]: http://hackage.haskell.org/package/enumerator "enumerator"
[platform]: http://hackage.haskell.org/platform/ "Haskell Platform"
[install-paths]: http://www.haskell.org/cabal/users-guide/#paths-in-the-simple-build-system "Installation Paths"


