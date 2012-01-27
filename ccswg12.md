
# Critical Code Studies Working Group 2012

## Code Critique

### Overview

**Name of Software**: Bakers12

**Programming Language**: Haskell

**Hardware Requirements**: You'll need hardware. PC.

**Software Requirements**: Git, Haskell, command line. See *Getting the Code*
and *Compiling and Running*. This should work on Mac, Windows, and Linux. (And
I've tested it on all of them.)

**Year of Launch**: 2011

**Author**: [Eric Rochester](mailto:erochest@virginia.edu)

### Installation

#### Getting the Binary

**TODO**

#### Getting the Code

To get the code, either use `git`:

    git clone --branch dev git://github.com/erochest/bakers12.git

Or download it from

* [https://github.com/erochest/bakers12/zipball/dev][zipball]
* [https://github.com/erochest/bakers12/tarball/dev][tarball]

#### Compiling

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

#### Running

Assuming that the you've successfully compiled the program or downloaded it
(**TODO**) and that you've put it somewhere on your path, you can open a
console window and use it like this:

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

## About 

*[Write a few words to begin the discussion of the code, including your gloss
on the code, explaining its functionality and significance. Let the members
know what questions you have about the code and how it fits into larger
questions you are pursuing. Please link to any resources necessary to provide
context or enhance understanding. We would like to make these examples
accessible to the widest range of members possible.]*

[zipball]: https://github.com/erochest/bakers12/zipball/dev "ZIP file"
[tarball]: https://github.com/erochest/bakers12/tarball/dev "tar file"
[platform]: http://hackage.haskell.org/platform/ "Haskell Platform"
[install-paths]: http://www.haskell.org/cabal/users-guide/#paths-in-the-simple-build-system "Installation Paths"

