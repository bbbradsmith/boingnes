# Boing Ball NES

A recreation of the Amiga Boing! demo for NES, and also the Atari ST BOINK.

Download ROM: [Releases](https://github.com/bbbradsmith/boingnes/releases)

Support the author: [Patreon](https://www.patreon.com/rainwarrior)

[My Website](https://rainwarrior.ca)

## Controls

* **Left** - decrease spin rate
* **Right** - increase spin rate
* **Up** - increase bounce height
* **Down** - decrease bounce height
* **Select** - toggle Atari ST BOINK / Amiga Boing! mode
* **Start** - credits
* **B** - TV pixel aspect ratio
* **A** - square pixel aspect ratio

## How to Build

Download [**cc65**](https://cc65.github.io/) (Windows Snapshot) and place it in a
**cc65** folder in this directory.

Run **build.bat** to build the ROM to **temp/boing.nes**.

This program uses a reduced subset of the cc65 C runtime library.
This is pre-built, but if you wish to rebuild it, download the **libsrc** folder
from the [**cc65 source code**](https://github.com/cc65/cc65)
and place it in this directory, and run **build_runtime.bat**.
This will rebuild **temp/runtime.lib**.
(See batch file for more information.)

The graphics data can be rebuilt with **build_gfx.py**.
This converts several PNG images in the **gfx** folder into data for the build,
placed in the **temp** folder.
This script requires [**Python 3**](https://www.python.org/) with
[**PIL**](https://pillow.readthedocs.io).

These tools can also be run on other operating systems, but you would have to
duplicate the batch scripts in a format suitable for your OS.

## NES Development Notes

This project was partly intended as a simple C example for NES development,
and something I might use as a starting point for later projects.
It's sort of a continuation of my earlier
[NESert Golfing](https://github.com/bbbradsmith/NESertGolfing) project,
but trying to provide for more "normal" NES needs.

It contains an assembly framework for normal NES operations which can be
accessed from C. You can see the contents of this library declared at the top
of **boing.c**. The assembly implementation appears in **boing.s**.

For the most part, all code related to the Boing demo was in C, contained in
**boing.c** and the assembly file **boing.s** is almost entirely just the
NES library code, except for a little bit of data layout stuff.

Basic NES library functions:
* NES ROM and header generation
* Tools for generating nametable and sprite data (**build_gfx.py**)
* Rendering control and PPU data uploading
* Sprite drawing
* Simple sound effect engine
* Gamepad input

The C runtime library provided is very minimal. It contains only what is needed
to support cc65's generated C code. The C standard library is not included,
so including most C library headers (e.g. *string.h*) will probably not be
useful here.

The sprite generator looks for redundant horizontal and vertical flipped tiles
and tries to reuse them. For this demo it also has an additional redundancy check
that swaps the red-white ball tiles for white-red, but for a more generic use this
should be removed. Look for "palette shifted" in the comments of **build_gfx.py**
to find the adjustment.

This project used a [CNROM](https://wiki.nesdev.org/w/index.php/CNROM)
mapper configuration, just so it could fit the ball graphics in two aspect ratios,
 but **boing.cfg** can be modified for other mappers.
In particular the **CHR** memory regions and **TILES** segments,
and possibly the **DPCM** segment could be removed or relocated if you
need more space for sound samples. The NES header will also need a small edit,
found at the bottom of **boing.s**.

There is a test of NES library features that weren't otherwise used in **boing.nes**
accessed by pressing **Select + Start**.

## License

This source code is provided under the Creative Commons Attribution license. (CC BY 4.0)

Full details of this license are available here:
https://creativecommons.org/licenses/by/4.0/

This approximately means that you are free to reuse this source code for your own purposes,
provided that you include an attribution to me (Brad Smith) in documentation and
accessible credits for the work it is used in.
