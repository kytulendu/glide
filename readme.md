3Dfx Voodoo and Voodoo 2 driver
===============================

This is the source code to 3Dfx Glide for Voodoo graphics accelerators folked from https://github.com/sezero/glide
which is forked from the original CVS repo of Glide open source project at
sourceforge: https://sourceforge.net/projects/glide/

Source is licensed under 3DFX GLIDE Source Code General Public License.

This was mainly for 3Dfx Voodoo and Voodoo 2. I will incorporate changes from and send pull request to upstream
if the changes I made was applicable.

To build this you need Microsoft Visual C++ 6.0, [Netwide Assembler](https://nasm.us/) and [TDM-GCC 5.1.0 (32bit)](https://sourceforge.net/projects/tdm-gcc/files/TDM-GCC%20Installer/),
then add nasm to your path (you will need to manually copy mingw32-make.exe to make.exe in `TDM-GCC\bin`).

Open `MinGW Command Prompt` and run `C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT`
to set MSVC6 path, then change directory to where the repository is, then type `build`.

Windows 98 DDK and Windows 2000/XP/2003 DDK is needed to build fxmemmap and fxvoodoo.

### TODO

* Add system tray setting program and control panel, written using cross platform UI library
(fxtray.exe/fxctrl.exe, fxcpl.cpl)?
* MMX, 3DNow! support for SST1. SSE support for CVG and maybe SST1 (using code from H5 Glide3)
* Update Linux driver for newer kernel
* Port fxvoodoo drivers to x64
