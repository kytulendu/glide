3Dfx Voodoo and Voodoo 2 driver
===============================

This is the source code to 3Dfx Glide for Voodoo graphics accelerators,
mainly for 3Dfx Voodoo and Voodoo 2. I will incorporate changes from
and send pull request to upstream if the changes was applicable.

To build this you need MSVC6, nasm and TDM-GCC 5.1.0 (32bit),
then add nasm to your path (you may need to manually copy mingw32-make.exe to make.exe).

Open `MinGW Command Prompt` and run `C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT`
to set MSVC6 path, then change directory to where the repository is, then type `build`.

Windows 98 DDK and Windows 2000/XP/2003 DDK is needed to build fxmemmap and fxvoodoo.

### TODO

* Add system tray setting program and control panel, written using cross platform UI library
(fxtray.exe/fxctrl.exe, fxcpl.cpl)?
* MMX, 3DNow! support for SST1. SSE support for CVG and maybe SST1 (using code from H5 Glide3)
* Update Linux driver for newer kernel
* Port fxvoodoo drivers to x64
