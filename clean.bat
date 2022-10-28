@echo off
cd glide2x
mingw32-make -f Makefile.win32 FX_GLIDE_HW=cvg clean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst1 clean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst96 clean
cd ..
cd glide3x
mingw32-make -f Makefile.win32 FX_GLIDE_HW=cvg clean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst1 clean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst96 clean
