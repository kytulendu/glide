@echo off
cd glide2x
mingw32-make -f Makefile.win32 FX_GLIDE_HW=cvg realclean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst1 realclean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst96 realclean
cd ..
cd glide3x
mingw32-make -f Makefile.win32 FX_GLIDE_HW=cvg realclean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst1 realclean
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst96 realclean
