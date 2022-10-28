@echo off
cd glide2x
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst1 USE_X86=1
rem mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst96 USE_X86=1
mingw32-make -f Makefile.win32 FX_GLIDE_HW=cvg USE_X86=1 USE_MMX=1 USE_3DNOW=1
cd ..
cd glide3x
mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst1 USE_X86=1
rem mingw32-make -f Makefile.win32 FX_GLIDE_HW=sst96 USE_X86=1
mingw32-make -f Makefile.win32 FX_GLIDE_HW=cvg USE_X86=1 USE_MMX=1 USE_3DNOW=1
