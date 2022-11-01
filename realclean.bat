@echo off
cd glide2x
make -f Makefile.win32 FX_GLIDE_HW=cvg realclean
make -f Makefile.win32 FX_GLIDE_HW=sst1 realclean
make -f Makefile.win32 FX_GLIDE_HW=sst96 realclean
cd ..
cd glide3x
make -f Makefile.win32 FX_GLIDE_HW=cvg realclean
make -f Makefile.win32 FX_GLIDE_HW=sst1 realclean
make -f Makefile.win32 FX_GLIDE_HW=sst96 realclean
cd ..
