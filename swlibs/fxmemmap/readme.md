3Dfx Voodoo Board Mapping Virtual Device
========================================

The primary function of this Windows 95 Dynamically Loaded VxD
is to exposes the VMM service _MapPhysToLinear. The DDK doc says that
"because physical addresses do not move, the linear address returned by
this service remains valid until the system is shut down."

The doc also says that MapPhysToLinear is "intended to be used to
examine device-specific memory. Virtual devices must not use this service
for any other purpose." OK?

By default, a DLL that uses this VxD will expect to find it in the
C:\Windows\System directory.

The VxD can be build using Windows 95 or Windows 98 DDK, by set up
the environment and type `nmake` at fxmemmap directory.
