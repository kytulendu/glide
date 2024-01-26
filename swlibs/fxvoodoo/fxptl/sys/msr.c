/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * MSR functions
 */

#include <ntddk.h>

#include "debug.h"

#include "memmap.h"

ULONG ReadMSR(
    IN OUT MSRInfo* msrInfo
    )
{
#if defined(_X86_)
    _asm {
        push eax
        push edx
        push ecx
        xor eax, eax
        xor edx, edx
        mov ecx, dword ptr [msrInfo]
        rdmsr
        mov dword ptr [msrInfo], ecx        /* msrInfo->msrNum */
        mov dword ptr [msrInfo+4], eax      /* msrInfo->msrLo */
        mov dword ptr [msrInfo+8], edx      /* msrInfo->msrHi */
        pop ecx
        pop edx
        pop eax
    }
#else
#if define 0
    unsigned __int64 readmsr_ret = 0;

    readmsr_ret = __readmsr(msrInfo->msrNum);

    msrInfo->msrLo = (ULONG)readmsr_ret;
    msrInfo->msrHi = (ULONG)(readmsr_ret >> 32);
#endif
#endif

    return msrInfo->msrNum;
}

ULONG WriteMSR(
    IN OUT MSRInfo* msrInfo
    )
{
#if defined(_X86_)
    _asm {
        push eax
        push edx
        push ecx
        mov ecx, dword ptr [msrInfo]        /* msrInfo->msrNum */
        mov eax, dword ptr [msrInfo+4]      /* msrInfo->msrLo */
        mov edx, dword ptr [msrInfo+8]      /* msrInfo->msrHi */
        wrmsr
        pop ecx
        pop edx
        pop eax
    }
#else
#if define 0
    __int64 data = 0;

    data = (msrInfo->msrHi << 32) + msrInfo->msrLo;
    __writemsr(msrInfo->msrNum, data);
#endif
#endif

    return msrInfo->msrNum;
}
