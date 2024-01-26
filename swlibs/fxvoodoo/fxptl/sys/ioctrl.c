/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * I/O Control functions
 */

#include <ntddk.h>

#include "debug.h"

/* Get IOCTL interface definitions */
/* from ../../../fxmisc/ */
#include "fxptl.h"
#include "gpioctl.h"

#include "memmap.h"

NTSTATUS
IoctlMapMemory(
    IN PDEVICE_OBJECT   DeviceObject,
    IN BOOLEAN          Is32bit,
    IN OUT PVOID        IoBuffer,
    IN ULONG            InputBufferLength,
    IN ULONG            OutputBufferLength
    )
{
    PPHYSICAL_MEMORY_INFO   pmi = NULL;
#if defined(_WIN64)
    PPHYSICAL_MEMORY_INFO32 pmi32;
#endif
    INTERFACE_TYPE      interfaceType;
    ULONG               busNumber;
    PHYSICAL_ADDRESS    physicalAddress;
#if defined(_WIN64)
    SIZE_T              length;
#else
    ULONG               length;
#endif
    UNICODE_STRING      physicalMemoryUnicodeString;
    OBJECT_ATTRIBUTES   objectAttributes;
    HANDLE              physicalMemoryHandle  = NULL;
    PVOID               PhysicalMemorySection = NULL;
    ULONG               inIoSpace, inIoSpace2;
    NTSTATUS            status;
    PHYSICAL_ADDRESS    physicalAddressBase;
    PHYSICAL_ADDRESS    physicalAddressEnd;
    PHYSICAL_ADDRESS    viewBase;
    PHYSICAL_ADDRESS    mappedLength;
    BOOLEAN             translateBaseAddress;
    BOOLEAN             translateEndAddress;
    PVOID               virtualAddress = NULL;

    DebugPrint(("MapTheMemory()\n"));

    /* 32-Bit I/O thunking */
#if defined(_WIN64)
    if (Is32bit) {
        if ((InputBufferLength < sizeof(PHYSICAL_MEMORY_INFO32)) ||
            (OutputBufferLength < sizeof(PVOID32)))
        {
            DebugPrint(("Input or output buffer size is too small (32bit)\n"));
            return STATUS_INSUFFICIENT_RESOURCES;
        }

        pmi32 = (PPHYSICAL_MEMORY_INFO32) IoBuffer;
        pmi->InterfaceType = pmi32->InterfaceType;
        pmi->BusNumber     = pmi32->BusNumber;
        pmi->BusAddress    = pmi32->BusAddress;
        pmi->AddressSpace  = pmi32->AddressSpace;
        pmi->Length        = pmi32->Length;
    }
    else
#endif
    {
        if ((InputBufferLength < sizeof(PHYSICAL_MEMORY_INFO)) ||
            (OutputBufferLength < sizeof(PVOID)))
        {
            DebugPrint(("Input or output buffer size is too small\n"));
            return STATUS_INSUFFICIENT_RESOURCES;
        }

        pmi = (PPHYSICAL_MEMORY_INFO) IoBuffer;
    }

    interfaceType          = pmi->InterfaceType;
    busNumber              = pmi->BusNumber;
    physicalAddress        = pmi->BusAddress;
    inIoSpace = inIoSpace2 = pmi->AddressSpace;
    length                 = pmi->Length;

    /* Get a pointer to physical memory */

    RtlInitUnicodeString(&physicalMemoryUnicodeString, PHYSICAL_MEMORY_DEVICE);
    InitializeObjectAttributes(
        &objectAttributes,
        &physicalMemoryUnicodeString,
        OBJ_CASE_INSENSITIVE,
        (HANDLE) NULL,
        (PSECURITY_DESCRIPTOR) NULL);
    status = ZwOpenSection(
        &physicalMemoryHandle,
        SECTION_ALL_ACCESS,
        &objectAttributes);
    if (!NT_SUCCESS(status))
    {
        DebugPrint(("ZwOpenSection failed: %x\n", status));
        return status;
    }

    status = ObReferenceObjectByHandle(
                physicalMemoryHandle,
                SECTION_ALL_ACCESS,
                (POBJECT_TYPE) NULL,
                KernelMode,
                &PhysicalMemorySection,
                (POBJECT_HANDLE_INFORMATION) NULL);
    if (!NT_SUCCESS(status))
    {
        DebugPrint(("ObReferenceObjectByHandle failed: %x\n", status));
        ZwClose(physicalMemoryHandle);
        return status;
    }

    /* Initialize the physical addresses that will be translated */
    physicalAddressEnd = RtlLargeIntegerAdd(
                            physicalAddress,
                            RtlConvertUlongToLargeInteger((ULONG) length));

    /* Translate the physical addresses */

    translateBaseAddress = HalTranslateBusAddress(
                            interfaceType,
                            busNumber,
                            physicalAddress,
                            &inIoSpace,
                            &physicalAddressBase);
    translateEndAddress = HalTranslateBusAddress(
                            interfaceType,
                            busNumber,
                            physicalAddressEnd,
                            &inIoSpace2,
                            &physicalAddressEnd);

    if (!translateBaseAddress || !translateEndAddress)
    {
        DebugPrint(("HalTranslatePhysicalAddress failed\n"));
        ZwClose(physicalMemoryHandle);
        status = STATUS_UNSUCCESSFUL;
        return status;
    }

    /* Calculate the length of the memory to be mapped */
    mappedLength = RtlLargeIntegerSubtract(
                    physicalAddressEnd,
                    physicalAddressBase);
    if (mappedLength.LowPart == 0)
    {
        DebugPrint(("MappedLength.LowPart == 0\n"));
        ZwClose(physicalMemoryHandle);
        status = STATUS_UNSUCCESSFUL;
        return status;
    }

    length = mappedLength.LowPart;

    /* If the address is in IO space, just return the address,
     * else go through the mapping mechanism */
    if (inIoSpace)
    {
#if defined(_WIN64)
        if (Is32bit)
        {
            *((PVOID32 *) IoBuffer) = (PVOID32) physicalAddressBase.LowPart;
        }
        else
#endif
        {
            *((PVOID *) IoBuffer) = (PVOID) physicalAddressBase.LowPart;
        }
    }
    else
    {
        /* Initialize view base that will receive the physical mapped
         * address after the MapViewOfSection call. */
        viewBase = physicalAddressBase;

        /* Map the section */
        status = ZwMapViewOfSection(
                    physicalMemoryHandle,
                    (HANDLE) -1,
                    &virtualAddress,
                    0L,
                    length,
                    &viewBase,
                    &length,
                    ViewShare,
                    0,
                    PAGE_READWRITE | PAGE_NOCACHE);
        if (!NT_SUCCESS(status))
        {
            DebugPrint(("ZwMapViewOfSection failed: %x\n", status));
            ZwClose(physicalMemoryHandle);
            return status;
        }

        /* Mapping the section rounded the physical address down to
         * the nearest 64 K boundary. Return a virtual address in where
         * we want by add the offset from the beginning of the section. */
#if defined(_WIN64)
        (SIZE_T) virtualAddress += (ULONG) physicalAddressBase.LowPart -
                                  (ULONG) viewBase.LowPart;
#else
        (ULONG) virtualAddress += (ULONG) physicalAddressBase.LowPart -
                                  (ULONG) viewBase.LowPart;
#endif

        /* Return the mapped address */
#if defined(_WIN64)
        if (Is32bit)
        {
            *((PVOID32 *) IoBuffer) = (PVOID32) virtualAddress;
        }
        else
#endif
        {
            *((PVOID *) IoBuffer) = virtualAddress;
        }
    }

    ZwClose(physicalMemoryHandle);
    status = STATUS_SUCCESS;
    return status;
}
