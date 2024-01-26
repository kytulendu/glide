/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * Main driver functions
 */

#include <ntddk.h>

#include "debug.h"

/* Get IOCTL interface definitions */
/* from ../../../fxmisc/ */
#include "fxptl.h"

#include "memmap.h"

PDEVICE_OBJECT MapMemDeviceObject = NULL;

#if defined(_X86_)
/* i386/p6chk.asm */
extern ULONG __stdcall CheckForP6(VOID);

ULONG isP6 = 0;
#endif

NTSTATUS
FxAddDevice(
    IN PDRIVER_OBJECT   DriverObject
    )
{
    NTSTATUS            status;
    UNICODE_STRING      MemMapntDeviceName;
    UNICODE_STRING      MemMapwin32DeviceName;

    DebugPrint(("FxAddDevice()\n"));

    /* Create NT device object */

    RtlInitUnicodeString(&MemMapntDeviceName, MAPMEM_NT_DEVICE_NAME);
    status = IoCreateDevice(
        DriverObject,
        0,
        &MemMapntDeviceName,
        FILE_DEVICE_MAPMEM,
        0,
        TRUE,
        &MapMemDeviceObject);
    if (!NT_SUCCESS(status))
    {
        DebugPrint(("IoCreateDevice MEMMAP failed: %x\n", status));
        return status;
    }

    /* Create a symbolic link */

    RtlInitUnicodeString(&MemMapwin32DeviceName, MAPMEM_DOS_DEVICE_NAME);
    status = IoCreateSymbolicLink(&MemMapwin32DeviceName, &MemMapntDeviceName);
    if (!NT_SUCCESS(status))
    {
        DebugPrint(("IoCreateSymbolicLink failed: %x\n", status));
        IoDeleteDevice(MapMemDeviceObject);
        return status;
    }

#if defined(_X86_)
    /* Check for MSR support. */
    isP6 = CheckForP6();
#endif

    return status;
}

NTSTATUS
FxDeviceControl(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp
    )
{
    PIO_STACK_LOCATION irpStack;
    PVOID              ioBuffer;
    ULONG              inputBufferLength;
    ULONG              outputBufferLength;
    NTSTATUS           status;
    BOOLEAN            is32bit;

    DebugPrint(("FxDeviceControl()\n"));

    status = STATUS_NOT_IMPLEMENTED;    /* For setting Irp->IoStatus.Status */
    Irp->IoStatus.Information = 0;

#if defined(_WIN64)
    /* For 32bit I/O thunking */
    is32bit = IoIs32bitProcess(Irp);
#else
    is32bit = TRUE;
#endif

    irpStack = IoGetCurrentIrpStackLocation(Irp);

    ioBuffer           = Irp->AssociatedIrp.SystemBuffer;
    inputBufferLength  = IO_IN_BUFLEN(irpStack);
    outputBufferLength = IO_OUT_BUFLEN(irpStack);

    switch (IO_CTRLCODE(irpStack))
    {
        case IOCTL_MAPMEM_MAP_USER_PHYSICAL_MEMORY:
            DebugPrint(("IOCTL_MAPMEM_MAP_USER_PHYSICAL_MEMORY\n"));
            status = IoctlMapMemory(
                        DeviceObject,
                        is32bit,
                        ioBuffer,
                        inputBufferLength,
                        outputBufferLength);
            if (!NT_SUCCESS(status))
            {
                status = STATUS_INVALID_PARAMETER;
                DebugPrint(("Memory map failed\n"));
                break;
            }

#if defined(_WIN64)
            if (is32bit)
            {
                Irp->IoStatus.Information = sizeof(PVOID32);
                DebugPrint(("Memory successfully mapped (32bit)\n"));
            }
            else
#endif
            {
                Irp->IoStatus.Information = sizeof(PVOID);
                DebugPrint(("Memory successfully mapped\n"));
            }
            break;

        case IOCTL_MAPMEM_UNMAP_USER_PHYSICAL_MEMORY:
            DebugPrint(("IOCTL_MAPMEM_UNMAP_USER_PHYSICAL_MEMORY\n"));
#if defined(_WIN64)
            if (is32bit)
            {
                if (inputBufferLength >= sizeof(PVOID32))
                {
                    status = ZwUnmapViewOfSection(
                                (HANDLE) -1,
                                *((PVOID32 *) ioBuffer));
                    DebugPrint(("Memory successfully unmapped (32bit)\n"));
                }
            }
            else
#endif
                if (inputBufferLength >= sizeof(PVOID))
            {
                status = ZwUnmapViewOfSection(
                            (HANDLE) -1,
                            *((PVOID *) ioBuffer));
                DebugPrint(("Memory successfully unmapped\n"));
            }
            else
            {
                status = STATUS_INSUFFICIENT_RESOURCES;
                DebugPrint(("ZwUnmapViewOfSection failed\n"));
            }
            break;

        case IOCTL_MAPMEM_GET_MSR:
            DebugPrint(("IOCTL_MAPMEM_GET_MSR\n"));
#if defined(_X86_)
            if (inputBufferLength != sizeof(MSRInfo) ||
                outputBufferLength != sizeof(MSRInfo))
            {
                status = STATUS_INVALID_PARAMETER;
                DebugPrint(("inputBufferLength or outputBufferLength != sizeof(MSRInfo)\n"));
                break;
            }
            if (!isP6)
            {
                status = STATUS_NOT_IMPLEMENTED;
                DebugPrint(("Not P6\n"));
                break;
            }

            if (ReadMSR(ioBuffer))
            {
                status = STATUS_SUCCESS;
                Irp->IoStatus.Information = sizeof(MSRInfo);
                DebugPrint(("MSR successfully readed\n"));
            }
            else
            {
                status = STATUS_INVALID_PARAMETER;
                DebugPrint(("Failed to read MSR\n"));
            }
#else
            status = STATUS_NOT_IMPLEMENTED;
            DebugPrint(("MSR not implemented\n"));
#endif
            break;

        case IOCTL_MAPMEM_SET_MSR:
            DebugPrint(("IOCTL_MAPMEM_SET_MSR\n"));
#if defined(_X86_)
            if (inputBufferLength != sizeof(MSRInfo) ||
                outputBufferLength != sizeof(MSRInfo))
            {
                status = STATUS_INVALID_PARAMETER;
                DebugPrint(("inputBufferLength or outputBufferLength != sizeof(MSRInfo)\n"));
                break;
            }
            if (!isP6)
            {
                status = STATUS_NOT_IMPLEMENTED;
                DebugPrint(("Not P6\n"));
                break;
            }

            if (WriteMSR(ioBuffer))
            {
                status = STATUS_SUCCESS;
                Irp->IoStatus.Information = sizeof(MSRInfo);
                DebugPrint(("MSR successfully written\n"));
            }
            else
            {
                status = STATUS_INVALID_PARAMETER;
                DebugPrint(("Failed to write MSR\n"));
            }
#else
            status = STATUS_NOT_IMPLEMENTED;
            DebugPrint(("MSR not implemented\n"));
#endif
            break;

        default:
            DebugPrint(("Unknown IRP_MJ_DEVICE_CONTROL\n"));
            status = STATUS_INVALID_PARAMETER;
            break;
    }

    Irp->IoStatus.Status = status;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return status;
}

NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT   DriverObject,
    IN PUNICODE_STRING  RegistryPath
    )
{
    NTSTATUS status = STATUS_SUCCESS;

    UNREFERENCED_PARAMETER(RegistryPath);

    DebugPrint(("DriverEntry()\n"));

    status = FxAddDevice(DriverObject);
    DriverObject->MajorFunction[IRP_MJ_CREATE]          = FxCreate;
    DriverObject->MajorFunction[IRP_MJ_CLOSE]           = FxClose;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL]  = FxDeviceControl;
    DriverObject->DriverUnload                          = FxCleanup;

    return status;
}
