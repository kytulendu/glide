/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * Main driver functions
 */

#include <ntddk.h>

#include "debug.h"

/* Get IOCTL interface definitions */
/* from ../../../fxmisc/ */
#include "gpioctl.h"

#include "fxgpio.h"

PDEVICE_OBJECT GpdDeviceObject = NULL;

NTSTATUS
FxAddDevice(
    IN PDRIVER_OBJECT   DriverObject
    )
{
    NTSTATUS            status;
    UNICODE_STRING      GpdntDeviceName;
    UNICODE_STRING      Gpdwin32DeviceName;

    DebugPrint(("FxAddDevice()\n"));

    /* Create NT device object */

    RtlInitUnicodeString(&GpdntDeviceName, GPD_NT_DEVICE_NAME);
    status = IoCreateDevice(
        DriverObject,
        0,
        &GpdntDeviceName,
        GPD_TYPE,
        0,
        FALSE,
        &GpdDeviceObject);
    if (!NT_SUCCESS(status))
    {
        DebugPrint(("IoCreateDevice GPD failed: %x\n", status));
        return status;
    }

    /* Create a symbolic link */

    RtlInitUnicodeString(&Gpdwin32DeviceName, GPD_DOS_DEVICE_NAME);
    status = IoCreateSymbolicLink(&Gpdwin32DeviceName, &GpdntDeviceName);
    if (!NT_SUCCESS(status))
    {
        DebugPrint(("IoCreateSymbolicLink failed: %x\n", status));
        IoDeleteDevice(GpdDeviceObject);
        return status;
    }

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

    DebugPrint(("FxDeviceControl()\n"));

    status = STATUS_NOT_IMPLEMENTED;    /* For setting Irp->IoStatus.Status */
    Irp->IoStatus.Information = 0;

    irpStack = IoGetCurrentIrpStackLocation(Irp);

    ioBuffer           = Irp->AssociatedIrp.SystemBuffer;
    inputBufferLength  = IO_IN_BUFLEN(irpStack);
    outputBufferLength = IO_OUT_BUFLEN(irpStack);

    switch (IO_CTRLCODE(irpStack))
    {
        case IOCTL_GPD_READ_PORT_UCHAR:
        case IOCTL_GPD_READ_PORT_USHORT:
        case IOCTL_GPD_READ_PORT_ULONG:
            DebugPrint(("IOCTL_GPD_READ_PORT\n"));
            status = IoctlReadPort(
                        DeviceObject,
                        Irp,
                        ioBuffer,
                        inputBufferLength,
                        outputBufferLength,
                        IO_CTRLCODE(irpStack));
            break;

        case IOCTL_GPD_WRITE_PORT_UCHAR:
        case IOCTL_GPD_WRITE_PORT_USHORT:
        case IOCTL_GPD_WRITE_PORT_ULONG:
            DebugPrint(("IOCTL_GPD_WRITE_PORT\n"));
            status = IoctlWritePort(
                        DeviceObject,
                        Irp,
                        ioBuffer,
                        inputBufferLength,
                        IO_CTRLCODE(irpStack));
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
