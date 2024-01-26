/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * IRP_MJ_CREATE, IRP_MJ_CLEANUP and IRP_MJ_CLOSE operations
 */

#include <ntddk.h>

#include "debug.h"

#include "memmap.h"

/* defined in memmap.c */
extern PDEVICE_OBJECT MapMemDeviceObject;

NTSTATUS
FxCreate(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp
    )
{
    UNREFERENCED_PARAMETER(DeviceObject);
    DebugPrint(("IRP_MJ_CREATE\n"));

    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    return STATUS_SUCCESS;
}

NTSTATUS
FxClose(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp
    )
{
    UNREFERENCED_PARAMETER(DeviceObject);
    DebugPrint(("IRP_MJ_CLOSE\n"));

    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    return STATUS_SUCCESS;
}

VOID
FxCleanup(
    IN PDRIVER_OBJECT   DriverObject
    )
{
    UNICODE_STRING MapMemWin32DeviceName;

    DebugPrint(("FxCleanup()\n"));

    /* Delete the symbolic links */

    RtlInitUnicodeString(&MapMemWin32DeviceName, MAPMEM_DOS_DEVICE_NAME);
    IoDeleteSymbolicLink(&MapMemWin32DeviceName);

    /* Delete the device objects */

    IoDeleteDevice(MapMemDeviceObject);
}
