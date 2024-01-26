/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * Main driver functions
 */

#if !defined(__FXGPIO_H__)
#define __FXGPIO_H__

#define IO_CTRLCODE(p)      (p)->Parameters.DeviceIoControl.IoControlCode
#define IO_IN_BUFLEN(p)     (p)->Parameters.DeviceIoControl.InputBufferLength;
#define IO_OUT_BUFLEN(p)    (p)->Parameters.DeviceIoControl.OutputBufferLength;

/* NT device name */
#define GPD_NT_DEVICE_NAME          L"\\Device\\Gpd0"

/* DOS device name */
#define GPD_DOS_DEVICE_NAME         L"\\DosDevices\\GpdDev"

/********************* function prototypes ***********************************/

NTSTATUS
FxAddDevice(
    IN PDRIVER_OBJECT   DriverObject
    );

NTSTATUS
FxDeviceControl(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT   DriverObject,
    IN PUNICODE_STRING  RegistryPath
    );

/* From createclose.c */

NTSTATUS
FxCreate(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP Irp
    );

VOID
FxCleanup(
    IN PDRIVER_OBJECT   DriverObject
    );

NTSTATUS
FxClose(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP Irp
    );

/* From ioctrl.c */

NTSTATUS
IoctlReadPort(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN OUT PVOID        IoBuffer,
    IN ULONG            InputBufferLength,
    IN ULONG            OutputBufferLength,
    IN ULONG            IoctlCode
    );

NTSTATUS
IoctlWritePort(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN OUT PVOID        IoBuffer,
    IN ULONG            InputBufferLength,
    IN ULONG            IoctlCode
    );

#endif /* __FXGPIO_H__ */
