/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * Main driver functions
 */

#if !defined(__MEMMAP_H__)
#define __MEMMAP_H__

#define IO_CTRLCODE(p)      (p)->Parameters.DeviceIoControl.IoControlCode
#define IO_IN_BUFLEN(p)     (p)->Parameters.DeviceIoControl.InputBufferLength;
#define IO_OUT_BUFLEN(p)    (p)->Parameters.DeviceIoControl.OutputBufferLength;

/* NT device name */
#define MAPMEM_NT_DEVICE_NAME       L"\\Device\\MapMem"
#define PHYSICAL_MEMORY_DEVICE      L"\\Device\\PhysicalMemory"

/* DOS device name */
#define MAPMEM_DOS_DEVICE_NAME      L"\\DosDevices\\MAPMEM"

/* from ../../../newpci/pcilib/pcilib.h */
typedef struct
{
    ULONG msrNum;                   /* Which MSR? */
    ULONG msrLo;                    /* MSR Values */
    ULONG msrHi;
} MSRInfo;

#if defined(_WIN64)

/* 32bit thunking structure
 * from ../../fxmisc/fxptl.h */
typedef struct
{
    INTERFACE_TYPE   InterfaceType; /* Isa, Eisa, etc.... */
    ULONG            BusNumber;     /* Bus number */
    PHYSICAL_ADDRESS BusAddress;    /* Bus-relative address */
    ULONG            AddressSpace;  /* 0 is memory, 1 is I/O */
    ULONG            Length;        /* Length of section to map */
} PHYSICAL_MEMORY_INFO32, *PPHYSICAL_MEMORY_INFO32;
#endif

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
IoctlMapMemory(
    IN PDEVICE_OBJECT   DeviceObject,
    IN BOOLEAN          Is32bit,
    IN OUT PVOID        IoBuffer,
    IN ULONG            InputBufferLength,
    IN ULONG            OutputBufferLength
    );

/* From msr.c */

ULONG ReadMSR(
    IN OUT MSRInfo* msrInfo
    );

ULONG WriteMSR(
    IN OUT MSRInfo* msrInfo
    );

#endif /* __MEMMAP_H__ */
