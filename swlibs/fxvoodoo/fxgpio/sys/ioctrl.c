/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * I/O Control functions
 */

#include <ntddk.h>

#include "debug.h"

/* Get IOCTL interface definitions */
/* from ../../../fxmisc/ */
#include "gpioctl.h"

#include "fxgpio.h"

ULONG
TranslatePortAddress(
    IN OUT ULONG        Port
    )
{
    PHYSICAL_ADDRESS    portAddress;
    PHYSICAL_ADDRESS    mappedAddress;
    ULONG               addressSpace;

    portAddress.LowPart  = Port;
    portAddress.HighPart = 0;

    /* Translate the IO port address into system mapped address */
    addressSpace = 1;               /* Located in IO space */
    HalTranslateBusAddress(
        Isa,
        0,
        portAddress,
        &addressSpace,
        &mappedAddress);

    Port = mappedAddress.LowPart;

    return addressSpace;
}

NTSTATUS
IoctlReadPort(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN OUT PVOID        IoBuffer,
    IN ULONG            InputBufferLength,
    IN ULONG            OutputBufferLength,
    IN ULONG            IoctlCode
    )
{
    ULONG               port;               /* Port number to read */
    ULONG               dataBufferSize;
    PULONG              ioBuffer = IoBuffer;
    
    port = *ioBuffer;
    Irp->IoStatus.Information = 0;

    /* Check for input and output buffer size. */

    switch (IoctlCode)
    {
        case IOCTL_GPD_READ_PORT_UCHAR:
            dataBufferSize = sizeof(UCHAR);
            break;
        case IOCTL_GPD_READ_PORT_USHORT:
            dataBufferSize = sizeof(USHORT);
            break;
        case IOCTL_GPD_READ_PORT_ULONG:
            dataBufferSize = sizeof(ULONG);
            break;
        default:
            return STATUS_INVALID_PARAMETER;
    }

    if (InputBufferLength != sizeof(ULONG) ||
        OutputBufferLength < dataBufferSize)
    {
        return STATUS_INVALID_PARAMETER;
    }

    /* Buffers are big enough. */
    
    if (!TranslatePortAddress(port))
    {
        /* Address is in Memory space */
        return STATUS_ACCESS_VIOLATION;
    }

    /* Address is in I/O space */

    switch (IoctlCode)
    {
        case IOCTL_GPD_READ_PORT_UCHAR:
            *(PUCHAR)ioBuffer = READ_PORT_UCHAR(
                                    (PUCHAR)((ULONG_PTR) port));
            break;
        case IOCTL_GPD_READ_PORT_USHORT:
            *(PUSHORT)ioBuffer = READ_PORT_USHORT(
                                    (PUSHORT)((ULONG_PTR) port));
            break;
        case IOCTL_GPD_READ_PORT_ULONG:
            *(PULONG)ioBuffer = READ_PORT_ULONG(
                                    (PULONG)((ULONG_PTR) port));
            break;
        default:
            return STATUS_INVALID_PARAMETER;
    }

    /* Bytes read */
    Irp->IoStatus.Information = dataBufferSize;
    return STATUS_SUCCESS;
}

NTSTATUS
IoctlWritePort(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN OUT PVOID        IoBuffer,
    IN ULONG            InputBufferLength,
    IN ULONG            IoctlCode
    )
{
    ULONG               port;              /* Port number to write */
    ULONG               dataBufferSize;
    PULONG              ioBuffer = (PULONG) IoBuffer;

    port = *ioBuffer++;
    Irp->IoStatus.Information = 0;

    /* Check for input buffer size. */

    switch (IoctlCode)
    {
        case IOCTL_GPD_WRITE_PORT_UCHAR:
            dataBufferSize = sizeof(UCHAR);
            break;
        case IOCTL_GPD_WRITE_PORT_USHORT:
            dataBufferSize = sizeof(USHORT);
            break;
        case IOCTL_GPD_WRITE_PORT_ULONG:
            dataBufferSize = sizeof(ULONG);
            break;
        default:
            return STATUS_INVALID_PARAMETER;
    }

    if (InputBufferLength < (sizeof(ULONG) + dataBufferSize))
    {
        return STATUS_INVALID_PARAMETER;
    }

    /* Buffers are big enough. */

    if (TranslatePortAddress(port) != 1)
    {
        /* Address is in Memory space */
        return STATUS_ACCESS_VIOLATION;
    }

    /* Address is in I/O space */

    switch (IoctlCode)
    {
        case IOCTL_GPD_WRITE_PORT_UCHAR:
            WRITE_PORT_UCHAR(
                (PUCHAR)((ULONG_PTR) port),
                *(PUCHAR) ioBuffer);
            break;
        case IOCTL_GPD_WRITE_PORT_USHORT:
            WRITE_PORT_USHORT(
                (PUSHORT)((ULONG_PTR) port),
                *(PUSHORT) ioBuffer);
            break;
        case IOCTL_GPD_WRITE_PORT_ULONG:
            WRITE_PORT_ULONG(
                (PULONG)((ULONG_PTR) port),
                *(PULONG) ioBuffer);
            break;
        default:
            return STATUS_INVALID_PARAMETER;
    }

    /* Bytes written */
    Irp->IoStatus.Information = dataBufferSize;
    return STATUS_SUCCESS;
}
