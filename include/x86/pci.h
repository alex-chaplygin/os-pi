#ifndef _PCI_H
#define _PCI_H

#include "x86/x86.h"
#include <x86/console.h>

#define PCI_CONFIG_PORT      0x0CF8
#define PCI_DATA_PORT        0x0CFC

#define PCI_MAX_BUSES        255
#define PCI_MAX_DEVICES      32
#define PCI_MAX_FUNCTIONS    8

#define PCI_HEADERTYPE_NORMAL        0
#define PCI_HEADERTYPE_BRIDGE        1
#define PCI_HEADERTYPE_CARDBUS       2
#define PCI_HEADERTYPE_MULTIFUNC     0x80

typedef union 
{
    struct
    {
        u16int vendorID;
        u16int deviceID;
        u16int commandReg;
        u16int statusReg;
        u8int revisionID;
        u8int progIF;
        u8int subClassCode;
        u8int classCode;
        u8int cachelineSize;
        u8int latency;
        u8int headerType;
        u8int BIST;
    } __attribute__((packed)) option;
    u32int header[4];
} PCIDevHeader;

void ReadConfig32(u32int bus, u32int dev, u32int func, u32int reg, u32int *data);
char *GetPCIDevClassName(u32int class_code);
void PCIScan();
void init_pci();

#endif
