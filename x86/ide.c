/**
 * @file ide.c
 * @author finko-ilya (you@domain.com)
 * @brief Модуль работы с IDE
 * @version 0.1
 * @date 2021-01-15
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include <portable/types.h>
#include <x86/x86.h>
#include <portable/libc.h>
#include <x86/ide.h>

unsigned char ide_buf[2048] = {0};
ide_device ide_devices[4];
IDEChannelRegisters channels[2];



static void __delay(int ms)
{
	int i;
	for( i = 0; i < 10*ms; i++){}
}

/**
 * @brief ЗАпись в шину pci
 * 
 * @param channel 
 * @param reg 
 * @param data 
 */
void ide_write(unsigned char channel, unsigned char reg, unsigned char data) {
	if (reg > 0x07 && reg < 0x0C)
		ide_write(channel, ATA_REG_CONTROL, 0x80 | channels[channel].nIEN);
	if (reg < 0x08)
		write_port(channels[channel].base  + reg - 0x00, data);
	else if (reg < 0x0C)
		write_port(channels[channel].base  + reg - 0x06, data);
	else if (reg < 0x0E)
		write_port(channels[channel].ctrl  + reg - 0x0A, data);
	else if (reg < 0x16)
		write_port(channels[channel].bmide + reg - 0x0E, data);
	if (reg > 0x07 && reg < 0x0C)
		ide_write(channel, ATA_REG_CONTROL, channels[channel].nIEN);
}

/**
 * @brief Чтение из шины pci
 * 
 * @param channel 
 * @param reg 
 * @param data 
 */
unsigned char ide_read(unsigned char channel, unsigned char reg) {
	unsigned char result;
	if (reg > 0x07 && reg < 0x0C)
		ide_write(channel, ATA_REG_CONTROL, 0x80 | channels[channel].nIEN);
	if (reg < 0x08)
		result = read_port(channels[channel].base + reg - 0x00);
	else if (reg < 0x0C)
		result = read_port(channels[channel].base  + reg - 0x06);
	else if (reg < 0x0E)
		result = read_port(channels[channel].ctrl  + reg - 0x0A);
	else if (reg < 0x16)
		result = read_port(channels[channel].bmide + reg - 0x0E);
	if (reg > 0x07 && reg < 0x0C)
		ide_write(channel, ATA_REG_CONTROL, channels[channel].nIEN);
	return result;
}


/**
 * @brief Инициализация ide по заданным базовым адресам шины
 * 

 */
void ide_initialize(unsigned int BAR0, unsigned int BAR1, unsigned int BAR2, unsigned int BAR3, unsigned int BAR4)
{
	int i, j, k, count = 0;

	// 1- Detect I/O Ports which interface IDE Controller:
	channels[ATA_PRIMARY  ].base  = (BAR0 & 0xFFFFFFFC) + 0x1F0 * (!BAR0);
	channels[ATA_PRIMARY  ].ctrl  = (BAR1 & 0xFFFFFFFC) + 0x3F6 * (!BAR1);
	channels[ATA_SECONDARY].base  = (BAR2 & 0xFFFFFFFC) + 0x170 * (!BAR2);
	channels[ATA_SECONDARY].ctrl  = (BAR3 & 0xFFFFFFFC) + 0x376 * (!BAR3);
	channels[ATA_PRIMARY  ].bmide = (BAR4 & 0xFFFFFFFC) + 0; // Bus Master IDE
	channels[ATA_SECONDARY].bmide = (BAR4 & 0xFFFFFFFC) + 8; // Bus Master IDE
	// 2- Disable IRQs:
	ide_write(ATA_PRIMARY  , ATA_REG_CONTROL, 2);
	ide_write(ATA_SECONDARY, ATA_REG_CONTROL, 2);

	int ide_init_result = read_port(0xCFC) >> 16;
	if (ide_init_result == 0xFFFF){
		kprint("No IDE controller found");
		return;
	}

	//kprint("ide intit %i", ide_init_result);
	// 3- Detect ATA-ATAPI Devices:
	for (i = 0; i < 2; i++)
		for (j = 0; j < 1; j++) {
			//j = 1;

			unsigned char err = 0, type = IDE_ATA, status;
			ide_devices[count].Reserved = 0; // Assuming that no drive here.

			// (I) Select Drive:
			ide_write(i, ATA_REG_HDDEVSEL, 0xA0 | (j << 4)); // Select Drive.
			__delay(1); // Wait 1ms for drive select to work.
			// (II) Send ATA Identify Command:
			ide_write(i, ATA_REG_COMMAND, ATA_CMD_IDENTIFY);
			__delay(1); // This function should be implemented in your OS. which waits for 1 ms.
			// it is based on System Timer Device Driver.

			// (III) Polling:
			if (ide_read(i, ATA_REG_STATUS) == 0) {
				//continue;
			} // If Status = 0, No Device.
			// printk("--Get device %d\n",i);
			while(1) {
				status = ide_read(i, ATA_REG_STATUS);
				if ((status & ATA_SR_ERR)) {err = 1; break;} // If Err, Device is not ATA.
				if (!(status & ATA_SR_BSY) && (status & ATA_SR_DRQ)) break; // Everything is right.
			}

			// (IV) Probe for ATAPI Devices:
			if (err != 0) {
				unsigned char cl = ide_read(i, ATA_REG_LBA1);
				unsigned char ch = ide_read(i, ATA_REG_LBA2);

				if (cl == 0x14 && ch ==0xEB)
					type = IDE_ATAPI;
				else if (cl == 0x69 && ch == 0x96)
					type = IDE_ATAPI;
				//else
				//	continue; // Unknown Type (may not be a device).

				ide_write(i, ATA_REG_COMMAND, ATA_CMD_IDENTIFY_PACKET);
				__delay(1);
			}

			// (V) Read Identification Space of the Device:
			//ide_read_buffer(i, ATA_REG_DATA, (unsigned int) ide_buf, 128);

			// (VI) Read Device Parameters:
			ide_devices[count].Reserved     = 1;
			ide_devices[count].Type         = type;
			ide_devices[count].Channel      = i;
			ide_devices[count].Drive        = j;
			ide_devices[count].Signature    = *((unsigned short *)(ide_buf + ATA_IDENT_DEVICETYPE));
			ide_devices[count].Capabilities = *((unsigned short *)(ide_buf + ATA_IDENT_CAPABILITIES));
			ide_devices[count].CommandSets  = *((unsigned int *)(ide_buf + ATA_IDENT_COMMANDSETS));

			// (VII) Get Size:
			if (ide_devices[count].CommandSets & (1 << 26))
			{
				// Device uses 48-Bit Addressing:
				//printk("Device %d use 48-Bit Addressing\n", count);
				ide_devices[count].Size   = *((unsigned int *)(ide_buf + ATA_IDENT_MAX_LBA_EXT));
			}
			else{
				// Device uses CHS or 28-bit Addressing:
				//printk("Device %d use CHS Addressing\n", count);
				ide_devices[count].Size   = *((unsigned int *)(ide_buf + ATA_IDENT_MAX_LBA));
			}

			// (VIII) String indicates model of device (like Western Digital HDD and SONY DVD-RW...):
			for(k = 0; k < 40; k += 2) {
				ide_devices[count].Model[k] = ide_buf[ATA_IDENT_MODEL + k + 1];
				ide_devices[count].Model[k + 1] = ide_buf[ATA_IDENT_MODEL + k];}
			ide_devices[count].Model[40] = 0; // Terminate String.

			count++;
		}
		

	// 4- Print Summary:
	for (i = 0; i < 4; i++)
		if (ide_devices[i].Reserved == 1) {
			kprint(" Slot %d found %s Drive %dMB - %s\n", i,
					(const char *[]){"ATA", "ATAPI"}[ide_devices[i].Type],         /* Type */
					ide_devices[i].Size / 2048 ,               /* Size */
					ide_devices[i].Model);
		}
}


void init_ide(){
  kprint("Init IDE\n");
    ide_initialize(0x1F0, 0x3F6, 0x170, 0x376, 0x000);
}

