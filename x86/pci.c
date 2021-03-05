/**
 * @file pci.c
 * @author artiesy (artezka@gmail.com)
 * @brief Модуль работы с PCI
 * @version 0.1
 * @date 2021-03-04
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#include <x86/pci.h>

typedef struct 
{
    u32int class_code;
    char name[32];
} PCIClassName;


/**
 * @brief Описание типов устройств
 * 
 */
static PCIClassName g_PCIClassNames[] = 
{
    { 0x00, "before PCI 2.0"},
    { 0x01, "disk controller"},
    { 0x02, "network interface"},
    { 0x03, "graphics adapter"},
    { 0x04, "multimedia controller"},
    { 0x05, "memory controller"},
    { 0x06, "bridge device"},
    { 0x07, "communication controller"},
    { 0x08, "system device"},
    { 0x09, "input device"},
    { 0x0a, "docking station"},
    { 0x0b, "CPU"},
    { 0x0c, "serial bus"},
    { 0x0d, "wireless controller"},
    { 0x0e, "intelligent I/O controller"},
    { 0x0f, "satellite controller"},
    { 0x10, "encryption controller"},
    { 0x11, "signal processing controller"},
    { 0xFF, "proprietary device"}
};

typedef struct 
{
    u32int prog_code;
    char name[32];
} PCIProgName;


/**
 * @brief Описание контроллера устройства
 * 
 */
static PCIProgName g_PCIProgNames[] = 
{
    { 0x00, "USB universall host"},
    { 0x10, "USB open host"},
    { 0x20, "USB2 host controller"},
    { 0x80, "USB"},
    { 0xFE, "USB not host controller"}
};

typedef union
{
    struct
    {
        u32int zero 		: 2;
        u32int reg_num     : 6;
        u32int func_num    : 3;
        u32int dev_num     : 5;
        u32int bus_num     : 8;
        u32int reserved    : 7;
        u32int enable_bit  : 1;
    };
    u32int val;
} PCIConfigAddres;

/**
 * @brief Чтение конфигурации устройства
 * 
 * @param bus Шины
 * @param dev Устройство
 * @param func Функция
 * @param reg Индекс регистра
 * @param data Данные
 */
void ReadConfig32(u32int bus, u32int dev, u32int func, u32int reg, u32int *data)
{
    PCIConfigAddres addr;
    
    addr.val = 0;
    addr.enable_bit = 1;
    addr.reg_num =  reg;
    addr.func_num = func;
    addr.dev_num =  dev;
    addr.bus_num =  bus;		

    write_port(PCI_CONFIG_PORT, addr.val);
    data = (u32int*) read_port(PCI_DATA_PORT);
    // kprint("Data: %x\n", data);
    return;
}

/**
 * @brief Возвращает класс устройства по номеру класса
 * 
 * @param class_code Код класса
 * @return char* Название устройства
 */
char *GetPCIDevClassName(u32int class_code)
{
    int i;
    for (i = 0; i < sizeof(g_PCIClassNames)/sizeof(g_PCIClassNames[0]); i++)
    {
        if (g_PCIClassNames[i].class_code == class_code)
            return g_PCIClassNames[i].name;
    }
    return 0;
}

/**
 * @brief Возвращает контроллер устройства по коду
 * 
 * @param progIF Код
 * @return Название контроллера
 */
char *GetPCIDevProgName(u8int progIF)
{
    int i;
    for (i = 0; i < sizeof(g_PCIProgNames)/sizeof(g_PCIProgNames[0]); i++)
    {
        if (g_PCIProgNames[i].prog_code == progIF)
            return g_PCIProgNames[i].name;
    }
    return 0;
}


/**
 * @brief Чтение информации об устройстве
 * 
 * @param bus Шина
 * @param dev Устройство
 * @param func Функция
 * @param p_pciDevice Указатель на структуру PCIDevHeader
 * @return 0 при успешном чтении, 1 - при неудаче
 */
int ReadPCIDevHeader(u32int bus, u32int dev, u32int func, PCIDevHeader *p_pciDevice)
{
    int i;
    
    if (p_pciDevice == 0)
        return 1;
    
	ulong len = sizeof(p_pciDevice->header)/sizeof(p_pciDevice->header[0]);

    for (i = 0; i < len; i++)
        ReadConfig32(bus, dev, func, i, &p_pciDevice->header[i]);
        
	//Данные значения обозначают, что устройства не существует - возвращаем 1
    if (p_pciDevice->option.vendorID == 0x0000 || 
        p_pciDevice->option.vendorID == 0xffff ||
        p_pciDevice->option.deviceID == 0xffff)
        return 1;
        
    return 0;
}

/**
 * @brief Вывод в консоль информации об устройстве
 * 
 * @param bus Шина
 * @param dev Устройство
 * @param func Функция
 * @param p_pciDevice Указатель на структуру PCIDevHeader
 */
void PrintPCIDevHeader(u32int bus, u32int dev, u32int func, PCIDevHeader *p_pciDevice)
{
	// kprint("Class Code: %x\n", p_pciDevice->option.classCode);
	if(p_pciDevice->option.classCode==0x0c && p_pciDevice->option.subClassCode==0x03)
	{
		char *prog_name = GetPCIDevProgName(p_pciDevice->option.progIF);
		if (prog_name)
			kprint("Description=%s\n", prog_name);    
			
		kprint(
			"Bus:Dev:Func=0x%02x:%02x:%02x vID=0x%04x dID=0x%04x ClCode=0x%02x SbClCode=0x%02x PIF=0x%02x", 
			bus, dev, func, p_pciDevice->option.vendorID, p_pciDevice->option.deviceID,
			p_pciDevice->option.classCode, p_pciDevice->option.subClassCode, p_pciDevice->option.progIF
		);
	}   
}


/**
 * @brief Инициализация (сканирование) PCI-устройств
 * 
 */
void init_pci() 
{
	int bus;
	int dev;
	
	console_clear();
	kprint("Scanning PCI Devices...\n");
	
	for (bus = 0; bus < PCI_MAX_BUSES; bus++)
		for (dev = 0; dev < PCI_MAX_DEVICES; dev++)
		{
			u32int func = 0;
			PCIDevHeader pci_device;
			
			if (ReadPCIDevHeader(bus, dev, func, &pci_device))
			{
				kprint("ERROR READING DEVICE AT: %i %i %i", bus, dev, func);	
				continue;
			}
			
			PrintPCIDevHeader(bus, dev, func, &pci_device);

			if (pci_device.option.headerType & PCI_HEADERTYPE_MULTIFUNC)
			{
				for (func = 1; func < PCI_MAX_FUNCTIONS; func++)
				{
					if (ReadPCIDevHeader(bus, dev, func, &pci_device))
						continue;
					PrintPCIDevHeader(bus, dev, func, &pci_device);
				}
			}
		}

	kprint("PCI scan done");
}
