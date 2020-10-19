#include "types.h"
#include "x86.h"

void mouseWait(byte type) {
    uint timeout = 10000;

    if (type == 0) {
        while (timeout--) {
            if ((read_port(0x64) & 1) == 1) {
                return;
            }
        }
    } else {
        while (timeout--) {
            if ((read_port(0x64) & 2) == 0) {
                return;
            }
        }
    }
}

void mouseWrite(byte data) {
    // Wait to be able to send a command.
    mouseWait(1);
    // Tell the mouse we are sending a command.
    write_port(0x64, 0xD4);
    // Wait for the final part.
    mouseWait(1);
    // Finally write.
    write_port(0x60, data);
}

byte mouseRead() {
    // Get the response from the mouse.
    mouseWait(0);

    return read_port(0x60);
}

byte mouseCheck() {
    mouseWrite(0xFF);
    // Acknowledge.
    read_port(0x60);

    return mouseRead();
}

void mouseInit() {
    byte status;

    // Allow mouse.
    mouseWait(1);
    write_port(0x64, 0xA8);

    // Enable the interrupts.
    mouseWait(1);
    write_port(0x64, 0x20);
    mouseWait(0);
    status = read_port(0x60) | 2;
    mouseWait(1);
    write_port(0x64, 0x60);
    mouseWait(1);
    write_port(0x60, status);

    // Tell the mouse to use default settings.
    mouseWrite(0xF6);
    // Acknowledge.
    mouseRead();

    // Enable the mouse.
    mouseWrite(0xF4);
    // Acknowledge.
    mouseRead();
}