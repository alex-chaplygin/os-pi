#include <x86/ports.h>
#include <x86/x86.h>

void mouse_wait(byte type) {
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

void mouse_write(byte data) {
    // Wait to be able to send a command.
    mouse_wait(1);
    // Tell the mouse we are sending a command.
    write_port(0x64, 0xD4);
    // Wait for the final part.
    mouse_wait(1);
    // Finally write.
    write_port(0x60, data);
}

byte mouse_read() {
    // Get the response from the mouse.
    mouse_wait(0);

    return read_port(0x60);
}

byte mouse_check() {
    mouse_write(0xFF);
    // Acknowledge.
    read_port(0x60);

    return mouse_read();
}

void mouse_init() {
    byte status;

    // Allow mouse.
    mouse_wait(1);
    write_port(0x64, 0xA8);

    // Enable the interrupts.
    mouse_wait(1);
    write_port(0x64, 0x20);
    mouse_wait(0);
    status = read_port(0x60) | 2;
    mouse_wait(1);
    write_port(0x64, 0x60);
    mouse_wait(1);
    write_port(0x60, status);

    // Tell the mouse to use default settings.
    mouse_write(0xF6);
    // Acknowledge.
    mouse_read();

    // Enable the mouse.
    mouse_write(0xF4);
    // Acknowledge.
    mouse_read();
}