#include <objects.h>
#include <eval.h>
#include <symbols.h>
#include <portable/libc.h>
#include <parser.h>

/**
 * Отправляет массив в буфер видеопамяти
 * 
 * @param params - массив байтов
 */
object_t *graph_send_buffer(object_t *params)
{
    array_t *s = FIRST(params)->u.arr;
    byte *buf = alloc_region(s->length);
    byte *dst = buf;
    object_t **src = s->data;
    for (int i = 0; i < s->length; i++) {
        *dst++ = (*src)->u.value;
        src++;
    }
    memcpy((void *)0xA0000, buf, s->length);
    free_region(buf);
    return NULL;
}

void graph_init()
{
    register_func("GRAPH-SEND-BUFFER", graph_send_buffer);
}
