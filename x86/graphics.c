#include <objects.h>
#include <eval.h>
#include <portable/libc.h>

object_t *graph_send_buffer(object_t *params)
{
    string_t *s = FIRST(params)->u.str;
    memcpy(0xA0000, s->data, s->length);
    return NULL;
}

object_t *text_send_buffer(object_t *params)
{
    string_t *s = FIRST(params)->u.str;
    memcpy(0xB8000, s->data, s->length);
    return NULL;
}

graph_init()
{
    register_func("GRAPH-SEND-BUFFER", graph_send_buffer);
    register_func("TEXT-SEND-BUFFER", text_send_buffer);
}