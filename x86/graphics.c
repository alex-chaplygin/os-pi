#include <objects.h>
#include <eval.h>
#include <portable/libc.h>
#include <parser.h>

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
    memcpy(0xA0000, buf, s->length);
    free_region(buf);
    return NULL;
}

object_t *text_send_buffer(object_t *params)
{
    string_t *s = FIRST(params)->u.str;
    memcpy(0xB8000, s->data, s->length);
    return NULL;
}

ushort set_pixel(int x, int y, int colour)
{
    // if (x >= 320 || y >= 200)
    //     return 0;
    char *buf = 0xA0000;
    buf[x + y * 320] = colour;
    return 1;
}

object_t *draw_pixel(object_t *params)
{
    ushort res = set_pixel(FIRST(params)->u.value, SECOND(params)->u.value, THIRD(params)->u.value);
    if (res == 0)
        return ERROR;
    //graph_send_buffer();
    return NULL;
}

/* object_t *draw_line(object_t *params)
{
    int x1 = FIRST(params)->u.value;
    int y1 = SECOND(params)->u.value;
    int x2 = THIRD(params)->u.value;
    int y2 = THIRD(params)->u.pair->right->u.pair->left->u.value;
    int colour = THIRD(params)->u.pair->right->u.pair->right->u.pair->left->u.value;
    double m = (double)((y2 - y1) / (x2 - x1));
    double b = y1 - m * x1;
    for (int x = min(x1, x2); x <= max(x1, x2); x++) {
        double y = m * x + b;
        int yr = round(y);
        ushort res = set_pixel(x, yr, colour);
        // if (res == 0)
        //     return ERROR;
    }
    return NULL;
}
 */
graph_init()
{
    register_func("GRAPH-SEND-BUFFER", graph_send_buffer);
    register_func("DRAW-PIXEL", draw_pixel);
    // register_func("DRAW-LINE", draw_line);
}