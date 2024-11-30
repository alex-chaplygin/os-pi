#include <objects.h>
#include <alloc.h>
#include <eval.h>
#include <symbols.h>
#include <portable/libc.h>
#include <parser.h>

#define TEXT_BUF_WIDTH 160 // число байт в строке текстового буфера
#define TEXT_BUF_HEIGHT 25 // число строк в текстовом буфере
#define TEXT_VIDEO_MEM 0xB8000 // Адрес текстовой видео памяти 

/**
 * Отправляет массив в буфер видеопамяти
 * 
 * @param params - массив байтов
 */
object_t graph_send_buffer(object_t params)
{
    array_t *s = GET_ARRAY(FIRST(params));
    byte *buf = alloc_region(s->length);
    byte *dst = buf;
    object_t *src = s->data;
    for (int i = 0; i < s->length; i++)
        *dst++ = get_value(*src++);
    memcpy((void *)0xA0000, buf, s->length);
    free_region(buf);
    return NULLOBJ;
}

/**
 * Отправляет часть экрана в буфер текстовой видеопамяти
 * 
 * @param params - (буфер, координаты x,y копируемой области, ширина и высота копируемой области )
 */
object_t send_text_buffer(object_t params)
{
    array_t *buf = GET_ARRAY(FIRST(params));
    int x = get_value(SECOND(params));
    int y = get_value(THIRD(params));
    int w = 2 * get_value(FIRST(TAIL(TAIL(TAIL(params)))));
    int h = get_value(SECOND(TAIL(TAIL(TAIL(params)))));
    byte b[TEXT_BUF_WIDTH] = {0x30, 0x33, 0x31, 0x34};
    byte *bp;

    int start_offset = TEXT_BUF_WIDTH * y + x * 2;
    byte *dst = (byte *)TEXT_VIDEO_MEM + start_offset;
    object_t *src = buf->data + start_offset;
    for (int yi = 0; yi < h; yi++) {
	bp = b;
	for (int i = 0; i < w; i++)
	    *bp++ = GET_CHAR(src[i]);
	memcpy(dst, b, w);
	dst += TEXT_BUF_WIDTH;
	src += TEXT_BUF_WIDTH;
    }
}

void graph_init()
{
    register_func("GRAPH-SEND-BUFFER", graph_send_buffer);
    register_func("SEND-TEXT-BUFFER", send_text_buffer);
    
}

