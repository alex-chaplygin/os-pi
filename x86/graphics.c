#include <objects.h>
#include <alloc.h>
#include <eval.h>
#include <symbols.h>
#include <portable/libc.h>
#include <parser.h>

#define TEXT_BUF_WIDTH 160 // число байт в строке текстового буфера
#define TEXT_BUF_HEIGHT 25 // число строк в текстовом буфере видеопамяти
#define GRAPHIC_BUF_WIDTH 320 // число байт в строке буфера видеопамяти
#define GRAPHIC_BUF_HEIGHT 200 // число строк в буфере
#define TEXT_VIDEO_MEM 0xB8000 // Адрес текстовой видеопамяти 
#define VIDEO_MEM 0xA0000 // Адрес видеопамяти 

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
object_t send_text_buffer(object_t bb, object_t xx, object_t yy, object_t ww, object_t hh)
{
    array_t *buf = GET_ARRAY(bb);
    int x = get_value(xx);
    int y = get_value(yy);
    int w = get_value(ww);
    int h = get_value(hh);
    byte b[TEXT_BUF_WIDTH];
    byte *bp;

    int start_offset = TEXT_BUF_WIDTH * y + x * 2;
    byte *dst = (byte *)TEXT_VIDEO_MEM + start_offset;
    object_t *src = buf->data + start_offset;
    for (int yi = 0; yi < h; yi++) {
	bp = b;
	for (int i = 0; i < (w << 1); i++)
	    *bp++ = GET_CHAR(src[i]);
	memcpy(dst, b, w << 1);
	dst += TEXT_BUF_WIDTH;
	src += TEXT_BUF_WIDTH;
    }
}

/**
 * Отправляет часть экрана в буфер видеопамяти
 * 
 * @param params - (буфер, координаты x,y копируемой области, ширина и высота копируемой области )
 */
object_t send_graphics_buffer(object_t bb, object_t xx, object_t yy, object_t ww, object_t hh)
{
    array_t *buf = GET_ARRAY(bb);
    int x = get_value(xx);
    int y = get_value(yy);
    int w = get_value(ww);
    int h = get_value(hh);
    byte b[GRAPHIC_BUF_WIDTH];
    byte *bp;

    int start_offset = GRAPHIC_BUF_WIDTH * y + x;
    byte *dst = (byte *)VIDEO_MEM + start_offset;
    object_t *src = buf->data + start_offset;
    for (int yi = 0; yi < h; yi++) {
	bp = b;
	for (int i = 0; i < w; i++)
	    *bp++ = GET_CHAR(src[i]);
	memcpy(dst, b, w);
	dst += GRAPHIC_BUF_WIDTH;
	src += GRAPHIC_BUF_WIDTH;
    }
    return NULLOBJ;
}

/** 
 * Копирование массива байт в память
 *
 * @param dst адрес в памяти (число)
 * @param src массив байт
 *
 * @return nil
 */
object_t MEMCPY(object_t dst, object_t src)
{
    if (!IS_NUMBER(dst))
	error("memcpy: dst - not number");
    if (TYPE(src) != ARRAY)
	error("memcpy: src - not array");
    array_t *a = GET_ARRAY(src);
    byte *buf = alloc_region(a->length);
    byte *b = buf;
    object_t *s = a->data;
    for (int i = 0; i < a->length; a++)
	*b++ = get_value(*s++);
    memcpy((unsigned int *)get_value(dst), buf, a->length);
    free_region(buf);
}

void graph_init()
{
    // register_func("GRAPH-SEND-BUFFER", graph_send_buffer);
    register_func("SEND-TEXT-BUFFER", send_text_buffer, 0, 5);
    register_func("SEND-GRAPHICS-BUFFER", send_graphics_buffer, 0, 5);
    register_func("MEMCPY", MEMCPY, 0, 2);
}

