object_t make_array(object_t size);
object_t seta(object_t  arr_o, object_t index_obj, object_t value);
object_t aref(object_t  arr_o, object_t index_obj);
object_t array_size(object_t arr_o);
object_t alloc_bytes(object_t size);
object_t array_addr(object_t arr_o);
void init_arrays();
