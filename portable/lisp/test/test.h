#define ASSERT(v1, v2)\
    if ((v1) == (v2))\
	    printf(" %d OK\n", v1);\
      else\
	    printf("%d != %d fail\n", v1, v2);
