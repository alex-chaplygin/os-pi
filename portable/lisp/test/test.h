#define ASSERT(v1, v2)\
    if ((v1) == (v2))\
	printf(" %d OK\n", (int)v1);		\
    else {\
	printf("%d != %d fail\n", (int)v1, (int)v2);	\
    }
