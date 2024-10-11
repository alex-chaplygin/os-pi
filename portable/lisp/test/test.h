#define ASSERT(v1, v2)\
    if ((v1) == (v2))\
	printf(" %d OK\n", (int)v1);		\
    else {\
	printf("%d != %d fail\n", (int)v1, (int)v2);	\
    }
#define EPSILON 0.000001f
#define ABS(v) ((v) < 0 ? -(v) : v)
#define ASSERT_FLOAT(f1, f2) \
    if (ABS(f1 - f2) < EPSILON) \
    printf("%f OK \n", f2); \
    else {\
    printf("%f != %f fail \n", f1, f2); \
    }


#define OK ASSERT(0,0)
#define FAIL ASSERT(0,1)
