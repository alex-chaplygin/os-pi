void memcpy(unsigned char* destptr, unsigned char* srcptr, int num ){
    for (int i = 0; i < num; i++)
    {
        destptr[i] = srcptr[i];
    }
    
}