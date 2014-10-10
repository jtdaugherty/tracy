
void running_average(double oldcnt, int len, double * dst, double * src) {
    int i;

    for (i = 0; i < len; i++) {
        dst[i] = (dst[i] * oldcnt + src[i]) / (oldcnt + 1);
    }
}

