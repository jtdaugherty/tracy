
void running_average(int cnt, int len, double * dst, double * src) {
    int i;

    for (i = 0; i < len; i++) {
        dst[i] = (dst[i] * cnt + src[i]) / (cnt + 1);
    }
}

