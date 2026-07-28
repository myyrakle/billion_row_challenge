#pragma GCC target("avx2")
#pragma GCC optimize("O3,unroll-loops")

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <pthread.h>
#include <unistd.h>
#include <immintrin.h>

#include "../common.h"

#define HASH_SIZE   512
#define HASH_MASK   (HASH_SIZE - 1)
#define MAX_THREADS 64

typedef struct __attribute__((aligned(32))) R_t {
    long long t;
    uint64_t  w;
    int       c;
    int       n;
    int       m;
    uint16_t  l;
    uint16_t  p;
} R_t;

typedef struct {
    off_t       o;
    off_t       s;
    R_t*        t;
    const char* p[HASH_SIZE];
} W_t;

typedef struct {
    char      a[32];
    int       n;
    int       m;
    long long t;
    int       c;
} F_t;

static uint8_t* _d;

static inline __attribute__((always_inline))
uint32_t h_f(uint64_t _w) {
    return (uint32_t)((_w * 0xaf7b84ccc0308f9bULL) >> 54) & HASH_MASK;
}

static void* __attribute__((hot)) w_f(void* _a) {
    W_t* _ctx = (W_t*)_a;

    const uint8_t* _b = _d;
    off_t _p = _ctx->o;
    const off_t _e = _ctx->o + _ctx->s;
    R_t* __restrict _tbl = _ctx->t;

    const __m256i _sv = _mm256_set1_epi8(';');

    while (_p < _e) {
        const uint8_t* _ns = &_b[_p];
        int _nl;
        uint32_t _h;
        uint64_t _nw = 0;
        int _mv;

        if (__builtin_expect(_p + 64 <= _e, 1)) {
            __m256i _chk = _mm256_loadu_si256((const __m256i*)&_b[_p]);
            uint32_t _sm = (uint32_t)_mm256_movemask_epi8(
                _mm256_cmpeq_epi8(_chk, _sv));

            _nl = __builtin_ctz(_sm);

            memcpy(&_nw, &_b[_p], 8);
            int _l8 = _nl < 8 ? _nl : 8;
            int _sh = (8 - _l8) * 8;
            _nw = (_nw << _sh) >> _sh;
            _h = h_f(_nw);
            _p += _nl + 1;

            uint64_t _w8;
            memcpy(&_w8, &_b[_p], 8);

            uint64_t _xnl = _w8 ^ 0x0a0a0a0a0a0a0a0aULL;
            uint64_t _nll = (_xnl - 0x0101010101010101ULL)
                          & ~_xnl
                          & 0x8080808080808080ULL;

            int _nd = (_nll == 0) ? 8 : (__builtin_ctzll(_nll) >> 3);

            uint64_t _sw = _w8 - 0x3030303030303030ULL;
            _sw <<= (8 - _nd) * 8;

            uint64_t _lo = (_sw & 0x000F000F000F000FULL) * 10 + ((_sw >> 8) & 0x000F000F000F000FULL);
            uint64_t _q = (_lo & 0x0000FFFF0000FFFFULL) * 100 + ((_lo >> 16) & 0x0000FFFF0000FFFFULL);
            _mv = (_q & 0xFFFFFFFFULL) * 10000 + ((_q >> 32) & 0xFFFFFFFFULL);

            _p += _nd + 1;
        } else {
            _nl = 0;
            while (_b[_p + _nl] != ';') _nl++;
            memcpy(&_nw, &_b[_p], _nl < 8 ? _nl : 8);
            _h = h_f(_nw);
            _p += _nl + 1;

            _mv = 0;
            while (_b[_p] != '\n') {
                _mv = _mv * 10 + (_b[_p] - '0');
                _p++;
            }
            _p++;
        }

        uint32_t _idx = _h;
        R_t* _r = &_tbl[_idx];

        if (__builtin_expect(_r->c == 0, 0)) {
            _r->t = 0;
            _r->n = 0x7fffffff;
            _r->m = -1;
            _r->w = _nw;
            _r->l = (uint16_t)_nl;
            _ctx->p[_idx] = (const char*)_ns;
        }

        _r->c++;
        _r->t += _mv;
        _r->m = _mv > _r->m ? _mv : _r->m;
        _r->n = _mv < _r->n ? _mv : _r->n;
    }

    return NULL;
}

static int c_f(const void* _x, const void* _y) {
    return strcmp(((const F_t*)_x)->a, ((const F_t*)_y)->a);
}

static const char* solution() {
    int _fd = open(MEASUREMENTS_PATH, O_RDONLY);
    struct stat _sb;
    fstat(_fd, &_sb);

    off_t _fs = _sb.st_size;
    off_t _ms = (_fs - _fs % 0x1000) + 0x1000;

    _d = (uint8_t*)mmap64(NULL, _ms, PROT_READ, MAP_PRIVATE, _fd, 0);

    madvise(_d, _ms, MADV_SEQUENTIAL);
    madvise(_d, _ms, MADV_WILLNEED);
    madvise(_d, _ms, MADV_HUGEPAGE);

    long _nt = sysconf(_SC_NPROCESSORS_ONLN);
    if (_nt > MAX_THREADS) _nt = MAX_THREADS;
    off_t _cs = _fs / _nt;

    pthread_t _th[MAX_THREADS];
    W_t       _cx[MAX_THREADS];

    off_t _st = 0;
    for (int _i = 0; _i < _nt; ++_i) {
        if (_st >= _fs) {
            _nt = _i;
            break;
        }

        _cx[_i].t = (R_t*)calloc(HASH_SIZE, sizeof(R_t));
        memset((void*)_cx[_i].p, 0, sizeof(_cx[_i].p));

        off_t _end = _st + _cs;
        if (_end >= _fs) {
            _end = _fs - 1;
        } else {
            while (_d[_end] != '\n') ++_end;
        }

        _cx[_i].o = _st;
        _cx[_i].s = _end - _st + 1;

        pthread_create(&_th[_i], NULL, w_f, &_cx[_i]);
        _st += _cx[_i].s;
    }

    for (int _i = 0; _i < _nt; ++_i) {
        pthread_join(_th[_i], NULL);
    }

    R_t _mg[HASH_SIZE];
    memset(_mg, 0, sizeof(_mg));
    const char* _mgn[HASH_SIZE];
    memset((void*)_mgn, 0, sizeof(_mgn));

    F_t _res[100];
    int _tot = 0;

    for (int _i = 0; _i < _nt; ++_i) {
        for (int _j = 0; _j < HASH_SIZE; ++_j) {
            R_t* _src = &_cx[_i].t[_j];
            if (_src->c == 0) continue;

            uint32_t _idx = _j;
            R_t*  _dst = &_mg[_idx];

            if (_dst->c == 0) {
                _dst->t = 0;
                _dst->n = 0x7fffffff;
                _dst->m = -1;
                _dst->w = _src->w;
                _dst->l = _src->l;
                _mgn[_idx] = _cx[_i].p[_j];
            }

            _dst->c += _src->c;
            _dst->t += _src->t;
            _dst->m = _src->m > _dst->m ? _src->m : _dst->m;
            _dst->n = _src->n < _dst->n ? _src->n : _dst->n;
        }
    }

    for (int _j = 0; _j < HASH_SIZE; ++_j) {
        R_t* _dst = &_mg[_j];
        if (_dst->c == 0) continue;

        F_t* _r = &_res[_tot++];
        memcpy(_r->a, _mgn[_j], _dst->l);
        _r->a[_dst->l] = '\0';
        _r->n   = _dst->n;
        _r->m   = _dst->m;
        _r->t   = _dst->t;
        _r->c   = _dst->c;
    }

    qsort(_res, _tot, sizeof(F_t), c_f);

    char* _out = (char*)calloc(32768, 1);
    int   _len = 0;

    for (int _i = 0; _i < _tot; ++_i) {
        F_t* _r = &_res[_i];
        _len += sprintf(&_out[_len], "%s=%d;%d;%lld(%lld/%d)\n",
                        _r->a, _r->n, _r->m,
                        _r->t / _r->c, _r->t, _r->c);
    }

    munmap(_d, _ms);
    close(_fd);
    for (int _i = 0; _i < _nt; ++_i) {
        free(_cx[_i].t);
    }

    return _out;
}

static const char* get_expect_outputs() {
    FILE* _f = fopen(OUTPUT_PATH, "r");
    fseek(_f, 0, SEEK_END);
    long _sz = ftell(_f);
    rewind(_f);
    char* _b = (char*)malloc(_sz + 1);
    fread(_b, 1, _sz, _f);
    _b[_sz] = '\0';
    fclose(_f);
    return _b;
}

int main() {
    const char* expected = get_expect_outputs();

    Timer timer;
    timer.start();
    const char* result = solution();
    timer.stop();

    printf("Elapsed: %lld ms\n", timer.get_milli());

    if (strcmp(result, expected) == 0) {
        puts("Test passed!");
    } else {
        puts("Test failed!");
        printf("Expect: %s\n", expected);
        printf("Got: %s\n", result);
    }

    return 0;
}
