#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <pthread.h>
#include <unistd.h>

#include "../common.h"

#define MAX_KEYS 16384
#define KEY_SIZE 32
#define VALUE_SIZE 32

typedef struct Region {
    long long max;
    long long min;
    long long total;
    long long count;
} Region;

typedef struct {
    char key[KEY_SIZE];
    uint8_t value[VALUE_SIZE];
} TableEntry;

typedef struct WorkerContext {
    off_t start_offset;
    off_t length;
    char** regions;
    int region_count;
    TableEntry* table;
    long long dummy[3];
} WorkerContext;

char* mapped_measurements_buffer;

void* get_region(TableEntry* table, const char* key) {
    uint32_t hash = 5381;
    for (const char* p = key; *p; p++) {
        hash = ((hash << 5) + hash) + *p;
    }
    uint32_t index = hash % MAX_KEYS;

    TableEntry* TableEntry = &table[index];
    if (TableEntry->key[0] == '\0') {
        strncpy(TableEntry->key, key, KEY_SIZE);
        ((Region*)TableEntry->value)->count = 0;
        ((Region*)TableEntry->value)->total = 0;
        ((Region*)TableEntry->value)->min = 0x7fffffffffffffff;
        ((Region*)TableEntry->value)->max = -1;
    }

    return TableEntry->value;
}

int compare(const void* a, const void* b) {
    const char* str1 = *(const char**)a;
    const char* str2 = *(const char**)b;
    return strcmp(str1, str2);
}

inline long long str_to_int(const char* str) {
    int result = 0;

    while (*str) {
        result = result * 10 + (*str++ - '0');
    }

    return result;
}

void* worker(void* arg) {
    char region_buffer[32];
    int region_buffer_offset = 0;

    char measurement_buffer[32];
    int measurement_buffer_offset = 0;

    int state = 0;

    WorkerContext* context = (WorkerContext*)arg;

    context->region_count = 0;

    context->regions = (char**) malloc(sizeof(char*) * 128);

    for (int i = 0; i < 128; ++i) {
        context->regions[i] = (char*) malloc(sizeof(char) * 32);
    }

    off_t offset = context->start_offset;
    off_t end_offset = context->start_offset + context->length;

    while (offset < end_offset) {
        off_t i;
        for (i = offset; mapped_measurements_buffer[i] != ';'; ++i) {
            region_buffer[region_buffer_offset++] = mapped_measurements_buffer[i];
        }
        region_buffer[region_buffer_offset] = 0;

        offset = i + 1;

        for (i = offset; mapped_measurements_buffer[i] != '\n'; ++i) {
            measurement_buffer[measurement_buffer_offset++] = mapped_measurements_buffer[i];
        }
        measurement_buffer[measurement_buffer_offset] = 0;

        offset = i + 1;

        long long measurement = str_to_int(measurement_buffer);

        Region* data = (Region*)get_region(context->table, region_buffer);

        if (data->count == 0) {
            strcpy(context->regions[context->region_count++], region_buffer);
        }

        data->count++;
        data->total += measurement;
        data->max = data->max < measurement ? measurement : data->max;
        data->min = data->min > measurement ? measurement : data->min;

        state = 0;
        region_buffer_offset = 0;
        measurement_buffer_offset = 0;
    }

    pthread_exit(NULL);
}

const char* solution() {
    int fd = open(MEASUREMENTS_PATH, O_RDONLY);

    struct stat sb;
    fstat(fd, &sb);

    off_t file_size = sb.st_size;
    off_t map_size = (file_size - file_size % 0x1000) + 0x1000;

    mapped_measurements_buffer = (char*) mmap64(NULL, map_size, PROT_READ, MAP_SHARED, fd, 0);

    long num_threads = sysconf(_SC_NPROCESSORS_ONLN);
    off_t chunk_size = file_size / num_threads;

    pthread_t threads[128];
    WorkerContext* contexts[128];

    for (int i = 0; i < 128; ++i) {
        contexts[i] = (WorkerContext*) malloc(sizeof(WorkerContext));
    }

    off_t start_offset = 0;
    for (int i = 0; i < num_threads; ++i) {
        contexts[i]->table = (TableEntry*) malloc(sizeof(TableEntry) * MAX_KEYS);

        off_t end = start_offset + chunk_size;

        if (file_size <= end) {
            end = file_size - 1;
        } else {
            for (; mapped_measurements_buffer[end] != '\n'; ++end);
        }

        contexts[i]->start_offset = start_offset;
        contexts[i]->length = end - start_offset + 1;

        pthread_create(&threads[i], NULL, worker, (void *)contexts[i]);

        start_offset += contexts[i]->length;
    }

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(threads[i], NULL);
    }

    TableEntry* result_table = (TableEntry*) malloc(sizeof(TableEntry) * MAX_KEYS);

    char* regions[128];
    int region_count = 0;

    for (int i = 0; i < 128; ++i) {
        regions[i] = (char*) malloc(sizeof(char) * 32);
    }

    for (int i = 0; i < num_threads; ++i) {
        TableEntry* thread_table = contexts[i]->table;

        for (int j = 0; j < contexts[i]->region_count; ++j) {
            Region* thread_region = (Region*) get_region(thread_table, contexts[i]->regions[j]);
            Region* result_region = (Region*) get_region(result_table, contexts[i]->regions[j]);

            if (result_region->count == 0) {
                strcpy(regions[region_count++], contexts[i]->regions[j]);
            }

            result_region->count += thread_region->count;
            result_region->total += thread_region->total;            
            result_region->max = result_region->max < thread_region->max ? thread_region->max : result_region->max;
            result_region->min = result_region->min > thread_region->min ? thread_region->min : result_region->min;
        }
    }

    qsort(regions, region_count, sizeof(char*), compare);

    char* result = (char*) calloc(sizeof(char), 32768);

    for (int i = 0; i < region_count; ++i) {
        Region* data = (Region*) get_region(result_table, regions[i]);

        char line[256];

        sprintf(line, "%s=%lld;%lld;%lld(%lld/%lld)\n", regions[i], data->min, data->max, data->total / data->count, data-> total, data->count);
        strcat(result, line);
    }

    close(fd);

    return result;
}

const char* get_expect_outputs() {
    FILE *stream = fopen(OUTPUT_PATH, "r");

    fseek(stream, 0, SEEK_END);
    long output_file_size = ftell(stream);
    rewind(stream);
    
    char* expect_outputs = (char *)malloc(sizeof(char) * (output_file_size + 1));

    fread(expect_outputs, sizeof(char), output_file_size, stream);
    expect_outputs[output_file_size] = '\0';

    return expect_outputs;
}

int main() {
    const char* expect_outputs = get_expect_outputs();

    auto timer = Timer();
    timer.start();
    const char* got = solution();
    timer.stop();
    const auto elaped = timer.get_milli();

    printf("Elapsed: %lld ms\n", elaped);

    if (strcmp(got, expect_outputs) == 0) {
        puts("Test passed!");
    } else {
        puts("Test failed!");
        printf("Expect: %s\n", expect_outputs);
        printf("Got: %s\n", got);
    }

    pthread_exit(NULL);
}
