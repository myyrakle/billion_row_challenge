#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <pthread.h>
#include <unistd.h>

#include "../common.h"

#define MAX_KEYS 511
#define VALUE_SIZE 32

typedef struct Region {
    long long total;
    int count;
    int min;   
    int max;
    long long dummy[5];
} Region;

typedef struct WorkerContext {
    off_t start_offset;
    off_t length;
    uint8_t** regions;
    int region_count;
    Region* table;
    long long dummy[3];
} WorkerContext;

uint8_t* input_data;

int compare(const void* a, const void* b) {
    const char* str1 = *(const char**)a;
    const char* str2 = *(const char**)b;
    return strcmp(str1, str2);
}

void* worker(void* arg) {
    char region_buffer[32];
    int region_buffer_offset = 0;

    WorkerContext* context = (WorkerContext*)arg;

    context->region_count = 0;
    context->regions = (uint8_t**) malloc(sizeof(uint8_t*) * 100);

    for (int i = 0; i < 100; ++i) {
        context->regions[i] = (uint8_t*) malloc(sizeof(uint8_t) * 32);
    }

    off_t offset = context->start_offset;
    off_t end_offset = context->start_offset + context->length;

    while (offset < end_offset) {   
        uint32_t hash = 7243;

        for (; input_data[offset] != ';'; ++offset) {
            hash = ((hash << 5) + hash) + input_data[offset];
            region_buffer[region_buffer_offset++] = input_data[offset];
        }
        offset++;
        region_buffer[region_buffer_offset] = 0;

        int measurement = 0;
        for (; input_data[offset] != '\n'; ++offset) {
             measurement = measurement * 10 + (input_data[offset] - '0');
        }
        offset++;

        off_t index = hash % MAX_KEYS;
        Region* region = &context->table[index];

        if (region->count == 0) {
            region->total = 0;
            region->min = 0x7fffffff;
            region->max = -1;
            strcpy((char*)context->regions[context->region_count++], region_buffer);
        }

        region->count++;
        region->total += measurement;
        region->max = region->max < measurement ? measurement : region->max;
        region->min = region->min > measurement ? measurement : region->min;

        region_buffer_offset = 0;
    }

    pthread_exit(NULL);
}

const char* solution() {
    int fd = open(MEASUREMENTS_PATH, O_RDONLY);

    struct stat sb;
    fstat(fd, &sb);

    off_t file_size = sb.st_size;
    off_t map_size = (file_size - file_size % 0x1000) + 0x1000;

    input_data = (uint8_t*) mmap64(NULL, map_size, PROT_READ, MAP_SHARED, fd, 0);

    long num_threads = sysconf(_SC_NPROCESSORS_ONLN);
    off_t chunk_size = file_size / num_threads;

    pthread_t threads[100];
    WorkerContext* contexts[100];

    for (int i = 0; i < 100; ++i) {
        contexts[i] = (WorkerContext*) malloc(sizeof(WorkerContext));
    }

    off_t start_offset = 0;
    for (int i = 0; i < num_threads; ++i) {
        contexts[i]->table = (Region*) calloc(sizeof(Region), MAX_KEYS);

        off_t end = start_offset + chunk_size;

        if (file_size <= end) {
            end = file_size - 1;
        } else {
            for (; input_data[end] != '\n'; ++end);
        }

        contexts[i]->start_offset = start_offset;
        contexts[i]->length = end - start_offset + 1;

        pthread_create(&threads[i], NULL, worker, (void *)contexts[i]);

        start_offset += contexts[i]->length;
    }

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(threads[i], NULL);
    }

    Region* result_table = (Region*) calloc(sizeof(Region), MAX_KEYS);

    uint8_t* regions[100];
    int region_count = 0;

    for (int i = 0; i < 100; ++i) {
        regions[i] = (uint8_t*) calloc(1, 32);
    }

    for (int i = 0; i < num_threads; ++i) {
        Region* thread_table = contexts[i]->table;

        for (int j = 0; j < contexts[i]->region_count; ++j) {
            uint32_t hash = 7243;
            for (int k = 0; contexts[i]->regions[j][k] != '\0'; ++k) {
                hash = ((hash << 5) + hash) + contexts[i]->regions[j][k];
            }

            off_t index = hash % MAX_KEYS;

            Region* thread_region = &thread_table[index];
            Region* result_region = &result_table[index];

            if (result_region->count == 0) {
                result_region->total = 0;
                result_region->min = 0x7fffffff;
                result_region->max = -1;
                strcpy((char*)regions[region_count++], (char*)contexts[i]->regions[j]);
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
        uint32_t hash = 7243;
        for (int j = 0; regions[i][j] != '\0'; ++j) {
            hash = ((hash << 5) + hash) + regions[i][j];
        }
        
        off_t index = hash % MAX_KEYS;

        Region* region = &result_table[index];

        char line[256];

        sprintf(line, "%s=%d;%d;%lld(%lld/%d)\n", regions[i], region->min, region->max, region->total / region->count, region-> total, region->count);
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
