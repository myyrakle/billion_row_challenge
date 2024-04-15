#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <pthread.h>
#include <unistd.h>

#include "../common.h"

typedef struct Region {
    uint64_t total;
    uint32_t count;
    uint32_t min;   
    int32_t max;
    uint64_t dummy[5];
} Region;

typedef struct WorkerContext {
    off_t start_offset;
    off_t length;
    Region* table;
    uint8_t* mem;
    uint64_t dummy[4];
} WorkerContext;

typedef struct FourBytes {
    uint8_t first;
    uint8_t second;
    uint8_t third;
    uint8_t fourth;
} FirstToThird;

const char* CITY_NAMES[] = {"Adenarith", "Amsterdam", "Anápolis", "Aparecida de Goiânia", "Athens", "Austin", "Bahrain", "Barcelona", "Bangalore", "Belém", "Belo Horizonte", "Boa Vista", "Boston", "Brasília", "Brussels", "Bucharest", "Campinas", "Canada", "Central", "Chennai", "Chongqing", "Copenhagen", "Cuiabá", "Curitiba", "Dallas", "Duque de Caxias", "Dublin", "Frankfurt", "Feira de Santana", "Fortaleza", "Gaaphis", "Guarulhos", "Guadalajara", "Goiania", "Hyderabad", "Helsinki", "Hong Kong", "Ireland", "Istanbul", "Indianapolis", "Juiz de Fora", "Kiev", "Krore", "Krofast", "Kolkata", "Larfast", "Los Angeles", "Londrina", "London", "Macapá", "Madrid", "Manaus", "Mexico City", "Miami", "Milan", "Moscow", "Montreal", "Mumbai", "Niterói", "New York", "New Delhi", "N. California", "N. Virginia", "Nova Iguaçu", "Ohio", "Oregon", "Osasco", "Osaka", "Oslo", "Paris", "Palmas", "Prico", "Prover", "Pune", "Porto Alegre", "Porto Velho", "Qreigh", "Qrokwood", "Ribeirão Preto", "Rio de Janeiro", "Recife", "Salvador", "Santo André", "Sao Paulo", "São Paulo", "São Bernardo do Campo", "São José dos Campos", "São Gonçalo", "Seoul", "Singapore", "St. Petersburg", "Stockholm", "Sydney", "Toronto", "Tokyo", "Urgtin", "Vancouver", "Vienna", "Warsaw", "Zurich"};
const int INDEX_MAP[100] = {0, 1, 2, 3, 4, 5, 6, 8, 7, 10, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 25, 28, 29, 27, 30, 33, 32, 31, 35, 36, 34, 39, 37, 38, 40, 41, 44, 43, 42, 45, 48, 47, 46, 49, 50, 51, 52, 53, 54, 56, 55, 57, 61, 62, 60, 59, 58, 63, 64, 65, 67, 66, 68, 70, 69, 74, 75, 71, 72, 73, 76, 77, 80, 78, 79, 81, 82, 83, 88, 89, 90, 91, 92, 85, 87, 86, 84, 94, 93, 95, 96, 97, 98, 99};

void* worker(void* arg) {
    WorkerContext* context = (WorkerContext*)arg;

    Region* table = context->table;    

    off_t offset = context->start_offset;
    off_t end_offset = context->start_offset + context->length;

    uint8_t* mem = &context->mem[offset];
    uint8_t* end = &context->mem[end_offset];

    for (int i = 0; i < 100; i++) {
        Region* region = &table[i];
        region->total = 0;
        region->min = 0x7fffffff;
        region->max = -1;
    }

    while (mem != end) {
        Region* region;

        FourBytes first_four_bytes = *((FourBytes*)mem);

        if (first_four_bytes.first == 65) {
            if (first_four_bytes.second == 100) {
                mem += 10;
                region = table + 0;
            } else if (first_four_bytes.second == 109) {
                mem += 10;
                region = table + 1;
            } else if (first_four_bytes.second == 110) {
                mem += 10;
                region = table + 2;
            } else if (first_four_bytes.second == 112) {
                mem += 22;
                region = table + 3;
            } else if (first_four_bytes.second == 116) {
                mem += 7;
                region = table + 4;
            } else {
                mem += 7;
                region = table + 5;
            }
        } else if (first_four_bytes.first == 66) {
            if (first_four_bytes.second == 97) {
                if (first_four_bytes.third == 104) {
                    mem += 8;
                    region = table + 6;
                } else if (first_four_bytes.third == 114) {
                    mem += 10;
                    region = table + 7;
                } else {
                    mem += 10;
                    region = table + 8;
                }
            } else if (first_four_bytes.second == 101) {
                if (first_four_bytes.fourth == 195) {
                    mem += 7;
                    region = table + 9;
                } else {
                    mem += 15;
                    region = table + 10;
                }
            } else if (first_four_bytes.second == 111) {
                if (first_four_bytes.third == 97) {
                    mem += 10;
                    region = table + 11;
                } else {
                    mem += 7;
                    region = table + 12;
                }
            } else if (first_four_bytes.second == 114) {
                if (first_four_bytes.third == 97) {
                    mem += 10;
                    region = table + 13;
                } else {
                    mem += 9;
                    region = table + 14;
                }
            } else {
                mem += 10;
                region = table + 15;
            }
        } else if (first_four_bytes.first == 67) {
            if (first_four_bytes.second == 97) {
                if (first_four_bytes.third == 109) {
                    mem += 9;
                    region = table + 16;
                } else {
                    mem += 7;
                    region = table + 17;
                }
            } else if (first_four_bytes.second == 101) {
                mem += 8;
                region = table + 18;
            } else if (first_four_bytes.second == 104) {
                if (first_four_bytes.third == 101) {
                    mem += 8;
                    region = table + 19;
                } else {
                    mem += 10;
                    region = table + 20;
                }
            } else if (first_four_bytes.second == 111) {
                mem += 11;
                region = table + 21;
            } else {
                if (first_four_bytes.third == 105) {
                    mem += 8;
                    region = table + 22;
                } else {
                    mem += 9;
                    region = table + 23;
                }
            }
        } else if (first_four_bytes.first == 68) {
            if (first_four_bytes.second == 97) {
                mem += 7;
                region = table + 24;
            } else {
                if (first_four_bytes.third == 113) {
                    mem += 16;
                    region = table + 25;
                } else {
                    mem += 7;
                    region = table + 26;
                }
            }
        } else if (first_four_bytes.first == 70) {
            if (first_four_bytes.second == 114) {
                mem += 10;
                region = table + 27;
            } else if (first_four_bytes.second == 101) {
                mem += 17;
                region = table + 28;
            } else {
                mem += 10;
                region = table + 29;
            }
        } else if (first_four_bytes.first == 71) {
            if (first_four_bytes.second == 97) {
                mem += 8;
                region = table + 30;
            } else if (first_four_bytes.second == 117) {
                if (first_four_bytes.fourth == 114) {
                    mem += 10;
                    region = table + 31;
                } else {
                    mem += 12;
                    region = table + 32;
                }
            } else {
                mem += 8;
                region = table + 33;
            }
        } else if (first_four_bytes.first == 72) {
            if (first_four_bytes.second == 121) {
                mem += 10;
                region = table + 34;
            } else if (first_four_bytes.second == 101) {
                mem += 9;
                region = table + 35;
            } else {
                mem += 10;
                region = table + 36;
            }
        } else if (first_four_bytes.first == 73) {
            if (first_four_bytes.second == 114) {
                mem += 8;
                region = table + 37;
            } else if (first_four_bytes.second == 115) {
                mem += 9;
                region = table + 38;
            } else {
                mem += 13;
                region = table + 39;
            }
        } else if (first_four_bytes.first == 74) {
            mem += 13;
            region = table + 40;
        } else if (first_four_bytes.first == 75) {
            if (first_four_bytes.second == 105) {
                mem += 5;
                region = table + 41;
            } else if (first_four_bytes.second == 114) {
                if (first_four_bytes.fourth == 114) {
                    mem += 6;
                    region = table + 42;
                } else {
                    mem += 8;
                    region = table + 43;
                }
            } else {
                mem += 8;
                region = table + 44;
            }
        } else if (first_four_bytes.first == 76) {
            if (first_four_bytes.second == 97) {
                mem += 8;
                region = table + 45;
            } else {
                if (first_four_bytes.third == 115) {
                    mem += 12;
                    region = table + 46;
                } else {
                    if (mem[4] == 114) {
                        mem += 9;
                        region = table + 47;
                    } else {
                        mem += 7;
                        region = table + 48;
                    }
                }
            }
        } else if (first_four_bytes.first == 77) {
            if (first_four_bytes.second == 97) {
                if (first_four_bytes.third == 99) {
                    mem += 8;
                    region = table + 49;
                } else if (first_four_bytes.third == 100) {
                    mem += 7;
                    region = table + 50;
                } else {
                    mem += 7;
                    region = table + 51;
                }
            } else if (first_four_bytes.second == 101) {
                mem += 12;
                region = table + 52;
            } else if (first_four_bytes.second == 105) {
                if (first_four_bytes.third == 97) {
                    mem += 6;
                    region = table + 53;
                } else {
                    mem += 6;
                    region = table + 54;
                }
            } else if (first_four_bytes.second == 111) {
                if (first_four_bytes.third == 115) {
                    mem += 7;
                    region = table + 55;
                } else {
                    mem += 9;
                    region = table + 56;
                }
            } else {
                mem += 7;
                region = table + 57;
            }
        } else if (first_four_bytes.first == 78) {
            if (first_four_bytes.second == 105) {
                mem += 9;
                region = table + 58;
            } else if (first_four_bytes.second == 101) {
                if (mem[4] == 89) {
                    mem += 9;
                    region = table + 59;
                } else {
                    mem += 10;
                    region = table + 60;
                }
            } else if (first_four_bytes.second == 46) {
                if (first_four_bytes.fourth == 67) {
                    mem += 14;
                    region = table + 61;
                } else {
                    mem += 12;
                    region = table + 62;
                }
            } else {
                mem += 13;
                region = table + 63;
            }
        } else if (first_four_bytes.first == 79) {
            if (first_four_bytes.second == 104) {
                mem += 5;
                region = table + 64;
            } else if (first_four_bytes.second == 114) {
                mem += 7;
                region = table + 65;
            } else {
                if (first_four_bytes.third == 97) {
                    if (first_four_bytes.fourth == 115) {
                        mem += 7;
                        region = table + 66;
                    } else {
                        mem += 6;
                        region = table + 67;
                    }
                } else {
                    mem += 5;
                    region = table + 68;
                }
            }
        } else if (first_four_bytes.first == 80) {
            if (first_four_bytes.second == 97) {
                if (first_four_bytes.third == 114) {
                    mem += 6;
                    region = table + 69;
                } else {
                    mem += 7;
                    region = table + 70;
                }
            } else if (first_four_bytes.second == 114) {
                if (first_four_bytes.third == 105) {
                    mem += 6;
                    region = table + 71;
                } else {
                    mem += 7;
                    region = table + 72;
                }
            } else if (first_four_bytes.second == 117) {
                mem += 5;
                region = table + 73;
            } else {
                if (mem[6] == 65) {
                    mem += 13;
                    region = table + 74;
                } else {
                    mem += 12;
                    region = table + 75;
                }
            }
        } else if (first_four_bytes.first == 81) {
            if (first_four_bytes.third == 101) {
                mem += 7;
                region = table + 76;
            } else {
                mem += 9;
                region = table + 77;
            }
        } else if (first_four_bytes.first == 82) {
            if (first_four_bytes.second == 105) {
                if (first_four_bytes.third == 98) {
                    mem += 16;
                    region = table + 78;
                } else {
                    mem += 15;
                    region = table + 79;
                }
            } else {
                mem += 7;
                region = table + 80;
            }
        } else if (first_four_bytes.first == 83) {
            if (first_four_bytes.second == 97) {
                if (first_four_bytes.third == 108) {
                    mem += 9;
                    region = table + 81;
                } else if (first_four_bytes.third == 110) {
                    mem += 13;
                    region = table + 82;
                } else {
                    mem += 10;
                    region = table + 83;
                }
            } else if (first_four_bytes.second == 195) {
                if (mem[5] == 80) {
                    mem += 11;
                    region = table + 84;
                } else if (mem[5] == 66) {
                    mem += 23;
                    region = table + 85;
                } else if (mem[5] == 74) {
                    mem += 22;
                    region = table + 86;
                } else {
                    mem += 14;
                    region = table + 87;
                }
            } else if (first_four_bytes.second == 101) {
                mem += 6;
                region = table + 88;
            } else if (first_four_bytes.second == 105) {
                mem += 10;
                region = table + 89;
            } else if (first_four_bytes.second == 116) {
                if (first_four_bytes.third == 46) {
                    mem += 15;
                    region = table + 90;
                } else {
                    mem += 10;
                    region = table + 91;
                }
            } else {
                mem += 7;
                region = table + 92;
            }
        } else if (first_four_bytes.first == 84) {
            if (first_four_bytes.third == 114) {
                mem += 8;
                region = table + 93;
            } else {
                mem += 6;
                region = table + 94;
            }
        } else if (first_four_bytes.first == 85) {
            mem += 7;
            region = table + 95;
        } else if (first_four_bytes.first == 86) {
            if (first_four_bytes.second == 97) {
                mem += 10;
                region = table + 96;
            } else {
                mem += 7;
                region = table + 97;
            }
        } else if (first_four_bytes.first == 87) {
            mem += 7;
            region = table + 98;
        } else {
            mem += 7;
            region = table + 99;
        }

        int measurement = 0;
        for (; *mem != '\n'; ++mem) {
             measurement = measurement * 10 + (*mem - '0');
        }
        mem++;

        region->count++;
        region->total += measurement;
        region->max = region->max < measurement ? measurement : region->max;
        region->min = region->min > measurement ? measurement : region->min;
    }

    pthread_exit(NULL);
}

const char* solution() {
    int fd = open(MEASUREMENTS_PATH, O_RDONLY);

    struct stat sb;
    fstat(fd, &sb);

    off_t file_size = sb.st_size;
    off_t map_size = (file_size - file_size % 0x1000) + 0x1000;

    uint8_t* mem = (uint8_t*) mmap64(NULL, map_size, PROT_READ, MAP_SHARED, fd, 0);

    long num_threads = sysconf(_SC_NPROCESSORS_ONLN) * 1.5;
    off_t chunk_size = file_size / num_threads;

    pthread_t threads[64];
    WorkerContext* contexts[64];

    for (int i = 0; i < num_threads; ++i) {
        contexts[i] = (WorkerContext*) malloc(sizeof(WorkerContext));
    }

    off_t start_offset = 0;
    for (int i = 0; i < num_threads; ++i) {
        contexts[i]->mem = mem;
        contexts[i]->table = (Region*) calloc(sizeof(Region), 100);

        off_t end = start_offset + chunk_size;

        if (file_size <= end) {
            end = file_size - 1;
        } else {
            for (; mem[end] != '\n'; ++end);
        }

        contexts[i]->start_offset = start_offset;
        contexts[i]->length = end - start_offset + 1;

        pthread_create(&threads[i], NULL, worker, (void *)contexts[i]);

        start_offset += contexts[i]->length;
    }

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(threads[i], NULL);
    }

    Region* result_table = (Region*) calloc(sizeof(Region), 100);

    for (int i = 0; i < num_threads; ++i) {
        Region* thread_table = contexts[i]->table;

        for (int j = 0; j < 100; ++j) {
            Region* thread_region = &thread_table[j];
            Region* result_region = &result_table[j];

            if (result_region->count == 0) {
                result_region->total = 0;
                result_region->min = 0x7fffffff;
                result_region->max = -1;
            }

            result_region->count += thread_region->count;
            result_region->total += thread_region->total;            
            result_region->max = result_region->max < thread_region->max ? thread_region->max : result_region->max;
            result_region->min = result_region->min > thread_region->min ? thread_region->min : result_region->min;
        }
    }

    char* result = (char*) calloc(sizeof(char), 32768);

    for (int i = 0; i < 100; ++i) {
        Region* region = &result_table[INDEX_MAP[i]];

        char line[256];

        sprintf(line, "%s=%d;%d;%lu(%lu/%d)\n", CITY_NAMES[INDEX_MAP[i]], region->min, region->max, region->total / region->count, region-> total, region->count);
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
