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
    long long dummy[5];
} Region;

typedef struct WorkerContext {
    off_t start_offset;
    off_t length;
    Region* table;
    uint8_t* mem;
    long long dummy[4];
} WorkerContext;

const char* CITY_NAMES[] = {"Adenarith", "Amsterdam", "Anápolis", "Aparecida de Goiânia", "Athens", "Austin", "Bahrain", "Barcelona", "Bangalore", "Belém", "Belo Horizonte", "Boa Vista", "Boston", "Brasília", "Brussels", "Bucharest", "Campinas", "Canada", "Central", "Chennai", "Chongqing", "Copenhagen", "Cuiabá", "Curitiba", "Dallas", "Duque de Caxias", "Dublin", "Frankfurt", "Feira de Santana", "Fortaleza", "Gaaphis", "Guarulhos", "Guadalajara", "Goiania", "Hyderabad", "Helsinki", "Hong Kong", "Ireland", "Istanbul", "Indianapolis", "Juiz de Fora", "Kiev", "Krore", "Krofast", "Kolkata", "Larfast", "Los Angeles", "Londrina", "London", "Macapá", "Madrid", "Manaus", "Mexico City", "Miami", "Milan", "Moscow", "Montreal", "Mumbai", "Niterói", "New York", "New Delhi", "N. California", "N. Virginia", "Nova Iguaçu", "Ohio", "Oregon", "Osasco", "Osaka", "Oslo", "Paris", "Palmas", "Prico", "Prover", "Pune", "Porto Alegre", "Porto Velho", "Qreigh", "Qrokwood", "Ribeirão Preto", "Rio de Janeiro", "Recife", "Salvador", "Santo André", "Sao Paulo", "São Paulo", "São Bernardo do Campo", "São José dos Campos", "São Gonçalo", "Seoul", "Singapore", "St. Petersburg", "Stockholm", "Sydney", "Toronto", "Tokyo", "Urgtin", "Vancouver", "Vienna", "Warsaw", "Zurich"};
const int INDEX_MAP[100] = {0, 1, 2, 3, 4, 5, 6, 8, 7, 10, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 25, 28, 29, 27, 30, 33, 32, 31, 35, 36, 34, 39, 37, 38, 40, 41, 44, 43, 42, 45, 48, 47, 46, 49, 50, 51, 52, 53, 54, 56, 55, 57, 61, 62, 60, 59, 58, 63, 64, 65, 67, 66, 68, 70, 69, 74, 75, 71, 72, 73, 76, 77, 80, 78, 79, 81, 82, 83, 88, 89, 90, 91, 92, 85, 87, 86, 84, 94, 93, 95, 96, 97, 98, 99};

void* worker(void* arg) {
    WorkerContext* context = (WorkerContext*)arg;

    Region* table = context->table;    

    off_t offset = context->start_offset;
    off_t end_offset = context->start_offset + context->length;

    uint8_t* mem = &context->mem[offset];
    uint8_t* end = &context->mem[end_offset];

    while (mem != end) {
        int index;

        if (mem[0] == 65) {
            if (mem[1] == 100) {
                mem += 10;
                index = 0;
            } else if (mem[1] == 109) {
                mem += 10;
                index = 1;
            } else if (mem[1] == 110) {
                mem += 10;
                index = 2;
            } else if (mem[1] == 112) {
                mem += 22;
                index = 3;
            } else if (mem[1] == 116) {
                mem += 7;
                index = 4;
            } else {
                mem += 7;
                index = 5;
            }
        } else if (mem[0] == 66) {
            if (mem[1] == 97) {
                if (mem[2] == 104) {
                    mem += 8;
                    index = 6;
                } else if (mem[2] == 114) {
                    mem += 10;
                    index = 7;
                } else {
                    mem += 10;
                    index = 8;
                }
            } else if (mem[1] == 101) {
                if (mem[2] == 108) {
                    if (mem[3] == 195) {
                        mem += 7;
                        index = 9;
                    } else {
                        mem += 15;
                        index = 10;
                    }
                }
            } else if (mem[1] == 111) {
                if (mem[2] == 97) {
                    mem += 10;
                    index = 11;
                } else {
                    mem += 7;
                    index = 12;
                }
            } else if (mem[1] == 114) {
                if (mem[2] == 97) {
                    mem += 10;
                    index = 13;
                } else {
                    mem += 9;
                    index = 14;
                }
            } else {
                mem += 10;
                index = 15;
            }
        } else if (mem[0] == 67) {
            if (mem[1] == 97) {
                if (mem[2] == 109) {
                    mem += 9;
                    index = 16;
                } else {
                    mem += 7;
                    index = 17;
                }
            } else if (mem[1] == 101) {
                mem += 8;
                index = 18;
            } else if (mem[1] == 104) {
                if (mem[2] == 101) {
                    mem += 8;
                    index = 19;
                } else {
                    mem += 10;
                    index = 20;
                }
            } else if (mem[1] == 111) {
                mem += 11;
                index = 21;
            } else {
                if (mem[2] == 105) {
                    mem += 8;
                    index = 22;
                } else {
                    mem += 9;
                    index = 23;
                }
            }
        } else if (mem[0] == 68) {
            if (mem[1] == 97) {
                mem += 7;
                index = 24;
            } else {
                if (mem[2] == 113) {
                    mem += 16;
                    index = 25;
                } else {
                    mem += 7;
                    index = 26;
                }
            }
        } else if (mem[0] == 70) {
            if (mem[1] == 114) {
                mem += 10;
                index = 27;
            } else if (mem[1] == 101) {
                mem += 17;
                index = 28;
            } else {
                mem += 10;
                index = 29;
            }
        } else if (mem[0] == 71) {
            if (mem[1] == 97) {
                mem += 8;
                index = 30;
            } else if (mem[1] == 117) {
                if (mem[2] == 97) {
                    if (mem[3] == 114) {
                        mem += 10;
                        index = 31;
                    } else {
                        mem += 12;
                        index = 32;
                    }
                }
            } else {
                mem += 8;
                index = 33;
            }
        } else if (mem[0] == 72) {
            if (mem[1] == 121) {
                mem += 10;
                index = 34;
            } else if (mem[1] == 101) {
                mem += 9;
                index = 35;
            } else {
                mem += 10;
                index = 36;
            }
        } else if (mem[0] == 73) {
            if (mem[1] == 114) {
                mem += 8;
                index = 37;
            } else if (mem[1] == 115) {
                mem += 9;
                index = 38;
            } else {
                mem += 13;
                index = 39;
            }
        } else if (mem[0] == 74) {
            mem += 13;
            index = 40;
        } else if (mem[0] == 75) {
            if (mem[1] == 105) {
                mem += 5;
                index = 41;
            } else if (mem[1] == 114) {
                if (mem[2] == 111) {
                    if (mem[3] == 114) {
                        mem += 6;
                        index = 42;
                    } else {
                        mem += 8;
                        index = 43;
                    }
                }
            } else {
                mem += 8;
                index = 44;
            }
        } else if (mem[0] == 76) {
            if (mem[1] == 97) {
                mem += 8;
                index = 45;
            } else {
                if (mem[2] == 115) {
                    mem += 12;
                    index = 46;
                } else {
                    if (mem[3] == 100) {
                        if (mem[4] == 114) {
                            mem += 9;
                            index = 47;
                        } else {
                            mem += 7;
                            index = 48;
                        }
                    }
                }
            }
        } else if (mem[0] == 77) {
            if (mem[1] == 97) {
                if (mem[2] == 99) {
                    mem += 8;
                    index = 49;
                } else if (mem[2] == 100) {
                    mem += 7;
                    index = 50;
                } else {
                    mem += 7;
                    index = 51;
                }
            } else if (mem[1] == 101) {
                mem += 12;
                index = 52;
            } else if (mem[1] == 105) {
                if (mem[2] == 97) {
                    mem += 6;
                    index = 53;
                } else {
                    mem += 6;
                    index = 54;
                }
            } else if (mem[1] == 111) {
                if (mem[2] == 115) {
                    mem += 7;
                    index = 55;
                } else {
                    mem += 9;
                    index = 56;
                }
            } else {
                mem += 7;
                index = 57;
            }
        } else if (mem[0] == 78) {
            if (mem[1] == 105) {
                mem += 9;
                index = 58;
            } else if (mem[1] == 101) {
                if (mem[2] == 119) {
                    if (mem[3] == 32) {
                        if (mem[4] == 89) {
                            mem += 9;
                            index = 59;
                        } else {
                            mem += 10;
                            index = 60;
                        }
                    }
                }
            } else if (mem[1] == 46) {
                if (mem[2] == 32) {
                    if (mem[3] == 67) {
                        mem += 14;
                        index = 61;
                    } else {
                        mem += 12;
                        index = 62;
                    }
                }
            } else {
                mem += 13;
                index = 63;
            }
        } else if (mem[0] == 79) {
            if (mem[1] == 104) {
                mem += 5;
                index = 64;
            } else if (mem[1] == 114) {
                mem += 7;
                index = 65;
            } else {
                if (mem[2] == 97) {
                    if (mem[3] == 115) {
                        mem += 7;
                        index = 66;
                    } else {
                        mem += 6;
                        index = 67;
                    }
                } else {
                    mem += 5;
                    index = 68;
                }
            }
        } else if (mem[0] == 80) {
            if (mem[1] == 97) {
                if (mem[2] == 114) {
                    mem += 6;
                    index = 69;
                } else {
                    mem += 7;
                    index = 70;
                }
            } else if (mem[1] == 114) {
                if (mem[2] == 105) {
                    mem += 6;
                    index = 71;
                } else {
                    mem += 7;
                    index = 72;
                }
            } else if (mem[1] == 117) {
                mem += 5;
                index = 73;
            } else {
                if (mem[2] == 114) {
                    if (mem[3] == 116) {
                        if (mem[4] == 111) {
                            if (mem[5] == 32) {
                                if (mem[6] == 65) {
                                    mem += 13;
                                    index = 74;
                                } else {
                                    mem += 12;
                                    index = 75;
                                }
                            }
                        }
                    }
                }
            }
        } else if (mem[0] == 81) {
            if (mem[1] == 114) {
                if (mem[2] == 101) {
                    mem += 7;
                    index = 76;
                } else {
                    mem += 9;
                    index = 77;
                }
            }
        } else if (mem[0] == 82) {
            if (mem[1] == 105) {
                if (mem[2] == 98) {
                    mem += 16;
                    index = 78;
                } else {
                    mem += 15;
                    index = 79;
                }
            } else {
                mem += 7;
                index = 80;
            }
        } else if (mem[0] == 83) {
            if (mem[1] == 97) {
                if (mem[2] == 108) {
                    mem += 9;
                    index = 81;
                } else if (mem[2] == 110) {
                    mem += 13;
                    index = 82;
                } else {
                    mem += 10;
                    index = 83;
                }
            } else if (mem[1] == 195) {
                if (mem[2] == 163) {
                    if (mem[3] == 111) {
                        if (mem[4] == 32) {
                            if (mem[5] == 80) {
                                mem += 11;
                                index = 84;
                            } else if (mem[5] == 66) {
                                mem += 23;
                                index = 85;
                            } else if (mem[5] == 74) {
                                mem += 22;
                                index = 86;
                            } else {
                                mem += 14;
                                index = 87;
                            }
                        }
                    }
                }
            } else if (mem[1] == 101) {
                mem += 6;
                index = 88;
            } else if (mem[1] == 105) {
                mem += 10;
                index = 89;
            } else if (mem[1] == 116) {
                if (mem[2] == 46) {
                    mem += 15;
                    index = 90;
                } else {
                    mem += 10;
                    index = 91;
                }
            } else {
                mem += 7;
                index = 92;
            }
        } else if (mem[0] == 84) {
            if (mem[1] == 111) {
                if (mem[2] == 114) {
                    mem += 8;
                    index = 93;
                } else {
                    mem += 6;
                    index = 94;
                }
            }
        } else if (mem[0] == 85) {
            mem += 7;
            index = 95;
        } else if (mem[0] == 86) {
            if (mem[1] == 97) {
                mem += 10;
                index = 96;
            } else {
                mem += 7;
                index = 97;
            }
        } else if (mem[0] == 87) {
            mem += 7;
            index = 98;
        } else {
            mem += 7;
            index = 99;
        }

        Region* region = &table[index];

        int measurement = 0;
        for (; *mem != '\n'; ++mem) {
             measurement = measurement * 10 + (*mem - '0');
        }
        mem++;

        if (region->count == 0) {
            region->total = 0;
            region->min = 0x7fffffff;
            region->max = -1;
        }

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

    long num_threads = sysconf(_SC_NPROCESSORS_ONLN);
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
