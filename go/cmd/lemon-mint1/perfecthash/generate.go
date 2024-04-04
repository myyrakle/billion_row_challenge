package main

import (
	"brc/cmd/lemon-mint1/wyhash"
	"fmt"
	"runtime"
	"time"
	"unsafe"
)

var city_names = []string{
	"Adenarith",
	"Amsterdam",
	"Anápolis",
	"Aparecida de Goiânia",
	"Athens",
	"Austin",
	"Bahrain",
	"Bangalore",
	"Barcelona",
	"Belo Horizonte",
	"Belém",
	"Boa Vista",
	"Boston",
	"Brasília",
	"Brussels",
	"Bucharest",
	"Campinas",
	"Canada",
	"Central",
	"Chennai",
	"Chongqing",
	"Copenhagen",
	"Cuiabá",
	"Curitiba",
	"Dallas",
	"Dublin",
	"Duque de Caxias",
	"Feira de Santana",
	"Fortaleza",
	"Frankfurt",
	"Gaaphis",
	"Goiania",
	"Guadalajara",
	"Guarulhos",
	"Helsinki",
	"Hong Kong",
	"Hyderabad",
	"Indianapolis",
	"Ireland",
	"Istanbul",
	"Juiz de Fora",
	"Kiev",
	"Kolkata",
	"Krofast",
	"Krore",
	"Larfast",
	"London",
	"Londrina",
	"Los Angeles",
	"Macapá",
	"Madrid",
	"Manaus",
	"Mexico City",
	"Miami",
	"Milan",
	"Montreal",
	"Moscow",
	"Mumbai",
	"N. California",
	"N. Virginia",
	"New Delhi",
	"New York",
	"Niterói",
	"Nova Iguaçu",
	"Ohio",
	"Oregon",
	"Osaka",
	"Osasco",
	"Oslo",
	"Palmas",
	"Paris",
	"Porto Alegre",
	"Porto Velho",
	"Prico",
	"Prover",
	"Pune",
	"Qreigh",
	"Qrokwood",
	"Recife",
	"Ribeirão Preto",
	"Rio de Janeiro",
	"Salvador",
	"Santo André",
	"Sao Paulo",
	"Seoul",
	"Singapore",
	"St. Petersburg",
	"Stockholm",
	"Sydney",
	"São Bernardo do Campo",
	"São Gonçalo",
	"São José dos Campos",
	"São Paulo",
	"Tokyo",
	"Toronto",
	"Urgtin",
	"Vancouver",
	"Vienna",
	"Warsaw",
	"Zurich",
}

var cores = runtime.NumCPU()

const hash_space = 1 << 9
const hash_mask = hash_space - 1

func main() {
	if hash_space < len(city_names) {
		panic("hash space is too small")
	}

	found := make(chan struct{})
	var global_seed uint64 = 42

	// Brute force perfect hash function using all CPUs.
	for i := 0; i < cores; i++ {
		worker_seed := wyhash.WYRAND(&global_seed)
		fmt.Println("Worker", i, "Starting with Seed:", worker_seed)

		go func(seed uint64) {
			t := time.Now()
			var hashseed uint64
		L:
			for {
				var bitset [(hash_space + 63) / 64]uint64
				hashseed = wyhash.WYRAND(&seed)

				for _, city := range city_names {
					hash := wyhash.WYHASH_RAW(unsafe.Pointer(unsafe.StringData(city)), uintptr((len(city))), hashseed)
					hash = hash & hash_mask

					if hash > hash_space-32 || bitset[hash>>6]&(1<<(hash&63)) != 0 {
						continue L
					}
					bitset[hash>>6] |= 1 << (hash & 63)
				}
				break
			}
			fmt.Println("Found perfect hash function seed in", time.Since(t), ", hashseed:", hashseed)
			close(found)
		}(worker_seed)
	}

	<-found
}
