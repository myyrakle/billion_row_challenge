import os
from multiprocessing import Pool, cpu_count
from common import OUTPUT_PATH, MEASUREMENTS_PATH, Timer

def min_bigint(a, b):
    return a if a < b else b

def max_bigint(a, b):
    return a if a > b else b

def process_lines(index, total_workers):
    results = []
    with open(MEASUREMENTS_PATH, 'r') as file:
        for line_counter, line in enumerate(file):
            if line_counter % total_workers == index:
                city_name, measurement_string = line.strip().split(';')
                measurement = int(measurement_string)

                existing = next((result for result in results if result['city_name'] == city_name), None)
                if not existing:
                    results.append({
                        'city_name': city_name,
                        'min': measurement,
                        'max': measurement,
                        'total': measurement,
                        'count': 1
                    })
                else:
                    existing['min'] = min_bigint(existing['min'], measurement)
                    existing['max'] = max_bigint(existing['max'], measurement)
                    existing['total'] += measurement
                    existing['count'] += 1

    return results

def solution():
    num_workers = cpu_count()
    with Pool(num_workers) as pool:
        workers_results = pool.starmap(process_lines, [(i, num_workers) for i in range(num_workers)])

    city_map = {}
    for worker_result in workers_results:
        for city_data in worker_result:
            city_name = city_data['city_name']
            if city_name not in city_map:
                city_map[city_name] = {
                    'min': city_data['min'],
                    'max': city_data['max'],
                    'total': city_data['total'],
                    'count': city_data['count']
                }
            else:
                existing = city_map[city_name]
                existing['min'] = min_bigint(existing['min'], city_data['min'])
                existing['max'] = max_bigint(existing['max'], city_data['max'])
                existing['total'] += city_data['total']
                existing['count'] += city_data['count']

    sorted_cities = sorted(city_map.items())
    result = '\n'.join(
        f"{city_name}={data['min']};{data['max']};{data['total'] // data['count']}({data['total']}/{data['count']})"
        for city_name, data in sorted_cities
    ) + '\n'

    return result

def main():
    expected_output = open(OUTPUT_PATH, 'r').read()

    timer = Timer()
    got = solution()
    elapsed_time = timer.elapsed_as_milliseconds()

    print(f"Elapsed: {elapsed_time}ms")

    if expected_output == got:
        print("Test passed")
    else:
        print("Test failed")
        print("Expected:")
        print(expected_output)
        print("Actual:")
        print(got)

if __name__ == "__main__":
    main()